/**
   Copyright: Copyright (c) 2019, Oscar Holm. All rights reserved.
   License: MPL-2
   Author: Oscar Holm (oscar_holm94@hotmail.com)

   This Source Code Form is subject to the terms of the Mozilla Public License,
   v.2.0. If a copy of the MPL was not distributed with this file, You can obtain
   one at http://mozilla.org/MPL/2.0/.
*/
module dextool.plugin.cppmcdc.backend.backend;

import logger = std.experimental.logger;

import std.typecons : Flag, Yes, Nullable, NullableRef, Tuple;
import std.traits : Unqual;
import dsrcgen.cpp : CppModule, CppHModule;
import cpptooling.data : CppNs, CppClassName, CppNsStack, LocationTag, Location, TypeKind;
import cpptooling.data.type : IDType, AnalyzeVariable;
import cpptooling.data.representation : DecisionBlock, CxParam, AnalyzeVarData, CppAssignment;
import cpptooling.data.symbol.types : FullyQualifiedNameType;
import dextool.plugin.cppmcdc.backend.generate_cpp : generate;
import dextool.type : CustomHeader, DextoolVersion, FileName, AbsolutePath;
import dextool.plugin.cppmcdc.backend.interface_ : Parameters, Products, Transform;
import dextool.plugin.cppmcdc.backend.type : Code, GeneratedData, ImplData, ProcessData, FuncCallData,
  SolverAssign, SolverType, getSolverType, tkToSolverType;
import dextool.plugin.cppmcdc.backend.visitor.type : AnalyzeData;
import dextool.plugin.cppmcdc.backend.visitor.visitor : CppMcdcTUVisitor;

struct Backend {
    import std.typecons : Nullable;
    import cpptooling.analyzer.clang.context : ClangContext;

    import dextool.type : ExitStatusType;

    this(Parameters params, Products products, Transform transform) {
        this.analyze = AnalyzeData.make;
        this.ctx = ClangContext(Yes.useInternalHeaders, Yes.prependParamSyntaxOnly);
        this.params = params;
        this.products = products;
        this.transform = transform;
    }

  bool isHeader(string f) {
    import std.string : endsWith;
    return f.endsWith(".h") || f.endsWith(".hpp");
  }

  ExitStatusType analyzeFile(ClangContextT)(const AbsolutePath input_file, const string[] cflags, ref ClangContextT ctx) @trusted {
    import std.file : exists;
    import std.conv : to;
    import cpptooling.analyzer.clang.check_parse_result : hasParseErrors, logDiagnostic;
    import cpptooling.analyzer.clang.ast : ClangAST;
    import cpptooling.data : MergeMode;
    
    if (!exists(input_file)) {
      logger.errorf("File '%s' do not exist", input_file);
      return ExitStatusType.Errors;
    }

    logger.infof("Analyzing '%s'", input_file);

    auto translation_unit = ctx.makeTranslationUnit(input_file, cflags);
    if (translation_unit.hasParseErrors) {
      logDiagnostic(translation_unit);
      logger.error("Compile error...");
      return ExitStatusType.Errors;
    }

    if (isHeader(to!string(input_file))) {
      includes ~= input_file;
    } else {
      NullableRef!Container cont_ = &container;
      NullableRef!AnalyzeData analyz = &analyze.get();
      auto visitor = new CppMcdcTUVisitor(products, analyz, cont_);
      
      auto ast = ClangAST!CppMcdcTUVisitor(translation_unit.cursor);
      ast.accept(visitor);

      debug logger.tracef("%u", visitor.root);

      string[] namespaces = params.getNamespaces();
      auto fl = rawFilter(visitor.root, products, namespaces);

      analyze.root.merge(fl, MergeMode.full);
    }
    
    return ExitStatusType.Ok;
  }

  ExitStatusType analyzeFile(const AbsolutePath abs_in_file, const string[] use_cflags) {
    return analyzeFile(abs_in_file, use_cflags, ctx);
  }

    void process() {
        assert(!analyze.isNull);

        debug logger.trace(container.toString);

        logger.info("-- Starting to process --");
        logger.tracef("Filtered:\n%u", analyze.root);

	ImplData impl_data = ImplData.make;
	impl_data.putForLookup(analyze.classes);
	impl_data.putForLookup(analyze.cFunctions);
	impl_data.funcImplData = analyze.funcImplData;

        logger.info("Translating..");
	
        translate(analyze.root, impl_data, container, params);
        analyze.nullify();

	logger.tracef("Translated to implementation:\n%u", impl_data.root);

	logger.info("-- Starting solving --");
	recurseSolve(impl_data, container, params);

        // Wait for all solver process to finish
        impl_data.waitForProcesses();

	logger.tracef("Received solver results:\n", impl_data.solverResults);

        // At this stage we generate from information found in translation step
        logger.info("Generating..");
        GeneratedData gen_data;
        generate(impl_data, params, gen_data, container, includes);

        logger.info("Post processing..");
        postProcess(params, products, transform, gen_data);
    }

private:
    ClangContext ctx;
    Container container;
    Nullable!AnalyzeData analyze;
    Parameters params;
    Products products;
    Transform transform;
    AbsolutePath[] includes;
}

private:

@safe:

import cpptooling.data : CppRoot, CppClass, CppMethod, CppCtor, CppDtor,
    CFunction, CppNamespace, CppMethodOp;
import cpptooling.data.symbol : Container, USRType;
import dsrcgen.cpp : E;

bool isValidNamespace(const(CppNsStack) fullyQualifiedName, const string[] namespaces) {
    import std.algorithm : canFind;

    string fqn = fullyQualifiedName[$ - 1];
    return namespaces.canFind(fqn);
}

/** 
 * Filter the raw intermediate representation (IR)
 */
CppT rawFilter(CppT)(CppT input, Products prod, const ref string[] namespaces) @safe {
    import std.array : array;
    import std.algorithm : each, map, filter;
    import std.range : tee;
    import dextool.type : FileName;
    import cpptooling.data.representation : MergeMode;

    // setup
    static if (is(CppT == CppRoot)) {
        auto filtered = CppRoot.make;
    } else static if (is(CppT == CppNamespace)) {
        auto filtered = CppNamespace(input.resideInNs);
        filtered.merge(input, MergeMode.full);
        assert(!input.isAnonymous);
        assert(input.name.length > 0);
    } else {
        static assert("Type not supported: " ~ CppT.stringof);
    }

    // dfmt off
    input.namespaceRange
      .filter!(a => isValidNamespace(a.resideInNs, namespaces))
      .filter!(a => !a.isAnonymous)
      .map!(a => rawFilter(a, prod, namespaces))
      .each!(a => filtered.put(a));
    // dfmt on

    return filtered;
}

/*
 * Converts a class member range to a suitable form for input to solver.
 * @vars is the filter for which members we want to convert
 */
SolverType[] convertMemberRange(const ref CppClass c, ref Container container,
				ref AnalyzeVarData varData, ref ImplData data, IDType funcId) @trusted {
  import dextool.plugin.cppmcdc.backend.type : ClassType, resolveClassType, getTemplateParamTypes;
  import std.algorithm : filter, map, countUntil;
  import std.array : array;
  import std.format : format;
  
  auto vars = varData.variables;
  SolverType[] resVars;

  auto varNames = vars.values.map!(a => a.name).array;
  auto filteredMemberRange = c.memberRange
    .filter!(a => cast(IDType)format("%s_%s", funcId, a.name.payload) in vars);

  foreach(member ; filteredMemberRange) {
    TypeKind kind = member.type.kind;
    auto typeName = tkToSolverType(kind, container);
    if (!typeName.isNull) {
      // For each version..
      auto varName = format("%s_%s", funcId, member.name.payload);
      auto solverType = getSolverType(typeName);
      if (!solverType.isNull) {
	resVars ~= SolverType(solverType, varName);
      } else { // Not a primitive, check for a class with same name
	auto fqn = FullyQualifiedNameType(typeName); 
	auto classType = resolveClassType(fqn, data);

	if (!classType.isNull) {
	  if (auto tIndex = classType.c.templateParamRange.countUntil!(a => a.name == typeName)) {
	    resVars ~= SolverType(classType.templateArguments[tIndex], varName);
	  } else {
	    resVars ~= convertMemberRange(classType.c, container, varData, data, funcId);
	  }
	}
      }
    }
  }
  return resVars;
}

/*
 * Converts a assigns within a decision block to a suitable form for input to solver.
 */
SolverAssign[] convertAssignRange(CppAssignment[] assigns, ref Container container, ref AnalyzeVarData varData) @trusted {

  auto fcm = varData.funcCallMapping;
  SolverAssign[] resAssigns;
  
  void createAssign(string varName, TypeKind kind, CppAssignment a) {
    auto typeName = tkToSolverType(kind, container);
    if (!typeName.isNull) {
      auto sType = getSolverType(typeName);
      if (!sType.isNull) {
	auto solverType = SolverType(sType, varName, varName);
	auto varAssign = SolverAssign(solverType, a.toSolverFormat);
	resAssigns ~= varAssign;
      }
    }
  }

  auto vars = varData.variables;

  foreach(a ; assigns) {
    auto var = a.var;
    string varName = var.varId;
    TypeKind kind = var.typeKind;
    createAssign(varName, kind, a);
  }

  return resAssigns;
}

/*
 * Converts a function parameter range to a suitable form for input to solver
 * @vars is the filter for which params we want to convert
 */
SolverType[] convertParamRange(const CxParam[] params, ref Container container,
			       ref AnalyzeVarData varData, ref ImplData data, IDType funcId) @trusted {
  import cpptooling.data.representation : getName, getType, paramTypeToString;
  import std.array : array;
  import std.algorithm : map, filter, each;
  import std.conv : to;

  auto vars = varData.variables;

  string[string] paramPairs;
  foreach (a ; params) {
    auto t = getSolverType(getType(a));
    if (t.isNull) {
      auto fqn = FullyQualifiedNameType(paramTypeToString(a));
      auto paramC = data.lookupFirstClass(fqn);
      // Fqn matched a class => parameter is a class
      if (!paramC.isNull) {
	// Extract variables from the member range of the class
	convertMemberRange(paramC.get, container, varData, data, funcId)
	  .each!(a => paramPairs[a.name] = a.type);
      }
    } else {
      auto n = getName(a,"");
      paramPairs[n] = t;
    }
  }

  return vars.byKeyValue
    .filter!(a => a.value.name in paramPairs && a.value.origin)
    .map!(a => SolverType(paramPairs[a.value.name], to!string(a.key), a.value.name))
    .array;
}

/*
 * Converts a function call parameter range to a suitable form for input to solver
 */
string[] funcParamRangeToString(const CxParam[] params) @trusted {
  import cpptooling.data.representation : getName;
  import std.array : array;
  import std.algorithm : map;
  
  return params
    .map!(a => getName(a,""))
    .array;
}

SolverType[] convertParamRegexRange(string callName, NullableRef!AnalyzeVarData varData, ref Container container) @trusted {
  import std.string : isNumeric, split;
  import std.conv : to;
  
  SolverType[] resVars;
  if (varData.isNull || !(callName in varData.callParamMapping)) {
    return resVars;
  }
  
  auto vars = varData.variables;
  auto cpm = varData.callParamMapping;
  auto prm = varData.paramRegexMapping;
  
  foreach(a ; cpm[callName]) {
    if (!(a in prm)) {
      continue;
    }
    auto realName = prm[a];
    if (!(cast(IDType)realName in vars)) {
      continue;
    }
    auto varName = a;
    auto var = vars[cast(IDType)realName];
    TypeKind kind = var.typeKind;
    auto typeName = tkToSolverType(kind, container);
    if (!typeName.isNull) {
      auto solverType = getSolverType(typeName);
      if (!solverType.isNull) {
	resVars ~= SolverType(solverType, varName, realName);
      }
    }
  }
  return resVars;
}

/* Checks if any of the variables corresponds to a function hash.
 * When it does, it's a function call. Thus extract information.
 */
FuncCallData[] convertFunctions(ref AnalyzeVarData varData, ref ImplData data,
				ref Container container, ref SolverType[] dbVars) @trusted {
  import std.algorithm : filter;
  import std.conv : to;
  
  auto fcm = varData.funcCallMapping;
  auto cpm = varData.callParamMapping;
  auto prm = varData.paramRegexMapping;
  auto vars = varData.variables;

  FuncCallData[] funcCallData;
  foreach(var ; vars.byKey.filter!(a => (a in fcm))) {
    if (auto fData = var in data.funcImplData) {
      auto db = fData.decisionBlock;
      TypeKind kind = varData.variables[var].var.type.kind;
      auto typeName = tkToSolverType(kind, container);
      if (!typeName.isNull) {
	auto callType = getSolverType(typeName);
	if (!callType.isNull) {
	  auto params = funcParamRangeToString(fData.paramRange);
	  foreach (callName ; fcm[var]) {
	    NullableRef!AnalyzeVarData inVarData = &varData;
	    dbVars ~= convertParamRegexRange(callName, inVarData, container);
	    inVarData.funcId = callName;
	    string[] callExpressions = db.toSolverFormat(inVarData);
	    auto solverType = SolverType(callType, callName);
	    funcCallData ~= FuncCallData(solverType, callExpressions);
	  }
	}
      }  else if (auto cFunc = var in data.cFunctions) {
	//TODO: same but for cfunctions...
      }
    }
  }

  return funcCallData;
}

void callSolver(const CppMethod method_impl, CppClass c, ref ImplData data,
		ref Container container, string solver) @trusted {
  IDType funcId = cast(IDType)method_impl.id;
  auto fData = data.lookupFuncImpl(funcId);
  if (!fData.isNull) {
    auto db = fData.decisionBlock;
    auto params = fData.paramRange;

    if (db.hasCondition) {
      logger.info(c.name, ", ", method_impl.name);
      auto dbVarData = db.varData;   
      auto dbVars = dbVarData.variables;
      SolverType[] vars = convertMemberRange(c, container, dbVarData, data, funcId);
      vars ~= convertParamRange(params, container, dbVarData, data, funcId);
      auto assigns = convertAssignRange(db.assignRange, container, dbVarData);
      string[] expressions = db.toSolverFormat;
      auto funcCallData = convertFunctions(dbVarData, data, container, vars);
      ProcessData pd = ProcessData(vars, funcId, assigns);
      pd.startProcess(funcCallData, expressions, solver);
      data.pdata ~= pd;
    }
  }
  
}

void callSolver(const CppCtor ctor_impl, CppClass c, ref ImplData data,
		ref Container container, string solver) @trusted {
  import std.variant : visit;
  
  IDType funcId = cast(IDType) ctor_impl.id;
  auto fData = data.lookupFuncImpl(funcId);
  if (!fData.isNull) {
    auto db = fData.decisionBlock;
    auto params = fData.paramRange;
  
    if (db.hasCondition) {
      auto dbVarData = db.varData;
      auto dbVars = dbVarData.variables;
      SolverType[] vars = convertMemberRange(c, container, dbVarData, data, funcId);
      vars ~= convertParamRange(params, container, dbVarData, data, funcId);
      auto assigns = convertAssignRange(db.assignRange, container, dbVarData);
      string[] expressions = db.toSolverFormat;
      auto funcCallData = convertFunctions(dbVarData, data, container, vars);
      ProcessData pd = ProcessData(vars, funcId, assigns);
      pd.startProcess(funcCallData, expressions, solver);
      data.pdata ~= pd;
    }
  }
}

void callSolver(CppClass c, ref ImplData data, ref Container container, string solver) @trusted {
  import std.variant : visit;
 
  // dfmt off
  foreach(a ; c.methodRange) {
    a.visit!((const(CppMethod) a) => callSolver(a, c, data, container, solver),
	     (const(CppMethodOp) a) => logger.info(""),
	     (const(CppCtor) a) => callSolver(a, c, data, container, solver),
	     (const(CppDtor) a) => logger.info(""));
  }
  // dfmt on
}

void callSolver(CppNamespace ns, ref ImplData data, ref Container container, string solver) @trusted {
  import std.algorithm : each;
  ns.namespaceRange.each!(a => callSolver(a, data, container, solver));
  ns.classRange.each!(a => callSolver(a, data, container, solver));
  //foreach(method ; ns.funcRange) {
 
  //logger.info("callSolver: ", method);
    // dfmt off
    /*
      method.visit!((const(CppMethod) a) => callSolver(a, c_lookup.get, data, container, solver),
		  (const(CppMethodOp) a) => logger.info(a),
		  (const(CppCtor) a) => callSolver(a, c_lookup.get, data, solver),
		  (const(CppDtor) a) => logger.info(a));
    */
    //dfmt on
  //}
}

void recurseSolve(ref ImplData data, ref Container container, Parameters params) {
  import std.algorithm : each;

  CppRoot r = data.root;

  // dfmt off
  foreach(ns ; r.namespaceRange) {
    callSolver(ns, data, container, params.getSolverPath);
  }
  //r.namespaceRange
  //  .each!(a => callSolver(a, data, params.getSolverPath));
  r.classRange
    .each!(a => callSolver(a, data, container, params.getSolverPath));
  // dfmt on
}

/** Structurally transform the input.
 *  The filtered IR is transformed to a new IR 
 *  that represents the code to generate.
 */
void translate(CppRoot root, ref ImplData data, ref Container container, Parameters params) {
    import std.algorithm : map, filter, each;
    
    // dfmt off
    root.namespaceRange
      .map!(a => translate(a, data, container, params))
      .filter!(a => !a.isNull)
      .each!(a => data.root.put(a));

    root.classRange
      // .filter!(a => !a.isVirtual)
      // .filter!(a => !a.isAbstract)
      // .filter!(a => !a.isPure)
      .each!(a => data.root.put(a));

    // dfmt on
}

/** Translate namspaces and the content.
 */
Nullable!CppNamespace translate(CppNamespace input, ref ImplData data,
        const ref Container container, Parameters params) {
    import std.algorithm : map, filter, each;

    Nullable!CppNamespace ns = CppNamespace(input.resideInNs);
    
    // dfmt off
    input.funcRange
      .each!(a => ns.put(a));
    
    input.namespaceRange
      .map!(a => translate(a, data, container, params))
      .filter!(a => !a.isNull)
      .each!(a => ns.put(a));
    
    input.classRange
      // .filter!(a => !a.isVirtual)
      // .filter!(a => !a.isAbstract)
      // .filter!(a => !a.isPure)
      .each!(a => ns.put(a));
    // dfmt on

    return ns;
}

void postProcess(Parameters params, Products prods, Transform transf, ref GeneratedData gen_data) {
    import std.path : baseName;
    import cpptooling.generator.includes : convToIncludeGuard,
        generatePreInclude, generatePostInclude, makeHeader;

    static auto outputHdr(CppModule hdr, AbsolutePath fname, DextoolVersion ver) {
        auto o = CppHModule(convToIncludeGuard(cast(FileName) fname));
        o.header.append(makeHeader(cast(FileName) fname, ver));
        o.content.append(hdr);

        return o;
    }

    static auto output(CppModule code, AbsolutePath incl_fname,
            AbsolutePath dest, DextoolVersion ver) {
        import std.path : baseName;

        auto o = new CppModule;
        o.suppressIndent(1);
        o.append(makeHeader(cast(FileName) dest, ver));
        o.include(incl_fname.baseName);
        o.sep(2);
        o.append(code);

        return o;
    }

    auto mcdc_hdr = transf.createHeaderFile(null);

    foreach (k, v; gen_data.uniqueData) {
        final switch (k) with (Code) {
        case Kind.hdr:
            prods.putFile(mcdc_hdr, outputHdr(v, mcdc_hdr, params.getToolVersion));
            break;
        case Kind.impl:
            auto mcdc_cpp = transf.createImplFile(null);
            prods.putFile(mcdc_cpp, output(v, mcdc_hdr, mcdc_cpp, params.getToolVersion));
            break;
        }
    }
}
