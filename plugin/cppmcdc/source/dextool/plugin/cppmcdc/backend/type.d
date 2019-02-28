/**
   Copyright: Copyright (c) 2019, Oscar Holm. All rights reserved.
   License: MPL-2
   Author: Oscar Holm (oscar_holm94@hotmail.com)

   This Source Code Form is subject to the terms of the Mozilla Public License,
   v.2.0. If a copy of the MPL was not distributed with this file, You can obtain
   one at http://mozilla.org/MPL/2.0/.
*/
module dextool.plugin.cppmcdc.backend.type;

import cpptooling.data : CppRoot, CppClass, CppMethod, CppCtor, CppDtor, CFunction, CppNamespace, DecisionBlock, TypeKind;
import dsrcgen.cpp : CppModule, noIndent;
import cpptooling.data.symbol : Container, FullyQualifiedNameType, USRType;
import std.process : ProcessPipes, wait, Pid;
import std.json : JSONValue, parseJSON, JSONException;
import std.typecons : Nullable, NullableRef;

import logger = std.experimental.logger;

@safe:

struct ProcessData {
  import cpptooling.data.type : IDType, AnalyzeVariable;
  import std.algorithm : map;
  import std.array : array, join;
  
  ProcessPipes proc;
  IDType processFqn;
  SolverType[] vars;
  SolverAssign[] assigns;
  JSONResult[] results;

  this(SolverType[] vars, IDType fqn, SolverAssign[] assigns) {
    this.vars = vars;
    this.assigns = assigns;
    this.processFqn = fqn;
  }

  void startProcess(FuncCallData[] funcCallData, string[] expressions,
		    string solver) @trusted {
    import std.process : pipeProcess, Redirect;
    import std.array : array;
    import std.algorithm : map;

    string[] command = ["python", solver, "--var"]
      ~ vars.map!(a => a.toSolverFormat).array
      ~ [ "-e"] ~ expressions;

    if (assigns.length != 0) {
      command ~= ["-a"] ~ assigns.map!(a => a.toSolverFormat).array;
    }
    
    if (funcCallData.length != 0) {
      command ~= ["-fcd"] ~ funcCallData.map!(a => a.funcDefinition).array;
      command ~= ["-fce"] ~ funcCallData.map!(a => a.funcCallExpressions).join;
    }

    //logger.info("Command for solving: ", command);
    logger.info(join(command, " "));
    logger.info("vars: ", vars.map!(a => a.name));
    //logger.info("assigns: ", assigns.map!(a => a.toSolverFormat));
    foreach( e ; expressions) {
      logger.info("e: ", e);
    }
    
    this.proc = pipeProcess(command, Redirect.stdout | Redirect.stderr);
    //logger.info("Started solver process with PID: ", this.proc.pid.processID);
  }

  void waitPD() @trusted {
    import std.array : split, array;
    import std.algorithm : map;
    
    int pid = this.proc.pid.processID;
    int proc_code = wait(this.proc.pid);
    logger.info("Process ", pid, " finished with code: ", proc_code);

    string output;
    foreach (line; this.proc.stdout.byLine) output ~= line.idup;
    //logger.info("process returned result: ", output);

    if (proc_code != 0) {
      string[] errors;
      logger.info("-------------------");
      foreach (line; this.proc.stderr.byLine) errors ~= line.idup;
      logger.info("process returned errors: ", errors);
    }


    JSONValue[] solutions;
    try {
      solutions = parseJSON(output).array;
    } catch (JSONException err) {
      logger.info("Failed to parse json result!");
      logger.info("-------------------\n");
    }

    foreach(JSONValue solution ; solutions) {
      JSONResult res;
      foreach(var ; this.vars) {
	// Some vars might not be assigned so check that they are
	if (auto val = var.name in solution) {
	  auto rData = ResultData(var);
	  res.put(rData, val);
	}
      }
    
      if (res.assigns.length > 0) {
	this.results ~= res;
      }
    }
  }
}

struct SolverType {
  string type;
  string name;
  string realName;

  string toSolverFormat() {
    import std.string : format;
    return format("%s,%s", type, name);
  }
}

struct SolverAssign {
  SolverType type;
  string assignment;

  string toSolverFormat() {
    import std.string : format;
    return format("%s,%s", type.toSolverFormat, assignment);
  }
}

struct FuncCallData {
  import std.string : format;
  SolverType type;
  string[] callExpressions;

  string toString() {
    import std.array : join;
    
    return format("%s,%s", type.toSolverFormat, join(callExpressions, ","));
  }

  string funcDefinition() {
    return type.toSolverFormat;
  }
  
  string[] funcCallExpressions() {
    import std.algorithm : map;
    import std.array : array;
    
    return callExpressions
      .map!(a => format("%s,%s", type.name, a))
      .array;
  }
}

struct ImplData {
  import cpptooling.data.type : CppMethodName, IDType;
  import dextool.plugin.cppmcdc.backend.visitor.type : FuncImplData;

  CppRoot root;
  ProcessData[] pdata;

  /// Classes found during src analysis.
  CppClass[FullyQualifiedNameType] classes;
  
  /// CFunctions found during src analysis.
  CFunction[IDType] cFunctions;

  FuncImplData[IDType] funcImplData;

  // Results from decisions derived during analysis
  JSONResult[][IDType] solverResults;

  static auto make() {
    return ImplData(CppRoot.make);
  }

  void putForLookup(ref CppClass[FullyQualifiedNameType] other) @trusted {
    foreach (v; other.byKeyValue) {
      classes[v.key] = v.value;
    }
  }

  void putForLookup(ref CFunction[IDType] other) @trusted {
    foreach (v; other.byKeyValue) {
      cFunctions[v.key] = v.value;
    }
  }

  /// Returns: a range containing the class matching fqn, if found.
  auto lookupClass(FullyQualifiedNameType fqn) @safe {
    import std.range : only;
    import std.typecons : NullableRef;

    typeof(only(NullableRef!CppClass())) rval;
    if (auto c = fqn in classes) {
      rval = only(NullableRef!CppClass(c));
    }

    return rval;
  }

  /// Returns: Same as above but only returns first result
  auto lookupFirstClass(FullyQualifiedNameType fqn) @safe {
    import std.typecons : NullableRef;

    typeof(NullableRef!CppClass()) rval;
    if (auto c = fqn in classes) {
      rval = NullableRef!CppClass(c);
    }

    return rval;
  }

  auto lookupFuncImpl(IDType id) @safe {
    import std.typecons : NullableRef;

    typeof(NullableRef!FuncImplData()) rval;
    if (auto funcImpl = id in funcImplData) {
      rval = NullableRef!FuncImplData(funcImpl);
    }

    return rval;
  }

  auto lookupSolverResult(IDType id) @safe {
    import std.typecons : NullableRef;

    typeof(NullableRef!(JSONResult[])()) rval;
    if (auto solverResult = id in solverResults) {
      rval = NullableRef!(JSONResult[])(solverResult);
    }

    return rval;
  }

  void waitForProcesses() @trusted {
    foreach (p; pdata) {
      p.waitPD();
      solverResults[p.processFqn] ~= p.results;
    }
  }
}

struct GeneratedData {
  Code[Code.Kind] uniqueData;

  auto makeImpl(Code.Kind kind) {
    if (auto c = kind in uniqueData) {
      return *c;
    }

    Code m;
    m.cpp = new CppModule;

    uniqueData[kind] = m;
    return m;
  }

  auto makeHdr(Code.Kind kind) {
    if (auto c = kind in uniqueData) {
      return *c;
    }

    Code m;
    m.cpp = (new CppModule).noIndent;

    uniqueData[kind] = m;
    return m;
  }
}

struct Code {
  enum Kind {
	     hdr,
	     impl,
  }

  CppModule cpp;
  alias cpp this;
}

struct ResultData {
  import std.variant : Algebraic;
  alias aType = Algebraic!(bool, string);
  SolverType solverType;
  aType res;
}

struct JSONResult {
  import std.json : JSONValue, JSONType;
  import std.conv : to;
  import std.format : format;
  import std.string : toLower;

  ResultData[string] assigns;
 
  void put(ResultData data, const(JSONValue)* v) @trusted {
    switch(data.solverType.type) {
    case "bool":
      if (v.type() == JSONType.true_) { 
	data.res = true;
      } else {
	data.res = false;
      }
      break;
    case "string":
      data.res = format("\"%s\"", v.str);
      break;
    default:
      data.res = v.str;
      break;
    }
    assigns[data.solverType.name] = data;
  }
}

alias SolverResult = ResultData[string];

struct ClassType {
  CppClass c;
  FullyQualifiedNameType fqn;
  string[] templateArguments;

  this(CppClass c, string[] tArgs = []) {
    this.c = c;
    this.templateArguments = tArgs;
    this.fqn = c.fullyQualifiedName;
  }

  auto fmt() {
    import std.format : format;
    import std.array : join;

    if (c.isTemplate) {
      string args;
      if (templateArguments.length > 0) {
	args = templateArguments.join(",");
      } else {
	args = "double";
      }
      return format("%s<%s>", fqn, args);
    }
    return fqn;
  }

  auto ctor(ref ImplData data, ref const Container container) @trusted {
    import cpptooling.data.representation : getFirstConstructor;
    import cpptooling.data.type : IDType;
    import dextool.plugin.cppmcdc.backend.generate_cpp : paramsToCall;
    import std.conv : to;
    import std.format : format;
    
    auto ctor = getFirstConstructor(c);

    if (!ctor.isNull) {
      auto ctorId = to!IDType(ctor.id);
      return format("(%s)", paramsToCall(ctor.paramRange, data, NullableRef!(SolverResult).init, container, ctorId));
    } else {
      return "()";
    }
  }
}

string[] getTemplateParamTypes(string templateArgs) {
  import std.array : split;

  //Minimum template param is <T>
  assert (templateArgs.length >= 3);

  // remove "<" and ">" at start and end.
  return templateArgs[1..$-1].split(",");
}

Nullable!ClassType resolveClassType(FullyQualifiedNameType fqn, ref ImplData data) @trusted {
  import std.algorithm : find, map;
  import std.array : join, array;
  import std.format : format;
  import std.string : startsWith;

  Nullable!ClassType res;

  auto cIndex = data.classes.keys.find!(a => fqn.payload.startsWith(a.payload));
  if (cIndex.length > 0) {
    auto typeC = data.lookupFirstClass(cIndex[0]);
    if (!typeC.isNull) {
      auto typeFqn = typeC.fullyQualifiedName;
      if (typeC.isTemplate) {
	string[] usedTemplateTypes;
	if (fqn.length > typeFqn.length) {
	  auto prefixLen = cast(int)typeFqn.length;
	  usedTemplateTypes = getTemplateParamTypes(fqn.payload[prefixLen..$]);
	} else {
	  // Happens for non specialized templated class.
	  usedTemplateTypes = typeC.templateParamRange.map!(a => "double").array;
	}
	assert (usedTemplateTypes.length == typeC.templateParamRange.length);
	res = ClassType(typeC.get, usedTemplateTypes);

      } else {
	res = ClassType(typeC.get);
      }
    }
  }
  
  return res;
}

Nullable!string getSolverType(string typeName) {
  import std.algorithm : endsWith;

  Nullable!string res;
  if (typeName.endsWith("bool")) {
    res = "bool";
  }
  else if (typeName.endsWith("int") || typeName.endsWith("long")) {
    res = "int";
  }
  else if (typeName.endsWith("float") || typeName.endsWith("double")) {
    res = "real";
  }
  else if (typeName.endsWith("char*") || typeName.endsWith("char[]") | typeName.endsWith("char&") ||
	   typeName.endsWith("string") || typeName.endsWith("basic_string<char>")) {
    res = "string";
  }

  return res;
}

Nullable!string tkToSolverType(TypeKind kind, ref const Container container) {
  import cpptooling.data.kind : resolveTypeRef;
  
  auto findType(USRType a) {
    return container.find!TypeKind(a);
  }

  Nullable!string res;
  
  auto canonical_t = resolveTypeRef(kind, &findType);
  
  // TODO: implement missing cases
  final switch(canonical_t.info.kind) with (TypeKind.Info) {
    case Kind.ctor:
      auto info = cast(TypeKind.CtorInfo)  canonical_t.info;
      res = info.fmt.typeId.payload;
      break;
    case Kind.dtor:
      auto info = cast(TypeKind.DtorInfo)  canonical_t.info;
      break;
    case Kind.array:
      auto info = cast(TypeKind.ArrayInfo) canonical_t.info;
      // What is on right?
      res = info.fmt.typeId.left.payload;
      break;
      //return info.fmt.typeId.right.payload;
    case Kind.func:
      auto info = cast(TypeKind.FuncInfo) canonical_t.info;
      // What is on right?
      res = info.fmt.typeId.left.payload;
      break;
    case Kind.funcSignature:
      auto info = cast(TypeKind.FuncSignatureInfo) canonical_t.info;
      // What is on right?
      res = info.fmt.typeId.left.payload;
      break;
    case Kind.primitive:
      auto info = cast(TypeKind.PrimitiveInfo) canonical_t.info;
      res = info.fmt.typeId.payload;
      break;
    case Kind.record:
      auto info = cast(TypeKind.RecordInfo) canonical_t.info;
      res = info.fmt.typeId.payload;
      break;
    case Kind.simple:
      auto info = cast(TypeKind.SimpleInfo) canonical_t.info;
      res = info.fmt.typeId.payload;
      break;
    case Kind.typeRef:
    case Kind.funcPtr:
    case Kind.pointer:
      auto info = cast(TypeKind.PointerInfo) canonical_t.info;
      res = info.fmt.typeId.right.payload;
      break;
    case Kind.null_:
    }
  
  return res;
}
