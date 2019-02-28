/**
   Copyright: Copyright (c) 2019, Oscar Holm. All rights reserved.
   License: MPL-2
   Author: Oscar Holm (oscar_holm94@hotmail.com)

   This Source Code Form is subject to the terms of the Mozilla Public License,
   v.2.0. If a copy of the MPL was not distributed with this file, You can obtain
   one at http://mozilla.org/MPL/2.0/.
*/
module dextool.plugin.cppmcdc.backend.analyze.analyze_helper;

import logger = std.experimental.logger;

import std.traits : Unqual;
import std.typecons : tuple, Flag, Yes, No, NullableRef;
import std.meta : staticIndexOf;

import clang.c.Index : CX_CXXAccessSpecifier, CX_StorageClass, CXLanguageKind;
import clang.Cursor : Cursor;
import clang.SourceLocation : SourceLocation;
import clang.Token : Token, TokenRange;

public import dextool.clang_extensions;

import cpptooling.analyzer.clang.ast : ClassTemplate, FunctionTemplate,
  Constructor, CxxMethod, ClassDecl, ClassTemplatePartialSpecialization,
  CxxBaseSpecifier, Destructor, FieldDecl, FunctionDecl, StructDecl, TemplateTypeParameter,
  TranslationUnit, UnionDecl, VarDecl, Visitor, IfStmt, WhileStmt, Statement, Declaration,
  ForStmt, SwitchStmt, CaseStmt, BinaryOperator, Expression, CompoundStmt, CallExpr, UnaryOperator,
  CompoundLiteralExpr, CxxBoolLiteralExpr, IntegerLiteral, CxxNullPtrLiteralExpr, FloatingLiteral,
  StringLiteral, CharacterLiteral, ParenExpr, ReturnStmt, MacroDefinition;
import cpptooling.analyzer.clang.type : retrieveType, TypeKind, TypeKindAttr,
    TypeResult, TypeResults, logTypeResult;
import cpptooling.analyzer.clang.store : put;
import cpptooling.analyzer.clang.analyze_helper : analyzeFieldDecl, toAccessType,
  toStorageClass, toLanguage, analyzeVarDecl;
import cpptooling.data : AccessType, VariadicType, CxParam, TypeKindVariable,
    CppVariable, LocationTag, Location, CxReturnType, CppVirtualMethod,
    CppMethodName, CppClassName, CppNs, CppAccess, StorageClass, CFunctionName,
  Language, CFunction, CxGlobalVariable;
import cpptooling.data.representation : DecisionBlock, CppIfStmt, CppReturnStmt, CppAssignment, CppForLoop, CppWhileLoop;
import cpptooling.data.symbol : Container, USRType;
import cpptooling.data.type : IDType, AnalyzeVarData, AnalyzeVariable;

import dextool.nullable;

immutable string[] tokenFilter;
immutable string[] capitalizeFilter;
immutable string[OpKind] logicalOp;
immutable string[OpKind] binaryAssignOp;
immutable string[OpKind] unaryAssignOp;

shared static this() {
  tokenFilter = ["std", "::", "return"];
  capitalizeFilter = ["true","false"];
  logicalOp = [
	       // [C99 6.5.7] Bitwise shift operators.
	       OpKind.Shl:"<<", // "<<"
	       OpKind.Shr:">>", // ">>"
	       // [C99 6.5.8] Relational operators.
	       OpKind.LT:"<", // "<"
	       OpKind.GT:">", // ">"
	       OpKind.LE:"<=", // "<="
	       OpKind.GE:">=", // ">="
	       // [C99 6.5.9] Equality operators.
	       OpKind.EQ:"==", // "=="
	       OpKind.NE:"!=", // "!="
	       // [C99 6.5.10] Bitwise AND operator.
	       OpKind.And:"&", // "&"
	       // [C99 6.5.11] Bitwise XOR operator.
	       OpKind.Xor:"^", // "^"
	       // [C99 6.5.12] Bitwise OR operator.
	       OpKind.Or:"|", // "|"
	       // [C99 6.5.13] Logical AND operator.
	       OpKind.LAnd:"&&", // "&&"
	       // [C99 6.5.14] Logical OR operator.
	       OpKind.LOr:"||", // "||"
	       ];
  binaryAssignOp = [
		    OpKind.Assign:"=", // "="
		    OpKind.MulAssign:"*=", // "*="
		    OpKind.DivAssign:"/=", // "/="
		    OpKind.RemAssign:"%=", // "%="
		    OpKind.AddAssign:"+=", // "+="
		    OpKind.SubAssign:"-=", // "-="
		    OpKind.ShlAssign:"<<=", // "<<="
		    OpKind.ShrAssign:">>=", // ">>="
		    OpKind.AndAssign:"&=", // "&="
		    OpKind.XorAssign:"^=", // "^="
		    OpKind.OrAssign:"|=", // "|="
		    ];
  unaryAssignOp = [
		   OpKind.PreInc:"++",
		   OpKind.PreDec:"--",
		   OpKind.PostInc:"++",
		   OpKind.PostDec:"--",
		   ];
}

/// Convert Cursor attributes to enum representation.
private CppVirtualMethod classify(T)(T c) @safe if (is(Unqual!T == Cursor)) {
    import cpptooling.data.type : MemberVirtualType;

    auto is_virtual = MemberVirtualType.Normal;
    auto func = () @trusted{ return c.func; }();

    if (!func.isValid) {
        // do nothing
    } else if (func.isPureVirtual) {
        is_virtual = MemberVirtualType.Pure;
    } else if (func.isVirtual) {
        is_virtual = MemberVirtualType.Virtual;
    }

    return CppVirtualMethod(is_virtual);
}

private CxParam[] toCxParam(ref TypeKind kind, ref Container container) @safe {
    import std.array;
    import std.algorithm : map, filter;
    import std.range : chain, zip, tee;
    import std.string : strip;

    import cpptooling.data.kind_type;

    auto tr_params = kind.info.params;

    auto t = tr_params.map!(a => container.find!TypeKind(a.usr));
    
    // dfmt off
    auto params = zip(
		      tr_params
		      .map!(a => container.find!TypeKind(a.usr))
		      .filter!(a => a.length > 0)
		      .map!(a => a.front),
		      tr_params)
      .map!((a) {
              if (a[1].isVariadic) {
                  return CxParam(VariadicType.yes);
              } else if (a[1].id.strip.length == 0) {
                  //TODO fix the above workaround with strip by fixing type.d
                  return CxParam(TypeKindAttr(a[0], a[1].attr));
              } else {
                  return CxParam(TypeKindVariable(TypeKindAttr(a[0], a[1].attr), CppVariable(a[1].id)));
              }
              });
    // dfmt on

    return () @trusted{ return params.array(); }();
}

private auto locToTag(SourceLocation c_loc) {
    auto l = c_loc.expansion();
    auto into = LocationTag(Location(l.file.name(), l.line, l.column));

    return into;
}

private bool isOperator(CppMethodName name_) @safe {
    import std.algorithm : among;

    if (name_.length <= 8) {
        // "operator" keyword is 8 char long, thus an optimization to first
        // look at the length
        return false;
    } else if (name_[8 .. $].among("=", "==", "+=", "-=", "++", "--", "+", "-",
            "*", ">", ">=", "<", "<=", ">>", "<<")) {
        return true;
    }

    return false;
}

bool isStandardLibraryIdentifier(string identifier) @trusted {
  import std.string : startsWith;

  return identifier.startsWith("std");
}

/* According to ANSI-C standard, double underscore ('__') in identifier are reserved for internal compiler use
 * single underscore followed by capital letter is reserved for compiler aswell.
 */
bool isCompilerIdentifier(string identifier) @trusted {
  import std.string : startsWith;
  import std.algorithm : canFind;
  import std.ascii : isUpper;

  if (identifier.length < 2) {
    return false;
  }
  if (identifier.startsWith("__") || (identifier[0] == '_' && isUpper(identifier[1]))) {
      return true;
  }
  return false;
}

IDType createFunctionHash(string funcName, CxParam[] params) {
  import std.format : format;
  import dextool.hash : makeHash;
  import cpptooling.data.representation : joinParamTypes;

  IDType defId = makeHash(format("%s(%s)", funcName, params.joinParamTypes));
  return defId;		  
}


DecisionBlock analyzeBlockDecisions(T)(const(T) v, ref Container container, ref AnalyzeVarData varData) @trusted {
  import std.typecons : scoped;
  
  auto visitor = scoped!DecisionBlockVisitor(container, varData);
  v.accept(visitor);

  return visitor.root;
}

struct MacroResult {
  Flag!"isValid" isValid;
  TypeKindAttr type;
  CppVariable name;
  LocationTag location;
  USRType instanceUSR;
  StorageClass storageClass;
  Flag!"isMacro" isMacro;
  string value;
}

MacroResult analyzeMacro(const(MacroDefinition) v, ref Container container, in uint indent) @trusted {
  import clang.c.Index : CXTokenKind, CXLinkageKind;
  import std.string : isNumeric;
  import std.array : array;
  import std.algorithm : filter, canFind;
  import cpptooling.data.kind_type_format : SimpleFmt, TypeId;
  import cpptooling.data.kind : TypeAttr;
  
  auto c_in = v.cursor;

  if(c_in.isMacroBuiltin || c_in.isPredefined || c_in.linkage != CXLinkageKind.invalid) {
    return MacroResult.init;
  }

  auto name = CppVariable(c_in.displayName);
  auto loc = locToTag(c_in.location());

  auto instance_usr = USRType(c_in.usr);
  assert(instance_usr.length > 0);

  container.put(loc, instance_usr, Yes.isDefinition);

  TypeKind tk;
  auto toks = c_in.tokens.filter!(a => a.kind == CXTokenKind.literal).array;
  string value;
  
  if (toks.length > 0) {
    auto spelling = toks[0].spelling;
    TypeKind.Info info;
    if (spelling.isNumeric) {
      // float
      if(spelling.canFind(".")) {
	info = TypeKind.SimpleInfo(SimpleFmt(TypeId("float")));
      } else // int {
	info = TypeKind.SimpleInfo(SimpleFmt(TypeId("int")));
    } else { // Treat everything else as strings..
      info = TypeKind.SimpleInfo(SimpleFmt(TypeId("string")));
    }
    tk = TypeKind(info, instance_usr);
    container.put(tk);
    value = spelling;
  }
  auto tAttr = TypeAttr.init;
  tAttr.isDefinition = Yes.isDefinition;
  auto type = TypeKindAttr(tk, tAttr);
  auto storage = () @trusted{ return c_in.storageClass.toStorageClass; }();

  return MacroResult(Yes.isValid, type, name, loc, instance_usr, storage, Yes.isMacro, value);
}

struct FunctionImplResult {
  Flag!"isValid" isValid;
  TypeKindAttr type;
  CFunctionName name;
  TypeKindAttr returnType;
  VariadicType isVariadic;
  StorageClass storageClass;
  CxParam[] params;
  LocationTag location;
  Flag!"isDefinition" isDefinition;
  Language language;
  string parentName; 
  DecisionBlock db;
}

FunctionImplResult analyzeFunctionImpl(const(FunctionDecl) v, ref Container container, in uint indent) @trusted {
  FunctionImplResult res = analyzeFunctionImpl(v.cursor, container, indent);
  if(res.isValid) {
    AnalyzeVarData tmp;
    tmp.funcId = createFunctionHash(res.name, res.params);
    NullableRef!AnalyzeVarData varData = &tmp;
    res.db = analyzeBlockDecisions(v, container, varData);
  }
  
  return res;
}

FunctionImplResult analyzeFunctionImpl(const(Cursor) c_in, ref Container container, in uint indent) @safe
in {
    import clang.c.Index : CXCursorKind;
    
    assert(c_in.kind == CXCursorKind.functionDecl);
}
body {
    import std.algorithm : among;
    import std.functional : pipe;

    import clang.Cursor : Cursor;
    import cpptooling.analyzer.clang.type : TypeKind, retrieveType,
        logTypeResult;
    import cpptooling.data : TypeResult, TypeKindAttr, CxParam, CFunctionName,
        CxReturnType, CFunction, VariadicType, LocationTag, StorageClass;
    import cpptooling.data.symbol : Container;

    // hint, start reading the function from the bottom up.
    // design is pipe and data transformation

    Nullable!TypeResults extractAndStoreRawType(const(Cursor) c) @safe {
        auto tr = () @trusted{ return retrieveType(c, container, indent); }();
        if (tr.isNull) {
            return tr;
        }

        assert(tr.primary.type.kind.info.kind.among(TypeKind.Info.Kind.func,
                TypeKind.Info.Kind.typeRef, TypeKind.Info.Kind.simple));
        put(tr, container, indent);

        return tr;
    }

    Nullable!TypeResults lookupRefToConcreteType(Nullable!TypeResults tr) @safe {
        if (tr.isNull) {
            return tr;
        }

        if (tr.primary.type.kind.info.kind == TypeKind.Info.Kind.typeRef) {
            // replace typeRef kind with the func
            auto kind = container.find!TypeKind(tr.primary.type.kind.info.canonicalRef).front;
            tr.primary.type.kind = kind;
        }

        logTypeResult(tr, indent);
        assert(tr.primary.type.kind.info.kind == TypeKind.Info.Kind.func);

        return tr;
    }

    static struct ComposeData {
        TypeResults tr;
        CFunctionName name;
        LocationTag loc;
        VariadicType isVariadic;
        StorageClass storageClass;
        Flag!"isDefinition" is_definition;
        Language language;
        string parentName;
    }

    ComposeData getCursorData(TypeResults tr) @safe {
        auto data = ComposeData(tr);

        data.name = CFunctionName(c_in.spelling);
        data.loc = locToTag(c_in.location());
        data.is_definition = cast(Flag!"isDefinition") c_in.isDefinition;
        data.storageClass = c_in.storageClass().toStorageClass;
        data.language = c_in.toLanguage;
	data.parentName = c_in.semanticParent.spelling;
	
        return data;
    }

    FunctionImplResult composeFunc(ComposeData data) @safe {
        Nullable!CFunction rval;

	if (isCompilerIdentifier(data.name) || isStandardLibraryIdentifier(data.name)) {
	  return FunctionImplResult.init;
	}

        auto return_type = container.find!TypeKind(data.tr.primary.type.kind.info.return_);
        if (return_type.length == 0) {
            return FunctionImplResult.init;
        }

        auto params = toCxParam(data.tr.primary.type.kind, container);

        VariadicType is_variadic;
        // according to C/C++ standard the last parameter is the only one
        // that can be a variadic, therefor only needing to peek at that
        // one.
        if (params.length > 0) {
            is_variadic = cast(VariadicType)() @trusted{
                return params[$ - 1].peek!VariadicType;
            }();
        }

	return FunctionImplResult(Yes.isValid, data.tr.primary.type, data.name,
				  TypeKindAttr(return_type.front, data.tr.primary.type.kind.info.returnAttr), is_variadic,
				  data.storageClass, params, data.loc, data.is_definition, data.language, data.parentName);
	
    }

    // dfmt off
    auto rval = pipe!(extractAndStoreRawType,
                      lookupRefToConcreteType,
                      // either break early if null or continue composing a
                      // function representation
                      (Nullable!TypeResults tr) {
                          if (tr.isNull) {
                              return FunctionImplResult.init;
                          } else {
                              return pipe!(getCursorData, composeFunc)(tr.get);
                          }
                      }
                      )
        (c_in);
    // dfmt on

    return rval;
}

struct ConstructorImplResult {
    TypeKindAttr type;
    CppMethodName name;
  CxParam[] params;
  LocationTag location;
  string parentName;
  DecisionBlock db;
}

/** Analyze the node for actionable data.
 * Params:
 *   v = node
 *   container = container to store the type in
 *   indent = to use when logging
 *
 * Returns: analyzed data.
 */
auto analyzeConstructorImpl(const(Constructor) v, ref Container container, in uint indent) @trusted {
    auto type = () @trusted{ return retrieveType(v.cursor, container, indent); }();
    put(type, container, indent);

    auto params = toCxParam(type.primary.type.kind, container);
    auto name = CppMethodName(v.cursor.spelling);
    auto parentName = v.cursor.semanticParent.spelling;
    AnalyzeVarData tmp;
    tmp.funcId = createFunctionHash(name, params);
    NullableRef!AnalyzeVarData varData = &tmp;
    Nullable!DecisionBlock db = analyzeBlockDecisions(v, container, varData);
    return ConstructorImplResult(type.primary.type, name, params, type.primary.location, parentName, db);
}

struct CxxMethodImplResult {
    TypeKindAttr type;
    CppMethodName name;
    CxParam[] params;
    Flag!"isOperator" isOperator;
    CxReturnType returnType;
    CppVirtualMethod virtualKind;
    Flag!"isConst" isConst;
    LocationTag location;
  string parentName;
  DecisionBlock db;
}

CxxMethodImplResult analyzeCxxMethodImpl(const(CxxMethod) v, ref Container container, in uint indent) @trusted {
  auto res = analyzeCxxMethodImpl(v.cursor, container, indent);
  AnalyzeVarData tmp;
  tmp.funcId = createFunctionHash(res.name, res.params);
  NullableRef!AnalyzeVarData varData = &tmp;
  res.db = analyzeBlockDecisions(v, container, varData);
  
  return res;
}

/// ditto
CxxMethodImplResult analyzeCxxMethodImpl(const(Cursor) c_in, ref Container container, in uint indent) @safe {
    auto type = () @trusted{ return retrieveType(c_in, container, indent); }();
    assert(type.get.primary.type.kind.info.kind == TypeKind.Info.Kind.func);
    put(type, container, indent);

    auto name = CppMethodName(c_in.spelling);
    auto params = toCxParam(type.primary.type.kind, container);
    auto return_type = CxReturnType(TypeKindAttr(container.find!TypeKind(
            type.primary.type.kind.info.return_).front, type.primary.type.kind.info.returnAttr));
    auto is_virtual = classify(c_in);
    auto parentName = c_in.semanticParent.spelling;

    return CxxMethodImplResult(type.primary.type, name, params,
			       cast(Flag!"isOperator") isOperator(name), return_type, is_virtual,
			       cast(Flag!"isConst") type.primary.type.attr.isConst, type.primary.location, parentName);
}

final class DecisionBlockVisitor : Visitor {
  import cpptooling.data;
  import cpptooling.analyzer.clang.ast;
  import cpptooling.analyzer.clang.cursor_logger : logNode, mixinNodeLog;

  mixin generateIndentIncrDecr;
  
  alias visit = Visitor.visit;

  DecisionBlock root;
  private {
    Container* container;
    NullableRef!AnalyzeVarData varData;
  }

  this(ref Container container, ref AnalyzeVarData varData) {
    this.container = &container;
    this.varData = &varData;
  }

  override void visit(const(FunctionDecl) v) {
    mixin(mixinNodeLog!());
    v.accept(this);
  }

  override void visit(const(CompoundStmt) v) {
    mixin(mixinNodeLog!());
    v.accept(this);
  }

  override void visit(const(Statement) v) {
    mixin(mixinNodeLog!());
    v.accept(this);
  }

  override void visit(const(Expression) v) {
    mixin(mixinNodeLog!());
    v.accept(this);
  }

  override void visit(const(IfStmt) v) @trusted {
    mixin(mixinNodeLog!());

    auto result = analyzeIfStmt(v, *container, varData);
    DecisionBlock.Decision if_stmt = new CppIfStmt(result.varData, result.condition, result.body_, result.else_);
    root.put(if_stmt);
  }

  override void visit(const(SwitchStmt) v) {
    mixin(mixinNodeLog!());
    v.accept(this);
  }

  override void visit(const(ForStmt) v) {
    mixin(mixinNodeLog!());
    v.accept(this);
  }

  override void visit(const(WhileStmt) v) {
    mixin(mixinNodeLog!());
    v.accept(this);
  }

  /*
   * Variable declarations such as "int a = t + 5;", "int a;"
   */
  override void visit(const(VarDecl) v) @trusted {
    import std.algorithm : map;
    import std.array : array;
    import std.format : format;
  
    mixin(mixinNodeLog!());
    
    auto result = analyzeVarDecl(v, *container, indent);
    auto tk = result.type.kind;
    tk.usr = result.instanceUSR;
    container.put(tk);

    auto tkVar = TypeKindVariable(result.type, result.name);
    auto varId = format("%s_%s", varData.funcId, tkVar.name.payload);

    auto children = v.cursor.children;
    if (children.length != 0) {
      auto lhs_len = children[0].tokens.length;
      auto expandRange = v.cursor.tokens[lhs_len..$];
      auto varAssigns = createCppAssignment(tkVar, children[0], expandRange, varData, *container, OpKind.Assign);
      root.put(varAssigns);
    } else {
      // Variable declared but not assigned to something.
      // We assign it to itself. This is so solver knows it is not defined.
      auto assignment = CppAssignment(varData.versionVariable(tkVar, varId, true), format("v['%s']", varId));
      root.put([assignment]);
    }
  }

  override void visit(const(BinaryOperator) v) @trusted {
    mixin(mixinNodeLog!());

    root.put(analyzeAssignment(v.cursor, varData, *container));
    v.accept(this);
  }

  override void visit(const(CompoundAssignOperator) v) @trusted {
    mixin(mixinNodeLog!());

    root.put(analyzeAssignment(v.cursor, varData, *container));
    v.accept(this);
  }

  /* TODO: We want to support calls to for example setter functions
   * within decision blocks. To be implemented here.
   */
  override void visit(const(CallExpr) v) {
    mixin(mixinNodeLog!());
    v.accept(this);
  }

  override void visit(const(UnaryOperator) v) @trusted {
    mixin(mixinNodeLog!());
    
    root.put(analyzeAssignment(v.cursor, varData, *container));
    v.accept(this);
    //auto result = analyzeUnaryOperator(v.cursor, varData, *container);
  }

  override void visit(const(ReturnStmt) v) @trusted {
    mixin(mixinNodeLog!());

    auto condition = analyzeCondition(v.cursor, varData, *container);
    bool isConditional = isReturnStmtConditional(v);
    DecisionBlock.Decision return_stmt = CppReturnStmt(varData.get, condition, isConditional);
    root.put(return_stmt);
  }
}

CppAssignment[] analyzeAssignment(const(Cursor) c_in, ref AnalyzeVarData varData, ref Container container) {
  import clang.c.Index : CXCursorKind;
  import cpptooling.analyzer.clang.type : makeTypeAttr;
  import std.format : format;
  assert(c_in.kind == CXCursorKind.binaryOperator || c_in.kind == CXCursorKind.compoundAssignOperator
	 || c_in.kind == CXCursorKind.unaryOperator);

  auto opExpr = getExprOperator(c_in);
  auto opSides = opExpr.sides;

  CppAssignment[] res;
  if(opExpr.kind in binaryAssignOp || opExpr.kind in unaryAssignOp) {
    auto lhs_len = opSides.lhs.tokens.length;
  
    TypeKindVariable tkVar;
    string varName = tokensToString(opSides.lhs.tokens);
    auto varId = format("%s_%s", varData.funcId, varName);
    
    if (auto tmp = cast(IDType)varId in varData.variables) {
      tkVar = tmp.var;
    } else {
      const uint indent = 0;
      TypeKindAttr tkAttr;
      // Special cases
      if (opSides.lhs.kind == CXCursorKind.arraySubscriptExpr) {
	auto lookupUsr = USRType(c_in.tokens[0].cursor.definition.usr);
	auto tk = container.find!TypeKind(lookupUsr).front;
	
	const Cursor lhs_c = opSides.lhs;
	auto lhs_t = opSides.lhs.type;
	tkAttr = TypeKindAttr(tk, makeTypeAttr(lhs_t, lhs_c));
	
      } else {
	auto refCursor = opSides.lhs.referenced;
	auto type = () @trusted{ return retrieveType(refCursor, container, indent); }();
	tkAttr = type.primary.type;
      }
      
      tkVar = TypeKindVariable(tkAttr, CppVariable(varName));
    }

    auto expandRange = c_in.tokens[lhs_len+1..$];
    if (opExpr.kind in binaryAssignOp) {
      res = createCppAssignment(tkVar, opSides.rhs, expandRange, varData, container, opExpr.kind);
    } else if (opExpr.kind in unaryAssignOp) {
      res = createCppAssignment(tkVar, opSides.lhs, expandRange, varData, container, opExpr.kind);
    }
  }

  return res;
}

CppAssignment[] createCppAssignment(TypeKindVariable tkVar, const(Cursor) c_rhs, TokenRange expandRange,
				    ref AnalyzeVarData varData, ref Container container, OpKind assignOp) {
  import std.format : format;
  
  CppAssignment[] res;

  auto varId = format("%s_%s", varData.funcId, tkVar.name.payload);
  auto rhs_len = c_rhs.tokens.length;
  
  string rhs;
  if (assignOp in binaryAssignOp) {
    if (rhs_len == 0) {
      rhs = tokensToString(expandRange);
    } else {
      rhs = analyzeCondition(c_rhs, varData, container);
    }
  }

  /*
    We expand the rhs for certain assignments such as "x += 2" to "x1 = x0 + 2",
    where x0 is the previous version of x
  */

  bool versionVar = true;
  switch(assignOp) {
  case OpKind.Assign: // "="
    versionVar = false;
    break;
  case OpKind.MulAssign: // "*="
    rhs = format("v['%s']*%s", varData.versionVariable(tkVar, varId).varId, rhs);
    break;
  case OpKind.DivAssign: // "/="
    rhs = format("v['%s']/%s", varData.versionVariable(tkVar, varId).varId, rhs);
    break;
  case OpKind.RemAssign: // "%="
    rhs = format("v['%s']%%s", varData.versionVariable(tkVar, varId).varId, rhs);
    break;
  case OpKind.AddAssign: // "+="
    rhs = format("v['%s']+%s", varData.versionVariable(tkVar, varId).varId, rhs);
    break;
  case OpKind.SubAssign: // "-="
    rhs = format("v['%s']-%s", varData.versionVariable(tkVar, varId).varId, rhs);
    break;
  case OpKind.ShlAssign: // "<<="
    rhs = format("v['%s']<<%s", varData.versionVariable(tkVar, varId).varId, rhs);
    break;
  case OpKind.ShrAssign: // ">>="
    rhs = format("v['%s']>>%s", varData.versionVariable(tkVar, varId).varId, rhs);
    break;
  case OpKind.AndAssign: // "&="
    rhs = format("v['%s']&%s", varData.versionVariable(tkVar, varId).varId, rhs);
    break;
  case OpKind.XorAssign: // "^="
    rhs = format("v['%s']^%s", varData.versionVariable(tkVar, varId).varId, rhs);
    break;
  case OpKind.OrAssign: // "|="
    rhs = format("v['%s']|%s", varData.versionVariable(tkVar, varId).varId, rhs);
    break;
  case OpKind.PreInc:
    rhs = format("v['%s']+1", varData.versionVariable(tkVar, varId).varId);
    break;
  case OpKind.PreDec:
    rhs = format("v['%s']-1", varData.versionVariable(tkVar, varId).varId);
    break;
  case OpKind.PostInc:
    rhs = format("v['%s']+1", varData.versionVariable(tkVar, varId).varId);
    break;
  case OpKind.PostDec:
    rhs = format("v['%s']-1", varData.versionVariable(tkVar, varId).varId);
    break;
  default:
    break;
  }
  
  auto var = varData.versionVariable(tkVar, varId, versionVar);
  auto assignment = CppAssignment(var, rhs);
  //logger.info(varId," == ", rhs);
  res ~= assignment ~ varData.innerAssigns;

  return res;
}

bool tokenIsMacro(Token tok) {
  import clang.c.Index : CXCursorKind;
  return tok.cursor.definition.kind == CXCursorKind.macroDefinition;
}

/* Returns true if a cursor has at least one token that is a macro identifier.
 */
bool cursorContainsMacro(const(Cursor) c_in) {
  import clang.Token : Token;
  import std.algorithm : canFind;
  
  return canFind!((Token a) => tokenIsMacro(a))(c_in.tokens);
}

string getMacroName(Token tok) {
  import clang.c.Index : CXCursorKind;
  assert (tok.cursor.definition.kind == CXCursorKind.macroDefinition);

  return tok.spelling;
}

void analyzeMacroCursor(const(Cursor) c_in, ref AnalyzeVarData varData, ref Container container) {
  import clang.c.Index : CXCursorKind;
  import std.format : format;
  import std.algorithm : filter;
  import std.array : array;
  
  auto macroTokens = c_in.tokens
    .filter!(a => a.cursor.definition.kind == CXCursorKind.macroDefinition)
    .array;

  string typeName, name;
  foreach(t ; macroTokens) {
    const uint indent = 0;
    const Cursor macroCursor = t.cursor.definition;
    assert (macroCursor.kind == CXCursorKind.macroDefinition);
      
    auto type = () @trusted{ return container.find!TypeKind(USRType(macroCursor.usr)); }();
      
    //Add macroId to varData.variables, we can get the value of it later
    auto info = cast(TypeKind.SimpleInfo)type.front.info;
    typeName = info.fmt.typeId.payload;
    name = t.spelling;
    logger.info("macro type: ", typeName);
    logger.info("macro name: ", name);
    break;
  }
  return;
}

AnalyzeVariable assignVersionVar(const(Cursor) lhs_c, string varName, ref AnalyzeVarData varData, ref Container container) {
  import std.format : format;
  string varId = format("%s_%s", varData.funcId, varName);
    
  TypeKindVariable tkVar;
  // Lookup in vardata, should exist due from an earlier VarDecl
  if (auto tmp = cast(IDType)varId in varData.variables) {
    tkVar = tmp.var;
  }  else {
    const uint indent = 0;
    auto lhs_ref = lhs_c.referenced;
    auto type = () @trusted{ return retrieveType(lhs_ref, container, indent); }();
    tkVar = TypeKindVariable(type.primary.type, CppVariable(varName));
  }

  auto lookupVar = varData.versionVariable(tkVar, varId, true);

  return lookupVar;
}

string analyzeBinaryOperator(const(Cursor) c_in, ref AnalyzeVarData varData, ref Container container) {
  import clang.c.Index : CXCursorKind;
  assert(c_in.kind == CXCursorKind.binaryOperator || c_in.kind == CXCursorKind.compoundAssignOperator);
  
  import std.format : format;
  import std.algorithm : map;
  import std.array : array, join;
  import std.conv : to;

  auto opExpr = getExprOperator(c_in);
  auto opSides = opExpr.sides;
  auto lhs_len = opSides.lhs.tokens.length;
  auto rhs_len = opSides.rhs.tokens.length; 
  
  string lhs;
  if (lhs_len == 0 && c_in.tokens.length > 0) {
    lhs = tokensToString(c_in.tokens[0..1]);
  } else {
    lhs = analyzeCondition(opSides.lhs, varData, container);
  }

  if (opExpr.kind in binaryAssignOp) {
    auto lookupVar = assignVersionVar(opSides.lhs, lhs, varData, container);

    if (opExpr.kind != OpKind.Assign) {
      auto expandRange = c_in.tokens[lhs_len..$];
      auto varAssigns = createCppAssignment(lookupVar.var, opSides.rhs, expandRange, varData, container, opExpr.kind);
      varData.innerAssigns ~= varAssigns;
    }

    return format("v['%s']", lookupVar.varId);
  }

  string rhs;
  if (rhs_len == 0 && c_in.tokens.length > 0) {
    rhs = tokensToString(c_in.tokens[lhs_len+1..$]);
  } else {
    rhs = analyzeCondition(opSides.rhs, varData, container);
  }

  auto op = opExpr.opString;

  //TODO: do something here for macros
  
  switch(opExpr.kind) {
    return format("Not(%s,%s)", lhs, rhs);
  case OpKind.LAnd:
    return format("And(%s,%s)", lhs, rhs);
  case OpKind.Or:
    return format("Or(%s,%s)", lhs, rhs);
  default:
    return format("%s%s%s", lhs, op, rhs);
  }
}

string analyzeParenExpr(const(Cursor) c_in, ref AnalyzeVarData varData, ref Container container) @trusted {
  import clang.c.Index : CXCursorKind;
  assert(c_in.kind == CXCursorKind.parenExpr );
  
  import std.format : format;
  import std.algorithm : map;
  import std.array : join, array;

  string insideParanthesis = c_in.children
    .map!(a => analyzeCondition(a, varData, container))
    .array
    .join;

  return format("(%s)", insideParanthesis);
}

string analyzeCallExpr(const(Cursor) c_in, ref AnalyzeVarData varData, ref Container container) @trusted {
  import clang.c.Index : CXCursorKind;
  assert(c_in.kind == CXCursorKind.callExpr);
  
  import std.format : format;
  import cpptooling.data.representation : paramNameToString;
  import std.algorithm : map, canFind;
  import std.array : array, join;
  import std.conv : to;

  //TODO: remove special cases.. This handles simple string comparisons, for example expression: parent == "None"
  if (c_in.tokens.length >= 3) {
    auto tmp = c_in.tokens[1].cursor.spelling;
    if (tmp.length > 2 && logicalOp.values.canFind(tmp[$-2..$])) {
      if (c_in.tokens.length > 3) {
	return "False";
      }
      auto lhs = analyzeCondition(c_in.tokens[0].cursor, varData, container);
      auto op = c_in.tokens[1].spelling;
      auto rhs = analyzeCondition(c_in.tokens[2].cursor, varData, container);
      return format("%s%s%s", lhs, op, rhs);
    }
  }

  auto children = c_in.children;
  auto varFqn = memberExprLhs(children[0]);
  auto defCursor = c_in.definition;
  auto callParams = children[1..$]
    .map!(a => varFqn ~ analyzeCondition(a, varData, container))
    .array;

  if (defCursor.kind != CXCursorKind.firstInvalid) {
    CxParam[] params;
    string callKey;
    const uint indent = 0;
    auto type = () @trusted{ return retrieveType(defCursor, container, indent); }();
    params = toCxParam(type.primary.type.kind, container);
    auto name = defCursor.spelling;
    // We associate the function call with a function hash for later use
    auto funcId = createFunctionHash(name, params);
    auto tkVar = TypeKindVariable(type.primary.type, CppVariable(to!string(funcId)));

    int idx = 0;
    if (funcId in varData.funcCallMapping) {
      idx = cast(int)varData.funcCallMapping[funcId].length;
    }
    
    AnalyzeVariable[IDType] callMethodVariables;
    // The function call may in its own decision block
    // be dependant on some variables, so add these.
    auto cpmVars = callParams.map!(a => format("%s_%s_%s", varFqn, a, idx)).array;
    if (defCursor.kind == CXCursorKind.cxxMethod) {
      auto tmpCursor = new CxxMethod(defCursor);
      auto res = analyzeCxxMethodImpl(tmpCursor, container, indent);
      callMethodVariables = res.db.varData.variables;
      auto callVars = callMethodVariables.keys.map!(a => format("%s_%s_%s", varFqn, to!string(a), idx)).array;
      cpmVars ~= callVars;
      foreach(a ; callMethodVariables.byKeyValue) {
	auto oldVarId = a.value.varId;
	a.value.varId = format("%s_%s_%s", varFqn, to!string(a.value.varId), idx);
	varData.paramRegexMapping[a.value.varId] = oldVarId;
	varData.variables[a.key] = a.value;
      }
    }

    varData.variables[funcId] = AnalyzeVariable(tkVar, to!string(funcId));
    auto callVars = callMethodVariables.keys.map!(a => to!string(a)).array;
    if (funcId in varData.funcCallMapping) {
      callKey = format("%s_%s", funcId, idx);
      varData.funcCallMapping[funcId] ~= callKey;
      varData.callParamMapping[callKey] ~= cpmVars;
    } else {
      callKey = format("%s_%s", funcId, idx);
      varData.funcCallMapping[funcId] = [callKey];
      varData.callParamMapping[callKey] = cpmVars;
    }
    
    auto paramsToMap = params.map!(a => paramNameToString(a)).array;
    for (int i = 0 ; i < paramsToMap.length ; i++) {
      auto a = paramsToMap[i];
      auto prmKey = format("%s_%s_%s_%s", varFqn, funcId, a, idx);
      varData.paramRegexMapping[prmKey] = callKey;
    }

    return format("v['%s']", callKey);
  }

  auto callName = tokensToString(children[0].tokens);
  
  return format("%s(%s)", callName, callParams.join(","));
}

string analyzeReturnStmt(const(Cursor) c_in, ref AnalyzeVarData varData, ref Container container) @trusted {
  import clang.c.Index : CXCursorKind;
  assert(c_in.kind == CXCursorKind.returnStmt );
  
  import std.algorithm : map;
  import std.array : join, array;
  
  auto children = c_in.children;
  if (children.length == 0) {
    return "";
  }

  string returnCondition;
  // Expand macro
  if (children[0].tokens.length == 0) {
    returnCondition ~= tokensToString(c_in.tokens[0..1]);
  }
  
  returnCondition ~= c_in.children
    .map!(a => analyzeCondition(a, varData, container))
    .array
    .join;

  return returnCondition;
}

string analyzeDeclRefExpr(const(Cursor) c_in, ref AnalyzeVarData varData, ref Container container)  {
  import clang.c.Index : CXCursorKind;
  assert(c_in.kind == CXCursorKind.declRefExpr);
  
  import std.format : format;
  import std.conv : to;

  string varName = tokensToString(c_in.tokens);
  auto defCursor = c_in.definition;
  auto declKind = defCursor.kind;

  string varId = format("%s_%s", varData.funcId, varName);
  if (declKind == CXCursorKind.parmDecl || declKind == CXCursorKind.varDecl) {
    const uint indent = 0;
    auto type = () @trusted{ return retrieveType(defCursor, container, indent); }();
    auto tkVar = TypeKindVariable(type.primary.type, CppVariable(varName));
    auto lookupVar = varData.versionVariable(tkVar, varId);

    return format("v['%s']", varId);
  } else {
    return format("v['%s']", varName);
  }
}

string analyzeUnaryOperator(const(Cursor) c_in, ref AnalyzeVarData varData, ref Container container) {
  import clang.c.Index : CXCursorKind;
  assert(c_in.kind == CXCursorKind.unaryOperator);

  import std.format : format;

  auto opExpr = getExprOperator(c_in);
  auto opSides = opExpr.sides;
  auto lhs = analyzeCondition(opSides.lhs, varData, container);
  
  string op = opExpr.opString;
  
  //TODO: Implement assigns using this.

  switch(opExpr.kind) {
  case OpKind.LNot:
    return format("Not(%s)", lhs);
  case OpKind.PreInc:
    auto lookupVarId = assignVersionVar(opSides.lhs, lhs, varData, container);
    return format("v['%s']", lookupVarId);
  case OpKind.PreDec:
    auto lookupVarId = assignVersionVar(opSides.lhs, lhs, varData, container);
    return format("v['%s']", lookupVarId);
  case OpKind.PostInc:
    auto lookupVarId = assignVersionVar(opSides.lhs, lhs, varData, container);
    return format("v['%s']", lookupVarId);
  case OpKind.PostDec:
    auto lookupVarId = assignVersionVar(opSides.lhs, lhs, varData, container);
    return format("v['%s']", lookupVarId);
  default:
    return format("%s%s", op, lhs);
  }
}

string analyzeMemberRefExpr(const(Cursor) c_in, ref AnalyzeVarData varData, ref Container container) {
  import clang.c.Index : CXCursorKind;
  assert(c_in.kind == CXCursorKind.memberRefExpr);

  import std.format : format;
    
  auto tokenString = tokensToString(c_in.tokens);
    
  auto defCursor = c_in.definition;
  const uint indent = 0;
  auto type = () @trusted{ return retrieveType(defCursor, container, indent); }();
  if (!type.isNull) {
    auto tkVar = TypeKindVariable(type.primary.type, CppVariable(tokenString));
    auto varId = format("%s_%s", varData.funcId, tkVar.name.payload);
    auto lookupVar = varData.versionVariable(tkVar, varId);
    
    return format("v['%s']", lookupVar.varId);
  } else {
    return format("v['%s']", tokenString);
  }
}

/*
 * Visits a Condition and extracts and transforms the condition
 */
final class ConditionVisitor : Visitor {
  import cpptooling.data;
  import cpptooling.analyzer.clang.ast;
  import cpptooling.analyzer.clang.cursor_logger : logNode, mixinNodeLog;

  mixin generateIndentIncrDecr;
  
  alias visit = Visitor.visit;

  string cond;
  AnalyzeVarData* varData;
  Container* container;

  this(ref AnalyzeVarData varData, ref Container container) {
    this.varData = &varData;
    this.container = &container;
  }

  override void visit(const(TranslationUnit) v) {
    mixin(mixinNodeLog!());
    v.accept(this); 
  }

  override void visit(const(Expression) v) {
    mixin(mixinNodeLog!());
    v.accept(this); 
  }

  override void visit(const(CompoundAssignOperator) v) @trusted {
    mixin(mixinNodeLog!());
    cond ~= analyzeBinaryOperator(v.cursor, *varData, *container);
  }

  override void visit(const(BinaryOperator) v) @trusted {
    mixin(mixinNodeLog!());
    cond ~= analyzeBinaryOperator(v.cursor, *varData, *container);
  }

  override void visit(const(UnaryOperator) v) @trusted {
    mixin(mixinNodeLog!());
    cond ~= analyzeUnaryOperator(v.cursor, *varData, *container);
  }

  // nestled paranthesis within condition
  override void visit(const(ParenExpr) v) {  
    mixin(mixinNodeLog!());
    cond ~= analyzeParenExpr(v.cursor, *varData, *container);
  }

  // references to declared expressions (vars, params etc)
  override void visit(const(DeclRefExpr) v) @trusted { 
    mixin(mixinNodeLog!());
    cond ~= analyzeDeclRefExpr(v.cursor, *varData, *container);
  }

  override void visit(const(CompoundStmt) v) {
    mixin(mixinNodeLog!());
    v.accept(this);
  }
  
  override void visit(const(MemberRefExpr) v) @trusted  {
    mixin(mixinNodeLog!());
    cond ~= analyzeMemberRefExpr(v.cursor, *varData, *container);
  }

  override void visit(const(CallExpr) v) @trusted {
    mixin(mixinNodeLog!());
    cond ~= analyzeCallExpr(v.cursor, *varData, *container);
  }

  override void visit(const(CompoundLiteralExpr) v) {
    mixin(mixinNodeLog!());
    cond ~= tokensToString(v.cursor.tokens);
  }
  
  override void visit(const(CxxBoolLiteralExpr) v) {
    import std.string : capitalize;
    mixin(mixinNodeLog!());
    cond ~= capitalize(tokensToString(v.cursor.tokens));
  }

  override void visit(const(IntegerLiteral) v) {
    mixin(mixinNodeLog!());
    cond ~= tokensToString(v.cursor.tokens);
  }

  override void visit(const(CxxNullPtrLiteralExpr) v) {
    mixin(mixinNodeLog!());
    cond ~= tokensToString(v.cursor.tokens);
  }

  override void visit(const(FloatingLiteral) v) {
    mixin(mixinNodeLog!());
    cond ~= tokensToString(v.cursor.tokens);
  }

  override void visit(const(CharacterLiteral) v) {
    mixin(mixinNodeLog!());
    cond ~= tokensToString(v.cursor.tokens);
  }

  override void visit(const(StringLiteral) v) @trusted {
    mixin(mixinNodeLog!());
    cond ~= createStringConstant(v.cursor, *varData);
  }
}

string createStringConstant(const(Cursor) c_in, ref AnalyzeVarData varData) {
  import clang.c.Index : CXCursorKind;
  assert (c_in.kind == CXCursorKind.stringLiteral);

  import std.format : format;

  auto tokenStr = tokensToString(c_in.tokens);
  return format("StringVal(%s)", tokenStr);
}

string memberExprLhs(const(Cursor) c_in) @trusted {
  import clang.c.Index : CXCursorKind;
  import cpptooling.analyzer.clang.ast : MemberRefExpr, DeclRefExpr;

  import std.typecons : scoped;
  import std.array : join;

  auto visitor = scoped!MemberExprVisitor();
  if (c_in.kind == CXCursorKind.memberRefExpr) {
    (new MemberRefExpr(c_in)).accept(visitor);
  } else if (c_in.kind == CXCursorKind.declRefExpr) {
    (new DeclRefExpr(c_in)).accept(visitor);
  }

  return join(visitor.varScope, "_");
}

/*
 * Visits a member expression and determines the lhs side.
 */
final class MemberExprVisitor : Visitor {
  import cpptooling.data;
  import cpptooling.analyzer.clang.ast;
  import std.array : insertInPlace;
  
  alias visit = Visitor.visit;

  string[] varScope;

  override void visit(const(MemberRefExpr) v) {
    varScope.insertInPlace(0, v.cursor.spelling);
    v.accept(this);
  }
  override void visit(const(DeclRefExpr) v) {
    varScope.insertInPlace(0, v.cursor.spelling);
    v.accept(this);
  }
}


/*
 * Visits a ReturnStmt and determines if it contains a logical condition
 * TODO: Could move more return statement logic here later?
 */
final class ReturnStmtVisitor : Visitor {
  import cpptooling.data;
  import cpptooling.analyzer.clang.ast;
  
  alias visit = Visitor.visit;

  bool isConditional = false;

  override void visit(const(Expression) v) {
    v.accept(this);
  }

  override void visit(const(Statement) v) {
    v.accept(this);
  }

  override void visit(const(BinaryOperator) v) {
    auto opExpr = getExprOperator(v.cursor);
    if (opExpr.kind in logicalOp) {
      isConditional = true;
    } else {
      v.accept(this);
    }
  }

  override void visit(const(UnaryOperator) v) {
    auto opExpr = getExprOperator(v.cursor);
    if (opExpr.kind == OpKind.LNot) { // "!x" etc.. 
      isConditional = true;
    } else {
      v.accept(this);
    }
  }
}

bool isReturnStmtConditional(const(ReturnStmt) v) @trusted {
  import std.typecons : scoped;
  auto visitor = scoped!ReturnStmtVisitor();
  v.accept(visitor);

  return visitor.isConditional;
}

string tokensToString(TokenRange tokens) @trusted {
  import std.algorithm : map, filter, canFind;
  import std.array : join, array;

  string capitalizeIfBool(string lit) {
    import std.string : capitalize;
    if (capitalizeFilter.canFind(lit)) {
      return capitalize(lit);
    }
    return lit;
  }

  string expandIfMacro(Token tok) {
    if (tokenIsMacro(tok)) {
      return getMacroName(tok);
    }
    return capitalizeIfBool(tok.spelling);
  }

  return tokens
    .map!(a => expandIfMacro(a))
    .filter!(a => !tokenFilter.canFind(a))
    .array
    .join;
}

string analyzeCondition(const(Cursor) c_in, ref AnalyzeVarData varData, ref Container container) @trusted {
  import std.typecons : scoped;
  import clang.c.Index : CXCursorKind;

  switch(c_in.kind) {
  case CXCursorKind.binaryOperator:
    return analyzeBinaryOperator(c_in, varData, container);
  case CXCursorKind.unaryOperator:
    return analyzeUnaryOperator(c_in, varData, container);
  case CXCursorKind.firstExpr: 
    auto visitor = scoped!ConditionVisitor(varData, container);
    // If it works it ain't stupid. On a sidenote we don't alter the cursor so..
    auto v = new TranslationUnit(c_in);
    v.accept(visitor);
    return visitor.cond;
  case CXCursorKind.parenExpr:
    return analyzeParenExpr(c_in, varData, container);
  case CXCursorKind.callExpr:
    return analyzeCallExpr(c_in, varData, container);
  case CXCursorKind.returnStmt:
    return analyzeReturnStmt(c_in, varData, container);
  case CXCursorKind.memberRefExpr:
    return analyzeMemberRefExpr(c_in, varData, container);
  case CXCursorKind.declRefExpr:
    return analyzeDeclRefExpr(c_in, varData, container);
  case CXCursorKind.stringLiteral:
    return createStringConstant(c_in, varData);
  default:
    return tokensToString(c_in.tokens);
  }
}

struct IfStmtResult {
  import cpptooling.data.representation : CppIfStmt, DecisionBlock;
  LocationTag location;
  string condition;
  AnalyzeVarData varData;
  Nullable!DecisionBlock body_;
  Nullable!CppIfStmt else_;
}

IfStmtResult analyzeIfStmt(const(IfStmt) v, ref Container container, NullableRef!AnalyzeVarData varData) @safe {
  return analyzeIfStmt(v.cursor, container, varData);
}

IfStmtResult analyzeIfStmt(const(Cursor) c_in, ref Container container, NullableRef!AnalyzeVarData varData) @safe
in {
  import clang.c.Index : CXCursorKind;

  assert(c_in.kind == CXCursorKind.ifStmt);
 }
body {
  static struct ComposeData {
    LocationTag loc;
    string cond;
    AnalyzeVarData varData;
    Nullable!DecisionBlock body_;
    Nullable!CppIfStmt else_;
  }
  
  IfStmtResult composeFunc() @trusted {
    import std.typecons : scoped;
    import clang.c.Index : CXCursorKind;
    import std.string : startsWith;
    import std.algorithm : canFind;
    
    auto data = ComposeData();
    data.loc = locToTag(c_in.location());

    auto children = c_in.children;

    if (!varData.isNull) {
      data.varData.funcId = varData.get.funcId;
      data.varData.variables = varData.get.variables;
    }

    auto ifstmt = getIfStmt(c_in);

    data.cond = analyzeCondition(ifstmt.cond, data.varData, container);

    // JSF++ specifies that if bodies should be compound statements (enclosed in brackets {...}).
    // Assumption: if bodies are always enclosed with brackets.
    if(ifstmt.then.kind != CXCursorKind.firstInvalid) {
      CompoundStmt body_node = new CompoundStmt(ifstmt.then);
      data.body_ = analyzeBlockDecisions(body_node, container, varData);
    }
    if (ifstmt.else_.kind != CXCursorKind.firstInvalid) {
      if (ifstmt.else_.kind == CXCursorKind.ifStmt) {
	auto elif_node = ifstmt.else_;
	auto result = analyzeIfStmt(elif_node, container, varData);
	CppIfStmt elif_stmt = new CppIfStmt(result.varData, result.condition, result.body_, result.else_);
	data.else_ = elif_stmt;
	data.varData.putVars(result.varData.variables);
      } else {
	CompoundStmt else_node = new CompoundStmt(ifstmt.else_);
	Nullable!DecisionBlock else_db = analyzeBlockDecisions(else_node, container, varData);
	CppIfStmt else_stmt = new CppIfStmt(data.varData, "", else_db);
	data.else_ = else_stmt;
      }
    }
 
    return IfStmtResult(data.loc, data.cond, data.varData, data.body_, data.else_);
  }

    auto rval = composeFunc();
    return rval;
 }
