/**
   Copyright: Copyright (c) 2019, Oscar Holm. All rights reserved.
   License: MPL-2
   Author: Oscar Holm (oscar_holm94@hotmail.com)

   This Source Code Form is subject to the terms of the Mozilla Public License,
   v.2.0. If a copy of the MPL was not distributed with this file, You can obtain
   one at http://mozilla.org/MPL/2.0/.
*/
module dextool.plugin.cppmcdc.backend.visitor.visitor;

import logger = std.experimental.logger;

import cpptooling.analyzer.clang.ast : Visitor;
import cpptooling.data : CppRoot, CppClass, CppCtor, CFunction,
  CppNamespace, AccessType, CppAccess;

import dextool.plugin.cppmcdc.backend.visitor.type;
import dextool.plugin.cppmcdc.backend.interface_;

alias CppMcdcTUVisitor = CppMcdcVisitor!(VisitorKind.root);

final class CppMcdcVisitor(VisitorKind RootT) : Visitor {
    import std.typecons : scoped, NullableRef;
  import cpptooling.data.type : toStringNs;
    import cpptooling.analyzer.clang.ast;
  import cpptooling.analyzer.clang.analyze_helper : analyzeVarDecl;
  import dextool.plugin.cppmcdc.backend.analyze.analyze_helper : analyzeFunctionImpl,
    analyzeConstructorImpl, analyzeCxxMethodImpl, isCompilerIdentifier,
    isStandardLibraryIdentifier;
    import cpptooling.data : CppRoot, CxGlobalVariable, CppNsStack,
        CxReturnType, CppNs, TypeKindVariable;
    import cpptooling.data.symbol : Container;
  import cpptooling.data.kind_type : toStringDecl;
    import cpptooling.analyzer.clang.cursor_logger : logNode, mixinNodeLog;
    import dsrcgen.cpp;

    alias visit = Visitor.visit;

    mixin generateIndentIncrDecr;

    NullableRef!Container container;
    NullableRef!AnalyzeData analyze_data;

    private {
        Products prod;
        CppNsStack ns_stack;
    }

    static if (RootT == VisitorKind.root) {
        CppRoot root;

        this(Products prod, NullableRef!AnalyzeData analyze, NullableRef!Container container) {
            this.prod = prod;
            this.analyze_data = analyze;
            this.container = container;
            this.root = CppRoot.make;
        }
    } else {
        CppNamespace root;

        this(Products prod, uint indent, CppNsStack ns_stack,
                NullableRef!AnalyzeData analyze, NullableRef!Container container) {
            this.root = CppNamespace(ns_stack);
            this.prod = prod;
            this.indent = indent;
            this.ns_stack = ns_stack;
            this.analyze_data = analyze;
            this.container = container;
        }
    }

    override void visit(const(TranslationUnit) v) {
      mixin(mixinNodeLog!());
      v.accept(this);
    }

    override void visit(const(Attribute) v) {
        mixin(mixinNodeLog!());
        v.accept(this);
    }

    override void visit(const(Declaration) v)@trusted {
      mixin(mixinNodeLog!());									    	
      v.accept(this);
    }

    override void visit(const(FunctionDecl) v) @trusted {
	auto nsString = toStringNs(ns_stack);
	if (validLocation(v)) {
	  mixin(mixinNodeLog!());
	  auto result = analyzeFunctionImpl(v, container, indent);
	  if (result.isValid) {
	    result.db.putNs(ns_stack);
	    auto func = CFunction(result.type.kind.usr, result.name, result.params,
				  CxReturnType(result.returnType), result.isVariadic,
				  result.storageClass, result.db);
	    root.put(func);
	    analyze_data.putForLookup(func);
	  }
	}
    }

  override void visit(const(CxxMethod) v) @trusted {
    import cpptooling.data.symbol.types : FullyQualifiedNameType;
    import cpptooling.data.representation : CppMethodOp, CppMethod;
    import cpptooling.data.type : CppConstMethod;

    auto ns = ns_stack;
    auto nsString = toStringNs(ns);
    if (validLocation(v)) {
      mixin(mixinNodeLog!());
      auto result = analyzeCxxMethodImpl(v, container, indent);
      ns.put(CppNs(result.parentName));
      result.db.putNs(ns);
    
      if (result.isOperator) {
	auto op = CppMethodOp(result.type.kind.usr, result.name, result.params,
			      result.returnType, CppAccess(AccessType.Public),
			      CppConstMethod(result.isConst), result.virtualKind);
	op.put(result.db);
	analyze_data.putFuncForLookup(cast(CppClass.CppFunc)op);
      
	debug logger.trace("operator: ", op.toString);
      } else {
	auto method = CppMethod(result.type.kind.usr, result.name, result.params,
				result.returnType, CppAccess(AccessType.Public),
				CppConstMethod(result.isConst), result.virtualKind);
	method.put(result.db);
	analyze_data.putFuncForLookup(cast(CppClass.CppFunc)method);
	
	debug logger.trace("method: ", method.toString);
      }
    }
  }

  override void visit(const(Constructor) v) @trusted {
    import cpptooling.data.symbol.types : FullyQualifiedNameType;
    import cpptooling.analyzer.clang.analyze_helper : toAccessType;

    auto nsString = toStringNs(ns_stack);
    if (!isCompilerIdentifier(nsString) && !isStandardLibraryIdentifier(nsString)) {
      mixin(mixinNodeLog!());
   
      auto result = analyzeConstructorImpl(v, container, indent);
      auto accessType = CppAccess(toAccessType(() @trusted{ return v.cursor.access; }().accessSpecifier));
      auto tor = CppCtor(result.type.kind.usr, result.name, result.params, accessType);
      tor.put(result.db);
      analyze_data.putFuncForLookup(cast(CppClass.CppFunc)tor);
    }
  }

  override void visit(const(VarDecl) v) @trusted {
    import clang.c.Index : CX_StorageClass;

    mixin(mixinNodeLog!());

    if (v.cursor.storageClass() == CX_StorageClass.extern_) {
      auto result = analyzeVarDecl(v, container, indent);
      auto var = CxGlobalVariable(result.instanceUSR,
				  TypeKindVariable(result.type, result.name));
      root.put(var);
    }
  }

    override void visit(const(UnexposedDecl) v) {
        mixin(mixinNodeLog!());
        v.accept(this);
    }

    override void visit(const(Directive) v) {
        mixin(mixinNodeLog!());
        v.accept(this);
    }

    override void visit(const(Preprocessor) v) {
        mixin(mixinNodeLog!());
        v.accept(this);
    }

  override void visit(const(MacroDefinition) v) {
    import dextool.plugin.cppmcdc.backend.analyze.analyze_helper : analyzeMacro;
    
    mixin(mixinNodeLog!());
    
    if(validLocation(v)) {
      auto result = analyzeMacro(v, container, indent);
      if (result.isValid) {
	auto var = CxGlobalVariable(result.instanceUSR,
				    TypeKindVariable(result.type, result.name), result.value);
	root.put(var);
      }
    }
  }

    override void visit(const(Reference) v) {
      mixin(mixinNodeLog!());
      v.accept(this);
    }

  override void visit(const(Statement) v) {
        mixin(mixinNodeLog!());
        v.accept(this);
    }

  override void visit(const(ClassTemplate) v){
    visitRecord(v);
  }

    override void visit(const(ClassDecl) v) {
        visitRecord(v);
    }

    override void visit(const(StructDecl) v) {
        visitRecord(v);
    }

    void visitRecord(T)(const T v) @trusted {
        import std.typecons : scoped;
        import cpptooling.analyzer.clang.analyze_helper : analyzeRecord;
	//import dextool.plugin.cppmcdc.backend.analyze.analyze_helper : ClassVisitor;
	import cpptooling.analyzer.clang.analyze_helper : ClassVisitor;

        mixin(mixinNodeLog!());

	if (validLocation(v)) {
	  auto result = analyzeRecord(v, container, indent + 1);
	  debug logger.trace("class: ", result.name);
	  if (result.name.length != 0 && v.cursor.isDefinition) {
	    auto visitor = scoped!ClassVisitor(v, ns_stack, result, container, indent + 1);
	    v.accept(visitor);
	    root.put(visitor.root);
	    analyze_data.putForLookup(visitor.root);
	  }
	}
    }

  bool validLocation(T)(const T v) {
    import std.string : endsWith, startsWith;
    
    auto c_in = v.cursor;
    if (isCompilerIdentifier(c_in.spelling) || isStandardLibraryIdentifier(c_in.spelling)) {
      return false;
    }
    auto fname = c_in.location.file.name;
    if (fname.endsWith("iscanonical.h")) {
      return false;
    }
    if (fname.startsWith("/usr/lib/")) {
      return false;
    }
    if (fname.startsWith("/usr/include/") && !fname.endsWith("/math.h")) {
      return false;
    }
    
    return true;
  }

    override void visit(const(Namespace) v) @trusted {
      mixin(mixinNodeLog!());

      if (validLocation(v)) {
	auto ns = v.cursor.spelling;
        () @trusted { ns_stack ~= CppNs(ns); }();
        scope (exit)
	  ns_stack = ns_stack[0 .. $ - 1];

        auto ns_visitor = scoped!(CppMcdcVisitor!(VisitorKind.child))(prod,
								      indent, ns_stack, analyze_data, container);

        v.accept(ns_visitor);

        root.put(ns_visitor.root);
      }
    }
}
