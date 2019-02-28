/**
   Copyright: Copyright (c) 2019, Oscar Holm. All rights reserved.
   License: MPL-2
   Author: Oscar Holm (oscar_holm94@hotmail.com)

   This Source Code Form is subject to the terms of the Mozilla Public License,
   v.2.0. If a copy of the MPL was not distributed with this file, You can obtain
   one at http://mozilla.org/MPL/2.0/.
*/
module dextool.plugin.cppmcdc.backend.visitor.type;

import cpptooling.data : CppRoot, CppClass, DecisionBlock, CFunction;
import cpptooling.data.type : CxParam, IDType;

import std.variant : Algebraic; 

import logger = std.experimental.logger;

/// Data derived during analyze
struct AnalyzeData {
  import cpptooling.data.symbol.types : FullyQualifiedNameType;

    static auto make() {
        AnalyzeData r;
        r.root = CppRoot.make;
        return r;
    }

    CppRoot root;

    /// Classes found during src analysis.
    CppClass[FullyQualifiedNameType] classes;

  /// C functions found during src analysis.
    CFunction[IDType] cFunctions;

  FuncImplData[IDType] funcImplData;

  void putFuncForLookup(CppClass.CppFunc f) {
    auto fData = FuncImplData(f);
    funcImplData[cast(IDType)fData.id] = fData;
  }

  void putForLookup(CppClass c) {
    classes[c.fullyQualifiedName] = c;
  }

  void putForLookup(CFunction cfunc) {
    cFunctions[cast(IDType)cfunc.id] = cfunc;
  }
}

struct FuncImplData {
  import cpptooling.data.representation : methodParamsToString, methodNameToString,
    CppMethod, CppMethodOp, CppCtor, CppDtor;
  import std.format : format;
  import std.variant : visit;
  
  CppClass.CppFunc func;

  this(CppClass.CppFunc f) @trusted {
    this.func = f;
  }

  auto id() {
    return func.visit!((CppMethod a) => a.id,
		       (CppMethodOp a) => a.id,
		       (CppCtor a) => a.id,
		       (CppDtor a) => a.id);
  }

  auto decisionBlock() {
    // dfmt off
    return func.visit!((CppMethod a) => a.decisionBlock,
		       (CppMethodOp a) => a.decisionBlock,
		       (CppCtor a) => a.decisionBlock,
		       (CppDtor a) => a.decisionBlock);
    // dfmt on
  }

  auto paramRange() {
    // dfmt off
    return func.visit!((CppMethod a) => a.paramRange,
		       (CppMethodOp a) => a.paramRange,
		       (CppCtor a) => a.paramRange,
		       (CppDtor a) => CxParam[].init);
    // dfmt on
  }
}

enum VisitorKind {
    root,
    child
}
