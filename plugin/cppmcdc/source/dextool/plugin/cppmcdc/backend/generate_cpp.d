/**
   Copyright: Copyright (c) 2019, Oscar Holm. All rights reserved.
   License: MPL-2
   Author: Oscar Holm (oscar_holm94@hotmail.com)

   This Source Code Form is subject to the terms of the Mozilla Public License,
   v.2.0. If a copy of the MPL was not distributed with this file, You can obtain
   one at http://mozilla.org/MPL/2.0/.
*/
module dextool.plugin.cppmcdc.backend.generate_cpp;

import cpptooling.data : CppNamespace, LocationTag, CppNs, CppClassName, CppClass,
  CFunction, CppCtor, CppMethod, CppMethodOp, CppDtor, TypeKind;
import cpptooling.data.type : IDType;
import cpptooling.data.symbol.types : FullyQualifiedNameType;
import cpptooling.data.symbol : Container;
import cpptooling.data.representation : CxParam, funcToString;
import dextool.type : AbsolutePath;
import std.typecons : NullableRef, Nullable, Tuple;

import dsrcgen.cpp : CppModule, noIndent;
import dsrcgen.c;

import dextool.plugin.cppmcdc.backend.interface_ : Parameters;
import dextool.plugin.cppmcdc.backend.visitor.type : FuncImplData;
import dextool.plugin.cppmcdc.backend.type : Code, GeneratedData, ImplData, JSONResult,
  resolveClassType, getSolverType, tkToSolverType, ResultData, ClassType, SolverResult;

import logger = std.experimental.logger;

immutable string[string] typeDefaultValue;

shared static this() {
  typeDefaultValue = [
		      "int":"0","bool":"True", "float":"0.0",
		      "real":"0.0", "double":"0.0", "string":"",
		      "std::__cxx11::string":"\"\"", "char": "", "T":"0.0",
		      ];
};

/** Translate the structure to code.
 */

void generate(ref ImplData impl, Parameters params, ref GeneratedData gen_data,
	      ref const Container container, ref const AbsolutePath[] includes) {
  

  CppModule inner = gen_data.makeImpl(Code.Kind.impl);
  CppModule hdr = gen_data.makeHdr(Code.Kind.hdr);
  inner.stmt("int main(int argc, char** argv) {");
  foreach (a; impl.root.funcRange) {
  
  }

  foreach (a; impl.root.namespaceRange()) {
    generateForEachNs(impl, a, params, inner, hdr, container);
  }

  with(inner.text("")) {
    return_("0");
  }
  inner.stmt("}");

  generateMainHdr(impl, includes, hdr);
}

void generateMainHdr(ref ImplData impl, ref const AbsolutePath[] includes, ref CppModule hdr) {
  import std.algorithm : each;
  includes.each!(a => hdr.include(a));
}

string paramsToCall(const CxParam[] paramRange, ref ImplData impl, NullableRef!SolverResult solverResult,
		    ref const Container container, ref IDType funcId) @trusted {
  import std.array : join;
  import cpptooling.data.representation : getName, getType;
  import std.algorithm : map;
  import std.conv : to;
  import std.format : format;

  if (solverResult.isNull) {
    return paramRange.map!(a => resolveDefaultValue(getType(a), impl, container)).join(",");
  }

  string[] paramArr;
  
  foreach(a ; paramRange) {
    string varName = getName(a,"");
    string typeName = getType(a);
    auto tryKey = format("%s_%s", to!string(funcId), varName);
    if (tryKey in solverResult) {
      paramArr ~= to!string(solverResult[tryKey].res);
    } else {
      auto fqn = FullyQualifiedNameType(typeName);
      auto classType = resolveClassType(fqn, impl);

      if (!classType.isNull) {
	auto callIds = getCallIds(funcId, impl);
	foreach(callId ; callIds.byKeyValue) {
	  auto callResults = paramsFromFunctionId(callId.value, funcId, impl, solverResult);
	  foreach(callRes ; callResults) {
	    auto cid = cast(IDType)callId.key;
	    paramArr ~= membersToCall(fqn.payload, impl, classType.get, callRes, container, cid);
	  }
	}
	return format("%s(%s)", fqn, join(paramArr, ","));
      } else {
	paramArr ~= resolveDefaultValue(typeName, impl, container);
      }
    }
  }

  return join(paramArr, ", ");
}

private:

struct ClassInstance {
  string name_;
  string type;
  string ctor;
  int idx;

  this(CppClass c, string instanceName, ref ImplData data, ref const Container container) {

    this.name_ = c.name ~ instanceName;
    this.type = c.fullyQualifiedName;
    this.idx = 0;

    auto ct = resolveClassType(FullyQualifiedNameType(type), data);
    if (!ct.isNull) {
      if (c.isTemplate) {
	this.type = ct.fmt;
      }
      this.ctor = ct.ctor(data, container);
    } else {
      if (c.isTemplate) {
	this.type ~= "<double>";
      }
      this.ctor = "()";
    }
  }

  auto fmt() {
    import std.format : format;
    import std.algorithm : map;
    import std.array : join;

    return format("%s %s", type, name);
  }

  auto name() {
    import std.format : format;
    return format("%s_%s", name_, idx);
  }
}

string resolveDefaultValue(string typeName, ref ImplData impl, ref const Container container) {
  import std.format : format;
  import std.algorithm : find, map, canFind;
  import std.array : join, split;
  import std.string : startsWith;
  
  string res;
  if (typeName in typeDefaultValue) {
    res = typeDefaultValue[typeName];
  } else {

    // Reference types. Remove the reference since "&"
    auto splitRef = typeName.split("&");
    auto fqn = FullyQualifiedNameType(typeName);
    auto classType = resolveClassType(fqn, impl);
    if (!classType.isNull) {
      if (splitRef.length == 2) {
	// Default value for a reference parameter. Creating a new object and passing the object.
	res = format("*(new %s%s)", classType.fmt, classType.ctor(impl, container));
      } else {
	res = format("%s%s", classType.fmt, classType.ctor(impl, container));
      }
    } else if(typeName.canFind("vector<")) {
      res = format("std::vector<%s>()", typeName.split("vector<")[1].split(",")[0]);
    } else {
      logger.info("Unable to resolve type ", typeName);
    }
  }

  return res;
}

string membersToCall(string prevType, ref ImplData impl, ref ClassType classType,
		       ref ResultData[] solverRes, ref const Container container, ref IDType funcId) @trusted {
  import std.algorithm : filter, map, countUntil;
  import std.format : format;
  import std.conv : to;
  import std.array : join;
  
  string res;
  ResultData[] toAdd;
  
  foreach(member ; classType.c.memberRange) {
    TypeKind kind = member.type.kind;
    auto typeName = tkToSolverType(kind, container);
    if (!typeName.isNull) {
      auto varName = member.name.payload;
      auto tryKey = format("%s_%s", to!string(funcId), varName);
      
      auto i = solverRes.map!(a => a.solverType.realName).countUntil(tryKey);
      if (i != -1) {
	toAdd ~= solverRes[i];
      } else {
	auto fqn = FullyQualifiedNameType(typeName);
	auto memberClassType = resolveClassType(fqn, impl);
	if (!memberClassType.isNull) {
	  return membersToCall(typeName.get, impl, memberClassType.get, solverRes, container, funcId);
	}
      }
    }
  }

  return format("%s(%s)", prevType, toAdd.map!(a => to!string(a.res)).join(","));
}

string[][string] getCallIds(ref IDType funcId, ref ImplData impl) {
  import std.conv : to;
  
  string[][string] res;
  auto funcImplData = impl.lookupFuncImpl(funcId);
  if (!funcImplData.isNull) {
    auto varData = funcImplData.decisionBlock.varData;
    auto fcm = varData.funcCallMapping;
    foreach(a ; varData.variables.byKeyValue) {
      if (cast(IDType)a.key in fcm) {
	foreach(b ; fcm[cast(IDType)a.key]) {
	  if (to!string(a.key) in res) {
	    res[to!string(a.key)] ~= b;
	  } else {
	    res[to!string(a.key)] = [b];
	  }
	}
      }
    }
  }
  return res;
}

ResultData[][] paramsFromFunctionId(ref string[] callVars, ref IDType funcId,
				    ref ImplData impl, ref SolverResult solverResult) {
  import std.algorithm : canFind;
  import std.conv : to;

  ResultData[][] paramArr;
  if (callVars.length > 0) {
    auto funcImplData = impl.lookupFuncImpl(funcId);
    if (!funcImplData.isNull) {
      auto varData = funcImplData.decisionBlock.varData;
      auto fcm = varData.funcCallMapping;
      auto cpm = varData.callParamMapping;
      auto prm = varData.paramRegexMapping;
 
      foreach(a ; callVars) {
	auto callName = to!string(a);
	if (callName in cpm) {
	  auto prmVars = cpm[callName];
	  ResultData[] toAdd;
	  foreach(var ; prmVars) {
	    if (var in solverResult) {
	      toAdd ~= solverResult[var];
	    }
	  }
	  paramArr ~= toAdd;
	}
      }
    }
  }
  return paramArr;
}

void generateClassMethodTest(ref ImplData impl, ref CppModule inner, ref const CppMethod method,
			     ref ClassInstance instance, ref const Container container) {
  import std.format : format;
  
  IDType funcId = cast(IDType) method.id;
  auto classMethodName = method.name;

  with(inner.text("")) {
    stmt(E(format("%s = %s%s", instance.fmt, instance.type, instance.ctor)));
  }

  // Functions with decisions
  auto solverResult = impl.lookupSolverResult(funcId);
  auto params = method.paramRange;
  logger.info(format("method name: %s, id: %s", method.name, funcId));

  if (!solverResult.isNull) {
    if (params.length == 0) {
      
      //Get required states for class..
    } 
    else {
      foreach(r ; solverResult) {
	NullableRef!(SolverResult) result = &r.assigns;
	auto paramVals = paramsToCall(params, impl, result, container, funcId);
	with(inner.text("")) {
	  stmt(E(format("%s.%s(%s)", instance.name, classMethodName, paramVals)));
	}
      }
    }
  }
  // Functions without decisions
  else {
    // Function without decisions and parameters
    if (params.length == 0) {
      with(inner.text("")) {
	stmt(E(format("%s.%s()", instance.name, classMethodName)));
      }
    } else {
      // Function without decisions but with parameters
      auto paramVals = paramsToCall(params, impl, NullableRef!(SolverResult).init, container, funcId);
      with(inner.text("")) {
	stmt(E(format("%s.%s(%s)", instance.name, classMethodName, paramVals)));
      }
    }
  }
}

void generateFunctionTest(ref ImplData impl, ref CppModule inner, ref CppModule hdr,
			  ref CFunction f, ref const Container container) {
  
}

void generateClassCtorTest(ref ImplData impl, ref CppModule inner, ref const CppCtor ctor,
			   ref ClassInstance instance, ref const Container container) {
  import std.format : format;
  import std.conv : to;

  IDType funcId = cast(IDType) ctor.id;

  auto solverResult = impl.lookupSolverResult(funcId);
  auto params = ctor.paramRange;
  
  // Constructor with decisions
  if (!solverResult.isNull) {
    if (params.length == 0) {
      //Get required states for class..
    }
    else {
      foreach(r ; solverResult) {
	NullableRef!(SolverResult) result = &r.assigns;
	auto paramVals = paramsToCall(params, impl, result, container, funcId);
	//Get required state for class.. Map it to the callParams as needed
	with(inner.text("")) {
	  stmt(E(format("%s(%s)", instance.fmt, paramVals)));
	}
      }
    }
  }
  // Constructor without decisions
  else {
    // Constructor without decisions and parameters
    if (params.length == 0) {
      with(inner.text("")) {
	stmt(E(format("%s()", instance.fmt)));
      }
    } else {
      // Constructor without decisions but with parameters
      auto paramVals = paramsToCall(params, impl, NullableRef!(SolverResult).init, container, funcId);
      with(inner.text("")) {
	stmt(E(format("%s(%s)", instance.fmt, paramVals)));
      }
    }
  }
}

void generateClassTest(ref ImplData impl, ref CppModule inner,
		       ref CppModule hdr,  ref CppClass c, ref const Container container) @trusted {
  import std.conv : to;
  import std.variant : visit;
  import std.array : array;
  import std.string : format;

  if (c.methodRange.length == 0) {
    return;
  }

  auto instance = ClassInstance(c, "TestObject", impl, container);
  with(inner.text("")) { comment(format("Tests for class %s", c.name)); }

  // Skip protected functions as only derived classes can use them.
  foreach(a ; c.methodPublicRange ~ c.methodPrivateRange) {
    a.visit!((b) {
	alias T = typeof(b);
	static if (is(T == const(CppMethod))) {
	  auto tmp = cast(const(CppMethod))b;
	  generateClassMethodTest(impl, inner, tmp, instance, container);
	} else static if (is(T == const(CppMethodOp))) {
	  // TODO: Implement operators
	} else static if (is(T == const(CppCtor))) {
	  auto tmp = cast(const(CppCtor))b;
	  generateClassCtorTest(impl, inner, tmp, instance, container);
	} else static if (is(T == const(CppDtor))) {
	  // TODO: Implement Dtors
	}
      });
    instance.idx++;
  }
}

void generateForEachNs(ref ImplData impl, ref CppNamespace ns,
		       Parameters params, ref CppModule inner,
		       ref CppModule hdr, ref const Container container) {
  foreach (a; ns.funcRange) {
    //generateFunctionTest(impl, inner, hdr, a, container);
  }

  foreach(a ; ns.classRange) {
    generateClassTest(impl, inner, hdr, a, container);
  }
    
  foreach (a; ns.namespaceRange) {
    generateForEachNs(impl, a, params, inner, hdr, container);
  }
}
