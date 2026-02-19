module func_param_tests;

import std.conv : to;
import std.string : capitalize;
import std.traits : EnumMembers, isFloatingPoint;

import gir.func;
import gir.param;
import gir.type_node;

import common;
import generator;

/// Function parameter tests
void genFuncParamsTests()
{
  foreach (kind; EnumMembers!TypeKind)
  {
    final switch (kind) with(TypeKind)
    {
      case Unknown:
        break;
      case Basic:
        genInstance.dCow ~= ["", "@(\"FuncBasicParams\")", "unittest", "{"];

        dstring type, cType, dFuncName, funcName, cFuncName, decl;

        static foreach (i, basic; BasicTypes)
        { // Basic input -> return parameter tests
          type = basic.stringof.to!dstring;
          cType = gBasicTypes[i].to!dstring;
          dFuncName = "func"d ~ type.capitalize ~ "InReturn"d;
          funcName = "func_"d ~ type ~ "_in_return"d;
          cFuncName = genInstance.cSymPrefix.to!dstring ~ "_"d ~ funcName;
          decl = cType ~ " "d ~ cFuncName ~ "("d ~ cType ~ " param)"d;

          genInstance.cCow ~= ["", decl.to!dstring, "{", "return param;", "}"];

          writeGirFunction(GirFunc(name: funcName.to!string, cName: cFuncName.to!string, funcType: FuncType.Function,
            retType: cType.to!string, cRetType: cType.to!string,
            params: [GirParam(name: "param", type: cType.to!string, cType: cType.to!string)]));

          static if (isFloatingPoint!basic)
            genInstance.dCow ~= "assert(" ~ dFuncName ~ "(cast(" ~ type ~ ")-" ~ basic.max.to!dstring ~ ") == cast(" ~ type ~ ")-"
              ~ basic.max.to!dstring ~ ");";
          else
            genInstance.dCow ~= "assert(" ~ dFuncName ~ "(cast(" ~ type ~ ")" ~ basic.min.to!dstring ~ ") == cast(" ~ type ~ ")"
              ~ basic.min.to!dstring ~ ");";

          genInstance.dCow ~= "assert(" ~ dFuncName ~ "(cast(" ~ type ~ ")" ~ basic.max.to!dstring ~ ") == cast(" ~ type ~ ")"
            ~ basic.max.to!dstring ~ ");";

          // Basic input -> output parameter tests
          dFuncName = "func" ~ type.capitalize ~ "InToOut";
          funcName = "func_"d ~ type ~ "_in_to_out";
          cFuncName = genInstance.cSymPrefix.to!dstring ~ "_" ~ funcName;
          decl = "void " ~ cFuncName ~ "(" ~ cType ~ " param, " ~ cType ~ "* out_param)";

          genInstance.cCow ~= ["", decl.to!dstring, "{", "*out_param = param;", "}"];

          writeGirFunction(GirFunc(name: funcName.to!string, cName: cFuncName.to!string, funcType: FuncType.Function,
            params: [
              GirParam(name: "param", type: cType.to!string, cType: cType.to!string),
              GirParam(name: "out_param", type: cType.to!string, cType: cType.to!string ~ "*",
                direction: ParamDirection.Out)
            ]));

          genInstance.dCow ~= ["{"d, type ~ " outVal;"];

          static if (isFloatingPoint!basic)
            genInstance.dCow ~= [dFuncName ~ "(cast(" ~ type ~ ")-" ~ basic.max.to!dstring ~ ", outVal);",
              "assert(outVal == cast(" ~ type ~ ")-" ~ basic.max.to!dstring ~ ");"];
          else
            genInstance.dCow ~= [dFuncName ~ "(cast(" ~ type ~ ")" ~ basic.min.to!dstring ~ ", outVal);",
              "assert(outVal == cast(" ~ type ~ ")" ~ basic.min.to!dstring ~ ");"];

          genInstance.dCow ~= [dFuncName ~ "(cast(" ~ type ~ ")" ~ basic.max.to!dstring ~ ", outVal);",
            "assert(outVal == cast(" ~ type ~ ")" ~ basic.max.to!dstring ~ ");"];

          genInstance.dCow ~= "}"d;

          // Basic inout parameter tests
          dFuncName = "func" ~ type.capitalize ~ "Inout";
          funcName = "func_"d ~ type ~ "_inout";
          cFuncName = genInstance.cSymPrefix.to!dstring ~ "_" ~ funcName;
          decl = "void " ~ cFuncName ~ "(" ~ cType ~ "* param)";

          genInstance.cCow ~= ["", decl.to!dstring, "{", "*param = !*param;", "}"];

          writeGirFunction(GirFunc(name: funcName.to!string, cName: cFuncName.to!string, funcType: FuncType.Function,
            params: [
              GirParam(name: "param", type: cType.to!string, cType: cType.to!string ~ "*",
                direction: ParamDirection.InOut)
            ]));

          genInstance.dCow ~= ["{"d, type ~ " val = cast(" ~ type ~ ")0;", dFuncName ~ "(val);",
            "assert(val == cast(" ~ type ~ ")!0);", "}"d];
        }

        genInstance.dCow ~= "}";
        break;
      case String:
        genInstance.dCow ~= ["", "@(\"FuncStringParams\")", "unittest", "{"];

        // String in -> return no transfer
        auto dFuncName = "funcStringInReturnTransNone"d;
        auto funcName = "func_string_in_return_trans_none"d;
        auto cFuncName = genInstance.cSymPrefix.to!dstring ~ "_" ~ funcName;
        auto decl = "const char* " ~ cFuncName ~ "(const char* param)";

        genInstance.cCow ~= ["", decl.to!dstring, "{", "return param;", "}"];

        writeGirFunction(GirFunc(name: funcName.to!string, cName: cFuncName.to!string, funcType: FuncType.Function,
          retType: "utf8", cRetType: "const char *", retOwnership: Ownership.None,
          params: [GirParam(name: "param", type: "utf8", cType: "const char *", ownership: Ownership.None)]));

        // memCapture
        genInstance.dCow ~= "assert("d ~ dFuncName ~ "(\"blah123\") == \"blah123\");"d;
        // memAssertNone

        // String in -> return both transfer full
        dFuncName = "funcStringInReturnTransFull"d;
        funcName = "func_string_in_return_trans_full"d;
        cFuncName = genInstance.cSymPrefix.to!dstring ~ "_" ~ funcName;
        decl = "char* " ~ cFuncName ~ "(char* param)";

        genInstance.cCow ~= ["", decl.to!dstring, "{", "char* ret = g_strdup(param);", "g_free(param);", "return ret;", "}"];

        writeGirFunction(GirFunc(name: funcName.to!string, cName: cFuncName.to!string, funcType: FuncType.Function,
          retType: "utf8", cRetType: "char *", retOwnership: Ownership.Full,
          params: [GirParam(name: "param", type: "utf8", cType: "char *", ownership: Ownership.Full)]));

        // memCapture
        genInstance.dCow ~= "assert(" ~ dFuncName ~ "(\"blah123\") == \"blah123\");";
        // memAssertAllocFree

        // String in -> out no transfer
        dFuncName = "funcStringInToOutTransNone"d;
        funcName = "func_string_in_to_out_trans_none"d;
        cFuncName = genInstance.cSymPrefix.to!dstring ~ "_" ~ funcName;
        decl = "void " ~ cFuncName ~ "(const char* param, const char** outParam)";

        genInstance.cCow ~= ["", decl.to!dstring, "{", "*outParam = param;", "}"];

        writeGirFunction(GirFunc(name: funcName.to!string, cName: cFuncName.to!string, funcType: FuncType.Function,
          params: [
            GirParam(name: "param", type: "utf8", cType: "const char *", ownership: Ownership.None),
            GirParam(name: "out_param", type: "utf8", cType: "const char **", direction: ParamDirection.Out,
              ownership: Ownership.None)
          ]));

        // memCapture
        genInstance.dCow ~= ["{", "string outVal;", dFuncName ~ "(\"blah123\", outVal);", "assert(outVal == \"blah123\");", "}"];
        // memAssertNone

        // String in -> out both transfer full
        dFuncName = "funcStringInToOutTransFull"d;
        funcName = "func_string_in_to_out_trans_full"d;
        cFuncName = genInstance.cSymPrefix.to!dstring ~ "_" ~ funcName;
        decl = "void " ~ cFuncName ~ "(char* param, char** out_param)";

        genInstance.cCow ~= ["", decl.to!dstring, "{", "*out_param = g_strdup(param);", "g_free(param);", "}"];

        writeGirFunction(GirFunc(name: funcName.to!string, cName: cFuncName.to!string, funcType: FuncType.Function,
          params: [
            GirParam(name: "param", type: "utf8", cType: "char *", ownership: Ownership.Full),
            GirParam(name: "out_param", type: "utf8", cType: "char **", direction: ParamDirection.Out,
              ownership: Ownership.Full)
          ]));

        // memCapture
        genInstance.dCow ~= ["{", "string outVal;", dFuncName ~ "(\"blah123\", outVal);", "assert(outVal == \"blah123\");", "}"];
        // memAssertAllocFree

        genInstance.dCow ~= "}";
        break;
      case BasicAlias:
        break;
      case Enum:
        break;
      case Flags:
        break;
      case Callback:
        break;
      case Container:
        break;
      case StructAlias:
        break;
      case Struct:
        break;
      case Pointer:
        break;
      case Opaque:
        break;
      case Wrap:
        break;
      case Boxed:
        break;
      case Reffed:
        break;
      case Object:
        break;
      case Interface:
        break;
      case Namespace:
        break;
    }
  }
}

private void writeGirFunction(GirFunc girFunc)
{
  genInstance.girXml.openStartTag(FuncTypeValues[cast(int)girFunc.funcType].to!string);
  genInstance.girXml.writeAttr("name", girFunc.name);
  genInstance.girXml.writeAttr("c:identifier", girFunc.cName);
  genInstance.girXml.closeStartTag;

  genInstance.girXml.openStartTag("return-value");

  if (girFunc.retOwnership != Ownership.Unset)
    genInstance.girXml.writeAttr("transfer-ownership", girFunc.retOwnership == Ownership.Full ? "full" : "none");

  genInstance.girXml.closeStartTag;
  genInstance.girXml.openStartTag("type");
  genInstance.girXml.writeAttr("name", girFunc.retType.length > 0 ? girFunc.retType : "none");
  genInstance.girXml.writeAttr("c:type", girFunc.cRetType.length > 0 ? girFunc.cRetType
    : (girFunc.retType.length > 0 ? girFunc.retType : "void"));
  genInstance.girXml.closeStartTag;
  genInstance.girXml.writeEndTag; // type
  genInstance.girXml.writeEndTag; // return-value

  genInstance.girXml.openStartTag("parameters");
  genInstance.girXml.closeStartTag;

  foreach (param; girFunc.params)
  {
    genInstance.girXml.openStartTag("parameter");
    genInstance.girXml.writeAttr("name", param.name);

    if (param.ownership != Ownership.Unset)
      genInstance.girXml.writeAttr("transfer-ownership", param.ownership == Ownership.Full ? "full" : "none");

    if (param.direction != ParamDirection.In)
      genInstance.girXml.writeAttr("direction", param.direction == ParamDirection.Out ? "out" : "inout");

    genInstance.girXml.closeStartTag;
    genInstance.girXml.openStartTag("type");
    genInstance.girXml.writeAttr("name", param.type);
    genInstance.girXml.writeAttr("c:type", param.cType.length > 0 ? param.cType : param.type);
    genInstance.girXml.closeStartTag;
    genInstance.girXml.writeEndTag; // type
    genInstance.girXml.writeEndTag; // param
  }

  genInstance.girXml.writeEndTag; // parameters
  genInstance.girXml.writeEndTag; // function
}
