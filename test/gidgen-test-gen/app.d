module generate;

import dxml.writer;
import std.array : Appender, appender;
import std.conv : to;
import std.file : writeFile = write;
import std.meta : AliasSeq;
import std.path : buildPath;
import std.string : capitalize, split, toLower;
import std.traits : EnumMembers, isFloatingPoint;

import code_writer;
import gir.func;
import gir.param;
import gir.type_node;

// Basic types which are tested for the TypeKind.Basic
alias BasicTypes = AliasSeq!(bool, byte, ubyte, short, ushort, int, uint, long, ulong, float, double);

// glib types for BasicTypes
immutable string[] gBasicTypes = ["gboolean", "gint8", "guint8", "gint16", "guint16", "int", "guint", "gint64",
  "guint64", "gfloat", "gdouble"];

struct GirFunc
{
  string name;
  string cName;
  FuncType funcType;
  string retType;
  string cRetType;
  Ownership retOwnership = Ownership.Unset;
  string retArrayType;
  string retArrayCType;
  GirParam[] params;
}

struct GirParam
{
  string name;
  string type;
  string cType;
  ParamDirection direction = ParamDirection.In;
  Ownership ownership = Ownership.Unset;
}

class Generator
{
  enum cIdentPrefix = "Gidg";
  enum cSymPrefix = "gidg";
  enum girPath = "gir";
  enum gidgenTestPath = "gidgen-test";

  this()
  {
    cCow = new CodeWriter(buildPath(gidgenTestPath, "gidgen-test.c"), [
      "#include <glib.h>", "",
    ]);

    dCow = new CodeWriter(buildPath(gidgenTestPath, "gidgen-test.d"), [
      "module gidgen_test;", "",
      "import gidgentest.global;",
      "",
      "void main() {}",
    ]);

    girAp = appender!string;
    girAp.writeXMLDecl!string;
    girXml = xmlWriter(girAp);

    girXml.openStartTag("repository");
    girXml.writeAttr("version", "1.2");
    girXml.writeAttr("xmlns", "http://www.gtk.org/introspection/core/1.0");
    girXml.writeAttr("xmlns:c", "http://www.gtk.org/introspection/c/1.0");
    girXml.writeAttr("xmlns:glib", "http://www.gtk.org/introspection/glib/1.0");
    girXml.closeStartTag;

    girXml.openStartTag("include");
    girXml.writeAttr("name", "GLib");
    girXml.writeAttr("version", "2.0");
    girXml.closeStartTag;
    girXml.writeEndTag;

    girXml.openStartTag("include");
    girXml.writeAttr("name", "GObject");
    girXml.writeAttr("version", "2.0");
    girXml.closeStartTag;
    girXml.writeEndTag;

    girXml.openStartTag("package");
    girXml.writeAttr("name", "gidgen-test");
    girXml.closeStartTag;
    girXml.writeEndTag;

    girXml.openStartTag("c:include");
    girXml.writeAttr("name", "gidgen-test.h");
    girXml.closeStartTag;
    girXml.writeEndTag;

    girXml.openStartTag("namespace");
    girXml.writeAttr("name", "GidgenTest");
    girXml.writeAttr("version", "1.0");
    girXml.writeAttr("shared-library", "libgidgen-test.so.0");
    girXml.writeAttr("c:identifier-prefixes", cIdentPrefix);
    girXml.writeAttr("c:symbol-prefixes", cSymPrefix);
    girXml.closeStartTag;
  }

  void generate()
  {
    genFuncParams;

    girXml.writeEndTag;  // namespace
    girXml.writeEndTag;  // repository
  }

  void write()
  {
    cCow.write;
    dCow.write;
    writeFile(buildPath(girPath, "GidgenTest-1.0.gir"), girAp[]);
  }

  void genFuncParams()
  {
    foreach (kind; EnumMembers!TypeKind)
    {
      final switch (kind) with(TypeKind)
      {
        case Unknown:
          break;
        case Basic:
          dCow ~= ["", "@(\"FuncBasicParams\")", "unittest", "{"];

          dstring type, cType, dFuncName, funcName, cFuncName, decl;

          static foreach (i, basic; BasicTypes)
          { // Basic input -> return parameter tests
            type = basic.stringof.to!dstring;
            cType = gBasicTypes[i].to!dstring;
            dFuncName = "func"d ~ type.capitalize ~ "InReturn"d;
            funcName = "func_"d ~ type ~ "_in_return"d;
            cFuncName = cSymPrefix.to!dstring ~ "_"d ~ funcName;
            decl = cType ~ " "d ~ cFuncName ~ "("d ~ cType ~ " param)"d;

            cCow ~= ["", decl.to!dstring, "{", "return param;", "}"];

            writeFunc(GirFunc(name: funcName.to!string, cName: cFuncName.to!string, funcType: FuncType.Function,
              retType: cType.to!string, cRetType: cType.to!string,
              params: [GirParam(name: "param", type: cType.to!string, cType: cType.to!string)]));

            static if (isFloatingPoint!basic)
              dCow ~= "assert(" ~ dFuncName ~ "(cast(" ~ type ~ ")-" ~ basic.max.to!dstring ~ ") == cast(" ~ type ~ ")-"
                ~ basic.max.to!dstring ~ ");";
            else
              dCow ~= "assert(" ~ dFuncName ~ "(cast(" ~ type ~ ")" ~ basic.min.to!dstring ~ ") == cast(" ~ type ~ ")"
                ~ basic.min.to!dstring ~ ");";

            dCow ~= "assert(" ~ dFuncName ~ "(cast(" ~ type ~ ")" ~ basic.max.to!dstring ~ ") == cast(" ~ type ~ ")"
              ~ basic.max.to!dstring ~ ");";

            // Basic input -> output parameter tests
            dFuncName = "func" ~ type.capitalize ~ "InToOut";
            funcName = "func_"d ~ type ~ "_in_to_out";
            cFuncName = cSymPrefix.to!dstring ~ "_" ~ funcName;
            decl = "void " ~ cFuncName ~ "(" ~ cType ~ " param, " ~ cType ~ "* out_param)";

            cCow ~= ["", decl.to!dstring, "{", "*out_param = param;", "}"];

            writeFunc(GirFunc(name: funcName.to!string, cName: cFuncName.to!string, funcType: FuncType.Function,
              params: [
                GirParam(name: "param", type: cType.to!string, cType: cType.to!string),
                GirParam(name: "out_param", type: cType.to!string, cType: cType.to!string ~ "*",
                  direction: ParamDirection.Out)
              ]));

            dCow ~= ["{"d, type ~ " outVal;"];

            static if (isFloatingPoint!basic)
              dCow ~= [dFuncName ~ "(cast(" ~ type ~ ")-" ~ basic.max.to!dstring ~ ", outVal);",
                "assert(outVal == cast(" ~ type ~ ")-" ~ basic.max.to!dstring ~ ");"];
            else
              dCow ~= [dFuncName ~ "(cast(" ~ type ~ ")" ~ basic.min.to!dstring ~ ", outVal);",
                "assert(outVal == cast(" ~ type ~ ")" ~ basic.min.to!dstring ~ ");"];

            dCow ~= [dFuncName ~ "(cast(" ~ type ~ ")" ~ basic.max.to!dstring ~ ", outVal);",
              "assert(outVal == cast(" ~ type ~ ")" ~ basic.max.to!dstring ~ ");"];

            dCow ~= "}"d;

            // Basic inout parameter tests
            dFuncName = "func" ~ type.capitalize ~ "Inout";
            funcName = "func_"d ~ type ~ "_inout";
            cFuncName = cSymPrefix.to!dstring ~ "_" ~ funcName;
            decl = "void " ~ cFuncName ~ "(" ~ cType ~ "* param)";

            cCow ~= ["", decl.to!dstring, "{", "*param = !*param;", "}"];

            writeFunc(GirFunc(name: funcName.to!string, cName: cFuncName.to!string, funcType: FuncType.Function,
              params: [
                GirParam(name: "param", type: cType.to!string, cType: cType.to!string ~ "*",
                  direction: ParamDirection.InOut)
              ]));

            dCow ~= ["{"d, type ~ " val = cast(" ~ type ~ ")0;", dFuncName ~ "(val);",
              "assert(val == cast(" ~ type ~ ")!0);", "}"d];
          }

          dCow ~= "}";
          break;
        case String:
          dCow ~= ["", "@(\"FuncStringParams\")", "unittest", "{"];

          // String in -> return no transfer
          auto dFuncName = "funcStringInReturnTransNone"d;
          auto funcName = "func_string_in_return_trans_none"d;
          auto cFuncName = cSymPrefix.to!dstring ~ "_" ~ funcName;
          auto decl = "const char* " ~ cFuncName ~ "(const char* param)";

          cCow ~= ["", decl.to!dstring, "{", "return param;", "}"];

          writeFunc(GirFunc(name: funcName.to!string, cName: cFuncName.to!string, funcType: FuncType.Function,
            retType: "utf8", cRetType: "const char *", retOwnership: Ownership.None,
            params: [GirParam(name: "param", type: "utf8", cType: "const char *", ownership: Ownership.None)]));

          // memCapture
          dCow ~= "assert("d ~ dFuncName ~ "(\"blah123\") == \"blah123\");"d;
          // memAssertNone

          // String in -> return both transfer full
          dFuncName = "funcStringInReturnTransFull"d;
          funcName = "func_string_in_return_trans_full"d;
          cFuncName = cSymPrefix.to!dstring ~ "_" ~ funcName;
          decl = "char* " ~ cFuncName ~ "(char* param)";

          cCow ~= ["", decl.to!dstring, "{", "char* ret = g_strdup(param);", "g_free(param);", "return ret;", "}"];

          writeFunc(GirFunc(name: funcName.to!string, cName: cFuncName.to!string, funcType: FuncType.Function,
            retType: "utf8", cRetType: "char *", retOwnership: Ownership.Full,
            params: [GirParam(name: "param", type: "utf8", cType: "char *", ownership: Ownership.Full)]));

          // memCapture
          dCow ~= "assert(" ~ dFuncName ~ "(\"blah123\") == \"blah123\");";
          // memAssertAllocFree

          // String in -> out no transfer
          dFuncName = "funcStringInToOutTransNone"d;
          funcName = "func_string_in_to_out_trans_none"d;
          cFuncName = cSymPrefix.to!dstring ~ "_" ~ funcName;
          decl = "void " ~ cFuncName ~ "(const char* param, const char** outParam)";

          cCow ~= ["", decl.to!dstring, "{", "*outParam = param;", "}"];

          writeFunc(GirFunc(name: funcName.to!string, cName: cFuncName.to!string, funcType: FuncType.Function,
            params: [
              GirParam(name: "param", type: "utf8", cType: "const char *", ownership: Ownership.None),
              GirParam(name: "out_param", type: "utf8", cType: "const char **", direction: ParamDirection.Out,
                ownership: Ownership.None)
            ]));

          // memCapture
          dCow ~= ["{", "string outVal;", dFuncName ~ "(\"blah123\", outVal);", "assert(outVal == \"blah123\");", "}"];
          // memAssertNone

          // String in -> out both transfer full
          dFuncName = "funcStringInToOutTransFull"d;
          funcName = "func_string_in_to_out_trans_full"d;
          cFuncName = cSymPrefix.to!dstring ~ "_" ~ funcName;
          decl = "void " ~ cFuncName ~ "(char* param, char** out_param)";

          cCow ~= ["", decl.to!dstring, "{", "*out_param = g_strdup(param);", "g_free(param);", "}"];

          writeFunc(GirFunc(name: funcName.to!string, cName: cFuncName.to!string, funcType: FuncType.Function,
            params: [
              GirParam(name: "param", type: "utf8", cType: "char *", ownership: Ownership.Full),
              GirParam(name: "out_param", type: "utf8", cType: "char **", direction: ParamDirection.Out,
                ownership: Ownership.Full)
            ]));

          // memCapture
          dCow ~= ["{", "string outVal;", dFuncName ~ "(\"blah123\", outVal);", "assert(outVal == \"blah123\");", "}"];
          // memAssertAllocFree

          dCow ~= "}";
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

  void writeFunc(GirFunc girFunc)
  {
    girXml.openStartTag(FuncTypeValues[cast(int)girFunc.funcType].to!string);
    girXml.writeAttr("name", girFunc.name);
    girXml.writeAttr("c:identifier", girFunc.cName);
    girXml.closeStartTag;

    girXml.openStartTag("return-value");

    if (girFunc.retOwnership != Ownership.Unset)
      girXml.writeAttr("transfer-ownership", girFunc.retOwnership == Ownership.Full ? "full" : "none");

    girXml.closeStartTag;
    girXml.openStartTag("type");
    girXml.writeAttr("name", girFunc.retType.length > 0 ? girFunc.retType : "none");
    girXml.writeAttr("c:type", girFunc.cRetType.length > 0 ? girFunc.cRetType
      : (girFunc.retType.length > 0 ? girFunc.retType : "void"));
    girXml.closeStartTag;
    girXml.writeEndTag; // type
    girXml.writeEndTag; // return-value

    girXml.openStartTag("parameters");
    girXml.closeStartTag;

    foreach (param; girFunc.params)
    {
      girXml.openStartTag("parameter");
      girXml.writeAttr("name", param.name);

      if (param.ownership != Ownership.Unset)
        girXml.writeAttr("transfer-ownership", param.ownership == Ownership.Full ? "full" : "none");

      if (param.direction != ParamDirection.In)
        girXml.writeAttr("direction", param.direction == ParamDirection.Out ? "out" : "inout");

      girXml.closeStartTag;
      girXml.openStartTag("type");
      girXml.writeAttr("name", param.type);
      girXml.writeAttr("c:type", param.cType.length > 0 ? param.cType : param.type);
      girXml.closeStartTag;
      girXml.writeEndTag; // type
      girXml.writeEndTag; // param
    }

    girXml.writeEndTag; // parameters
    girXml.writeEndTag; // function
  }

private:
  CodeWriter cCow; // C source appender
  CodeWriter dCow; // D appender
  Appender!string girAp; // GIR XML file appender
  XMLWriter!(Appender!string) girXml; // XML writer
}

int main(string[] argv)
{
  auto generator = new Generator;
  generator.generate;
  generator.write;

  return 0;
}
