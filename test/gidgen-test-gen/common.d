module common;

import std.meta : AliasSeq;

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
