/**
 * matrix-gen: Generates the GidgenMatrix test library functions and the D test manifest.
 *
 * Every type in type_table.d is crossed with every position in position_table.d (and each supported transfer and
 * nullability) to produce a matrix "cell". For each cell a C function is generated which verifies a received value
 * and/or produces a known value, and a manifest entry is generated which the generic D unittest in
 * gidgen-test/matrix_test.d uses to call the gidgen generated binding of the function.
 *
 * Usage: matrix-gen <C output directory> <D manifest output file> <gidgen definitions file>
 */
module app;

import std.array : appender, array, join, replace;
import std.conv : to;
import std.file : write;
import std.format : format;
import std.path : buildPath;
import std.string : indexOf, split;
import std.algorithm : endsWith, startsWith;
import std.uni : toLower, toUpper;

import aggregates;
import position_table;
import tmpl;
import type_table;

/// A matrix cell: a type in a position with a transfer and nullability
struct Cell
{
  const(TypeEntry)* type;
  const(Position)* pos;
  string transfer; /// "none" or "full"
  bool nullable;

  /// C function name: gidgen_matrix_<position>_<type>[_<transfer>][_nullable]
  /// (position first, since g-ir-scanner would turn gidgen_matrix_<type>_* functions into methods of the type, which
  /// is used for method positions: gidgen_matrix_<type>_<position>)
  string cFunc() const
  {
    if (pos.method)
      return "gidgen_matrix_" ~ type.id ~ "_" ~ pos.id;

    if (pos.fieldsMethod)
      return "gidgen_matrix_fields_" ~ pos.id ~ "_" ~ type.id;

    auto s = "gidgen_matrix_" ~ pos.id ~ "_" ~ type.id;

    if (type.pointer)
      s ~= "_" ~ transfer;

    return nullable ? s ~ "_nullable" : s;
  }

  /// D function name as created by gidgen (camelCase of the C name without the namespace prefix)
  string dFunc() const
  {
    auto parts = cFunc[(pos.fieldsMethod ? "gidgen_matrix_fields_" : "gidgen_matrix_").length .. $].split("_");
    foreach (ref p; parts[1 .. $])
      p = p[0 .. 1].toUpper ~ p[1 .. $];
    return parts.join;
  }
}

/// Convert a C name with '_' or '-' separators to camelCase (or CamelCase if upperFirst)
string camel(string name, bool upperFirst = false)
{
  import std.array : split;
  import std.regex : regex, splitter;
  string s;
  foreach (i, p; name.splitter(regex("[-_]")).array)
    s ~= (i > 0 || upperFirst) && p.length ? p[0 .. 1].toUpper ~ p[1 .. $] : p;
  return s;
}

/// Generate all matrix cells
Cell[] allCells()
{
  Cell[] cells;

  foreach (ref t; types)
    foreach (ref p; positions)
    {
      if (!p.supports(t))
        continue;

      foreach (transfer; p.transfers(t))
      {
        if (isUnsupported(p.id, t, transfer))
          continue;

        cells ~= Cell(&t, &p, transfer, false);

        if (p.nullable && t.pointer && t.nullable)
          cells ~= Cell(&t, &p, transfer, true);
      }
    }

  return cells;
}

/// Compute the template variables for a cell (see position_table.d for their descriptions)
string[string] cellVars(const Cell c)
{
  auto t = c.type;
  bool full = c.transfer == "full";

  // C value helpers
  bool hasLen = t.lenType.length > 0;
  auto lenName = c.pos.lenName.length ? fill(c.pos.lenName, ["id": t.id]) : "n";
  auto n = c.pos.lenLocal || !c.pos.lenPtr ? lenName : "(*" ~ lenName ~ ")"; // Length value expression
  string make(Role role) => fill(full ? t.cMake : t.makeNone, ["v": t.cVals[role], "n": n]);
  string eq(string x, Role role) => fill(t.cEq, ["x": x, "v": "(" ~ t.cVals[role] ~ ")", "n": n]);
  string orNull(string expr) => c.nullable ? "is_null ? " ~ (hasLen ? "(" ~ n ~ " = 0, NULL)" : "NULL") ~ " : " ~ expr
    : expr;

  string[] annots;
  if (c.pos.direction.length) annots ~= c.pos.direction;
  if (t.pointer && c.pos.id != "out_ca") annots ~= "(transfer " ~ c.transfer ~ ")";
  if (c.nullable) annots ~= c.pos.id == "out" ? "(optional) (nullable)" : "(nullable)";
  if (t.annot.length) annots ~= t.annot;
  if (hasLen) annots ~= "(array length=" ~ lenName ~ ")";

  string[string] v;
  v["func"] = c.cFunc;
  v["type"] = full || !t.pointer ? t.cType : t.constType;
  v["annots"] = annots.length ? annots.join(" ") ~ ": " : "";
  v["nullArg"] = c.nullable ? "gboolean is_null, " : "";
  v["nullDoc"] = c.nullable ? " * @is_null: %TRUE to pass or return %NULL\n" : "";
  // Length parameter of C arrays with a separate length (see Position.lenName)
  auto lenParam = t.lenType ~ (c.pos.lenPtr ? " *" : " ") ~ lenName;
  v["lenB"] = hasLen && t.lenFirst ? lenParam ~ ", " : "";
  v["lenA"] = hasLen && !t.lenFirst ? ", " ~ lenParam : "";
  v["lenDoc"] = hasLen ? " * @" ~ lenName ~ ": " ~ (c.pos.lenDir.length ? c.pos.lenDir ~ ": " : "") ~ "Array length\n" : "";
  v["lenDecl"] = hasLen && c.pos.lenLocal ? "  " ~ t.lenType ~ " " ~ lenName ~ " = 0;\n" : "";
  auto lenArg = (c.pos.lenPtr ? "&" : "") ~ lenName;
  v["lenCallB"] = hasLen && t.lenFirst ? lenArg ~ ", " : "";
  v["lenCallA"] = hasLen && !t.lenFirst ? ", " ~ lenArg : "";
  v["lenParamB"] = hasLen ? lenParam ~ ", " : ""; // Length parameter preceding the others (no array parameter)
  v["lenArgB"] = hasLen ? lenArg ~ ", " : "";
  v["n"] = n;
  v["lenType"] = t.lenType;
  v["nRet"] = "(" ~ t.cVals[Role.Ret] ~ ")->n"; // Array length of the values (C arrays with a separate length)
  v["nOut"] = "(" ~ t.cVals[Role.Out] ~ ")->n";
  v["fixedElem"] = t.fixedElem;
  v["embed"] = t.embed;
  v["borrow"] = t.borrow;
  v["dId"] = camel(t.id);
  v["vIn"] = t.cVals[Role.In];
  v["vOut"] = t.cVals[Role.Out];
  v["vRet"] = t.cVals[Role.Ret];
  string[] retArgs;
  if (c.nullable) retArgs ~= "gboolean is_null";
  if (hasLen) retArgs ~= lenParam;
  v["retArgs"] = retArgs.length ? retArgs.join(", ") : "void";

  auto val = fill(c.pos.val, ["id": t.id]);
  auto cellName = c.cFunc["gidgen_matrix_".length .. $];
  v["val"] = val;
  v["id"] = t.id;
  v["ID"] = t.id.toUpper;
  v["cType"] = t.cType;
  v["cFree"] = t.cFree;
  v["freeVal"] = t.pointer ? format!"  g_clear_pointer (&%s, %s);\n"(val, t.cFree) : "";
  v["makeOutFull"] = fill(t.cMake, ["v": t.cVals[Role.Out], "n": n]);
  v["copyVal"] = t.pointer ? format!"%s ? %s : NULL"(val, fill(t.cCopy, ["x": val])) : val;
  v["gtype"] = t.gtype;
  v["gvalue"] = t.gvalue;
  v["gvGet"] = (t.pointer ? "dup_" : "get_") ~ t.gvalue;
  v["pspec"] = fill(t.pspec, ["name": `"` ~ t.id ~ `-prop"`]);
  v["pspecAcc"] = fill(t.pspec, ["name": `"` ~ t.id ~ `-acc"`]);
  // Property flag variants (read-only, write-only and construct-only)
  foreach (suffix, flags; ["Ro": "G_PARAM_READABLE", "Wo": "G_PARAM_WRITABLE",
      "Co": "G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY"])
    v["pspec" ~ suffix] = fill(t.pspec, ["name": `"` ~ t.id ~ "-" ~ suffix.toLower ~ `"`])
      .replace("G_PARAM_READWRITE", flags);
  // Annotations of a transfer none value which is nullable if the type is (independent of the cell nullability)
  v["accAnnots"] = ((t.pointer ? "(transfer none) " : "") ~ (t.pointer && t.nullable ? "(nullable) " : "")
    ~ (t.annot.length ? t.annot ~ " " : ""));
  if (v["accAnnots"].length) v["accAnnots"] = v["accAnnots"][0 .. $ - 1] ~ ": ";
  v["cbType"] = "GidgenMatrix" ~ camel(cellName, true) ~ "Func";
  v["cbGirName"] = camel(cellName, true) ~ "Func"; // GIR name strips namespace prefix
  v["cell"] = cellName;
  v["sigName"] = cellName.replace("_", "-");
  v["SIG"] = "SIG_" ~ cellName.toUpper;
  v["nullAnnot"] = c.nullable ? "(nullable): " : "";
  v["sigGtype"] = t.signalGtype;
  auto sigAnnot = t.sigAnnot.length ? t.sigAnnot : t.annot;
  v["sigAnnots"] = (sigAnnot.length ? sigAnnot ~ " " : "") ~ (c.nullable ? "(nullable) " : "");
  v["sigRetAnnots"] = (t.pointer ? "(transfer " ~ c.transfer ~ ") " : "") ~ v["sigAnnots"];
  if (v["sigRetAnnots"].length && !v["sigRetAnnots"].endsWith(": "))
    v["sigRetAnnots"] = v["sigRetAnnots"][0 .. $ - 1] ~ ": ";
  if (v["sigAnnots"].length) v["sigAnnots"] = v["sigAnnots"][0 .. $ - 1] ~ ": ";
  v["checkIn"] = c.nullable ? format!"is_null ? %s == NULL : (%s != NULL && %s)"(val, val, eq(val, Role.In))
    : eq(val, Role.In);
  v["checkOut"] = c.nullable ? format!"is_null ? %s == NULL : (%s != NULL && %s)"(val, val, eq(val, Role.Out))
    : eq(val, Role.Out);
  v["eqIn"] = eq(val, Role.In);
  v["isNull"] = t.pointer ? val ~ " == NULL" : "FALSE";
  v["keptSame"] = !t.pointer || t.own == Own.Ref ? " && " ~ val ~ " == orig" : "";
  v["release"] = !full ? "" : t.cFreeN.length ? format!"  %s;\n  %s = NULL;\n"(fill(t.cFreeN, ["x": val, "n": n]), val)
    : format!"  g_clear_pointer (&%s, %s);\n"(val, t.cFree);
  v["makeOut"] = orNull(make(Role.Out));
  v["makeRet"] = orNull(make(Role.Ret));
  v["fillOutVal"] = fill(t.cFill, ["x": val, "v": t.cVals[Role.Out], "n": n]);
  v["fillOut"] = fill(t.cFill.length ? t.cFill : "", ["x": "value", "v": t.cVals[Role.Out], "n": n]);
  return v;
}

/// The common C function template, filled with the parts from the position templates
enum functionTemplate = `/**
 * ${func}:
${paramDocs} *
 * Matrix cell: type ${typeId}, position ${posId}, transfer ${transfer}${nullableDesc}.
 *
${returnDoc} */
${ret}
${func} (${args})
{
${body}}

`;

private bool[string] sectionDone; /// Section texts already added

/// Generate the C function definition and prototype for a cell and append its section texts
void genCFunc(const Cell c, ref string def, ref string proto, ref string[string] sections)
{
  auto v = cellVars(c);
  string part(string tmplText) => fill(tmplText, v);

  auto fv = v.dup;
  fv["paramDocs"] = part(c.pos.paramDocs);
  fv["returnDoc"] = part(c.pos.returnDoc);
  fv["ret"] = part(c.pos.ret);
  fv["args"] = part(c.pos.args);
  fv["body"] = part(c.pos.body);
  fv["typeId"] = c.type.id;
  fv["posId"] = c.pos.id;
  fv["transfer"] = c.transfer;
  fv["nullableDesc"] = c.nullable ? ", nullable" : "";

  foreach (sec; c.pos.sections)
  {
    import std.algorithm : canFind;
    import std.exception : enforce;
    enforce(sectionNames.canFind(sec.name), "Unknown section " ~ sec.name);
    auto text = part(sec.text);
    if (text !in sectionDone) // Cells sharing a member (nullable variants for example) add the same text only once
    {
      sections[sec.name] ~= text;
      sectionDone[text] = true;
    }
  }

  def = fill(functionTemplate, fv);
  def = def.replace("* *", "**");
  proto = format!"%s %s (%s);\n"(fv["ret"], c.cFunc, fv["args"]).replace("* *", "**");
}

/// D manifest entry for a cell (D expressions are pre-filled with the type's D values, `a` is the checked value)
string genManifestEntry(const Cell c)
{
  auto t = c.type;
  string dMake(Role r) => fill(t.dMake, ["v": t.dVals[r]]);
  string dEq(Role r) => fill(t.dEq, ["a": "a", "v": "(" ~ t.dVals[r] ~ ")"]);

  auto member = fill(c.pos.member, cellVars(c));
  // D function of the type's non-nullable transfer none "in" cell (its last parameter has the D type of the value)
  import std.algorithm : find;
  auto inPos = positions[].find!(p => p.id == "in");
  auto inFunc = Cell(c.type, &inPos[0], "none", false).dFunc;
  auto dMember = member.length ? (member.indexOf('-') >= 0 && c.pos.id.startsWith("sig") ? "connect"
    ~ camel(member, true) : camel(member)) : "";

  return format!"  MatrixCell(%(%s, %), %s, %(%s, %), %s),\n"([c.dFunc, c.pos.id, c.transfer], c.nullable,
    [dMake(Role.In), dEq(Role.In), dEq(Role.Out), dEq(Role.Ret), dMember, dMake(Role.Ret), dMake(Role.Out), inFunc,
    t.counter, t.own.to!string], t.fixedSize);
}

enum cHeaderStart = `/* Generated by matrix-gen from the type and position tables, do not edit */

#pragma once

#include "gidgen-matrix-support.h"

G_BEGIN_DECLS

`;

enum cSourceStart = `/* Generated by matrix-gen from the type and position tables, do not edit */

#include "gidgen-matrix-gen.h"
#include "gidgen-matrix-private.h"

`;

enum manifestStart = `/// Generated by matrix-gen from the type and position tables, do not edit
module matrix_manifest;

/// A matrix cell: a generated function testing a type in a position
struct MatrixCell
{
  string func; /// D function name in gidgenmatrix.global (not used for method positions, see member)
  string pos; /// Position ID
  string transfer; /// Ownership transfer ("none" or "full")
  bool nullable; /// true if the function has a leading is_null parameter and also accepts/returns null
  string makeIn; /// D expression creating the In value (T is the D parameter type)
  string eqIn; /// D expression which is true if "a" is the In value
  string eqOut; /// D expression which is true if "a" is the Out value
  string eqRet; /// D expression which is true if "a" is the Ret value
  string member; /// D name of the tested field, property or signal connect method (empty for other positions)
  string makeRet; /// D expression creating the Ret value (values returned from D to C)
  string makeOut; /// D expression creating the Out value (sizes caller allocated arrays)
  string inFunc; /// D function of the type's "in" cell (its last parameter type is the D value type)
  string counter; /// Live counter (Counter member) of the type's C instances (empty if not counted)
  string own; /// How D owns received C instances ("None", "Ref", "Copy" or "Value", see Own in type_table.d)
  int fixedSize; /// Size of a fixed size C array type (0 otherwise)
}

/// All matrix cells
enum MatrixCell[] matrixCells = [
`;

int main(string[] args)
{
  if (args.length != 4)
  {
    import std.stdio : stderr;
    stderr.writeln("Usage: matrix-gen <C output directory> <D manifest output file> <gidgen definitions file>");
    return 1;
  }

  auto header = appender!string(cHeaderStart);
  auto source = appender!string(cSourceStart);
  auto manifest = appender!string(manifestStart);

  auto cells = allCells;
  string[string] sections;
  foreach (name; sectionNames)
    sections[name] = "";

  auto protos = appender!string;
  auto defs = appender!string;

  foreach (c; cells)
  {
    string def, proto;
    genCFunc(c, def, proto, sections);
    protos ~= proto;
    defs ~= def;
    manifest ~= genManifestEntry(c);
  }

  header ~= fill(headerTemplate, sections);
  header ~= "/* Cell functions */\n\n";
  header ~= protos[];
  source ~= fill(sourceTemplate, sections);
  source ~= defs[];
  header ~= "\nG_END_DECLS\n";
  manifest ~= "];\n";

  write(buildPath(args[1], "gidgen-matrix-gen.h"), header[]);
  write(buildPath(args[1], "gidgen-matrix-gen.c"), source[]);
  write(args[2], manifest[]);
  foreach (ref t; types)
    sections["defs"] ~= t.defs;

  write(args[3], fill(defsTemplate, sections));

  import std.stdio : writefln;
  writefln("matrix-gen: %d cells (%d types, %d positions)", cells.length, types.length, positions.length);
  return 0;
}
