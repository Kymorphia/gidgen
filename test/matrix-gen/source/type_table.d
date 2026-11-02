/**
 * The matrix TYPE TABLE.
 *
 * Each entry describes one type kind: its C type, how to make, compare and free a known value in C and how to make
 * and compare it in D. Every type is crossed with every position in position_table.d which it supports.
 *
 * To add a new type, add an entry to `types` below (and if it is a new built-in test type, add it to
 * gidgen-matrix/gidgen-matrix-support.h/.c).
 *
 * Template placeholders:
 *   C: ${v} = raw C value, ${x} = C variable/expression being checked or filled
 *   D: ${v} = raw D value, ${a} = D variable being checked, T = D type of the parameter or return value (from
 *      reflection on the generated binding, so the D type name never needs to be specified)
 */
module type_table;

/// Value roles, each role uses a distinct value so that mixed up values are detected
enum Role { In, Out, Ret }

/// How D owns a C instance it receives (selects the generic live and reference count checks in matrix_test.d)
enum Own
{
  None, /// Not an instance type (or not counted)
  Ref, /// Reference counted: D takes a full reference or adds one for a borrowed instance
  Copy, /// Copyable record bound as a D class: D takes a full instance or copies a borrowed one
  Borrow, /// Opaque record: D takes a full instance or borrows a transfer none one (OpaqueWrap does not copy)
  Value, /// Record bound as a D value struct: D copies the C contents and frees a full instance
}

/// A type table entry
struct TypeEntry
{
  string id; /// Identifier used in generated function names (gidgen_matrix_<position>_<id>...)
  string cType; /// C type of an owned value (transfer full)
  string cConstType; /// C type of an unowned value (transfer none), defaults to cType
  string annot; /// Additional GIR annotation (for example "(type filename)")
  bool pointer; /// Pointer type: supports transfer full (see transferFull) and nullability
  string[3] cVals; /// Raw C values for each Role
  string[3] dVals; /// Raw D values for each Role (same values as cVals)
  string cMake = "${v}"; /// C expression for an owned value (transfer full)
  string cMakeNone; /// C expression for an unowned value (transfer none), defaults to cMake
  string cEq = "${x} == ${v}"; /// C expression which is true if ${x} equals ${v}
  string cFree; /// C free function for owned values (pointer types)
  string cFill; /// C statement to fill caller allocated ${x} with ${v} (empty if caller-allocates not supported)
  string dMake = "cast(T)(${v})"; /// D expression which creates a value from ${v}
  string dEq = "${a} == ${v}"; /// D expression which is true if ${a} equals ${v}
  bool nullable = true; /// Pointer type which can be null in D (false for boxed types bound as D value structs)
  string cCopy; /// C expression which copies ${x} (pointer types, used for record field copies)
  string gtype; /// C GType expression (empty if the type can't be a GObject property or signal value)
  string gvalue; /// GValue accessor suffix (g_value_get_<gvalue>, g_value_set_<gvalue>)
  string pspec; /// C GParamSpec constructor expression, ${name} = property name (empty if not a property type)
  bool container; /// Container type (GList, arrays, hash tables, ...)
  string sigGtype; /// Signal value GType if it differs from gtype (G_TYPE_POINTER for containers)
  string sigAnnot; /// Signal value annotation if it differs from annot (containers passed as pointers need a type)
  string lenType; /// C type of the separate length parameter of a C array (empty if none, ${n} = the length)
  bool lenFirst; /// The length parameter precedes the array parameter
  string cFreeN; /// C statement which frees owned array ${x} with length ${n} (C arrays without a terminator)
  string fixedElem; /// Element C type of fixed size arrays which can be inline record fields (empty otherwise)
  string counter; /// D member name of the type's live instance counter (empty if not counted)
  Own own; /// How D owns received C instances (see Own)
  bool transferFull = true; /// Pointer type which supports transfer full (false if C has no way to free it)
  string embed; /// C type of an embedded (non-pointer) record field of the type (empty if not embeddable)

  /// Set cFill (caller allocated values)
  TypeEntry withFill(string cFill) const
  {
    TypeEntry t = this;
    t.cFill = cFill;
    return t;
  }

  /// Use the generic D element helpers of matrix_test.d (elements created from and compared with integer values)
  TypeEntry withElems() const
  {
    TypeEntry t = this;
    t.dMake = "makeElems!T(${v})";
    t.dEq = "elemsEq(${a}, ${v})";
    return t;
  }

  string signalGtype() const { return sigGtype.length ? sigGtype : gtype; }

  string constType() const { return cConstType.length ? cConstType : cType; }
  string makeNone() const { return cMakeNone.length ? cMakeNone : cMake; }

  /// Set the GType, GValue accessor and GParamSpec of a type (pspecArgs are the GParamSpec constructor arguments
  /// following the name, nick and blurb and preceding the flags)
  TypeEntry g(string gtype, string gvalue, string pspecFunc, string pspecArgs) const
  {
    TypeEntry t = this;
    t.gtype = gtype;
    t.sigGtype = null; // Signals use the GType too
    t.gvalue = gvalue;
    t.pspec = "g_param_spec_" ~ pspecFunc ~ " (${name}, NULL, NULL, " ~ (pspecArgs.length ? pspecArgs ~ ", " : "")
      ~ "G_PARAM_READWRITE)";
    return t;
  }
}

/// Basic (non-pointer) type with identical C and D values
private TypeEntry basic(string id, string cType, string[3] vals)
{
  return TypeEntry(id, cType, null, null, false, vals, vals);
}

/// Basic type with differing C and D value spelling
private TypeEntry basic(string id, string cType, string[3] cVals, string[3] dVals)
{
  return TypeEntry(id, cType, null, null, false, cVals, dVals);
}

/// String types (utf8 and filename)
private TypeEntry str(string id, string annot)
{
  TypeEntry t = {id: id, cType: "gchar *", cConstType: "const gchar *", annot: annot, pointer: true,
    cVals: [`"in-` ~ id ~ `"`, `"out-` ~ id ~ `"`, `"ret-` ~ id ~ `"`],
    cMake: "g_strdup (${v})", cMakeNone: "${v}", cEq: "g_strcmp0 (${x}, ${v}) == 0", cFree: "g_free",
    cCopy: "g_strdup (${x})", dMake: "${v}"};
  t.dVals = t.cVals;
  return t;
}

/**
 * Container type with elements created by the container helpers in gidgen-matrix-private.h.
 * Params:
 *   id = Type id
 *   cType = C type
 *   annot = Element type or array annotation
 *   helper = Container helper prefix (gm_<helper>_new/eq/free_<kind>)
 *   kind = Element kind (INT, STR or OBJ)
 *   elems = C element values (string literals or integers) for each role (hash tables: key/value pairs)
 *   dVals = D values for each role (array or associative array literals, object elements are their integer values)
 */
private TypeEntry container(string id, string cType, string annot, string helper, string kind, string[][3] elems,
  string[3] dVals)
{
  import std.array : join;
  import std.conv : to;
  import std.uni : toLower;

  TypeEntry t = {id: id, cType: cType, annot: annot, pointer: true, nullable: false, container: true, dVals: dVals};
  foreach (i, e; elems)
  {
    string[] casts;
    foreach (x; e)
      casts ~= "(gintptr)" ~ x;
    t.cVals[i] = "&(const GmVals){ " ~ e.length.to!string ~ ", (const gintptr[]){ " ~ casts.join(", ") ~ " } }";
  }

  auto free = "gm_" ~ helper ~ "_free_" ~ kind.toLower;
  t.cMake = "gm_" ~ helper ~ "_new (GM_" ~ kind ~ ", ${v})";
  t.cMakeNone = "KEEP (" ~ t.cMake ~ ", " ~ free ~ ")";
  t.cEq = "gm_" ~ helper ~ "_eq (GM_" ~ kind ~ ", ${x}, ${v})";
  t.cFree = free;
  t.sigGtype = "G_TYPE_POINTER"; // Containers are passed as pointers in signals (typed by the GIR annotations)

  // Signal pointer values need a complete type annotation, for example "(type GLib.List(utf8))"
  enum glibNames = ["list": "List", "slist": "SList", "ptrarray": "PtrArray", "garray": "Array", "hash": "HashTable"];
  import std.string : replace;
  if (auto name = helper in glibNames)
    t.sigAnnot = "(type GLib." ~ *name ~ "(" ~ annot["(element-type ".length .. $ - 1].replace(" ", ",") ~ "))";
  else // Zero terminated array
    t.sigAnnot = annot ~ (kind == "STR" ? " (element-type utf8)" : " (element-type GidgenMatrixObj)");

  if (kind == "STR" || kind == "INT")
    t.dMake = "${v}";
  else // Instance elements are created from and compared with their integer values
    t = t.withElems;

  return t;
}

/**
 * A C array with a separate length parameter (see container() for the arguments). The length type is lenType and
 * lenFirst places the length parameter before the array. Signals aren't supported (separate length parameter).
 */
private TypeEntry carray(string id, string cType, string kind, string[][3] elems, string[3] dVals, string lenType,
  bool lenFirst = false)
{
  import std.uni : toLower;

  auto t = container(id, cType, "", "carray", kind, elems, dVals);
  t.lenType = lenType;
  t.lenFirst = lenFirst;
  t.cMake = "(${n} = (${v})->n, gm_carray_new (GM_" ~ kind ~ ", ${v}))";
  t.cMakeNone = "KEEP (" ~ t.cMake ~ ", " ~ t.cFree ~ ")";
  t.cEq = "gm_carray_eq (GM_" ~ kind ~ ", ${x}, ${n}, ${v})";
  t.cFreeN = "gm_carray_freen_" ~ kind.toLower ~ " (${x}, ${n})";
  t.sigGtype = null;
  t.sigAnnot = null;
  return t;
}

/**
 * A fixed size C array (see container() for the arguments, all elems have fixedSize values). Signals aren't
 * supported (no fixed size annotation for signal pointer values). fixedElem is the element C type of arrays
 * which can be inline record fields (basic types only, the record is copied with an assignment).
 */
private TypeEntry fixedArray(string id, string cType, string kind, string[][3] elems, string[3] dVals, int fixedSize,
  string fixedElem = null)
{
  import std.conv : to;
  import std.uni : toLower;

  auto size = fixedSize.to!string;
  auto t = container(id, cType, "(array fixed-size=" ~ size ~ ")", "carray", kind, elems, dVals);
  t.cEq = "gm_carray_eq (GM_" ~ kind ~ ", ${x}, " ~ size ~ ", ${v})";
  t.cFreeN = "gm_carray_freen_" ~ kind.toLower ~ " (${x}, " ~ size ~ ")";
  t.sigGtype = null;
  t.sigAnnot = null;
  t.fixedElem = fixedElem;
  if (kind == "INT")
    t.cFill = "gm_carray_fill (GM_INT, ${x}, " ~ size ~ ", ${v})";
  return t;
}

/// Fixed size (3) element values for each role
private enum string[][3] fixedIntElems = [["101", "102", "103"], ["201", "202", "203"], ["301", "302", "303"]];
private enum string[3] fixedIntDVals = ["[101, 102, 103]", "[201, 202, 203]", "[301, 302, 303]"];
private enum string[][3] fixedStrElems = [[`"in-a"`, `"in-b"`, `"in-c"`], [`"out-a"`, `"out-b"`, `"out-c"`],
  [`"ret-a"`, `"ret-b"`, `"ret-c"`]];
private enum string[3] fixedStrDVals = [`["in-a", "in-b", "in-c"]`, `["out-a", "out-b", "out-c"]`,
  `["ret-a", "ret-b", "ret-c"]`];

/// String container element values for each role
private enum string[][3] strElems = [[`"in-a"`, `"in-b"`], [`"out-a"`], [`"ret-a"`, `"ret-b"`, `"ret-c"`]];
private enum string[3] strDVals = [`["in-a", "in-b"]`, `["out-a"]`, `["ret-a", "ret-b", "ret-c"]`];
/// Object (or integer) container element values for each role (In and Out have the same length so that arrays can
/// be modified in place)
private enum string[][3] intElems = [["101", "102"], ["201", "202"], ["301", "302", "303"]];
private enum string[3] intDVals = ["[101, 102]", "[201, 202]", "[301, 302, 303]"];
/// Hash table key/value pairs for each role
private enum string[][3] strHashElems = [[`"k1"`, `"in-a"`, `"k2"`, `"in-b"`], [`"k1"`, `"out-a"`],
  [`"k1"`, `"ret-a"`, `"k2"`, `"ret-b"`, `"k3"`, `"ret-c"`]];
private enum string[3] strHashDVals = [`["k1": "in-a", "k2": "in-b"]`, `["k1": "out-a"]`,
  `["k1": "ret-a", "k2": "ret-b", "k3": "ret-c"]`];
private enum string[][3] objHashElems = [[`"k1"`, "101", `"k2"`, "102"], [`"k1"`, "201"],
  [`"k1"`, "301", `"k2"`, "302", `"k3"`, "303"]];
private enum string[][3] intHashElems = [[`"k1"`, "101", `"k2"`, "102"], [`"k1"`, "201"],
  [`"k1"`, "301", `"k2"`, "302", `"k3"`, "303"]];
private enum string[3] intHashDVals = [`["k1": 101, "k2": 102]`, `["k1": 201]`, `["k1": 301, "k2": 302, "k3": 303]`];
/// Enum (GidgenMatrixFruit) array values for each role
private enum string[][3] fruitElems = [["1", "2"], ["2", "0"], ["0", "1", "2"]];
private enum string[3] fruitDVals = ["[1, 2]", "[2, 0]", "[0, 1, 2]"];
private enum string[3] objHashDVals = [`["k1": 101, "k2": 102]`, `["k1": 201]`, `["k1": 301, "k2": 302, "k3": 303]`];

/// The type table
immutable TypeEntry[] types = [
  basic("boolean", "gboolean", ["TRUE", "FALSE", "TRUE"], ["true", "false", "true"]).g("G_TYPE_BOOLEAN", "boolean", "boolean", "FALSE"),
  basic("int8", "gint8", ["-8", "7", "-100"]).g("G_TYPE_CHAR", "schar", "char", "G_MININT8, G_MAXINT8, 0"),
  basic("uint8", "guint8", ["8", "250", "100"]).g("G_TYPE_UCHAR", "uchar", "uchar", "0, G_MAXUINT8, 0"),
  basic("int16", "gint16", ["-16", "30000", "-30000"]),
  basic("uint16", "guint16", ["16", "60000", "1600"]),
  basic("int32", "gint32", ["-32", "2000000000", "-2000000000"]).g("G_TYPE_INT", "int", "int", "G_MININT, G_MAXINT, 0"),
  basic("uint32", "guint32", ["32", "4000000000U", "320"], ["32", "4000000000U", "320"]).g("G_TYPE_UINT", "uint", "uint", "0, G_MAXUINT, 0"),
  basic("int64", "gint64", ["G_GINT64_CONSTANT (-5000000000)", "G_GINT64_CONSTANT (64)",
    "G_GINT64_CONSTANT (7000000000)"], ["-5_000_000_000L", "64L", "7_000_000_000L"])
    .g("G_TYPE_INT64", "int64", "int64", "G_MININT64, G_MAXINT64, 0"),
  basic("uint64", "guint64", ["G_GUINT64_CONSTANT (10000000000)", "G_GUINT64_CONSTANT (64)",
    "G_GUINT64_CONSTANT (18000000000000000000)"], ["10_000_000_000UL", "64UL", "18_000_000_000_000_000_000UL"])
    .g("G_TYPE_UINT64", "uint64", "uint64", "0, G_MAXUINT64, 0"),
  basic("int", "gint", ["-1", "2", "-3"]).g("G_TYPE_INT", "int", "int", "G_MININT, G_MAXINT, 0"),
  basic("uint", "guint", ["1", "2", "3"]).g("G_TYPE_UINT", "uint", "uint", "0, G_MAXUINT, 0"),
  basic("long", "glong", ["-100000", "200000", "-300000"]).g("G_TYPE_LONG", "long", "long", "G_MINLONG, G_MAXLONG, 0"),
  basic("ulong", "gulong", ["100000", "200000", "300000"]).g("G_TYPE_ULONG", "ulong", "ulong", "0, G_MAXULONG, 0"),
  basic("size", "gsize", ["4096", "8192", "16384"]),
  basic("ssize", "gssize", ["-4096", "8192", "-16384"]),
  basic("float", "gfloat", ["1.5f", "-2.25f", "0.125f"]).g("G_TYPE_FLOAT", "float", "float", "-G_MAXFLOAT, G_MAXFLOAT, 0"),
  basic("double", "gdouble", ["1.5e100", "-2.25", "0.125e-100"]).g("G_TYPE_DOUBLE", "double", "double", "-G_MAXDOUBLE, G_MAXDOUBLE, 0"),
  basic("unichar", "gunichar", ["0x3A9", "0x1F600", "0x41"]).g("G_TYPE_UINT", "uint", "uint", "0, G_MAXUINT, 0"),
  // GType values (fundamental type IDs are fixed: G_TYPE_INT = 24, G_TYPE_STRING = 64, G_TYPE_BOOLEAN = 20)
  basic("gtype", "GType", ["G_TYPE_INT", "G_TYPE_STRING", "G_TYPE_BOOLEAN"], ["24", "64", "20"])
    .g("G_TYPE_GTYPE", "gtype", "gtype", "G_TYPE_NONE"),
  str("utf8", null).g("G_TYPE_STRING", "string", "string", "NULL"),
  str("filename", "(type filename)").g("G_TYPE_STRING", "string", "string", "NULL"),

  TypeEntry("fruit", "GidgenMatrixFruit", null, null, false,
    ["GIDGEN_MATRIX_FRUIT_BANANA", "GIDGEN_MATRIX_FRUIT_CHERRY", "GIDGEN_MATRIX_FRUIT_APPLE"],
    ["T.Banana", "T.Cherry", "T.Apple"])
    .g("GIDGEN_MATRIX_TYPE_FRUIT", "enum", "enum", "GIDGEN_MATRIX_TYPE_FRUIT, GIDGEN_MATRIX_FRUIT_APPLE"),
  TypeEntry("bits", "GidgenMatrixBits", null, null, false,
    ["GIDGEN_MATRIX_BITS_A | GIDGEN_MATRIX_BITS_C", "GIDGEN_MATRIX_BITS_B", "GIDGEN_MATRIX_BITS_NONE"],
    ["T.A | T.C", "T.B", "T.None"])
    .g("GIDGEN_MATRIX_TYPE_BITS", "flags", "flags", "GIDGEN_MATRIX_TYPE_BITS, GIDGEN_MATRIX_BITS_NONE"),

  { id: "obj", cType: "GidgenMatrixObj *", pointer: true, cVals: ["11", "22", "33"], dVals: ["11", "22", "33"],
    cMake: "gidgen_matrix_obj_new (${v})", cMakeNone: "KEEP (gidgen_matrix_obj_new (${v}), g_object_unref)",
    cEq: "GIDGEN_MATRIX_IS_OBJ (${x}) && gidgen_matrix_obj_get_value (${x}) == ${v}", cFree: "g_object_unref",
    cCopy: "g_object_ref (${x})", gtype: "GIDGEN_MATRIX_TYPE_OBJ", gvalue: "object",
    pspec: "g_param_spec_object (${name}, NULL, NULL, GIDGEN_MATRIX_TYPE_OBJ, G_PARAM_READWRITE)",
    dMake: "new T(${v})", dEq: "${a}.getValue == ${v}", counter: "Obj", own: Own.Ref },

  // Simple boxed record which gidgen binds as a D value struct (so it can't be null in D)
  { id: "box", cType: "GidgenMatrixBox *", pointer: true, nullable: false, cVals: ["44", "55", "66"], dVals: ["44", "55", "66"],
    cMake: "gidgen_matrix_box_new (${v})", cMakeNone: "KEEP (gidgen_matrix_box_new (${v}), gidgen_matrix_box_free)",
    cEq: "${x}->value == ${v}", cFree: "gidgen_matrix_box_free", cFill: "${x}->value = ${v}",
    cCopy: "gidgen_matrix_box_copy (${x})", gtype: "GIDGEN_MATRIX_TYPE_BOX", gvalue: "boxed",
    pspec: "g_param_spec_boxed (${name}, NULL, NULL, GIDGEN_MATRIX_TYPE_BOX, G_PARAM_READWRITE)",
    dMake: "T(${v})", dEq: "${a}.value == ${v}", counter: "Box", own: Own.Value },

  // Boxed record with a pointer field which gidgen binds as a D class
  { id: "rec", cType: "GidgenMatrixRec *", pointer: true, cVals: ["77", "88", "99"], dVals: ["77", "88", "99"],
    cMake: "gidgen_matrix_rec_new (${v})", cMakeNone: "KEEP (gidgen_matrix_rec_new (${v}), gidgen_matrix_rec_free)",
    cEq: "${x}->value == ${v} && g_strcmp0 (${x}->name, \"rec\") == 0", cFree: "gidgen_matrix_rec_free",
    cFill: "${x}->value = ${v}; g_free (${x}->name); ${x}->name = g_strdup (\"rec\")",
    cCopy: "gidgen_matrix_rec_copy (${x})", gtype: "GIDGEN_MATRIX_TYPE_REC", gvalue: "boxed",
    pspec: "g_param_spec_boxed (${name}, NULL, NULL, GIDGEN_MATRIX_TYPE_REC, G_PARAM_READWRITE)",
    dMake: "new T(${v})", dEq: "${a}.value == ${v} && ${a}.name == \"rec\"", counter: "Rec", own: Own.Copy },

  // Reference counted fundamental type (no GValue support, so no properties or signals)
  { id: "reffed", cType: "GidgenMatrixReffed *", pointer: true, cVals: ["12", "23", "34"], dVals: ["12", "23", "34"],
    cMake: "gidgen_matrix_reffed_new (${v})",
    cMakeNone: "KEEP (gidgen_matrix_reffed_new (${v}), gidgen_matrix_reffed_unref)",
    cEq: "gidgen_matrix_reffed_get_value (${x}) == ${v}", cFree: "gidgen_matrix_reffed_unref",
    cCopy: "gidgen_matrix_reffed_ref (${x})", dMake: "new T(${v})", dEq: "${a}.getValue == ${v}", counter: "Reffed",
    own: Own.Ref },

  // Plain C structure without a GType (or copy/free functions) which gidgen binds as a D value struct
  { id: "plain", cType: "GidgenMatrixPlain *", cConstType: "const GidgenMatrixPlain *", pointer: true, nullable: false,
    cVals: ["13", "24", "35"], dVals: ["13", "24", "35"], cMake: "gm_plain_new (${v})",
    cMakeNone: "KEEP (gm_plain_new (${v}), g_free)", cEq: "${x}->value == ${v}", cFree: "g_free",
    cFill: "${x}->value = ${v}", dMake: "T(${v})", dEq: "${a}.value == ${v}", embed: "GidgenMatrixPlain",
    transferFull: false }, // g-ir-scanner rejects non-const returns of bare structures

  // Plain C structure with a pointer field (no GType or copy/free functions) which gidgen binds as a D class
  { id: "plainwrap", cType: "GidgenMatrixPlainWrap *", cConstType: "const GidgenMatrixPlainWrap *", pointer: true,
    cVals: ["15", "26", "37"], dVals: ["15", "26", "37"], cMake: "gm_plainwrap_new (${v})",
    cMakeNone: "KEEP (gm_plainwrap_new (${v}), gm_plainwrap_free)",
    cEq: "${x}->value == ${v} && g_strcmp0 (${x}->name, \"pw\") == 0", cFree: "gm_plainwrap_free",
    cCopy: "gm_plainwrap_copy (${x})", dMake: "new T(${v}, \"pw\")", dEq: "${a}.value == ${v} && ${a}.name == \"pw\"",
    transferFull: false }, // g-ir-scanner rejects non-const returns of bare structures

  // Opaque record with copy and free functions and no GType (no properties or signals)
  { id: "opaque", cType: "GidgenMatrixOpaque *", pointer: true, cVals: ["16", "27", "38"], dVals: ["16", "27", "38"],
    cMake: "gidgen_matrix_opaque_new (${v})",
    cMakeNone: "KEEP (gidgen_matrix_opaque_new (${v}), gidgen_matrix_opaque_free)",
    cEq: "gidgen_matrix_opaque_get_value (${x}) == ${v}", cFree: "gidgen_matrix_opaque_free",
    cCopy: "gidgen_matrix_opaque_copy (${x})", dMake: "T.new_(${v})", dEq: "${a}.getValue == ${v}",
    counter: "Opaque", own: Own.Borrow },

  // GLib boxed types with special D bindings
  { id: "bytes", cType: "GBytes *", pointer: true, cVals: [`"in-bytes"`, `"out-bytes"`, `"ret-bytes"`],
    dVals: [`"in-bytes"`, `"out-bytes"`, `"ret-bytes"`], cMake: "gm_bytes_new (${v})",
    cMakeNone: "KEEP (gm_bytes_new (${v}), g_bytes_unref)", cEq: "gm_bytes_eq (${x}, ${v})", cFree: "g_bytes_unref",
    cCopy: "g_bytes_ref (${x})", gtype: "G_TYPE_BYTES", gvalue: "boxed",
    pspec: "g_param_spec_boxed (${name}, NULL, NULL, G_TYPE_BYTES, G_PARAM_READWRITE)",
    dMake: "new T(cast(ubyte[])${v}.dup)", dEq: "cast(string)${a}.getData == ${v}" },
  { id: "variant", cType: "GVariant *", pointer: true, cVals: ["14", "25", "36"], dVals: ["14", "25", "36"],
    cMake: "g_variant_ref_sink (g_variant_new_int32 (${v}))",
    cMakeNone: "KEEP (g_variant_ref_sink (g_variant_new_int32 (${v})), g_variant_unref)",
    cEq: "g_variant_is_of_type (${x}, G_VARIANT_TYPE_INT32) && g_variant_get_int32 (${x}) == ${v}",
    cFree: "g_variant_unref", cCopy: "g_variant_ref (${x})", gtype: "G_TYPE_VARIANT", gvalue: "variant",
    pspec: "g_param_spec_variant (${name}, NULL, NULL, G_VARIANT_TYPE_INT32, NULL, G_PARAM_READWRITE)",
    dMake: "new T(cast(int)${v})", dEq: "${a}.getInt32 == ${v}" },
  // GValue holding an int (no properties or signals of GValues)
  { id: "gvalue", cType: "GValue *", cConstType: "const GValue *", pointer: true, cVals: ["17", "28", "39"],
    dVals: ["17", "28", "39"], cMake: "gm_value_new (${v})", cMakeNone: "KEEP (gm_value_new (${v}), gm_value_free)",
    cEq: "G_VALUE_HOLDS_INT (${x}) && g_value_get_int (${x}) == ${v}", cFree: "gm_value_free",
    cFill: "gm_value_fill (${x}, ${v})", cCopy: "gm_value_copy (${x})", dMake: "new T(cast(int)${v})",
    dEq: "${a}.getInt == ${v}" },

  // Containers (pointer elements for GList, GSList, GPtrArray and GHashTable, integers for GArray)
  container("list_utf8", "GList *", "(element-type utf8)", "list", "STR", strElems, strDVals),
  container("list_obj", "GList *", "(element-type GidgenMatrixObj)", "list", "OBJ", intElems, intDVals),
  container("slist_utf8", "GSList *", "(element-type utf8)", "slist", "STR", strElems, strDVals),
  container("slist_obj", "GSList *", "(element-type GidgenMatrixObj)", "slist", "OBJ", intElems, intDVals),
  container("ptrarray_utf8", "GPtrArray *", "(element-type utf8)", "ptrarray", "STR", strElems, strDVals),
  container("ptrarray_obj", "GPtrArray *", "(element-type GidgenMatrixObj)", "ptrarray", "OBJ", intElems, intDVals),
  container("list_rec", "GList *", "(element-type GidgenMatrixRec)", "list", "REC", intElems, intDVals),
  container("ptrarray_rec", "GPtrArray *", "(element-type GidgenMatrixRec)", "ptrarray", "REC", intElems, intDVals),
  container("list_reffed", "GList *", "(element-type GidgenMatrixReffed)", "list", "REFFED", intElems, intDVals),
  container("ptrarray_reffed", "GPtrArray *", "(element-type GidgenMatrixReffed)", "ptrarray", "REFFED", intElems,
    intDVals),
  container("garray_int", "GArray *", "(element-type gint)", "garray", "INT", intElems, intDVals),
  container("strv", "gchar **", "(array zero-terminated=1)", "zt", "STR", strElems, strDVals)
    .g("G_TYPE_STRV", "boxed", "boxed", "G_TYPE_STRV"),
  container("ztarray_obj", "GidgenMatrixObj **", "(array zero-terminated=1)", "zt", "OBJ", intElems, intDVals),
  carray("carray_int", "gint *", "INT", intElems, intDVals, "gint").withFill("gm_carray_fill (GM_INT, ${x}, ${n}, ${v})"),
  carray("carray_int_lb", "gint *", "INT", intElems, intDVals, "gsize", true),
  carray("carray_utf8", "gchar **", "STR", strElems, strDVals, "gsize"),
  // Array of plain structures (the structure has a single gint field, so the integer array helpers are used)
  carray("carray_plain", "GidgenMatrixPlain *", "INT", intElems, intDVals, "gsize").withElems,
  carray("carray_fruit", "GidgenMatrixFruit *", "INT", fruitElems, fruitDVals, "gsize").withElems,
  carray("carray_obj", "GidgenMatrixObj **", "OBJ", intElems, intDVals, "guint", true),
  fixedArray("fixed_int", "gint *", "INT", fixedIntElems, fixedIntDVals, 3, "gint"),
  fixedArray("fixed_utf8", "gchar **", "STR", fixedStrElems, fixedStrDVals, 3),
  container("hash_utf8", "GHashTable *", "(element-type utf8 utf8)", "hash", "STR", strHashElems, strHashDVals),
  container("hash_obj", "GHashTable *", "(element-type utf8 GidgenMatrixObj)", "hash", "OBJ", objHashElems,
    objHashDVals),
  container("hash_int", "GHashTable *", "(element-type utf8 gint)", "hash", "INT", intHashElems, intHashDVals),
];
