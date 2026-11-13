/**
 * The matrix POSITION TABLE.
 *
 * A position is a place where a value of a type can appear (function parameter in/out/inout, return value, ...).
 * Each position has small C templates for the parts of the generated function. The common function template in
 * app.d assembles these parts, and the D test template (gidgen-test/matrix_test.d) has a matching `static if`
 * branch for each position id.
 *
 * Template placeholders available to position templates (computed by app.d from the type entry and cell):
 *   ${type}      C type of the value (const for transfer none pointers)
 *   ${annots}    GIR annotations for the value (direction, transfer, nullable, type), with a trailing space if any
 *   ${nullArg}   "gboolean is_null, " for nullable cells (empty otherwise)
 *   ${nullDoc}   Doc line for is_null for nullable cells (empty otherwise)
 *   ${retArgs}   Argument list for functions with no value parameter ("gboolean is_null" or "void")
 *   ${checkIn}   C expression checking that ${val} is the In role value (or NULL when is_null)
 *   ${checkOut}  C expression checking that ${val} is the Out role value (or NULL when is_null)
 *   ${eqIn}      C expression checking that ${val} is the In role value (not NULL)
 *   ${eqInPrefix} C expression checking that the first n_set elements of fixed size array ${val} are those of the In
 *                role value and the remaining ones are zero
 *   ${isNull}    C expression checking that ${val} is NULL (FALSE for non-pointer types)
 *   ${keptSame}  " && ${val} == orig" for values which are kept unchanged (non-pointer and reference counted
 *                types), empty for types which are copied
 *   ${release}   Statement freeing ${val} if ownership was transferred to the callee (empty otherwise)
 *   ${makeOut}   C expression creating the Out role value (or NULL when is_null)
 *   ${makeRet}   C expression creating the Ret role value (or NULL when is_null)
 *   ${fillOut}   C statement filling caller allocated memory with the Out role value
 *   ${val}       C expression of the value (the position's val, with ${id} filled)
 *   ${id}/${ID}  Type id (lower and upper case)
 *   ${cType}     C type of an owned value
 *   ${freeVal}   Statement freeing an owned pointer ${val} (empty for non-pointer types)
 *   ${makeOutFull} C expression creating an owned Out role value
 *   ${copyVal}   C expression copying ${val} (owned copy for pointer types, NULL safe)
 *   ${gtype}, ${gvalue}, ${gvGet}, ${pspec} GType, GValue accessor suffix, GValue getter for an owned value
 *                (get_ or dup_ prefixed) and GParamSpec constructor (with the name "${id}-prop")
 *   ${cbType}    C callback typedef name of the cell (GidgenMatrix<CamelCaseCell>Func)
 *   ${cell}      Cell name (C function name without the gidgen_matrix_ prefix)
 *   ${sigName}, ${SIG} Signal name and signal enum identifier of the cell
 *   ${nullAnnot} "(nullable): " for nullable cells (empty otherwise)
 *
 * Besides the C function, a position can add text to named sections (see Section) which are assembled into the
 * aggregate test types (fields record, properties and signals objects, callback typedefs) by aggregates.d.
 *
 * To add a new position, add an entry to `positions` and a matching branch in gidgen-test/matrix_test.d.
 */
module position_table;

import std.algorithm : among;
import type_table;

/// Text added to a named section of the generated C/defs files for each cell of a position (see aggregates.d)
struct Section
{
  string name; /// Section name
  string text; /// Template text
}

/// A position table entry
struct Position
{
  string id; /// Identifier used in generated function names and the D manifest
  string direction; /// GIR direction annotation (empty for in parameters and return values)
  string val; /// C expression for the value in the function (used by ${checkIn} and ${release})
  bool function(const TypeEntry) supports; /// Returns true if the position supports a type
  string[] function(const TypeEntry) transfers; /// Ownership transfers to generate for a type
  bool nullable; /// Generate nullable variants for pointer types
  string ret; /// C return type template
  string args; /// C argument list template
  string paramDocs; /// gtk-doc parameter lines template
  string returnDoc; /// gtk-doc Returns line template (empty if none)
  string body; /// C function body template
  Section[] sections; /// Additional section templates
  string member; /// C name template of the field, property, signal or method tested by the cell (D name derived from it)
  bool method; /// The cell function is a method of the type (named gidgen_matrix_<type>_<position>)
  bool fieldsMethod; /// The cell function is a method of the fields record (named gidgen_matrix_fields_<position>_<type>)

  // C arrays with a separate length parameter (TypeEntry.lenType): ${lenB}/${lenA} are the length parameter
  // declaration placed before/after the array (function or callback typedef), ${lenDoc} its gtk-doc line,
  // ${lenDecl} a local length declaration (lenLocal), ${lenCallB}/${lenCallA} the length callback argument and
  // ${n} the length value expression
  string lenName = "n_value"; /// Name of the length parameter
  bool lenPtr; /// The length parameter is a pointer (out/inout)
  string lenDir; /// GIR direction annotation of the length parameter
  bool lenLocal; /// The length is a local variable of the C function (callbacks)
}

/// Pointer types support transfer none and full (unless TypeEntry.transferFull is false), other types only none (no
/// transfer suffix in function names)
private string[] anyTransfer(const TypeEntry t) => t.pointer && t.transferFull ? ["none", "full"] : ["none"];
private string[] fullTransfer(const TypeEntry t) => t.pointer && t.transferFull ? ["full"] : ["none"];
private string[] noneTransfer(const TypeEntry t) => ["none"];
private bool allTypes(const TypeEntry t) => true;
private bool callerAllocTypes(const TypeEntry t) => t.cFill.length > 0;
private bool lenTypes(const TypeEntry t) => t.lenType.length > 0;
private bool fieldTypes(const TypeEntry t) => (!t.pointer || t.nullable) && !t.container;
private bool fixedFieldTypes(const TypeEntry t) => t.fixedElem.length > 0;
private bool fixedTypes(const TypeEntry t) => t.fixedSize > 0;
private bool embedTypes(const TypeEntry t) => t.embed.length > 0;
private bool borrowTypes(const TypeEntry t) => t.borrow.length > 0;
private bool inPlaceTypes(const TypeEntry t) => t.inPlace;
private bool instanceTypes(const TypeEntry t) => t.own.among(Own.Ref, Own.Copy, Own.Borrow) != 0;
private bool staticRetTypes(const TypeEntry t) => t.pointer && !t.container;
private bool gtypeTypes(const TypeEntry t) => t.gtype.length > 0;
private bool basicTypes(const TypeEntry t) => !t.pointer;
private bool sigOutTypes(const TypeEntry t) => !t.pointer && t.signalGtype.length > 0;
private bool signalTypes(const TypeEntry t) => t.signalGtype.length > 0;

/// C body of the map positions (fills the structure with the Out value and a static name)
private enum mapBody = "  CHECK (rec != NULL);\n  info->value = ${vOut};\n  info->name = (gchar *)\"pw\";\n"
  ~ "  gm_live_add (GIDGEN_MATRIX_COUNTER_MAPS, 1);\n  return TRUE;\n";

/// C body of the unmap functions of the map positions (checks the structure is mapped and invalidates it)
private enum unmapBody = "  CHECK (info->value == ${vOut});\n  info->value = -1;\n  gm_live_add (GIDGEN_MATRIX_COUNTER_MAPS, -1);\n";

/// Fields record sections of the borrowed structure positions (shared, so they are added once per type)
private immutable Section[] borrowSections = [
  {"fieldsMember", "  ${borrow} ${id}_bfield;\n  ${borrow} *${id}_pfield;\n"},
  {"fieldsDoc", " * @${id}_bfield: Embedded structure field of type ${id} (borrowed by D)\n"
    ~ " * @${id}_pfield: Owned structure pointer field of type ${id} (borrowed by D)\n"},
  {"fieldsCopy", "  gm_${id}_copy_into (&copy->${id}_bfield, &rec->${id}_bfield);\n"
    ~ "  copy->${id}_pfield = rec->${id}_pfield ? gm_${id}_copy (rec->${id}_pfield) : NULL;\n"},
  {"fieldsFree", "  gm_${id}_clear (&rec->${id}_bfield);\n  g_clear_pointer (&rec->${id}_pfield, gm_${id}_free);\n"},
  {"fieldsAddr", "  if (g_strcmp0 (member, \"${dId}Bfield\") == 0)\n    return &rec->${id}_bfield;\n"
    ~ "  if (g_strcmp0 (member, \"${dId}Pfield\") == 0)\n    return rec->${id}_pfield;\n"},
  {"defs", "//!set record[Fields].field[${id}_bfield][writable] 0\n//!set record[Fields].field[${id}_pfield][writable] 0\n"},
];

/// A known unsupported matrix cell combination (not generated), with the reason
struct Unsupported
{
  string pos; /// Position id ("*" for any)
  string type; /// Type id ("container" for any container type, "*" for any type, "prefix*" for ids with a prefix)
  string transfer; /// Transfer ("none", "full" or "*" for any)
  string reason; /// Why the cell is unsupported
}

/// Known unsupported cells
immutable Unsupported[] unsupportedCells = [
  // gidgen disables callbacks returning strings with transfer none (the D string memory can't be kept alive)
  {"cb_ret", "utf8", "none", "callback string return value with transfer none is not supported by gidgen"},
  {"cb_ret", "filename", "none", "callback string return value with transfer none is not supported by gidgen"},

  // gidgen disables callback structure pointer return values and out parameters without full ownership transfer
  // (a D copy of the structure can't be kept alive for C)
  {"cb_ret", "box", "none", "callback structure pointer return value without full ownership not supported by gidgen"},
  {"cb_out", "box", "none", "callback out structure pointer without full ownership not supported by gidgen"},
  {"cb_ret", "plain", "none", "callback structure pointer return value without full ownership not supported by gidgen"},
  {"cb_out", "plain", "none", "callback out structure pointer without full ownership not supported by gidgen"},
  {"cb_ret", "plainwrap", "none", "callback structure pointer return value without full ownership not supported by gidgen"},
  {"cb_ret", "allocwrap", "none", "callback structure pointer return value without full ownership not supported by gidgen"},
  {"cb_ret", "initwrap", "none", "callback structure pointer return value without full ownership not supported by gidgen"},
  {"cb_out", "plainwrap", "none", "callback out structure pointer without full ownership not supported by gidgen"},
  {"cb_out", "allocwrap", "none", "callback out structure pointer without full ownership not supported by gidgen"},
  {"cb_out", "initwrap", "none", "callback out structure pointer without full ownership not supported by gidgen"},

  // Signal values are typed by their GType in the GIR, which differs from the C type of the emitting function
  // for these types (gint8 signals are gchar = D char, gunichar signals are guint = D uint), so the D test can't
  // derive the handler type from the emit function
  {"sig_arg", "int8", "none", "G_TYPE_CHAR signal value is gchar (D char), not gint8"},
  {"sig_ret", "int8", "none", "G_TYPE_CHAR signal value is gchar (D char), not gint8"},
  {"sig_arg", "unichar", "none", "no gunichar GType, G_TYPE_UINT signal value is guint (D uint), not gunichar"},
  {"sig_ret", "unichar", "none", "no gunichar GType, G_TYPE_UINT signal value is guint (D uint), not gunichar"},

  // gidgen disables signals returning boxed types bound as D value structs
  {"sig_ret", "box", "full", "signal return type 'Struct' is not supported by gidgen"},

  // gidgen disables signals returning containers
  {"sig_ret", "container", "*", "signal container return values not supported by gidgen"},

  // gidgen disables callback container return values and out parameters without full ownership transfer (the
  // container created by D can't be kept alive)
  {"cb_ret", "container", "none", "callback return container without full ownership not supported by gidgen"},
  {"cb_out", "container", "none", "callback out container without full ownership not supported by gidgen"},

  // gidgen disables InOut GLib container (not C array) parameters
  {"inout", "list_*", "*", "InOut GList parameters not supported by gidgen"},
  {"inout", "slist_*", "*", "InOut GSList parameters not supported by gidgen"},
  {"inout", "ptrarray_*", "*", "InOut GPtrArray parameters not supported by gidgen"},
  {"inout", "garray_*", "*", "InOut GArray parameters not supported by gidgen"},
  {"inout", "hash_*", "*", "InOut GHashTable parameters not supported by gidgen"},

  // gidgen requires InOut arrays without ownership transfer to be caller allocated
  {"inout", "strv", "none", "InOut arrays must be caller allocated (gidgen)"},
  {"inout", "carray_*", "none", "InOut arrays must be caller allocated (gidgen)"},
  {"inout", "fixed_*", "none", "InOut arrays must be caller allocated (gidgen)"},

  // gidgen disables InOut parameters of records without a GType (the memory management of the D value is unknown)
  {"inout", "opaque", "*", "InOut Opaque pointer parameters not supported by gidgen"},
  {"inout", "plainwrap", "*", "InOut Wrap pointer parameters not supported by gidgen"},
  {"inout", "allocwrap", "*", "InOut Wrap pointer parameters not supported by gidgen"},
  {"inout", "initwrap", "*", "InOut Wrap pointer parameters not supported by gidgen"},

  // gidgen makes pointer fields of records without a GType read-only
  {"field", "opaque", "*", "Opaque pointer fields are not writable in gidgen"},
  {"field", "plainwrap", "*", "Wrap pointer fields are not writable in gidgen"},
  {"field", "allocwrap", "*", "Wrap pointer fields are not writable in gidgen"},
  {"field", "initwrap", "*", "Wrap pointer fields are not writable in gidgen"},

  // gidgen only supports InOut full C arrays of basic types and strings
  {"inout", "carray_obj", "full", "InOut full object arrays not supported by gidgen"},
  {"inout", "carray_plain", "full", "InOut full structure arrays not supported by gidgen"},
  {"inout", "fixed_plain", "full", "InOut full structure arrays not supported by gidgen"},

  // gidgen disables callback InOut containers and arrays
  {"cb_inout", "container", "*", "callback InOut containers and arrays not supported by gidgen"},

  // gidgen disables callback InOut instance parameters without full ownership transfer (the C side would borrow the
  // resulting instance, which no D owner can keep alive), and of records without a GType (unknown memory management)
  {"cb_inout", "obj", "none", "callback InOut instance parameters without full ownership not supported by gidgen"},
  {"cb_inout", "rec", "none", "callback InOut instance parameters without full ownership not supported by gidgen"},
  {"cb_inout", "aligned", "none", "callback InOut instance parameters without full ownership not supported by gidgen"},
  {"cb_inout", "reffed", "none", "callback InOut instance parameters without full ownership not supported by gidgen"},
  {"cb_inout", "bytes", "none", "callback InOut instance parameters without full ownership not supported by gidgen"},
  {"cb_inout", "variant", "none", "callback InOut instance parameters without full ownership not supported by gidgen"},
  {"cb_inout", "gvalue", "none", "callback InOut instance parameters without full ownership not supported by gidgen"},
  {"cb_inout", "opaque", "*", "InOut Opaque pointer parameters not supported by gidgen"},
  {"cb_inout", "plainwrap", "*", "InOut Wrap pointer parameters not supported by gidgen"},
  {"cb_inout", "allocwrap", "*", "InOut Wrap pointer parameters not supported by gidgen"},
  {"cb_inout", "initwrap", "*", "InOut Wrap pointer parameters not supported by gidgen"},

  // gidgen disables transfer full containers of Wrap structures with pointer fields (the D copies can't free them)
  {"*", "garray_plainwrap", "full", "transfer full containers of Wrap structures with pointer fields not supported by gidgen"},

  // KNOWN GIDGEN BUG (not fixed): g-ir-scanner omits zero-terminated="1" since it is the GIR default for arrays
  // without a length or fixed size, but gidgen defaults it to 0 (only string arrays are assumed zero terminated),
  // so zero terminated object arrays are disabled with "indeterminate length"
  {"*", "ztarray_obj", "*", "gidgen ignores the GIR zero-terminated default for non-string arrays"},
];

/// Check if a cell is known to be unsupported
bool isUnsupported(string pos, const TypeEntry type, string transfer)
{
  import std.algorithm : endsWith, startsWith;
  foreach (u; unsupportedCells)
    if ((u.pos == "*" || u.pos == pos) && (u.type == "*" || u.type == type.id || (u.type == "container" && type.container)
        || (u.type.endsWith("*") && type.id.startsWith(u.type[0 .. $ - 1])))
        && (u.transfer == "*" || u.transfer == transfer))
      return true;
  return false;
}

/// The position table
immutable Position[] positions = [
  { id: "in", val: "value", supports: &allTypes, transfers: &anyTransfer, nullable: true,
    ret: "void", args: "${nullArg}${lenB}${type} value${lenA}",
    paramDocs: "${nullDoc} * @value: ${annots}Input value\n${lenDoc}",
    body: "  CHECK (${checkIn});\n${release}" },

  // Fixed size array In parameter sized by the D wrapper: D passes the first n_set In value elements (or a longer
  // array), the C function gets the first n_set elements followed by zeros
  { id: "in_fixed", val: "value", supports: &fixedTypes, transfers: &anyTransfer,
    ret: "void", args: "guint n_set, ${type} value",
    paramDocs: " * @n_set: Number of In value elements passed by D (the remaining elements must be zero)\n"
      ~ " * @value: ${annots}Input value\n",
    body: "  CHECK (${eqInPrefix});\n${release}" },

  // Nullable fixed size array In parameter: like in_fixed, but D passes an empty array as NULL
  { id: "in_fixed_null", val: "value", supports: &fixedTypes, transfers: &anyTransfer,
    ret: "void", args: "guint n_set, ${type} value",
    paramDocs: " * @n_set: Number of In value elements passed by D (the remaining elements must be zero)\n"
      ~ " * @value: (nullable) ${annots}Input value\n",
    body: "  CHECK (n_set == 0 ? value == NULL : ${eqInPrefix});\n${release}" },

  { id: "out", direction: "(out)", lenPtr: true, lenDir: "(out)", supports: &allTypes, transfers: &anyTransfer, nullable: true,
    ret: "void", args: "${nullArg}${lenB}${type} *value${lenA}",
    paramDocs: "${nullDoc} * @value: ${annots}Output value\n${lenDoc}",
    body: "  *value = ${makeOut};\n" },

  { id: "out_ca", direction: "(out caller-allocates)", lenDir: "(in)", supports: &callerAllocTypes, transfers: &noneTransfer,
    ret: "void", args: "${lenB}${cType} value${lenA}",
    paramDocs: " * @value: ${annots}Caller allocated output value\n${lenDoc}",
    body: "  ${fillOut};\n" },

  // Transfer none inout pointer values: the callee only borrows the input value and returns a borrowed value
  { id: "inout", direction: "(inout)", lenPtr: true, lenDir: "(inout)", val: "(*value)", supports: &allTypes, transfers: &anyTransfer, nullable: true,
    ret: "void", args: "${nullArg}${lenB}${type} *value${lenA}",
    paramDocs: "${nullDoc} * @value: ${annots}Input and output value\n${lenDoc}",
    body: "  CHECK (${checkIn});\n${release}  *value = ${makeOut};\n" },

  // Inout value modified in place (a single pointer, so caller allocated): the callee checks the In value and fills
  // in the Out value
  { id: "inout_ca", direction: "(inout)", lenDir: "(in)", val: "value", supports: &callerAllocTypes,
    transfers: &noneTransfer, ret: "void", args: "${lenB}${cType} value${lenA}",
    paramDocs: " * @value: ${annots}Input value replaced by the output value\n${lenDoc}",
    body: "  CHECK (${checkIn});\n  ${fillOut};\n" },

  { id: "ret", lenName: "n_ret", lenPtr: true, lenDir: "(out)", supports: &allTypes, transfers: &anyTransfer, nullable: true,
    ret: "${type}", args: "${retArgs}",
    paramDocs: "${nullDoc}${lenDoc}", returnDoc: " * Returns: ${annots}Return value\n",
    body: "  return ${makeRet};\n" },

  // Arrays with a size given by the caller: the length is an input parameter passed by value
  { id: "ret_len_in", lenName: "n_ret", supports: &lenTypes, transfers: &anyTransfer, nullable: true,
    ret: "${type}", args: "${retArgs}",
    paramDocs: "${nullDoc}${lenDoc}", returnDoc: " * Returns: ${annots}Return value\n",
    body: "  CHECK (${n} == ${nRet});\n  return ${makeRet};\n" },

  { id: "out_len_in", direction: "(out)", supports: &lenTypes, transfers: &anyTransfer, nullable: true,
    ret: "void", args: "${nullArg}${lenB}${type} *value${lenA}",
    paramDocs: "${nullDoc} * @value: ${annots}Output value\n${lenDoc}",
    body: "  CHECK (${n} == ${nOut});\n  *value = ${makeOut};\n" },

  // Transfer none return value of an instance which the library keeps (the same instance is returned by each call
  // until the kept value is released): D copies or references it, independent of other calls
  { id: "ret_static", supports: &staticRetTypes, transfers: &noneTransfer,
    ret: "${type}", args: "void", paramDocs: "", returnDoc: " * Returns: ${annots}Library owned value\n",
    body: "  ${type} value = gidgen_matrix_kept ();\n  return value ? value : ${makeRet};\n" },

  // Instance parameter with transfer full: the method takes ownership of the instance (D passes a reference or copy)
  { id: "self_full", method: true, val: "self", supports: &instanceTypes, transfers: &fullTransfer,
    member: "self_full", ret: "void", args: "${cType} self",
    paramDocs: " * @self: (transfer full): The instance (In value)\n",
    body: "  CHECK (${checkIn});\n  ${cFree} (self);\n" },

  // Structure instance parameter passed by value
  { id: "self_value", method: true, val: "(&self)", supports: &embedTypes, transfers: &noneTransfer,
    member: "self_value", ret: "void", args: "${embed} self",
    paramDocs: " * @self: The instance (In value)\n",
    body: "  CHECK (${checkIn});\n" },

  // Record field: D sets the field to the In value, the C function checks it and replaces it with the Out value
  // which D then reads back. The record owns its pointer fields (marked owned in the generated definitions).
  // Pointers to boxed types bound as D value structs and container fields are not writable in gidgen (by design)
  { id: "field", val: "rec->${id}_field", supports: &fieldTypes, transfers: &noneTransfer, member: "${id}_field",
    nullable: true, ret: "void", args: "${nullArg}GidgenMatrixFields *rec",
    paramDocs: "${nullDoc} * @rec: The fields record\n",
    body: "  CHECK (${checkIn});\n${freeVal}  ${val} = ${makeOutFull};\n",
    sections: [
      {"fieldsMember", "  ${cType} ${id}_field;\n"},
      {"fieldsDoc", " * @${id}_field: Field of type ${id}\n"},
      {"fieldsCopy", "  copy->${id}_field = ${copyVal};\n"},
      {"fieldsFree", "${freeVal}"},
      {"defs", "//!set record[Fields].field[${id}_field][owned] 1\n"},
    ] },

  // Borrowed structures: Wrap structures with pointer fields owned by the fields record (embedded ${id}_bfield
  // and owned pointer ${id}_pfield) which D borrows (points at the C structure and keeps the record alive) from a
  // field getter, a transfer none method return or out value, or a method returning a structure embedded in its
  // instance. gidgen_matrix_fields_addr() returns the C address of a field structure (by the D field name).
  { id: "borrow_field", fieldsMethod: true, supports: &borrowTypes, transfers: &noneTransfer, member: "${id}_bfield",
    ret: "void", args: "GidgenMatrixFields *rec", paramDocs: " * @rec: The fields record\n",
    body: "  CHECK (rec->${id}_bfield.value == ${vIn});\n  rec->${id}_bfield.value = ${vOut};\n",
    sections: borrowSections },

  { id: "self_borrow", fieldsMethod: true, supports: &borrowTypes, transfers: &noneTransfer, member: "${id}_bfield",
    ret: "const ${borrow} *", args: "GidgenMatrixFields *rec", paramDocs: " * @rec: The fields record\n",
    returnDoc: " * Returns: (transfer none): Structure embedded in @rec\n",
    body: "  return &rec->${id}_bfield;\n", sections: borrowSections },

  { id: "borrow_ret", fieldsMethod: true, supports: &borrowTypes, transfers: &noneTransfer, member: "${id}_pfield",
    ret: "const ${borrow} *", args: "GidgenMatrixFields *rec", paramDocs: " * @rec: The fields record\n",
    returnDoc: " * Returns: (transfer none): Structure owned by @rec\n",
    body: "  if (!rec->${id}_pfield)\n    rec->${id}_pfield = gm_${id}_new (${vRet});\n  return rec->${id}_pfield;\n",
    sections: borrowSections },

  { id: "borrow_out", fieldsMethod: true, supports: &borrowTypes, transfers: &noneTransfer, member: "${id}_pfield",
    ret: "void", args: "GidgenMatrixFields *rec, const ${borrow} **value",
    paramDocs: " * @rec: The fields record\n * @value: (out) (transfer none): Structure owned by @rec\n",
    body: "  if (!rec->${id}_pfield)\n    rec->${id}_pfield = gm_${id}_new (${vOut});\n  *value = rec->${id}_pfield;\n",
    sections: borrowSections },

  // Borrowed structure returned (transfer none) by a function from a parameter which owns it (gid-borrow definition)
  { id: "borrow_owner_ret", supports: &borrowTypes, transfers: &noneTransfer, member: "${id}_pfield",
    ret: "const ${borrow} *", args: "GidgenMatrixFields *rec", paramDocs: " * @rec: The fields record\n",
    returnDoc: " * Returns: (transfer none): Structure owned by @rec\n",
    body: "  if (!rec->${id}_pfield)\n    rec->${id}_pfield = gm_${id}_new (${vRet});\n  return rec->${id}_pfield;\n",
    sections: borrowSections ~ [Section("defs", "//!set function[${cell}].return-value[][gid-borrow] rec\n")] },

  // Borrowed structure returned (transfer none) by a function from static data (gid-borrow "static" definition)
  { id: "static_borrow", supports: &borrowTypes, transfers: &noneTransfer,
    ret: "const ${borrow} *", args: "void", returnDoc: " * Returns: (transfer none): Static structure\n",
    body: "  static ${borrow} value = { ${vRet}, (gchar *)\"pw\" };\n  return &value;\n",
    sections: [{"defs", "//!set function[${cell}].return-value[][gid-borrow] static\n"}] },

  // Caller allocated out structure filled by a method of the fields record: the D copy's pointer fields may reference
  // the instance, so the copy keeps the fields record alive
  { id: "fill_out_ca", fieldsMethod: true, supports: &borrowTypes, transfers: &noneTransfer,
    ret: "void", args: "GidgenMatrixFields *rec, ${borrow} *value",
    paramDocs: " * @rec: The fields record\n * @value: (out caller-allocates): Structure filled from @rec\n",
    body: "  CHECK (rec != NULL);\n  value->value = ${vOut};\n  value->name = (gchar *)\"pw\";\n" },

  // Map function: fills a caller allocated structure (Out Value, static name) which the D MapScope must unmap once
  // (explicit unmap or scope exit) with the unmap function named by the gid-unmap definition, keeping the source
  // fields record alive while mapped. The C unmap checks and invalidates the structure (a second unmap fails).
  { id: "map_method", fieldsMethod: true, supports: &borrowTypes, transfers: &noneTransfer,
    ret: "gboolean", args: "GidgenMatrixFields *rec, ${borrow} *info",
    paramDocs: " * @rec: The fields record\n * @info: (out caller-allocates): Mapped structure\n",
    returnDoc: " * Returns: %TRUE if mapped\n", body: mapBody,
    sections: [
      {"mapProtos", "void gidgen_matrix_fields_unmap_method_${id} (GidgenMatrixFields *rec, ${borrow} *info);\n"},
      {"mapFuncs", "/**\n * gidgen_matrix_fields_unmap_method_${id}:\n * @rec: The fields record\n * @info: Mapped structure\n"
        ~ " *\n * Unmap a structure.\n */\nvoid\ngidgen_matrix_fields_unmap_method_${id} (GidgenMatrixFields *rec, ${borrow} *info)\n"
        ~ "{\n  CHECK (rec != NULL);\n" ~ unmapBody ~ "}\n\n"},
      {"defs", "//!set record[Fields].method[map_method_${id}][gid-unmap] gidgen_matrix_fields_unmap_method_${id}\n"
        ~ "//!set record[Fields].method[unmap_method_${id}][disable] 1\n"},
    ] },

  { id: "map_func", supports: &borrowTypes, transfers: &noneTransfer,
    ret: "gboolean", args: "GidgenMatrixFields *rec, ${borrow} *info",
    paramDocs: " * @rec: The fields record\n * @info: (out caller-allocates): Mapped structure\n",
    returnDoc: " * Returns: %TRUE if mapped\n", body: mapBody,
    sections: [
      {"mapProtos", "void gidgen_matrix_un${cell} (${borrow} *info);\n"},
      {"mapFuncs", "/**\n * gidgen_matrix_un${cell}:\n * @info: Mapped structure\n *\n * Unmap a structure.\n */\n"
        ~ "void\ngidgen_matrix_un${cell} (${borrow} *info)\n{\n" ~ unmapBody ~ "}\n\n"},
      {"defs", "//!set function[${cell}][gid-unmap] gidgen_matrix_un${cell}\n//!set function[${cell}][gid-map-source] rec\n"
        ~ "//!set function[un${cell}][disable] 1\n"},
    ] },

  // Borrowed structure callback/signal argument (transfer none): D borrows the C structure for the duration of the
  // call (wrapper with no owner), its write must reach C and a dup taken in D must survive the C structure being freed
  { id: "cb_borrow", supports: &borrowTypes, transfers: &noneTransfer,
    ret: "void", args: "${cbType} func, gpointer user_data",
    paramDocs: " * @func: (scope call) (closure user_data): Callback\n * @user_data: User data\n",
    body: "  ${borrow} *value = gm_${id}_new (${vOut});\n  func (value, user_data);\n  CHECK (value->value == ${vIn});\n"
      ~ "  gm_${id}_free (value);\n",
    sections: [{"cbTypedef", "/**\n * ${cbType}:\n * @value: (transfer none): Borrowed structure\n"
      ~ " * @user_data: (closure): User data\n *\n * Matrix callback.\n */\n"
      ~ "typedef void (*${cbType}) (const ${borrow} *value, gpointer user_data);\n\n"}] },

  // Borrowed structure array callback argument (transfer none, inline elements with a length): D borrows each
  // element for the duration of the call (wrappers with no owner), its writes must reach C and dups must survive
  { id: "cb_borrow_array", supports: &borrowTypes, transfers: &noneTransfer,
    ret: "void", args: "${cbType} func, gpointer user_data",
    paramDocs: " * @func: (scope call) (closure user_data): Callback\n * @user_data: User data\n",
    body: "  ${borrow} *a = gm_${id}_new (${vOut});\n  ${borrow} *b = gm_${id}_new (${vOut});\n"
      ~ "  ${borrow} values[2] = { *a, *b };\n\n  func (values, 2, user_data);\n"
      ~ "  CHECK (values[0].value == ${vIn} && values[1].value == ${vIn});\n"
      ~ "  *a = values[0];\n  *b = values[1];\n  gm_${id}_free (a);\n  gm_${id}_free (b);\n",
    sections: [{"cbTypedef", "/**\n * ${cbType}:\n * @values: (array length=n_values) (transfer none): Borrowed structures\n"
      ~ " * @n_values: Number of structures\n * @user_data: (closure): User data\n *\n * Matrix callback.\n */\n"
      ~ "typedef void (*${cbType}) (const ${borrow} *values, gsize n_values, gpointer user_data);\n\n"}] },

  // Boxed callback argument passed as a non-const pointer (transfer none): D borrows the C instance during the call,
  // so the In value it writes (mode GM_INOUT_REPLACE) reaches C, a reference kept by D survives the C instance being
  // freed (copied after the call), and destroying the borrowed wrapper (mode GM_INOUT_KEEP) does not free the C instance
  { id: "cb_inplace", val: "value", supports: &inPlaceTypes, transfers: &noneTransfer,
    ret: "void", args: "gint mode, ${cbType} func, gpointer user_data",
    paramDocs: " * @mode: What the callback does (0 = write the In value, 1 = destroy the D wrapper)\n"
      ~ " * @func: (scope call) (closure user_data): Callback\n * @user_data: User data\n",
    body: "  ${cType} value = ${makeOutFull};\n\n  func (value, user_data);\n"
      ~ "  CHECK (mode == GM_INOUT_KEEP ? ${eqOut} : ${eqIn});\n  ${cFree} (value);\n",
    sections: [{"cbTypedef", "/**\n * ${cbType}:\n * @value: (transfer none): Value modified in place by the callback\n"
      ~ " * @user_data: (closure): User data\n *\n * Matrix callback.\n */\n"
      ~ "typedef void (*${cbType}) (${cType} value, gpointer user_data);\n\n"}] },

    // Callback InOut borrowed Wrap** with an owner (gid-borrow): null removes the value, keep leaves the same
  // pointer. Replacing with a different instance is a programmer error (Gid critical); not exercised here because
  // G_DEBUG=fatal-criticals would abort. Matches BufferForeachMetaFunc.
  { id: "cb_inout_borrow", supports: &borrowTypes, transfers: &noneTransfer, nullable: true,
    ret: "void", args: "gint mode, ${cbType} func, gpointer user_data",
    paramDocs: " * @mode: What the callback does (1 = keep, 2 = set to %NULL)\n"
      ~ " * @func: (scope call) (closure user_data): Callback\n * @user_data: User data\n",
    body: "  GidgenMatrixFields owner = {0};\n  ${borrow} *held = gm_${id}_new (${vOut});\n"
      ~ "  ${borrow} *value = held;\n\n"
      ~ "  func (&owner, &value, user_data);\n"
      ~ "  CHECK (mode == GM_INOUT_KEEP ? (value == held && value->value == ${vOut}) : value == NULL);\n"
      ~ "  gm_${id}_free (held);\n",
    sections: [
      {"cbTypedef", "/**\n * ${cbType}:\n * @owner: Owner of the borrowed structure\n"
        ~ " * @value: (inout) (nullable) (transfer none) (type ${borrow}): Borrowed structure; set to %NULL to remove\n"
        ~ " * @user_data: (closure): User data\n *\n * Matrix callback.\n */\n"
        ~ "typedef void (*${cbType}) (GidgenMatrixFields *owner, ${borrow} **value, gpointer user_data);\n\n"},
      {"defs", "//!set callback[${cbGirName}].parameters.parameter[value][gid-borrow] owner\n"
        ~ "//!set callback[${cbGirName}].parameters.parameter[value][transfer-ownership] none\n"},
    ] },

{ id: "sig_borrow", supports: &borrowTypes, transfers: &noneTransfer, member: "${sigName}",
    ret: "void", args: "GidgenMatrixSignals *self", paramDocs: " * @self: The signals object\n",
    body: "  ${borrow} *value = gm_${id}_new (${vOut});\n  g_signal_emit (self, matrix_signals[${SIG}], 0, value);\n"
      ~ "  CHECK (value->value == ${vIn});\n  gm_${id}_free (value);\n",
    sections: [
      {"sigEnum", "  ${SIG},\n"},
      {"sigNew", "  matrix_signals[${SIG}] = g_signal_new (\"${sigName}\", G_TYPE_FROM_CLASS (klass), G_SIGNAL_RUN_LAST,"
        ~ " 0, NULL, NULL, NULL, G_TYPE_NONE, 1, G_TYPE_POINTER);\n"},
      {"sigDocs", "/**\n * GidgenMatrixSignals::${sigName}:\n * @self: The object\n * @value: (type ${borrow}): Borrowed"
        ~ " structure\n *\n * Matrix signal.\n */\n\n"},
    ] },

  // Embedded (non-pointer) structure record field: D sets the In value, the C function checks it and sets the Out
  // value
  { id: "field_embed", val: "(&rec->${id}_efield)", supports: &embedTypes, transfers: &noneTransfer,
    member: "${id}_efield", ret: "void", args: "GidgenMatrixFields *rec",
    paramDocs: " * @rec: The fields record\n",
    body: "  CHECK (${checkIn});\n  ${fillOutVal};\n",
    sections: [
      {"fieldsMember", "  ${embed} ${id}_efield;\n"},
      {"fieldsDoc", " * @${id}_efield: Embedded structure field of type ${id}\n"},
    ] },

  // Read-only record field array with a length field: the C function sets the array (borrowed from the library)
  // and its length, D reads it back
  { id: "field_array", val: "rec->${id}_afield", lenName: "rec->n_${id}_afield", supports: &lenTypes,
    transfers: &noneTransfer, member: "${id}_afield", ret: "void", args: "GidgenMatrixFields *rec",
    paramDocs: " * @rec: The fields record\n",
    body: "  ${val} = ${makeOut};\n",
    sections: [
      {"fieldsMember", "  ${cType} ${id}_afield;\n  ${lenType} n_${id}_afield;\n"},
      {"fieldsDoc", " * @${id}_afield: (array length=n_${id}_afield): Array field of type ${id}\n"
        ~ " * @n_${id}_afield: Length of ${id}_afield\n"},
      {"defs", "//!set record[Fields].field[${id}_afield][writable] 0\n"}, // Array field setters not supported
    ] },

  // Inline fixed size array record field: D sets the In value, the C function checks it and sets the Out value
  { id: "field_fixed", val: "rec->${id}_ffield", supports: &fixedFieldTypes, transfers: &noneTransfer,
    member: "${id}_ffield", ret: "void", args: "GidgenMatrixFields *rec",
    paramDocs: " * @rec: The fields record\n",
    body: "  CHECK (${checkIn});\n  ${fillOutVal};\n",
    sections: [
      {"fieldsMember", "  ${fixedElem} ${id}_ffield[3];\n"},
      {"fieldsDoc", " * @${id}_ffield: Inline fixed size array field of type ${id}\n"},
    ] },

  // GObject property: D sets the property to the In value with the generated property accessor, the C function
  // checks the stored value and replaces it with the Out value which D then reads back.
  { id: "prop", val: "self->${id}_prop", supports: &gtypeTypes, transfers: &noneTransfer, member: "${id}-prop",
    nullable: true, ret: "void", args: "${nullArg}GidgenMatrixProps *self",
    paramDocs: "${nullDoc} * @self: The properties object\n",
    body: "  CHECK (${checkIn});\n${freeVal}  ${val} = ${makeOutFull};\n",
    sections: [
      {"propsMember", "  ${cType} ${id}_prop;\n"},
      {"propsEnum", "  PROP_${ID},\n"},
      {"propsInstall", "  props[PROP_${ID}] = ${pspec};\n"},
      {"propsSet", "    case PROP_${ID}:\n  ${freeVal}      ${val} = g_value_${gvGet} (value);\n      break;\n"},
      {"propsGet", "    case PROP_${ID}:\n      g_value_set_${gvalue} (value, ${val});\n      break;\n"},
      {"propsFree", "${freeVal}"},
    ] },

  // GObject property with annotated C getter and setter methods (used by the generated D property accessors)
  { id: "prop_acc", val: "self->${id}_acc", supports: &gtypeTypes, transfers: &noneTransfer, member: "${id}-acc",
    nullable: true, ret: "void", args: "${nullArg}GidgenMatrixProps *self",
    paramDocs: "${nullDoc} * @self: The properties object\n",
    body: "  CHECK (${checkIn});\n${freeVal}  ${val} = ${makeOutFull};\n",
    sections: [
      {"propsMember", "  ${cType} ${id}_acc;\n"},
      {"propsEnum", "  PROP_${ID}_ACC,\n"},
      {"propsInstall", "  props[PROP_${ID}_ACC] = ${pspecAcc};\n"},
      {"propsSet", "    case PROP_${ID}_ACC:\n  ${freeVal}      ${val} = g_value_${gvGet} (value);\n      break;\n"},
      {"propsGet", "    case PROP_${ID}_ACC:\n      g_value_set_${gvalue} (value, ${val});\n      break;\n"},
      {"propsFree", "${freeVal}"},
      {"propsProtos", "void gidgen_matrix_props_set_${id}_acc (GidgenMatrixProps *self, ${type} value);\n"
        ~ "${type} gidgen_matrix_props_get_${id}_acc (GidgenMatrixProps *self);\n"},
      {"propsMethods", "/**\n * GidgenMatrixProps:${id}-acc: (setter set_${id}_acc) (getter get_${id}_acc)\n *\n"
        ~ " * Property with accessor methods.\n */\n\n"
        ~ "/**\n * gidgen_matrix_props_set_${id}_acc: (set-property ${id}-acc)\n * @self: The object\n"
        ~ " * @value: ${accAnnots}The value\n */\nvoid\ngidgen_matrix_props_set_${id}_acc (GidgenMatrixProps *self, ${type} value)\n"
        ~ "{\n  g_object_set (self, \"${id}-acc\", value, NULL);\n}\n\n"
        ~ "/**\n * gidgen_matrix_props_get_${id}_acc: (get-property ${id}-acc)\n * @self: The object\n *\n"
        ~ " * Returns: ${accAnnots}The value\n */\n${type}\ngidgen_matrix_props_get_${id}_acc (GidgenMatrixProps *self)\n"
        ~ "{\n  return self->${id}_acc;\n}\n\n"},
    ] },

  // Read-only GObject property: the C function sets it to the Out value which D reads
  { id: "prop_ro", val: "self->${id}_ro", supports: &gtypeTypes, transfers: &noneTransfer, member: "${id}-ro",
    ret: "void", args: "GidgenMatrixProps *self",
    paramDocs: " * @self: The properties object\n",
    body: "${freeVal}  ${val} = ${makeOutFull};\n",
    sections: [
      {"propsMember", "  ${cType} ${id}_ro;\n"},
      {"propsEnum", "  PROP_${ID}_RO,\n"},
      {"propsInstall", "  props[PROP_${ID}_RO] = ${pspecRo};\n"},
      {"propsGet", "    case PROP_${ID}_RO:\n      g_value_set_${gvalue} (value, ${val});\n      break;\n"},
      {"propsFree", "${freeVal}"},
    ] },

  // Write-only GObject property: D sets it to the In value which the C function checks
  { id: "prop_wo", val: "self->${id}_wo", supports: &gtypeTypes, transfers: &noneTransfer, member: "${id}-wo",
    ret: "void", args: "GidgenMatrixProps *self",
    paramDocs: " * @self: The properties object\n",
    body: "  CHECK (${checkIn});\n",
    sections: [
      {"propsMember", "  ${cType} ${id}_wo;\n"},
      {"propsEnum", "  PROP_${ID}_WO,\n"},
      {"propsInstall", "  props[PROP_${ID}_WO] = ${pspecWo};\n"},
      {"propsSet", "    case PROP_${ID}_WO:\n  ${freeVal}      ${val} = g_value_${gvGet} (value);\n      break;\n"},
      {"propsFree", "${freeVal}"},
    ] },

  // Construct-only GObject property: the C function constructs an object with the Out value which D reads
  { id: "prop_co", val: "self->${id}_co", supports: &gtypeTypes, transfers: &noneTransfer, member: "${id}-co",
    ret: "GidgenMatrixProps *", args: "void", paramDocs: "",
    returnDoc: " * Returns: (transfer full): A new object with the construct-only property set to the Out value\n",
    body: "  GValue v = G_VALUE_INIT;\n  const char *name = \"${id}-co\";\n  GObject *obj;\n\n"
      ~ "  g_value_init (&v, ${gtype});\n  g_value_set_${gvalue} (&v, ${makeOut});\n"
      ~ "  obj = g_object_new_with_properties (GIDGEN_MATRIX_TYPE_PROPS, 1, &name, &v);\n  g_value_unset (&v);\n"
      ~ "  return GIDGEN_MATRIX_PROPS (obj);\n",
    sections: [
      {"propsMember", "  ${cType} ${id}_co;\n"},
      {"propsEnum", "  PROP_${ID}_CO,\n"},
      {"propsInstall", "  props[PROP_${ID}_CO] = ${pspecCo};\n"},
      {"propsSet", "    case PROP_${ID}_CO:\n  ${freeVal}      ${val} = g_value_${gvGet} (value);\n      break;\n"},
      {"propsGet", "    case PROP_${ID}_CO:\n      g_value_set_${gvalue} (value, ${val});\n      break;\n"},
      {"propsFree", "${freeVal}"},
    ] },

  // Callback argument: C calls a D delegate with the Out value
  { id: "cb_arg", lenName: "n", lenLocal: true,  supports: &allTypes, transfers: &anyTransfer, nullable: true,
    ret: "void", args: "${nullArg}${cbType} func, gpointer user_data",
    paramDocs: "${nullDoc} * @func: (scope call) (closure user_data): Callback\n * @user_data: User data\n",
    body: "${lenDecl}  ${type} value = ${makeOut};\n  func (${lenCallB}value${lenCallA}, user_data);\n",
    sections: [{"cbTypedef", "/**\n * ${cbType}:\n * @value: ${annots}Value passed to the callback\n${lenDoc}"
      ~ " * @user_data: (closure): User data\n *\n * Matrix callback.\n */\n"
      ~ "typedef void (*${cbType}) (${lenB}${type} value${lenA}, gpointer user_data);\n\n"}] },

  // Callback return value: C calls a D delegate which returns the In value
  { id: "cb_ret", lenName: "n", lenLocal: true, lenPtr: true, lenDir: "(out)", val: "value", supports: &allTypes, transfers: &anyTransfer, nullable: true,
    ret: "void", args: "${nullArg}${cbType} func, gpointer user_data",
    paramDocs: "${nullDoc} * @func: (scope call) (closure user_data): Callback\n * @user_data: User data\n",
    body: "${lenDecl}  ${type} value = func (${lenArgB}user_data);\n  CHECK (${checkIn});\n${release}",
    sections: [{"cbTypedef", "/**\n * ${cbType}:\n * @user_data: (closure): User data\n${lenDoc} *\n * Matrix callback.\n *\n"
      ~ " * Returns: ${annots}Value returned by the callback\n */\n"
      ~ "typedef ${type} (*${cbType}) (${lenParamB}gpointer user_data);\n\n"}] },

  // Fixed size array returned by a callback (sized by the D wrapper): the D delegate returns the first n_set In value
  // elements (or a longer array), C gets the first n_set elements followed by zeros
  { id: "cb_ret_fixed", val: "value", supports: &fixedTypes, transfers: &fullTransfer,
    ret: "void", args: "guint n_set, ${cbType} func, gpointer user_data",
    paramDocs: " * @n_set: Number of In value elements returned by the callback (the remaining elements must be zero)\n"
      ~ " * @func: (scope call) (closure user_data): Callback\n * @user_data: User data\n",
    body: "  ${type} value = func (user_data);\n  CHECK (${eqInPrefix});\n${release}",
    sections: [{"cbTypedef", "/**\n * ${cbType}:\n * @user_data: (closure): User data\n *\n * Matrix callback.\n *\n"
      ~ " * Returns: ${annots}Value returned by the callback\n */\n"
      ~ "typedef ${type} (*${cbType}) (gpointer user_data);\n\n"}] },

  // Callback out parameter: C calls a D delegate which sets the In value
  { id: "cb_out", lenName: "n", lenLocal: true, lenPtr: true, lenDir: "(out)", direction: "(out)", val: "value", supports: &allTypes, transfers: &anyTransfer, nullable: true,
    ret: "void", args: "${nullArg}${cbType} func, gpointer user_data",
    paramDocs: "${nullDoc} * @func: (scope call) (closure user_data): Callback\n * @user_data: User data\n",
    body: "${lenDecl}  ${type} value = 0;\n  func (${lenCallB}&value${lenCallA}, user_data);\n  CHECK (${checkIn});\n${release}",
    sections: [{"cbTypedef", "/**\n * ${cbType}:\n * @value: ${annots}Value set by the callback\n${lenDoc}"
      ~ " * @user_data: (closure): User data\n *\n * Matrix callback.\n */\n"
      ~ "typedef void (*${cbType}) (${lenB}${type} *value${lenA}, gpointer user_data);\n\n"}] },

  // Callback inout parameter: C calls a D delegate with the Out value (or NULL for nullable cells with is_null) which
  // it replaces with the In value, keeps (the same instance for reference counted types) or sets to NULL according
  // to mode (GM_INOUT_REPLACE, GM_INOUT_KEEP or GM_INOUT_SET_NULL, see gidgen-matrix-private.h)
  { id: "cb_inout", lenName: "n", lenLocal: true, lenPtr: true, lenDir: "(inout)", direction: "(inout)", val: "value",
    supports: &allTypes, transfers: &anyTransfer, nullable: true,
    ret: "void", args: "${nullArg}gint mode, ${cbType} func, gpointer user_data",
    paramDocs: "${nullDoc} * @mode: What the callback does with the value (0 = replace, 1 = keep, 2 = set to %NULL)\n"
      ~ " * @func: (scope call) (closure user_data): Callback\n * @user_data: User data\n",
    body: "${lenDecl}  ${type} value = ${makeOut};\n  G_GNUC_UNUSED ${type} orig = value;\n\n"
      ~ "  func (${lenCallB}&value${lenCallA}, user_data);\n"
      ~ "  CHECK (mode == GM_INOUT_KEEP ? (${checkOut})${keptSame} : mode == GM_INOUT_SET_NULL ? ${isNull} : ${eqIn});\n"
      ~ "${release}",
    sections: [{"cbTypedef", "/**\n * ${cbType}:\n * @value: ${annots}Value replaced by the callback\n${lenDoc}"
      ~ " * @user_data: (closure): User data\n *\n * Matrix callback.\n */\n"
      ~ "typedef void (*${cbType}) (${lenB}${type} *value${lenA}, gpointer user_data);\n\n"}] },

  // Throwing callback (throws=1): C calls a D delegate which returns the In value and then calls it again expecting
  // it to set the GError "gidgen-matrix" 1 "matrix error"
  { id: "cb_throws", val: "value", supports: &basicTypes, transfers: &noneTransfer,
    ret: "void", args: "${cbType} func, gpointer user_data",
    paramDocs: " * @func: (scope call) (closure user_data): Callback\n * @user_data: User data\n",
    body: "  GError *error = NULL;\n  ${type} value = func (user_data, &error);\n  CHECK (error == NULL && ${checkIn});\n"
      ~ "  func (user_data, &error);\n  CHECK (g_error_matches (error, g_quark_from_string (\"gidgen-matrix\"), 1)"
      ~ " && g_strcmp0 (error->message, \"matrix error\") == 0);\n  g_clear_error (&error);\n",
    sections: [{"cbTypedef", "/**\n * ${cbType}:\n * @user_data: (closure): User data\n * @error: Return location for a"
      ~ " #GError\n *\n * Matrix throwing callback.\n *\n * Returns: ${annots}Value returned by the callback\n */\n"
      ~ "typedef ${type} (*${cbType}) (gpointer user_data, GError **error);\n\n"}] },

  // Callback argument with notified scope: C calls the delegate once and then the destroy notify
  { id: "cb_notified", lenName: "n", lenLocal: true,  supports: &allTypes, transfers: &noneTransfer,
    ret: "void", args: "${cbType} func, gpointer user_data, GDestroyNotify destroy",
    paramDocs: " * @func: (scope notified) (closure user_data) (destroy destroy): Callback\n * @user_data: User data\n"
      ~ " * @destroy: Destroy notify for @user_data\n",
    body: "${lenDecl}  ${type} value = ${makeOut};\n  func (${lenCallB}value${lenCallA}, user_data);\n  destroy (user_data);\n",
    sections: [{"cbTypedef", "/**\n * ${cbType}:\n * @value: ${annots}Value passed to the callback\n${lenDoc}"
      ~ " * @user_data: (closure): User data\n *\n * Matrix callback.\n */\n"
      ~ "typedef void (*${cbType}) (${lenB}${type} value${lenA}, gpointer user_data);\n\n"}] },

  // Callback argument with async scope: C calls the delegate once (which frees it)
  { id: "cb_async", lenName: "n", lenLocal: true,  supports: &allTypes, transfers: &noneTransfer,
    ret: "void", args: "${cbType} func, gpointer user_data",
    paramDocs: " * @func: (scope async) (closure user_data): Callback\n * @user_data: User data\n",
    body: "${lenDecl}  ${type} value = ${makeOut};\n  func (${lenCallB}value${lenCallA}, user_data);\n",
    sections: [{"cbTypedef", "/**\n * ${cbType}:\n * @value: ${annots}Value passed to the callback\n${lenDoc}"
      ~ " * @user_data: (closure): User data\n *\n * Matrix callback.\n */\n"
      ~ "typedef void (*${cbType}) (${lenB}${type} value${lenA}, gpointer user_data);\n\n"}] },

  // Signal parameter: D passes the In value to the C function which emits it with a signal connected in D
  { id: "sig_arg", supports: &signalTypes, transfers: &noneTransfer, nullable: true, member: "${sigName}",
    ret: "void", args: "GidgenMatrixSignals *self, ${type} value",
    paramDocs: " * @self: The signals object\n * @value: ${annots}Value to emit\n",
    body: "  g_signal_emit (self, matrix_signals[${SIG}], 0, value);\n",
    sections: [
      {"sigEnum", "  ${SIG},\n"},
      {"sigNew", "  matrix_signals[${SIG}] = g_signal_new (\"${sigName}\", G_TYPE_FROM_CLASS (klass), G_SIGNAL_RUN_LAST,"
        ~ " 0, NULL, NULL, NULL, G_TYPE_NONE, 1, ${sigGtype});\n"},
      {"sigDocs", "/**\n * GidgenMatrixSignals::${sigName}:\n * @self: The object\n * @value: ${sigAnnots}The value\n"
        ~ " *\n * Matrix signal.\n */\n\n"},
    ] },

  // Signal return value: the C function emits a signal whose D handler returns the Ret value and returns it
  // (transfer full for pointer types, since g_signal_emit() returns a copy of the handler's return value)
  { id: "sig_ret", supports: &signalTypes, transfers: &fullTransfer, nullable: true, member: "${sigName}",
    ret: "${type}", args: "GidgenMatrixSignals *self",
    paramDocs: " * @self: The signals object\n", returnDoc: " * Returns: ${annots}Value returned by the signal handler\n",
    body: "  ${type} value = 0;\n  g_signal_emit (self, matrix_signals[${SIG}], 0, &value);\n  return value;\n",
    sections: [
      {"sigEnum", "  ${SIG},\n"},
      {"sigNew", "  matrix_signals[${SIG}] = g_signal_new (\"${sigName}\", G_TYPE_FROM_CLASS (klass), G_SIGNAL_RUN_LAST,"
        ~ " 0, NULL, NULL, NULL, ${sigGtype}, 0);\n"},
      {"sigDocs", "/**\n * GidgenMatrixSignals::${sigName}:\n * @self: The object\n *\n * Matrix signal.\n *\n"
        ~ " * Returns: ${sigRetAnnots}The value\n */\n\n"},
    ] },

  // Signal out parameter: the C function emits a pointer to its value which the D signal handler sets to the In
  // value (pointer signal parameter typed by its GIR annotations)
  { id: "sig_out", val: "value", supports: &sigOutTypes, transfers: &noneTransfer, member: "${sigName}",
    ret: "void", args: "GidgenMatrixSignals *self",
    paramDocs: " * @self: The signals object\n",
    body: "  ${type} value = 0;\n  g_signal_emit (self, matrix_signals[${SIG}], 0, &value);\n  CHECK (${checkIn});\n",
    sections: [
      {"sigEnum", "  ${SIG},\n"},
      {"sigNew", "  matrix_signals[${SIG}] = g_signal_new (\"${sigName}\", G_TYPE_FROM_CLASS (klass), G_SIGNAL_RUN_LAST,"
        ~ " 0, NULL, NULL, NULL, G_TYPE_NONE, 1, G_TYPE_POINTER);\n"},
      {"sigDocs", "/**\n * GidgenMatrixSignals::${sigName}:\n * @self: The object\n"
        ~ " * @value: (out) (type ${cType}): The value set by the handler\n *\n * Matrix signal.\n */\n\n"},
    ] },

  // Signal inout parameter: the C function emits a pointer to the Out value which the D signal handler checks and
  // replaces with the In value
  { id: "sig_inout", val: "value", supports: &sigOutTypes, transfers: &noneTransfer, member: "${sigName}",
    ret: "void", args: "GidgenMatrixSignals *self",
    paramDocs: " * @self: The signals object\n",
    body: "  ${type} value = ${makeOut};\n  g_signal_emit (self, matrix_signals[${SIG}], 0, &value);\n"
      ~ "  CHECK (${checkIn});\n",
    sections: [
      {"sigEnum", "  ${SIG},\n"},
      {"sigNew", "  matrix_signals[${SIG}] = g_signal_new (\"${sigName}\", G_TYPE_FROM_CLASS (klass), G_SIGNAL_RUN_LAST,"
        ~ " 0, NULL, NULL, NULL, G_TYPE_NONE, 1, G_TYPE_POINTER);\n"},
      {"sigDocs", "/**\n * GidgenMatrixSignals::${sigName}:\n * @self: The object\n"
        ~ " * @value: (inout) (type ${cType}): The value checked and replaced by the handler\n *\n"
        ~ " * Matrix signal.\n */\n\n"},
    ] },
];
