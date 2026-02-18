module gir.type_node;

import defs;
import gir.alias_;
public import gir.base;
import gir.field;
import gir.func;
import gir.param;
import gir.return_value;
import gir.structure;
import import_manager;
import std_includes;
import utils;

/// Type information class
/// Combines multiple Gir information (type, array, ownership, etc)
class TypeNode : Base
{
  this(Base parent)
  {
    super(parent);
  }

  this(Base parent, XmlNode node)
  {
    super(parent);
    this.fromXml(node);
  }

  override @property dstring name()
  {
    auto ptn = cast(TypeNode)parent;
    if (ptn && ptn.elemTypes.canFind(this))
      return "[" ~ ptn.elemTypes.countUntil(this).to!dstring ~ "]";

    return super.name;
  }

  override @property void name(dstring val)
  {
    super.name(val);
  }

  /// Get the D type string
  @property dstring dType()
  {
    if (importManager)
    {
      if (containerType != ContainerType.None)
      {
        foreach (elem; elemTypes)
          if (elem.typeObject)
            importManager.add(elem.typeObject.fullModuleName);
      }
      else if (typeObject)
        importManager.add(typeObject.fullModuleName);
      else if (inModule || inGlobal)
        importManager.add(fullModuleName);
    }

    return _dType;
  }

  /// Set the D type string
  @property void dType(dstring val)
  {
    _dType = val;
  }

  /// D type name
  override @property dstring dName()
  {
    return _dType;
  }

  /**
   * Get the fundamental root type object. De-references type aliases until the root type is found.
   * Returns: The fundamental root de-aliased type
   */
  @property TypeNode typeObjectRoot()
  {
    TypeNode tn = typeObject;

    foreach (i; 0 .. 4) // Resolve aliases (up to 4 dereferences)
    {
      if (auto al = cast(Alias)tn)
      {
        if (auto nextTn = tn.typeRepo.typeObjectHash.get(al._dType, null))
        {
          tn = nextTn;
          continue;
        }
      }

      break;
    }

    return tn;
  }

  /// Get the type kind of a type node
  @property TypeKind kind()
  { // HACK - Return the referenced type kind if it has changed (kind override)
    if (typeObject && _kind != TypeKind.Unknown && typeObject._kind != TypeKind.Unknown && _kind != typeObject._kind)
      return typeObject._kind;

    return _kind;
  }

  /// Set the type kind of a type node
  @property void kind(TypeKind k)
  {
    _kind = k;
    updateUnresolvedFlags(UnresolvedFlags.Kind, _kind == TypeKind.Unknown); // Set or clear unresolved flag
  }

  /// Check if this type is defined in its own module file
  @property bool inModule()
  {
    return false; // Overridden by subclasses such as Structure
  }

  /// Check if this type is defined in the repository global module
  @property bool inGlobal()
  {
    with (TypeKind) return kind.among(BasicAlias, Enum, Flags, StructAlias, Callback) != 0;
  }

  /**
   * Get the module structure which a type node is in.
   * Returns: The module or null
   */
  @property Structure typeModule()
  {
    for (Base n = this; n; n = n.parent) // Find structure whose parent is the repository (top-level structure/class)
      if (auto st = cast(Structure)n)
        if (st.parent == repo)
          return st;

    return null;
  }

  /**
   * Get the module name which a type node is in.
   * Returns: The module name or null
   */
  @property dstring typeModuleName()
  {
    auto mod = typeModule;
    return mod ? mod.moduleName : null;
  }

  /**
   * Get full module name of a type node with the package namespace directory separated by a period
   * Returns: The fully qualified module name
   */
  @property dstring fullModuleName()
  {
    return repo.packageNamespace ~ "." ~ typeModuleName;
  }

  /// Check if type has been resolved
  @property bool resolved()
  {
    return unresolvedFlags == 0;
  }

  /// Full name of the D type with the GIR namespace followed by a period and then the D type
  dstring fullGirType()
  {
    if (typeRepo && !_dType.empty)
      return typeRepo.namespace ~ "." ~ _dType;

    return origDType.canFind('.') ? origDType : repo.namespace ~ "." ~ origDType;
  }

  /**
   * Get full D type including package "namespace", module, and type separated by periods.
   * Returns; Fully qualified D type string
   */
  dstring fullDType()
  {
    if (containerType == ContainerType.HashTable)
      return elemTypes.length == 2 ? (elemTypes[1].fullDType ~ "[" ~ elemTypes[0].fullDType ~ "]") : dType;
    else if (containerType != ContainerType.None)
      return elemTypes.length == 1 ? (elemTypes[0].fullDType ~ "[]") : dType;
    else if (typeObject)
      return typeObject.fullModuleName ~ "." ~ dType;
    else if (!inModule && !inGlobal)
      return dType;
    else
      return fullModuleName ~ "." ~ dType;
  }

  /**
   * Check if other type node is the same type.
   * Returns: true if TypeNode objects are the same type, false otherwise
   */
  bool typeEqual(TypeNode other)
  {
    return (typeObject is null && other.typeObject is null && fullDType == other.fullDType)
      || (typeObject && typeObject is other.typeObject);
  }

  /// cType with a single '*' removed (if it has any)
  dstring cTypeRemPtr()
  {
    if (!cType.endsWith("*"))
      return cType;

    if (cType.length > 3 && cType[$ - 3 .. $ - 1] == "*)") // Move * out of const(TYPE*) parenthesis if it is the last one
      return cType[0 .. $ - 3] ~ ")*";

    return cType[0 .. $ - 1];
  }

  /// Returns "Yes" if ownership is Full, "No" otherwise (helper function to use with Flags)
  dstring fullOwnerFlag()
  {
    return ownership == Ownership.Full ? "Yes"d : "No"d;
  }

  /**
   * Method which returns a descriptive string of an array size.
   * Returns: null if not an array, "length" if length is specified, "fixed" if fixed size, "unknown" if array size unknown.
   *   If length is specified or fixed size, and the array is zero terminated, "-zero" is appended to "length" or "fixed".
   */
  string arraySizeStr()
  {
    if (containerType != ContainerType.Array)
      return null;

    if (zeroTerminated)
    {
      if (lengthParamIndex != ArrayLengthUnset)
        return "length-zero";
      else if (fixedSize != ArrayNotFixed)
        return "fixed-zero";
      else
        return "zero";
    }

    if (lengthParamIndex == ArrayLengthCaller)
      return "caller";
    else if (lengthParamIndex != ArrayLengthUnset)
      return "length";
    else if (fixedSize != ArrayNotFixed)
      return "fixed";
    else
      return "unknown";
  }

  /**
   * Update unresolved flags and add/remove to/from Defs.unresolvedTypes as appropriate.
   * Params:
   *   flags = The unresolved flags to set or clear
   *   set = true to set the flags, false to clear them
   */
  void updateUnresolvedFlags(UnresolvedFlags flags, bool set)
  {
    if (!set)
    {
      if (unresolvedFlags)
      {
        unresolvedFlags &= ~flags;

        if (unresolvedFlags)
          repo.defs.unresolvedTypes[this] = unresolvedFlags;
        else
          repo.defs.unresolvedTypes.remove(this);
      }
    }
    else
    {
      unresolvedFlags |= flags;
      repo.defs.unresolvedTypes[this] = unresolvedFlags;
    }
  }

  override void fromXml(XmlNode node)
  {
    super.fromXml(node);

    containerType = ContainerType.None;

    if (auto typ = node.findChild("type")) // Is there a child <type> node?
    {
      _dType = origDType = typ.get("name");
      cType = origCType = typ.get("c:type");
    }
    else if (auto arr = node.findChild("array")) // Is there a child <array> node
    {
      _dType = origDType = arr.get("name");
      cType = origCType = arr.get("c:type");

      if (_dType.empty) // Regular memory type arrays don't have a D type, only array wrappers like GArray, GPtrArray, and GByteArray do, processed in fixup()
      {
        containerType = ContainerType.Array;
        zeroTerminated = arr.get("zero-terminated", "0") == "1";

        if (auto pFixedSize = "fixed-size" in arr.attrs)
          fixedSize = (*pFixedSize).to!int;
        else
          fixedSize = ArrayNotFixed;

        if (auto pLength = "length" in arr.attrs)
          lengthParamIndex = (*pLength).to!int;
        else if (arr.get("caller-length", "0") == "1")
          lengthParamIndex = ArrayLengthCaller;
        else
          lengthParamIndex = ArrayLengthUnset;
      }

      if (_dType == "GLib.Array")
        containerType = ContainerType.ArrayG;
    }
    else // Is this a <type> node or other node with "name" and "c:type"? (Structure for example).
    {
      _dType = origDType = node.get("name");
      cType = origCType = node.get("c:type");
    }
  }

  /// Just calls fixup with a code trap before
  void doFixup()
  {
    codeTrap("type.fixup", fullDName);
    fixup;
  }

  /// Fixup independent state (only called once)
  protected void fixup()
  {
    dstring parseRepoType(dstring type, out Repo outRepo)
    {
      auto t = type.split('.');
      if (t.length > 1)
      {
        outRepo = repo.includeRepoHash.get(t[0], null);
        if (outRepo)
          return t[1];

        warnWithLoc(__FILE__, __LINE__, null, "Repo '" ~ t[0].to!string ~ "' not found for type '" ~ type.to!string ~ "'");
        type = t[1];
      }

      outRepo = repo;
      return type;
    }

    // Regular <array> containers don't have dType set, use the first element's dType so that symbol substitutions below work correctly
    if (!origDType && containerType != ContainerType.None && elemTypes.length == 1)
      origDType = elemTypes[0].origDType;

    origDType = parseRepoType(origDType, typeRepo);
    _dType = typeRepo.subTypeStr(origDType);

    if (_dType.canFind('.')) // If substituted type contains a repo name, resolve it
      _dType = parseRepoType(_dType, typeRepo);

    cType = typeRepo.subTypeStr(origCType, Yes.CType);

    foreach (typ; elemTypes) // Fixup container element types
      typ.doFixup;

    if (_dType == "void*" && cType == "const(void)*") // If dType is void* and cType is const(void)*, make the dType const as well
      _dType = "const(void)*";

    if (containerType == ContainerType.Array)
    {
      if (elemTypes.length == 1 && elemTypes[0].cType == "char"
        && lengthParamIndex != ArrayLengthUnset) // If this is a char[] array, set element type to Basic char, but FuncWriter will consider it as a string.
      {
        elemTypes[0].kind = TypeKind.Basic;
        elemTypes[0]._dType = "char";
        info("'" ~ fullDName.to!string ~ "' using string for char array with length");
      }
      else if (lengthParamIndex != ArrayLengthUnset && !elemTypes.empty && elemTypes[0]._dType == "ubyte"
        && cType.stripConst.startsWith("char")) // If there is a length parameter, dType is "ubyte", and array type uses char - treat it as a ubyte array
      {
        info("Changing array cType from " ~ cType.to!string ~ " to ubyte for " ~ fullDName.to!string);
        elemTypes[0].cType = "ubyte";
        cType = cType.replace("char", "ubyte");
      }
      else if (!elemTypes.empty && elemTypes[0].cType.empty) // Missing array element C type? Try and derive it from array C type
        deriveElemCTypeFromArrayType;
    }
    else if (ContainerTypeValues.canFind(fullGirType) && !cast(Structure)this) // Not an array, check if it is another container type and not a structure (the type itself)
      containerType = cast(ContainerType)ContainerTypeValues.countUntil(fullGirType);

    if (containerType != ContainerType.None)
    {
      kind = TypeKind.Container;
      _dType = null;
    }
  }

  // Derive array element C type from the array C type
  private void deriveElemCTypeFromArrayType()
  {
    auto elemType = cType.stripConst;
    auto count = elemType.countStars;

    if (count > 0)
    {
      elemType = elemType[0 .. $ - count]; // Strip the stars off

      if (cType.canFind("const") && count > 1)
        elemType = "const(" ~ elemType ~ "*"d.replicate(count - 2) ~ ")*";
      else
        elemType ~= "*"d.replicate(count - 1);

      if (elemType != "void")
        elemTypes[0].origCType = elemType;
    }

    if (elemTypes[0].origCType.empty && elemTypes[0]._dType.isBasicType) // Don't use kind, since it isn't resolved yet
      elemTypes[0].origCType = elemTypes[0]._dType; // Use D type if unable to derive C type from array C type, as long as D type is Basic

    if (elemTypes[0].origCType)
    {
      elemTypes[0].cType = elemTypes[0].typeRepo.subTypeStr(elemTypes[0].origCType, Yes.CType);

      info("Using member C type '" ~ elemTypes[0].cType.to!string ~ "' for D type '"
        ~ elemTypes[0]._dType.to!string ~ "' for an array with C type '" ~ cType.to!string ~ "' in "
        ~ fullDName.to!string);
    }
  }

  /// Just calls resolve with a code trap before
  void doResolve()
  {
    codeTrap("type.resolve", fullDName);
    resolve;
  }

  /// Resolve dependencies (may get called multiple times)
  protected void resolve()
  {
    if (kind != TypeKind.Container)
    {
      if (!typeObject)
        typeObject = typeRepo.typeObjectHash.get(_dType, null);

      if (kind == TypeKind.Unknown)
        kind = typeRepo.typeKind(_dType);

      if (cType.empty)
      {
        if (kind == TypeKind.String)
        {
          cType = "char*";
          info(fullDName ~ ": Using char* for missing cType");
        }
        else if (typeObject && typeObject.cType)
        {
          cType = typeObject.cType;
          info(fullDName ~ ": Using " ~ cType ~ " for missing cType");
        }
      }

      with (TypeKind) if (!kind.among(Unknown, Basic, String, Pointer, Namespace))
        _dType = _dType.normalizeDTypeName(); // Strip _t from type name and CamelCase it

      if (auto st = cast(Structure)typeObject) // Should only be set to a Structure for non-struct dependencies
      {
        if (cType == "void*" || cType == "const(void)*") // HACK? - Fix anonymous gpointer object parameters to have proper C types
        {
          cType = st.cType ~ "*";
          info(fullDName ~ ": Using '" ~ cType ~ "' for anonymous pointer cType");
        }
        else if (cType.empty && !st.cType.empty) // HACK? - Use structure cType if cType is missing
        {
          cType = st.cType ~ "*";
          info(fullDName ~ ": Using '" ~ cType ~ "' for missing cType");
        }
      }

      return;
    }

    foreach (tn; elemTypes) // Fixup container element types
    {
      tn.doResolve;
      updateUnresolvedFlags(UnresolvedFlags.Element, tn.unresolvedFlags != cast(UnresolvedFlags)0);
    }

    if (_dType.empty && (unresolvedFlags & UnresolvedFlags.Element) == 0) // If container D type has not yet been set and elements are resolved
    {
      if (containerType == ContainerType.HashTable && elemTypes.length == 2)
        _dType = elemTypes[1]._dType ~ "[" ~ elemTypes[0]._dType ~ "]"; // Uses an associative array
      else if (elemTypes.length == 1)
        _dType = elemTypes[0]._dType ~ "[]";
    }
  }

  /// Just calls verify with a code trap before
  void doVerify()
  {
    codeTrap("type.verify", fullDName);
    verify;
  }

  /// Verify type state
  protected void verify()
  {
    if (unresolvedFlags)
      throw new Exception("Unresolved type '" ~ dType.to!string ~ "' (unresolvedFlags: " ~ unresolvedFlags.to!string ~ ")");

    if (typeObject && typeObject.active != Active.Enabled)
      throw new Exception("Resolved type '" ~ typeObject.fullDName.to!string ~ "' is not active");

    foreach (typ; elemTypes)
      if (typ.containerType != ContainerType.None)
        throw new Exception("Nested container types not yet supported");

    if (containerType == ContainerType.Array)
    {
      if (elemTypes.empty)
        throw new Exception("Array type '" ~ cType.to!string ~ "' has no element type");

      if (lengthParamIndex == ArrayLengthUnset && fixedSize == ArrayNotFixed && !zeroTerminated)
      {
        if (elemTypes[0].kind != TypeKind.String)
          throw new Exception("Array of type '" ~ elemTypes[0]._dType.to!string ~ "' has indeterminate length");

        info("Setting string array to null terminated for '" ~ fullDName.to!string ~ "'");
        zeroTerminated = true;
      }

      if (elemTypes[0].cType.empty) // Missing array element C type?
        throw new Exception("Could not determine member type for array type '" ~ cType.to!string ~ "'");

      if (cType.empty && fixedSize == 0) // No array C type and not fixed size?
      { // Just append a * on the element cType
        cType = elemTypes[0].cType ~ "*";
        info("Setting array c:type to '" ~ cType.to!string ~ "' in '" ~ fullDName.to!string ~ "'");
      }

      if (elemTypes[0]._dType == "ubyte" && cType.canFind("char"))
        throw new Exception("Unsure if array is a null terminated string or not for array cType "
          ~ cType.to!string ~ " element cType " ~ elemTypes[0].cType.to!string ~ " dType "
          ~ elemTypes[0]._dType.to!string);
    }

    if (containerType != ContainerType.None)
    {
      auto reqElemTypes = containerTypeElemCount(containerType);

      if (elemTypes.length < reqElemTypes)
        throw new Exception(fullGirType.to!string ~ " requires " ~ reqElemTypes.to!string ~ " container type(s), found "
            ~ elemTypes.length.to!string);

      if (elemTypes.length > reqElemTypes)
        warnWithLoc(__FILE__, __LINE__, xmlLocation, "Container '" ~ fullGirType.to!string ~ "' has excess types");

      foreach (elem; elemTypes) // Check if element types are active
        if (elem.typeObject && elem.typeObject.active != Active.Enabled)
          throw new Exception("Container type '" ~ containerType.to!string ~ "' element type '" ~ elem.dType.to!string
            ~ "' is " ~ elem.typeObject.active.to!string);

      if (containerType == ContainerType.HashTable)
      {
        with (TypeKind) if (!elemTypes[0].kind.among(String, Pointer))
          throw new Exception("Unsupported hash table key type " ~ elemTypes[0].dType.to!string ~ " ("
            ~ elemTypes[0].kind.to!string ~ ")");

        with (TypeKind) if (!elemTypes[1].kind.among(Object, Interface, String, Pointer))
          throw new Exception("Unsupported hash table value type " ~ elemTypes[1].dType.to!string ~ " ("
            ~ elemTypes[1].kind.to!string ~ ")");
      }
      else if (containerType != ContainerType.None)
      {
        with (TypeKind) if (elemTypes[0].kind.among(Unknown, Callback, Container, Opaque, Wrap, Namespace))
          throw new Exception("Unsupported " ~ containerType.to!string ~ " container element type "
            ~ elemTypes[0].dType.to!string ~ " (" ~ elemTypes[0].kind.to!string ~ ")");
      }
    }

    if (containerType == ContainerType.None && kind != TypeKind.Namespace)
    {
      if (cType.empty && kind != TypeKind.Callback) // Has no C type and not a callback?
      {
        auto parentTypeNode = cast(TypeNode)parent;
        if (!parentTypeNode || parentTypeNode.containerType != ContainerType.Array) // Warn if not an array container type (handled separately)
        {
          warnWithLoc(__FILE__, __LINE__, xmlLocation, "No c:type for D type '" ~ _dType.to!string ~ "' in '" ~ fullDName.to!string ~ "'");
          dumpSelectorOnWarning(this);
        }
      }

      if (kind.among(TypeKind.Unknown, TypeKind.Namespace))
        throw new Exception("unhandled type kind '" ~ kind.to!string ~ "' for type '"
            ~ _dType.to!string ~ "'");

      if (!elemTypes.empty)
      {
        warnWithLoc(__FILE__, __LINE__, xmlLocation, "Unexpected element type in unrecognized container '" ~ fullDName.to!string ~ "'");
        dumpSelectorOnWarning(this);
      }
    }
  }

  static void dumpSelectorOnWarning(TypeNode node)
  {
    if (dumpSelectorWarnings)
      writeln("//!set " ~ node.xmlSelector);
  }

  override void toJson(ref JSONValue js)
  {
    super.toJson(js);

    js["typeRepo"] = typeRepo ? typeRepo.dName : ""d;
    js["dType"] = _dType;
    js["cType"] = cType;
    js["origDType"] = origDType;
    js["origCType"] = origCType;
    js["kind"] = _kind.to!string;

    if (typeObject)
      js["typeObject"] = typeObject.fullDType;

    if (elemTypes.length > 0)
      js["elemTypes"] = elemTypes.map!(x => x.fullDType).array;

    js.jsonSetNonDefault("ownership", ownership.to!string, Ownership.Unset.to!string);

    if (lengthParam)
      js["lengthParam"] = lengthParam.fullDName;

    if (lengthReturn)
      js["lengthReturn"] = lengthReturn.fullDName;

    js.jsonSetNonDefault("unresolvedFlags", cast(uint)unresolvedFlags);

    if (containerType != ContainerType.None)
    {
      js["containerType"] = containerType.to!string;
      js["zeroTerminated"] = zeroTerminated;
      js.jsonSetNonDefault("fixedSize", fixedSize, ArrayNotFixed);
      js.jsonSetNonDefault("lengthParamIndex", lengthParamIndex, ArrayLengthUnset);
    }
  }

  Repo typeRepo; /// Repo containing the dType (can be this.repo, will always be valid after fixup() is called)
  dstring _dType; /// D type (container type for containers, Gir "name"), is accessed directly by ImportManager
  dstring cType; /// C type (container type for containers)
  dstring origDType; /// Original D type (before substitution)
  dstring origCType; /// Original C type (before substitution)
  private TypeKind _kind; /// Type kind
  TypeNode typeObject; /// Resolved type object for dType
  TypeNode[] elemTypes; /// Container element types (2 for HashTable, 1 for other container types)
  Ownership ownership; /// Ownership of passed value (return values, parameters, and properties)
  ContainerType containerType; /// The type of container or None
  Param lengthParam; /// Set to a length parameter for arrays
  ReturnValue lengthReturn; /// Set to length return value for arrays
  UnresolvedFlags unresolvedFlags; /// Flags of what type references are unresolved (0 if none)
  bool zeroTerminated; /// true if array is zero terminated
  int fixedSize = ArrayNotFixed; /// Non-zero if array is a fixed size
  int lengthParamIndex = ArrayLengthUnset; /// >= 0 if a length parameter index is set, -1 (ArrayLengthReturn) if length is return value (GIR non-standard extension)

  static bool dumpSelectorWarnings; /// Enable dumping of XML selectors for warnings
}

enum ArrayNotFixed = 0; /// Value for TypeNode.fixedSize which indicates size is not fixed

enum ArrayLengthReturn = -1; /// Value used for TypeNode.lengthParamIndex which indicates no length parameter
enum ArrayLengthUnset = -2; /// Value used for TypeNode.lengthParamIndex which indicates no length parameter
enum ArrayLengthCaller = -3; /// Caller is responsible for defining the proper length (callerAllocates out/inout buffer)

/// Ownership transfer of a type
enum Ownership
{
  Unset = -1, /// Ownership not specified
  None, /// No transfer of ownership
  Container, /// Transfer container ownership
  Full, /// Transfer container and values
}

immutable dstring[] OwnershipValues = ["none", "container", "full"];

/// Kind of a type
enum TypeKind
{
  Unknown, /// Unknown type
  Basic, /// A basic data type
  String, /// A string
  BasicAlias, /// An alias to a basic type
  Enum, /// Enumeration type
  Flags, /// Flags type
  Callback, /// Callback function type
  Container, /// A container type
  StructAlias, /// Record or Union with basic fields (Basic, Enum, Flags) and no methods (alias to C type in types.d)
  Struct, /// Record or Union with basic fields (Basic, Enum, Flags) and methods (module with identical C struct definition)
  Pointer, /// Opaque Record pointer type with no accessible fields or methods (alias to C type)
  Opaque, /// Opaque Record pointer wrapped by a D class with methods
  Wrap, /// Record or Union wrapped by a D class with defined fields and/or methods
  Boxed, /// A GLib boxed Record type which can have fields
  Reffed, /// Referenced Class type with possible inheritance (not GObject derived), can have fields
  Object, /// A GObject Class
  Interface, /// Interface type
  Namespace, /// Namespace structure (no C type, global module for example)
}

/**
 * Check if a TypeKind is a structured type.
 * Params:
 *   kind = The TypeKind enum to check
 * Returns: true if kind is structured
 */
bool typeKindIsStructured(TypeKind kind)
{
  with (TypeKind) return kind.among(StructAlias, Struct, Pointer, Opaque, Wrap, Boxed, Reffed, Object, Interface) != 0;
}

/// Container type
enum ContainerType
{
  None = -2, /// No container
  Array = -1, /// Memory array
  ByteArray, /// GByteArray (uses <array>), enforces element type to be ubyte, does not use a template type
  ArrayG, /// GArray (uses <array>)
  PtrArray, /// GPtrArray (uses <array>)
  List, /// GList
  SList, /// GSList
  HashTable, /// GHashTable (has 2 element types for the key and value)
}

/// Container type string values matching ContainerType
immutable dstring[] ContainerTypeValues =
  ["GLib.ByteArray", "GLib.Array", "GLib.PtrArray", "GLib.List", "GLib.SList", "GLib.HashTable"];

long containerTypeElemCount(ContainerType container)
{
  if (container == ContainerType.None)
    return 0;
  else if (container == ContainerType.HashTable)
    return 2;
  else
    return 1;
}

/**
 * Get the C structure type for a container
 * Params:
 *   type = The container type
 * Returns: The C type name or null if no C structure for given type
 */
dstring containerTypeCType(ContainerType type)
{
  switch (type) with (ContainerType)
  {
    case ArrayG:
      return "GArray";
    case PtrArray:
      return "GPtrArray";
    case List:
      return "GList";
    case SList:
      return "GSList";
    case HashTable:
      return "GHashTable";
    default:
      return null;
  }
}

/// Basic type names
immutable string[] BasicTypeValues = [
  "bool", "byte", "char", "dchar", "double", "float", "glong", "gulong", "int", "long", "ptrdiff_t", "real", "short", // glong/gulong are versioned alias types which change depending on if Windows or not
  "size_t", "ubyte", "uint", "ulong", "ushort", "void*", "void"
];

/// Hash of basic type names to true values
immutable bool[dstring] BasicTypeHash;

shared static this()
{
  foreach (s; BasicTypeValues)
    BasicTypeHash[s.to!dstring] = true;
}

/**
 * Is the type string a basic type?
 * Params:
 *   type = The type string
 * Returns: true if type is a basic type, false otherwise
 */
bool isBasicType(dstring type)
{
  return BasicTypeHash.get(type.stripConstPtr, false);
}

/// Flags which indicate what type references are unresolved
enum UnresolvedFlags
{
  Kind = 1 << 0, /// The TypeNode kind is unresolved
  ParentStruct = 1 << 1, /// The parent structure is unresolved
  Implements = 1 << 2, /// Not all implementation interfaces are resolved
  Element = 1 << 3, /// A container element is unresolved
}

// Test fixture infrastructure - only compiled when running unit tests
version (unittest)
{
  // Test fixture for creating TypeNode instances with proper Repo/Defs setup
  struct TypeNodeFixture
  {
    Defs defs;
    Repo repo;

    // Static opCall is called when TypeNodeFixture() is used
    static TypeNodeFixture opCall()
    {
      TypeNodeFixture fixture;
      fixture.defs = new Defs();
      fixture.repo = new Repo(fixture.defs);
      return fixture;
    }

    // Create a TypeNode with proper parent hierarchy
    TypeNode createNode()
    {
      return new TypeNode(repo);
    }

    // Create multiple TypeNode instances sharing the same Repo
    TypeNode[] createNodes(size_t count)
    {
      TypeNode[] nodes;
      foreach (_; 0 .. count)
        nodes ~= createNode();
      return nodes;
    }
  }
}

// Test typeKindIsStructured function with all structured type kinds
unittest
{
  // Test structured types
  assert(typeKindIsStructured(TypeKind.StructAlias));
  assert(typeKindIsStructured(TypeKind.Struct));
  assert(typeKindIsStructured(TypeKind.Pointer));
  assert(typeKindIsStructured(TypeKind.Opaque));
  assert(typeKindIsStructured(TypeKind.Wrap));
  assert(typeKindIsStructured(TypeKind.Boxed));
  assert(typeKindIsStructured(TypeKind.Reffed));
  assert(typeKindIsStructured(TypeKind.Object));
  assert(typeKindIsStructured(TypeKind.Interface));
  
  // Test non-structured types
  assert(!typeKindIsStructured(TypeKind.Unknown));
  assert(!typeKindIsStructured(TypeKind.Basic));
  assert(!typeKindIsStructured(TypeKind.String));
  assert(!typeKindIsStructured(TypeKind.BasicAlias));
  assert(!typeKindIsStructured(TypeKind.Enum));
  assert(!typeKindIsStructured(TypeKind.Flags));
  assert(!typeKindIsStructured(TypeKind.Callback));
  assert(!typeKindIsStructured(TypeKind.Container));
  assert(!typeKindIsStructured(TypeKind.Namespace));
}

// Test containerTypeElemCount function for all container types
unittest
{
  assert(containerTypeElemCount(ContainerType.None) == 0);
  assert(containerTypeElemCount(ContainerType.HashTable) == 2);
  assert(containerTypeElemCount(ContainerType.Array) == 1);
  assert(containerTypeElemCount(ContainerType.ByteArray) == 1);
  assert(containerTypeElemCount(ContainerType.ArrayG) == 1);
  assert(containerTypeElemCount(ContainerType.PtrArray) == 1);
  assert(containerTypeElemCount(ContainerType.List) == 1);
  assert(containerTypeElemCount(ContainerType.SList) == 1);
}

// Test containerTypeCType function for all container types
unittest
{
  assert(containerTypeCType(ContainerType.ArrayG) == "GArray");
  assert(containerTypeCType(ContainerType.PtrArray) == "GPtrArray");
  assert(containerTypeCType(ContainerType.List) == "GList");
  assert(containerTypeCType(ContainerType.SList) == "GSList");
  assert(containerTypeCType(ContainerType.HashTable) == "GHashTable");
  assert(containerTypeCType(ContainerType.None) is null);
  assert(containerTypeCType(ContainerType.Array) is null);
  assert(containerTypeCType(ContainerType.ByteArray) is null);
}

// Test isBasicType function with various basic and non-basic types
unittest
{
  // Test basic types
  assert(isBasicType("int"));
  assert(isBasicType("uint"));
  assert(isBasicType("long"));
  assert(isBasicType("ulong"));
  assert(isBasicType("short"));
  assert(isBasicType("ushort"));
  assert(isBasicType("byte"));
  assert(isBasicType("ubyte"));
  assert(isBasicType("char"));
  assert(isBasicType("double"));
  assert(isBasicType("float"));
  assert(isBasicType("bool"));
  assert(isBasicType("void"));
  assert(isBasicType("void*"));
  assert(isBasicType("size_t"));
  assert(isBasicType("ptrdiff_t"));
  assert(isBasicType("dchar"));
  assert(isBasicType("real"));
  assert(isBasicType("glong"));
  assert(isBasicType("gulong"));
  
  // Test with const prefix
  assert(isBasicType("const(int)"));
  assert(isBasicType("const(char)"));
  assert(isBasicType("const(void*)"));
  
  // Test with pointer suffix
  assert(isBasicType("int*"));
  assert(isBasicType("char*"));
  
  // Test non-basic types
  assert(!isBasicType("GtkWidget"));
  assert(!isBasicType("gchar"));
  assert(!isBasicType("MyCustomType"));
  assert(!isBasicType("SomeStruct"));
}

// Test isBasicType with edge cases and const/pointer variations
unittest
{
  // Test with various const and pointer combinations
  assert(isBasicType("const(int*)"));
  assert(isBasicType("const(char*)"));
  assert(isBasicType("const(void)**"));
  
  // Test edge cases
  assert(isBasicType("int"));
  assert(!isBasicType(""));
  assert(!isBasicType("nonexistent_type"));
}

// Test TypeNode.cTypeRemPtr method with various C type strings
unittest
{
  // Create TypeNode with proper test fixture
  auto fixture = TypeNodeFixture();
  auto node = fixture.createNode();
  
  // Test: simple pointer removal
  node.cType = "char*";
  assert(node.cTypeRemPtr == "char");
  
  // Test: const pointer
  node.cType = "const(char)*";
  assert(node.cTypeRemPtr == "const(char)");
  
  // Test: GtkWidget pointer
  node.cType = "GtkWidget*";
  assert(node.cTypeRemPtr == "GtkWidget");
  
  // Test: const with parenthesis - moves * out
  node.cType = "const(GtkWidget*)*";
  assert(node.cTypeRemPtr == "const(GtkWidget)*");
  
  // Test: double pointer
  node.cType = "char**";
  assert(node.cTypeRemPtr == "char*");
  
  // Test: no pointer (returns as-is)
  node.cType = "int";
  assert(node.cTypeRemPtr == "int");
  
  // Test: triple pointer
  node.cType = "void***";
  assert(node.cTypeRemPtr == "void**");
}

// Test TypeNode.fullOwnerFlag method returns correct string representation
unittest
{
  auto fixture = TypeNodeFixture();
  auto node = fixture.createNode();
  
  // Test: Full ownership returns "Yes"
  node.ownership = Ownership.Full;
  assert(node.fullOwnerFlag == "Yes");
  
  // Test: None ownership returns "No"
  node.ownership = Ownership.None;
  assert(node.fullOwnerFlag == "No");
  
  // Test: Container ownership returns "No"
  node.ownership = Ownership.Container;
  assert(node.fullOwnerFlag == "No");
  
  // Test: Unset ownership returns "No"
  node.ownership = Ownership.Unset;
  assert(node.fullOwnerFlag == "No");
}

// Test TypeNode.arraySizeStr method with different array configurations
unittest
{
  auto fixture = TypeNodeFixture();
  auto node = fixture.createNode();
  
  // Test: non-array type returns null
  node.containerType = ContainerType.None;
  assert(node.arraySizeStr is null);
  
  // Test: array with length parameter
  node.containerType = ContainerType.Array;
  node.lengthParamIndex = 0;
  node.zeroTerminated = false;
  node.fixedSize = ArrayNotFixed;
  assert(node.arraySizeStr == "length");
  
  // Test: fixed-size array
  node.lengthParamIndex = ArrayLengthUnset;
  node.fixedSize = 10;
  assert(node.arraySizeStr == "fixed");
  
  // Test: zero-terminated array
  node.fixedSize = ArrayNotFixed;
  node.zeroTerminated = true;
  assert(node.arraySizeStr == "zero");
  
  // Test: length + zero-terminated
  node.lengthParamIndex = 1;
  assert(node.arraySizeStr == "length-zero");
  
  // Test: fixed + zero-terminated
  node.lengthParamIndex = ArrayLengthUnset;
  node.fixedSize = 5;
  assert(node.arraySizeStr == "fixed-zero");
  
  // Test: caller-length
  node.fixedSize = ArrayNotFixed;
  node.zeroTerminated = false;
  node.lengthParamIndex = ArrayLengthCaller;
  assert(node.arraySizeStr == "caller");
  
  // Test: unknown size
  node.lengthParamIndex = ArrayLengthUnset;
  assert(node.arraySizeStr == "unknown");
}

// Test TypeNode.typeEqual method for comparing type nodes
unittest
{
  auto fixture = TypeNodeFixture();
  auto nodes = fixture.createNodes(3);
  auto node1 = nodes[0];
  auto node2 = nodes[1];
  auto node3 = nodes[2];
  
  // Test: both have no typeObject and same dType
  node1._dType = "int";
  node2._dType = "int";
  assert(node1.typeEqual(node2));
  
  // Test: both have no typeObject but different dType
  node3._dType = "char";
  assert(!node1.typeEqual(node3));
  
  // Test: both have same typeObject
  node1.typeObject = node1;
  node2.typeObject = node1;
  assert(node1.typeEqual(node2));
  
  // Test: different typeObject
  node2.typeObject = node3;
  assert(!node1.typeEqual(node2));
  
  // Test: one has typeObject, other doesn't
  node2.typeObject = null;
  assert(!node1.typeEqual(node2));
  
  // Test: both have null typeObject
  node1.typeObject = null;
  node2.typeObject = null;
  node1._dType = "void*";
  node2._dType = "void*";
  assert(node1.typeEqual(node2));
}

// Test TypeKind enum values and their usage
unittest
{
  // Verify TypeKind enum has all expected values
  assert(TypeKind.Unknown == 0);
  assert(TypeKind.Basic != TypeKind.Unknown);
  assert(TypeKind.String != TypeKind.Basic);
  
  // Verify that among() works with TypeKind
  with (TypeKind)
  {
    assert(Basic.among(Basic, String, Pointer) != 0);
    assert(Basic.among(String, Pointer) == 0);
    assert(Enum.among(Enum, Flags, StructAlias) != 0);
  }
}

// Test ContainerType enum values and relationships
unittest
{
  // Verify ContainerType enum ordering
  assert(ContainerType.None == -2);
  assert(ContainerType.Array == -1);
  assert(ContainerType.ByteArray == 0);
  
  // Verify ContainerTypeValues array matches enum
  assert(ContainerTypeValues.length == 6);
  assert(ContainerTypeValues[0] == "GLib.ByteArray");
  assert(ContainerTypeValues[$ - 1] == "GLib.HashTable");
}

// Test Ownership enum values and their usage
unittest
{
  // Verify Ownership enum values
  assert(Ownership.Unset == -1);
  assert(Ownership.None == 0);
  assert(Ownership.Container != Ownership.Full);
  
  // Verify OwnershipValues array
  assert(OwnershipValues.length == 3);
  assert(OwnershipValues[0] == "none");
  assert(OwnershipValues[1] == "container");
  assert(OwnershipValues[2] == "full");
}

// Test UnresolvedFlags enum bitmask operations
unittest
{
  // Test that flags can be combined with bitwise OR
  auto flags = UnresolvedFlags.Kind | UnresolvedFlags.Element;
  assert((flags & UnresolvedFlags.Kind) != 0);
  assert((flags & UnresolvedFlags.Element) != 0);
  assert((flags & UnresolvedFlags.ParentStruct) == 0);
  
  // Test individual flags
  assert(UnresolvedFlags.Kind == 1);
  assert(UnresolvedFlags.ParentStruct == 2);
  assert(UnresolvedFlags.Implements == 4);
  assert(UnresolvedFlags.Element == 8);
}

// Test array-related constants
unittest
{
  // Verify array constants
  assert(ArrayNotFixed == 0);
  assert(ArrayLengthReturn == -1);
  assert(ArrayLengthUnset == -2);
  assert(ArrayLengthCaller == -3);
}
