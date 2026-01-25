module gir.field;

import std.conv : to, ConvException;

import defs;
import gir.func;
import gir.structure;
import gir.type_node;
import utils;

/// Field in a structure
final class Field : TypeNode
{
  this(Base parent, XmlNode node)
  {
    super(parent);
    fromXml(node);
  }

  override @property dstring name()
  {
    return _name;
  }

  override @property void name(dstring val)
  {
    _name = val;
  }

  // Get field name and handle reserved words by adding an underscore to the field name
  @property dstring subName()
  {
    return repo.defs.symbolName(_name);
  }

  /// Get the field name formatted in D camelCase
  override dstring dName()
  {
    return repo.defs.symbolName(_name.camelCase);
  }

  override void fromXml(XmlNode node)
  {
    super.fromXml(node);

    _name = node.get("name");
    readable = node.get("readable", "1") == "1";
    writable = node.get("writable", "0") == "1";
    introspectable = node.get("introspectable", "1") == "1";
    private_ = node.get("private") == "1";

    try
      bits = node.get("bits", "0").to!ubyte;
    catch (ConvException e)
      warnWithLoc(__FILE__, __LINE__, xmlLocation, "Invalid field bits value '" ~ node.get("bits").to!string ~ "'");
  }

  protected override void fixup()
  {
    if (private_ || !introspectable) // Ignore private or non-introspectable fields
      active = Active.Ignored;

    if (directStruct) // Embedded structure?
    {
      directStruct.doFixup;
      return;
    }

    super.fixup;

    if (callback) // Embedded callback type
    {
      cType = origCType = null;
      dType = origDType = null;
      kind = TypeKind.Callback;
      callback.doFixup;
    }
    else if (kind == TypeKind.Callback)
      callback = cast(Func)typeObject;
  }

  protected override void resolve()
  {
    if (directStruct) // Embedded structure?
    {
      directStruct.doResolve;
      kind = directStruct.kind;
      active = Active.Ignored; // Embedded structure properties not supported
      return;
    }

    super.resolve;

    if (callback) // Embedded callback type
      callback.doResolve;
  }

  protected override void verify()
  {
    if (active != Active.Enabled)
      return;

    if (directStruct)
    {
      directStruct.doVerify;
      return;
    }

    super.verify;

    auto starCount = cType.countStars;

    if (kind == TypeKind.String && starCount > 1)
      throw new Exception("Array of strings not supported");

    with(TypeKind) if ((kind.among(Basic, BasicAlias, Enum, Flags, Callback) && starCount != 0)
        || (kind.among(String, StructAlias, Struct, Pointer, Opaque, Wrap, Boxed, Reffed, Object, Interface)
        && starCount > 1))
      throw new Exception("Unexpected number of pointer references for field " ~ fullDName.to!string);

    if (directStruct)
      throw new Exception("Embedded structure fields not supported");

    if (containerType != ContainerType.None)
      throw new Exception("Container type '" ~ containerType.to!string ~ "' not supported");

    if (kind.among(TypeKind.Unknown, TypeKind.Namespace))
      throw new Exception("Unhandled type '" ~ dType.to!string ~ "' (" ~ kind.to!string ~ ")");

    with (TypeKind) if (writable
      && ((kind.among(Struct, StructAlias) && starCount != 0)  // Writable structure pointer fields are not supported
        || kind.among(Pointer, Opaque, Wrap) // Unsupported structure types (unknown memory allocation methods)
        || (kind == Boxed && starCount == 0))) // Non-pointer boxed types not currently supported
    {
      writable = false;
      warnWithLoc(__FILE__, __LINE__, xmlLocation, "Setting writable to false for field '" ~ fullDName.to!string
        ~ "' with unhandled type '" ~ dType.to!string ~ "' (" ~ kind.to!string ~ ")");
      TypeNode.dumpSelectorOnWarning(this);
    }

    if (callback)
    {
      if (callback.active == Active.Enabled)
        callback.doVerify;
      else
        throw new Exception("Field callback type is disabled");
    }
  }

  override void toJson(ref JSONValue js)
  {
    super.toJson(js);

    js["name"] = _name;
    js.jsonSetNonDefault("callback", callback);
    js.jsonSetNonDefault("directStruct", directStruct);
    js["readable"] = readable;
    js["writable"] = writable;
    js["introspectable"] = introspectable;
    js["private"] = private_;
    js.jsonSetNonDefault("bits", bits);
  }

  private dstring _name; /// Field name
  Func callback; /// For callback fields (embedded callback type or alias reference)
  Structure directStruct; /// Directly embedded structure or union
  bool readable; /// Readable field?
  bool writable; /// Writable field?
  bool introspectable = true; /// Is field introspectable?
  bool private_; /// Private field?
  ubyte bits; /// For C bit fields, number of bits occupied for this field in integer type (or 0 if not a bit field)
}
