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

  override void fixup()
  {
    super.fixup;

    if (private_ || !introspectable) // Ignore private or non-introspectable fields
      active = Active.Ignored;

    if (callback) // Embedded callback type
    {
      cType = origCType = null;
      dType = origDType = null;
      kind = TypeKind.Callback;
      callback.fixup;
    }
    else if (kind == TypeKind.Callback)
      callback = cast(Func)typeObject;
    else if (directStruct) // Embedded structure
      foreach (f; directStruct.fields)
        f.fixup;
  }

  override void resolve()
  {
    super.resolve;

    if (callback) // Embedded callback type
      callback.resolve;
    else if (directStruct) // Embedded structure
      foreach (f; directStruct.fields)
        f.resolve;
  }

  override void verify()
  {
    if (active != Active.Enabled)
      return;

    super.verify;

    auto starCount = cType.countStars;

    if (kind == TypeKind.String && starCount > 1)
      throw new Exception("Array of strings not supported");

    with(TypeKind) if ((kind.among(Basic, BasicAlias, Enum, Flags, Callback) && starCount != 0)
        || (kind.among(String, Simple, Pointer, Opaque, Wrap, Boxed, Reffed, Object, Interface) && starCount > 1))
      throw new Exception("Unexpected number of pointer references for field " ~ fullName.to!string);

    if (directStruct)
      throw new Exception("Embedded structure fields not supported");

    if (containerType != ContainerType.None)
      throw new Exception("Container type '" ~ containerType.to!string ~ "' not supported");

    if (kind.among(TypeKind.Unknown, TypeKind.Namespace))
      throw new Exception("Unhandled type '" ~ dType.to!string ~ "' (" ~ kind.to!string ~ ")");

    with (TypeKind) if (writable && (kind.among(Opaque, Wrap) || (kind == Boxed && cType.countStars == 0))) // Non-pointer boxed types not currently supported (GValue for example)
    {
      writable = false;
      warnWithLoc(__FILE__, __LINE__, xmlLocation, "Setting writable to false for field '" ~ fullName.to!string ~ "' with unhandled type '"
          ~ dType.to!string ~ "' (" ~ kind.to!string ~ ")");
      TypeNode.dumpSelectorOnWarning(this);
    }

    if (callback)
    {
      if (callback.active == Active.Enabled)
        callback.verify;
      else
        throw new Exception("Field callback type is disabled");
    }
    else if (directStruct)
      foreach (f; directStruct.fields)
        f.verify;
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
