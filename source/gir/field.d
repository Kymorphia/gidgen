module gir.field;

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
  }

  override void fixup()
  {
    super.fixup;

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

    foreach (s; ["reserved"d, "dummy"d])
    {
      if (active == Active.Enabled && name.toLower.canFind(s))
      {
        active = Active.Disabled;
        info("Disabling field '" ~ name.to!string ~ "' containing '" ~ s.to!string ~ "'");
        break;
      }
    }
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
    if (active != Active.Enabled || private_)
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

    if (kind.among(TypeKind.Unknown, TypeKind.Interface, TypeKind.Namespace))
      throw new Exception("Unhandled type '" ~ dType.to!string ~ "' (" ~ kind.to!string ~ ")");

    with (TypeKind) if (writable && kind.among(Opaque, Boxed, Wrap, Reffed, Object, Interface))
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

  private dstring _name; /// Field name
  Func callback; /// For callback fields (embedded callback type or alias reference)
  Structure directStruct; /// Directly embedded structure or union
  bool readable; /// Readable field?
  bool writable; /// Writable field?
  bool introspectable = true; /// Is field introspectable?
  bool private_; /// Private field?
}
