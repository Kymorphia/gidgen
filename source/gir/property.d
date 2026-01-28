module gir.property;

import gir.func;
import gir.param;
import gir.structure;
import gir.type_node;
import std_includes;
import utils;

/// Class property
final class Property : TypeNode
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

  /// Get the property name formatted in D camelCase
  override dstring dName()
  {
    return repo.defs.symbolName(_name.camelCase);
  }

  override void fromXml(XmlNode node)
  {
    super.fromXml(node);

    _name = node.get("name");
    defaultValue = node.get("default-value");
    ownership = cast(Ownership)ownershipValues.countUntil(node.get("transfer-ownership"));
    readable = node.get("readable", "1") == "1";
    writable = node.get("writable") == "1";
    construct = node.get("construct") == "1";
    constructOnly = node.get("construct-only") == "1";
    deprecated_ = node.get("deprecated") == "1";
    introspectable = node.get("introspectable", "1") == "1";

    version_ = node.get("version");
    deprecatedVersion = node.get("deprecated-version");

    getter = node.get("getter");
    setter = node.get("setter");

    // Look for alternative getter/setter org.gtk.Property.get and org.gtk.Property.set attribute elements
    foreach (child; node.children)
    {
      if (child.id == "attribute")
      {
        auto name = child.get("name");

        if (name == "org.gtk.Property.get")
          propGet = child.get("value");
        else if (name == "org.gtk.Property.set")
          propSet = child.get("value");
      }
    }
  }

  protected override void fixup()
  {
    super.fixup;

    if (!introspectable)
      active = Active.Disabled;
  }

  protected override void verify()
  {
    if (active != Active.Enabled)
      return;

    super.verify;

    if (auto st = cast(Structure)parent)
    {
      if (dName in st.dMethodHash) // Check if there is a conflicting method name with property
      {
        if (!getter.empty || !setter.empty)
        {
          infoWithLoc(__FILE__, __LINE__, xmlLocation, "Disabling property '" ~ fullDName.to!string
            ~ "' which has an identically named getter/setter method");
          active = Active.Ignored;
        }
        else
          throw new Exception("Disabling property '" ~ fullDName.to!string ~ "' which conflicts with a method");
      }
    }

    if (kind == TypeKind.Unknown)
      throw new Exception("Unresolved type for property");

    if (ownership != Ownership.None && ownership != Ownership.Unset)
      throw new Exception("Property ownership '" ~ ownership.to!string ~ "' not supported");

    if (containerType != containerType.None)
      throw new Exception("Properties with container values not supported");
  }

  /**
   * Check if a function is a valid getter for property.
   * Params:
   *   f = The function
   * Returns: true if function is a valid getter, false otherwise
   */
  Func checkGetter(Func f)
  {
    return (f && f.active == Active.Enabled && f.funcType == FuncType.Method && typeEqual(f.returnVal)
      && f.params.length == 1 && f.params[0].isInstanceParam) ? f : null;
  }

  /**
   * Check if a function is a valid setter for property.
   * Params:
   *   f = The function
   * Returns: true if function is a valid setter, false otherwise
   */
  Func checkSetter(Func f)
  {
    return (f && f.active == Active.Enabled && f.funcType == FuncType.Method && (!f.returnVal
      || f.returnVal.origDType == "none") && f.params.length == 2 && f.params[0].isInstanceParam
      && typeEqual(f.params[1])) ? f : null;
  }

  /**
   * Search a class' ancestry for a conflicting method (requiring an alias or override).
   * Params:
   *   st = Structure to search parent ancestry of (null will search from parent of function's class)
   *   getter = Yes.Getter to check for a conflict with property getter, No.Getter to check for setter conflict
   *   outConforms = Output value set to true if matching method conforms
   * Returns: The ancestor class containing the conflicting method or null if none
   */
  Structure findMethodConflict(Structure st, Flag!"Getter" getter, out bool outConforms)
  {
    if (!st) // If klass not specified, use this property's klass parent
      st = cast(Structure)parent ? (cast(Structure)parent).parentStruct : null;

    if (!st || st.structType != StructType.Class)
      return null;

    auto methodName = dName;

    for (auto klass = st; klass; klass = klass.parentStruct)
    {
      auto node = klass.dMethodHash.get(methodName, null);
      if (!node || node.active != Active.Enabled)
        continue;

      if (auto cmpFunc = cast(Func)node) // Regular function method?
      {
        if (cmpFunc.shadowedByFunc) // If the method is shadowed by another one, use it instead
          cmpFunc = cmpFunc.shadowedByFunc;

        outConforms = (getter && checkGetter(cmpFunc)) || (!getter && checkSetter(cmpFunc));
        return klass;
      }
      else if (auto cmpProp = cast(Property)node) // Property method
      {
        outConforms = ((readable && cmpProp.readable) || (writable && cmpProp.writable)) && typeEqual(cmpProp);
        return klass;
      }
    }

    return null;
  }

  override void toJson(ref JSONValue js)
  {
    super.toJson(js);

    js["name"] = _name;
    js.jsonSetNonDefault("defaultValue", defaultValue);
    js["readable"] = readable;
    js["writable"] = writable;
    js.jsonSetNonDefault("construct", construct);
    js.jsonSetNonDefault("constructOnly", constructOnly);
    js.jsonSetNonDefault("deprecated", deprecated_);
    js.jsonSetNonDefault("introspectable", introspectable);
    js.jsonSetNonDefault("version", version_);
    js.jsonSetNonDefault("deprecatedVersion", deprecatedVersion);
    js.jsonSetNonDefault("getter", getter);
    js.jsonSetNonDefault("setter", setter);
    js.jsonSetNonDefault("propGet", propGet);
    js.jsonSetNonDefault("propSet", propSet);
  }

  private dstring _name; /// Name of property
  dstring defaultValue; /// Default value
  bool readable; /// Property is readable
  bool writable; /// Property is writable
  bool construct; /// Construct property?
  bool constructOnly; /// Construct only property?
  bool deprecated_; /// Deprecated?
  bool introspectable; /// Introspectable?
  dstring version_; /// Version
  dstring deprecatedVersion; /// Deprecated version
  dstring getter; /// Getter method
  dstring setter; /// Setter method
  dstring propGet; // <attribute name="org.gtk.Property.get" value="CFunc"> alternative to getter
  dstring propSet; // <attribute name="org.gtk.Property.set" value="CFunc"> alternative to setter
}
