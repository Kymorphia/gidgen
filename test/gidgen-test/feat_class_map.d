/// GidgenFeat GType to D class map tests (no get_type() calls when the class maps are created)
module feat_class_map;

import gid.gid : Flag, No, Yes;
import gidgenfeat.child_iface;
import gidgenfeat.global : lateInit, newLateObject, lateTypeEarly, newHiddenChild;
import gidgenfeat.late_object;
import gidgenfeat.object : FeatObject = ObjectWrap;
import gobject.c.functions;
import gobject.c.types;
import gobject.object : ObjectWrap;
import gobject.types : GType;

/// D subclass without its own GType, must not be used for wrapping C instances of its parent type
class FeatSubclass : FeatObject
{
  this()
  {
    super();
  }
}

/// D class with a GType registered in D, overriding _gType but not _gTypeName (GType is looked up when the class
/// maps are created)
class FeatCustom : FeatObject
{
  this(void* ptr, Flag!"Take" take) nothrow
  {
    super(ptr, take);
  }

  __gshared GType customType;

  static GType _getGType() nothrow
  {
    if (!customType)
    {
      GTypeQuery query;
      g_type_query(FeatObject._getGType, &query);
      customType = g_type_register_static_simple(FeatObject._getGType, "GidgenFeatDCustom", query.classSize, null,
        query.instanceSize, null, cast(GTypeFlags)0);
    }

    return customType;
  }

  override @property GType _gType() nothrow
  {
    return _getGType;
  }
}

/// Creating the class maps (on the first wrap of a C instance) doesn't call the get_type() function of D classes
unittest
{
  auto child = newHiddenChild(1); // Wraps a C instance, which creates the class maps
  assert(child !is null);
  assert(!lateTypeEarly, "get_type() called before the initialization function");
  destroy(child);

  lateInit;
  auto late = newLateObject; // Returned as GObject, D class found from the GType name
  assert(typeid(late) is typeid(LateObject), typeid(late).name);
  assert(!lateTypeEarly);
  destroy(late);
}

/// D subclasses without their own GType are not used, custom D GType classes are
unittest
{
  auto sub = new FeatSubclass;
  auto obj = FeatObject.newWithValue(2); // Wrapped by the lookup
  assert(typeid(obj) is typeid(FeatObject), typeid(obj).name);
  destroy(obj);
  destroy(sub);

  auto custom = ObjectWrap._getDObject!ObjectWrap(g_object_new(FeatCustom._getGType, null), Yes.Take);
  assert(typeid(custom) is typeid(FeatCustom), typeid(custom).name);
  assert((cast(FeatObject)custom).getValue == 0);
  destroy(custom);
}
