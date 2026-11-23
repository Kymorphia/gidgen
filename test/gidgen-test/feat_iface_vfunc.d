/// GidgenFeat interface virtual method override tests: D overrides installed in the interface structures of D subclass
/// GTypes (interface_init, see GObjectSubclass)
module feat_iface_vfunc;

import gidgenfeat.c.functions;
import gidgenfeat.c.types;
import gidgenfeat.child_iface;
import gidgenfeat.child_iface_mixin; // Interface mixins are imported without selection, for their public imports
import gidgenfeat.global : liveCount;
import gidgenfeat.iface;
import gidgenfeat.iface_mixin;
import gidgenfeat.object : FeatObject = ObjectWrap;
import gidgenfeat.types : Counter;
import gid.glib2.error : ErrorWrap;
import gid.glib2.c.functions : g_quark_from_string;
import gid.gobject2.c.functions;
import gid.gobject2.c.types;
import GObj = gid.gobject2.object;
import gid.gobject2.object : GObjectSubclass;
import std.typecons : No;

private GidgenFeatIfaceInterface* ifaceOf(GType type)
{
  return cast(GidgenFeatIfaceInterface*)g_type_interface_peek(g_type_class_peek(type), Iface._getGType);
}

private GidgenFeatIface* cIface(GObj.ObjectWrap obj)
{
  return cast(GidgenFeatIface*)obj._cPtr;
}

/// Case A: D subclass of a C class implementing the interface, overriding its vfuncs and chaining up
class DFeat : FeatObject
{
  mixin GObjectSubclass;

  this(int value)
  {
    super();
    setValue(value);
  }

  override protected int vfuncGetId()
  {
    return super.vfuncGetId() + 1; // Chain up to the C implementation of FeatObject
  }

  override protected bool vfuncLookup(int key, out int value)
  {
    if (key == 13)
      throw new ErrorWrap(g_quark_from_string("feat-iface-vfunc-test"), 7, "unlucky");

    auto found = super.vfuncLookup(key, value); // Throws for a negative key
    value *= 2;
    return found;
  }
}

/// Case A, second level D subclass with its own GType
class DFeatChild : DFeat
{
  mixin GObjectSubclass;

  this(int value)
  {
    super(value);
  }

  override protected int vfuncGetId()
  {
    return super.vfuncGetId() * 2; // Chains up through DFeat to the C implementation
  }
}

/// Case A, overriding a single interface vfunc
class DFeatPartial : FeatObject
{
  mixin GObjectSubclass;

  this(int value = 2)
  {
    setValue(value);
  }

  override protected int vfuncGetId()
  {
    return -1;
  }
}

/// Case A, no interface vfunc overrides (interface not added to the D GType)
class DFeatNone : FeatObject
{
  mixin GObjectSubclass;

  this()
  {
  }
}

/// Case B: D class newly implementing the interface
class DImpl : GObj.ObjectWrap, Iface
{
  mixin IfaceT!();
  mixin GObjectSubclass;

  int base = 7;

  this()
  {
  }

  protected int vfuncGetId() // Implementation (hides the default of the mixin, no override)
  {
    return base;
  }

  protected bool vfuncLookup(int key, out int value)
  {
    if (key < 0)
      throw new Exception("negative key in D");

    value = key * base;
    return true;
  }
}

/// Case B, second level D subclass overriding an implementation
class DImplChild : DImpl
{
  mixin GObjectSubclass;

  override protected int vfuncGetId()
  {
    return super.vfuncGetId() + 1;
  }
}

/// Case B, implementing only one vfunc of the interface
class DImplPartial : GObj.ObjectWrap, Iface
{
  mixin IfaceT!();
  mixin GObjectSubclass;

  protected int vfuncGetId()
  {
    int value;
    return vfuncLookup(1, value) ? 1 : 3; // Default of the mixin, the C parent does not implement the interface
  }
}

/// Case B, interface with an interface prerequisite (both are added, the prerequisite first)
class DChildImpl : GObj.ObjectWrap, ChildIface
{
  mixin IfaceT!();
  mixin ChildIfaceT!();
  mixin GObjectSubclass;

  protected int vfuncGetLevel()
  {
    return 42;
  }

  protected int vfuncGetId()
  {
    return 5;
  }
}

/// Case A: C calls of interface vfuncs reach the D overrides, which chain up to the C implementation
unittest
{
  auto start = liveCount(Counter.Object);
  auto obj = new DFeat(3);
  assert(obj.getId == 301, "C invoker reaches the D override, which chains up");

  int value;
  assert(obj.lookup(5, value) && value == 16, "out parameter and return value");
  assert(!obj.lookup(0, value) && value == 6);

  try
  {
    obj.lookup(-1, value); // Chain up to the C implementation which sets a GError
    assert(0, "lookup(-1) should throw");
  }
  catch (ErrorWrap e)
    assert(e.code == 2 && e.message == "negative key");

  try
  {
    obj.lookup(13, value); // ErrorWrap thrown by the D override
    assert(0, "lookup(13) should throw");
  }
  catch (ErrorWrap e)
    assert(e.code == 7 && e.domain == g_quark_from_string("feat-iface-vfunc-test") && e.message == "unlucky");

  auto child = new DFeatChild(3);
  assert(child.getId == 602, "two level D override chain up");
  assert(child.lookup(5, value) && value == 16, "inherited D override installed in the D child interface");
  assert(g_type_is_a(DFeatChild._getGType, Iface._getGType));

  destroy(obj);
  destroy(child);
  assert(liveCount(Counter.Object) == start, "objects leaked");
}

/// Case A: GObject copies the parent interface structure, slots which are not overridden keep the C implementation
unittest
{
  auto obj = new DFeatPartial(4);
  assert(obj.getId == -1);
  int value;
  assert(obj.lookup(1, value) && value == 5, "C implementation of the slot which is not overridden");

  auto dIface = ifaceOf(DFeatPartial._getGType);
  auto cIface = ifaceOf(FeatObject._getGType);
  assert(dIface !is cIface, "D GType has its own interface structure");
  assert(dIface.lookup is cIface.lookup, "copied from the parent interface structure");
  assert(dIface.getId !is cIface.getId, "overridden slot");

  auto none = new DFeatNone;
  none.setValue(2);
  assert(none.getId == 200);
  assert(ifaceOf(DFeatNone._getGType) is cIface, "interface not added without overrides (parent structure used)");

  destroy(obj);
  destroy(none);
}

/// Case B: D class newly implementing an interface
unittest
{
  auto obj = new DImpl;
  assert(gidgen_feat_iface_get_id(cIface(obj)) == 7 && obj.getId == 7, "C call reaches the D implementation");
  assert(g_type_is_a(DImpl._getGType, Iface._getGType) && !g_type_is_a(GObj.ObjectWrap._getGType, Iface._getGType));

  int value;
  assert(obj.lookup(3, value) && value == 21, "out parameter and return value");

  try
  {
    obj.lookup(-1, value); // Other exception thrown by the D implementation
    assert(0, "lookup(-1) should throw");
  }
  catch (ErrorWrap e)
    assert(e.domain == g_quark_from_string("gid-exception") && e.message == "negative key in D");

  auto child = new DImplChild;
  assert(child.getId == 8, "two level D implementation chain up");
  assert(child.lookup(2, value) && value == 14);

  auto partial = new DImplPartial;
  assert(partial.getId == 3, "default vfunc method of the mixin is a no-op without a C implementation");
  assert(!gidgen_feat_iface_has_lookup(cIface(partial)), "slot which is not implemented stays NULL");

  try
  {
    partial.lookup(1, value); // The C invoker handles the NULL slot
    assert(0, "lookup should throw");
  }
  catch (ErrorWrap e)
    assert(e.message == "not implemented");

  auto childImpl = new DChildImpl;
  assert(gidgen_feat_child_iface_get_level(cast(GidgenFeatChildIface*)childImpl._cPtr) == 42);
  assert(gidgen_feat_iface_get_id(cIface(childImpl)) == 5, "prerequisite interface");

  foreach (o; [cast(GObj.ObjectWrap)obj, child, partial, childImpl])
    assert((cast(GObject*)o._cPtr).refCount == 1, "only the toggle reference");

  destroy(obj);
  destroy(child);
  destroy(partial);
  destroy(childImpl);
}

/// C created instances of D GTypes implementing or overriding interfaces
unittest
{
  auto start = liveCount(Counter.Object);

  auto cObj = g_object_new_with_properties(DImplChild._getGType, 0, null, null);
  assert(gidgen_feat_iface_get_id(cast(GidgenFeatIface*)cObj) == 8, "Case B C created instance");
  auto dObj = GObj.ObjectWrap._vfuncDObject(cObj);
  assert(cast(DImplChild)dObj && (cast(DImplChild)dObj).base == 7, "D object constructed");
  int weakAlive = 1;
  static extern(C) void weakNotify(void* data, GObject* obj) { *cast(int*)data = 0; }
  g_object_weak_ref(cast(GObject*)cObj, &weakNotify, &weakAlive);
  g_object_unref(cast(GObject*)cObj);
  destroy(dObj);
  assert(weakAlive == 0, "C instance finalized");

  auto cFeat = g_object_new_with_properties(DFeatPartial._getGType, 0, null, null);
  assert(gidgen_feat_iface_get_id(cast(GidgenFeatIface*)cFeat) == -1, "Case A C created instance");
  int value;
  assert(gidgen_feat_iface_lookup(cast(GidgenFeatIface*)cFeat, 1, &value, null) && value == 3);
  g_object_unref(cast(GObject*)cFeat);
  destroy(GObj.ObjectWrap._vfuncDObject(cFeat)); // Still bound (toggle reference) after the C unref

  assert(liveCount(Counter.Object) == start, "objects leaked");
}
