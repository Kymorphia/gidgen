/// C entry points of GObjectExport classes and GInterfaceExport interfaces: _new, invokers of @gVirtual methods
/// (which call the implementation in the class or interface struct of the instance) and get_type of interfaces.
module feat_export_entry;

import gid.glib2.c.functions : g_log_remove_handler, g_log_set_always_fatal, g_log_set_handler;
import gid.glib2.c.types : GLogLevelFlags;
import gid.gobject2.c.functions;
import gid.gobject2.c.types;
import gid.gobject2.types : GTypeEnum;
import gid.gobject2.object : GObjectExport, GInterfaceExport, GObjectNamespace, GObjectSubclass, ObjectWrap,
  gVirtual;
import std.string : fromStringz;
import std.typecons : No;

mixin GObjectNamespace!("GidgenEntry", "gidgen_entry");

/// Exported D interface
interface EntryShape
{
  mixin GInterfaceExport!(null, "Shape");

  @gVirtual int vfuncSides();
  @gVirtual void vfuncSetLabel(string text);
}

/// Exported class with new virtual methods, implementing the exported interface
class EntryBase : ObjectWrap, EntryShape
{
  mixin GObjectExport!(null, "Base");

  string label;
  int last;

  this()
  {
    super();
  }

  int vfuncSides()
  {
    return 5;
  }

  void vfuncSetLabel(string text)
  {
    label = text;
  }

  @gVirtual protected double vfuncScaleBy(double f, int n)
  {
    return f * n;
  }

  @gVirtual protected bool vfuncCheck(bool b, string s, ObjectWrap o)
  {
    last = o ? 1 : 0;
    return b && s == "ok";
  }

  @gVirtual protected void vfuncTouch()
  {
    last = 42;
  }
}

/// Exported subclass overriding a virtual method: no invoker of its own
class EntryChild : EntryBase
{
  mixin GObjectExport!(null, "Child");

  override protected double vfuncScaleBy(double f, int n)
  {
    return super.vfuncScaleBy(f, n) + 1;
  }
}

/// Exported class without a public default constructor: no _new
class EntryNoNew : ObjectWrap
{
  mixin GObjectExport!(null, "NoNew");

  this(int x)
  {
    super();
  }
}

/// Exported class with the implicit default constructor (ObjectWrap.this() is protected): _new
class EntryImplicit : ObjectWrap
{
  mixin GObjectExport!(null, "Implicit");
}

/// Exported class with a private default constructor: no _new
class EntryPrivate : ObjectWrap
{
  mixin GObjectExport!(null, "Private");

  private this()
  {
    super();
  }
}

/// Exported abstract class: no _new
abstract class EntryAbstract : ObjectWrap
{
  mixin GObjectExport!(null, "Abstract");

  this()
  {
    super();
  }
}

/// D subclass which is not exported: no C entry points
class EntryLocal : ObjectWrap
{
  mixin GObjectSubclass!"GidgenEntryLocal";

  @gVirtual protected int vfuncLocal()
  {
    return 1;
  }
}

// The exported functions as C code sees them
extern(C) nothrow
{
  GType gidgen_entry_shape_get_type();
  int gidgen_entry_shape_sides(GObject* self);
  void gidgen_entry_shape_set_label(GObject* self, const(char)* text);
  GType gidgen_entry_base_get_type();
  GObject* gidgen_entry_base_new();
  double gidgen_entry_base_scale_by(GObject* self, double f, int n);
  int gidgen_entry_base_check(GObject* self, int b, const(char)* s, GObject* o);
  void gidgen_entry_base_touch(GObject* self);
  GType gidgen_entry_child_get_type();
  GObject* gidgen_entry_child_new();
  GObject* gidgen_entry_implicit_new();
}

/// Symbol names
unittest
{
  static assert(EntryShape._gidExportSymbolBase == "gidgen_entry_shape");
  static assert(EntryShape._gidExportGetTypeSymbol == "gidgen_entry_shape_get_type");
  static assert(EntryBase._gidExportSymbolBase == "gidgen_entry_base");
  static assert(__traits(hasMember, EntryBase, "_gidExportNew") && __traits(hasMember, EntryChild, "_gidExportNew"));
  static assert(__traits(hasMember, EntryImplicit, "_gidExportNew"));
  static assert(!__traits(hasMember, EntryNoNew, "_gidExportNew"));
  static assert(!__traits(hasMember, EntryPrivate, "_gidExportNew"));
  static assert(!__traits(hasMember, EntryAbstract, "_gidExportNew"));
  static assert(__traits(hasMember, EntryBase, "_gidExportInvoker_vfuncScaleBy"));
  static assert(!__traits(hasMember, EntryLocal, "_gidExportInvoker_vfuncLocal"));
  static assert(!__traits(hasMember, EntryLocal, "_gidExportNew"));
}

/// Exported get_type of a D interface
unittest
{
  auto t = gidgen_entry_shape_get_type();
  assert(t == EntryShape._getGType && g_type_fundamental(t) == GTypeEnum.Interface);
  assert(g_type_name(t).fromStringz == "GidgenEntryShape");
  assert(g_type_is_a(gidgen_entry_base_get_type(), t));
}

/// _new returns a new instance with its D object (transfer full)
unittest
{
  auto obj = gidgen_entry_base_new();
  assert(obj && g_type_check_instance_is_a(cast(GTypeInstance*)obj, EntryBase._getGType));
  auto d = ObjectWrap._getDObject!EntryBase(obj, No.Take);
  assert(d !is null);
  assert(d.last == 0);
  g_object_unref(obj); // The C reference (the D object keeps its own)

  auto child = gidgen_entry_child_new();
  assert(g_type_check_instance_is_a(cast(GTypeInstance*)child, EntryChild._getGType));
  g_object_unref(child);

  auto implicit = gidgen_entry_implicit_new();
  assert(g_type_check_instance_is_a(cast(GTypeInstance*)implicit, EntryImplicit._getGType));
  g_object_unref(implicit);
}

/// Invokers call the implementation of the type of the instance, with C arguments
unittest
{
  auto base = new EntryBase;
  auto child = new EntryChild;
  auto self = cast(GObject*)base._cPtr;

  assert(gidgen_entry_base_scale_by(self, 1.5, 2) == 3.0);
  assert(gidgen_entry_base_scale_by(cast(GObject*)child._cPtr, 1.5, 2) == 4.0); // D override
  assert(gidgen_entry_base_check(self, 1, "ok", cast(GObject*)child._cPtr) == 1 && base.last == 1);
  assert(gidgen_entry_base_check(self, 1, "no", null) == 0 && base.last == 0);
  gidgen_entry_base_touch(self);
  assert(base.last == 42);

  assert(gidgen_entry_shape_sides(self) == 5); // Interface invokers
  gidgen_entry_shape_set_label(cast(GObject*)child._cPtr, "lbl");
  assert(child.label == "lbl");
}

private int criticals;

private extern(C) void entryCountCritical(const(char)* domain, GLogLevelFlags level, const(char)* message, void* data)
  nothrow
{
  criticals++;
}

/// An invoker called with an instance of another type logs a critical error and returns 0
unittest
{
  auto oldFatal = g_log_set_always_fatal(GLogLevelFlags.LevelError | GLogLevelFlags.FlagFatal);
  auto handler = g_log_set_handler("Gid", GLogLevelFlags.LevelCritical, &entryCountCritical, null);
  scope(exit)
  {
    g_log_remove_handler("Gid", handler);
    g_log_set_always_fatal(oldFatal);
  }

  auto other = new EntryLocal;
  criticals = 0;
  assert(gidgen_entry_base_scale_by(cast(GObject*)other._cPtr, 1.0, 1) == 0.0);
  assert(gidgen_entry_shape_sides(cast(GObject*)other._cPtr) == 0);
  assert(gidgen_entry_shape_sides(null) == 0);
  assert(criticals == 3);
}
