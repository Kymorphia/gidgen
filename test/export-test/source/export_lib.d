/**
 * D shared library exporting GObject types with GObjectExport, loaded by the C program export_test.c which has no D
 * main(): the D runtime is initialized by the library constructor of GObjectExport and C threads are attached at C to
 * D entry points.
 */
module export_lib;

import core.atomic : atomicOp, atomicLoad;
import core.memory : GC;
import gid.core.utils : gidAttachThread;
import gid.gidgenfeat1.iface;
import gid.gidgenfeat1.iface_mixin; // Interface mixins are imported without selection
import gid.gidgenfeat1.object : FeatObject = ObjectWrap;
import gid.glib2.c.functions : g_main_context_iteration, g_quark_from_string;
import gid.glib2.error : ErrorWrap;
import gid.gobject2.initially_unowned : InitiallyUnowned;
import gid.gobject2.object : GirInfo, GObjectExport, GObjectExportAll, GObjectGirMain, GObjectNamespace,
  GInterfaceExport, ObjectWrap, gConstructor, gMethod, gNullable, gThrows, gProperty, gNotifyAlways, gSignal, gVirtual;
import gid.gobject2.types : ParamFlags, SignalFlags;

// Namespace with its GIR version, needed by the GIR of export_mixer.d which uses these types
mixin GObjectNamespace!("GidgenExport", "1.0");

// The gir configuration builds a program which writes GidgenExport-1.0.gir (the version comes from GObjectNamespace)
version (GidGir) mixin GObjectGirMain!(GirInfo("GidgenExport", null, "libgidgen-export.so"), export_lib);

private shared int liveDObjects; // D objects of the exported types which were not finalized

/// GidgenExportObj, exported as gidgen_export_obj_get_type()
class ExportObj : FeatObject
{
  mixin GObjectExport!("GidgenExport", "Obj");

  int initValue;
  @gProperty int count = 1;
  @gProperty string title = "hi";
  @gProperty(ParamFlags.Construct) int constructVal = 10;
  @gProperty @gNotifyAlways int noisy = 3;
  @gProperty int bumpTotal;

  @gSignal void bumped(int amount, string who)
  {
    bumpTotal = bumpTotal + amount;
  }

  @gSignal string greet(string name)
  {
    return "hello " ~ name;
  }

  @gSignal(SignalFlags.Detailed) void tagged(string text) {}

  @gSignal(SignalFlags.Action) void reset()
  {
    bumpTotal = 0;
  }

  this()
  {
    super();
    initValue = 77;
    liveDObjects.atomicOp!"+="(1);
  }

  ~this()
  {
    liveDObjects.atomicOp!"-="(1);
  }

  override protected void vfuncConstructed()
  {
    super.vfuncConstructed();
    setName(_cPtr !is null ? "constructed-bound" : "constructed-unbound");
  }

  override protected int vfuncCompute(int x)
  {
    return x * 3 + initValue;
  }

  override protected int vfuncGetId()
  {
    return 4242;
  }

  override protected bool vfuncLookup(int key, out int value)
  {
    if (key == 13)
      throw new ErrorWrap(g_quark_from_string("gidgen-export"), 13, "unlucky");

    return super.vfuncLookup(key, value); // Chain up to the C implementation of GidgenFeatObject
  }
}

/// GidgenExportChild, inherits the overrides of ExportObj
class ExportChild : ExportObj
{
  mixin GObjectExport!("GidgenExport", "Child");

  override protected int vfuncCompute(int x)
  {
    return super.vfuncCompute(x) + 1000;
  }

  @gSignal void childEvent(int x) {}
}

/// GidgenExportShape, a GObject interface defined in D, exported as gidgen_export_shape_get_type() with the invokers
/// gidgen_export_shape_sides() and gidgen_export_shape_label()
interface ExportShape
{
  mixin GInterfaceExport!("GidgenExport", "Shape");

  @gVirtual int vfuncSides();
  @gVirtual void vfuncLabel(string text);
}

/// GidgenExportVirt, declares new virtual methods (the class struct of this type has two slots after GObjectClass)
class ExportVirt : ObjectWrap, ExportShape
{
  mixin GObjectExport!("GidgenExport", "Virt");

  int vfuncSides()
  {
    return 4;
  }

  void vfuncLabel(string text)
  {
    title = "label " ~ text;
  }

  @gProperty string title = "hi";

  /// New virtual method
  @gVirtual protected int vfuncScale(int x)
  {
    return x * 2;
  }

  /// New virtual method with strings
  @gVirtual protected void vfuncJoin(string a, string b)
  {
    title = a ~ b;
  }

  this()
  {
    super();
  }

  /// gidgen_export_virt_new_with_title()
  @gConstructor("with_title") this(string t)
  {
    super();
    title = t;
  }

  /// Returns a new string, prefix can be NULL
  @gMethod string describe(@gNullable string prefix)
  {
    return (prefix is null ? "" : prefix ~ " ") ~ title;
  }

  /// GError with the domain of an ErrorWrap, or the D exception domain for other exceptions
  @gMethod @gThrows int divide(int a, int b)
  {
    if (b == 0)
      throw new ErrorWrap(g_quark_from_string("gidgen-export"), 3, "division by zero");

    if (b < 0)
      throw new Exception("negative divisor");

    return a / b;
  }

  /// Out parameters: transfer full for the string, array and object
  @gMethod void stats(string s, out int length, out string upper, out string[] chars, out ExportVirt same)
  {
    import std.uni : toUpper;

    length = cast(int)s.length;
    upper = s.toUpper;

    foreach (c; s)
      chars ~= [c];

    same = this;
  }

  /// Static method: plain C function
  @gMethod("abi_version") static int abiVersion()
  {
    return 3;
  }
}

/// GidgenExportVirtChild, overrides a new virtual method of its parent type
class ExportVirtChild : ExportVirt
{
  mixin GObjectExport!("GidgenExport", "VirtChild");

  override protected int vfuncScale(int x)
  {
    return super.vfuncScale(x) + 1;
  }
}

/// GidgenExportFloating, a GInitiallyUnowned subclass (floating reference for C)
class ExportFloating : InitiallyUnowned
{
  mixin GObjectExport!("GidgenExport", "Floating", "gidgen_export_floating_type");

  this()
  {
    super();
    liveDObjects.atomicOp!"+="(1);
  }

  ~this()
  {
    liveDObjects.atomicOp!"-="(1);
  }
}

/// GidgenExportAuto, exported without attributes: methods, static functions and @property accessors
class ExportAuto : ObjectWrap
{
  mixin GObjectExportAll!("GidgenExport", "Auto");

  private int _level = 1;

  int add(int x) // gidgen_export_auto_add
  {
    return _level + x;
  }

  static int twice(int x) // gidgen_export_auto_twice
  {
    return 2 * x;
  }

  @property int level() // Property "level"
  {
    return _level;
  }

  @property void level(int v)
  {
    _level = v;
  }
}

/// Run D garbage collections and the default main context, so that unreferenced D objects are finalized and their
/// deferred final GObject unrefs run
export extern(C) void gidgen_export_collect() nothrow
{
  gidAttachThread();

  foreach (i; 0 .. 4)
  {
    GC.collect;
    while (g_main_context_iteration(null, false)) {}
  }
}

/// Returns: Number of live D objects of the exported types
export extern(C) int gidgen_export_live_dobjects() nothrow
{
  return liveDObjects.atomicLoad;
}

/// Emit the bumped signal from D (on a thread created by C)
export extern(C) void gidgen_export_emit_bumped(void* obj, int amount) nothrow
{
  gidAttachThread();

  try
  {
    if (auto o = cast(ExportObj)ObjectWrap._vfuncDObject(obj))
      o.emitBumped(amount, "D");
  }
  catch (Exception e)
    assert(0, e.msg);
}
