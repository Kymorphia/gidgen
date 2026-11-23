/// GObjectExport tests: exported get_type C functions, exact GType names, C created instances (also from threads
/// created by C) and the D runtime thread attach helpers. The C program test without a D main() is in
/// ../export-test/.
module feat_export;

import core.memory : GC;
import core.sys.posix.pthread;
import core.thread : Thread;
import gid.core.utils : gidAttachThread, gidInitRuntime;
import gid.gidgenfeat1.c.functions;
import gid.gidgenfeat1.c.types;
import gid.gidgenfeat1.global : liveCount;
import gid.gidgenfeat1.iface;
import gid.gidgenfeat1.iface_mixin;
import gid.gidgenfeat1.object : FeatObject = ObjectWrap;
import gid.gidgenfeat1.types : Counter;
import gid.glib2.c.functions;
import gid.glib2.c.types;
import gid.gobject2.c.functions;
import gid.gobject2.c.types;
import GObj = gid.gobject2.object;
import gid.gobject2.object : GObjectExport;
import std.string : fromStringz;

/// Exported D subclass with the default get_type symbol name
class ExportObj : FeatObject
{
  mixin GObjectExport!("GidgenTest", "ExportObj");

  int initValue;
  bool ctorOnAttachedThread; // Constructor ran on a thread attached to the D runtime

  this()
  {
    super();
    initValue = 77;
    ctorOnAttachedThread = Thread.getThis !is null;
  }

  override protected int vfuncCompute(int x)
  {
    return x * 3 + initValue;
  }

  override protected int vfuncGetId()
  {
    return 4242;
  }
}

/// Exported D subclass with a custom get_type symbol name
class ExportCustom : FeatObject
{
  mixin GObjectExport!("GidgenTest", "ExportCustom", "gidgen_test_custom_type_fn");

  this()
  {
    super();
  }
}

/// Exported D subclass with a GType name which is already registered (GidgenFeatObject)
class ExportDup : FeatObject
{
  mixin GObjectExport!("Gidgen", "FeatObject", "gidgen_test_dup_get_type");

  this()
  {
    super();
  }
}

// The exported functions as C code sees them
extern(C) GType gidgen_test_export_obj_get_type() nothrow;
extern(C) GType gidgen_test_custom_type_fn() nothrow;

/// get_type symbol names
unittest
{
  alias sym = GObj.ObjectWrap._exportGetTypeSymbol;
  assert(sym("Mylib", "Foo") == "mylib_foo_get_type");
  assert(sym("Mylib", "FooBar") == "mylib_foo_bar_get_type");
  assert(sym("Gtk", "HTTPServer") == "gtk_http_server_get_type");
  assert(sym("Gtk", "Widget2D") == "gtk_widget2_d_get_type");
  assert(sym("GidgenTest", "ExportObj") == "gidgen_test_export_obj_get_type");
  assert(ExportObj._gidExportGetTypeSymbol == "gidgen_test_export_obj_get_type");
  assert(ExportCustom._gidExportGetTypeSymbol == "gidgen_test_custom_type_fn");
}

/// Exported get_type functions register the exact GType names
unittest
{
  auto type = gidgen_test_export_obj_get_type();
  assert(type != 0 && type == ExportObj._getGType);
  assert(g_type_name(type).fromStringz == "GidgenTestExportObj", "exact exported GType name");
  assert(g_type_from_name("GidgenTestExportObj") == type);
  assert(g_type_parent(type) == gidgen_feat_object_get_type());
  assert(g_type_is_a(type, Iface._getGType));

  auto custom = gidgen_test_custom_type_fn();
  assert(custom == ExportCustom._getGType && g_type_name(custom).fromStringz == "GidgenTestExportCustom");
}

private int dupCriticals;

private extern(C) void countCritical(const(char)* domain, GLogLevelFlags level, const(char)* message, void* data)
  nothrow
{
  dupCriticals++;
}

/// An exported GType name which is already registered is not renamed: registration fails with a critical error
unittest
{
  gidgen_feat_object_get_type();
  auto oldFatal = g_log_set_always_fatal(GLogLevelFlags.LevelError | GLogLevelFlags.FlagFatal);
  auto handler = g_log_set_handler("Gid", GLogLevelFlags.LevelCritical, &countCritical, null);
  scope(exit)
  {
    g_log_remove_handler("Gid", handler);
    g_log_set_always_fatal(oldFatal);
  }

  assert(ExportDup._getGType == 0, "colliding exported name is not registered");
  assert(dupCriticals == 1);
  assert(ExportDup._getGType == 0 && dupCriticals == 1, "critical logged once, still 0 on retry");
}

/// Objects created from C get their D object in instance_init (default constructor), D vfunc overrides are used
unittest
{
  auto start = liveCount(Counter.Object);
  auto cObj = g_object_new(gidgen_test_export_obj_get_type(), null);
  auto dObj = cast(ExportObj)GObj.ObjectWrap._getDObject!FeatObject(cObj, No.Take);
  assert(dObj && dObj.initValue == 77 && dObj._cPtr is cObj, "C created instance has its D object");
  assert(gidgen_feat_object_compute(cast(GidgenFeatObject*)cObj, 2) == 83, "class vfunc override");
  assert(gidgen_feat_iface_get_id(cast(GidgenFeatIface*)cObj) == 4242, "interface vfunc override");
  assert(cast(ExportObj)GObj.ObjectWrap._getDObject!FeatObject(cObj, No.Take) is dObj, "wrapper identity");

  auto d = new ExportObj; // D created
  assert(d.compute(1) == 80 && g_type_name(d._gType).fromStringz == "GidgenTestExportObj");

  g_object_unref(cObj); // Only the toggle reference remains
  dObj = null;
  destroy(d);
  d = null;

  foreach (i; 0 .. 3)
  {
    GC.collect;
    while (g_main_context_iteration(null, false)) {}
  }

  assert(liveCount(Counter.Object) == start, "objects leaked");
}

import std.typecons : No;

private extern(C) void* createInThread(void* data) nothrow
{
  auto attachedBefore = Thread.getThis !is null;
  auto cObj = g_object_new(gidgen_test_export_obj_get_type(), null); // Attaches the thread (instance_init)
  auto dObj = cast(ExportObj)GObj.ObjectWrap._getDObject!FeatObject(cObj, No.Take);
  auto ok = !attachedBefore && Thread.getThis !is null && dObj && dObj.ctorOnAttachedThread
    && gidgen_feat_object_compute(cast(GidgenFeatObject*)cObj, 1) == 80;
  *cast(void**)data = ok ? cObj : null;
  return null;
}

private extern(C) void* attachInThread(void* data) nothrow
{
  auto before = Thread.getThis is null;
  gidAttachThread();
  gidAttachThread(); // Fast path
  *cast(bool*)data = before && Thread.getThis !is null;
  return null;
}

/// Threads created by C are attached at C to D entry points and detached when they exit, the GC doesn't hang on
/// exited attached threads
unittest
{
  gidInitRuntime(); // Only increments the runtime init count once

  bool attached;
  pthread_t tid;
  assert(pthread_create(&tid, null, &attachInThread, &attached) == 0);
  pthread_join(tid, null);
  assert(attached, "gidAttachThread attaches a C thread");
  GC.collect; // Would hang if the exited thread was still attached

  auto start = liveCount(Counter.Object);
  void* cObj;
  assert(pthread_create(&tid, null, &createInThread, &cObj) == 0);
  pthread_join(tid, null);
  assert(cObj, "g_object_new on a C thread");
  GC.collect;

  auto dObj = cast(ExportObj)GObj.ObjectWrap._getDObject!FeatObject(cObj, No.Take);
  assert(dObj && dObj.initValue == 77, "D object created on the C thread is still bound");
  dObj = null;
  g_object_unref(cast(GObject*)cObj);

  foreach (i; 0 .. 3)
  {
    GC.collect;
    while (g_main_context_iteration(null, false)) {}
  }

  assert(liveCount(Counter.Object) == start, "objects leaked");
}

private extern(C) void* emitInThread(void* data) nothrow
{
  gidgen_feat_object_emit_changed(cast(GidgenFeatObject*)data, 9); // Signal marshal attaches the thread
  return null;
}

/// Signal handlers of D objects run on threads created by C (attached by the signal marshal)
unittest
{
  auto obj = new ExportObj;
  int got;
  bool attached;
  obj.connectChanged((int v) { got = v; attached = Thread.getThis !is null; GC.collect; });

  pthread_t tid;
  assert(pthread_create(&tid, null, &emitInThread, obj._cPtr) == 0);
  pthread_join(tid, null);
  assert(got == 9 && attached, "signal handler on an attached C thread");
  GC.collect;
  destroy(obj);
}

private __gshared int probeDtors;

/// Class which is never constructed, its destructor must not run for the class map probe objects
class ProbeVictim : FeatObject
{
  mixin GObjectExport!("GidgenTest", "ProbeVictim");

  this()
  {
    super();
  }

  ~this()
  {
    probeDtors++;
  }
}

/// Class map creation probes D classes with objects created without constructors, which are freed without running
/// their destructors (a D destructor of an object which was never constructed)
unittest
{
  import std.typecons : Yes;

  auto obj = GObj.ObjectWrap._getDObject!FeatObject(gidgen_feat_object_new(), Yes.Take); // Creates the class maps
  assert(obj);
  destroy(obj);

  foreach (i; 0 .. 3)
  {
    GC.collect;
    while (g_main_context_iteration(null, false)) {}
  }

  assert(probeDtors == 0, "destructor ran for a class map probe object");
}
