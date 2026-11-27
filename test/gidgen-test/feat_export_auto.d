/// Export defaults and auto-export (GidExport, GidSkip, GObjectExportAll, @gNoExport): which members are exported
/// without attributes, D @property accessors as GObject properties, overloads and skipped members
module feat_export_auto;

import gid.glib2.c.functions;
import gid.glib2.c.types;
import gid.gobject2.c.functions;
import gid.gobject2.c.types;
import gid.gobject2.types : GTypeEnum;
import gid.gobject2.object : GidExport, GidSkip, GObjectExport, GObjectExportAll, GObjectNamespace, ObjectWrap,
  gidDExceptionQuark, gMethod, gNoExport, gProperty, gSignal, gThrows, gVirtual;
import gid.gobject2.dig_method : _DExportKind, _dExportMethods, _dExportSkipped;
import std.string : fromStringz;
import std.typecons : No;

// Module defaults: methods (not functions or properties), GError for methods which can throw, skip silently
mixin GObjectNamespace!("GidgenAuto", "gidgen_auto", GidExport.Methods | GidExport.Throws, GidSkip.Silent);

/// A plain D interface: implementations are not exported as methods
interface AutoPlain
{
  int plainValue();
}

/// Uses the namespace defaults
class AutoNs : ObjectWrap, AutoPlain
{
  mixin GObjectExport!(null, "Ns");

  int last;

  int sum(int a, int b) nothrow // gidgen_auto_ns_sum, no GError (nothrow)
  {
    return a + b;
  }

  void fail(string msg) // gidgen_auto_ns_fail(self, msg, error)
  {
    throw new Exception(msg);
  }

  @gThrows(false) int quiet() // No GError
  {
    return 4;
  }

  static int helper() // Not exported: no GidExport.Functions
  {
    return 1;
  }

  @property int size() // Not exported: no GidExport.Properties, and accessors are never methods
  {
    return 2;
  }

  void feed(int delegate() d) // Unsupported type: skipped silently (GidSkip.Silent)
  {
  }

  @gNoExport void hidden() // Excluded
  {
  }

  @gMethod("renamed") int explicit() nothrow // Explicit attribute: its own name (GidExport.Throws applies too)
  {
    return 5;
  }

  int plainValue() // Interface implementation: not exported
  {
    return 6;
  }

  override string toString() // Override: not exported
  {
    return "AutoNs";
  }

  bool opEquals(int x) // Operator overload: not exported
  {
    return false;
  }

  void tmpl(T)(T x) // Template: not exported
  {
  }

  @gSignal void changed(int v) // Signal (emitChanged and connectChanged are generated): not a method
  {
  }

  @gVirtual protected int vfuncCompute(int x) // Vfunc: the invoker compute is exported as such, not as a method
  {
    return x + 1;
  }

  protected void prot() // Not public
  {
  }

  void _under() // Internal name
  {
  }
}

/// The class options replace the namespace defaults: functions and properties, warnings for skipped members
class AutoClass : ObjectWrap
{
  mixin GObjectExport!(null, "Class", GidExport.Functions | GidExport.Properties, GidSkip.Warn);

  private int _count;
  private string _label = "lbl";
  private int _secret;

  int method() // Not exported: no GidExport.Methods
  {
    return 1;
  }

  static int twice(int x) // gidgen_auto_class_twice, no GError (no GidExport.Throws)
  {
    return 2 * x;
  }

  @property int count() // Read-write property "count"
  {
    return _count;
  }

  @property void count(int v)
  {
    _count = v;
  }

  @property string label() // Read-only property "label"
  {
    return _label;
  }

  @property void secret(int v) // Write-only property "secret"
  {
    _secret = v;
  }

  @gNoExport @property int hiddenProp() // Excluded
  {
    return 0;
  }

  @property int delegate() callback() // Unsupported property type: warning
  {
    return null;
  }

  @property static int staticProp() // Static: not a property
  {
    return 0;
  }

  @gProperty int explicitProp = 3; // Explicit attribute
}

/// GObjectExportAll: methods, functions and properties (no Throws)
class AutoAll : ObjectWrap
{
  mixin GObjectExportAll!(null, "All");

  private int _level = 7;

  int add(int a) // gidgen_auto_all_add
  {
    return _level + a;
  }

  static string greet(string who) // gidgen_auto_all_greet
  {
    return "hi " ~ who;
  }

  @property int level() // Property "level"
  {
    return _level;
  }

  @property void level(int v)
  {
    _level = v;
  }

  // Overloads: each with a distinct @gMethod name, or all but one @gNoExport
  @gMethod("scale_int") int scale(int x)
  {
    return x * 2;
  }

  @gMethod("scale_double") double scale(double x)
  {
    return x * 2;
  }

  int pick(int x)
  {
    return x;
  }

  @gNoExport int pick(string s)
  {
    return 0;
  }
}

// The exported functions as C code sees them
extern(C) nothrow
{
  int gidgen_auto_ns_sum(GObject* self, int a, int b);
  void gidgen_auto_ns_fail(GObject* self, const(char)* msg, GError** error);
  int gidgen_auto_ns_quiet(GObject* self);
  int gidgen_auto_ns_renamed(GObject* self);
  int gidgen_auto_ns_compute(GObject* self, int x);
  int gidgen_auto_class_twice(int x);
  GObject* gidgen_auto_all_new();
  int gidgen_auto_all_add(GObject* self, int a);
  char* gidgen_auto_all_greet(const(char)* who);
  int gidgen_auto_all_scale_int(GObject* self, int x);
  double gidgen_auto_all_scale_double(GObject* self, double x);
  int gidgen_auto_all_pick(GObject* self, int x);
}

private string[] exportedNames(T)()
{
  string[] names;
  foreach (m; _dExportMethods!T)
    names ~= m.name;
  return names;
}

/// Which members are exported
unittest
{
  import std.algorithm : sort;

  assert(exportedNames!AutoNs.sort.release == ["fail", "quiet", "renamed", "sum"]);
  assert(exportedNames!AutoClass == ["twice"]);
  assert(exportedNames!AutoAll.sort.release == ["add", "greet", "pick", "scale_double", "scale_int"]);

  foreach (m; _dExportMethods!AutoNs)
    assert(m.throws == (m.name == "fail"), m.name); // GidExport.Throws: not nothrow, no @gThrows(false)

  foreach (m; _dExportMethods!AutoAll)
  {
    assert(!m.throws);
    assert(m.kind == (m.name == "greet" ? _DExportKind.Function : _DExportKind.Method));
  }
}

/// Calls through the exported functions
unittest
{
  auto ns = new AutoNs;
  auto self = cast(GObject*)ns._cPtr;
  GError* err;

  assert(gidgen_auto_ns_sum(self, 2, 3) == 5);
  gidgen_auto_ns_fail(self, "bad", &err);
  assert(err && err.domain == gidDExceptionQuark && err.message.fromStringz == "bad");
  g_clear_error(&err);
  assert(gidgen_auto_ns_quiet(self) == 4);
  assert(gidgen_auto_ns_renamed(self) == 5);
  assert(gidgen_auto_ns_compute(self, 1) == 2); // vfunc invoker

  assert(gidgen_auto_class_twice(4) == 8);

  auto all = gidgen_auto_all_new();
  assert(gidgen_auto_all_add(all, 1) == 8);
  auto s = gidgen_auto_all_greet("you");
  assert(s.fromStringz == "hi you");
  g_free(s);
  assert(gidgen_auto_all_scale_int(all, 3) == 6 && gidgen_auto_all_scale_double(all, 1.5) == 3.0);
  assert(gidgen_auto_all_pick(all, 9) == 9);
  g_object_unref(all);
}

private GParamSpec* findProperty(GObject* obj, string name)
{
  import std.string : toStringz;
  return g_object_class_find_property(cast(GObjectClass*)(cast(GTypeInstance*)obj).gClass, name.toStringz);
}

private extern(C) void countNotify(GObject* obj, GParamSpec* pspec, void* data)
{
  (*cast(int*)data)++;
}

/// D @property accessors as GObject properties (GidExport.Properties)
unittest
{
  import gid.gobject2.types : ParamFlags;

  auto cls = new AutoClass;
  auto self = cast(GObject*)cls._cPtr;

  auto count = findProperty(self, "count");
  assert(count && (count.flags & ParamFlags.Readable) && (count.flags & ParamFlags.Writable));
  auto label = findProperty(self, "label");
  assert(label && (label.flags & ParamFlags.Readable) && !(label.flags & ParamFlags.Writable));
  auto secret = findProperty(self, "secret");
  assert(secret && !(secret.flags & ParamFlags.Readable) && (secret.flags & ParamFlags.Writable));
  assert(findProperty(self, "explicit-prop"));
  assert(!findProperty(self, "hidden-prop") && !findProperty(self, "callback") && !findProperty(self, "static-prop"));
  assert(!findProperty(cast(GObject*)(new AutoNs)._cPtr, "size")); // No GidExport.Properties

  // Same semantics as @gProperty on accessors: g_object_set calls the setter and notifies when the value changes
  int notified;
  g_signal_connect_data(self, "notify::count", cast(GCallback)&countNotify, &notified, null, cast(GConnectFlags)0);

  GValue val;
  g_value_init(&val, GTypeEnum.Int);
  g_value_set_int(&val, 5);
  g_object_set_property(self, "count", &val);
  assert(cls.count == 5 && notified == 1);
  g_object_set_property(self, "count", &val); // Same value: no notify
  assert(notified == 1);
  cls.count = 6; // The D setter doesn't notify
  assert(notified == 1);
  g_value_set_int(&val, 0);
  g_object_get_property(self, "count", &val);
  assert(g_value_get_int(&val) == 6);
  g_value_unset(&val);

  g_value_init(&val, GTypeEnum.String);
  g_object_get_property(self, "label", &val);
  assert(g_value_get_string(&val).fromStringz == "lbl");
  g_value_unset(&val);

  auto all = new AutoAll; // GObjectExportAll includes properties
  assert(findProperty(cast(GObject*)all._cPtr, "level"));
}

/// GidSkip.Warn: skipped members are reported (printed with pragma(msg) when the module is compiled)
unittest
{
  static assert(_dExportSkipped!AutoClass == ["AutoClass.callback: not exported, unsupported property type"
    ~ " int delegate() (mark it @gNoExport to silence this)"]);
  static assert(_dExportSkipped!AutoNs.length == 1); // feed, skipped silently
}

/// Rules checked at compile time
unittest
{
  // Positive control
  static assert(__traits(compiles, {
    static class Good : ObjectWrap { mixin GObjectExport!("GidgenAutoGood", "A", GidExport.All); int f(int a) { return a; } }
  }));
  static assert(!__traits(compiles, {
    static class Bad : ObjectWrap
    {
      mixin GObjectExport!("GidgenAutoBad", "A", GidExport.Methods);
      void f(int a) {}
      void f(string s) {}
    }
  }), "Two auto-exported overloads");
  static assert(!__traits(compiles, {
    static class Bad : ObjectWrap
    {
      mixin GObjectExport!("GidgenAutoBad", "B", GidExport.Methods);
      @gMethod("f_int") void f(int a) {}
      void f(string s) {}
    }
  }), "One overload without a @gMethod name");
  static assert(!__traits(compiles, {
    static class Bad : ObjectWrap
    {
      mixin GObjectExport!("GidgenAutoBad", "C", GidExport.Methods, GidSkip.Error);
      void f(int a) {}
      void f(int delegate() d) {}
    }
  }), "Overloads are errors whatever the GidSkip mode, also with an unsupported overload");
  static assert(!__traits(compiles, {
    static class Bad : ObjectWrap { mixin GObjectExport!("GidgenAutoBad", "D", GidExport.Methods, GidSkip.Error);
      void f(int delegate() d) {} }
  }), "GidSkip.Error");
  static assert(!__traits(compiles, {
    static class Bad : ObjectWrap { mixin GObjectExport!("GidgenAutoBad", "E", GidExport.Properties, GidSkip.Error);
      @property int delegate() f() { return null; } }
  }), "GidSkip.Error for a property");
  static assert(__traits(compiles, {
    static class Good : ObjectWrap { mixin GObjectExport!("GidgenAutoGood", "B", GidExport.Methods, GidSkip.Error);
      @gNoExport void f(int delegate() d) {} }
  }), "@gNoExport silences GidSkip.Error");
  static assert(!__traits(compiles, {
    static class Bad : ObjectWrap { mixin GObjectExport!("GidgenAutoBad", "F", GidExport.Methods, GidSkip.Silent);
      @gMethod void f(int delegate() d) {} }
  }), "Explicit attribute on an unsupported type stays an error");
  static assert(!__traits(compiles, {
    static class Bad : ObjectWrap { mixin GObjectExport!("GidgenAutoBad", "G", GidExport.Methods); int getType() { return 0; } }
  }), "Auto-exported name collides with get_type");
  static assert(!__traits(compiles, {
    static class Bad : ObjectWrap { mixin GObjectExport!("GidgenAutoBad", "H"); @gMethod @gNoExport void f() {} }
  }), "@gNoExport with @gMethod");
  static assert(!__traits(compiles, {
    static class Bad : ObjectWrap { mixin GObjectExport!("GidgenAutoBad", "I", GidSkip.Warn, GidSkip.Error); }
  }), "Two GidSkip options");
  static assert(!__traits(compiles, {
    static class Bad : ObjectWrap { mixin GObjectExport!("GidgenAutoBad", "J", "x_get_type", "extra"); }
  }), "Too many strings");
  static assert(!__traits(compiles, {
    static class Bad : ObjectWrap { mixin GObjectExport!("GidgenAutoBad", "K", 42); }
  }), "Unsupported option");
}
