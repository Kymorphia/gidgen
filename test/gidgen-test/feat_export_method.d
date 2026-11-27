/// Exported methods, functions and constructors of GObjectExport classes (@gMethod, @gConstructor, @gThrows,
/// @gNullable, @gTransfer), called from D through their C functions
module feat_export_method;

import gid.core.utils : GidCallbackExceptionInfo, setCallbackExceptionHandler;
import gid.gidgenfeat1.types : Color;
import gid.glib2.bytes : Bytes;
import gid.glib2.c.functions;
import gid.glib2.c.types;
import gid.glib2.error : ErrorWrap;
import gid.glib2.variant : Variant;
import gid.gobject2.c.functions;
import gid.gobject2.c.types;
import gid.gobject2.types : GTypeEnum;
import gid.gobject2.object : GidOwnership, GObjectExport, GObjectNamespace, GObjectSubclass, GInterfaceExport,
  ObjectWrap, gConstructor, gidDExceptionQuark, gMethod, gNullable, gThrows, gTransfer, gVirtual;
import std.string : fromStringz, toStringz;
import std.typecons : No;

mixin GObjectNamespace!("GidgenMeth", "gidgen_meth");

/// Exported class with exported methods
class MethObj : ObjectWrap
{
  mixin GObjectExport!(null, "Obj");

  int value;
  string text;

  this()
  {
    super();
  }

  /// gidgen_meth_obj_new_value: a constructor with a name
  @gConstructor("value") this(int v)
  {
    super();
    value = v;
  }

  /// gidgen_meth_obj_new_with_text: a static factory
  @gConstructor static MethObj withText(string t)
  {
    auto o = new MethObj;
    o.text = t;
    return o;
  }

  @gMethod int add(int a, int b)
  {
    return value + a + b;
  }

  @gMethod double scalars(bool b, byte i8, ubyte u8, short i16, ushort u16, uint u32, long i64, ulong u64, float f,
    Color c)
  {
    return (b ? 1.0 : 0.0) + i8 + u8 + i16 + u16 + cast(double)u32 + cast(double)i64 + cast(double)u64 + f + c;
  }

  @gMethod Color nextColor(Color c)
  {
    return c == Color.Red ? Color.Green : Color.Blue;
  }

  @gMethod string describe(@gNullable string prefix)
  {
    return (prefix is null ? "none" : prefix) ~ ":" ~ text;
  }

  @gMethod @gNullable string maybe(bool give)
  {
    return give ? "given" : null;
  }

  @gMethod string[] split(string s)
  {
    import std.array : split;
    return s.split(",");
  }

  @gMethod int count(string[] items)
  {
    return cast(int)items.length;
  }

  @gMethod void outs(int a, out int doubled, out string str, out MethObj obj, ref int counter)
  {
    doubled = a * 2;
    str = "out" ~ cast(char)('0' + a);
    obj = this;
    counter++;
  }

  @gMethod MethObj same()
  {
    return this;
  }

  @gMethod @gTransfer(GidOwnership.None) MethObj peek()
  {
    return this;
  }

  @gMethod @gNullable MethObj other(@gNullable ObjectWrap o)
  {
    return o ? cast(MethObj)o : null;
  }

  @gMethod @gThrows int fail(int code)
  {
    if (code == 1)
      throw new ErrorWrap(g_quark_from_string("gidgen-meth"), 7, "wrapped error");

    if (code == 2)
      throw new Exception("plain exception");

    return 5;
  }

  @gMethod @gThrows void failOut(out string s)
  {
    s = "set";
    throw new Exception("after out");
  }

  @gMethod int failNoThrows()
  {
    throw new Exception("not reported");
  }

  @gMethod @gThrows(false) int notThrowing()
  {
    return 3;
  }

  @gMethod("to_text") string toText()
  {
    return "text " ~ text;
  }

  @gMethod static int twice(int x)
  {
    return x * 2;
  }

  @gMethod Variant echoVariant(Variant v)
  {
    return v;
  }

  @gMethod Bytes echoBytes(Bytes b)
  {
    return b;
  }
}

/// Exported subclass overriding an exported method: the C function of the parent calls the override
class MethChild : MethObj
{
  mixin GObjectExport!(null, "Child");

  override int add(int a, int b)
  {
    return 100 + super.add(a, b);
  }
}

// The exported functions as C code sees them
extern(C) nothrow
{
  GType gidgen_meth_obj_get_type();
  GObject* gidgen_meth_obj_new();
  GObject* gidgen_meth_obj_new_value(int v);
  GObject* gidgen_meth_obj_new_with_text(const(char)* t);
  int gidgen_meth_obj_add(GObject* self, int a, int b);
  double gidgen_meth_obj_scalars(GObject* self, int b, byte i8, ubyte u8, short i16, ushort u16, uint u32, long i64,
    ulong u64, float f, int c);
  int gidgen_meth_obj_next_color(GObject* self, int c);
  char* gidgen_meth_obj_describe(GObject* self, const(char)* prefix);
  char* gidgen_meth_obj_maybe(GObject* self, int give);
  char** gidgen_meth_obj_split(GObject* self, const(char)* s);
  int gidgen_meth_obj_count(GObject* self, const(char*)* items);
  void gidgen_meth_obj_outs(GObject* self, int a, int* doubled, char** str, GObject** obj, int* counter);
  GObject* gidgen_meth_obj_same(GObject* self);
  GObject* gidgen_meth_obj_peek(GObject* self);
  GObject* gidgen_meth_obj_other(GObject* self, GObject* o);
  int gidgen_meth_obj_fail(GObject* self, int code, GError** error);
  void gidgen_meth_obj_fail_out(GObject* self, char** s, GError** error);
  int gidgen_meth_obj_fail_no_throws(GObject* self);
  int gidgen_meth_obj_not_throwing(GObject* self);
  char* gidgen_meth_obj_to_text(GObject* self);
  int gidgen_meth_obj_twice(int x);
  GVariant* gidgen_meth_obj_echo_variant(GObject* self, GVariant* v);
  void* gidgen_meth_obj_echo_bytes(GObject* self, void* b);
  GObject* gidgen_meth_child_new();
}

/// Names and kinds
unittest
{
  import gid.gobject2.dig_method : _DExportKind, _dExportMethods;

  enum methods = _dExportMethods!MethObj;
  static assert(methods[0].mem == "__ctor" && methods[0].name == "new_value"
    && methods[0].kind == _DExportKind.Constructor);
  static assert(methods[1].name == "new_with_text" && methods[1].kind == _DExportKind.Constructor);
  static assert(methods[2].name == "add" && methods[2].kind == _DExportKind.Method && !methods[2].throws);

  foreach (m; methods)
  {
    if (m.name == "fail")
      assert(m.throws);
    else if (m.name == "not_throwing")
      assert(!m.throws);
    else if (m.name == "twice")
      assert(m.kind == _DExportKind.Function);
    else if (m.mem == "toText")
      assert(m.name == "to_text");
  }

  static assert(_dExportMethods!MethChild.length == 0); // Overrides are exported by the parent
}

/// Constructors
unittest
{
  auto obj = gidgen_meth_obj_new_value(7);
  assert(g_type_check_instance_is_a(cast(GTypeInstance*)obj, MethObj._getGType));
  auto d = ObjectWrap._getDObject!MethObj(obj, No.Take);
  assert(d.value == 7);
  g_object_unref(obj); // Transfer full

  obj = gidgen_meth_obj_new_with_text("abc");
  assert(ObjectWrap._getDObject!MethObj(obj, No.Take).text == "abc");
  g_object_unref(obj);
}

/// Scalars, enums, strings and string arrays
unittest
{
  auto d = new MethObj(10);
  auto self = cast(GObject*)d._cPtr;

  assert(gidgen_meth_obj_add(self, 1, 2) == 13);
  assert(gidgen_meth_obj_scalars(self, 1, -2, 200, -300, 60_000, 4_000_000_000u, -5_000_000_000, 6, 0.5f,
    Color.Green) == 1 - 2 + 200 - 300 + 60_000 + 4_000_000_000.0 - 5_000_000_000 + 6 + 0.5 + Color.Green);
  assert(gidgen_meth_obj_next_color(self, Color.Red) == Color.Green);

  d.text = "t";
  auto s = gidgen_meth_obj_describe(self, "p");
  assert(s.fromStringz == "p:t");
  g_free(s);
  s = gidgen_meth_obj_describe(self, null); // @gNullable
  assert(s.fromStringz == "none:t");
  g_free(s);
  assert(gidgen_meth_obj_maybe(self, 0) is null);
  s = gidgen_meth_obj_maybe(self, 1);
  assert(s.fromStringz == "given");
  g_free(s);

  auto strv = gidgen_meth_obj_split(self, "a,bc,d");
  assert(g_strv_length(strv) == 3 && strv[1].fromStringz == "bc");
  assert(gidgen_meth_obj_count(self, cast(const(char*)*)strv) == 3);
  g_strfreev(strv);

  s = gidgen_meth_obj_to_text(self);
  assert(s.fromStringz == "text t");
  g_free(s);
  assert(gidgen_meth_obj_twice(21) == 42); // Static method
}

/// Out and ref parameters, objects and transfer
unittest
{
  auto d = new MethObj;
  auto self = cast(GObject*)d._cPtr;
  int doubled, counter = 5;
  char* str;
  GObject* obj;
  auto refs = self.refCount;

  gidgen_meth_obj_outs(self, 3, &doubled, &str, &obj, &counter);
  assert(doubled == 6 && str.fromStringz == "out3" && obj == self && counter == 6);
  assert(self.refCount == refs + 1); // Out object: transfer full
  g_free(str);
  g_object_unref(obj);
  gidgen_meth_obj_outs(self, 1, null, null, null, &counter); // Out pointers are optional
  assert(counter == 7);

  obj = gidgen_meth_obj_same(self);
  assert(obj == self && self.refCount == refs + 1);
  g_object_unref(obj);
  obj = gidgen_meth_obj_peek(self); // @gTransfer(GidOwnership.None)
  assert(obj == self && self.refCount == refs);

  assert(gidgen_meth_obj_other(self, null) is null);
  obj = gidgen_meth_obj_other(self, self);
  assert(obj == self);
  g_object_unref(obj);

  auto v = g_variant_new_int32(42); // Floating, sunk by the method
  auto vret = gidgen_meth_obj_echo_variant(self, v);
  assert(vret == v && g_variant_get_int32(vret) == 42);
  g_variant_unref(vret);

  auto bytes = g_bytes_new("xyz".ptr, 3);
  auto bret = cast(GBytes*)gidgen_meth_obj_echo_bytes(self, bytes);
  assert(g_bytes_get_size(bret) == 3);
  g_bytes_unref(bret);
  g_bytes_unref(bytes);
}

/// Exported methods use D virtual dispatch: overrides in D subclasses are called
unittest
{
  auto child = gidgen_meth_child_new();
  assert(gidgen_meth_obj_add(child, 1, 2) == 103);
  g_object_unref(child);
}

private __gshared int handledExceptions;

private void countException(Exception e, GidCallbackExceptionInfo info) nothrow
{
  handledExceptions++;
}

/// Exceptions: GError with @gThrows, the callback exception handler otherwise
unittest
{
  auto d = new MethObj;
  auto self = cast(GObject*)d._cPtr;
  GError* err;

  assert(gidgen_meth_obj_fail(self, 0, &err) == 5 && err is null);
  assert(gidgen_meth_obj_fail(self, 1, &err) == 0);
  assert(err && err.domain == g_quark_from_string("gidgen-meth") && err.code == 7
    && err.message.fromStringz == "wrapped error");
  g_clear_error(&err);
  assert(gidgen_meth_obj_fail(self, 2, &err) == 0);
  assert(err && err.domain == gidDExceptionQuark && err.code == 0 && err.message.fromStringz == "plain exception");
  assert(g_quark_to_string(err.domain).fromStringz == "gid-d-exception-quark");
  g_clear_error(&err);
  assert(gidgen_meth_obj_fail(self, 2, null) == 0); // Error ignored

  char* s = cast(char*)1;
  gidgen_meth_obj_fail_out(self, &s, &err);
  assert(err && s is null); // Out values are cleared on errors
  g_clear_error(&err);

  setCallbackExceptionHandler(&countException);
  scope(exit) setCallbackExceptionHandler(null);
  handledExceptions = 0;
  assert(gidgen_meth_obj_fail_no_throws(self) == 0 && handledExceptions == 1);
  assert(gidgen_meth_obj_not_throwing(self) == 3);
}

private __gshared int criticals;

private extern(C) void methCountCritical(const(char)* domain, GLogLevelFlags level, const(char)* message, void* data)
  nothrow
{
  criticals++;
}

/// Wrong instances and NULL arguments log a critical message and return 0
unittest
{
  auto oldFatal = g_log_set_always_fatal(GLogLevelFlags.LevelError | GLogLevelFlags.FlagFatal);
  auto handler = g_log_set_handler("Gid", GLogLevelFlags.LevelCritical, &methCountCritical, null);
  scope(exit)
  {
    g_log_remove_handler("Gid", handler);
    g_log_set_always_fatal(oldFatal);
  }

  auto d = new MethObj;
  auto self = cast(GObject*)d._cPtr;
  auto other = cast(GObject*)g_object_new_with_properties(GTypeEnum.Object, 0, null, null);
  int counter;
  criticals = 0;

  assert(gidgen_meth_obj_add(other, 1, 2) == 0);
  assert(gidgen_meth_obj_add(null, 1, 2) == 0);
  auto valid = gidgen_meth_obj_describe(self, "x");
  assert(valid !is null);
  g_free(valid);
  assert(gidgen_meth_obj_split(self, null) is null); // Not @gNullable
  assert(gidgen_meth_obj_count(self, null) == 0);
  gidgen_meth_obj_outs(self, 1, null, null, null, null); // ref pointer is required
  assert(criticals == 5);
  g_object_unref(other);
}

/// Invalid declarations are compile-time errors
unittest
{
  static assert(__traits(compiles, {
    static class Good : ObjectWrap { mixin GObjectExport!("GidgenMethGood", "A"); @gMethod int f(int a) { return a; } }
  }));
  static assert(!__traits(compiles, {
    static class Bad : ObjectWrap { mixin GObjectExport!("GidgenMethBad", "A"); @gMethod void f(int delegate() d) {} }
  }), "Unsupported parameter type");
  static assert(!__traits(compiles, {
    static class Bad : ObjectWrap { mixin GObjectExport!("GidgenMethBad", "B"); @gMethod int[] f() { return null; } }
  }), "Unsupported return type");
  static assert(!__traits(compiles, {
    static class Bad : ObjectWrap { mixin GObjectExport!("GidgenMethBad", "C"); @gMethod void f(ref string s) {} }
  }), "ref string");
  static assert(!__traits(compiles, {
    static class Bad : ObjectWrap { mixin GObjectExport!("GidgenMethBad", "D"); @gMethod private void f() {} }
  }), "Not public");
  static assert(!__traits(compiles, {
    static class Bad : ObjectWrap
    {
      mixin GObjectExport!("GidgenMethBad", "E");
      @gMethod void f(int a) {}
      @gMethod void f(string s) {}
    }
  }), "Overloads with the same C name");
  static assert(!__traits(compiles, {
    static class Bad : ObjectWrap { mixin GObjectExport!("GidgenMethBad", "F"); @gMethod("get_type") void f() {} }
  }), "Name collision with get_type");
  static assert(!__traits(compiles, {
    static class Bad : ObjectWrap { mixin GObjectExport!("GidgenMethBad", "G"); @gMethod("Bad-Name") void f() {} }
  }), "Invalid name");
  static assert(!__traits(compiles, {
    static class Bad : ObjectWrap { mixin GObjectSubclass!"GidgenMethBadH"; @gMethod void f() {} }
  }), "Not exported");
  static assert(!__traits(compiles, {
    static class Bad : ObjectWrap { mixin GObjectExport!("GidgenMethBad", "I"); @gConstructor void f() {} }
  }), "Constructor which is not static");
  static assert(!__traits(compiles, {
    static class Local : ObjectWrap { mixin GObjectSubclass!"GidgenMethLocal"; }
    static class Bad : ObjectWrap { mixin GObjectExport!("GidgenMethBad", "J"); @gMethod void f(Local l) {} }
  }), "D class which is not exported");
  static assert(!__traits(compiles, {
    interface Bad { mixin GInterfaceExport!("GidgenMethBad", "K"); @gMethod int f(); }
  }), "Interfaces");
  static assert(!__traits(compiles, {
    static class Bad : ObjectWrap { mixin GObjectExport!("GidgenMethBad", "L"); @gMethod @gTransfer(GidOwnership.None)
      string f() { return null; } }
  }), "@gTransfer on a string");
}
