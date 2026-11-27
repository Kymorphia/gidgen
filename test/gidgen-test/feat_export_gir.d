/// GIR export (girXml): classes, interfaces, constructors, methods, functions, virtual methods, properties, signals,
/// includes and annotations, checked in the XML and compiled with g-ir-compiler
module feat_export_gir;

import gid.glib2.variant : Variant;
import gid.gobject2.initially_unowned : InitiallyUnowned;
import gid.gobject2.object : GidExport, GirInfo, GInterfaceExport, GObjectExport, GObjectNamespace, GObjectSubclass,
  ObjectWrap, girXml, gConstructor, gMethod, gNullable, gProperty, gSignal, gThrows, gTransfer, gVirtual, GidOwnership;
import gid.gobject2.types : BindingFlags, ParamFlags, SignalFlags;
import std.algorithm : canFind;
import std.string : indexOf;

mixin GObjectNamespace!("GidgenGir", "gidgen_gir"); // No GIR version: GirInfo gives it

/// Exported interface
interface GirShape
{
  mixin GInterfaceExport!(null, "Shape");

  @gVirtual int vfuncSides();
}

/// Exported interface extending another one
interface GirNamed : GirShape
{
  mixin GInterfaceExport!(null, "Named");

  @gVirtual void vfuncRename(string name);
}

/// D-defined enum: base type in virtual methods
enum GirLevel : ubyte
{
  Low,
  High,
}

/// Exported class
class GirBase : ObjectWrap, GirNamed
{
  mixin GObjectExport!(null, "Base");

  @gProperty bool flag = true;
  @gProperty double ratio = 1.5;
  @gProperty(ParamFlags.Readable | ParamFlags.Writable | ParamFlags.ConstructOnly) int fixed = -2;
  @gProperty string nothing;
  @gProperty string odd = "a<b>\"c\"&";

  private int _ro = 4;
  @gProperty @property int readOnly() { return _ro; }

  @gSignal(SignalFlags.RunFirst | SignalFlags.NoRecurse | SignalFlags.NoHooks) bool changed(long v, GirBase other)
  {
    return false;
  }

  @gSignal(SignalFlags.RunCleanup) void cleaned() {}

  int vfuncSides() { return 4; }
  void vfuncRename(string name) {}

  @gVirtual protected double vfuncWeigh(GirLevel level, bool heavy, string label, ObjectWrap obj) { return 0; }

  this() { super(); }

  @gConstructor("with_ratio") this(double r)
  {
    super();
    ratio = r;
  }

  @gMethod @gNullable string maybe() { return null; }
  @gMethod @gTransfer(GidOwnership.None) GirBase self() { return this; }
  @gMethod void outs(out GirBase full, @gTransfer(GidOwnership.None) out GirBase none, ref int counter,
    out string[] list) {}
  @gMethod string[] strv(string[] input, @gNullable GirBase other) { return input; }
  @gMethod Variant variant(Variant v) { return v; }
  @gMethod BindingFlags flags(BindingFlags f) { return f; }
  @gMethod @gThrows void fail() {}
  @gMethod static ushort width(ubyte a, short b, ulong c, float d) { return 0; }
  @gMethod @gConstructor static GirBase create() { return new GirBase; }
}

/// Exported subclass, a GInitiallyUnowned and an abstract class
class GirChild : GirBase
{
  mixin GObjectExport!(null, "Child");

  override protected double vfuncWeigh(GirLevel level, bool heavy, string label, ObjectWrap obj) { return 1; }
}

class GirFloating : InitiallyUnowned
{
  mixin GObjectExport!(null, "Floating", GidExport.Methods);

  int value() nothrow { return 1; }
}

abstract class GirAbstract : ObjectWrap
{
  mixin GObjectExport!(null, "Abstract");
}

/// Not exported: GObjectSubclass only, or another namespace
class GirInternal : ObjectWrap
{
  mixin GObjectSubclass;
}

class GirOther : ObjectWrap
{
  mixin GObjectExport!("GidgenGirOther", "Other");
}

/// Uses a type of another D namespace, which can't be described yet
class GirBad : ObjectWrap
{
  mixin GObjectExport!("GidgenGirBad", "Bad");

  @gMethod GirOther other() { return null; }
}

private void checkHas(string xml, string[] parts...)
{
  foreach (p; parts)
    assert(xml.canFind(p), "Missing in GIR: " ~ p);
}

/// The XML of the types of the namespace
unittest
{
  auto xml = girXml!(GirInfo("GidgenGir", "1.0", "libgidgen-gir.so", ["gidgen-gir.h"], ["gidgen-gir"]),
    feat_export_gir);

  checkHas(xml, `<include name="GLib" version="2.0"/>`, `<include name="GObject" version="2.0"/>`,
    `<package name="gidgen-gir"/>`, `<c:include name="gidgen-gir.h"/>`,
    `<namespace name="GidgenGir" version="1.0" shared-library="libgidgen-gir.so" c:identifier-prefixes="GidgenGir"`
    ~ ` c:symbol-prefixes="gidgen_gir">`);

  // Only the GObjectExport and GInterfaceExport types of the namespace
  assert(!xml.canFind("Internal") && !xml.canFind("Other") && !xml.canFind("Bad"));

  // Interfaces
  checkHas(xml, `<interface name="Shape" c:symbol-prefix="shape" c:type="GidgenGirShape" glib:type-name="GidgenGirShape"`
    ~ ` glib:get-type="gidgen_gir_shape_get_type" glib:type-struct="ShapeInterface">`,
    `<interface name="Named"`, `<prerequisite name="Shape"/>`, `<virtual-method name="rename" invoker="rename">`,
    `<method name="sides" c:identifier="gidgen_gir_shape_sides">`,
    `<record name="NamedInterface" c:type="GidgenGirNamedInterface" glib:is-gtype-struct-for="Named">`,
    `<field name="g_iface"><type name="GObject.TypeInterface" c:type="GTypeInterface"/></field>`);

  // Classes, class structs, constructors
  checkHas(xml, `<class name="Base" c:symbol-prefix="base" c:type="GidgenGirBase" parent="GObject.Object"`
    ~ ` glib:type-name="GidgenGirBase" glib:get-type="gidgen_gir_base_get_type" glib:type-struct="BaseClass">`,
    `<implements name="Named"/>`, `<class name="Child" c:symbol-prefix="child" c:type="GidgenGirChild" parent="Base"`,
    `<field name="parent_class"><type name="BaseClass" c:type="GidgenGirBaseClass"/></field>`,
    `<field name="parent_instance" readable="0" private="1"><type name="Base" c:type="GidgenGirBase"/></field>`,
    `<constructor name="new" c:identifier="gidgen_gir_base_new">`,
    `<constructor name="new_with_ratio" c:identifier="gidgen_gir_base_new_with_ratio">`,
    `<constructor name="new_create" c:identifier="gidgen_gir_base_new_create">`,
    `<class name="Abstract" c:symbol-prefix="abstract" c:type="GidgenGirAbstract" parent="GObject.Object"`
    ~ ` glib:type-name="GidgenGirAbstract" glib:get-type="gidgen_gir_abstract_get_type"`
    ~ ` glib:type-struct="AbstractClass" abstract="1">`,
    `<field name="parent_class"><type name="GObject.InitiallyUnownedClass" c:type="GInitiallyUnownedClass"/></field>`,
    `<constructor name="new" c:identifier="gidgen_gir_floating_new">` ~ "\n"
    ~ `        <return-value transfer-ownership="none"><type name="Floating" c:type="GidgenGirFloating*"/>`,
    `<method name="value" c:identifier="gidgen_gir_floating_value">`);
  assert(!xml.canFind("gidgen_gir_abstract_new"));
  auto child = xml[xml.indexOf(`<class name="Child"`) .. $];
  child = child[0 .. child.indexOf("</class>")];
  assert(!child.canFind("<implements") && !child.canFind("<virtual-method")); // Not repeated, overrides aren't new

  // Virtual methods: slots in the class struct, D enums as their base type, strings and objects not owned
  checkHas(xml, `<virtual-method name="weigh" invoker="weigh">`, `<method name="weigh" c:identifier="gidgen_gir_base_weigh">`,
    `<field name="weigh">`, `<callback name="weigh">`,
    `<parameter name="self" transfer-ownership="none"><type name="Base" c:type="GidgenGirBase*"/></parameter>`,
    `<parameter name="level" transfer-ownership="none"><type name="guint8" c:type="guint8"/></parameter>`,
    `<parameter name="heavy" transfer-ownership="none"><type name="gboolean" c:type="gboolean"/></parameter>`,
    `<parameter name="obj" transfer-ownership="none"><type name="GObject.Object" c:type="GObject*"/></parameter>`);

  // Methods, functions and their annotations
  checkHas(xml, `<method name="maybe" c:identifier="gidgen_gir_base_maybe">` ~ "\n"
    ~ `        <return-value transfer-ownership="full" nullable="1"><type name="utf8" c:type="char*"/></return-value>`,
    `<return-value transfer-ownership="none"><type name="Base" c:type="GidgenGirBase*"/></return-value>`,
    `<parameter name="full" direction="out" caller-allocates="0" optional="1" transfer-ownership="full"><type`
    ~ ` name="Base" c:type="GidgenGirBase**"/></parameter>`,
    `<parameter name="none" direction="out" caller-allocates="0" optional="1" transfer-ownership="none">`,
    `<parameter name="counter" direction="inout" transfer-ownership="full"><type name="gint" c:type="gint*"/>`,
    `<array c:type="char***" zero-terminated="1"><type name="utf8" c:type="char*"/></array>`,
    `<parameter name="input" transfer-ownership="none"><array c:type="const char* const*" zero-terminated="1">`,
    `<parameter name="other" transfer-ownership="none" nullable="1">`,
    `<type name="GLib.Variant" c:type="GVariant*"/>`,
    `<parameter name="f" transfer-ownership="none"><type name="GObject.BindingFlags" c:type="GBindingFlags"/>`,
    `<method name="fail" c:identifier="gidgen_gir_base_fail" throws="1">`,
    `<function name="width" c:identifier="gidgen_gir_base_width">`,
    `<return-value transfer-ownership="none"><type name="guint16" c:type="guint16"/></return-value>`,
    `<type name="gint16" c:type="gint16"/>`, `<type name="guint64" c:type="guint64"/>`,
    `<type name="gfloat" c:type="gfloat"/>`);

  // Properties: flags and default values from the registered GParamSpecs
  checkHas(xml, `<property name="flag" writable="1" transfer-ownership="none" default-value="TRUE">`
    ~ `<type name="gboolean" c:type="gboolean"/></property>`,
    `<property name="ratio" writable="1" transfer-ownership="none" default-value="1.5">`,
    `<property name="fixed" writable="1" construct-only="1" transfer-ownership="none" default-value="-2">`,
    `<property name="nothing" writable="1" transfer-ownership="none"><type name="utf8"`,
    `<property name="odd" writable="1" transfer-ownership="none" default-value="a&lt;b&gt;&quot;c&quot;&amp;">`,
    `<property name="read-only" transfer-ownership="none"`);

  // Signals: flags from g_signal_query
  checkHas(xml, `<glib:signal name="changed" when="first" no-recurse="1" no-hooks="1">`,
    `<parameter name="v" transfer-ownership="none"><type name="gint64" c:type="gint64"/></parameter>`,
    `<parameter name="other" transfer-ownership="none"><type name="Base" c:type="GidgenGirBase*"/></parameter>`,
    `<glib:signal name="cleaned" when="cleanup">`);
}

/// Auto-exported members, and a type of another namespace without a GIR version (problems are reported with an
/// exception)
unittest
{
  import feat_export_auto : AutoNs;
  import std.exception : collectExceptionMsg;

  auto xml = girXml!(GirInfo("GidgenAuto", "1.0", "x.so"), imported!"feat_export_auto");
  checkHas(xml, `<method name="fail" c:identifier="gidgen_auto_ns_fail" throws="1">`,
    `<function name="twice" c:identifier="gidgen_auto_class_twice">`,
    `<property name="count" writable="1" transfer-ownership="none"`, `<property name="label" transfer-ownership="none"`,
    `<property name="secret" readable="0" writable="1"`, `<constructor name="new" c:identifier="gidgen_auto_all_new">`);
  assert(!xml.canFind("plain_value") && !xml.canFind(`name="size"`));

  auto msg = collectExceptionMsg(girXml!(GirInfo("GidgenGirBad", "1.0", "x.so"), feat_export_gir));
  assert(msg.canFind("GirBad.other: type GirOther is in namespace GidgenGirOther, which has no GIR version"), msg);
}

/// g-ir-compiler accepts the GIR
unittest
{
  import std.file : mkdirRecurse, rmdirRecurse, tempDir, write;
  import std.path : buildPath;
  import std.process : execute;

  auto dir = buildPath(tempDir, "gidgen-test-gir");
  mkdirRecurse(dir);
  scope(exit) rmdirRecurse(dir);

  auto gir = buildPath(dir, "GidgenGir-1.0.gir");
  write(gir, girXml!(GirInfo("GidgenGir", "1.0", "libgidgen-gir.so"), feat_export_gir));
  auto res = execute(["g-ir-compiler", gir, "-o", buildPath(dir, "GidgenGir-1.0.typelib")]);
  assert(res.status == 0, res.output);

  gir = buildPath(dir, "GidgenAuto-1.0.gir");
  write(gir, girXml!(GirInfo("GidgenAuto", "1.0", "x.so"), imported!"feat_export_auto"));
  res = execute(["g-ir-compiler", gir, "-o", buildPath(dir, "GidgenAuto-1.0.typelib")]);
  assert(res.status == 0, res.output);
}
