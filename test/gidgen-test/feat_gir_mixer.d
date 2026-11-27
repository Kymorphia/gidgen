/// GIR export across D namespaces: the "mixer" library uses the types of the "widgets" library (feat_gir_widgets),
/// which are written as GidgenWidgets.Knob with an include of GidgenWidgets-1.0
module feat_gir_mixer;

import feat_gir_widgets : Knob, Turnable;
import gid.gobject2.object : GirInfo, GObjectExport, GObjectNamespace, ObjectWrap, girHeader, girXml, gMethod, gNullable,
  gProperty, gSignal;
import std.algorithm : canFind;

mixin GObjectNamespace!("GidgenMixer", "2.0", "gidgen_mixer");

/// Class with methods, a property and a signal using the types of the other namespace
class Channel : ObjectWrap, Turnable
{
  mixin GObjectExport;

  private Knob _gain;

  @gProperty Knob knob;

  this()
  {
    super();
    _gain = new Knob;
  }

  @gMethod Knob getGainKnob() { return _gain; }
  @gMethod void setGainKnob(Knob k) { _gain = k; }
  @gMethod bool spin(@gNullable Turnable t) { return t !is null; }
  @gSignal void knobChanged(Knob k) {}

  int vfuncTurn(int steps) { return steps; }
}

/// Subclass of a class of the other namespace
class BigKnob : Knob
{
  mixin GObjectExport;

  override protected double vfuncScaled(double factor) { return 2 * factor; }
}

private void checkHas(string xml, string[] parts...)
{
  foreach (p; parts)
    assert(xml.canFind(p), "Missing in GIR: " ~ p);
}

/// Types of another D namespace: include and qualified names, the version comes from GObjectNamespace
unittest
{
  auto xml = girXml!(GirInfo("GidgenMixer", null, "libgidgen-mixer.so"), feat_gir_mixer);
  checkHas(xml, `<include name="GidgenWidgets" version="1.0"/>`,
    `<namespace name="GidgenMixer" version="2.0" shared-library="libgidgen-mixer.so"`,
    `<implements name="GidgenWidgets.Turnable"/>`,
    `<return-value transfer-ownership="full"><type name="GidgenWidgets.Knob" c:type="GidgenWidgetsKnob*"/>`,
    `<parameter name="k" transfer-ownership="none"><type name="GidgenWidgets.Knob" c:type="GidgenWidgetsKnob*"/>`,
    `<parameter name="t" transfer-ownership="none" nullable="1"><type name="GidgenWidgets.Turnable"`
    ~ ` c:type="GidgenWidgetsTurnable*"/>`,
    `<property name="knob" writable="1" transfer-ownership="none"><type name="GidgenWidgets.Knob"`,
    `<glib:signal name="knob-changed" when="last">`,
    `<class name="BigKnob" c:symbol-prefix="big_knob" c:type="GidgenMixerBigKnob" parent="GidgenWidgets.Knob"`,
    `<field name="parent_class"><type name="GidgenWidgets.KnobClass" c:type="GidgenWidgetsKnobClass"/></field>`,
    `<field name="parent_instance" readable="0" private="1"><type name="GidgenWidgets.Knob" c:type="GidgenWidgetsKnob"/>`);
  assert(!xml.canFind(`<class name="Knob"`)); // Only the types of the namespace

  auto widgets = girXml!(GirInfo("GidgenWidgets", "1.0", "libgidgen-widgets.so"), imported!"feat_gir_widgets");
  checkHas(widgets, `<namespace name="GidgenWidgets" version="1.0"`, `<class name="Knob"`, `<interface name="Turnable"`);
}

/// GirInfo and GObjectNamespace versions must agree, and one of them is needed
unittest
{
  import std.exception : collectExceptionMsg;

  assert(girXml!(GirInfo("GidgenMixer", "2.0", "x.so"), feat_gir_mixer).canFind(`version="2.0"`));

  auto msg = collectExceptionMsg(girXml!(GirInfo("GidgenMixer", "3.0", "x.so"), feat_gir_mixer));
  assert(msg.canFind("the GirInfo version 3.0 differs from the GIR version 2.0 given to GObjectNamespace"), msg);

  msg = collectExceptionMsg(girXml!(GirInfo("GidgenGir", null, "x.so"), imported!"feat_export_gir"));
  assert(msg.canFind("no GIR version: pass it to GirInfo or to GObjectNamespace"), msg);
}

/// The GIR of each namespace compiles, the mixer one with the widgets one as include
unittest
{
  import std.file : mkdirRecurse, rmdirRecurse, tempDir, write;
  import std.path : buildPath;
  import std.process : execute;

  auto dir = buildPath(tempDir, "gidgen-test-gir-ns");
  mkdirRecurse(dir);
  scope(exit) rmdirRecurse(dir);

  write(buildPath(dir, "GidgenWidgets-1.0.gir"), girXml!(GirInfo("GidgenWidgets", null, "libgidgen-widgets.so"),
    imported!"feat_gir_widgets"));
  write(buildPath(dir, "GidgenMixer-2.0.gir"), girXml!(GirInfo("GidgenMixer", null, "libgidgen-mixer.so"),
    feat_gir_mixer));

  foreach (name; ["GidgenWidgets-1.0", "GidgenMixer-2.0"])
  {
    auto res = execute(["g-ir-compiler", "--includedir=" ~ dir, buildPath(dir, name ~ ".gir"), "-o",
      buildPath(dir, name ~ ".typelib")]);
    assert(res.status == 0, res.output);
  }
}

/// GIR version and symbol prefix options of GObjectNamespace
unittest
{
  import gid.gobject2.dig : _dNamespaceStrings;
  import gid.gobject2.object : GidExport, GidSkip;

  static assert(_dNamespaceStrings(["1.0"]).nsVersion == "1.0" && _dNamespaceStrings(["1.0"]).prefix is null);
  static assert(_dNamespaceStrings(["my_lib", "2"]).prefix == "my_lib" && _dNamespaceStrings(["my_lib", "2"]).nsVersion == "2");
  static assert(_dNamespaceStrings([null, "1.0"]).error is null && _dNamespaceStrings([null]).prefix is null);
  static assert(_dNamespaceStrings(["1.0", "2.0"]).error == "GObjectNamespace: more than one GIR version");
  static assert(_dNamespaceStrings(["a", "b"]).error.canFind("more than one C symbol prefix"));
  static assert(_dNamespaceStrings(["1..0"]).error.canFind("invalid GIR version"));
  static assert(_dNamespaceStrings(["1.0a"]).error.canFind("invalid GIR version"));
  static assert(_dNamespaceStrings(["my-lib"]).error.canFind("invalid C symbol prefix"));

  // All the forms compile
  static assert(__traits(compiles, { struct S { mixin GObjectNamespace!("Ns1"); } }));
  static assert(__traits(compiles, { struct S { mixin GObjectNamespace!("Ns1", "1.0"); static assert(_gidNamespaceVersion == "1.0"
    && _gidSymbolPrefix == "ns1"); } }));
  static assert(__traits(compiles, { struct S { mixin GObjectNamespace!("Ns1", "ns", "1.0", GidExport.All, GidSkip.Error);
    static assert(_gidNamespaceVersion == "1.0" && _gidSymbolPrefix == "ns"); } }));
  static assert(__traits(compiles, { struct S { mixin GObjectNamespace!("Ns1", null, GidExport.All);
    static assert(_gidNamespaceVersion is null && _gidSymbolPrefix == "ns1"); } }));
  static assert(!__traits(compiles, { struct S { mixin GObjectNamespace!("Ns1", "1.0", "2.0"); } }));
  static assert(!__traits(compiles, { struct S { mixin GObjectNamespace!("Ns1", "a", "b", "1.0"); } }));
}

/// C header of a namespace using the types of another D namespace: includes its header (<namespace>.h)
unittest
{
  import std.array : split;
  import std.file : mkdirRecurse, rmdirRecurse, tempDir, write;
  import std.path : buildPath;
  import std.process : execute, executeShell;

  auto h = girHeader!(GirInfo("GidgenMixer", null, "libgidgen-mixer.so"), feat_gir_mixer);
  checkHas(h, "#include <glib-object.h>\n#include <gidgen_widgets.h>\n",
    "struct _GidgenMixerBigKnob\n{\n  GidgenWidgetsKnob parent_instance;\n};\n",
    "struct _GidgenMixerBigKnobClass\n{\n  GidgenWidgetsKnobClass parent_class;\n");

  auto dir = buildPath(tempDir, "gidgen-test-header-ns");
  mkdirRecurse(dir);
  scope(exit) rmdirRecurse(dir);

  write(buildPath(dir, "gidgen_widgets.h"), girHeader!(GirInfo("GidgenWidgets", null, "libgidgen-widgets.so"),
    imported!"feat_gir_widgets"));
  write(buildPath(dir, "gidgen_mixer.h"), h);
  write(buildPath(dir, "test.c"), "#include <gidgen_mixer.h>\n");
  auto cflags = executeShell("pkg-config --cflags gobject-2.0");
  auto res = execute(["gcc", "-fsyntax-only", "-Wall", "-Werror", "-I" ~ dir] ~ cflags.output.split
    ~ buildPath(dir, "test.c"));
  assert(res.status == 0, res.output);
}
