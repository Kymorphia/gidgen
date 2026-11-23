/// GObject property tests for @gProperty on GObjectSubclass classes
module feat_property;

import core.memory : GC;
import gidgenfeat.c.functions;
import gidgenfeat.c.types;
import gidgenfeat.object : FeatObject = ObjectWrap;
import gid.glib2.c.functions : g_free, g_main_context_iteration;
import gid.glib2.variant : Variant;
import gid.gobject2.c.functions;
import gid.gobject2.c.types;
import gid.gobject2.object : GObjectSubclass, ObjectWrap, gProperty, gNotifyAlways;
import gid.gobject2.types : GTypeEnum, ParamFlags;
import gid.gobject2.value : Value, getVal, setVal;
import std.string : fromStringz, toStringz;
import std.typecons : No, Yes;

/// Test enum registered as a GType
enum PropColor
{
  Red = 0,
  Blue = 1,
}

/// Test flags registered as a GType
enum PropStyle : uint
{
  None = 0,
  Bold = 1,
  Italic = 2,
}

shared static this()
{
  static GEnumValue[3] colors = [
    GEnumValue(0, "prop-color-red", "red"),
    GEnumValue(1, "prop-color-blue", "blue"),
    GEnumValue(0, null, null),
  ];
  static GFlagsValue[4] styles = [
    GFlagsValue(0, "prop-style-none", "none"),
    GFlagsValue(1, "prop-style-bold", "bold"),
    GFlagsValue(2, "prop-style-italic", "italic"),
    GFlagsValue(0, null, null),
  ];
  g_enum_register_static("GidgenTestPropColor", colors.ptr);
  g_flags_register_static("GidgenTestPropStyle", styles.ptr);
}

/// Class with a property of each supported kind, plus construct / construct-only / explicit-notify
class PropObj : FeatObject
{
  mixin GObjectSubclass!"GidgenTestPropObj";

  @gProperty bool on = true;
  @gProperty byte tiny = -3;
  @gProperty ubyte utiny = 7;
  @gProperty short small = -200;
  @gProperty ushort usmall = 200;
  @gProperty int count = 1;
  @gProperty uint ucount = 2;
  @gProperty long big = -3;
  @gProperty ulong ubig = 4;
  @gProperty float frac = 1.5f;
  @gProperty double precise = 2.25;
  @gProperty string name = "n";
  @gProperty("full-name") string fullName = "fn";
  @(gProperty.create(typeName: "GidgenTestPropColor")) PropColor color = PropColor.Red;
  @(gProperty.create(typeName: "GidgenTestPropStyle")) PropStyle style = PropStyle.None;
  @gProperty Variant data;
  @gProperty FeatObject peer;
  @gProperty(ParamFlags.Construct) int constructVal = 10;
  @gProperty(ParamFlags.Readwrite | ParamFlags.ConstructOnly) int constructOnly = 20;

  private int _level = 5;
  int levelSets;

  @gProperty @property int level()
  {
    return _level;
  }

  @property int level(int v)
  {
    _level = v;
    levelSets++;
    return v;
  }

  private int _hidden = 3;
  @gProperty(ParamFlags.Readable) @property int hidden()
  {
    return _hidden;
  }

  @gProperty(ParamFlags.Readwrite | ParamFlags.ExplicitNotify) int quiet;
  @gProperty @gNotifyAlways int noisy = 1;

  private int _secret;
  @gProperty @property int secret(int v)
  {
    _secret = v;
    return v;
  }
}

private GType instType(void* cObj)
{
  return (cast(GTypeInstance*)cObj).gClass.gType;
}

private int notifyCount;
private string lastNotify;

private extern(C) void onNotify(GObject* obj, GParamSpec* pspec, void* data) nothrow
{
  notifyCount++;
  lastNotify = pspec && pspec.name ? pspec.name.fromStringz.idup : null;
}


private void collectProps()
{
  foreach (i; 0 .. 3)
  {
    GC.collect;
    while (g_main_context_iteration(null, false)) {}
  }
}

/// Property names, defaults, get/set from D and C, notify, and construct flags
unittest
{
  auto obj = new PropObj;
  assert(instType(obj._cPtr) == PropObj._getGType);
  assert(g_object_class_find_property(cast(GObjectClass*)g_type_class_peek(PropObj._getGType), "count") !is null);
  assert(g_object_class_find_property(cast(GObjectClass*)g_type_class_peek(PropObj._getGType), "full-name") !is null);
  assert(g_object_class_find_property(cast(GObjectClass*)g_type_class_peek(PropObj._getGType), "fullName") is null);

  auto countSpec = g_object_class_find_property(cast(GObjectClass*)g_type_class_peek(PropObj._getGType), "count");
  assert(countSpec && (countSpec.flags & GParamFlags.ExplicitNotify));
  auto noisySpec = g_object_class_find_property(cast(GObjectClass*)g_type_class_peek(PropObj._getGType), "noisy");
  assert(noisySpec && (noisySpec.flags & GParamFlags.ExplicitNotify) == 0);

  assert(obj.getProperty!bool("on") && obj.on);
  assert(obj.getProperty!byte("tiny") == -3);
  assert(obj.getProperty!ubyte("utiny") == 7);
  assert(obj.getProperty!short("small") == -200);
  assert(obj.getProperty!ushort("usmall") == 200);
  assert(obj.getProperty!int("count") == 1);
  assert(obj.getProperty!uint("ucount") == 2);
  assert(obj.getProperty!long("big") == -3);
  assert(obj.getProperty!ulong("ubig") == 4);
  assert(obj.getProperty!float("frac") == 1.5f);
  assert(obj.getProperty!double("precise") == 2.25);
  assert(obj.getProperty!string("name") == "n");
  assert(obj.getProperty!string("full-name") == "fn");
  assert(obj.getProperty!PropColor("color") == PropColor.Red);
  assert(obj.getProperty!PropStyle("style") == PropStyle.None);
  assert(obj.getProperty!int("level") == 5);
  assert(obj.getProperty!int("construct-val") == 10);
  assert(obj.getProperty!int("construct-only") == 20);
  assert(obj.getProperty!int("hidden") == 3);

  g_signal_connect_data(cast(GObject*)obj._cPtr, "notify".toStringz, cast(GCallback)&onNotify, null, null, GConnectFlags.Default);
  notifyCount = 0;

  obj.setProperty("count", 9);
  assert(obj.count == 9 && obj.getProperty!int("count") == 9 && notifyCount == 1 && lastNotify == "count");

  obj.setProperty("name", "z");
  assert(obj.name == "z" && notifyCount == 2 && lastNotify == "name");

  obj.setProperty("full-name", "Zed");
  assert(obj.fullName == "Zed");

  obj.setProperty("on", false);
  obj.setProperty("tiny", cast(byte)-1);
  obj.setProperty("utiny", cast(ubyte)9);
  obj.setProperty("small", cast(short)-8);
  obj.setProperty("usmall", cast(ushort)8);
  obj.setProperty("ucount", 11u);
  obj.setProperty("big", -99L);
  obj.setProperty("ubig", 99uL);
  obj.setProperty("frac", 0.5f);
  obj.setProperty("precise", 0.25);
  obj.setProperty("color", PropColor.Blue);
  obj.setProperty("style", PropStyle.Bold);
  obj.setProperty("level", 8);
  assert(obj.level == 8 && obj.levelSets == 1);

  auto peer = new FeatObject;
  obj.setProperty("peer", peer);
  assert(obj.getProperty!FeatObject("peer") is peer);

  auto variant = new Variant(42);
  obj.setProperty("data", variant);
  assert(obj.getProperty!Variant("data") !is null);

  // C get/set
  int cCount;
  g_object_get(cast(GObject*)obj._cPtr, "count", &cCount, null);
  assert(cCount == 9);
  g_object_set(cast(GObject*)obj._cPtr, "count", 15, null);
  assert(obj.count == 15 && obj.getProperty!int("count") == 15);

  char* cName;
  g_object_get(cast(GObject*)obj._cPtr, "name", &cName, null);
  assert(cName.fromStringz == "z");
  g_free(cName);

  // Explicit notify: set does not emit notify
  auto before = notifyCount;
  obj.setProperty("quiet", 4);
  assert(obj.quiet == 4 && notifyCount == before);
  obj.notify("quiet");
  assert(notifyCount == before + 1 && lastNotify == "quiet");

  auto hiddenSpec = g_object_class_find_property(cast(GObjectClass*)g_type_class_peek(PropObj._getGType), "hidden");
  assert(hiddenSpec && (hiddenSpec.flags & GParamFlags.Writable) == 0);

  destroy(obj);
  destroy(peer);
  collectProps();
}

/// Construct properties from C: g_object_new sets them (after the D default constructor)
unittest
{
  const(char)*[2] names = ["construct-val".ptr, "construct-only".ptr];
  GValue[2] vals;
  g_value_init(&vals[0], GTypeEnum.Int);
  g_value_set_int(&vals[0], 70);
  g_value_init(&vals[1], GTypeEnum.Int);
  g_value_set_int(&vals[1], 80);

  auto cObj = g_object_new_with_properties(PropObj._getGType, 2, names.ptr, vals.ptr);
  auto obj = cast(PropObj)ObjectWrap._getDObject!FeatObject(cObj, Yes.Take);
  assert(obj && obj.constructVal == 70 && obj.constructOnly == 80, "construct properties from C");
  // Default constructor ran first (field defaults), then construct properties overwrote them
  assert(obj.count == 1, "other defaults remain");

  auto coSpec = g_object_class_find_property(cast(GObjectClass*)g_type_class_peek(PropObj._getGType), "construct-only");
  assert(coSpec && (coSpec.flags & GParamFlags.ConstructOnly));

  destroy(obj);
  collectProps();
}


/// Notify modes: on-change (default), notifyAlways, user ExplicitNotify, write-only
unittest
{
  auto obj = new PropObj;
  g_signal_connect_data(cast(GObject*)obj._cPtr, "notify".toStringz, cast(GCallback)&onNotify, null, null, GConnectFlags.Default);
  notifyCount = 0;

  void checkOnChange(T)(string name, T same, T different)
  {
    obj.setProperty(name, same);
    auto before = notifyCount;
    obj.setProperty(name, same);
    assert(notifyCount == before, name ~ " same value notified");
    obj.setProperty(name, different);
    assert(notifyCount == before + 1 && lastNotify == name, name ~ " different value did not notify once");
  }

  checkOnChange("on", true, false);
  checkOnChange("tiny", cast(byte)-3, cast(byte)-1);
  checkOnChange("utiny", cast(ubyte)7, cast(ubyte)9);
  checkOnChange("small", cast(short)-200, cast(short)-8);
  checkOnChange("usmall", cast(ushort)200, cast(ushort)8);
  checkOnChange("count", 1, 9);
  checkOnChange("ucount", 2u, 11u);
  checkOnChange("big", -3L, -99L);
  checkOnChange("ubig", 4uL, 99uL);
  checkOnChange("frac", 1.5f, 0.5f);
  checkOnChange("precise", 2.25, 0.25);
  checkOnChange("name", "n", "z");
  checkOnChange("full-name", "fn", "Zed");
  checkOnChange("color", PropColor.Red, PropColor.Blue);
  checkOnChange("style", PropStyle.None, PropStyle.Bold);
  checkOnChange("level", 5, 8);
  checkOnChange("construct-val", 10, 11);

  auto peer1 = new FeatObject;
  auto peer2 = new FeatObject;
  checkOnChange("peer", peer1, peer2);

  auto v1 = new Variant(42);
  auto v2 = new Variant(43);
  checkOnChange("data", v1, v2);

  // Same boxed value, different D wrappers: Variant.equal, no notify
  auto vSameA = new Variant(7);
  auto vSameB = new Variant(7);
  obj.setProperty("data", vSameA);
  auto beforeVar = notifyCount;
  obj.setProperty("data", vSameB);
  assert(notifyCount == beforeVar, "variant same value notified");

  // notifyAlways: same value still notifies
  obj.setProperty("noisy", 1);
  auto beforeNoisy = notifyCount;
  obj.setProperty("noisy", 1);
  assert(notifyCount == beforeNoisy + 1 && lastNotify == "noisy", "notifyAlways same value");
  obj.setProperty("noisy", 2);
  assert(notifyCount == beforeNoisy + 2 && lastNotify == "noisy", "notifyAlways different value");

  // User ExplicitNotify: no automatic notify
  auto beforeQuiet = notifyCount;
  obj.setProperty("quiet", 4);
  obj.setProperty("quiet", 4);
  obj.setProperty("quiet", 5);
  assert(obj.quiet == 5 && notifyCount == beforeQuiet);
  obj.notify("quiet");
  assert(notifyCount == beforeQuiet + 1 && lastNotify == "quiet");

  // Write-only setter: the setter runs, but GObject does not emit notify for non-readable properties
  auto secretSpec = g_object_class_find_property(cast(GObjectClass*)g_type_class_peek(PropObj._getGType), "secret");
  assert(secretSpec && (secretSpec.flags & GParamFlags.Writable) && (secretSpec.flags & GParamFlags.Readable) == 0);
  auto beforeSecret = notifyCount;
  obj.setProperty("secret", 1);
  obj.setProperty("secret", 1);
  obj.setProperty("secret", 2);
  assert(obj._secret == 2 && notifyCount == beforeSecret, "write-only setter does not emit notify");

  destroy(obj);
  destroy(peer1);
  destroy(peer2);
  collectProps();
}

/// Parent properties (FeatObject value/name) still work on a D subclass with its own properties
unittest
{
  auto obj = new PropObj;
  obj.setValue(42);
  assert(obj.getValue == 42);
  obj.setProperty("value", 7);
  assert(obj.getValue == 7 && obj.getProperty!int("value") == 7);

  destroy(obj);
  collectProps();
}
