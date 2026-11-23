/// GObject signal tests for @gSignal on GObjectSubclass classes
module feat_signal;

import core.memory : GC;
import gid.gidgenfeat1.object : FeatObject = ObjectWrap;
import gid.glib2.bytes : Bytes;
import gid.glib2.c.functions : g_main_context_iteration;
import gid.glib2.variant : Variant;
import gid.gobject2.c.functions;
import gid.gobject2.c.types;
import gid.core.utils : GidCallbackExceptionInfo, setCallbackExceptionHandler;
import gid.gobject2.object : GObjectSubclass, ObjectWrap, gSignalAccumulator, gSignal;
import gid.gobject2.types : GTypeEnum, SignalFlags;
import std.string : fromStringz, toStringz;
import std.typecons : No, Yes;

/// Test enum registered as a GType
enum SigColor
{
  Red = 0,
  Blue = 1,
}

/// Test flags registered as a GType
enum SigStyle : uint
{
  None = 0,
  Bold = 1,
  Italic = 2,
}

shared static this()
{
  static GEnumValue[3] colors = [
    GEnumValue(0, "sig-color-red", "red"),
    GEnumValue(1, "sig-color-blue", "blue"),
    GEnumValue(0, null, null),
  ];
  static GFlagsValue[4] styles = [
    GFlagsValue(0, "sig-style-none", "none"),
    GFlagsValue(1, "sig-style-bold", "bold"),
    GFlagsValue(2, "sig-style-italic", "italic"),
    GFlagsValue(0, null, null),
  ];
  g_enum_register_static("GidgenTestSigColor", colors.ptr);
  g_flags_register_static("GidgenTestSigStyle", styles.ptr);
}

/// Class with signals of the supported kinds
class SigObj : FeatObject
{
  mixin GObjectSubclass!"GidgenTestSigObj";

  string log;
  int lastCount;
  string lastReason;
  int defaultHandled;
  int actionTotal;

  @gSignal void ping()
  {
    log ~= "d";
  }

  @gSignal void updated(int count, string reason)
  {
    lastCount = count;
    lastReason = reason;
  }

  @gSignal void valueSet() {}

  @gSignal void risky()
  {
    throw new Exception("default handler");
  }

  @gSignal("custom-name") void renamed() {}

  @gSignal(SignalFlags.Detailed | SignalFlags.RunFirst) void tagged(string text)
  {
    log ~= "d";
  }

  @gSignal(SignalFlags.Action) void doIt(int x)
  {
    actionTotal += x;
  }

  @gSignal void allTypes(bool a, byte b, ubyte c, short d, ushort e, int f, uint g, long h, ulong i, float j,
    double k, string l) {}

  @(gSignal.create(typeNames: ["GidgenTestSigColor", "GidgenTestSigStyle"])) void enums(SigColor c, SigStyle s) {}

  @gSignal void objects(FeatObject o, Bytes b, Variant v) {}

  @gSignal bool handled(int what)
  {
    defaultHandled++;
    return false;
  }

  @gSignal(gSignalAccumulator.Last) int lastWins(int x)
  {
    return -1;
  }

  @gSignal(gSignalAccumulator.FirstWins) int firstWins(int x)
  {
    return 100;
  }

  @(gSignal.create(accumulator: "sum")) int summed(int x)
  {
    return 1;
  }

  static bool sum(ref int total, int ret)
  {
    total += ret;
    return true;
  }

  @gSignal string describe(string s)
  {
    return "default:" ~ s;
  }

  @gSignal FeatObject makeObj()
  {
    return null;
  }

  @(gSignal.create(typeNames: ["GidgenTestSigColor", "GidgenTestSigColor"])) SigColor pick(SigColor c)
  {
    return c;
  }
}

/// Subclass of a subclass: adds a signal, overrides a default handler
class SigChild : SigObj
{
  mixin GObjectSubclass!"GidgenTestSigChild";

  int childPings;

  override void ping()
  {
    super.ping();
    childPings++;
    log ~= "c";
  }

  @gSignal void childEvent(int x) {}
}

// Emit a signal by (detailed) name like g_signal_emit_by_name() (which is variadic and not bound), returns the
// value of a signal that returns R
private R emitByName(R = void, Args...)(GObject* obj, string name, Args args)
{
  import gid.gobject2.value : setVal, getVal;

  auto itype = (cast(GTypeInstance*)obj).gClass.gType;
  uint id;
  GQuark detail;
  assert(g_signal_parse_name(name.toStringz, itype, &id, &detail, true));

  GValue[Args.length + 1] vals;
  g_value_init(&vals[0], itype);
  g_value_set_object(&vals[0], obj);

  static foreach (i, A; Args)
  {
    static if (is(A == int))
      g_value_init(&vals[i + 1], GTypeEnum.Int);
    else static if (is(A == string))
      g_value_init(&vals[i + 1], GTypeEnum.String);
    else
      static assert(0);

    setVal!A(&vals[i + 1], args[i]);
  }

  scope(exit)
  {
    foreach (ref v; vals)
      g_value_unset(&v);
  }

  static if (is(R == void))
    g_signal_emitv(vals.ptr, id, detail, null);
  else
  {
    GValue ret;
    g_value_init(&ret, is(R == bool) ? GTypeEnum.Boolean : GTypeEnum.Int);
    g_signal_emitv(vals.ptr, id, detail, &ret);
    scope(exit) g_value_unset(&ret);
    return getVal!R(&ret);
  }
}

private int cCalls;
private int cInt;
private string cStr;

private extern(C) void onCUpdated(GObject* obj, int count, const(char)* reason, void* data) nothrow
{
  cCalls++;
  cInt = count;
  cStr = reason.fromStringz.idup;
}

private extern(C) int onCHandled(GObject* obj, int what, void* data) nothrow
{
  cCalls++;
  return what == 7;
}

private GSignalQuery query(GType gtype, string name)
{
  auto id = g_signal_lookup(name.toStringz, gtype);
  GSignalQuery q;
  if (id)
    g_signal_query(id, &q);
  return q;
}

/// Signal registration: names, flags and types
unittest
{
  auto obj = new SigObj;
  auto gt = SigObj._getGType;

  assert(g_signal_lookup("ping", gt) != 0);
  assert(g_signal_lookup("value-set", gt) != 0);
  assert(g_signal_lookup("valueSet", gt) == 0);
  assert(g_signal_lookup("custom-name", gt) != 0);
  assert(g_signal_lookup("renamed", gt) == 0);
  assert(g_signal_lookup("do-it", gt) != 0);
  assert(g_signal_lookup("all-types", gt) != 0);
  assert(g_signal_lookup("make-obj", gt) != 0);
  assert(g_signal_lookup("child-event", gt) == 0);
  g_type_class_ref(SigChild._getGType); // g_signal_lookup() does not initialize the class
  assert(g_signal_lookup("child-event", SigChild._getGType) != 0);
  assert(g_signal_lookup("ping", SigChild._getGType) == g_signal_lookup("ping", gt));

  auto q = query(gt, "ping");
  assert(q.signalFlags == SignalFlags.RunLast && q.returnType == GTypeEnum.None && q.nParams == 0 && q.itype == gt);

  q = query(gt, "tagged");
  assert(q.signalFlags == (SignalFlags.RunFirst | SignalFlags.Detailed));
  assert(q.nParams == 1 && q.paramTypes[0] == GTypeEnum.String);

  q = query(gt, "do-it");
  assert(q.signalFlags == (SignalFlags.Action | SignalFlags.RunLast));

  q = query(gt, "all-types");
  assert(q.nParams == 12);
  GType[12] expected = [GTypeEnum.Boolean, GTypeEnum.Char, GTypeEnum.Uchar, GTypeEnum.Int, GTypeEnum.Uint,
    GTypeEnum.Int, GTypeEnum.Uint, GTypeEnum.Int64, GTypeEnum.Uint64, GTypeEnum.Float, GTypeEnum.Double,
    GTypeEnum.String];
  foreach (i; 0 .. 12)
    assert(q.paramTypes[i] == expected[i]);

  q = query(gt, "enums");
  assert(q.paramTypes[0] == g_type_from_name("GidgenTestSigColor"));
  assert(q.paramTypes[1] == g_type_from_name("GidgenTestSigStyle"));

  q = query(gt, "objects");
  assert(q.paramTypes[0] == FeatObject._getGType && q.paramTypes[1] == Bytes._getGType
    && q.paramTypes[2] == GTypeEnum.Variant);

  q = query(gt, "handled");
  assert(q.returnType == GTypeEnum.Boolean);

  q = query(gt, "describe");
  assert(q.returnType == GTypeEnum.String);
}

/// Emit and connect from D, default handlers, After, and instance parameter
unittest
{
  auto obj = new SigObj;

  int count;
  string reason;
  SigObj inst;
  obj.connectUpdated((int c, string r, SigObj self) { count = c; reason = r; inst = self; });
  obj.emitUpdated(5, "because");
  assert(count == 5 && reason == "because" && inst is obj);
  assert(obj.lastCount == 5 && obj.lastReason == "because"); // Default handler

  int plain;
  obj.connectUpdated((int c, string r) { plain += c; });
  obj.emitUpdated(2, "x");
  assert(plain == 2 && count == 2);

  obj.connectUpdated((int c) { plain += 100; }); // Trailing parameters are optional
  obj.emitUpdated(1, "y");
  assert(plain == 2 + 1 + 100);
}

/// After connections, the order of the handlers and the default handler
unittest
{
  auto obj = new SigObj;

  obj.connectPing({ obj.log ~= "1"; });
  obj.connectPing({ obj.log ~= "a"; }, Yes.After);
  obj.connectPing((SigObj self) { self.log ~= "2"; });
  obj.emitPing;
  assert(obj.log == "12da");

  // The signal is also a plain method for the default handler
  obj.log = null;
  obj.ping;
  assert(obj.log == "d");
}

/// C style connects and emits by name
unittest
{
  auto obj = new SigObj;
  auto cobj = cast(GObject*)obj._cPtr;

  g_signal_connect_data(cobj, "updated".toStringz, cast(GCallback)&onCUpdated, null, null, GConnectFlags.Default);
  cCalls = 0;
  emitByName(cobj, "updated", 12, "from C");
  assert(cCalls == 1 && cInt == 12 && cStr == "from C");
  assert(obj.lastCount == 12 && obj.lastReason == "from C"); // D default handler

  int dCount;
  obj.connectUpdated((int c) { dCount = c; });
  obj.emitUpdated(3, "from D");
  assert(cCalls == 2 && cInt == 3 && cStr == "from D" && dCount == 3);

  // Return values from C
  g_signal_connect_data(cobj, "handled".toStringz, cast(GCallback)&onCHandled, null, null, GConnectFlags.Default);
  cCalls = 0;
  assert(emitByName!bool(cobj, "handled", 7) == true && cCalls == 1 && obj.defaultHandled == 0); // True handler stops
  assert(emitByName!bool(cobj, "handled", 1) == false && cCalls == 2 && obj.defaultHandled == 1);

  // Action signal by name
  emitByName(cobj, "do-it", 4);
  emitByName(cobj, "do-it", 5);
  assert(obj.actionTotal == 9);
}

/// Detailed signal, RunFirst default handler
unittest
{
  auto obj = new SigObj;
  string seen;

  obj.connectTagged("a", (string t) { seen ~= "a:" ~ t ~ " "; });
  obj.connectTagged((string t) { seen ~= "any:" ~ t ~ " "; }); // Detail is optional
  obj.connectTagged("b", (string t, SigObj self) { seen ~= "b:" ~ t ~ self.log ~ " "; });

  obj.emitTagged("a", "x");
  assert(seen == "a:x any:x " && obj.log == "d");
  obj.log = null;
  seen = null;
  obj.emitTagged("b", "y");
  assert(seen == "any:y b:yd ", seen); // RunFirst: the default handler ran before the handlers
  seen = null;
  obj.emitTagged("z");
  assert(seen == "any:z ");

  // From C with signal::detail
  auto cobj = cast(GObject*)obj._cPtr;
  seen = null;
  emitByName(cobj, "tagged::a", "fromc");
  assert(seen == "a:fromc any:fromc ");
}

/// Argument types
unittest
{
  auto obj = new SigObj;
  bool called;

  obj.connectAllTypes((bool a, byte b, ubyte c, short d, ushort e, int f, uint g, long h, ulong i, float j, double k,
    string l) {
    called = a && b == -1 && c == 2 && d == -3 && e == 4 && f == -5 && g == 6 && h == -7_000_000_000L
      && i == 8_000_000_000UL && j == 1.5f && k == 2.25 && l == "str";
  });
  obj.emitAllTypes(true, -1, 2, -3, 4, -5, 6, -7_000_000_000L, 8_000_000_000UL, 1.5f, 2.25, "str");
  assert(called);

  SigColor color;
  SigStyle style;
  obj.connectEnums((SigColor c, SigStyle s) { color = c; style = s; });
  obj.emitEnums(SigColor.Blue, SigStyle.Bold | SigStyle.Italic);
  assert(color == SigColor.Blue && style == (SigStyle.Bold | SigStyle.Italic));

  auto peer = new FeatObject;
  auto bytes = new Bytes(cast(ubyte[])[1, 2, 3]);
  auto variant = new Variant(42);
  FeatObject gotPeer;
  Bytes gotBytes;
  Variant gotVariant;
  obj.connectObjects((FeatObject o, Bytes b, Variant v) { gotPeer = o; gotBytes = b; gotVariant = v; });
  obj.emitObjects(peer, bytes, variant);
  assert(gotPeer is peer && gotBytes !is null && gotBytes.getData == cast(ubyte[])[1, 2, 3]);
  assert(gotVariant !is null && gotVariant.getInt32 == 42);

  obj.emitObjects(null, null, null);
  assert(gotPeer is null && gotBytes is null && gotVariant is null);
}

/// Return values and accumulators
unittest
{
  auto obj = new SigObj;

  assert(obj.emitHandled(1) == false && obj.defaultHandled == 1);
  obj.connectHandled((int what) => what == 2);
  assert(obj.emitHandled(2) == true && obj.defaultHandled == 1);
  assert(obj.emitHandled(3) == false && obj.defaultHandled == 2);

  assert(obj.emitLastWins(0) == -1); // Default handler last
  obj.connectLastWins((int x) => x + 10, Yes.After);
  assert(obj.emitLastWins(1) == 11);

  assert(obj.emitFirstWins(0) == 100); // Default handler only
  obj.connectFirstWins((int x) => x + 1);
  obj.connectFirstWins((int x) => x + 2);
  assert(obj.emitFirstWins(5) == 6);

  assert(obj.emitSummed(0) == 1); // The default handler
  obj.connectSummed((int x) => x);
  obj.connectSummed((int x) => x * 2);
  assert(obj.emitSummed(10) == 10 + 20 + 1);

  assert(obj.emitDescribe("a") == "default:a");
  obj.connectDescribe((string s) => "handler:" ~ s, Yes.After);
  assert(obj.emitDescribe("b") == "handler:b"); // Last handler wins: the After handler runs after the default
  obj.connectDescribe((string s) => "x" ~ s);
  assert(obj.emitDescribe("c") == "handler:c");

  assert(obj.emitMakeObj is null);
  auto peer = new FeatObject;
  obj.connectMakeObj(() => peer, Yes.After);
  assert(obj.emitMakeObj is peer);

  assert(obj.emitPick(SigColor.Blue) == SigColor.Blue);
  obj.connectPick((SigColor c) => SigColor.Red);

  // Class handler is the last one, so the default (identity) wins
  assert(obj.emitPick(SigColor.Blue) == SigColor.Blue);
  obj.connectPick((SigColor c) => SigColor.Red, Yes.After);
  assert(obj.emitPick(SigColor.Blue) == SigColor.Red);
}

/// Signal on a subclass of a subclass, overridden default handler
unittest
{
  auto obj = new SigChild;
  int got;

  obj.connectChildEvent((int x) { got = x; });
  obj.emitChildEvent(8);
  assert(got == 8);

  string reason;
  obj.connectUpdated((int c, string r) { reason = r; }); // Inherited signal
  obj.emitUpdated(1, "inherited");
  assert(reason == "inherited" && obj.lastCount == 1);

  obj.emitPing; // The override runs as the default handler
  assert(obj.log == "dc" && obj.childPings == 1);

  // Through C on the child type
  auto cobj = cast(GObject*)obj._cPtr;
  emitByName(cobj, "ping");
  emitByName(cobj, "child-event", 9);
  assert(obj.childPings == 2 && got == 9);

  auto parent = new SigObj;
  parent.emitPing;
  assert(parent.log == "d");

  // Emitting through a base class reference of a child instance
  SigObj base = obj;
  obj.log = null;
  base.emitPing;
  assert(obj.log == "dc");
}

private __gshared int exceptionCount;

private void countException(Exception e, GidCallbackExceptionInfo info) nothrow
{
  exceptionCount++;
}

/// Exceptions in handlers and in default handlers don't escape from the emission
unittest
{
  auto obj = new SigObj;
  setCallbackExceptionHandler(&countException);
  scope(exit) setCallbackExceptionHandler(null);
  exceptionCount = 0;

  obj.connectPing({ throw new Exception("boom"); });
  obj.emitPing; // Reported to the exception handler, emission continues
  assert(obj.log == "d" && exceptionCount == 1);

  obj.emitRisky;
  assert(exceptionCount == 2);
}

/// Callbacks with a wrong signature are rejected at compile time
unittest
{
  auto obj = new SigObj;

  static assert(__traits(compiles, obj.connectUpdated((int c, string r, SigObj self) {})));
  static assert(__traits(compiles, obj.connectUpdated((int c) {})));
  static assert(__traits(compiles, obj.connectUpdated({})));
  static assert(!__traits(compiles, obj.connectUpdated((string c) {})));
  static assert(!__traits(compiles, obj.connectUpdated((int c, string r, SigObj self, int extra) {})));
  static assert(!__traits(compiles, obj.connectUpdated((int c, string r) => 1)));
  static assert(!__traits(compiles, obj.connectHandled((int w) {})));
  static assert(!__traits(compiles, obj.connectUpdated(42)));
  static assert(__traits(compiles, obj.connectTagged("a", (string t) {})));
  static assert(!__traits(compiles, obj.connectUpdated("a", (int c, string r) {})));
  static assert(!__traits(compiles, obj.emitUpdated(1)));
}
