/**
 * Round trip test of the GIR export: uses the GObjectExport test libraries (libgidgen-export.so and
 * libgidgen-mixer.so) through the D bindings gidgen generated from the GIRs they wrote, like any C library. The D
 * sources of the libraries are not used.
 */
module app;

import core.memory : GC;
import std.stdio : writeln;

import gid.glib2.error : ErrorWrap;
import gid.gobject2.c.functions : g_signal_emit_by_name;
import gid.gobject2.c.types : GObject;
import gid.gobject2.initially_unowned : InitiallyUnowned;
import gid.gobject2.object : GObjectSubclass, ObjectWrap;
import gid.gobject2.param_spec : ParamSpec;
import gid.gidgenfeat1.object : FeatObject = ObjectWrap;
import gidgrt.gidgenexport1.auto_ : Auto;
import gidgrt.gidgenexport1.child : Child;
import gidgrt.gidgenexport1.floating : Floating;
import gidgrt.gidgenexport1.obj : Obj;
import gidgrt.gidgenexport1.shape : Shape;
import gidgrt.gidgenexport1.shape_mixin; // Interface mixins need their module imported as a whole
import gidgrt.gidgenexport1.virt : Virt;
import gidgrt.gidgenexport1.virt_child : VirtChild;
import gidgrt.gidgenmixer1.channel : Channel;
import gidgrt.gidgenmixer1.fancy_virt : FancyVirt;

extern(C) int gidgen_export_live_dobjects() nothrow;
extern(C) void gidgen_export_collect() nothrow;

/// D subclass of the exported class, overriding its virtual methods
class ClientVirt : Virt
{
  mixin GObjectSubclass;

  string joined;

  this()
  {
    super();
  }

  override protected int vfuncScale(int x)
  {
    return x * 100;
  }

  override protected void vfuncJoin(string a, string b)
  {
    joined = a ~ "+" ~ b;
  }
}

/// D implementation of the exported interface
class ClientShape : ObjectWrap, Shape
{
  mixin GObjectSubclass;
  mixin ShapeT;

  string text;

  this()
  {
    super();
  }

  override protected int vfuncSides()
  {
    return 3;
  }

  override protected void vfuncLabel(string text)
  {
    this.text = text;
  }
}

/// D subclass of a class of the second library, which is a subclass of the first library's class
class ClientFancy : FancyVirt
{
  mixin GObjectSubclass;

  int id;

  this(int id)
  {
    super();
    this.id = id;
  }

  override protected int vfuncScale(int x)
  {
    return super.vfuncScale(x) * 7; // Chains up to the implementation of the library (x * 3)
  }
}

void run()
{
  // Class derived from a C class of another namespace: properties, signals, D overrides of C vfuncs
  auto obj = new Obj;
  assert(cast(FeatObject)obj);
  assert(obj.count == 1 && obj.title == "hi" && obj.constructVal == 10);
  string[] notified;
  obj.connectNotify("count", (ParamSpec p) { notified ~= p.getName; });
  obj.count = 5;
  obj.count = 5; // Same value: no notify
  assert(obj.count == 5 && notified == ["count"]);
  int[] amounts;
  string[] whos;
  obj.connectBumped((int amount, string who) { amounts ~= amount; whos ~= who; });
  g_signal_emit_by_name(cast(GObject*)obj._cPtr, "bumped", 2, "d".ptr);
  assert(amounts == [2] && whos == ["d"] && obj.bumpTotal == 2); // The D class handler ran too
  g_signal_emit_by_name(cast(GObject*)obj._cPtr, "reset"); // Action signal
  assert(obj.bumpTotal == 0);
  assert(obj.compute(5) == 5 * 3 + 77); // D override of a C vfunc

  auto child = new Child;
  assert(cast(Obj)child && child.compute(5) == 5 * 3 + 77 + 1000);
  int[] events;
  child.connectChildEvent((int x) { events ~= x; });
  g_signal_emit_by_name(cast(GObject*)child._cPtr, "child-event", 7);
  assert(events == [7]);

  // Constructors, methods, functions, out parameters, GError
  auto v = Virt.newWithTitle("t");
  assert(v.title == "t");
  assert(v.describe(null) == "t" && v.describe("x") == "x t");
  assert(v.divide(7, 2) == 3);

  try
  {
    v.divide(1, 0);
    assert(0);
  }
  catch (ErrorWrap e)
    assert(e.code == 3 && e.msg == "division by zero");

  try
  {
    v.divide(1, -1);
    assert(0);
  }
  catch (ErrorWrap e)
    assert(e.code == 0 && e.msg == "negative divisor");

  int length;
  string upper;
  string[] chars;
  Virt same;
  v.stats("abc", length, upper, chars, same);
  assert(length == 3 && upper == "ABC" && chars == ["a", "b", "c"] && same is v);
  assert(Virt.abiVersion == 3);

  // Virtual methods: implementation of the library, its subclass, D subclass of the client
  assert(v.scale(5) == 10);
  v.join("a", "b");
  assert(v.title == "ab");
  assert(new VirtChild().scale(5) == 11);
  auto cv = new ClientVirt;
  assert(cv.scale(5) == 500); // The exported invoker calls the client override
  cv.join("x", "y");
  assert(cv.joined == "x+y");
  assert(cast(ClientVirt)Virt.newWithTitle("q") is null);

  // Interface: library implementation and client implementation
  assert(cast(Shape)v && v.sides == 4);
  v.label("z");
  assert(v.title == "label z");
  auto s = new ClientShape;
  assert(s.sides == 3);
  s.label("q");
  assert(s.text == "q");

  // GInitiallyUnowned (floating reference), auto-exported members
  auto f = new Floating;
  assert(cast(InitiallyUnowned)f);
  auto a = new Auto;
  a.level = 4;
  assert(a.add(1) == 5 && Auto.twice(4) == 8);

  // Second library (namespace GidgenMixer) using GidgenExport types
  auto ch = new Channel;
  auto k = ch.getGainKnob();
  assert(typeid(k) is typeid(Virt) && k.title == "gain" && ch.knob is k);
  string[] seen;
  ch.connectKnobChanged((Virt knob) { seen ~= knob.title; });
  auto k2 = Virt.newWithTitle("mine");
  ch.setGainKnob(k2);
  assert(seen == ["mine"] && ch.knob is k2 && ch.getGainKnob() is k2);
  assert(ch.sidesOf(k2) == 4 && ch.sidesOf(s) == 3);
  auto fancy = new FancyVirt;
  assert(cast(Virt)fancy && fancy.scale(5) == 15);
  auto cf = new ClientFancy(1);
  assert(cf.scale(2) == 42);
  ch.knob = cf;
  assert(ch.getGainKnob() is cf);

  // The client's D object of an instance the library keeps stays alive without client references
  ch.knob = new ClientFancy(2);
  GC.collect();
  auto kept = cast(ClientFancy)ch.getGainKnob();
  assert(kept && kept.id == 2 && kept.scale(1) == 21);
  assert(ch.sidesOf(kept) == 4);
}

int main()
{
  auto start = gidgen_export_live_dobjects();
  run();

  // All D objects of the libraries are finalized once the client drops its objects
  foreach (i; 0 .. 10)
  {
    GC.collect();
    gidgen_export_collect();

    if (gidgen_export_live_dobjects() == start)
      break;
  }

  assert(gidgen_export_live_dobjects() == start);
  writeln("round trip test passed");
  return 0;
}
