/// GidgenFeat interface tests (interface prerequisites, interface signals)
module feat_iface;

import gidgenfeat.child_iface;
import gidgenfeat.child_iface_iface_proxy : ChildIfaceIfaceProxy;
import gidgenfeat.child_object;
import gidgenfeat.global : liveCount, newHiddenChild;
import gidgenfeat.iface;
import gidgenfeat.object : FeatObject = ObjectWrap;
import gidgenfeat.types : Counter;
import std.typecons : Yes;

/// Interface derives from its prerequisite interface (class prerequisites are skipped)
unittest
{
  static assert(is(ChildIface : Iface), "interface should derive from its prerequisite interface");
  static assert(is(ChildIfaceGidBuilderImpl!int : IfaceGidBuilderImpl!int), "interface builder should derive from the prerequisite builder");
}

/// Interface proxy object exposes the prerequisite interface methods
unittest
{
  auto start = liveCount(Counter.Object);
  auto child = newHiddenChild(3); // Private C type with no D binding, uses the interface proxy object
  assert(cast(ChildIfaceIfaceProxy)child !is null, "expected interface proxy object");
  assert(child.getLevel == 3);
  assert(child.getId == 300, "prerequisite method through the interface");

  Iface iface = child; // Implicit conversion to the prerequisite interface
  assert(iface.getId == 300);

  destroy(child);
  assert(liveCount(Counter.Object) == start, "hidden child leaked");
}

/// Class implementing an interface whose prerequisite is implemented by its parent class
unittest
{
  auto obj = new ChildObject(4);
  ChildIface child = obj;
  assert(child.getLevel == 40 && child.getId == 400);
  Iface iface = child;
  assert(iface.getId == 400);
  destroy(obj);
}

/// Interface signal connected through an interface typed reference (class, interface proxy and prerequisite interface)
unittest
{
  int lastId;
  Iface lastIface;

  auto obj = new FeatObject;
  Iface iface = obj;
  iface.connectIdChanged((int id, Iface self) { lastId = id; lastIface = self; });
  iface.emitIdChanged(5);
  assert(lastId == 5 && lastIface is iface, "signal connected via interface reference");

  obj.connectIdChanged((int id) { lastId = id * 2; }); // Class reference uses the mixin implementation
  obj.emitIdChanged(6);
  assert(lastId == 12);
  destroy(obj);

  auto start = liveCount(Counter.Object);
  auto child = newHiddenChild(7); // Interface proxy object
  child.connectIdChanged((int id) { lastId = id; }); // Prerequisite interface signal via the ChildIface reference
  child.emitIdChanged(8);
  assert(lastId == 8);

  Iface prereq = child;
  prereq.connectIdChanged((int id) { lastId = id + 1; }, Yes.After);
  prereq.emitIdChanged(9);
  assert(lastId == 10);
  lastIface = null;
  destroy(child);
  assert(liveCount(Counter.Object) == start, "hidden child leaked");
}
