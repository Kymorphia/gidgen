/// GidgenFeat interface tests (interface prerequisites)
module feat_iface;

import gidgenfeat.child_iface;
import gidgenfeat.child_iface_iface_proxy : ChildIfaceIfaceProxy;
import gidgenfeat.child_object;
import gidgenfeat.global : liveCount, newHiddenChild;
import gidgenfeat.iface;
import gidgenfeat.types : Counter;

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
