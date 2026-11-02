/// GidgenFeat non-copyable opaque record tests
module feat_no_copy;

import gidgenfeat.global : liveCount;
import gidgenfeat.no_copy;
import gidgenfeat.types : Counter;

/// Non-copyable opaque record (free function only)
unittest
{
  static assert(!NoCopy._copyable);
  static assert(!__traits(hasMember, NoCopy, "take"), "transfer full instance of non-copyable type is unsupported");

  auto start = liveCount(Counter.NoCopy);
  NoCopy n;
  NoCopy.create(3, n);
  assert(liveCount(Counter.NoCopy) == start + 1);
  assert(n.getValue == 3);
  n.setValue(4);
  assert(n.getValue == 4);
  destroy(n);
  assert(liveCount(Counter.NoCopy) == start, "owned non-copyable not freed by destroy");

  // Transfer none return of a non-copyable type is borrowed (not freed by the wrapper)
  auto none = NoCopy.returnNone;
  assert(liveCount(Counter.NoCopy) == start + 1);
  assert(none.getValue == 13);
  destroy(none);
  assert(liveCount(Counter.NoCopy) == start + 1, "borrowed non-copyable must not be freed by the wrapper");
  NoCopy.resetSingleton;
  assert(liveCount(Counter.NoCopy) == start);
}
