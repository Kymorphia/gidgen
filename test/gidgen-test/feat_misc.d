/// GidgenFeat enum and flags methods, error, destroy notify callback and property/method conflict tests
module feat_misc;

import glib.error : ErrorWrap;
import gidgenfeat.global;
import gidgenfeat.color : ColorNs = Color;
import gidgenfeat.style : StyleNs = Style;
import gidgenfeat.types : Color, Counter, Style;

/// Enum and flags methods (functions are in enum namespace structures since the types have a GType)
unittest
{
  static assert(is(ColorNs.Enum == Color));
  assert(ColorNs.next(Color.Red) == Color.Green);
  assert(ColorNs.next(Color.Green) == Color.Blue);
  assert(cast(int)Color.Blue == 10);
  assert(StyleNs.toggle(Style.Bold, Style.Bold) == Style.None);
  assert(StyleNs.toggle(Style.None, Style.Bold) == Style.Bold);
}

/// GError to D exception
unittest
{
  import std.exception : assertThrown, collectException;
  assert(mayFail(false));
  auto e = collectException!ErrorWrap(mayFail(true));
  assert(e !is null);
  assert(e.domain == errorQuark);
  assert(mayFailString(false).length > 0);
  assertThrown!ErrorWrap(mayFailString(true));
}

/// Notified callback closure: replacing or clearing it calls the destroy notify
unittest
{
  int factor = 3;
  auto start = liveCount(Counter.DestroyNotify);
  setNotified((int v) => v + factor);
  assert(liveCount(Counter.DestroyNotify) == start + 1);
  assert(callNotified(1) == 4);
  assert(callNotified(2) == 5);
  setNotified((int v) => v * 10); // Replacing should destroy the previous one
  assert(liveCount(Counter.DestroyNotify) == start + 1);
  assert(callNotified(2) == 20);
  clearNotified;
  assert(liveCount(Counter.DestroyNotify) == start);
  assert(callNotified(1) == -1);
}

/// Property and method name conflicts: a property with an identically named getter method is disabled (the method is
/// used), a property conflicting with a non-getter method is disabled, and a subclass property with the name of a
/// parent class method overrides it
unittest
{
  import gidgenfeat.conflict;
  import gidgenfeat.conflict_child;

  auto c = new Conflict;
  assert(c.level == 1, "getter method");
  assert(c.clash == 20, "method (the conflicting property is disabled)");
  assert(c.inherited == 30);

  auto child = new ConflictChild;
  assert(child.inherited == 3, "subclass property overriding the parent method");
  // NOTE: The property getter overrides the C method, so it is also called through a parent class reference
  Conflict asParent = child;
  assert(asParent.inherited == 3);
}
