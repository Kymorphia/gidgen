module shim_test;

import Shim = gid.shimlib.widget;
import Shim1 = gid.shimlib1.widget;
import Shim2 = gid.shimlib2.widget;

unittest // The unversioned shim of the latest major is the versioned type, and an older major is distinct
{
  static assert(is(Shim.Widget == Shim2.Widget));
  static assert(!is(Shim.Widget == Shim1.Widget));
  static assert(!is(Shim1.Widget == Shim2.Widget));
}
