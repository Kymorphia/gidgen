module shim_test;

import Shim = gid.shimlib.widget;
import Shim1 = gid.shimlib1.widget;
import Shim2 = gid.shimlib2.widget;
import Shim1c = gid.shimlib1.c.functions;
import Shim2c = gid.shimlib2.c.functions;

unittest // The unversioned shim of the latest major is the versioned type, and an older major is distinct
{
  static assert(is(Shim.Widget == Shim2.Widget));
  static assert(!is(Shim.Widget == Shim1.Widget));
  static assert(!is(Shim1.Widget == Shim2.Widget));
}

unittest // Two majors can be linked together even when their C function names are the same
{
  assert(&Shim1c.c_shim_lib_widget_get_type !is &Shim2c.c_shim_lib_widget_get_type);
  alias t1 = Shim1c.shim_lib_widget_get_type;
  alias t2 = Shim2c.shim_lib_widget_get_type;
  assert(&t1 !is &t2);
}
