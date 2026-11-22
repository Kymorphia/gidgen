/**
 * Tests of loading libraries of several versions of a library, the loader and GType to D class mapping with
 * ShimLib 1 and 2 (libshimlib-1.so.0 and libshimlib-2.so.0, which have the same C symbols and GType names, as GTK 3 and
 * GTK 4 do), and ShimLib 3 which has no library file.
 *
 * The GType name of a class can be registered by only one of the libraries, so these tests call the get_type() function
 * of ShimLibWidget in library 1 only, and of ShimLibGadget in library 2 only.
 */
module loader_test;

import core.sys.posix.dlfcn : dlsym;
import std.algorithm : canFind;

import gid.core : Flag, isLibAvailable, No, Yes;
import gid.core.loader : gidHasOtherMajor, gidLibHandleOfAddress, gidPackageLibs, GidRtldMode, gidSymbolNotFound, gidUnresolvedLibs;
import gid.gobject2.object : ObjectWrap;
import gid.gobject2.c.functions : g_type_from_name;
import Gadget1 = gid.shimlib1.gadget;
import Gadget2 = gid.shimlib2.gadget;
import Widget1 = gid.shimlib1.widget;
import Widget2 = gid.shimlib2.widget;
import Widget3 = gid.shimlib3.widget;
import Shim1c = gid.shimlib1.c.functions;
import Shim2c = gid.shimlib2.c.functions;
import Shim3c = gid.shimlib3.c.functions;
static import gid.glib2.c.functions;
static import gid.shimlib1.widget;
static import gid.shimlib2.widget;
static import gid.shimlib3.gadget;
static import gid.shimlib3.widget;

unittest // isLibAvailable reports whether the libraries of a package were loaded
{
  assert(isLibAvailable!(gid.shimlib1.widget));
  assert(isLibAvailable!(Widget2.Widget));
  assert(isLibAvailable!Shim2c); // A module alias
  assert(isLibAvailable!(gid.glib2.c.functions));
  assert(isLibAvailable("gid.shimlib1"));
  assert(isLibAvailable("gid.gobject2"));

  assert(!isLibAvailable!(Widget3.Widget)); // Package is linked but its library file doesn't exist
  assert(!isLibAvailable!(gid.shimlib3.gadget));
  assert(!isLibAvailable("gid.shimlib3"));
  assert(gidUnresolvedLibs.canFind("shimlib-3_0"));

  assert(!isLibAvailable("gid.notlinked")); // Unknown package
  assert(!isLibAvailable(""));
}

unittest // The check can choose between versions at runtime
{
  int major;

  if (isLibAvailable!(gid.shimlib3.widget))
    major = 3;
  else if (isLibAvailable!(gid.shimlib2.widget))
    major = 2;
  else if (isLibAvailable!(gid.shimlib1.widget))
    major = 1;

  assert(major == 2);
}

unittest // A symbol of a library which wasn't loaded is gidSymbolNotFound, not null, one that was loaded is not
{
  alias NotFound = void function();
  assert(cast(NotFound)Shim3c.shim_lib_widget_get_type is &gidSymbolNotFound);
  assert(cast(NotFound)Shim3c.shim_lib_widget_get_type !is null);
  assert(cast(NotFound)Shim1c.shim_lib_widget_get_type !is &gidSymbolNotFound);
  assert(cast(NotFound)Shim2c.shim_lib_widget_get_type !is &gidSymbolNotFound);
  assert(Shim3c.shim_lib_widget_get_type !is Shim1c.shim_lib_widget_get_type);
}

unittest // Two majors of a library are opened with RTLD_LOCAL; a single major uses RTLD_GLOBAL
{
  assert(gidHasOtherMajor("gid.shimlib1", "ShimLib"));
  assert(gidHasOtherMajor("gid.shimlib2", "ShimLib"));
  assert(!gidHasOtherMajor("gid.glib2", "GLib"));
  assert(!gidHasOtherMajor("gid.gobject2", "GObject"));
  assert(gidPackageLibs["gid.shimlib1"].rtldMode == GidRtldMode.local);
  assert(gidPackageLibs["gid.shimlib2"].rtldMode == GidRtldMode.local);
  assert(gidPackageLibs["gid.glib2"].rtldMode == GidRtldMode.global);
  assert(gidPackageLibs["gid.gobject2"].rtldMode == GidRtldMode.global);
  assert(gidPackageLibs["gid.gidgenfeat1"].rtldMode == GidRtldMode.global);

  version (linux)
  {
    // The two ShimLib majors keep shim_lib_widget_new out of the global scope; each handle has its own function
    assert(dlsym(null, "shim_lib_widget_new") is null); // RTLD_DEFAULT is null
    auto libs1 = gidPackageLibs["gid.shimlib1"].handles;
    auto libs2 = gidPackageLibs["gid.shimlib2"].handles;
    assert(libs1.length == 1 && libs2.length == 1 && libs1[0] !is libs2[0]);
    assert(dlsym(libs1[0], "shim_lib_widget_new") !is null);
    assert(dlsym(libs2[0], "shim_lib_widget_new") !is null);
    assert(dlsym(libs1[0], "shim_lib_widget_new") !is dlsym(libs2[0], "shim_lib_widget_new"));
    assert(gidLibHandleOfAddress(dlsym(libs1[0], "shim_lib_widget_new")) is libs1[0]);
    assert(gidLibHandleOfAddress(dlsym(libs2[0], "shim_lib_widget_new")) is libs2[0]);

    // glib and gidgenfeat are the only majors of their libraries, so they were opened with RTLD_GLOBAL
    assert(dlsym(null, "g_strdup") !is null);
    assert(dlsym(null, "gidgen_feat_color_next") !is null);
  }
}

unittest // Each C library works and creates objects of its own version
{
  auto w1 = Shim1c.shim_lib_widget_new();
  auto g2 = Shim2c.shim_lib_gadget_new();
  assert(Shim1c.shim_lib_widget_get_major(w1) == 1);
  assert(Shim2c.shim_lib_gadget_get_major(g2) == 2);
}

unittest // The D class of a GType name used by several versions is the one of the library which registered the GType
{
  // The Widget GType is registered by library 1 and the Gadget GType by library 2, whichever order the D classes are
  // listed in, a map of GType names to the last (or first) class would be wrong for one of them
  auto cw = Shim1c.shim_lib_widget_new();
  assert(g_type_from_name("ShimLibWidget") != 0);
  auto cg = Shim2c.shim_lib_gadget_new();
  assert(g_type_from_name("ShimLibGadget") != 0);

  auto w = ObjectWrap._getDObject!ObjectWrap(cw, Yes.Take);
  assert(cast(Widget1.Widget)w !is null);
  assert(cast(Widget2.Widget)w is null);
  assert(cast(Widget3.Widget)w is null);
  assert(typeid(w) is typeid(Widget1.Widget));

  auto g = ObjectWrap._getDObject!ObjectWrap(cg, Yes.Take);
  assert(cast(Gadget2.Gadget)g !is null);
  assert(cast(Gadget1.Gadget)g is null);
  assert(typeid(g) is typeid(Gadget2.Gadget));

  assert((cast(Widget1.Widget)w).getMajor == 1);
  assert((cast(Gadget2.Gadget)g).getMajor == 2);
}

unittest // The requested type selects the version, and an existing wrapper is reused
{
  auto cw = Shim1c.shim_lib_widget_new();
  auto w = ObjectWrap._getDObject!(Widget1.Widget)(cw, Yes.Take);
  assert(w !is null && typeid(w) is typeid(Widget1.Widget));
  assert(w.getMajor == 1);
  assert(ObjectWrap._getDObject!(Widget1.Widget)(cw, No.Take) is w);

  auto cg = Shim2c.shim_lib_gadget_new();
  auto g = ObjectWrap._getDObject!(Gadget2.Gadget)(cg, Yes.Take);
  assert(g !is null && typeid(g) is typeid(Gadget2.Gadget));
  assert(g.getMajor == 2);
}

unittest // Objects created with the D classes get the right class when wrapped again
{
  auto w = new Widget1.Widget;
  auto g = new Gadget2.Gadget;
  assert(w.getMajor == 1 && g.getMajor == 2);
  assert(ObjectWrap._getDObject!ObjectWrap(w._cPtr, No.Take) is w);
  assert(ObjectWrap._getDObject!ObjectWrap(g._cPtr, No.Take) is g);
}
