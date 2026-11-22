/**
 * Checks the GID_RTLD_LOCAL and GID_RTLD_GLOBAL per-package overrides. The test Makefile runs this with those
 * variables set, after the libraries have been opened in this process's static constructors.
 *
 * GID_RTLD_LOCAL is tested on gidgenfeat1, a single-major library that is not also opened as a dependency of
 * another GLOBAL package (unlike glib, whose symbols stay in the global scope if gobject is opened with RTLD_GLOBAL).
 */
module loader_override_app;

version (GidLoaderOverrideApp):

import core.sys.posix.dlfcn : dlsym;
import std.stdio : stderr, writeln;

import gid.core : isLibAvailable;
import gid.core.loader : gidPackageLibs, GidRtldMode;
static import gid.gidgenfeat1.c.functions;
static import gid.glib2.c.functions;
static import gid.shimlib1.widget;
static import gid.shimlib2.widget;

int main()
{
  if (!isLibAvailable!(gid.glib2.c.functions) || !isLibAvailable!(gid.gidgenfeat1.c.functions)
      || !isLibAvailable!(gid.shimlib1.widget) || !isLibAvailable!(gid.shimlib2.widget))
  {
    stderr.writeln("isLibAvailable failed under GID_RTLD overrides");
    return 1;
  }

  if (gidPackageLibs["gid.glib2"].rtldMode != GidRtldMode.global)
  {
    stderr.writeln("glib2 should stay RTLD_GLOBAL when it is not listed in an override");
    return 1;
  }

  if (gidPackageLibs["gid.gidgenfeat1"].rtldMode != GidRtldMode.local)
  {
    stderr.writeln("GID_RTLD_LOCAL=gid.gidgenfeat1 did not open gidgenfeat1 with RTLD_LOCAL");
    return 1;
  }

  if (gidPackageLibs["gid.shimlib1"].rtldMode != GidRtldMode.global
      || gidPackageLibs["gid.shimlib2"].rtldMode != GidRtldMode.global)
  {
    stderr.writeln("GID_RTLD_GLOBAL did not open shimlib1/2 with RTLD_GLOBAL");
    return 1;
  }

  version (linux)
  {
    if (dlsym(null, "gidgen_feat_color_next") !is null)
    {
      stderr.writeln("gidgen_feat_color_next is still in the global scope after GID_RTLD_LOCAL=gid.gidgenfeat1");
      return 1;
    }

    if (dlsym(null, "shim_lib_widget_new") is null)
    {
      stderr.writeln("shim_lib_widget_new is not in the global scope after GID_RTLD_GLOBAL");
      return 1;
    }

    // glib was not overridden, so its symbols stay global
    if (dlsym(null, "g_strdup") is null)
    {
      stderr.writeln("g_strdup should remain in the global scope");
      return 1;
    }
  }

  return 0;
}
