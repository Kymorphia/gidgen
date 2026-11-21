module cli;

import std.file : exists, isDir, mkdir, rmdirRecurse, getcwd, chdir, tempDir, writeFile = write;
import std.path : buildPath;

import defs : DefaultGirPath;
import std_includes;

enum DefaultDefsPath = "defs"; /// Default directory of binding definition files
enum DefaultGirDir = "gir"; /// Default directory to search for GIR files
enum DefaultPkgPath = "."; /// Default top-level package output path
enum DefaultSubPkgPath = "packages"; /// Default subpackage output path

/**
 * Apply default CLI paths when the corresponding flags were omitted.
 *
 * `--defs` defaults to `defs/` if that directory exists.
 * `--gir-path` defaults to `gir/` if that directory exists, otherwise `DefaultGirPath`.
 * `--pkg-path` defaults to `.` and `--subpkg-path` to `packages/`.
 * Explicitly passed paths are left unchanged.
 *
 * Params:
 *   defPaths = Binding definition file or directory paths
 *   girPaths = GIR search paths
 *   pkgPath = Top-level package output path
 *   subPkgPath = Subpackage output path
 * Returns: An error message if a required path cannot be resolved, otherwise null
 */
string applyPathDefaults(ref string[] defPaths, ref string[] girPaths, ref string pkgPath, ref string subPkgPath)
{
  if (defPaths.empty)
  {
    if (DefaultDefsPath.exists && DefaultDefsPath.isDir)
      defPaths = [DefaultDefsPath];
    else
      return "No definition path found; specify --defs (-d) or run from a directory that contains a '"
        ~ DefaultDefsPath ~ "' folder";
  }

  if (girPaths.empty)
  {
    if (DefaultGirDir.exists && DefaultGirDir.isDir)
      girPaths = [DefaultGirDir];
    else if (DefaultGirPath.exists && DefaultGirPath.isDir)
      girPaths = [DefaultGirPath];
    else
      return "No GIR search path found; specify --gir-path (-g), add a '" ~ DefaultGirDir
        ~ "' directory, or install GIR files in " ~ DefaultGirPath;
  }

  if (!pkgPath)
    pkgPath = DefaultPkgPath;

  if (!subPkgPath)
    subPkgPath = DefaultSubPkgPath;

  return null;
}

version (unittest)
{
  // Run a test in an empty temporary directory and restore the previous cwd.
  void inTempDir(void delegate() dg)
  {
    auto oldCwd = getcwd;
    auto dir = buildPath(tempDir, "gidgen-cli-defaults-test");
    if (dir.exists)
      rmdirRecurse(dir);
    mkdir(dir);
    chdir(dir);
    scope (exit)
    {
      chdir(oldCwd);
      rmdirRecurse(dir);
    }
    dg();
  }
}

unittest // Missing defs directory is an error
{
  inTempDir({
    string[] defPaths, girPaths;
    string pkgPath, subPkgPath;
    auto err = applyPathDefaults(defPaths, girPaths, pkgPath, subPkgPath);
    assert(err !is null);
    assert(err.canFind("--defs"));
    assert(defPaths.empty);
  });
}

unittest // Default defs, gir, pkg, and subpkg paths when omitted
{
  inTempDir({
    mkdir(DefaultDefsPath);
    mkdir(DefaultGirDir);

    string[] defPaths, girPaths;
    string pkgPath, subPkgPath;
    assert(applyPathDefaults(defPaths, girPaths, pkgPath, subPkgPath) is null);
    assert(defPaths == [DefaultDefsPath]);
    assert(girPaths == [DefaultGirDir]);
    assert(pkgPath == DefaultPkgPath);
    assert(subPkgPath == DefaultSubPkgPath);
  });
}

unittest // Explicit flags are not replaced by defaults
{
  inTempDir({
    mkdir(DefaultDefsPath);
    mkdir(DefaultGirDir);

    auto defPaths = ["custom-defs"];
    auto girPaths = ["custom-gir"];
    auto pkgPath = "out";
    auto subPkgPath = "subs";
    assert(applyPathDefaults(defPaths, girPaths, pkgPath, subPkgPath) is null);
    assert(defPaths == ["custom-defs"]);
    assert(girPaths == ["custom-gir"]);
    assert(pkgPath == "out");
    assert(subPkgPath == "subs");
  });
}

unittest // gir/ missing falls back to DefaultGirPath when it exists
{
  inTempDir({
    mkdir(DefaultDefsPath);

    string[] defPaths, girPaths;
    string pkgPath, subPkgPath;
    auto err = applyPathDefaults(defPaths, girPaths, pkgPath, subPkgPath);

    if (DefaultGirPath.exists && DefaultGirPath.isDir)
    {
      assert(err is null);
      assert(girPaths == [DefaultGirPath]);
    }
    else
    {
      assert(err !is null);
      assert(err.canFind("--gir-path"));
      assert(girPaths.empty);
    }
  });
}

unittest // A defs file is not used as the default defs path
{
  inTempDir({
    writeFile(DefaultDefsPath, "not a directory");

    string[] defPaths, girPaths;
    string pkgPath, subPkgPath;
    auto err = applyPathDefaults(defPaths, girPaths, pkgPath, subPkgPath);
    assert(err !is null);
    assert(defPaths.empty);
  });
}
