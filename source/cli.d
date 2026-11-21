module cli;

import std.conv : ConvException, to;
import std.experimental.logger : LogLevel;
import std.file : exists, isDir, mkdir, rmdirRecurse, getcwd, chdir, tempDir, writeFile = write, readText;
import std.json;
import std.path : absolutePath, asNormalizedPath, buildPath, dirName;

import defs : DefaultGirPath;
import std_includes;

enum DefaultDefsPath = "defs"; /// Default directory of binding definition files
enum DefaultGirDir = "gir"; /// Default directory to search for GIR files
enum DefaultPkgPath = "."; /// Default top-level package output path
enum DefaultSubPkgPath = "packages"; /// Default subpackage output path
enum DefaultConfigName = "gidgen.json"; /// Default config file name looked up in the current directory

/// Known gidgen.json keys
enum string[] KnownConfigKeys = [
  "defs", "gir-path", "pkg-path", "subpkg-path", "report", "report-file", "report-options",
  "log-level", "log-gir-locs", "log-code-locs", "suggest", "dependencies",
];

/// An external dependency repository from the `dependencies` config key
struct DependencyConfig
{
  string name; /// Dub umbrella package name, empty to use the global //!info name of the dependency defs
  string version_ = "*"; /// Dub version constraint for dependencies on its packages
  string[] defPaths; /// Definition file or directory paths of the dependency
  string[] girPaths; /// GIR search paths of the dependency
}

/// Options loaded from a gidgen.json file. Path values are stored as absolute, normalized paths.
struct GidgenConfig
{
  string[] defPaths; /// Binding definition file or directory paths
  bool hasDefPaths; /// True if `defs` was set
  string[] girPaths; /// GIR search paths
  bool hasGirPaths; /// True if `gir-path` was set
  string pkgPath; /// Top-level package output path
  string subPkgPath; /// Subpackage output path
  bool report; /// Enable the coverage report
  bool hasReport; /// True if `report` was set
  string reportFile; /// Coverage report output file
  string reportOptions; /// Coverage report options string
  LogLevel logLevel; /// Log level
  bool hasLogLevel; /// True if `log-level` was set
  bool logGirLocs; /// Log GIR file locations in warnings
  bool hasLogGirLocs; /// True if `log-gir-locs` was set
  bool logCodeLocs; /// Log code locations in warnings
  bool hasLogCodeLocs; /// True if `log-code-locs` was set
  bool suggest; /// Output definition file command suggestions
  bool hasSuggest; /// True if `suggest` was set
  DependencyConfig[] dependencies; /// External dependency repositories
}

/// Flags that were explicitly present on the command line (used for config precedence).
struct CliFlags
{
  bool defs;
  bool girPath;
  bool pkgPath;
  bool subPkgPath;
  bool report;
  bool reportFile;
  bool reportOptions;
  bool logLevel;
  bool logGirLocs;
  bool logCodeLocs;
  bool suggest;
  bool config;
}

/**
 * Apply default CLI paths when the corresponding flags (and config values) were omitted.
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

/**
 * Resolve which config file to load, if any.
 * Params:
 *   configOption = The `--config` value, or empty if the flag was omitted
 *   cwd = Directory to search for `gidgen.json` when `--config` is omitted
 *   path = Set to the absolute config file path when one should be loaded, otherwise left empty
 * Returns: An error message, or null if a file was found or no config should be loaded
 */
string resolveConfigPath(string configOption, string cwd, out string path)
{
  if (configOption.length)
  {
    auto candidate = configOption;
    if (!candidate.exists)
      return "Config file '" ~ configOption ~ "' not found";
    if (candidate.isDir)
      return "Config file '" ~ configOption ~ "' is a directory";
    path = absolutePath(candidate).asNormalizedPath.array.idup;
    return null;
  }

  auto candidate = buildPath(cwd, DefaultConfigName);
  if (!candidate.exists)
    return null;
  if (candidate.isDir)
    return "Config file '" ~ candidate ~ "' is a directory";
  path = absolutePath(candidate).asNormalizedPath.array.idup;
  return null;
}

/// ditto
string resolveConfigPath(string configOption, out string path)
{
  return resolveConfigPath(configOption, getcwd, path);
}

/**
 * Load a gidgen JSON config file.
 * Relative path values are resolved against the config file's directory.
 * Params:
 *   filename = Path to the config file
 *   cfg = Filled with the parsed values
 * Returns: An error message, or null on success
 */
string loadGidgenConfig(string filename, out GidgenConfig cfg)
{
  JSONValue root;

  try
    root = parseJSON(readText(filename));
  catch (JSONException e)
    return "Failed to parse '" ~ filename ~ "': " ~ e.msg;
  catch (Exception e)
    return "Failed to read '" ~ filename ~ "': " ~ e.msg;

  if (root.type != JSONType.object)
    return "Config file '" ~ filename ~ "' must contain a JSON object";

  string configDir = dirName(absolutePath(filename).asNormalizedPath.array).idup;

  foreach (string key, val; root.object)
  {
    if (!KnownConfigKeys.canFind(key))
      return "Unknown key '" ~ key ~ "' in '" ~ filename ~ "'";

    if (auto err = loadConfigKey(filename, configDir, key, val, cfg))
      return err;
  }

  return null;
}

/**
 * Apply loaded config values to CLI option variables, without replacing values set on the command line.
 * Params:
 *   cfg = Parsed config file
 *   fromCli = Flags that were present on the command line
 *   defPaths = Binding definition file or directory paths
 *   girPaths = GIR search paths
 *   pkgPath = Top-level package output path
 *   subPkgPath = Subpackage output path
 *   enableReport = Whether to write a coverage report
 *   reportFile = Coverage report output file
 *   reportOptions = Coverage report options string
 *   logLevel = Log level
 *   logGirLocs = Log GIR file locations in warnings
 *   logCodeLocs = Log code locations in warnings
 *   suggest = Output definition file command suggestions
 */
void applyGidgenConfig(ref const GidgenConfig cfg, const CliFlags fromCli, ref string[] defPaths,
    ref string[] girPaths, ref string pkgPath, ref string subPkgPath, ref bool enableReport, ref string reportFile,
    ref string reportOptions, ref LogLevel logLevel, ref bool logGirLocs, ref bool logCodeLocs, ref bool suggest)
{
  if (!fromCli.defs && cfg.hasDefPaths)
    defPaths = cfg.defPaths.dup;
  if (!fromCli.girPath && cfg.hasGirPaths)
    girPaths = cfg.girPaths.dup;
  if (!fromCli.pkgPath && cfg.pkgPath)
    pkgPath = cfg.pkgPath;
  if (!fromCli.subPkgPath && cfg.subPkgPath)
    subPkgPath = cfg.subPkgPath;
  if (!fromCli.report && cfg.hasReport)
    enableReport = cfg.report;
  if (!fromCli.reportFile && cfg.reportFile)
    reportFile = cfg.reportFile;
  if (!fromCli.reportOptions && cfg.reportOptions)
    reportOptions = cfg.reportOptions;
  if (!fromCli.logLevel && cfg.hasLogLevel)
    logLevel = cfg.logLevel;
  if (!fromCli.logGirLocs && cfg.hasLogGirLocs)
    logGirLocs = cfg.logGirLocs;
  if (!fromCli.logCodeLocs && cfg.hasLogCodeLocs)
    logCodeLocs = cfg.logCodeLocs;
  if (!fromCli.suggest && cfg.hasSuggest)
    suggest = cfg.suggest;
}

/**
 * Detect which supported options were given on the command line.
 * Params:
 *   args = The original argument array, including the program name
 * Returns: Flags that were present
 */
CliFlags cliFlagsFromArgs(const string[] args)
{
  CliFlags flags;
  flags.defs = cliOptionGiven(args, "defs", "d");
  flags.girPath = cliOptionGiven(args, "gir-path", "g");
  flags.pkgPath = cliOptionGiven(args, "pkg-path", "p");
  flags.subPkgPath = cliOptionGiven(args, "subpkg-path", "s");
  flags.report = cliOptionGiven(args, "report");
  flags.reportFile = cliOptionGiven(args, "report-file");
  flags.reportOptions = cliOptionGiven(args, "report-options");
  flags.logLevel = cliOptionGiven(args, "log-level");
  flags.logGirLocs = cliOptionGiven(args, "log-gir-locs");
  flags.logCodeLocs = cliOptionGiven(args, "log-code-locs");
  flags.suggest = cliOptionGiven(args, "suggest");
  flags.config = cliOptionGiven(args, "config");
  return flags;
}

/**
 * Return true if a long or short option appears in `args`.
 * Params:
 *   args = The original argument array, including the program name
 *   longName = Long option name without the leading dashes
 *   shortName = Optional single-character short option
 * Returns: true if the option was given
 */
bool cliOptionGiven(const string[] args, string longName, string shortName = null)
{
  foreach (arg; args.length > 1 ? args[1 .. $] : [])
  {
    if (arg == "--")
      break;
    if (arg == "--" ~ longName || arg.startsWith("--" ~ longName ~ "="))
      return true;
    if (shortName && (arg == "-" ~ shortName || (arg.startsWith("-" ~ shortName) && !arg.startsWith("--")
        && shortName.length == 1 && arg.length > 2)))
      return true;
  }

  return false;
}

private string loadConfigKey(string filename, string configDir, string key, JSONValue val, ref GidgenConfig cfg)
{
  string err;
  string strVal;
  string[] arrVal;
  bool boolVal;
  LogLevel levelVal;

  switch (key)
  {
    case "defs":
      if ((err = jsonStringOrArray(val, key, filename, arrVal)) !is null)
        return err;
      cfg.defPaths = resolveConfigPaths(arrVal, configDir);
      cfg.hasDefPaths = true;
      break;
    case "gir-path":
      if ((err = jsonStringOrArray(val, key, filename, arrVal)) !is null)
        return err;
      cfg.girPaths = resolveConfigPaths(arrVal, configDir);
      cfg.hasGirPaths = true;
      break;
    case "pkg-path":
      if ((err = jsonString(val, key, filename, strVal)) !is null)
        return err;
      cfg.pkgPath = resolveConfigPathValue(strVal, configDir);
      break;
    case "subpkg-path":
      if ((err = jsonString(val, key, filename, strVal)) !is null)
        return err;
      cfg.subPkgPath = resolveConfigPathValue(strVal, configDir);
      break;
    case "report":
      if ((err = jsonBool(val, key, filename, boolVal)) !is null)
        return err;
      cfg.report = boolVal;
      cfg.hasReport = true;
      break;
    case "report-file":
      if ((err = jsonString(val, key, filename, strVal)) !is null)
        return err;
      cfg.reportFile = resolveConfigPathValue(strVal, configDir);
      break;
    case "report-options":
      if ((err = jsonString(val, key, filename, strVal)) !is null)
        return err;
      cfg.reportOptions = strVal;
      break;
    case "log-level":
      if ((err = jsonString(val, key, filename, strVal)) !is null)
        return err;
      try
        levelVal = strVal.to!LogLevel;
      catch (ConvException)
        return "Invalid log-level value '" ~ strVal ~ "' in '" ~ filename ~ "'";
      cfg.logLevel = levelVal;
      cfg.hasLogLevel = true;
      break;
    case "log-gir-locs":
      if ((err = jsonBool(val, key, filename, boolVal)) !is null)
        return err;
      cfg.logGirLocs = boolVal;
      cfg.hasLogGirLocs = true;
      break;
    case "log-code-locs":
      if ((err = jsonBool(val, key, filename, boolVal)) !is null)
        return err;
      cfg.logCodeLocs = boolVal;
      cfg.hasLogCodeLocs = true;
      break;
    case "suggest":
      if ((err = jsonBool(val, key, filename, boolVal)) !is null)
        return err;
      cfg.suggest = boolVal;
      cfg.hasSuggest = true;
      break;
    case "dependencies":
      return loadDependencies(filename, configDir, val, cfg);
    default:
      assert(0);
  }

  return null;
}

private string loadDependencies(string filename, string configDir, JSONValue val, ref GidgenConfig cfg)
{
  if (val.type != JSONType.array)
    return "Key 'dependencies' in '" ~ filename ~ "' must be an array of objects";

  foreach (i, depVal; val.array)
  {
    auto where = "dependencies[" ~ i.to!string ~ "] in '" ~ filename ~ "'";

    if (depVal.type != JSONType.object)
      return "Entry " ~ where ~ " must be an object";

    DependencyConfig dep;
    bool hasDefs, hasGir;
    string err;
    string strVal;
    string[] arrVal;

    foreach (string key, v; depVal.object)
    {
      switch (key)
      {
        case "name":
          if ((err = jsonString(v, key, where, strVal)) !is null)
            return err;
          dep.name = strVal;
          break;
        case "version":
          if ((err = jsonString(v, key, where, strVal)) !is null)
            return err;
          dep.version_ = strVal;
          break;
        case "defs":
          if ((err = jsonStringOrArray(v, key, where, arrVal)) !is null)
            return err;
          dep.defPaths = resolveConfigPaths(arrVal, configDir);
          hasDefs = true;
          break;
        case "gir":
          if ((err = jsonStringOrArray(v, key, where, arrVal)) !is null)
            return err;
          dep.girPaths = resolveConfigPaths(arrVal, configDir);
          hasGir = true;
          break;
        default:
          return "Unknown key '" ~ key ~ "' in " ~ where;
      }
    }

    if (!hasDefs || dep.defPaths.empty)
      return "Key 'defs' is required in " ~ where;

    if (!hasGir || dep.girPaths.empty)
      return "Key 'gir' is required in " ~ where;

    foreach (path; dep.defPaths)
      if (!path.exists)
        return "Dependency defs path '" ~ path ~ "' does not exist in " ~ where;

    foreach (path; dep.girPaths)
      if (!path.exists || !path.isDir)
        return "Dependency gir path '" ~ path ~ "' is not a directory in " ~ where;

    if (dep.version_.empty)
      return "Key 'version' cannot be empty in " ~ where;

    if (dep.name && cfg.dependencies.canFind!(x => x.name == dep.name))
      return "Duplicate dependency name '" ~ dep.name ~ "' in " ~ where;

    cfg.dependencies ~= dep;
  }

  return null;
}

private string jsonString(JSONValue val, string key, string filename, out string result)
{
  if (val.type != JSONType.string)
    return "Key '" ~ key ~ "' in '" ~ filename ~ "' must be a string";
  result = val.str;
  return null;
}

private string jsonStringOrArray(JSONValue val, string key, string filename, out string[] result)
{
  if (val.type == JSONType.string)
  {
    result = [val.str];
    return null;
  }

  if (val.type != JSONType.array)
    return "Key '" ~ key ~ "' in '" ~ filename ~ "' must be a string or an array of strings";

  foreach (i, item; val.array)
  {
    if (item.type != JSONType.string)
      return "Key '" ~ key ~ "' in '" ~ filename ~ "' must be a string or an array of strings";
    result ~= item.str;
  }

  return null;
}

private string jsonBool(JSONValue val, string key, string filename, out bool result)
{
  if (val.type != JSONType.true_ && val.type != JSONType.false_)
    return "Key '" ~ key ~ "' in '" ~ filename ~ "' must be a boolean";
  result = val.boolean;
  return null;
}

private string resolveConfigPathValue(string path, string configDir)
{
  if (path.empty)
    return path;
  return absolutePath(path, configDir).asNormalizedPath.array.idup;
}

private string[] resolveConfigPaths(string[] paths, string configDir)
{
  return paths.map!(p => resolveConfigPathValue(p, configDir)).array;
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

  string writeConfig(string name, string json)
  {
    auto path = name;
    auto dir = dirName(path);
    if (dir != "." && !dir.exists)
      mkdir(dir);
    writeFile(path, json);
    return absolutePath(path).asNormalizedPath.array.idup;
  }

  string normPath(string path)
  {
    return absolutePath(path).asNormalizedPath.array.idup;
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

unittest // String and array path values, resolved relative to the config file
{
  inTempDir({
    mkdir("proj");
    mkdir("proj/defs");
    mkdir("proj/extra");
    auto file = writeConfig("proj/gidgen.json",
        `{ "defs": ["defs", "extra"], "gir-path": "gir", "pkg-path": ".", "subpkg-path": "packages",`
        ~ ` "report-file": "report.txt" }`);

    GidgenConfig cfg;
    assert(loadGidgenConfig(file, cfg) is null);
    assert(cfg.hasDefPaths);
    assert(cfg.defPaths == [normPath("proj/defs"), normPath("proj/extra")]);
    assert(cfg.hasGirPaths);
    assert(cfg.girPaths == [normPath("proj/gir")]);
    assert(cfg.pkgPath == normPath("proj"));
    assert(cfg.subPkgPath == normPath("proj/packages"));
    assert(cfg.reportFile == normPath("proj/report.txt"));
  });
}

unittest // Absolute paths in the config are left absolute
{
  inTempDir({
    auto absGir = normPath("system-gir");
    auto file = writeConfig("gidgen.json", `{ "gir-path": "` ~ absGir ~ `" }`);

    GidgenConfig cfg;
    assert(loadGidgenConfig(file, cfg) is null);
    assert(cfg.girPaths == [absGir]);
  });
}

unittest // Unknown keys are an error
{
  inTempDir({
    auto unknown = writeConfig("bad.json", `{ "foo": true }`);
    GidgenConfig cfg;
    auto err = loadGidgenConfig(unknown, cfg);
    assert(err !is null);
    assert(err.canFind("Unknown key 'foo'"));
  });
}

unittest // Invalid value types and log-level values are errors
{
  inTempDir({
    GidgenConfig cfg;
    auto err = loadGidgenConfig(writeConfig("defs.json", `{ "defs": 1 }`), cfg);
    assert(err !is null);
    assert(err.canFind("must be a string or an array of strings"));

    err = loadGidgenConfig(writeConfig("report.json", `{ "report": "yes" }`), cfg);
    assert(err !is null);
    assert(err.canFind("must be a boolean"));

    err = loadGidgenConfig(writeConfig("level.json", `{ "log-level": "loud" }`), cfg);
    assert(err !is null);
    assert(err.canFind("Invalid log-level"));
  });
}

unittest // Missing explicit --config is an error; a cwd gidgen.json is found when present
{
  inTempDir({
    string path;
    auto err = resolveConfigPath("missing.json", getcwd, path);
    assert(err !is null);
    assert(err.canFind("not found"));
    assert(path.empty);

    err = resolveConfigPath("", getcwd, path);
    assert(err is null);
    assert(path.empty);

    auto file = writeConfig(DefaultConfigName, `{ "report": true }`);
    err = resolveConfigPath("", getcwd, path);
    assert(err is null);
    assert(path == file);
  });
}

unittest // CLI flags take precedence over the config; config takes precedence over path defaults
{
  inTempDir({
    mkdir(DefaultDefsPath);
    mkdir(DefaultGirDir);
    mkdir("cfg-defs");
    mkdir("cfg-gir");

    auto file = writeConfig("gidgen.json",
        `{ "defs": "cfg-defs", "gir-path": "cfg-gir", "pkg-path": "cfg-out", "subpkg-path": "cfg-pkgs",`
        ~ ` "report": true, "log-level": "error" }`);

    GidgenConfig cfg;
    assert(loadGidgenConfig(file, cfg) is null);

    string[] defPaths = ["cli-defs"];
    string[] girPaths;
    string pkgPath;
    string subPkgPath;
    bool enableReport;
    string reportFile;
    string reportOptions = "AllUncovered";
    LogLevel logLevel = LogLevel.warning;
    bool logGirLocs, logCodeLocs, suggest;

    auto fromCli = cliFlagsFromArgs(["gidgen", "--defs", "cli-defs"]);
    assert(fromCli.defs);
    assert(!fromCli.girPath);
    applyGidgenConfig(cfg, fromCli, defPaths, girPaths, pkgPath, subPkgPath, enableReport, reportFile, reportOptions,
        logLevel, logGirLocs, logCodeLocs, suggest);

    assert(defPaths == ["cli-defs"]);
    assert(girPaths == [normPath("cfg-gir")]);
    assert(pkgPath == normPath("cfg-out"));
    assert(subPkgPath == normPath("cfg-pkgs"));
    assert(enableReport);
    assert(logLevel == LogLevel.error);

    assert(applyPathDefaults(defPaths, girPaths, pkgPath, subPkgPath) is null);
    assert(defPaths == ["cli-defs"]);
    assert(girPaths == [normPath("cfg-gir")]);
    assert(pkgPath == normPath("cfg-out"));
  });
}

unittest // cliOptionGiven recognizes long, short, and equals forms without prefix collisions
{
  assert(cliOptionGiven(["gidgen", "--report"], "report"));
  assert(!cliOptionGiven(["gidgen", "--report-file", "r.txt"], "report"));
  assert(cliOptionGiven(["gidgen", "--report-file", "r.txt"], "report-file"));
  assert(cliOptionGiven(["gidgen", "--defs=foo"], "defs", "d"));
  assert(cliOptionGiven(["gidgen", "-d", "foo"], "defs", "d"));
  assert(cliOptionGiven(["gidgen", "-dfoo"], "defs", "d"));
  assert(!cliOptionGiven(["gidgen", "--defs", "foo"], "config"));
}

unittest // Dependencies config: relative paths, string or array gir, optional name, default version
{
  inTempDir({
    mkdir("depdefs");
    mkdir("depgir");
    mkdir("depgir2");
    auto file = writeConfig("gidgen.json", `{ "dependencies": [`
        ~ ` { "name": "gid", "defs": "depdefs", "gir": ["depgir", "depgir2"], "version": "~>0.9.15" },`
        ~ ` { "defs": ["depdefs"], "gir": "depgir" } ] }`);

    GidgenConfig cfg;
    assert(loadGidgenConfig(file, cfg) is null);
    assert(cfg.dependencies.length == 2);
    assert(cfg.dependencies[0].name == "gid");
    assert(cfg.dependencies[0].version_ == "~>0.9.15");
    assert(cfg.dependencies[0].defPaths == [normPath("depdefs")]);
    assert(cfg.dependencies[0].girPaths == [normPath("depgir"), normPath("depgir2")]);
    assert(cfg.dependencies[1].name.empty);
    assert(cfg.dependencies[1].version_ == "*");
    assert(cfg.dependencies[1].girPaths == [normPath("depgir")]);
  });
}

unittest // Dependencies config validation errors
{
  inTempDir({
    mkdir("d");
    mkdir("g");
    GidgenConfig cfg;

    void check(string json, string expected)
    {
      cfg = GidgenConfig.init;
      auto err = loadGidgenConfig(writeConfig("bad.json", json), cfg);
      assert(err !is null, json);
      assert(err.canFind(expected), err);
    }

    check(`{ "dependencies": {} }`, "must be an array");
    check(`{ "dependencies": [ 1 ] }`, "must be an object");
    check(`{ "dependencies": [ { "gir": "g" } ] }`, "'defs' is required");
    check(`{ "dependencies": [ { "defs": "d" } ] }`, "'gir' is required");
    check(`{ "dependencies": [ { "defs": "d", "gir": "g", "foo": 1 } ] }`, "Unknown key 'foo'");
    check(`{ "dependencies": [ { "defs": "nope", "gir": "g" } ] }`, "defs path");
    check(`{ "dependencies": [ { "defs": "d", "gir": "nope" } ] }`, "gir path");
    check(`{ "dependencies": [ { "defs": "d", "gir": "g", "version": "" } ] }`, "'version' cannot be empty");
    check(`{ "dependencies": [ { "name": "a", "defs": "d", "gir": "g" }, { "name": "a", "defs": "d", "gir": "g" } ] }`,
        "Duplicate dependency");
  });
}
