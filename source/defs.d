module defs;

import std.utf : toUTF32;
import std.path : relativePath;

import import_manager;
import gir.alias_;
import gir.constant;
import gir.enumeration;
import gir.func;
import gir.member;
import gir.param;
import gir.repo;
import gir.property;
import gir.structure;
import gir.type_node;
import std_includes;
import utils;
import xml_patch;
import xml_tree;

/*
 * Binding generation logic:
 * loadDefFiles - Loads each definition file and populates Defs.repos and patches for the XML patches
 *
 * loadRepos - Loads the GIR files, applies XML patches, fixup(), resolve(), verify()
 *   For each repo:
 *     Loads GIR XML file
 *     Apply global XML patches
 *     Apply repo XML patches
 *     Load the XML tree to an TypeNode object tree
 *   Recursively calls fixup() on TypeNode tree to one time initialization and processing
 *   Calls resolve() on TypeNodes which haven't been resolved (can call multiple times, until all resolved or no more progress made)
 *   Calls verify() method recursively on TypeNode tree to verify all API items
 *
 *  writePackages - Write the each package and dub.json master package file
 *    Writes package for each repo
 *    Write toplevel dub.json package file
 */

enum DefsCmdPrefix = "//!"d; /// Prefix used for definition commands
enum DefsCommentPrefix = "//#"d; /// Prefix used for definition comments (not included in content)

class Defs
{
  this()
  {
  }

  /**
   * Load binding definition files
   * Params:
   *   paths = One or more paths to files or directories to load .d definition files from
   */
  void loadDefFiles(string[] defPaths)
  {
    string[] classFiles;

    foreach (path; defPaths)
    { // If path is a file load it as a single definition file, otherwise assume it is a directory and load all .d files it contains
      auto files = isFile(path) ? [path] : dirEntries(path, "*.d", SpanMode.shallow).map!(f => f.name).array.sort.array;

      foreach (filename; files)
      { // Process class files, which contain a dash in the filename, after the main repo files
        if (filename.baseName.canFind('-'))
          classFiles ~= filename;
        else
          loadDefs(filename);
      }
    }

    foreach (filename; classFiles)
      loadDefs(filename);
  }

  /**
   * Load a definition file. Filenames are of the form "Namespace.d" and "Namespace-Class.d",
   * such as "GLib.d" and "GLib-Boxed.d".
   *
   * Params:
   *   filename = The name of the definition file to load
   */
  void loadDefs(string filename)
  { // Curly brace block argument processing state machine
    enum BlockState
    {
      None, // Not processing a block
      Start, // Expecting opening brace '{'
      Content, // Processing content until '}'
    }

    // Definition code class state
    enum ClassState
    {
      Pre, // Pre class
      Start, // Start of class (expecting '{')
      In, // In class
      Post, // Post class
    }

    auto fileData = readText(filename).to!dstring; // The definition file data converted to a unicode dstring
    dstring[] cmdTokens; // Parsed definition command tokens (space separated and quoted strings)
    uint lineCount; // Current line count
    BlockState block; // Multi-line block processing state
    Repo curRepo; // Current repo or null
    dstring curModName; // Current module name or null
    ClassState classState; // Current definition code class state

    string posInfo()
    {
      return "(file '" ~ filename ~ "' line " ~ lineCount.to!string ~ ")";
    }

    // Process raw code lines in definition files (not definition commands)
    void processDefCode(dstring line, dstring lineRaw)
    {
      auto defCode = curRepo.modDefCode[curModName];

      if (classState == ClassState.Pre) // Not inside class?
      { // Is this a class declaration?
        if (line.startsWith("class") || line.startsWith("abstract class"))
        {
          defCode.classDecl = line;
          classState = ClassState.Start;
        }
        else
          defCode.preClass ~= line; // Append to pre class content
      }
      else if (classState == ClassState.Start)
      {
        if (lineRaw == "{")
          classState = ClassState.In;
      }
      else if (classState == ClassState.In)
      {
        if (lineRaw == "}")
          classState = ClassState.Post;
        else
          defCode.inClass ~= line; // Append to inside class content
      }
      else
        defCode.postClass ~= line; // Append to inside class content
    }

    auto classSplitName = filename.baseName.stripExtension.split('-');
    auto pkgName = classSplitName[0];

    if (classSplitName.length > 1) // Is this a module definition file? Will have a dash separator between the package name and module name.
    {
      auto findRepo = repos.find!(x => x.dubPackageName.to!string == pkgName);
      if (findRepo.empty) // No match to package name?
        throw new Exception("Module definition file '" ~ filename ~ "' has no matching package '"
          ~ classSplitName[0] ~ "'");

      curRepo = findRepo.front; // Assign current package repository
      curModName = classSplitName[1].to!dstring;

      if (curModName !in curRepo.modDefCode) // If structure definition code wasn't already created, create it
        curRepo.modDefCode[curModName] = new DefCode;
    }

    foreach (line; fileData.splitLines) // Loop on definition file lines
    {
      lineCount++;
      auto lineStrip = line.strip;

      if (lineStrip.startsWith(DefsCommentPrefix)) // Skip definition comments
        continue;

      dstring cmdLine;

      if (lineStrip.startsWith(DefsCmdPrefix))
        cmdLine = lineStrip[DefsCmdPrefix.length .. $];

      if (!cmdLine)
      { // All lines which aren't definition commands get processed as potential code
        if (!curModName.empty)
          processDefCode(lineStrip, line);

        continue;
      }

      if (block == BlockState.Start) // Expecting block start?
      {
        if (cmdLine.startsWith('{'))
        {
          block = BlockState.Content;
          continue;
        }

        throw new Exception("'" ~ cmdTokens[0].to!string ~ "' command requires " ~ cmdTokens.length.to!string
          ~ " arguments " ~ posInfo);
      }

      if (block == BlockState.None)
      {
        if (cmdLine.empty) // Skip empty command lines
          continue;

        cmdTokens = cmdLine.splitQuoted.array; // Parse command tokens
      }
      else if (block == BlockState.Content) // Processing multi-line brace block content
      {
        if (!cmdLine.startsWith('}'))
        { // Append content to last command argument and advance to next line
          cmdTokens[$ - 1] ~= cmdLine ~ "\n";
          continue;
        }

        block = BlockState.None; // Closing brace of multi-line block argument, fall through to process the command
      }

      auto cmd = cmdTokens[0];
      auto findCmdInfo = defCommandInfo.find!(x => x.name == cmd);

      if (findCmdInfo.empty)
        throw new Exception("Unknown command '" ~ cmd.to!string ~ "' " ~ posInfo);

      auto cmdInfo = findCmdInfo[0];

      if ((cmdInfo.flags & DefCmdFlags.AllowBlock) && cmdTokens.length == cmdInfo.argCount) // Allow multi-line block arguments for some commands
      {
        block = BlockState.Start;
        cmdTokens ~= "";
        continue;
      }

      if (cmdInfo.flags & DefCmdFlags.VarArgs)
      {
        if (cmdTokens.length < cmdInfo.argCount + 1)
          throw new Exception("'" ~ cmd.to!string ~ "' command requires at least " ~ cmdInfo.argCount.to!string
            ~ (cmdInfo.argCount == 1 ? " argument " : " arguments ") ~ posInfo);
      }
      else if (cmdTokens.length != cmdInfo.argCount + 1)
        throw new Exception("'" ~ cmd.to!string ~ "' command requires " ~ cmdInfo.argCount.to!string
          ~ (cmdInfo.argCount == 1 ? " argument " : " arguments ") ~ posInfo);

      if (cmdInfo.flags & DefCmdFlags.ReqRepo && !curRepo)
        throw new Exception("'" ~ cmd.to!string ~ "' command requires 'repo' to be specified " ~ posInfo);

      if (cmdInfo.flags & DefCmdFlags.ReqClass && !curModName)
        throw new Exception("'" ~ cmd.to!string ~ "' command requires module to be specified " ~ posInfo);

      switch (cmd)
      {
        case "add", "del", "rename", "set":
          auto patch = new XmlPatch();

          try
          {
            final switch (cmd)
            {
              case "add":
                patch.parseAddCmd(cmdTokens[1], cmdTokens[2]);
                break;
              case "del":
                patch.parseDeleteCmd(cmdTokens[1]);
                break;
              case "rename":
                patch.parseRenameCmd(cmdTokens[1], cmdTokens[2]);
                break;
              case "set":
                patch.parseSetCmd(cmdTokens[1], cmdTokens[2]);
                break;
            }
          }
          catch (XmlPatchError e)
            throw new Exception("XML patch error: " ~ e.msg ~ " " ~ posInfo);

          if (curRepo)
            curRepo.patches ~= patch;
          else
            patches ~= patch;
          break;
        case "class":
          if (cmdTokens.length > 3)
            throw new Exception("Too many arguments to 'class' command " ~ posInfo);

          if (classSplitName.length == 1) // Only update current module name if this is not a module definition file
            curModName = cmdTokens[1].snakeCase;

          if (cmdTokens.length > 2)
          {
            try
              classState = cmdTokens[2].capitalize.to!ClassState;
            catch (ConvException e)
              throw new Exception("Class command code location must be one of: "
                ~ [EnumMembers!ClassState].map!(x => x.to!string).join(", ") ~ " " ~ posInfo);
          }
          else
            classState = ClassState.In;

          if (curModName !in curRepo.modDefCode)
            curRepo.modDefCode[curModName] = new DefCode;

          curRepo.modDefCode[curModName].className = cmdTokens[1];
          break;
        case "gir":
          curRepo = new Repo(this, cmdTokens[1].to!string);
          curRepo.defsFilename = filename;
          curRepo.dubPackageName = pkgName.to!dstring; // From definition filename
          repos ~= curRepo;
          curModName = null;
          break;
        case "info":
          if (curRepo)
            curRepo.dubInfo[cmdTokens[1].to!string] ~= cmdTokens[2];
          else
            dubInfo[cmdTokens[1].to!string] ~= cmdTokens[2];
          break;
        case "inhibit":
          try
            curRepo.modDefCode[curModName].inhibitFlags = cmdTokens[1 .. $].map!(x => x.capitalize.to!DefInhibitFlags)
              .fold!((a, b) => a | b);
          catch (ConvException e)
            throw new Exception("Invalid inhibit flags '" ~ cmdTokens[1 .. $].join(" ").to!string ~ "'");

          break;
        case "kind":
          TypeKind kind;

          try
            kind = cmdTokens[2].to!TypeKind;
          catch (Exception e)
            throw new Exception("Unknown type kind '" ~ cmdTokens[2].to!string ~ "' should be one of: " ~
              [EnumMembers!TypeKind].map!(x => x.to!string).join(", ") ~ " " ~ posInfo);

          curRepo.kindSubs[cmdTokens[1]] = kind;
          break;
        case "merge":
          curRepo.mergeNsVer = NamespaceVersion(cmdTokens[1], cmdTokens[2]);
          break;
        case "namespace":
          curRepo = new Repo(this, null);
          curRepo.defsFilename = filename;
          curRepo.namespace = cmdTokens[1];
          curRepo.dubPackageName = pkgName.to!dstring; // From definition filename
          repos ~= curRepo;
          curModName = null;
          break;
        case "reserved":
          reservedWords[cmdTokens[1]] = true;
          break;
        case "subctype", "subdtype", "subtype":
          dstring[dstring]*[] subs;

          if (cmd == "subctype" || cmd == "subtype")
            subs ~= curRepo ? &curRepo.cTypeSubs : &cTypeSubs;

          if (cmd == "subdtype" || cmd == "subtype")
            subs ~= curRepo ? &curRepo.dTypeSubs : &dTypeSubs;

          foreach (subMap; subs)
          {
            if (cmdTokens[1] !in (*subMap))
              (*subMap)[cmdTokens[1]] = cmdTokens[2];
            else
              throw new Exception(cmd.to!string ~ " '" ~ cmdTokens[1].to!string ~ "' already exists " ~ posInfo);
          }
          break;
        default:
          assert(0);
      }
    }
  }

  /**
   * Load gir files specified in the loaded definition files to Repo objects.
   * Params:
   *   girPaths = Array of paths to search for Gir files
   */
  void loadRepos(string[] girPaths = ["/usr/share/gir-1.0"])
  {
    repos = repos.sort!((x, y) => x.namespace < y.namespace).array;

    foreach (repo; repos)
    {
      if (repo.filename.empty)
        continue;

      auto repoPaths = girPaths.map!(path => buildPath(path, repo.filename ~ ".gir")).filter!(x => x.exists);

      if (repoPaths.empty)
        throw new Exception("Repository Gir file '" ~ repo.filename ~ "' not found (search paths = "
          ~ girPaths.join(":") ~ ")");

      repo.filename = repoPaths.front;

      if (repoPaths.count > 1)
        warning("Multiple matches found for repository in path, using " ~ repo.filename);

      auto tree = new XmlTree();
      tree.parse(readText(repo.filename).toUTF32, repo.filename); // Load the Gir XML file

      XmlNode namespaceNode;

      if (tree.root)
      {
        foreach (n; tree.root.children)
          if (n.id == "namespace")
            namespaceNode = n;
      }

      if (!namespaceNode)
        throw new Exception("No 'namespace' XML node found in Gir file");

      foreach (patch; patches) // Apply global XML patches from defs file
      {
        try
          patch.apply(tree, namespaceNode);
        catch (XmlPatchError e)
        {
        }
      }

      foreach (patch; repo.patches) // Apply XML patches from defs file
        patch.apply(tree, namespaceNode);

      repo.fromXmlTree(tree); // Convert XML tree to Gir object tree
    }

    fixupVerifyRepos();
  }

  /// Ensure consistent state of repo data and fixup additional data (array parameter indexes, etc)
  private void fixupVerifyRepos()
  {
    repoHash = repos.map!(x => tuple(NamespaceVersion(x.namespace, x.nsVersion), x)).assocArray;

    foreach (repo; repos) // Resolve include and merge repos
    {
      foreach (inc; repo.includes)
      {
        if (auto incRepo = repoHash.get(inc, null))
          repo.includeRepos ~= incRepo;
        else
          throw new Exception("Unresolved dependency " ~ inc.name.to!string ~ " version " ~ inc.version_.to!string
            ~ " for package " ~ repo.dubPackageName.to!string);
      }

      if (repo.mergeNsVer.name)
      {
        repo.mergeRepo = repoHash.get(repo.mergeNsVer, null);
        if (!repo.mergeRepo)
          throw new Exception("Repo '" ~ repo.mergeNsVer.to!string ~ "' not found to merge '"
            ~ repo.dubPackageName.to!string ~ "' into");

        repo.mergeRepo.mergedRepos ~= repo;
      }
    }

    foreach (repo; repos) // Fix-up repos
      repo.fixup;

    // Keep trying to resolve types until there are no more or no progress is made
    while (true)
    {
      auto count = unresolvedTypes.length;

      foreach (typeNode; unresolvedTypes.keys)
        typeNode.doResolve;

      if (count == unresolvedTypes.length)
        break;
    }

//    if (!unresolvedTypes.empty)
//      unresolvedTypes.keys.map!(x => x.fullName ~ ", " ~ x.dType ~ ", " ~ x.to!dstring ~ ", " ~ unresolvedTypes[x].to!uint.to!dstring).array.sort.join("\n").writeln;

    foreach (repo; repos)
      repo.verify;
  }

  /**
   * Write the packages for the loaded Repo objects defined in the definitions files.
   * Params:
   *   pkgPath = The path to the toplevel packages directory (should be absolute)
   *   subPkgPath = Sub-package path to write package libraries to (should be absolute and a subdirectory of pkgPath)
   */
  void writePackages(string pkgPath, string subPkgPath)
  {
    foreach (repo; repos)
      repo.writePackage(subPkgPath);

    writeDubJsonFile(pkgPath, subPkgPath);
    writePackageReadme(pkgPath);
  }

  /**
   * Write master packages dub JSON file.
   * Params:
   *   pkgPath = The path to the toplevel packages directory (should be absolute)
   *   subPkgPath = Sub-package path to write package libraries to (should be absolute and a subdirectory of pkgPath)
   */
  private void writeDubJsonFile(string pkgPath, string subPkgPath)
  {
    auto relPath = relativePath(subPkgPath, pkgPath);

    string output = "{\n";

    foreach (key; ["name", "description", "copyright", "authors", "license", "homepage"])
    {
      if (auto val = dubInfo.get(key, null))
      {
        if (key == "authors")
          output ~= `  "authors": [` ~ val.map!(x => `"` ~ x.to!string ~ `"`).join(", ") ~ "],\n";
        else
          output ~= `  "` ~ key.to!string ~ `": "` ~ val[0].to!string ~ "\",\n";
      }
    }

    output ~= `  "targetType": "none",` ~ "\n";
	  output ~= `  "dependencies": {` ~ "\n";

    auto sortedRepos = repos.filter!(x => !x.mergeRepo).map!(x => x.dubPackageName.to!string.toLower).array.sort;

    output ~= sortedRepos.map!(x => `    ":` ~ x ~ `": "*"`).join(",\n");
    output ~= "\n  },\n";

    output ~= `  "subPackages": [` ~ "\n";
    output ~= sortedRepos.map!(x => `    "` ~ buildPath(relPath, x).replace("\\", "/") ~ `/"`).join(",\n"); // Just use forward slashes on Windows
    output ~= "\n  ]\n}\n";

    auto path = buildPath(pkgPath, "dub.json");

    if (!path.exists || readText(path) != output) // Only update dub.json if changed (build optimization)
      write(path, output);
  }

  /// Write the top-level package README.md file
  private void writePackageReadme(string pkgPath)
  {
    auto s = "# " ~ ("name" in dubInfo ? (dubInfo["name"][0] ~ " -") : "") ~ dubInfo["description"][0] ~ "\n\n";

    s ~= "This is the top-level [Dub](https://dub.pm/) package for [giD](https://github.com/Kymorphia/gid) "
      ~ "the GObject Introspection D Language Binding Repository.\n\n";
    s ~= "## Sub Package Information\n\n";

    // Create a markdown table with info/links
    s ~= "| Library | Dub Package | D API | C API |\n| --- | --- | --- | --- |\n";

    s ~= repos.filter!(x => x.name != "gid").map!(r => format("| %-80s | %-80s | %-80s | %-80s |\n"d,
      "homepage" in r.dubInfo ? ("[" ~ r.namespace ~ " " ~ r.nsVersion ~ "](" ~ r.dubInfo["homepage"][0] ~ ")")
      : r.namespace ~ " " ~ r.nsVersion,
      "[gid:" ~ r.dubPackageName ~ "](https://code.dlang.org/packages/gid%3A" ~ r.dubPackageName ~ ")",
      "docs" in r.dubInfo ? ("[D API](" ~ r.dubInfo["docs"][0] ~ ")") : "",
      "capi" in r.dubInfo ? ("[C API](" ~ r.dubInfo["capi"][0] ~ ")") : "",
    )).join ~ "\n";

    s ~= "Consult the [giD README](https://github.com/Kymorphia/gid) for more information on programming with giD"
      ~ " and links to examples.\n";

    write(buildPath(pkgPath, "packages", "README.md"), s.to!string);
  }

  /**
   * Fix symbol name if it is a reserved word, by appending an underscore to it.
   * Params:
   *   name = The symbol name
   * Returns: The symbol name possibly with underscore appended if it is a reserved Dlang word
   */
  dstring symbolName(dstring name)
  {
    if (name in reservedWords) // Add underscore to reserved words
      return name ~ "_";
    else if (!name.empty && name[0] >= '0' && name[0] <= '9') // Starts with a digit? Can happen with some shortened enum members.
      return "_" ~ name;

    return name;
  }

  /**
   * Dump JSON representation of object tree for troubleshooting and debugging purposes.
   * Returns: JSONValue JSON object
   */
  JSONValue dumpJson(bool dumpDocs)
  {
    Base.dumpJsonDocs = dumpDocs;

    JSONValue js = JSONValue.emptyObject;
    js.jsonSetNonDefault("reservedWords", reservedWords.keys);
    js.jsonSetNonDefault("cTypeSubs", cTypeSubs);
    js.jsonSetNonDefault("dTypeSubs", dTypeSubs);
    js.jsonSetNonDefault("repos", repos);
    return js;
  }

  bool[dstring] reservedWords; /// Reserved words (_ appended)
  dstring[dstring] cTypeSubs; /// Global C type substitutions
  dstring[dstring] dTypeSubs; /// Global D type substitutions
  dstring[][string] dubInfo; /// Dub JSON file info (name, description, copyright, authors, license, homepage, docs), only "authors" uses multiple values, docs is a URL for D API docs used in generated README.md files only
  XmlPatch[] patches; /// Global XML patches specified in definitions file
  Repo[] repos; /// Gir repositories
  Repo[NamespaceVersion] repoHash; /// Hash of repositories by namespace name and version
  TypeNode[dstring] cSymbolHash; /// Hash of all C symbols across all repos
  UnresolvedFlags[TypeNode] unresolvedTypes; /// TypeNode objects which have an unresolved type (for recursive type resolution)
}

/// Manual code from a definitions file
class DefCode
{
  DefInhibitFlags inhibitFlags; /// Module code generation inhibit flags
  dstring className; /// The class name
  dstring[] preClass; /// Pre class declaration code, line separated
  dstring classDecl; /// Class declaration
  dstring[] inClass; /// Code inside of the class, line separated
  dstring[] postClass; /// Post class code
}

/// Module code generation inhibit flags
enum DefInhibitFlags
{
  Nothing = 0, /// Don't inhibit anything
  Imports = 1 << 0, /// Inhibit automatic import code generation
  Init = 1 << 1, /// Inhibit class init code generation
  Funcs = 1 << 2, /// Inhibit function binding code generation
}

/// Definition command flags
enum DefCmdFlags
{
  AllowBlock = 1 << 0, /// Allow a multi-line block argument (last arg)
  ReqRepo = 1 << 1, /// Require repo to have been specified
  ReqClass = 1 << 2, /// Require struct to have been specified
  VarArgs = 1 << 3, /// Variable number of arguments, argCount is the minimum
  None = 0,
}

/// Definition command info
struct DefCmd
{
  dstring name;
  int argCount;
  BitFlags!DefCmdFlags flags;
  string help;
}

/// Display binding definition file help
void displayDefHelp()
{
  writeln("giD binding definition command help\n"
    ~ "Commands are prefixed with '//!'.\n"
    ~ "giD comments are prefixed with '//#' and aren't output to binding code.\n"
    ~ "Strings can be single or double quoted.\n"
    ~ "Some commands support multi-line values using opening and close braces within giD comment lines ('Block' flag).\n"
    ~ "Commands indicating 'Repo' in parenthesis require a repo to have been specified, 'Class' requires a class (or struct).\n"
    ~ "Commands:\n"
  );

  foreach (cmd; defCommandInfo)
  {
    auto flags = cast(int)cmd.flags;
    auto flagStr = flags != DefCmdFlags.None ? (" (" ~ 3.iota.filter!(x => (flags & (1 << x)))
      .map!(x => ["Block", "Repo", "Class"][x]).join(", ") ~ ")") : "";
    writeln(cmd.help ~ flagStr);
  }
}

// Command information
immutable DefCmd[] defCommandInfo = [
  {
    "add", 2, DefCmdFlags.AllowBlock, "add <XmlSelect> <AttributeValue | Xml> - Add an XML attribute or node"
  },
  {"class", 1, DefCmdFlags.ReqRepo | DefCmdFlags.VarArgs, "class <Class> [Pre|In|Post] - Current class/module and"
    ~ " location (defaults to In)"},
  {"del", 1, DefCmdFlags.None, "del <XmlSelect> - Delete an XML attribute or node"},
  {"gir", 1, DefCmdFlags.None, "gir <GirName> - GIR file to load"},
  {"info", 2, DefCmdFlags.None, "info <name> <value> - Set package info"
    ~ " (name, description, copyright, authors, license, homepage, docs, capi), multiple authors values can be given"},
  {"inhibit", 1, DefCmdFlags.ReqClass | DefCmdFlags.VarArgs, "inhibit [" ~ [EnumMembers!DefInhibitFlags]
    .map!(x => x.to!string.toLower).join(" ") ~ "] - Inhibit generation of certain module code (space separated flags)"},
  {"kind", 2, DefCmdFlags.ReqRepo, "kind <TypeName> <TypeKind> - Override a type kind"},
  {"merge", 2, DefCmdFlags.ReqRepo, "merge <Namespace> <Version> - Merge repo into the package with Namespace and Version"},
  {"namespace", 1, DefCmdFlags.None, "namespace <Namespace> - Create a repository from a namespace instead of a Gir file"},
  {
    "rename", 2, DefCmdFlags.None, "rename <XmlSelect> <AttributeName | XmlNodeId> - Rename an XML attribute or node ID"
  },
  {
    "reserved", 1, DefCmdFlags.None, "reserved <Word> - Identify a reserved word, an underscore will be appended to it"
  },
  {
    "set", 2, DefCmdFlags.AllowBlock, "set <XmlSelect> <AttributeValue | Xml> - Set an XML attribute or node"
  },
  {"subtype", 2, DefCmdFlags.None, "subtype <FromTypeName> <ToTypeName> - Substitute a type name (D and C types)"},
  {"subctype", 2, DefCmdFlags.None, "subctype <FromTypeName> <ToTypeName> - Substitute a C type name"},
  {"subdtype", 2, DefCmdFlags.None, "subdtype <FromTypeName> <ToTypeName> - Substitute a D type name"},
];
