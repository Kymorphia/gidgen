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
    dstring curStructName; // Current structure name or null
    ClassState classState; // Current definition code class state

    string posInfo()
    {
      return "(file '" ~ filename ~ "' line " ~ lineCount.to!string ~ ")";
    }

    // Process raw code lines in definition files (not definition commands)
    void processDefCode(dstring line, dstring lineRaw)
    {
      auto defCode = curRepo.structDefCode[curStructName];

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
    auto repoName = classSplitName[0];

    if (classSplitName.length > 1) // Is this a repo class file? Will have a dash separator between the namespace and class names.
    {
      auto findRepo = repos.find!(x => x.namespace.to!string == repoName);

      if (findRepo.length > 0) // Is there a repo with the matching namespace from the filename?
      { // Set current repo and struct name from filename
        curRepo = findRepo[0];
        curStructName = classSplitName[1].to!dstring;

        if (curStructName !in curRepo.structDefCode) // If structure definition code wasn't already created, create it
          curRepo.structDefCode[curStructName] = new DefCode();
      }
      else
      {
        warning("Ignoring class definition file '", filename, "' with no matching repository '",
            classSplitName[0], "'");
        return;
      }
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
        if (!curStructName.empty)
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

        warning("'", cmdTokens[0], "' command requires ", cmdTokens.length, " arguments ", posInfo);
        block = BlockState.None; // Just ignore previous command and continue to process the line
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
      {
        warning("Unknown command '", cmd, "'");
        continue;
      }

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
        {
          warning("'", cmd, "' command requires at least ", cmdInfo.argCount, " ", cmdInfo.argCount == 1
            ? "argument " : "arguments ", posInfo);
          break;
        }
      }
      else if (cmdTokens.length != cmdInfo.argCount + 1)
      {
        warning("'", cmd, "' command requires ", cmdInfo.argCount, " ", cmdInfo.argCount == 1
            ? "argument " : "arguments ", posInfo);
        break;
      }

      if (cmdInfo.flags & DefCmdFlags.ReqRepo && !curRepo)
      {
        warning("'", cmd, "' command requires 'repo' to be specified ", posInfo);
        continue;
      }

      if (cmdInfo.flags & DefCmdFlags.ReqClass && !curStructName)
      {
        warning("'", cmd, "' command requires 'struct' to be specified ", posInfo);
        continue;
      }

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
          {
            warning("XML patch error: ", e.msg, " ", posInfo);
            break;
          }

          if (curRepo)
            curRepo.patches ~= patch;
          else
            patches ~= patch;
          break;
        case "class":
          curStructName = cmdTokens[1];
          classState = ClassState.In; // FIXME - How to specify other sections of modules?

          if (curStructName !in curRepo.structDefCode)
            curRepo.structDefCode[curStructName] = new DefCode();
          else
            warning("Duplicate class command found for '", curStructName, "' ", posInfo);
          break;
        case "gir":
          curRepo = new Repo(this, cmdTokens[1].to!string);
          curRepo.defsFilename = filename;
          curRepo.namespace = repoName.to!dstring;
          repos ~= curRepo;
          curStructName = null;
          break;
        case "import":
          curRepo.structDefCode[curStructName].imports.add(cmdTokens[1]);
          break;
        case "info":
          if (curRepo)
            curRepo.dubInfo[cmdTokens[1].to!string] ~= cmdTokens[2];
          else
            dubInfo[cmdTokens[1].to!string] ~= cmdTokens[2];
          break;
        case "inhibit":
          try
            curRepo.structDefCode[curStructName].inhibitFlags = cmdTokens[1 .. $].map!(x => x.capitalize.to!DefInhibitFlags)
              .fold!((a, b) => a | b);
          catch (ConvException e)
          {
            warning("Invalid inhibit flags '" ~ cmdTokens[1 .. $].join(" ") ~ "'");
            break;
          }

          break;
        case "kind":
          TypeKind kind;

          try
            kind = cmdTokens[2].to!TypeKind;
          catch (Exception e)
          {
            warning("Unknown type kind '" ~ cmdTokens[2].to!string ~ "' should be one of: " ~
              [EnumMembers!TypeKind].map!(x => x.to!string).join(", "), " ", posInfo);
            break;
          }

          curRepo.kindSubs[cmdTokens[1]] = kind;
          break;
        case "merge":
          curRepo.mergeRepoName = cmdTokens[1];
          break;
        case "namespace":
          curRepo = new Repo(this, null);
          curRepo.defsFilename = filename;
          curRepo.namespace = cmdTokens[1];
          repos ~= curRepo;
          curStructName = null;
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
              warning(cmd, " '", cmdTokens[1], "' already exists ", posInfo);
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
    repoHash = repos.map!(x => tuple(x.namespace, x)).assocArray;

    foreach (repo; repos)
      repo.fixup;

    // Keep trying to resolve types until there are no more or no progress is made
    while (true)
    {
      auto count = unresolvedTypes.length;

      foreach (typeNode; unresolvedTypes.keys)
        typeNode.resolve;

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

    foreach (key; ["name", "description", "copyright", "authors", "license"])
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

    auto sortedRepos = repos.filter!(x => x.mergeRepoName.empty).map!(x => x.dubPackageName.to!string.toLower)
      .array.sort;

    output ~= sortedRepos.map!(x => `    ":` ~ x ~ `": "*"`).join(",\n");
    output ~= "\n  },\n";

    output ~= `  "subPackages": [` ~ "\n";
    output ~= sortedRepos.map!(x => `    "` ~ buildPath(relPath, x).replace("\\", "/") ~ `/"`).join(",\n"); // Just use forward slashes on Windows
    output ~= "\n  ]\n}\n";

    write(buildPath(pkgPath, "dub.json"), output);
  }

  /**
   * Get the kind classification of a type string
   * Params:
   *   type = The D type string
   *   repo = The repo the type was found in
   * Returns: The type classification, falls back to TypeKind.Basic if no other type applies
   */
  TypeKind typeKind(dstring type, Repo repo)
  {
    foreach (i; 0 .. 4) // Resolve up to 4 alias dereferences
    {
      if (type.among("char*"d, "const char*"d, "string"d, "utf8"d))
        return TypeKind.String;

      if (type.isBasicType)
        return i > 0 ? TypeKind.BasicAlias : TypeKind.Basic;

      if (auto obj = repo.typeObjectHash.get(type, null))
      {
        if (auto al = cast(Alias)obj)
        {
          type = al.dType;
          continue;
        }

        if (cast(Func)obj)
          return TypeKind.Callback;
        else if (auto node = cast(TypeNode)obj)
          return node.kind;
        else if (cast(Constant)obj)
          return TypeKind.Basic;
        else if (auto en = cast(Enumeration)obj)
          return en.bitfield ? TypeKind.Flags : TypeKind.Enum;
        else
          return TypeKind.Unknown;
      }
    }

    return TypeKind.Unknown; // Too many alias dereferences
  }

  /**
   * Find type object by D type name which may include the namespace separated by a period.
   * Params:
   *   typeName = Type name string
   *   repo = Repo to use if typeName does not contain a namespace (optional if typeName has a namespace)
   * Returns: The matching type object or null if not found (possible basic type), throws an exception if typeName has a namespace that wasn't resolved
   */
  TypeNode findTypeObject(dstring typeName, Repo repo = null)
  {
    auto t = typeName.split('.');
    if (t.length > 1)
    {
      repo = repoHash.get(t[0], null);

      if (!repo)
        throw new Exception("Failed to resolve namespace '" ~ t[0].to!string ~ "' for type '"
          ~ typeName.to!string ~ "'");

      typeName = t[1];
    }

    assert(repo);

    return repo.typeObjectHash.get(subTypeStr(typeName, dTypeSubs, repo.dTypeSubs), null);
  }

  /**
   * Find type object by Gir doc reference (as found in Gir documentation).
   * In the form [kind@Namespace.TypeName(.|::|:)SubTypeName] where SubTypeName is optional.
   * Params:
   *   refStr = Type name string
   *   repo = Repo to use if refStr does not contain a namespace (optional if refStr has a valid namespace)
   * Returns: The matching type object or null if not found (possible basic type)
   */
  TypeNode findTypeObjectByGDocRef(dstring refStr, Repo repo = null)
  {
    auto refRe = ctRegex!(`^(?P<kind>[a-z]+)@(?P<Namespace>[A-Za-z]+)\.(?P<TypeName>[A-Za-z0-9_]+)`d
      ~ `(?:\.|::|:)?(?P<SubTypeName>[A-Za-z0-9_]*)`d);
    auto c = refStr.matchFirst(refRe);

    dstring kind, nameSpace, typeName, subTypeName;

    if (c.empty) // No match to kind@Namespace.TypeName.SubTypeName? - Process it as a dot separated type name
    {
      auto t = refStr.split('.');

      if (t.length > 1)
      {
        nameSpace = t[0];
        typeName = t[1];

        if (t.length > 2)
          subTypeName = t[2];
      }
      else
        typeName = t[0];

      kind = "func"; // FIXME - This seems a little hackish, just assume it is a function/method rather than a property or signal
    }
    else
    {
      kind = c["kind"];
      nameSpace = c["Namespace"];
      typeName = c["TypeName"];
      subTypeName = c["SubTypeName"];
    }

    if (!nameSpace.empty)
    {
      if (auto r = repoHash.get(nameSpace, null))
        repo = r;
      else // Namespace did not match, assume it is TypeName.SubTypeName without Namespace
      {
        subTypeName = typeName;
        typeName = nameSpace;
      }
    }

    assert(repo);

    typeName = subTypeStr(typeName, dTypeSubs, repo.dTypeSubs);
    auto tn = repo.typeObjectHash.get(typeName, null);

    if (subTypeName.empty || !tn)
      return tn;

    if (auto st = cast(Structure)tn)
    {
      switch (kind)
      {
        case "func", "method", "ctor":
          return st.funcNameHash.get(subTypeName, null);
        case "property":
          return st.properties.find!(x => x.name == subTypeName).frontIfNotEmpty(cast(Property)null);
        case "signal":
          return st.signals.find!(x => x.name == subTypeName).frontIfNotEmpty(cast(Func)null);
        default: // Includes "vfunc"
          return null;
      }
    }
    else if (auto en = cast(Enumeration)tn)
      return en.members.find!(x => x.name == subTypeName).frontIfNotEmpty(cast(Member)null);

    return null;
  }

  /**
  * Format a GTK doc string to be a DDoc string.
  * Newlines are formatted with a prefix to match the indendation for the comment block.
  * References in the form [kind@Namespace.TypeName(.|::|:)SubTypeName] are replaced with DDoc references.
  * Function references func() are changed to the D function/method name and set to bold.
  *
  * Params:
  *   s = The GTK doc string
  *   prefix = The newline wrap prefix
  *   repo = A default repository to look up references in (defaults to null)
  * Returns: The DDoc formatted string
  */
  dstring gdocToDDoc(dstring s, dstring prefix, Repo repo = null)
  {
    import std.regex : Captures, ctRegex, replaceAll;
    auto escapeRe = ctRegex!(r"[$=]"d);
    auto nlSpaceRe = ctRegex!(r"\n\s*"d);
    auto refRe = ctRegex!(r"\[`?([a-z]+@[^\]]+)`?\]"d);
    auto funcRe = ctRegex!(r"([a-z0-9_]+)\(\)"d);

    s = s.replaceAll(escapeRe, "\\$&"); // Escape special characters

    // Format newlines for comment block
    prefix = "\n" ~ prefix;
    s = s.replaceAll(nlSpaceRe, prefix);

    dstring refReplace(Captures!(dstring) m)
    {
      auto tn = findTypeObjectByGDocRef(m[1], repo);

      if (auto fn = cast(Func)tn)
        return "[" ~ fn.fullDName ~ "]";
      else if (tn)
        return "[" ~ tn.fullName ~ "]";
      else
        return m[1];
    }

    s = replaceAll!(refReplace)(s, refRe); // Replace [kind@name] with DDoc reference

    dstring funcReplace(Captures!(dstring) m)
    {
      if (auto tn = cSymbolHash.get(m[1], null))
      {
        if (auto fn = cast(Func)tn)
          return "[" ~ fn.fullDName ~ "]";
        else
          return "[" ~ tn.fullName ~ "]";
      }

      return m[0];
    }

    s = replaceAll!(funcReplace)(s, funcRe); // Replace func() references

    return s.escapeNonRefLinkParens;
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
   * Substitute type.
   * Params:
   *   type = Type string
   *   subs = Primary substitution map (usually dTypeSubs or cTypeSubs)
   *   localSubs = Local substitution map or null (default)
   * Returns: Type string with any relevant substitutions
   */
  dstring subTypeStr(dstring type, dstring[dstring] subs, dstring[dstring] localSubs = null)
  {
    enum State
    {
      Start, // Start of type string
      Space, // After a space character
      Star, // After a * pointer
      Char, // After a type character
    }

    State state; // Current state of state machine
    bool isConst; // Set to true if there are any "const" strings
    dstring subType; // Substitute type string being built
    int starCount; // Number of stars

    if (auto s = subs.get(type, null)) // See if the type exactly matches a substitution (handles multi word substitutions too)
      return s;

    // Loop over the type string consuming it head first
    for (; !type.empty; type = type[1 .. $])
    {
      dstring skip; // A token to skip (const or volatile)

      if (state != State.Char) // Last state was not a type character?
      {
        if (type.startsWith("const"))
          skip = "const";
        else if (type.startsWith("volatile"))
          skip = "volatile";

        if (!skip.empty) // Skip first ask questions later
        {
          type = type[skip.length .. $];

          if (type.empty) // No more chars after skipped token? - Done, but probably an invalid type
            break;
        }
      }

      if (type[0] == ' ' || type[0] == '*') // Space or star?
      {
        if (skip == "const")
          isConst = true;

        if (type[0] == '*')
        {
          starCount++;
          state = State.Star;
        }
        else
          state = State.Space; // Spaces only get added if it is followed by a regular character
      }
      else // Type name character
      {
        if (state == State.Space && !subType.empty) // Add a space if the last character was a space, since this is a regular character
          subType ~= ' ';

        if (!skip.empty) // Possible skip token was followed by a regular type character? - Don't skip it then (append it to the subType)
          subType ~= skip;

        subType ~= type[0]; // Append the character to the substituted type
        state = State.Char;
      }
    }

    if (subType in localSubs)
      subType = localSubs[subType]; // Try localSubs first
    else
      subType = subs.get(subType, subType); // Then subs with fallback to itself

    if (starCount == 0) // Not a pointer type?
      return subType;

    if (isConst) // Constant pointer type?
      return "const(" ~ subType ~ "*"d.repeat(starCount - 1).join ~ ")*"d;

    return subType ~ "*"d.repeat(starCount).join; // Just a pointer type
  }

  /**
   * Designate the start of output processing for module imports. Enables tracking of symbol names and
   * creation of aliases for conflicts for used symbols.
   */
  void beginImports(Structure klassModule)
  {
    importManager = new ImportManager(klassModule);
    importManager.add("gid.gid");
    importManager.add("types");
    importManager.add(klassModule.repo.namespace ~ ".c.functions");
    importManager.add(klassModule.repo.namespace ~ ".c.types");
  }

  /**
   * Indicates the end of processing for the current output module imports which was activated with beginImports().
   */
  void endImports()
  {
    importManager = null;
  }

  /**
   * Resolve a D type symbol name. Uses the D type name or an alias if it conflicts with
   * another symbol imported into the current module which was defined by a call to beginImports().
   * Params:
   *   typeNode = The type node object to get the D type name of
   * Returns: The D type name or an alias
   */
  dstring resolveSymbol(dstring typeName)
  {
    auto typeNode = findTypeObject(typeName);

    if (!typeNode)
      throw new Exception("Failed to resolve symbol '" ~ typeName.to!string ~ "'");

    if (importManager)
      return importManager.resolveDType(typeNode);

    return typeNode.dType;
  }

  bool[dstring] reservedWords; /// Reserved words (_ appended)
  dstring[dstring] cTypeSubs; /// Global C type substitutions
  dstring[dstring] dTypeSubs; /// Global D type substitutions
  dstring[][string] dubInfo; /// Dub JSON file info ("name", "description", "copyright", "authors", "license"), only "authors" uses multiple values
  XmlPatch[] patches; /// Global XML patches specified in definitions file
  Repo[] repos; /// Gir repositories
  Repo[dstring] repoHash; /// Hash of repositories by namespace
  TypeNode[dstring] cSymbolHash; /// Hash of all C symbols across all repos
  ImportManager importManager; /// Current import manager used with beginImports()/endImports()
  UnresolvedFlags[TypeNode] unresolvedTypes; /// TypeNode objects which have an unresolved type (for recursive type resolution)
}

/// Manual code from a definitions file
class DefCode
{
  this()
  {
    imports = new ImportManager();
  }

  DefInhibitFlags inhibitFlags; /// Module code generation inhibit flags
  ImportManager imports; /// Imports
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
  {"class", 1, DefCmdFlags.ReqRepo, "class <Class> - Select the current structure/class"},
  {"del", 1, DefCmdFlags.None, "del <XmlSelect> - Delete an XML attribute or node"},
  {"gir", 1, DefCmdFlags.None, "gir <GirName> - GIR file to load"},
  {"import", 1, DefCmdFlags.ReqClass, "import <Import> - Add a D import"},
  {"info", 2, DefCmdFlags.None, "info <name> <value> - Set JSON dub info for repo or master package"
    ~ " (name, description, copyright, authors, license), multiple authors values can be given"},
  {"inhibit", 1, DefCmdFlags.ReqClass | DefCmdFlags.VarArgs, "inhibit [" ~ [EnumMembers!DefInhibitFlags]
    .map!(x => x.to!string.toLower).join(" ") ~ "] - Inhibit generation of certain module code (space separated flags)"},
  {"kind", 2, DefCmdFlags.ReqRepo, "kind <TypeName> <TypeKind> - Override a type kind"},
  {"merge", 1, DefCmdFlags.ReqRepo, "merge <Namespace> - Merge current repo into the package identified by Namespace"},
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
