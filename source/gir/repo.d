module gir.repo;

import code_writer;
import defs;
import gir.alias_;
import gir.base;
import gir.constant;
import gir.enumeration;
import gir.field;
import gir.func;
import gir.func_writer;
import gir.member;
import gir.param;
import gir.property;
import gir.return_value;
import gir.structure;
import gir.type_node;
import import_manager;
import std_includes;
import utils;
import xml_patch;
import xml_tree;

/// Gir repository
final class Repo : Base
{
  this(Defs defs)
  {
    this.defs = defs;

    // Add global namespace structure
    globalStruct = new Structure(this);
    globalStruct.kind = TypeKind.Namespace;
    globalStruct._moduleName = globalStruct.origDType = "global";
    globalStruct.structType = StructType.Class;
    structs ~= globalStruct;

    // Add global Types structure
    typesStruct = new Structure(this);
    typesStruct.kind = TypeKind.Namespace;
    typesStruct._moduleName = typesStruct.origDType = "types";
    typesStruct.structType = StructType.Class;
    structs ~= typesStruct;
  }

  this(Defs defs, string filename)
  {
    this(defs);
    this.filename = filename;
  }

  override @property dstring name()
  {
    return namespace;
  }

  override @property dstring dName()
  {
    return packageNamespace;
  }

  /// Convert an XML object tree to a Gir object tree
  void fromXmlTree(XmlTree tree)
  {
    void recurseXml(XmlNode node)
    {
      switch (node.id)
      {
        case "alias": // Alias info
          aliases ~= new Alias(typesStruct, node);
          break;
        case "array": // Array type info
          break; // Do nothing, TypeNode handles this
        case "attribute": // FIXME - Freeform attributes, but for which nodes?
          break;
        case "bitfield", "enumeration": // Flags and enumerations
          enums ~= new Enumeration(typesStruct, node);
          break;
        case "c:include": // C include header
          cIncludes ~= node["name"];
          break;
        case "callback": // Callback type
          if (node.parent && node.parent.id == "field")
          {
            if (auto field = baseParentFromXmlNodeWarn!Field(node))
            {
              if (!field.callback)
                field.callback = new Func(field, node);
              else
                node.warn("Field has multiple callbacks");
            }
          }
          else
            callbacks ~= new Func(typesStruct, node);
          break;
        case "class", "interface": // Classes and interfaces
          structs ~= new Structure(this, node);
          break;
        case "record", "union": // Structures and unions
          if (auto st = baseParentFromXmlNode!Structure(node))
          { // Embedded union or structure field
            st.fields ~= new Field(st, node);
            st.fields[$-1].directStruct = new Structure(this, node);
          }
          else
            structs ~= new Structure(this, node);
          break;
        case "constant": // Constants
          constants ~= new Constant(typesStruct, node);
          break;
        case "constructor": // Constructor method (class and record)
        case "function": // Function (class, enumeration, namespace, interface, record)
        case "method": // Method function (class, interface, record)
          if (auto st = node.baseParentFromXmlNode!Structure)
            st.addFunc(new Func(st, node));
          else if (auto en = node.baseParentFromXmlNode!Enumeration)
            en.functions ~= new Func(en, node);
          else
            globalStruct.addFunc(new Func(globalStruct, node));
          break;
        case "disable": // Not an actual Gir attribute, used for disabling arbitrary nodes
          break;
        case "doc": // Documentation
          if (auto base = baseParentFromXmlNodeWarn!Base(node))
          {
            base.docContent = node.content;
            base.docFilename = node.get("filename");
            base.docLine = node.get("line").to!uint;
          }
          break;
        case "doc-deprecated": // Deprecated note
          if (auto base = node.baseParentFromXmlNodeWarn!Base)
            base.docDeprecated = node.content;
          break;
        case "doc-version": // FIXME - Not sure what this is for
          if (auto base = baseParentFromXmlNodeWarn!Base(node))
            base.docVersion = node.content;
          break;
        case "docsection": // Documentation section
          docSections ~= new DocSection(this, node);
          break;
        case "field": // Field
          if (auto st = node.baseParentFromXmlNodeWarn!Structure)
            st.fields ~= new Field(st, node);
          break;
        case "function-macro": // Function macro (namespace)
          goto noRecurse; // Ignore function macros
        case "glib:boxed":
          break; // Silently ignore this seldom used node (only TreeRowData seen so far)
        case "glib:signal": // Signals (class and interface)
          if (auto cl = node.baseParentFromXmlNode!Structure)
            cl.signals ~= new Func(cl, node);
          else
            node.warn("Signal found in an unexpected location");
          break;
        case "implements": // Implemented interface
          if (auto cl = node.baseParentFromXmlNodeWarn!Structure)
            if (auto name = node.get("name"))
              cl.implements ~= name;
          break;
        case "include": // Package include
          includes ~= NamespaceVersion(node["name"], node["version"]);
          break;
        case "member": // Enumeration or bitfield member
          if (auto en = node.baseParentFromXmlNodeWarn!Enumeration)
            en.members ~= new Member(en, node);
          break;
        case "namespace": // Namespace
          namespace = node["name"];
          sharedLibrary = node.get("shared-library");
          nsVersion = node.get("version");
          identifierPrefixes = node.get("c:identifier-prefixes");
          symbolPrefixes = node.get("c:symbol-prefixes");
          break;
        case "package": // Package name
          packageName = node.get("name");
          break;
        case "parameter", "instance-parameter": // Parameter
          if (node.parent)
          {
            if (auto fn = node.parent.baseParentFromXmlNodeWarn!Func)
              fn.params ~= new Param(fn, node);
          }
          else
            node.warn("Expected node to have a parent");
          break;
        case "parameters": // Node which contains parameters
          break; // Do nothing, parameters are individually processed
        case "prerequisite": // Interface object prerequisite
          if (auto cl = node.baseParentFromXmlNodeWarn!Structure)
            if (auto name = node.get("name"))
              cl.prerequisites ~= name;
          break;
        case "property": // Class or interface property
          if (auto cl = node.baseParentFromXmlNodeWarn!Structure)
            cl.properties ~= new Property(cl, node);
          break;
        case "repository": // Toplevel repository
          repoVersion = node.get("version");
          xmlns = node.get("xmlns");
          xmlnsC = node.get("xmlns:c");
          xmlnsGlib = node.get("xmlns:glib");
          break;
        case "return-value": // Function return value info
          if (auto fn = node.baseParentFromXmlNodeWarn!Func)
            fn.returnVal = new ReturnValue(fn, node);
          break;
        case "source-position": // Source position information
          if (auto base = node.baseParentFromXmlNodeWarn!Base)
          {
            base.sourceFilename = node.get("filename");
            base.sourceLine = node.get("line").to!uint;
          }
          break;
        case "type": // Type information
          if (node.parent && node.parent.id.among("array"d, "type"d)) // Check for array or type (container) XML node parent
            if (auto parent = node.parent.baseParentFromXmlNode!TypeNode) // Get the parent of the array which contains the type information
              parent.elemTypes ~= new TypeNode(parent, node); // Add the element type to the container
          break;
        case "unsupported": // Not an actual Gir attribute, used for disabling arbitrary nodes and flagging them as currently unsupported
          break;
        case "varargs": // Varargs enable
          if (auto par = node.baseParentFromXmlNodeWarn!Param)
            par.varargs = true;
          break;
        case "virtual-method": // Virtual method (class, interface)
          goto noRecurse; // Ignore virtual methods for now (FIXME - do we want to support them?)
        default:
          static bool[dstring] unknownElements;

          if (node.id !in unknownElements)
          {
            unknownElements[node.id] = true;
            warning(cast(string)("Unknown XML element '" ~ node.id ~ "'"));
          }
          break;
      }

      foreach (child; node.children)
        recurseXml(child);

    noRecurse:
    }

    recurseXml(tree.root);
  }

  /// Fixup dependent data
  void fixup()
  {
    if (auto dubName = dubInfo.get("name", null))
      dubPackageName = dubName[0];
    else
    { // Strip .0 from versions
      dubPackageName = namespace.toLower ~ nsVersion.chomp(".0").replace(".", "-");
      dubInfo["name"] = [dubPackageName];
    }

    packageNamespace = namespace.toLower;

    if ("version" !in dubInfo && "version" in defs.dubInfo)
      dubInfo["version"] = defs.dubInfo["version"];

    if ("description" !in dubInfo)
      dubInfo["description"] ~= namespace ~ " library D binding";

    void recurseDeps(Repo r)
    {
      includeRepoHash[r.namespace] = r;

      foreach (m; r.mergedRepos) // Add merged repos to includes
        includeRepoHash[m.namespace] = m;

      foreach (d; r.includeRepos)
        recurseDeps(d);
    }

    (includeRepos ~ [this]).each!recurseDeps; // Resolve GIR includes and self recursively and add to includeRepoHash

    foreach (al; aliases) // Fixup aliases
    {
      al.fixup;
      al.resolve;
      typeObjectHash[al.name] = al;
      defs.cSymbolHash[al.origCType] = al;

      if (al.origName != al.name)
        typeObjectHash[al.origName] = al;
    }

    foreach (con; constants) // Hash constants (can reference other types, which are fixed up in fixupDeps())
    {
      con.fixup;
      con.resolve;
      typeObjectHash[con.name] = con;
      defs.cSymbolHash[con.origCType] = con;
    }

    foreach (en; enums) // Hash enums
    {
      en.fixup;
      en.resolve;
      typeObjectHash[en.dType] = en;
      defs.cSymbolHash[en.origCType] = en;

      if (en.origDType != en.dType)
        typeObjectHash[en.origDType] = en;

      foreach (m; en.members) // Add enum/flag member names
      {
        if (m.glibName) // FIXME - Is glibName more reliable?
          defs.cSymbolHash[m.cName] = m;
        else if (m.cName)
          defs.cSymbolHash[m.glibName] = m;
      }
    }

    foreach (cb; callbacks) // Hash callbacks
    {
      cb.fixup;
      cb.resolve;
      typeObjectHash[cb.name] = cb;
      defs.cSymbolHash[cb.origCType] = cb;

      if (cb.origName != cb.name)
        typeObjectHash[cb.origName] = cb;
    }

    foreach (st; structs) // Fixup structures (base type only, not dependencies which are fixed up below)
    {
      st.fixup;
      st.resolve;
      typeObjectHash[st.dType] = st;
      defs.cSymbolHash[st.origCType] = st;

      if (st.origDType != st.dType)
        typeObjectHash[st.origDType] = st;
    }

    foreach (modName, defCode; modDefCode) // Loop on structure definitions and assign to structs
    {
      auto className = !defCode.className.empty ? defCode.className : modName;
      auto st = cast(Structure)typeObjectHash.get(className, null);

      if (!st) // Create new class structures if non-existant, fixup base type, and hash
      {
        st = new Structure(this);
        st.dType = st.origDType = className;
        st.structType = StructType.Class;
        st._moduleName = modName;
        structs ~= st;
        st.fixup;
        st.resolve;
        typeObjectHash[st.name] = st;
        defs.cSymbolHash[st.origCType] = st;
      }

      st.defCode = defCode;
    }

    structs.sort!((x, y) => x.name < y.name); // Sort structures by name

    foreach (key, value; kindSubs) // Substitute type kinds
    {
      if (auto obj = typeObjectHash.get(key, null))
      {
        if (auto node = cast(TypeNode)obj)
          node.kind = value;
        else
          warning("Type '" ~ name ~ "." ~ key ~ "' kind cannot be substituted");
      }
      else
        warning("Type kind substitution '" ~ name ~ "." ~ key ~ "' not found");
    }
  }

  /// Ensure consistent state of repo data
  void verify()
  {
    if (namespace.empty)
      throw new Exception("Repo '" ~ filename ~ "' has empty namespace");

    foreach (al; aliases) // Verify aliases
    {
      if (al.active != Active.Enabled)
        continue;

      try
        al.verify;
      catch (Exception e)
      {
        al.active = Active.Unsupported;
        warnWithLoc(e.file, e.line, al.xmlLocation, "Disabling alias '" ~ al.fullName.to!string ~ "': " ~ e.msg);
        TypeNode.dumpSelectorOnWarning(al);
      }
    }

    foreach (con; constants) // Verify constants
    {
      if (con.active != Active.Enabled)
        continue;

      try
        con.verify;
      catch (Exception e)
      {
        con.active = Active.Unsupported;
        warnWithLoc(e.file, e.line, con.xmlLocation, "Disabling constant '" ~ con.fullName.to!string ~ "': " ~ e.msg);
        TypeNode.dumpSelectorOnWarning(con);
      }
    }

    foreach (cb; callbacks) // Verify callbacks
    {
      if (cb.active != Active.Enabled)
        continue;

      try
      {
        if (cb.funcType != FuncType.Callback)
          throw new Exception("Callback type '" ~ cb.funcType.to!string ~ "' not supported");

        cb.verify;
      }
      catch (Exception e)
      {
        cb.active = Active.Unsupported;
        warnWithLoc(e.file, e.line, cb.xmlLocation, "Disabling callback '" ~ cb.fullName.to!string ~ "': " ~ e.msg);
        TypeNode.dumpSelectorOnWarning(cb);
      }
    }

    foreach (st; structs) // Verify structures
    {
      if (st.active != Active.Enabled)
        continue;

      try
        st.verify;
      catch (Exception e)
      {
        st.active = Active.Unsupported;
        warnWithLoc(e.file, e.line, st.xmlLocation, "Disabling structure '" ~ st.fullName.to!string ~ "': " ~ e.msg);
        TypeNode.dumpSelectorOnWarning(st);
      }
    }
  }

  /**
   * Write repository D binding package.
   * Params:
   *   basePath = The path to the toplevel packages directory (defaults to "packages")
   */
  void writePackage(string basePath = "packages")
  {
    auto pkgName = (mergeRepo ? mergeRepo : this).dubPackageName.to!string; // Use the package of the merge namespace if merge specified
    auto packagePath = buildPath(basePath, pkgName);
    auto sourcePath = buildPath(packagePath, packageNamespace.to!string);
    auto cSourcePath = buildPath(sourcePath, "c");

    codeTrap("repo.write", namespace);

    writeCTypes(buildPath(cSourcePath, "types.d"));
    writeCFuncs(buildPath(cSourcePath, "functions.d"));

    if (typesStruct)
      writeTypesModule(buildPath(sourcePath, "types.d"));

    if (globalStruct)
      writeGlobalModule(buildPath(sourcePath, "global.d"));

    foreach (st; structs)
    {
      if (st.active == Active.Enabled && ((st.defCode && st.defCode.inClass) || st.inModule) && st !is globalStruct
        && st !is typesStruct)
      {
        st.write(sourcePath, st.kind == TypeKind.Interface ? ModuleType.IfaceTemplate : ModuleType.Normal);

        if (st.kind == TypeKind.Interface)
        {
          st.write(sourcePath, ModuleType.Iface);
          writeIfaceProxy(sourcePath, st);
        }
      }
    }

    foreach (al; aliases) // Write modules for aliases to structure types in modules
    {
      auto st = cast(Structure)al.typeObjectRoot;

      if (al.active == Active.Enabled && st && st.inModule)
      {
        auto codeWriter = new CodeWriter(buildPath(sourcePath, al.name.to!string ~ ".d"), [
          "module " ~ al.fullName ~ ";",
          "",
          "import " ~ st.fullName ~ ";",
          "",
          "alias " ~ al.name ~ " = " ~ st.name ~ ";"
        ]);

        codeWriter.write;
      }
    }

    if (!mergeRepo)
      writeDubJsonFile(buildPath(packagePath, "dub.json"));
  }

  // Write an interface proxy object (to use when a GObject has no known applicable D object binding when using the interface)
  private void writeIfaceProxy(string path, Structure st)
  {
    auto className = st.dType ~ "IfaceProxy";
    auto modName = st.moduleName ~ "_iface_proxy";
    auto writer = new CodeWriter(buildPath(path, modName.to!string ~ ".d"));
    writer ~= ["module " ~ packageNamespace ~ "." ~ modName ~ ";", "",
      "import gobject.object;",
      "import " ~ st.fullModuleName ~ ";",
      "import " ~ st.fullModuleName ~ "_mixin;", "",
      "/// Proxy object for " ~ st.fullName ~ " interface when a GObject has no applicable D binding",
      "class " ~ className ~ " : IfaceProxy, " ~ st.fullDType, "{",
      "this(void* ptr, Flag!\"Take\" take = No.Take)", "{", "super(cast(void*)ptr, take);", "}", "",
      "override TypeInfo_Interface getIface()", "{", "return typeid(" ~ st.fullDType ~ ");", "}", "",
      "mixin " ~ st.dType ~ "T!();",
      "}",
    ];

    writer.write;
  }

  /**
   * Write a dub JSON package file.
   * Params:
   *   path = Path of the file to write
   */
  private void writeDubJsonFile(string path)
  {
    string output = "{\n";

    foreach (key; ["name", "version", "description", "copyright", "authors", "license"])
    {
      if (auto val = dubInfo.get(key, defs.dubInfo.get(key, null))) // Fall back to master dub info
      {
        if (key == "authors")
          output ~= `  "authors": [` ~ val.map!(x => `"` ~ x.to!string ~ `"`).join(", ") ~ "],\n";
        else
          output ~= `  "` ~ key.to!string ~ `": "` ~ val[0].to!string ~ "\",\n";
      }
    }

    output ~= `  "targetType": "library",` ~ "\n";
    output ~= `  "importPaths": [".", ".."],` ~ "\n";

    // Include merged repos in sourcePaths list
    output ~= `  "sourcePaths": [` ~ ([this] ~ mergedRepos).map!(x => '"' ~ x.packageNamespace.to!string ~ '"').array
      .join(", ") ~ `]`;

    if (!includeRepos.empty)
    {
      auto deps = includeRepos.map!(x => x.mergeRepo ? x.mergeRepo : x).assocArray(true.repeat).keys; // Consider merge repos and deduplicate
      output ~= ",\n  \"dependencies\": {\n" ~ deps.map!(repo => `    "gid:` // Construct package dependencies with any specified versions from `info` definition commands
        ~ repo.dubPackageName.to!string ~ `": "` ~ repo.dubInfo.get("version", ["*"])[0].to!string
        ~ `"`).array.sort.join(",\n") ~ "\n  }";
    }

    output ~= "\n}\n";
    write(path, output);
  }

  /**
   * Write package D binding C types file.
   * Params:
   *   path = Path to D binding C types file to write
   */
  private void writeCTypes(string path)
  {
    auto writer = new CodeWriter(path);

    writer ~= ["module " ~ packageNamespace ~ ".c.types;", ""];
    writer ~= "public import gid.basictypes;"; // Imported for glong/gulong types which change size depending on Windows or not
    writer ~= includes.map!(x => "public import " ~ x.name.toLower ~ ".c.types;\n").array;
    writer ~= "import " ~ packageNamespace ~ ".types;";
    writer ~= "";

    foreach (a; aliases)
    {
      writer ~= a.genDocs;
      writer ~= ["alias " ~ a.cName ~ " = " ~ a.cType ~ ";", ""];
    }

    foreach (i, en; enums.filter!(x => x.active == Active.Enabled).enumerate) // Write out enum aliases
      writer ~= (i == 0 ? [""d, "// Enums"] : []) ~ ["alias " ~ en.cType ~ " = " ~ en.fullDType ~ ";"];

    foreach (st; structs)
    {
      codeTrap("ctypes.struct", st.fullName);

      if (st.kind == TypeKind.Namespace || st.cType.empty)
        continue;

      if (st.fields.length > 0 && !st.opaque && !st.pointer) // Regular structure?
        st.writeCStruct(writer);
      else if (st.pointer)
      {
        writer ~= st.genDocs;
        writer ~= ["alias " ~ st.cType ~ " = " ~ st.cType ~ "_st*;", ""];
        writer ~= ["struct " ~ st.cType ~ "_st;", ""];
      }
      else // Opaque structure or pointer to opaque structure
      {
        writer ~= st.genDocs;
        writer ~= ["struct " ~ st.cType ~ ";"d, ""];
      }
    }

    foreach (cb; callbacks)
      writer ~= ["alias extern(C) " ~ cb.getCPrototype ~ " " ~ cb.cName ~ ";", ""];

    writer.write();
  }

  /**
   * Write the functions.d for the package which contains the C function definitions and dynamic loading.
   * Params:
   *   path = Path to the functions.d file to write
   */
  private void writeCFuncs(string path)
  {
    auto writer = new CodeWriter(path);

    writer ~= ["module " ~ packageNamespace ~ ".c.functions;", ""];
    writer ~= ["public import gid.basictypes;", "import gid.loader;", "import " ~ packageNamespace ~ ".c.types;"]; // Import gid.basictypes for glong/gulong types which change size depending on Windows or not

    auto importNames = includes.map!(x => x.name).array;

    if (namespace == "GLib") // HACK - Add GObject to includes for GLib for GType
      importNames ~= "GObject";

    writer ~= importNames.sort.map!(x => "public import " ~ x.toLower ~ ".c.types;\n").array;
    writer ~= "";

    writeSharedLibs(writer);

    writer ~= ["__gshared extern(C)", "{"];

    foreach (st; structs)
    {
      auto preamble = ["", "// " ~ st.dType];

      if (writer.lines[$ - 1] == "{")
        preamble = preamble[1 .. $];

      if (!st.glibGetType.empty)
      { // Write GType function if set
        writer ~= preamble ~ ["extern(C) GType function() c_" ~ st.glibGetType ~ ";"];
        preamble = null;
      }

      foreach (f; st.functions)
      {
        if (f.movedTo || !f.funcType.among(FuncType.Function, FuncType.Constructor, FuncType.Method))
          continue;

        writer ~= preamble ~ [f.getCPrototype ~ " c_" ~ f.cName ~ ";"];
        preamble = null;
      }
    }

    writer ~= ["}"];

    foreach (st; structs)
    {
      auto preamble = ["", "// " ~ st.name];

      if (st && !st.glibGetType.empty)
      { // Write GType function if set
        writer ~= preamble ~ ["alias " ~ st.glibGetType ~ " = c_" ~ st.glibGetType ~ ";"];
        preamble = null;
      }

      foreach (f; st.functions)
      {
        if (f.movedTo || !f.funcType.among(FuncType.Function, FuncType.Constructor, FuncType.Method))
          continue;

        writer ~= preamble ~ ["alias " ~ f.cName ~ " = c_" ~ f.cName ~ ";"];
        preamble = null;
      }
    }

    writer ~= ["", "shared static this()", "{"];

    foreach (st; structs)
    {
      auto preamble = ["", "// " ~ st.name];

      if (writer.lines[$ - 1] == "{")
        preamble = preamble[1 .. $];

      if (st && !st.glibGetType.empty)
      { // Write GType function if set
        writer ~= preamble ~ ["gidLink(cast(void**)&" ~ st.glibGetType ~ ", \"" ~ st.glibGetType ~ "\", LIBS);"];
        preamble = null;
      }

      foreach (f; st.functions)
      {
        if (f.movedTo || !f.funcType.among(FuncType.Function, FuncType.Constructor, FuncType.Method))
          continue;

        writer ~= preamble ~ ["gidLink(cast(void**)&" ~ f.cName ~ ", \"" ~ f.cName ~ "\", LIBS);"];
        preamble = null;
      }
    }

    writer ~= ["}"];

    writer.write();
  }

  private void writeSharedLibs(CodeWriter writer)
  {
    dstring[] winLibs, osxLibs, posixLibs;

    foreach (lib; sharedLibrary.split(',')) // Multiple libraries separated by commas
    { // Example "libatk-1.0.so.0"
      auto t = lib.split(".so."); // Split example into "libatk-1.0" and "0"
      auto t2 = t[0].split("."); // Split "libatk-1.0" into "libatk-1" and "0"

      // libatk-1.0-0.dll;atk-1.0-0.dll;atk-1.dll
      winLibs ~= "\"" ~ t[0] ~ "-" ~ t[1] ~ ".dll;" ~ t[0][3 .. $] ~ "-" ~ t[1] ~ ".dll;" ~ t2[0][3 .. $] ~ ".dll\"";
      osxLibs ~= "\"" ~ t[0] ~ "." ~ t[1] ~ ".dylib\""; // libatk-1.0.0.dylib
      posixLibs ~= "\"" ~ lib ~ "\""; // libatk-1.0.so.0
    }

    writer ~= [
      "version(Windows)",
      "private immutable LIBS = [" ~ winLibs.join(", ") ~ "];",
      "else version(OSX)",
      "private immutable LIBS = [" ~ osxLibs.join(", ") ~ "];",
      "else",
      "private immutable LIBS = [" ~ posixLibs.join(", ") ~ "];",
      ""
    ];
  }

  /**
   * Write the global types module for a package.
   * Params:
   *   path = Path to the file to write the global module to
   */
  private void writeTypesModule(string path)
  {
    auto writer = new CodeWriter(path);
    writer ~= ["module " ~ packageNamespace ~ ".types;", ""];
    beginImports(typesStruct);
    scope(exit) endImports;

    dstring[] callbackDecls;

    foreach (i, cb; callbacks.filter!(x => x.active == Active.Enabled).enumerate) // Generate callback prototypes (to populate imports), added to writer output below
      callbackDecls ~= (i == 0 ? [""d, "// Callbacks"] : []) ~ ["", cb.genDocs, cb.getDelegPrototype];

    dstring[] aliasDecls;

    foreach (i, al; aliases.filter!(x => x.active == Active.Enabled).enumerate) // Generate aliases without writing them (to populate import manager)
    {
      auto st = cast(Structure)al.typeObjectRoot;

      if ((al.typeObjectRoot && al.typeObjectRoot.active != Active.Enabled) || (st && st.inModule)) // Skip if target type is disabled or an alias of a type in a module (alias is written as a module in writePackage())
        continue;

      if (al.kind == TypeKind.Callback) // Callback aliases should alias to D callback delegates, not C functions
        aliasDecls ~= ["", "/** */", "alias " ~ al.name ~ " = " ~ al.fullDType ~ ";"];
      else if (al.name == al.cName)
        aliasDecls ~= ["", "/** */", "alias " ~ al.name ~ " = " ~ packageNamespace ~ ".c.types." ~ al.cName ~ ";"];
      else
      {
        auto aliasType = al.typeRepo.typeObjectHash.get(al._dType, null);

        if (aliasType && aliasType._dType == al._dType)
          aliasDecls ~= ["", "/** */", "alias " ~ al.name ~ " = " ~ aliasType.fullDType ~ ";"];
        else
          aliasDecls ~= ["", "/** */", "alias " ~ al.name ~ " = " ~ al.cName ~ ";"];
      }
    }

    if (importManager.write(writer))
      writer ~= "";

    if (!aliasDecls.empty)
      writer ~= [""d, "// Aliases"];

    writer ~= aliasDecls; // Write the aliases

    // Filter out structures that aren't enabled, have their own module, or whose D types match the C type name (no prefix)
    auto simpleStructs = structs.filter!(x => x.active == Active.Enabled && !x.inModule && x.name != x.cType).enumerate;

    foreach (i, st; simpleStructs) // Write out simple struct aliases (not classes)
    {
      if (i == 0)
        writer ~= [""d, "// Structs"];

      writer ~= ["", "/** */", "alias " ~ st.name ~ " = " ~ st.cType ~ (st.kind == TypeKind.Pointer
        && !st.pointer ? "*"d : "") ~ ";"];
    }

    writer ~= callbackDecls;

    foreach (e; enums)
    {
      writer ~= "";
      writer ~= e.genDocs;
      writer ~= ["enum " ~ e.dType ~ (e.bitfield ? " : uint"d : ""), "{"];

      foreach (m; e.members)
      {
        if (m.active == Active.Enabled)
        {
          if (writer.lines[$ - 1] != "{")
            writer ~= "";

          writer ~= m.genDocs;
          writer ~= m.dName ~ " = " ~ m.value ~ ",";
        }
      }

      writer ~= ["}"];
    }

    foreach (i, con; constants.filter!(x => x.active == Active.Enabled).enumerate) // Write out constants
    {
      writer ~= "";
      writer ~= con.genDocs;
      writer ~= ["enum " ~ con.name ~ (con.kind == TypeKind.String ? (" = \"" ~ con.value ~ "\";")
        : (" = " ~ con.value ~ ";"))];
    }

    if (typesStruct.defCode.preClass.length > 0)
      writer ~= typesStruct.defCode.preClass;

    if (typesStruct.defCode.inClass.length > 0)
      writer ~= typesStruct.defCode.inClass;

    writer.write();
  }

  /**
   * Write the global module for a package containing global functions.
   * Params:
   *   path = Path to the file to write the global module to
   */
  private void writeGlobalModule(string path)
  {
    auto writer = new CodeWriter(path);

    writer ~= ["module " ~ packageNamespace ~ ".global;", ""];

    // Create the function writers first to construct the imports
    beginImports(globalStruct);
    scope(exit) endImports;

    FuncWriter[] funcWriters;

    foreach (fn; globalStruct.functions)
    {
      if (fn.active != Active.Enabled)
        continue;

      funcWriters ~= new FuncWriter(fn);
    }

    if (importManager.write(writer))
      writer ~= "";

    if (globalStruct.defCode.preClass.length > 0)
      writer ~= globalStruct.defCode.preClass;

    if (globalStruct.defCode.inClass.length > 0)
      writer ~= globalStruct.defCode.inClass;

    foreach (fnWriter; funcWriters) // Write out functions
    {
      writer ~= "";
      fnWriter.write(writer);
    }

    if (globalStruct.defCode.postClass.length > 0)
      writer ~= globalStruct.defCode.postClass;

    writer.write();
  }

  /**
   * Get the kind classification of a type string
   * Params:
   *   type = The D type string (should not include a namespace)
   * Returns: The type classification, falls back to TypeKind.Basic if no other type applies
   */
  TypeKind typeKind(dstring type)
  {
    foreach (i; 0 .. 4) // Resolve up to 4 alias dereferences
    {
      if (type.among("char*"d, "const char*"d, "string"d, "utf8"d))
        return TypeKind.String;

      if (type.isBasicType)
        return i > 0 ? TypeKind.BasicAlias : TypeKind.Basic;

      if (auto obj = typeObjectHash.get(type, null))
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
   * Returns: The matching type object or null if not found (possible basic type), throws an exception if typeName has a namespace that wasn't resolved
   */
  TypeNode findTypeObject(dstring typeName)
  {
    repo = this;

    auto t = typeName.split('.');
    if (t.length > 1)
    {
      repo = includeRepoHash.get(t[0], null);

      if (!repo)
        throw new Exception("Failed to resolve namespace '" ~ t[0].to!string ~ "' for type '"
          ~ typeName.to!string ~ "'");

      typeName = t[1];
    }

    return repo.typeObjectHash.get(repo.subTypeStr(typeName), null);
  }

  /**
   * Find type object by Gir doc reference (as found in Gir documentation).
   * In the form [kind@Namespace.TypeName(.|::|:)SubTypeName] where SubTypeName is optional.
   * Params:
   *   refStr = Type name string
   * Returns: The matching type object or null if not found (possible basic type)
   */
  TypeNode findTypeObjectByGDocRef(dstring refStr)
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

    auto repo = this;

    if (!nameSpace.empty)
    {
      if (auto r = includeRepoHash.get(nameSpace, null))
        repo = r;
      else // Namespace did not match, assume it is TypeName.SubTypeName without Namespace
      {
        subTypeName = typeName;
        typeName = nameSpace;
      }
    }

    typeName = subTypeStr(typeName);
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
  * Returns: The DDoc formatted string
  */
  dstring gdocToDDoc(dstring s, dstring prefix)
  {
    import std.regex : Captures, ctRegex, replaceAll;
    auto nlRe = ctRegex!(r"\n"d);
    auto refRe = ctRegex!(r"\[`?([a-z]+@[^\]]+)`?\]"d);
    auto funcRe = ctRegex!(r"([a-z0-9_]+)\(\)"d);
    auto backtickRe = ctRegex!(r"`([A-Za-z0-9_]+)`"d);
    auto constRe = ctRegex!(r"%([A-Za-z0-9_]+)"d);
    auto oldCodeBlockRe = ctRegex!("\\|\\[(?:<!-- +language=\"([^\"]+)\" +-->)?(.*?)\\]\\|"d, "s");

    dstring codeBlockReplace(Captures!dstring m) // Replace |[<!-- language="LANG" -->  ]| code blocks with triple backticks
    {
      return "```" ~ m[1].toLower ~ m[2] ~ "```";
    }

    dstring refReplace(Captures!dstring m) // Replace [kind@name] with DDoc reference
    {
      if (auto tn = findTypeObjectByGDocRef(m[1]))
        return "[" ~ tn.fullDName ~ "]";
      else
        return "`" ~ m[1] ~ "`";
    }

    dstring funcOrBacktickReplace(Captures!dstring m) // Replace func() or `backtick` references with links, or keep it as a backtick if not resolved
    {
      if (auto tn = defs.cSymbolHash.get(m[1], null))
        return "[" ~ tn.fullDName ~ "]";
      else
        return m[0];
    }

    dstring constReplace(Captures!dstring m) // Replace %CONST symbol references
    {
      auto lcMatch = m[1].toLower;

      if (lcMatch.among("null"d, "true"d, "false"d))
        return lcMatch;

      if (auto tn = defs.cSymbolHash.get(m[1], null))
        return "[" ~ tn.fullDName ~ "]";
      else
        return "`" ~ m[1] ~ "`";
    }

    s = replaceAll!codeBlockReplace(s, oldCodeBlockRe); // replace old code blocks with triple backticks
    s = s.markdownListToDDoc; // Replace markdown lists with adrdox lists

    auto lines = s.split("\n");
    bool inCodeBlock;

    foreach (ref line; lines) // Make sure other replacements don't occur inside of code blocks
    {
      if (line.stripLeft.startsWith("```"))
        inCodeBlock = !inCodeBlock;

      if (!inCodeBlock)
      {
        line = replaceAll!refReplace(line, refRe);
        line = replaceAll!funcOrBacktickReplace(line, funcRe);
        line = replaceAll!funcOrBacktickReplace(line, backtickRe);
        line = replaceAll!constReplace(line, constRe);
      }

      line = prefix ~ line;
    }

    return lines.join("\n");
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
      importManager.add(typeNode.fullModuleName);

    return typeNode.dType;
  }

  /**
   * Substitute type.
   * Params:
   *   type = Type string
   *   cType = No.CType for D type substitution, Yes.CType for C type (defaults to No)
   * Returns: Type string with any relevant substitutions
   */
  dstring subTypeStr(dstring type, Flag!"CType" cType = No.CType)
  {
    auto subs = (cType == No.CType) ? defs.dTypeSubs : defs.cTypeSubs;
    auto localSubs = (cType == No.CType) ? dTypeSubs : cTypeSubs;

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

  Defs defs; /// Defs loaded from def files
  string defsFilename; /// Defs filename responsible for loading this Repo object

  string filename; /// Gir filename
  dstring packageName; /// Gir package name
  dstring repoVersion; /// Gir repository version (usually, if not always, 1.2)

  dstring namespace; /// Name space of symbols in gir file
  dstring nsVersion; /// Version of the namespace
  dstring sharedLibrary; /// Namespace shared library (multiple values separated by commas)
  dstring identifierPrefixes; /// Prefix to identifiers
  dstring symbolPrefixes; /// C symbol prefix

  dstring dubPackageName; /// Dub package name
  dstring packageNamespace; /// Namespace directory to use for dub package (usually just namespace.toLower)

  Alias[] aliases; /// Aliases
  Constant[] constants; /// Constants
  Enumeration[] enums; /// Enumerations and bitfields
  Func[] callbacks; /// Callback function types
  Structure[] structs; /// Structures
  Structure globalStruct; /// Global namespace structure
  Structure typesStruct; /// Global Types module structure
  NamespaceVersion[] includes; /// Package includes
  Repo[] includeRepos; /// Resolved included repos
  Repo[dstring] includeRepoHash; /// Included repository dependencies keyed by namespace
  dstring[] cIncludes; /// C header includes
  DocSection[] docSections; /// Documentation sections

  XmlPatch[] patches; /// XML patches specified in definitions file
  dstring[dstring] cTypeSubs; /// C type substitutions defined in the definitions file
  dstring[dstring] dTypeSubs; /// D type substitutions defined in the definitions file
  TypeKind[dstring] kindSubs; /// Type kind substitutions defined in the definitions file
  DefCode[dstring] modDefCode; /// Code defined in definition file for classes (keyed by module name)
  NamespaceVersion mergeNsVer; /// Package namespace/version to merge this repo into
  Repo mergeRepo; /// Repo object to merge this repo into
  Repo[] mergedRepos; /// Repos which have been merged into this one
  dstring[][string] dubInfo; /// Dub JSON file info ("name", "description", "copyright", "authors", "license"), only "authors" uses multiple values

  TypeNode[dstring] typeObjectHash; /// Hash of type objects by name (Alias, Func (callback), Constant, Enumeration, or Structure)

  dstring xmlns;
  dstring xmlnsC;
  dstring xmlnsGlib;

  static bool logGirLoc; /// Log GIR locations in warnings
  static bool logCodeLoc; /// Log code locations in warnings
  static bool dumpCTypes; /// Set to true to dump C types
  static bool dumpDTypes; /// Set to true to dump D types
  static bool suggestDefCmds; /// Output definition command suggestions
  string[][string] suggestions; /// Suggested definitions (if suggestDefCmds is enabled), keyed by type of suggestion
}

/**
 * Log warning with location information.
 */
void warnWithLoc(string file, size_t line, string xmlLoc, string message)
{
  warning((Repo.logCodeLoc ? (file ~ ":" ~ line.to!string ~ " ") : "") ~ (Repo.logGirLoc ? (xmlLoc ~ " ") : "") ~ message);
}

/**
 * Log info message with location information.
 */
void infoWithLoc(string file, size_t line, string xmlLoc, string message)
{
  info((Repo.logCodeLoc ? (file ~ ":" ~ line.to!string ~ " ") : "") ~ (Repo.logGirLoc ? (xmlLoc ~ " ") : "") ~ message);
}

/// Package include
struct NamespaceVersion
{
  dstring name;
  dstring version_;
}

/// Documentation section
final class DocSection : Base
{
  this(Base parent, XmlNode node)
  {
    super(parent);
    fromXml(node);
  }

  override void fromXml(XmlNode node)
  {
    super.fromXml(node);
    name = node.get("name");
  }

  dstring name; /// Name of doc section
}
