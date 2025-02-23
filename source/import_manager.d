module import_manager;

import code_writer;
import std_includes;
import utils;

import gir.alias_;
import gir.func;
import gir.structure;
import gir.type_node;

final class ImportManager
{
  this(Structure klassModule)
  {
    this.klassModule = klassModule;
    this.defaultNamespace = klassModule.repo.namespace;
    this.symbolNames = [klassModule.name : klassModule];
  }

  this(ImportManager imSyms, Structure klassModule)
  {
    this(klassModule);
    merge(imSyms);
  }

  this(dstring defaultNamespace = null)
  {
    this.defaultNamespace = defaultNamespace;
  }

  this(ImportManager imSyms, dstring defaultNamespace = null)
  {
    this(defaultNamespace);
    merge(imSyms);
  }

  /**
   * Add import module as a string with an optional symbol to an import symbols object. Should only be used if there is no associated Structure object for the import.
   * Params:
   *   mod = The import module name (default namespace is used if not present)
   *   symbol = Optional symbol in the module to import
   */
  void add(dstring mod, dstring symbol = null)
  {
    if (symbol)
      add(mod, [symbol]);
    else
      add(mod, cast(dstring[])[]);
  }

  /**
   * Add import module as a string with symbols to an import symbols object. Should only be used if there is no associated Structure object for the import.
   * Params:
   *   mod = The import module name in the form namespace.module (if namespace is not supplied the supplied default is used)
   *   symbols = Array of symbols to add (empty to indicate all symbols wildcard)
   */
  void add(dstring mod, dstring[] symbols)
  {
    codeTrap("import.add", mod);

    if (!mod.canFind('.') && defaultNamespace)
      mod = defaultNamespace ~ "." ~ mod;

    if (mod !in stringImports) // Module name doesn't already exist?
      stringImports[mod] = symbols.map!(x => tuple(x, true)).assocArray; // Add module and symbols (can be empty)
    else if (!stringImports[mod].empty) // Module symbol array not wildcard?
    {
      if (!symbols.empty)
      {
        foreach (sym; symbols)
          stringImports[mod][sym] = true; // Add symbol to symbol map
      }
      else
        stringImports[mod].clear; // All symbols requested, empty the symbol array
    } // Module import is all symbols, doesn't matter if symbols were specified or not, will still be wildcard
  }

  /**
   * Remove a type node symbol from an import manager object.
   * Params:
   *   typeNode = Type node object to remove
   * Returns: true if removed, false if no match was found
   */
  bool remove(TypeNode typeNode)
  {
    if (typeNode !in symbolAliases)
      return false;

    symbolAliases.remove(typeNode);

    foreach (name, cmpTypeNode; symbolNames)
      if (cmpTypeNode == typeNode)
        symbolNames.remove(name);

    return true;
  }

  /**
   * Remove an import module by string or import module symbol from an import object.
   * Params:
   *   mod = The import module name in the form namespace.module (if namespace is not supplied the supplied default is used)
   *   symbol = The symbol to remove or null (default) to remove all symbols
   * Returns: true if removed, false if no match was found
   */
  bool remove(dstring mod, dstring symbol = null)
  {
    if (!mod.canFind('.') && defaultNamespace)
      mod = defaultNamespace ~ "." ~ mod;

    if (mod !in stringImports)
      return false;

    if (!symbol.empty)
    {
      if (symbol !in stringImports[mod])
        return false;

      stringImports[mod].remove(symbol);
      return true;
    }

    stringImports.remove(mod);
    return true;
  }

  /**
   * Merge import symbols from another module symbol array.
   * Params:
   *   mergeSyms = Other module symbols to merge into this module symbol array
   */
  void merge(ImportManager mergeSyms)
  {
    foreach (st, aliasName; mergeSyms.symbolAliases) // Add structure imports from other ImportManager object
      resolveDType(st);

    foreach (s, syms; mergeSyms.stringImports) // Add string imports from other ImportManager object
      add(s, syms.keys);
  }

  /**
   * Parse an import statement and add it to the import symbol array.
   * Params:
   *   statement = Import statement to parse
   */
  void parseImport(dstring statement)
  {
    auto tokens = statement.strip.stripRight(";").split;

    if (tokens.length >= 2 && tokens[0] == "import")
    {
      if (tokens.length > 3 && tokens[2] == ":")
        add(tokens[1], tokens[3 .. $].map!(x => x.strip(",")).array);
      else
        add(tokens[1]);
    }
    else
      throw new Exception("Invalid import statement '" ~ statement.to!string ~ "'");
  }

  /**
   * Resolve a symbol name for the given type node object. Uses an alias if it conflicts with another symbol or the D type otherwise.
   * Params:
   *   typeNode = Type node object to get D type of or an alias if it conflicts with another one used in the same module
   * Returns: The D type or an alias if it conflicts with another symbol in the module with the same name which was already used.
   */
  dstring resolveDType(TypeNode typeNode)
  {
    if (typeNode.containerType != ContainerType.None)
    {
      foreach (elem; typeNode.elemTypes) // Add imports for each of the container types
        resolveDType(elem);

      return typeNode._dType; // FIXME - Need to handle container element type aliases
    }

    auto refObject = typeNode;
    if (typeNode.typeObject && !cast(Alias)typeNode) // Use referenced type object (unless this is an alias, which should be imported)
      refObject = typeNode.typeObject;

    if (refObject is klassModule || (!refObject.inModule && !refObject.inGlobal))
      return typeNode._dType;

    if (auto pAliasName = refObject in symbolAliases)
      return (*pAliasName).empty ? typeNode._dType : *pAliasName;

    codeTrap("import.resolve", refObject.repo.namespace ~ "." ~ typeNode._dType);

    dstring name;

    if (typeNode._dType in symbolNames) // Check for symbol name conflicts
      name = "D" ~ refObject.repo.namespace ~ typeNode._dType; // Create an alias to use in local module
    else
    {
      symbolNames[typeNode._dType] = refObject;

      if (refObject.inGlobal) // Add repos to global Types repo hash if this is a global symbol
        typeRepos[refObject.repo] = true;
    }

    symbolAliases[refObject] = name;

    return name ? name : typeNode._dType;
  }

  /**
   * Generate the import commands for the import symbol object.
   * Params:
   *   prefix = A prefix to add to each import line (defaults to empty string)
   */
  dstring[] generate(dstring prefix = null)
  {
    bool[dstring] importHash;

    foreach (typeNode, aliasName; symbolAliases)
    {
      auto moduleName = typeNode.repo.packageNamespace;
      auto st = cast(Structure)typeNode;
      moduleName ~= (st && typeNode.inModule) ? ("." ~ st.moduleName) : ".types";

      if (aliasName.empty && typeNode.inGlobal)
      { // Check if there are any other conflicting global symbols from other repos and add an explicit alias if so
        foreach (repo; typeRepos.keys)
        {
          if (typeNode.repo != repo && typeNode._dType in repo.typeObjectHash)
          {
            aliasName = typeNode._dType;
            break;
          }
        }
      }

      if (!aliasName.empty)
      {
        importHash[moduleName ~ " : " ~ aliasName ~ " = " ~ typeNode._dType] = true;

        if (typeNode.kind == TypeKind.Interface)
          importHash[moduleName ~ "_mixin : " ~ aliasName ~ "T = " ~ typeNode._dType ~ "T"] = true;
      }
      else
      {
        importHash[moduleName] = true;

        if (typeNode.kind == TypeKind.Interface)
          importHash[moduleName ~ "_mixin"] = true;
      }
    }

    foreach (mod, syms; stringImports)
    {
      if (!syms.empty)
        importHash[mod.toLower ~ " : "d ~ syms.keys.join(", ")] = true;
      else
        importHash[mod.toLower] = true;
    }

    if (klassModule)
      importHash.remove(klassModule.fullName); // Remove import of ourself

    return importHash.keys.sort.map!(x => prefix ~ "import " ~ x ~ ";").array;
  }

  /**
   * Write import statements to a code writer.
   * Params:
   *   writer = The code writer to write to
   *   prefix = A prefix to add to each import line (defaults to empty string)
   * Returns: true if any imports lines were written, false if empty
   */
  bool write(CodeWriter writer, dstring prefix = null)
  {
    auto importLines = generate(prefix);
    writer ~= importLines;
    return !importLines.empty;
  }

private:
  immutable dstring[] notClassMods = [".Types", ".Global", ".c.types", ".c.functions"]; // Modules which aren't class modules (matches end of module name)

  Structure klassModule; /// The current class module or null if not a class module
  dstring[TypeNode] symbolAliases; /// TypeNode => Alias (or null if no alias)
  TypeNode[dstring] symbolNames; /// Hash of SymbolName -> TypeNode to detect conflicts
  bool[Repo] typeRepos; /// Repo objects which have imported symbols from their Types.d (for global Types.d conflict detection)

  bool[dstring][dstring] stringImports; /// moduleName => (Symbol => true)
  dstring defaultNamespace; /// Default namespace to use if not provided when adding imports
}
