module import_manager;

import code_writer;
import std_includes;
import utils;

import gir.structure;
import gir.type_node;

// Global import manager instance for use with beginImports/endImports
ImportManager importManager;

/**
  * Designate the start of output processing for module imports. Enables tracking of symbol names and
  * creation of aliases for conflicts for used symbols.
  * Params:
  *   klassModule = The klass or module being processed
  */
void beginImports(Structure klassModule)
{
  assert(!importManager);
  importManager = new ImportManager(klassModule);
  importManager.add("gid.gid");
  importManager.add("types");
  importManager.add(klassModule.repo.packageNamespace ~ ".c.functions");
  importManager.add(klassModule.repo.packageNamespace ~ ".c.types");
}

/**
  * Indicates the end of processing for the current output module imports which was activated with beginImports().
  */
void endImports()
{
  assert(importManager);
  importManager = null;
}

final class ImportManager
{
  this(Structure klassModule)
  {
    this.classFullModuleName = klassModule.fullModuleName;
    this.defaultNamespace = klassModule.repo.packageNamespace;
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

    if (classFullModuleName && mod == classFullModuleName) // Don't import self
      return;

    if (mod !in importHash) // Module name doesn't already exist?
      importHash[mod] = symbols.map!(x => tuple(x, true)).assocArray; // Add module and symbols (can be empty)
    else if (!importHash[mod].empty) // Module symbol array not wildcard?
    {
      if (!symbols.empty)
      {
        foreach (sym; symbols)
          importHash[mod][sym] = true; // Add symbol to symbol map
      }
      else
        importHash[mod].clear; // All symbols requested, empty the symbol array
    } // Module import is all symbols, doesn't matter if symbols were specified or not, will still be wildcard
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

    if (mod !in importHash)
      return false;

    if (!symbol.empty)
    {
      if (symbol !in importHash[mod])
        return false;

      importHash[mod].remove(symbol);
      return true;
    }

    importHash.remove(mod);
    return true;
  }

  /**
   * Merge import symbols from another module symbol array.
   * Params:
   *   mergeSyms = Other module symbols to merge into this module symbol array
   */
  void merge(ImportManager mergeSyms)
  {
    foreach (s, syms; mergeSyms.importHash) // Add string imports from other ImportManager object
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
   * Generate the import commands for the import symbol object.
   * Params:
   *   prefix = A prefix to add to each import line (defaults to empty string)
   * Returns: Array of import statements
   */
  dstring[] generate(dstring prefix = null)
  { // Construct import statements which can be a module name or a module name with one or more symbols
    return importHash.byPair.map!(pair => prefix ~ "import " ~ pair.key ~ (pair.value ? (" : " ~ pair.value.keys
      .join(", ") ~ ";") : ";")).array.sort.array;
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
  immutable dstring[] notClassMods = [".types", ".global", ".c.types", ".c.functions"]; // Modules which aren't class modules (matches end of module name)

  dstring classFullModuleName; /// The current class module name
  bool[dstring][dstring] importHash; /// moduleName => (Symbol => true)
  dstring defaultNamespace; /// Default namespace to use if not provided when adding imports
}
