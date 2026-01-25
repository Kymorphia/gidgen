module gir.enumeration;

import code_writer;
import gir.base;
import gir.func;
import gir.func_writer;
import gir.member;
import gir.type_node;
import import_manager;
import utils;

/// Enumeration or bitfield flags
final class Enumeration : TypeNode
{
  this(Base parent, XmlNode node)
  {
    super(parent);
    fromXml(node);
  }

  override @property dstring name()
  {
    return dType;
  }

  /// Get the fully qualified namespace structure for Enumeration types which have functions
  @property dstring fullNamespaceStruct()
  {
    return repo.packageNamespace ~ "." ~ moduleName ~ "." ~ dType;
  }

  override void fromXml(XmlNode node)
  {
    super.fromXml(node);

    bitfield = node.id == "bitfield";

    glibGetType = node.get("glib:get-type");
    glibTypeName = node.get("glib:type-name");
    glibErrorDomain = node.get("glib:error-domain");
    version_ = node.get("version");
  }

  /**
   * Add a function to an enumeration.
   * Params:
   *   func = The function to add
   */
  void addFunc(Func func)
  {
    functions ~= func;
    funcNameHash[func.name] = func;
  }

  protected override void fixup()
  {
    kind = bitfield ? TypeKind.Flags : TypeKind.Enum;
    super.fixup;

    Member[dstring] dupCheck; // Duplicate member check

    foreach (m; members)
    {
      m.doFixup;

      if (auto dup = m.dName in dupCheck)
      {
        m.active = Active.Ignored;
        infoWithLoc(__FILE__, __LINE__, dup.xmlLocation, "Ignoring duplicate enum member '" ~ m.fullDName.to!string ~ "'");
        continue;
      }

      dupCheck[m.dName] = m;
    }

    foreach (fn; functions)
    {
      fn.doFixup;

      if (!fn.shadows.empty)
        fn.shadowsFunc = funcNameHash.get(fn.shadows, null);

      if (!fn.shadowedBy.empty)
        fn.shadowedByFunc = funcNameHash.get(fn.shadowedBy, null);

      if (fn.funcType == FuncType.Function && fn.returnVal.dType == "Quark" && fn.cName.endsWith("error_quark"))
        errorQuarks ~= fn; // Add exception error quark functions to array

      repo.defs.cSymbolHash[fn.cName] = fn; // Add to global C symbol hash
    }

    if (functions.length > 0)
      moduleName = repo.defs.symbolName(origDType.snakeCase); // Module name if enum contains functions
  }

  protected override void resolve()
  {
    super.resolve;

    foreach (fn; functions) // Resolve enumeration functions
      fn.doResolve;
  }

  protected override void verify()
  {
    super.verify;

    foreach (fn; functions) // Verify structure function/methods
    {
      if (fn.active != Active.Enabled)
        continue;

      if (!fn.funcType != FuncType.Function)
      {
        fn.active = Active.Unsupported;
        warnWithLoc(__FILE__, __LINE__, fn.xmlLocation, "Disabling function '" ~ fn.fullDName.to!string ~ "' of type '"
          ~ fn.funcType.to!string ~ "' which is not supported");
        TypeNode.dumpSelectorOnWarning(fn);
      }
      else
        fn.doVerify;
    }
  }

  /**
   * Write enumeration/bitfield module.
   * Params:
   *   path = Directory to store the module to
   */
  void write(string path)
  {
    codeTrap("enum.write", fullNamespaceStruct);

    auto writer = new CodeWriter(buildPath(path, moduleName.to!string ~ ".d"));

    if (dType)
      writer ~= "/// Module for [" ~ dType ~ "] " ~ (bitfield ? "flags"d : "enum"d) ~ " namespace";

    writer ~= ["module " ~ repo.packageNamespace ~ "." ~ moduleName ~ ";"d, ""];

    beginImports(repo.packageNamespace ~ "." ~ moduleName, repo.packageNamespace);
    scope(exit) endImports;

    importManager.add(repo.packageNamespace ~ ".types"); // Include the types module of the enum's repo

    if (!errorQuarks.empty)
    {
      importManager.add("glib.types");
      importManager.add("glib.error");
    }

    auto importLine = writer.length; // Save position where imports should go, which are inserted later, to gather all dependencies

    writer ~= ["/// Namespace for [" ~ dType ~ "] " ~ (bitfield ? "flags"d : "enum"d), "struct " ~ dType, "{",
      "alias Enum = " ~ fullDType ~ "; ///"];

    foreach (fn; functions)
    {
      if (fn.active == Active.Enabled)
      {
        writer ~= "";
        (new FuncWriter(fn)).write(writer);
      }
    }

    writer ~= "}";

    foreach (quarkFunc; errorQuarks) // Add error exceptions
    {
      if (quarkFunc.active == Active.Enabled)
      {
        writer ~= "";
        writer ~= constructEnumException(quarkFunc);
      }
    }

    auto imports = importManager.generate;

    if (imports.length)
      imports ~= "";

    writer.insert(cast(int)importLine, imports);

    writer.write;
  }

  /**
   * Construct a GError Exception class from a "error_quark" function.
   * Returns: D code for the GError exception.
   */
  dstring constructEnumException(Func func)
  {
    assert (func.name == "quark", func.name.to!string ~ " != quark");

    dstring output;

    auto exceptionName = dType.stripRight("Error");

    if (exceptionName.length == 0) // If this is a plain Error enum for a package, use the namespace as a prefix
      exceptionName = repo.namespace;

    output = "class " ~ exceptionName ~ "Exception : ErrorWrap\n{\n";
    output ~= "this(GError* err)\n{\nsuper(err);\n}\n\n";
    output ~= "this(Code code, string msg)\n{\nsuper(" ~ func.fullDName ~ ", cast(int)code, msg);\n}\n";
    output ~= "\nalias Code = " ~ repo.packageNamespace ~ ".types." ~ dType ~ ";\n}";

    return output;
  }

  override void toJson(ref JSONValue js)
  {
    super.toJson(js);

    js["bitfield"] = bitfield;
    js.jsonSetNonDefault("glibGetType", glibGetType);
    js.jsonSetNonDefault("glibTypeName", glibTypeName);
    js.jsonSetNonDefault("glibErrorDomain", glibErrorDomain);
    js.jsonSetNonDefault("version", version_);
    js.jsonSetNonDefault("moduleName", moduleName);
    js.jsonSet("members", members);
    js.jsonSetNonDefault("functions", functions);
    js.jsonSetNonDefault("errorQuarks", errorQuarks);
  }

  bool bitfield; /// true if flags bitfield, false for enum
  dstring glibGetType; /// GLib get_type function name
  dstring glibTypeName; /// GLib type name
  dstring glibErrorDomain; /// GLib error domain
  dstring version_; /// Version
  dstring moduleName;

  Member[] members; /// Enum/flags members
  Func[] functions; /// Functions
  Func[] errorQuarks; /// List of GError quark functions for exceptions
  Func[dstring] funcNameHash; /// Hash of functions by GIR name (snake_case)
}
