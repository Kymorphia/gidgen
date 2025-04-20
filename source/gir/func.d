module gir.func;

import std.conv : to;
import std.range : zip;

import code_writer;
import defs;
import gir.alias_;
import gir.field;
import gir.param;
import gir.property;
import gir.repo;
import gir.return_value;
import gir.structure;
import gir.type_node;
import utils;

/**
 * Function like object. Can be a function, method, signal, callback, etc.
 */
final class Func : TypeNode
{
  this(Base parent, XmlNode node)
  {
    super(parent);
    fromXml(node);
  }

  override @property dstring name()
  {
    return _name;
  }

  override @property void name(dstring val)
  {
    _name = val;
  }

  /**
   * Get the function name formatted in D camelCase
   * Params:
   *   firstUpper = true to make first character uppercase also (defaults to false)
   */
  override dstring dName()
  {
    return repo.defs.symbolName(_name.camelCase);
  }

  /**
   * Get the function name formatted in TitleCase
   */
  dstring titleName()
  {
    return repo.defs.symbolName(_name.camelCase(true));
  }

  /// Returns true if function has an instance parameter
  bool hasInstanceParam()
  {
    return params.length > 0 && params[0].isInstanceParam;
  }

  /**
   * Get a signal delegate instance parameter name. This is the dType of the parent class with a lowercase first letter.
   * Returns: Signal delegate instance parameter name or null if parent is not a class
   */
  dstring signalDelegInstanceParam()
  {
    if (auto owningClass = cast(Structure)parent)
      if (owningClass != repo.globalStruct)
        return repo.defs.symbolName(owningClass.dType[0].toLower ~ owningClass.dType[1 .. $]);

    return null;
  }

  /**
   * Check if a function is static (not a method, constructor, and not in global module)
   * Returns: true if the function is static, false otherwise
   */
  bool isStatic()
  {
    return funcType != FuncType.Method && !isCtor && parent !is repo.globalStruct;
  }

  override void fromXml(XmlNode node)
  {
    super.fromXml(node);

    _name = origName = node.get("name");
    funcType = cast(FuncType)FuncTypeValues.countUntil(node.id);
    cName = node.get("c:identifier");

    if (cName.empty)
      cName = node.get("c:type");

    version_ = node.get("version");
    shadowedBy = node.get("shadowed-by");
    shadows = node.get("shadows");
    invoker = node.get("invoker");
    movedTo = node.get("moved-to");
    introspectable = node.get("introspectable", "1") == "1";
    throws = node.get("throws") == "1";
    action = node.get("action") == "1";
    detailed = node.get("detailed") == "1";
    noHooks = node.get("noHooks") == "1";
    noRecurse = node.get("noRecurse") == "1";
    deprecated_ = node.get("deprecated") == "1";
    deprecatedVersion = node.get("deprecated-version");
    when = cast(SignalWhen)SignalWhenValues.countUntil(node.get("when"));
  }

  override dstring genDocs()
  {
    if (docContent.length == 0)
      return "/** */"; // Add blank docs if none, so that it is still included in generated DDocs

    auto s = "/**\n    "d ~ gdocToDDocFunc(docContent, "    ").stripLeft ~ "\n";

    auto paramDescrs = params.filter!(pa => !(pa.isInstanceParam || pa.isArrayLength || pa.isClosure || pa.isDestroy))
      .map!(pa => "      " ~ pa.dName ~ " = " ~ gdocToDDocFunc(pa.docContent, "        ").stripLeft).array;

    if (!paramDescrs.empty)
    {
      s ~= "\n    Params:\n"d;
      s ~= paramDescrs.join("\n") ~ "\n";
    }

    if (returnVal && returnVal.origDType != "none" && returnVal.lengthArrayParams.length == 0)
      s ~= "    Returns: " ~ gdocToDDocFunc(returnVal.docContent, "      ").stripLeft ~ "\n";

    if (throws)
      s ~= "    Throws: [" ~ errorDomain ~ "]\n";

    if (!docVersion.empty || !docDeprecated.empty)
    {
      s ~= "\n";

      if (!docVersion.empty)
        s ~= "    Version: " ~ docVersion ~ "\n";

      if (!docDeprecated.empty)
        s ~= "    Deprecated: " ~ gdocToDDocFunc(docDeprecated, "      ").stripLeft ~ "\n";
    }

    return s ~ "*/";
  }

  /**
  * Format a GTK doc string to be a DDoc string.
  * Newlines are formatted with a prefix to match the indendation for the comment block.
  * Parameter references are convered to D names and set to bold.
  * Function references func() are changed to the D function/method name and set to bold.
  *
  * Params:
  *   gdoc = The GTK doc string
  *   prefix = The newline wrap prefix
  * Returns: The DDoc formatted string
  */
  dstring gdocToDDocFunc(dstring gdoc, dstring prefix = "    ")
  {
    auto paramRe = ctRegex!(r"@(\w)"d);

    gdoc = repo.gdocToDDoc(gdoc, prefix);

    dstring paramReplaceFunc(Captures!(dstring) m)
    {
      foreach (pa; params)
        if (pa.name == m[1])
          return "`" ~ pa.dName ~ "`";

      return m[1];
    }

    return gdoc.replaceAll!(paramReplaceFunc)(paramRe); // Replace @cName with dName for parameters
  }

  override void fixup()
  {
    if (!cast(Field)parent)
    {
      _name = repo.subTypeStr(origName);

      if (funcType == FuncType.Callback && _name == origName)
        _name = _name.normalizeDTypeName();
    }

    super.fixup;

    if (!introspectable || !movedTo.empty || funcType == FuncType.VirtualMethod || funcType == FuncType.FuncMacro
        || !shadowedBy.empty)
      active = Active.Disabled;

    if (returnVal)
    {
      if (returnVal.lengthParamIndex >= 0) // Return array has a length argument?
      {
        if (hasInstanceParam) // Array length parameter indexes don't count instance parameters
          returnVal.lengthParamIndex++;

        if (returnVal.lengthParamIndex < params.length)
        {
          returnVal.lengthParam = params[returnVal.lengthParamIndex];
          returnVal.lengthParam.isLengthReturnArray = true;
        }
      }

      returnVal.fixup; // Fixup return value

      if (funcType == FuncType.Constructor) // Return actual instance type for constructors, not a GTK convenience type (like GtkWidget)
      {
        auto retSt = cast(Structure)returnVal.typeObject;
        auto parentSt = getParentByType!Structure;

        if (!(retSt is parentSt || returnVal.dType == parentSt.dType))
        {
          info("Changing return value for " ~ fullName.to!string ~ " from type " ~ returnVal.dType.to!string
            ~ " to " ~ parentSt.dType.to!string);

          returnVal.dType = parentSt.dType;
          returnVal.typeObject = parentSt;
          returnVal.typeRepo = parentSt.typeRepo;
        }
      }
    }

    foreach (pa; params) // Fixup parameters
    {
      pa.fixup;

      if (pa.isClosure && funcType == FuncType.Callback)
        closureParam = pa;
    }

    if (funcType == FuncType.Callback && !closureParam)
    { // Search for closure parameter if it is not explicitly designated
      foreach (pa; params.retro) // Look in reverse, since it is more likely to be an end parameter
      {
        if (pa.dType == "void*" && pa.name.toLower.canFind("data"))
        {
          pa.isClosure = true;
          closureParam = pa;
          info("Designating parameter '" ~ pa.fullName.to!string ~ "' as closure");
        }
      }
    }
  }

  override void resolve()
  {
    super.resolve;

    if (returnVal)
      returnVal.resolve; // Resolve return value

    foreach (pa; params) // Resolve parameters
      pa.resolve;
  }

  override void verify()
  {
    if (active != Active.Enabled)
      return;

    void disableFunc(string file, size_t line, string msg, TypeNode errorNode = null)
    {
      active = Active.Unsupported;
      warnWithLoc(file, line, xmlLocation, "Disabling " ~ (funcType == FuncType.Signal ? "signal '" : "function '" )
        ~ fullName.to!string ~ "': " ~ msg);
      TypeNode.dumpSelectorOnWarning(errorNode ? errorNode : this);
    }

    if (!shadows.empty && !shadowsFunc)
    {
      disableFunc(__FILE__, __LINE__, "Could not resolve shadows function name " ~ shadows.to!string);
      return;
    }

    if (returnVal)
    {
      try
        returnVal.verify; // Verify the return type
      catch (Exception e)
      {
        disableFunc(e.file, e.line, "Return type error: " ~ e.msg, returnVal);
        return;
      }
    }

    if (funcType == FuncType.Signal)
    {
      if (returnVal.containerType != ContainerType.None)
      {
        disableFunc(__FILE__, __LINE__, "signal container return type '" ~ returnVal.containerType.to!string
          ~ "' not supported", returnVal);
        return;
      }

      with(TypeKind) if (returnVal.kind.among(Simple, Pointer, Callback, Opaque, Unknown, Namespace))
      {
        disableFunc(__FILE__, __LINE__, "signal return type '" ~ returnVal.kind.to!string ~ "' is not supported", returnVal);
        return;
      }
    }
    else if (funcType == FuncType.Callback)
    {
      if (returnVal)
      {
        if (returnVal.containerType != ContainerType.None)
        {
          if (returnVal.ownership != Ownership.Full)
          {
            disableFunc(__FILE__, __LINE__, "callback return container type '" ~ returnVal.containerType.to!string
              ~ "' ownership '" ~ returnVal.ownership.to!string ~ "' is not supported");
            return;
          }
        }
      }
    }

    if (returnVal)
    {
      if (returnVal.lengthParamIndex != ArrayLengthUnset)
      {
        if (!returnVal.lengthParam) // Return array has invalid length argument?
        {
          disableFunc(__FILE__, __LINE__, "invalid return array length parameter index", returnVal);
          return;
        }

        if (returnVal.lengthParam.direction != ParamDirection.Out)
        {
          disableFunc(__FILE__, __LINE__, "return array length parameter direction must be Out", returnVal.lengthParam);
          return;
        }
      }
    }

    foreach (pi, pa; params)
    {
      if (pa.isInstanceParam && pi != 0)
        disableFunc(__FILE__, __LINE__, "invalid additional instance param '" ~ pa.name.to!string ~ "'", pa);

      if (pa.isClosure && pa != closureParam && funcType == FuncType.Callback)
        disableFunc(__FILE__, __LINE__, "multiple closure parameters", pa);

      try
        pa.verify; // Verify parameter
      catch (Exception e)
        disableFunc(e.file, e.line, "Parameter '" ~ pa.name.to!string ~ "' error: " ~ e.msg, pa);

      if (!pa.resolved)
        disableFunc(__FILE__, __LINE__, "Unresolved parameter '" ~ pa.name.to!string ~ "' of type '" ~ pa.dType.to!string ~ "'", pa);

      // Resolve parameter type aliases to see if any are disabled and disable parameter if so
      for (TypeNode tn = pa.typeObject; tn; tn = typeRepo.typeObjectHash.get((cast(Alias)tn).dType, null))
      {
        if (tn.active != Active.Enabled)
          pa.active = tn.active;

        if (!cast(Alias)tn)
          break;
      }

      if (pa.active != Active.Enabled)
        disableFunc(__FILE__, __LINE__, "Parameter '" ~ pa.name.to!string ~ "' of type '" ~ pa.dType.to!string ~ "' is disabled", pa);
    }

    with (FuncType) if (funcType.among(Constructor, Function, Method))
    {
      bool optionalNotOk;
      foreach (pa; params.retro) // Loop in reverse over params to verify optional parameters
      {
        if (!pa.isDParam)
          continue;

        if (pa.optional || pa.nullable) // optional or nullable set? We treat them the same.
        { // Verify it is a supported parameter type
          if (pa.direction == ParamDirection.Out)
          {
            optionalNotOk = true;
            infoWithLoc(__FILE__, __LINE__, pa.xmlLocation, "Optional not supported for parameter '"
              ~ pa.fullDName.to!string ~ "' with output direction");
          }
          else with (TypeKind) if (!pa.kind.among(String, Callback, Container, Pointer, Opaque, Wrap, Boxed, Reffed,
            Object, Interface) && !(pa.kind == Basic && pa.dType.among("void*"d, "const(void)*"d))) // FIXME - void* shouldn't be a Basic kind
          {
            optionalNotOk = true;
            infoWithLoc(__FILE__, __LINE__, pa.xmlLocation, "Optional not supported for parameter '"
              ~ pa.fullDName.to!string ~ "' of type '" ~ pa.fullDType.to!string ~ "' kind '"
              ~ pa.kind.to!string ~  "'");
          }
          else if (optionalNotOk)
            infoWithLoc(__FILE__, __LINE__, pa.xmlLocation, "Optional not supported for parameter '"
              ~ pa.fullDName.to!string ~ "' as other non-optional parameters follow it");
          else
            pa.isOptional = true;
        }
        else // Optional is not OK for parameters before a non-optional param
          optionalNotOk = true;
      }
    }
  }

  /**
   * Get function prototype string for function.
   * Returns: Function prototype string which intentially omits the function name for definition by the caller.
   */
  dstring getCPrototype(dstring funcName = "function")
  {
    dstring proto = returnVal.cType ~ " " ~ funcName ~ "(";

    foreach (i, p; params)
      proto ~= (i > 0 ? ", "d : "") ~ p.cType ~ " " ~ repo.defs.symbolName(p.name.camelCase);

    if (throws)
      proto ~= (params.length > 0 ? ", "d : ""d) ~ "GError** _err";

    return proto ~ ")";
  }

  /**
   * Get delegate prototype for a callback.
   */
  dstring getDelegPrototype()
  {
    dstring proto = "alias " ~ dName ~ " = " ~ returnVal.fullDType ~ " delegate(";

    foreach (p; params)
    {
      if (p.isClosure || p.isArrayLength)
        continue;

      if (proto[$ - 1] != '(')
        proto ~= ", ";

      proto ~= p.directionStr ~ p.fullDType ~ " " ~ repo.defs.symbolName(p.dName);
    }

    if (throws) // If a delegate throws GErrors, pass the GError** directly to the delegate
    {
      if (proto[$ - 1] != '(')
        proto ~= ", ";

      proto ~= "GError **_err";
    }

    return proto ~ ");";
  }

  /**
   * Construct a GError Exception class from a "error_quark" function.
   * Returns: D code for the GError exception.
   */
  dstring constructException()
  {
    assert (name.endsWith("error_quark"));

    dstring output;

    auto exceptionName = name[0 .. $ - "error_quark".length];

    auto st = getParentByType!Structure;

    if (!st)
      throw new Exception("Cannot construct exception class from function " ~ fullName.to!string);

    if (!exceptionName.empty)
      exceptionName = st.origDType ~ exceptionName.stripRight("_").camelCase(true);
    else
      exceptionName = st.origDType;

    output = "class " ~ exceptionName ~ "Exception : ErrorWrap\n{\n";
    output ~= "this(GError* err)\n{\nsuper(err);\n}\n\n";
    output ~= "this(Code code, string msg)\n{\nsuper(" ~ st.fullDType ~ "." ~ dName ~ ", cast(int)code, msg);\n}\n";
    output ~= "\nalias Code = G" ~ exceptionName ~ "Error;\n}";

    return output;
  }

  /**
   * Search a class' ancestry for a conflicting method (requiring an alias or override).
   * Params:
   *   st = Structure to search parent ancestry of (null will search from parent of function's class)
   *   outConforms = Output value set to true if matching method conforms
   * Returns: The ancestor class containing the conflicting method or null if none
   */
  Structure findMethodConflict(Structure st, out bool outConforms)
  {
    if (!st) // If klass not specified, use this function's klass parent
      st = cast(Structure)parent ? (cast(Structure)parent).parentStruct : null;

    if (isStatic || funcType != FuncType.Method || !st || st.structType != StructType.Class)
      return null;

    auto methodName = dName;

    for (auto klass = st; klass; klass = klass.parentStruct)
    {
      auto node = klass.dMethodHash.get(methodName, null);
      auto cmpFunc = cast(Func)node;

      if (cmpFunc && cmpFunc.shadowedByFunc) // If the method is shadowed by another one, use it instead, must do this before checking node.active
      {
        cmpFunc = cmpFunc.shadowedByFunc;
        node = cmpFunc;
      }

      if (!node || node.active != Active.Enabled)
        continue;

      if (cmpFunc) // Regular function method?
      { // Check if the method conforms to the parent class method (identical or derived parameters/return value of ancestor method)
        outConforms = (params.length == cmpFunc.params.length // Same number of parameters
          && (returnVal is null) == (cmpFunc.returnVal is null) // Both have return value or both do not
          && (returnVal.typeEqual(cmpFunc.returnVal) // Return value D types are equal
            || structIsDerived(cmpFunc.returnVal.typeObject, returnVal.typeObject)) // or child method return type is derived from parent return type
          && zip(cmpFunc.params.filter!(x => !x.isInstanceParam), params.filter!(x => !x.isInstanceParam))
            .filter!(t => !t[0].typeEqual(t[1])).empty); // All arguments types match (not including instance types)

        return klass;
      }
      else if (auto cmpProp = cast(Property)node) // Property method
      {
        outConforms = cmpProp.checkGetter(this) || cmpProp.checkSetter(this);
        return klass;
      }
    }

    return null;
  }

  private dstring _name; /// Name of function
  dstring origName; /// Original name
  FuncType funcType; /// Function type
  dstring cName; /// C type name (Gir c:identifier)
  ReturnValue returnVal; /// Return value type
  Param[] params; /// Parameters
  Param closureParam; /// Closure data parameter or null (Callback only)
  bool isCtor; /// Set for the primary constructor of an instance (not a Gir field)
  Func shadowedByFunc; /// Resolved function object for shadowedBy
  Func shadowsFunc; /// Resolved function object for shadows

  bool nullable; /// Nullable return value pointer

  dstring version_; /// Version
  dstring shadowedBy; /// Function which shadows this
  dstring shadows; /// Function that this shadows
  dstring invoker; /// Invoker method
  dstring movedTo; /// Function moved to name

  bool introspectable = true; /// Introspectable?
  bool throws; /// Throws exception?
  bool action; /// Signal action (FIXME)
  bool detailed; /// Signal detailed (indicates the signal accepts a detail string)
  bool noHooks; /// Signal no hooks (FIXME)
  bool noRecurse; /// Signal no recurse (FIXME)

  bool deprecated_; /// Deprecated
  dstring deprecatedVersion; /// Deprecated version
  SignalWhen when; /// Signal when
}

enum FuncType
{
  Unknown = -1,
  Function,
  Callback,
  Constructor,
  Signal,
  Method,
  VirtualMethod,
  FuncMacro,
}

immutable dstring[] FuncTypeValues = [
  "function", "callback", "constructor", "glib:signal", "method",
  "virtual-method", "function-macro"
];

enum SignalWhen
{
  Unknown = -1,
  First,
  Last,
  Cleanup,
}

immutable dstring[] SignalWhenValues = ["first", "last", "cleanup"];
