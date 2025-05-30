module gir.structure;

import code_writer;
import defs;
import gir.base;
import gir.field;
import gir.func;
import gir.func_writer;
import gir.property;
import gir.repo;
import gir.signal_writer;
import gir.type_node;
import import_manager;
import utils;

/// Structure class which is used for class, interface, and records in Gir files
final class Structure : TypeNode
{
  this(Base parent)
  {
    super(parent);
    defCode = new DefCode;
  }

  this(Base parent, XmlNode node)
  {
    this(parent);
    fromXml(node);
  }

  override @property dstring name()
  {
    return dType;
  }

  /// D type name
  override @property dstring dName()
  {
    if (kind != TypeKind.Namespace)
      return moduleName ~ "." ~ _dType;
    else
      return moduleName;
  }

  override @property bool inModule()
  {
    with (TypeKind) return kind.among(Opaque, Wrap, Boxed, Reffed, Object, Interface, Namespace) != 0;
  }

  override @property bool inGlobal()
  {
    with (TypeKind) return kind.among(Simple, Pointer) != 0;
  }

  @property dstring moduleName()
  {
    return inModule ? _moduleName : "types";
  }

  override void fromXml(XmlNode node)
  {
    super.fromXml(node);

    structType = cast(StructType)StructTypeValues.countUntil(node.id);
    cSymbolPrefix = node.get("c:symbol-prefix");
    parentType = node.get("parent");
    version_ = node.get("version");

    abstract_ = node.get("abstract") == "1";
    deprecated_ = node.get("deprecated") == "1";
    opaque = node.get("opaque") == "1" || node.get("foreign") == "1";
    pointer = node.get("pointer") == "1";
    glibFundamental = node.get("glib:fundamental") == "1";

    deprecatedVersion = node.get("deprecated-version");
    glibGetType = node.get("glib:get-type");
    glibTypeName = node.get("glib:type-name");
    glibGetValueFunc = node.get("glib:get-value-func");
    glibSetValueFunc = node.get("glib:set-value-func");
    glibRefFunc = node.get("glib:ref-func");
    glibUnrefFunc = node.get("glib:unref-func");
    glibTypeStruct = node.get("glib:type-struct");
    glibIsGtypeStructFor = node.get("glib:is-gtype-struct-for");
    copyFunction = node.get("copy-function");
    freeFunction = node.get("free-function");
  }

  /**
   * Add a function to a structure.
   * Params:
   *   func = The function to add
   */
  void addFunc(Func func)
  {
    functions ~= func;
    funcNameHash[func.name] = func;
    dMethodHash[func.dName] = func;
  }

  /**
   * Look through a class structure's parent object ancestry to see which one implements a given interface.
   * Params:
   *   iface = The interface structure to find an implementation of
   * Returns: The ancestor structure that implements the interface or null if not found.
   */
  Structure getIfaceAncestor(Structure iface)
  {
    for (auto cl = parentStruct; cl; cl = cl.parentStruct)
      if (cl.implementStructs.canFind(iface))
        return cl;

    return null;
  }

  // Calculate the structure type kind
  private TypeKind calcKind()
  {
    if (structType == StructType.Record && !glibGetType.empty)
      return TypeKind.Boxed;

    if (structType == StructType.Record && (opaque || pointer))
      return functions.filter!(x => x.active == Active.Enabled).empty ? TypeKind.Pointer : TypeKind.Opaque;

    if (structType == StructType.Record || structType == StructType.Union)
    {
      if (!functions.empty)
        return TypeKind.Wrap;

      auto retKind = TypeKind.Simple;
      foreach (field; fields) // HACK: Check for field.callback since it is set before the kind is resolved (fixup dependency issue)
      {
        if (field.kind == TypeKind.Unknown)
          retKind = TypeKind.Unknown;
        else if (field.containerType == ContainerType.Array)
        {
          if (field.elemTypes.empty || field.elemTypes[0].kind == TypeKind.Unknown)
            retKind = TypeKind.Unknown;
          else if (!field.elemTypes[0].kind.among(TypeKind.Basic, TypeKind.BasicAlias,
              TypeKind.Callback, TypeKind.Enum, TypeKind.Flags))
          {
            retKind = TypeKind.Wrap;
            break;
          }
        }
        else if (field.containerType != ContainerType.None || (!field.callback && !field.kind.among(TypeKind.Basic,
            TypeKind.BasicAlias, TypeKind.Callback, TypeKind.Enum, TypeKind.Flags)))
        {
          retKind = TypeKind.Wrap;
          break;
        }
      }

      return retKind;
    }

    if (structType == StructType.Class && glibFundamental
        && (!parentType.empty || (!glibRefFunc.empty && !glibUnrefFunc.empty)))
      return TypeKind.Reffed;

    if (structType == StructType.Class && !parentType.empty && !glibGetType.empty)
      return TypeKind.Object;

    if (structType == StructType.Interface)
      return TypeKind.Interface;

    if (dType == "ObjectWrap") // Minor HACK: ObjectWrap is the OG Object
      return TypeKind.Object;

    return TypeKind.Unknown;
  }

  override void fixup()
  {
    import std.string : chomp;

    if (origDType.canFind('_')) // If the original D type was snake case, make sure to remove any _t (FIXME - Kind of a hack for Harfbuzz)
      _moduleName = repo.defs.symbolName(origDType.snakeCase.chomp("_t"));
    else // FIXME - Add support to set the module name, default to using the original type (prior to any postfixes like for ObjectAtk for example)
      _moduleName = repo.defs.symbolName(origDType.snakeCase);

    if (auto field = cast(Field)parent) // Structure as a field of another structure?
    { // dType and cType are the field name (not an actual type)
      dType = field.dName;
      cType = repo.defs.symbolName(origCType);
      kind = TypeKind.Simple;
      return;
    }

    super.fixup;

    if (cType.empty) // If cType is unset, set it to glibTypeName
      cType = glibTypeName;

    foreach (f; fields) // Fixup structure fields
    {
      f.fixup;

      if (f.active == Active.Enabled && (opaque || pointer))
        f.active = Active.Unsupported;
    }

    foreach (p; properties) // Fixup object properties
      p.fixup;

    foreach (fn; functions) // Fixup structure function/methods
    {
      fn.fixup;

      if (!fn.shadows.empty)
        fn.shadowsFunc = funcNameHash.get(fn.shadows, null);

      if (!fn.shadowedBy.empty)
        fn.shadowedByFunc = funcNameHash.get(fn.shadowedBy, null);

      if (fn.funcType == FuncType.Constructor && fn.name == "new") // Set "new" constructor as the primary constructor
        ctorFunc = fn;

      if (fn.funcType == FuncType.Function && fn.returnVal.dType == "Quark" && fn.name.endsWith("error_quark"))
        errorQuarks ~= fn; // Add exception error quark functions to array

      repo.defs.cSymbolHash[fn.cName] = fn; // Add to global C symbol hash
    }

    if (ctorFunc)
      ctorFunc.isCtor = true;

    foreach (sg; signals) // Fixup structure signals
      sg.fixup;
  }

  override void resolve()
  {
    if (auto field = cast(Field)parent) // Structure as a field of another structure?
      return;

    foreach (f; fields) // Resolve structure fields
      f.resolve;

    foreach (p; properties) // Resolve object properties
      p.resolve;

    foreach (fn; functions) // Resolve structure function/methods
      fn.resolve;

    foreach (sg; signals) // Resolve structure signals
      sg.resolve;

    if (kind == TypeKind.Unknown)
      kind = calcKind;

    super.resolve;

    if (kind == TypeKind.Boxed && parentType.empty)
      parentType = "GObject.Boxed";

    if (!parentType.empty)
    {
      parentStruct = cast(Structure)repo.findTypeObject(parentType);
      updateUnresolvedFlags(UnresolvedFlags.ParentStruct, parentStruct is null);
    }

    implementStructs.length = 0;

    updateUnresolvedFlags(UnresolvedFlags.Implements, false);

    foreach (ifaceName; implements)
    {
      if (auto ifaceStruct = cast(Structure)repo.findTypeObject(ifaceName))
        implementStructs ~= ifaceStruct;
      else
        updateUnresolvedFlags(UnresolvedFlags.Implements, true);
    }
  }

  override void verify()
  {
    if (active != Active.Enabled || cast(Field)parent) // Don't verify if structure is disabled or a field structure
      return;

    super.verify;

    if (!parentType.empty && !parentStruct)
      throw new Exception("Failed to resolve parent type '" ~ parentType.to!string ~ "'");
 
    if (parentStruct && parentStruct.active != Active.Enabled)
      throw new Exception("Structure parent type '" ~ parentStruct.fullName.to!string ~ "' is disabled");

    foreach (ifaceName; implements)
      if (!cast(Structure)repo.findTypeObject(ifaceName))
      {
        warnWithLoc(__FILE__, __LINE__, xmlLocation, "Unable to resolve structure " ~ fullName.to!string ~ " interface " ~ ifaceName.to!string);
        TypeNode.dumpSelectorOnWarning(this);
      }

    if (defCode.inhibitFlags & DefInhibitFlags.Funcs) // Skip verification of functions, signals, and fields if they aren't being generated
      return;

    foreach (fn; functions) // Verify structure function/methods
    {
      if (fn.active != Active.Enabled)
        continue;

      with(FuncType) if (!fn.funcType.among(Callback, Function, Constructor, Signal, Method))
      {
        fn.active = Active.Unsupported;
        warnWithLoc(__FILE__, __LINE__, fn.xmlLocation, "Disabling function '" ~ fn.fullName.to!string ~ "' of type '" ~ fn.funcType.to!string
            ~ "' which is not supported");
        TypeNode.dumpSelectorOnWarning(fn);
      }
      else
        fn.verify;
    }

    foreach (sig; signals) // Verify structure signals
    {
      if (sig.active != Active.Enabled)
        continue;

      try
        sig.verify;
      catch (Exception e)
      {
        sig.active = Active.Unsupported;
        warnWithLoc(e.file, e.line, sig.xmlLocation, "Disabling signal '" ~ sig.fullName.to!string ~ "': " ~ e.msg);
        TypeNode.dumpSelectorOnWarning(sig);
      }
    }

    foreach (f; fields) // Verify structure fields
    {
      if (kind != TypeKind.Wrap && kind != TypeKind.Boxed) // Ignore fields in structures other than Wrap and Boxed
        f.active = Active.Ignored;

      if (f.active != Active.Enabled)
        continue;

      try
        f.verify;
      catch (Exception e)
      {
        f.active = Active.Unsupported;
        warnWithLoc(e.file, e.line, f.xmlLocation, "Disabling field '" ~ f.fullName.to!string ~ "': " ~ e.msg);
        TypeNode.dumpSelectorOnWarning(f);
      }
    }

    foreach (p; properties) // Verify object properties
    {
      if (p.active != Active.Enabled)
        continue;

      try
      {
        p.verify;
        dMethodHash[p.dName] = p; // Add to method hash to detect conflicting method names
      }
      catch (Exception e)
      {
        p.active = Active.Unsupported;
        warnWithLoc(e.file, e.line, p.xmlLocation, "Disabling property '" ~ p.fullName.to!string ~ "': " ~ e.msg);
        TypeNode.dumpSelectorOnWarning(p);
      }
    }
  }

  /**
   * Write structure module.
   * Params:
   *   path = Directory to store the structure file(s) to (interfaces have multiple files)
   *   moduleType = Module file type being written (defaults to ModuleType.Normal)
   */
  void write(string path, ModuleType moduleType = ModuleType.Normal)
  {
    codeTrap("struct.write", fullName);

    auto isIfaceTemplate = kind == TypeKind.Interface && moduleType == ModuleType.IfaceTemplate;
    auto writer = new CodeWriter(buildPath(path, moduleName.to!string ~ (isIfaceTemplate ? "_mixin" : "") ~ ".d")); // Append T to type name for interface mixin template module

    dstring modType;

    if (isIfaceTemplate)
      modType = "interface mixin";
    else if (structType == StructType.Interface)
      modType = "interface";
    else
      modType = "class";

    if (dType)
      writer ~= "/// Module for [" ~ dType ~ "] " ~ modType;

    writer ~= ["module " ~ fullModuleName ~ (isIfaceTemplate ? "_mixin;"d : ";"d), ""];

    beginImports(this);
    scope(exit) endImports;

    if (parentStruct)
      importManager.add(parentStruct.fullModuleName); // Add parent to imports

    foreach (st; implementStructs) // Add implemented interfaces to imports
    {
      importManager.add(st.fullModuleName);
      importManager.add(st.fullModuleName ~ "_mixin");
    }

    if (!errorQuarks.empty)
    {
      importManager.add("glib.types");
      importManager.add("glib.error");
    }

    if (!(defCode.inhibitFlags & DefInhibitFlags.Imports) && kind == TypeKind.Interface)
      writer ~= "public import " ~ fullModuleName ~ "_iface_proxy;";

    auto importLine = writer.lines.length; // Save position where imports should go, which are inserted later, to gather all dependencies

    if (defCode.preClass.length > 0)
      writer ~= defCode.preClass;

    writer ~= genDocs;

    Structure[] objIfaces;

    if (defCode.classDecl.empty)
    {
      if (kind == TypeKind.Interface)
        writer ~= isIfaceTemplate ? ("template " ~ dType ~ "T()") : ("interface " ~ dType);
      else
      { // Create range of parent type and implemented interface types, but filter out interfaces already implemented by ancestors
        objIfaces = implementStructs.filter!(x => !getIfaceAncestor(x)).array;
        auto parentAndIfaces = (parentStruct ? [parentStruct] : []) ~ objIfaces;
        writer ~= "class " ~ dType ~ (!parentAndIfaces.empty ? " : " ~ parentAndIfaces.map!(x => x.fullDType)
          .join(", ") : "");
      }
    }
    else
      writer ~= defCode.classDecl;

    writer ~= "{";

    if (!(defCode.inhibitFlags & DefInhibitFlags.Init))
    {
      writeInitCode(writer, moduleType);

      if (kind == TypeKind.Wrap || kind == TypeKind.Boxed)
        writer ~= constructFieldProps; // Construct field property methods
      else if (kind == TypeKind.Object || kind == TypeKind.Interface)
        writer ~= constructProps(moduleType); // Construct property methods
    }

    if (kind == TypeKind.Object && !objIfaces.empty)
    {
      writer ~= "";

      foreach (iface; objIfaces)
        writer ~= "mixin " ~ iface.dType ~ "T!();";

      if (parentStruct)
      {
        foreach (iface; objIfaces) // Look for methods in parent classes which conflict with interface methods
        {
          foreach (func; iface.functions)
          {
            bool outIsIdentical;

            if (auto conflictClass = func.findMethodConflict(parentStruct, outIsIdentical))
              if (!outIsIdentical)
                writer ~= ["alias "d ~ func.dName ~ " = " ~ conflictClass.fullDType ~ "." ~ func.dName ~ ";"];
          }
        }
      }
    }

    if (defCode.inClass.length > 0)
      writer ~= defCode.inClass;

    if (!(defCode.inhibitFlags & DefInhibitFlags.Funcs))
    {
      foreach (fn; functions)
      {
        if (fn.active == Active.Enabled)
        {
          writer ~= "";
          (new FuncWriter(fn)).write(writer, moduleType);
        }
      }

      foreach (sig; signals)
      {
        if (sig.active == Active.Enabled)
        {
          writer ~= "";
          (new SignalWriter(sig)).write(writer, moduleType);
        }
      }
    }

    writer ~= "}";

    if (!isIfaceTemplate && !(defCode.inhibitFlags & DefInhibitFlags.Funcs))
    {
      foreach (quarkFunc; errorQuarks) // Add error exceptions
      {
        if (quarkFunc.active == Active.Enabled)
        {
          writer ~= "";
          writer ~= quarkFunc.constructException;
        }
      }
    }

    if (defCode.postClass.length > 0)
      writer ~= defCode.postClass;

    if (!(defCode.inhibitFlags & DefInhibitFlags.Imports))
    {
      auto imports = importManager.generate(isIfaceTemplate ? "public " : ""); // Interface templates use public imports so they are conveyed to the object they are mixed into

      if (imports.length)
        imports ~= "";

      writer.insert(cast(int)importLine, imports);
    }

    writer.write();
  }

  // Write class init code
  private void writeInitCode(CodeWriter writer, ModuleType moduleType)
  {
    if ((kind == TypeKind.Opaque && !pointer) || (kind == TypeKind.Reffed && !parentStruct))
      writer ~= [cTypeRemPtr ~ "* cInstancePtr;"];
    else if (kind == TypeKind.Opaque && pointer)
      writer ~= [cType ~ " cInstancePtr;"];
    else if (kind == TypeKind.Wrap)
      writer ~= [cTypeRemPtr ~ " cInstance;"];

    if (kind == TypeKind.Opaque)
      writer ~= "bool owned;";

    // Boxed structures with defined structures can be allocated, add ctor without args
    if (kind == TypeKind.Boxed && !ctorFunc && !opaque && !pointer && !fields.empty)
      writer ~= writeBoxedCtor;

    if (kind == TypeKind.Opaque)
      writer ~= ["", "/** */", "this(void* ptr, Flag!\"Take\" take)", "{",
        "if (!ptr)", "throw new GidConstructException(\"Null instance pointer for " ~ fullDName ~ "\");", ""];
    else if (kind == TypeKind.Wrap || kind == TypeKind.Reffed)
      writer ~= ["", "/** */", "this(void* ptr, Flag!\"Take\" take)", "{",
        "if (!ptr)", "throw new GidConstructException(\"Null instance pointer for " ~ fullDName ~ "\");", ""];
    else if (kind == TypeKind.Boxed || kind == TypeKind.Object)
      writer ~= ["", "/** */", "this(void* ptr, Flag!\"Take\" take)", "{",
        "super(cast(void*)ptr, take);", "}"];

    if (kind == TypeKind.Opaque && !pointer)
      writer ~= ["cInstancePtr = cast(" ~ cTypeRemPtr ~ "*)ptr;", "", "owned = take;", "}"];
    else if (kind == TypeKind.Opaque && pointer)
      writer ~= ["cInstancePtr = cast(" ~ cType ~ ")ptr;", "", "owned = take;", "}"];
    else if (kind == TypeKind.Wrap)
      writer ~= ["cInstance = *cast(" ~ cTypeRemPtr ~ "*)ptr;", "", "if (take)", "gFree(ptr);", "}"];
    else if (kind == TypeKind.Reffed && !parentStruct)
      writer ~= ["cInstancePtr = cast(" ~ cTypeRemPtr ~ "*)ptr;", "", "if (!take)", glibRefFunc
        ~ "(cInstancePtr);", "}", "", "~this()", "{", glibUnrefFunc ~ "(cInstancePtr);", "}", ""];
    else if (kind == TypeKind.Reffed && parentStruct)
      writer ~= ["super(cast(" ~ parentStruct.cType ~ "*)ptr, take);", "}"];

    if (kind == TypeKind.Opaque && freeFunction)
      writer ~= ["", "~this()", "{", "if (owned)", freeFunction ~ "(cInstancePtr);", "}"];
    else if (kind == TypeKind.Wrap && freeFunction)
      writer ~= ["", "~this()", "{", freeFunction ~ "(&cInstance);", "}"];

    if (kind == TypeKind.Opaque)
      writer ~= ["", "/** */", "void* _cPtr()", "{", "return cast(void*)cInstancePtr;", "}"];
    else if (kind == TypeKind.Reffed && !parentStruct)
      writer ~= ["", "/** */", "void* _cPtr(Flag!\"Dup\" dup = No.Dup)", "{", "if (dup)", glibRefFunc ~ "(cInstancePtr);", "",
        "return cInstancePtr;", "}"];
    else if (kind == TypeKind.Boxed)
      writer ~= ["", "/** */", "void* _cPtr(Flag!\"Dup\" dup = No.Dup)", "{", "return dup ? copy_ : cInstancePtr;", "}"];
    else if (kind == TypeKind.Wrap)
      writer ~= ["", "/** */", "void* _cPtr()", "{", "return cast(void*)&cInstance;", "}"];

    if (kind.among(TypeKind.Boxed, TypeKind.Object) || (kind == TypeKind.Interface && moduleType == ModuleType.Iface))
      writer ~= ["", "/** */", "static GType _getGType()", "{", "import gid.loader : gidSymbolNotFound;",
        "return cast(void function())" ~ glibGetType
        ~ " != &gidSymbolNotFound ? " ~ glibGetType ~ "() : cast(GType)0;", "}"]; // Return 0 if get_type() function was not resolved

    if (kind.among(TypeKind.Boxed, TypeKind.Object))
      writer ~= ["", "/** */", "override @property GType _gType()", "{", "return _getGType();", "}", "",
        "/** Returns `this`, for use in `with` statements. */", "override " ~ dType ~ " self()", "{", "return this;", "}"];
  }

  // Write a Boxed type constructor with all fields as parameters with default values (optional)
  private dstring writeBoxedCtor()
  {
    dstring s = "\n/**\n    Create a `" ~ dName ~ "` boxed type.\n";
    bool paramsShown;

    foreach (f; fields)
    {
      if (f.active == Active.Enabled && f.writable)
      {
        if (!paramsShown)
        {
          paramsShown = true;
          s ~= "    Params:\n";
        }

        s ~= "      " ~ f.dName ~ " = " ~ repo.gdocToDDoc(f.docContent, "        ").stripLeft ~ "\n";
      }
    }

    s ~= "*/\nthis(";

    foreach (f; fields)
    {
      if (f.active == Active.Enabled && f.writable)
      {
        if (s[$ - 1] != '(')
          s ~= ", ";

        dstring fieldType;

        if (f.kind != TypeKind.Callback)
          fieldType = f.fullDType;
        else if (f.typeObject) // Callback function is an alias type?
          fieldType = f.cType;
        else // Callback function type is directly defined in field
          fieldType = f.name.camelCase(true) ~ "FuncType";

        if (f.dType == "float" || f.dType == "double")
          s ~= fieldType ~ " " ~ f.dName ~ " = 0.0"; // Use 0.0 for default value for floating point values (not nan)
        else
          s ~= fieldType ~ " " ~ f.dName ~ " = " ~ fieldType ~ ".init"; // Otherwise use types .init value
      }
    }

    s ~= ")\n{\nsuper(gMalloc(" ~ cType ~ ".sizeof), Yes.Take);\n";

    foreach (f; fields)
      if (f.active == Active.Enabled && f.writable)
        s ~= "this." ~ f.dName ~ " = " ~ f.dName ~ ";\n";

    return s ~ "}";
  }

  // Construct struct wrapper field property methods
  private dstring[] constructFieldProps()
  {
    dstring[] lines;

    auto cPtr = "(cast(" ~ (cType.countStars > 0 ? cTypeRemPtr : cType) ~ "*)this._cPtr)";

    foreach (f; fields)
    {
      if (f.active != Active.Enabled)
        continue;

      assert(!f.directStruct, "Unsupported embedded structure field " ~ f.fullName.to!string);

      assert(f.containerType == ContainerType.None, "Unsupported structure field " ~ f.fullName.to!string
          ~ " with container type " ~ f.containerType.to!string);

      if (f.kind == TypeKind.Callback && !f.typeObject) // Callback function type directly defined in field?
        lines ~= ["", "/** Function alias for field `"~ f.dName ~"` */",
          "alias " ~ f.name.camelCase(true) ~ "FuncType = extern(C) "
          ~ f.callback.getCPrototype ~ ";"]; // Add a type alias, since extern(C) can't be used directly in arg definition

      lines ~= genPropDocs(f, Yes.Getter);

      if (f.kind != TypeKind.Callback)
        lines ~= ["@property " ~ f.fullDType ~ " " ~ f.dName ~ "()", "{"];

      dstring addrIfNeeded() // Returns an & if field is a direct structure, when we need a pointer to it
      {
        with (TypeKind) return (f.kind.among(Simple, Boxed, Reffed) && f.cType.countStars == 0) ? "&"d : "";
      }

      final switch (f.kind) with (TypeKind)
      {
        case Basic, BasicAlias, Pointer:
          lines ~= "return " ~ cPtr ~ "." ~ f.dName ~ ";";
          break;
        case Enum, Flags:
          lines ~= "return cast(" ~ f.fullDType ~ ")" ~ cPtr ~ "." ~ f.dName ~ ";";
          break;
        case Callback:
          if (f.typeObject) // Callback function is an alias type?
            lines ~= ["@property " ~ f.cType ~ " " ~ f.dName ~ "()", "{"];
          else // Callback function type is directly defined in field
            lines ~= ["@property " ~ f.name.camelCase(true) ~ "FuncType " ~ f.dName ~ "()", "{"];

          lines ~= "return " ~ cPtr ~ "." ~ f.dName ~ ";";
          break;
        case String, Simple, Object, Boxed, Reffed, Interface:
          lines ~= "return cToD!(" ~ f.fullDType ~ ")(cast(void*)" ~ addrIfNeeded ~ cPtr ~ "." ~ f.dName ~ ");";
          break;
        case Opaque, Wrap:
          auto starCount = f.cType.retro.countUntil!(x => x != '*');

          if (starCount < 1) // The cast is for casting away "const"
            lines ~= "return new " ~ f.fullDType ~ "(cast(" ~ f.cType.stripConst ~ "*)" ~ "&" ~ cPtr ~ "." ~ f.dName
              ~ ", No.Take);";
          else
            lines ~= "return new " ~ f.fullDType ~ "(cast(" ~ f.cType.stripConst ~ ")" ~ cPtr ~ "." ~ f.dName
              ~ ", No.Take);";
          break;
        case Unknown, Container, Namespace:
          throw new Exception(
              "Unhandled readable field property type '" ~ f.fullDType.to!string ~ "' (" ~ f.kind.to!string
              ~ ") for struct " ~ fullDType.to!string);
      }

      lines ~= "}";

      if (!f.writable)
        continue;

      lines ~= genPropDocs(f, No.Getter);

      if (f.kind != TypeKind.Callback) // Callback setter declaration is specially handled below
        lines ~= ["@property void " ~ f.dName ~ "(" ~ f.fullDType ~ " propval)", "{"];

      final switch (f.kind) with (TypeKind)
      {
        case Basic, BasicAlias, Pointer:
          lines ~= cPtr ~ "." ~ f.dName ~ " = propval;";
          break;
        case Enum, Flags:
          lines ~= cPtr ~ "." ~ f.dName ~ " = cast(" ~ f.cType ~ ")propval;";
          break;
        case Simple:
          lines ~= cPtr ~ "." ~ f.dName ~ " = " ~ (f.cType.countStars == 1 ? "&"d : ""d) ~ "propval;"; // If field is a pointer, use the address of the structure
          break;
        case Callback:
          if (f.typeObject) // Callback function is an alias type?
            lines ~= ["", "@property void " ~ f.dName ~ "(" ~ f.cType ~ " propval)", "{"];
          else // Callback function type is directly defined in field
            lines ~= ["", "@property void " ~ f.dName ~ "(" ~ f.name.camelCase(true) ~ "FuncType propval)", "{"];

          lines ~= cPtr ~ "." ~ f.dName ~ " = propval;";
          break;
        case String, Boxed, Reffed, Object, Interface:
          lines ~= ["cValueFree!(" ~ f.fullDType ~ ")(cast(void*)" ~ addrIfNeeded ~ cPtr ~ "." ~ f.dName ~ ");",
            "dToC(propval, cast(void*)&" ~ cPtr ~ "." ~ f.dName ~ ");"];
          break;
        case Opaque, Wrap, Container, Namespace, Unknown:
          throw new Exception("Unhandled writable field property type '" ~ f.fullDType.to!string ~ "' (" ~ f
              .kind.to!string ~ ") for struct " ~ fullDType.to!string);
      }

      lines ~= "}";
    }

    return lines;
  }

  /**
   * Generate adrdox documentation for a field or property getter/setter
   * Returns: Lines of generated documentation
   */
  private dstring[] genPropDocs(TypeNode node, Flag!"Getter" getter)
  {
    if (node.docContent.length == 0)
      return ["", "/** */"]; // Add blank docs if none, so that it is still included in generated DDocs

    dstring[] lines = [""];
    dstring type;
    dstring name;

    if (auto field = cast(Field)node)
    {
      type = "field";
      name = field.dName;
    }
    else if (auto prop = cast(Property)node)
    {
      type = "property";
      name = prop.dName;
    }

    if (getter)
      lines ~= ["/**"d, "    Get `"d ~ name ~ "` " ~ type ~ ".",
        "    Returns: "d ~ repo.gdocToDDoc(node.docContent, "      ").stripLeft];
    else
      lines ~= ["/**"d, "    Set `"d ~ name ~ "` " ~ type ~ ".",
      "    Params:", "      propval = "d ~ repo.gdocToDDoc(node.docContent, "        ").stripLeft];

    if (!node.docVersion.empty || !node.docDeprecated.empty)
    {
       lines ~= "";

      if (!node.docVersion.empty)
        lines ~= "    Version: " ~ node.docVersion;

      if (!node.docDeprecated.empty)
        lines ~= "    Deprecated: " ~ repo.gdocToDDoc(node.docDeprecated, "      ").stripLeft;
    }

    return lines ~ "*/";
  }

  // Construct struct wrapper property methods
  private dstring[] constructProps(ModuleType moduleType)
  {
    dstring[] lines;

    foreach (p; properties)
    {
      if (p.active != Active.Enabled || p.constructOnly)
        continue;

      if (p.readable)
      {
        bool outOverrideMethod;

        if (parentStruct)
          if (auto conflictClass = p.findMethodConflict(parentStruct, Yes.Getter, outOverrideMethod))
            if (!outOverrideMethod)
              lines ~= ["", "alias "d ~ p.dName ~ " = " ~ conflictClass.fullDType ~ "." ~ p.dName ~ ";"]; // Add an alias for conflicting methods which don't conform

        lines ~= genPropDocs(p, Yes.Getter);
        lines ~= (outOverrideMethod ? "override "d : ""d) ~ "@property " ~ p.fullDType ~ " " ~ p.dName ~ "()";

        if (moduleType != ModuleType.Iface)
        {
          Func checkGetter(TypeNode n)
          {
            auto f = cast(Func)n;
            return (f && p.checkGetter(f)) ? f : null;
          }

          auto getter = !p.getter.empty ? checkGetter(dMethodHash.get(repo.defs.symbolName(p.getter.camelCase), null)) : null; // Use getter method for improved performance
          if (!getter)
            getter = !p.propGet.empty ? checkGetter(cast(Func)repo.defs.cSymbolHash.get(p.propGet, null)) : null; // Use alternative org.gtk.Property.get attribute as a backup (full C symbol function name)

          if (!getter)
            addImport("gobject.object");

          lines ~= ["{", getter ? ("return " ~ getter.dName ~ "();") : ("return gobject.object.ObjectWrap.getProperty!("
            ~ p.fullDType ~ ")(\"" ~ p.name ~ "\");"), "}"]; // Use getProperty if no getter method
        }
        else
          lines[$ - 1] ~= ";";
      }

      if (!p.writable)
        continue;

      bool outOverrideMethod;

      if (parentStruct)
        if (auto conflictClass = p.findMethodConflict(parentStruct, No.Getter, outOverrideMethod))
          if (!outOverrideMethod)
            lines ~= ["", "alias "d ~ p.dName ~ " = " ~ conflictClass.fullDType ~ "." ~ p.dName ~ ";"]; // Add an alias for conflicting methods which don't conform

      lines ~= genPropDocs(p, No.Getter);
      lines ~= (outOverrideMethod ? "override "d : ""d) ~ "@property void " ~ p.dName ~ "(" ~ p.fullDType ~ " propval)";

      if (moduleType != ModuleType.Iface)
      {
        Func checkSetter(TypeNode n)
        {
          auto f = cast(Func)n;
          return (f && p.checkSetter(f)) ? f : null;
        }

        auto setter = !p.setter.empty ? checkSetter(dMethodHash.get(repo.defs.symbolName(p.setter.camelCase), null)) : null; // Use setter method for improved performance
        if (!setter)
          setter = !p.propSet.empty ? checkSetter(cast(Func)repo.defs.cSymbolHash.get(p.propSet, null)) : null; // Use alternative org.gtk.Property.set attribute as a backup (full C symbol function name)

        if (!setter)
          addImport("gobject.object");

        lines ~= ["{", setter ? ("return " ~ setter.dName ~ "(propval);") : ("gobject.object.ObjectWrap.setProperty!("
          ~ p.fullDType ~ ")(\"" ~ p.name ~ "\", propval);"), "}"];  // Use getProperty if no setter method
      }
      else
        lines[$ - 1] ~= ";";
    }

    return lines;
  }

  /**
   * Write a C structure definition
   * Params:
   *   writer = Code writer
   */
  void writeCStruct(CodeWriter writer)
  { // Recursive function to process embedded struct/union fields
    void recurseStruct(Structure st)
    {
      writer ~= st.genDocs;
      auto typeName = st is this ? st.cType : (st.name ? (st.name.camelCase(true) ~ "Type") : null); // Handles anonymous or named embedded struct/unions

      writer ~= [(st.structType == StructType.Union ? "union"d : "struct"d) ~ (typeName ? (" " ~ typeName) : ""), "{"];

      foreach (fi, f; st.fields)
      {
        writer ~= f.genDocs;

        if (f.directStruct)
        {
          recurseStruct(f.directStruct);
        }
        else if (f.containerType == ContainerType.Array)
        {
          if (f.fixedSize == ArrayNotFixed)
          { // Use array cType if array is not a fixed size
            if (!f.cType.empty)
              writer ~= f.cType ~ " " ~ f.dName ~ ";";
            else
              f.xmlNode.warn("Struct array field is not fixed size and array c:type not set");
          }
          else if (!f.elemTypes.empty && !f.elemTypes[0].cType.empty)
            writer ~= f.elemTypes[0].cType ~ "[" ~ f.fixedSize.to!dstring ~ "] " ~ f.dName ~ ";";
          else
            f.xmlNode.warn("Struct array field is missing c:type attribute");
        }
        else if (f.callback && !f.typeObject) // Directly defined callback field?
          writer ~= "extern(C) " ~ f.callback.getCPrototype ~ " " ~ f.callback.dName ~ ";";
        else // A regular field
        {
          if (!f.cType.empty)
            writer ~= f.cType ~ " " ~ f.dName ~ ";";
          else
            f.xmlNode.warn("Struct field is missing c:type attribute");
        }

        if (fi + 1 < st.fields.length)
          writer ~= "";
      }

      writer ~= ["}", ""];

      if (st != this && typeName)
        writer ~= [typeName ~ " " ~ st.dType ~ ";"]; // dType is the field name
    }

    recurseStruct(this);
  }

  StructType structType; /// Type of structure
  dstring cSymbolPrefix; /// C symbol prefix
  dstring parentType; /// Parent type name (for derived types)
  Structure parentStruct; /// Resolved parent type object

  dstring[] implements; /// Interfaces implemented by structure
  Structure[] implementStructs; /// Resolved interface implementation structures
  dstring[] prerequisites; /// Interface prerequisite types
  Func[] functions; /// Constructors, functions, methods, and virtual methods
  Func[] signals; /// Signals
  Field[] fields; /// Structure fields
  Property[] properties; /// Properties

  DefCode defCode; /// Code from definitions file
  dstring _moduleName; /// Package module file name (without the .d extension, usually just snake_case of origDType)
  Func ctorFunc; /// Primary instance constructor function in functions (not a Gir field)
  Func[] errorQuarks; /// List of GError quark functions for exceptions
  Func[dstring] funcNameHash; /// Hash of functions by GIR name (snake_case)
  TypeNode[dstring] dMethodHash; /// Hash of methods by D name, includes function methods and properties

  bool abstract_; /// Is abstract type?
  bool deprecated_; /// Deprecated?
  bool opaque; /// Opaque structure type
  bool pointer; /// Structure pointer type
  dstring version_; /// Version
  dstring deprecatedVersion; /// Deprecated version

  bool glibFundamental;
  dstring glibGetType; /// GLib get_type function
  dstring glibTypeName; /// GLib type name
  dstring glibGetValueFunc; /// GLib get value function
  dstring glibSetValueFunc; /// GLib set value function
  dstring glibRefFunc; /// GLib ref function
  dstring glibUnrefFunc; /// GLib unref function
  dstring glibTypeStruct; /// GLib class structure
  dstring glibIsGtypeStructFor; /// Indicates what type a class structure belongs to
  dstring copyFunction; /// Record/Union copy function (not seen in the wild, but defined in gir-1.2.rnc - we use it via XML patching)
  dstring freeFunction; /// Record/Union free function (not seen in the wild, but defined in gir-1.2.rnc - we use it via XML patching)
}

/// Type of structure
enum StructType
{
  Class, /// A class
  Interface, /// An interface
  Record, /// A structure record
  Union, /// A union
}

immutable dstring[] StructTypeValues = ["class", "interface", "record", "union"];

/// Module file type
enum ModuleType
{
  Normal, /// Normal module file
  Iface, /// Interface definition file
  IfaceTemplate, /// Interface mixin template file
}

/**
 * Check if a class structure is an ancestor of another. Takes TypeNode objects for convenience.
 * Params:
 *   possibleParent = A possible parent class
 *   possibleChild = A possible child class of parent
 * Returns: true if both objects are Structure objects of StructType.Class and possibleParent is an ancestor of possibleChild, false otherwise
 */
bool structIsDerived(TypeNode possibleParent, TypeNode possibleChild)
{
  auto pClass = cast(Structure)possibleParent;
  auto cClass = cast(Structure)possibleChild;

  if (!pClass || !cClass || pClass.structType != StructType.Class || cClass.structType != StructType.Class)
    return false;

  for (auto c = cClass.parentStruct; c; c = c.parentStruct)
    if (c is pClass)
      return true;

  return false;
}
