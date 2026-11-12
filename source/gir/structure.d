module gir.structure;

import std.typecons : Tuple;

import code_writer;
import defs;
import gir.base;
import gir.field;
import gir.func;
import gir.func_writer;
import gir.param;
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

  override bool isPlainData()
  {
    if (!_isPlainDataSet && resolved)
    {
      if (kind == TypeKind.Wrap)
      {
        _isPlainData = true;

        foreach (f; fields)
          _isPlainData &= f.isPlainData;

      }
      else if (kind != TypeKind.Unknown)
        _isPlainData = kind.isPlainData;

      _isPlainDataSet = true;
    }

    return _isPlainData;
  }

  /**
   * Are values of this type borrowed (D wrapper points at the C structure and keeps its owner alive) when passed from
   * C to D without ownership transfer, rather than copied. Wrap structures with pointer fields are borrowed by default
   * (a copy would share pointers which the C owner may free), can be overridden with the "gid-borrow" def attribute.
   */
  @property bool borrowed()
  {
    if (borrowAttr.length > 0)
      return borrowAttr == "1";

    return kind == TypeKind.Wrap && !isPlainData;
  }

  /**
   * Are owned instances of this Wrap structure C allocated (held by pointer and freed with freeFunction, deep copied
   * with copyFunction), rather than held inline by the D wrapper (see StructWrap._cAllocated).
   */
  @property bool cAllocated()
  {
    return kind == TypeKind.Wrap && freeFunction.length > 0;
  }

  /**
   * Do caller allocated structures of this type own their members once filled by the callee (C allocated Wrap with an
   * init function, see gid-init-function). Caller allocated Out parameters are then allocated with g_malloc0 and taken
   * by the wrapper. Otherwise filled members may be borrowed (transfer none), so the filled structure is copied.
   */
  @property bool initOwnsMembers()
  {
    return cAllocated && initFunction.length > 0;
  }

  override @property bool inModule()
  {
    with (TypeKind) return kind.among(Struct, Opaque, Wrap, Boxed, Reffed, Object, Interface, Namespace) != 0;
  }

  override @property bool inGlobal()
  {
    with (TypeKind) return kind.among(StructAlias, Pointer) != 0;
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
    borrowAttr = node.get("gid-borrow"); // Custom gid attribute set by def rule: "1" to force borrowing, "0" to copy
    cLayout = node.get("gid-c-layout") == "1"; // Custom gid attribute: emit GIR field layout in c/types.d when opaque
    glibTypeName = node.get("glib:type-name");
    glibGetValueFunc = node.get("glib:get-value-func");
    glibSetValueFunc = node.get("glib:set-value-func");
    glibRefFunc = node.get("glib:ref-func");
    glibUnrefFunc = node.get("glib:unref-func");
    glibTypeStruct = node.get("glib:type-struct");
    glibIsGtypeStructFor = node.get("glib:is-gtype-struct-for");
    copyFunction = node.get("copy-function");
    freeFunction = node.get("free-function");
    initFunction = node.get("gid-init-function"); // Custom gid attribute: initializes a new C allocated Wrap structure
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
    if (structType == StructType.Record || structType == StructType.Union)
    {
      if (opaque || pointer || freeFunction)
      {
        if (!glibGetType.empty)
          return TypeKind.Boxed;

        return functions.canFind!(x => x.active == Active.Enabled) ? TypeKind.Opaque : TypeKind.Pointer;
      }

      auto retKind = TypeKind.Struct;
      foreach (field; fields)
      {
        if (field.kind == TypeKind.Unknown)
          retKind = TypeKind.Unknown;
        else if (field.containerType == ContainerType.Array)
        {
          if (field.elemTypes.empty || field.elemTypes[0].kind == TypeKind.Unknown)
            retKind = TypeKind.Unknown;
          else if (field.cType.canFind("*") || !field.elemTypes[0].kind.among(TypeKind.Basic, TypeKind.BasicAlias,
              TypeKind.Callback, TypeKind.Enum, TypeKind.Flags))
            return glibGetType.empty ? TypeKind.Wrap : TypeKind.Boxed;
        }
        else if (field.containerType != ContainerType.None || (!field.callback && (field.cType.canFind("*")
            || !field.kind.among(TypeKind.Basic, TypeKind.BasicAlias, TypeKind.Callback, TypeKind.Enum, TypeKind.Flags))))
          return glibGetType.empty ? TypeKind.Wrap : TypeKind.Boxed;
      }

      if (retKind == TypeKind.Struct)
        return (functions.empty && !cast(Field)parent) ? TypeKind.StructAlias : TypeKind.Struct; // StructAlias if no functions and not a direct field structure, Struct otherwise
      else
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

  protected override void fixup()
  {
    import std.string : chomp;

    // Handle module name derivation from type name.
    // For types with underscores (common in Harfbuzz, e.g., hb_face_t), strip the _t suffix
    // that is conventional in C but not needed for D module names.
    // For other types, convert PascalCase to snake_case for the module name.
    // Note: Module names can be explicitly overridden via definition files if needed.
    if (origDType.canFind('_'))
      _moduleName = repo.defs.symbolName(origDType.snakeCase.chomp("_t"));
    else
      _moduleName = repo.defs.symbolName(origDType.snakeCase);

    auto parentField = cast(Field)parent; // Structure as a field of another structure?

    if (parentField)
    { // dType and cType are the field name (not an actual type)
      dType = parentField.dName;
      cType = repo.defs.symbolName(origCType);
      kind = TypeKind.Struct;
    }
    else
      super.fixup;

    if (cType.empty) // If cType is unset, set it to glibTypeName
      cType = glibTypeName;

    if (dType in repo.kindSubs) // Substitute type kinds
    {
      kind = repo.kindSubs[dType];
      repo.kindSubsApplied[dType] = true;
    }

    foreach (f; fields) // Fixup structure fields
    {
      f.doFixup;

      if (f.active == Active.Enabled && (opaque || pointer)) // Set opaque and pointer fields to inactive
        f.active = Active.Unsupported;
    }

    foreach (p; properties) // Fixup object properties
      p.doFixup;

    foreach (fn; functions) // Fixup structure function/methods
    {
      fn.doFixup;

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
      sg.doFixup;
  }

  protected override void resolve()
  {
    foreach (f; fields) // Resolve structure fields
      f.doResolve;

    foreach (p; properties) // Resolve object properties
      p.doResolve;

    foreach (fn; functions) // Resolve structure function/methods
      fn.doResolve;

    foreach (sg; signals) // Resolve structure signals
      sg.doResolve;

    if (kind == TypeKind.Unknown)
      kind = calcKind;

    if (!cast(Field)parent) // Don't call super.resolve if this is a direct structure of a field
      super.resolve;

    if (kind == TypeKind.Boxed && parentType.empty && !(repo.namespace == "GObject" && name == "Boxed")) // Boxed types derive from GObject.Boxed (except GObject.Boxed itself)
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

  protected override void verify()
  {
    if (active != Active.Enabled || cast(Field)parent) // Don't verify if structure is disabled or a field structure
      return;

    super.verify;

    if (!parentType.empty && !parentStruct)
      throw new Exception("Failed to resolve parent type '" ~ parentType.to!string ~ "'");
 
    if (parentStruct && parentStruct.active != Active.Enabled)
      throw new Exception("Structure parent type '" ~ parentStruct.fullDName.to!string ~ "' is disabled");

    foreach (ifaceName; implements)
      if (!cast(Structure)repo.findTypeObject(ifaceName))
      {
        warnWithLoc(__FILE__, __LINE__, xmlLocation, "Unable to resolve structure " ~ fullDName.to!string ~ " interface " ~ ifaceName.to!string);
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
        warnWithLoc(__FILE__, __LINE__, fn.xmlLocation, "Disabling function '" ~ fn.fullDName.to!string ~ "' of type '" ~ fn.funcType.to!string
            ~ "' which is not supported");
        TypeNode.dumpSelectorOnWarning(fn);
      }
      else
        fn.doVerify;
    }

    foreach (sig; signals) // Verify structure signals
    {
      if (sig.active != Active.Enabled)
        continue;

      try
        sig.doVerify;
      catch (Exception e)
      {
        sig.active = Active.Unsupported;
        warnWithLoc(e.file, e.line, sig.xmlLocation, "Disabling signal '" ~ sig.fullDName.to!string ~ "': " ~ e.msg);
        TypeNode.dumpSelectorOnWarning(sig);
      }
    }

    foreach (f; fields) // Verify structure fields
    {
      with (TypeKind)
        if (!kind.among(StructAlias, Struct, Wrap, Boxed, Reffed)) // Ignore fields in structures without field support
          f.active = Active.Ignored;

      if (f.active != Active.Enabled)
        continue;

      // Fields of wrapped structures are written as property methods, disable fields which conflict with a method name
      with (TypeKind) if (kind.among(Wrap, Boxed, Reffed))
      {
        if (auto fn = cast(Func)dMethodHash.get(f.dName, null))
        {
          if (fn.active == Active.Enabled)
          {
            f.active = Active.Ignored;
            infoWithLoc(__FILE__, __LINE__, f.xmlLocation, "Ignoring field '" ~ f.fullDName.to!string
              ~ "' which conflicts with an identically named method");
            continue;
          }
        }
      }

      try
        f.doVerify;
      catch (Exception e)
      {
        f.active = Active.Unsupported;
        warnWithLoc(e.file, e.line, f.xmlLocation, "Disabling field '" ~ f.fullDName.to!string ~ "': " ~ e.msg);
        TypeNode.dumpSelectorOnWarning(f);
      }
    }

    foreach (p; properties) // Verify object properties
    {
      if (p.active != Active.Enabled)
        continue;

      try
      {
        p.doVerify;
        dMethodHash[p.dName] = p; // Add to method hash to detect conflicting method names
      }
      catch (Exception e)
      {
        p.active = Active.Unsupported;
        warnWithLoc(e.file, e.line, p.xmlLocation, "Disabling property '" ~ p.fullDName.to!string ~ "': " ~ e.msg);
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
    codeTrap("struct.write", fullDName);

    auto isIfaceTemplate = kind == TypeKind.Interface && moduleType == ModuleType.IfaceTemplate;
    auto writer = new CodeWriter(buildPath(path, moduleName.to!string ~ (isIfaceTemplate ? "_mixin" : "") ~ ".d")); // Append T to type name for interface mixin template module

    dstring modType;

    if (isIfaceTemplate)
      modType = "interface mixin";
    else if (structType == StructType.Interface)
      modType = "interface";
    else if (moduleType == ModuleType.Struct)
      modType = "struct";
    else
      modType = "class";

    if (dType)
      writer ~= "/// Module for [" ~ dType ~ "] " ~ modType;

    writer ~= ["module " ~ fullModuleName ~ (isIfaceTemplate ? "_mixin;"d : ";"d), ""];

    beginImports(this);
    scope(exit) endImports;

    if (fullModuleName != "gid.basictypes")
      writer ~= "public import gid.basictypes;";

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

    if (kind == TypeKind.Interface || kind == TypeKind.Object)
      importManager.add("gobject.gid_builder");

    if (!(defCode.inhibitFlags & DefInhibitFlags.Imports) && kind == TypeKind.Interface)
      writer ~= "public import " ~ fullModuleName ~ "_iface_proxy;";

    auto importLine = writer.length; // Save position where imports should go, which are inserted later, to gather all dependencies

    if (defCode.preClass.length > 0)
      writer ~= defCode.preClass;

    if (moduleType != ModuleType.Struct && kind != TypeKind.Namespace) // Struct type docs are handled in writeStructDef
      writer ~= genDocs;

    Structure[] objIfaces;

    if (defCode.classDecl.empty)
    {
      if (kind == TypeKind.Interface)
        writer ~= [isIfaceTemplate ? ("template " ~ dType ~ "T()") : ("interface " ~ dType ~ " : GidWrap"), "{"];
      else if (moduleType == ModuleType.Struct) // Struct module?
        writeStructDef(writer, true);  // Write structure definition using D types
      else if (kind != TypeKind.Namespace)
      { // Create range of parent type and implemented interface types, but filter out interfaces already implemented by ancestors
        objIfaces = implementStructs.filter!(x => !getIfaceAncestor(x)).array;
        auto parentAndIfaces = ((parentStruct && !usesBoxedWrap) ? [parentStruct.fullDType]
          : ((usesBoxedWrap || kind.among(TypeKind.Opaque, TypeKind.Reffed, TypeKind.Wrap)) ? [wrapBaseClass] : []))
          ~ objIfaces.map!(x => x.fullDType).array;
        writer ~= ["class " ~ dType ~ (!parentAndIfaces.empty ? " : " ~ parentAndIfaces.join(", ") : ""), "{"];
      }
    }
    else
    {
      writer ~= defCode.classDecl;
      writer ~= "{";
    }

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
        if (fn.active == Active.Enabled
          && (moduleType != ModuleType.Struct || fn.funcType != FuncType.Constructor)) // Structs ignore constructors
        {
          writer ~= "";
          (new FuncWriter(fn, moduleType)).write(writer);
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

    if (kind != TypeKind.Namespace)
      writer ~= "}";

    if (kind == TypeKind.Interface || kind == TypeKind.Object)
      writeBuilder(writer, moduleType);

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

  /**
   * Check if this is a Boxed type which derives directly from the root Boxed class and therefore uses the BoxedWrap
   * template base class, which provides the GType and static C copy/free functions.
   * Returns: true if the BoxedWrap template base class is used
   */
  bool usesBoxedWrap()
  {
    return kind == TypeKind.Boxed && parentStruct && parentStruct.kind == TypeKind.Boxed && !parentStruct.parentStruct
      && !glibGetType.empty && glibGetType != "intern";
  }

  /// Opaque, root Reffed, Wrap or root Boxed base class template instance (OpaqueWrap, ReffedWrap, StructWrap or BoxedWrap)
  dstring wrapBaseClass()
  {
    if (kind == TypeKind.Boxed)
      return parentStruct.fullModuleName ~ ".BoxedWrap!(" ~ glibGetType ~ ")";

    auto ptrType = pointer ? cType : cTypeRemPtr ~ "*";

    if (kind == TypeKind.Wrap)
      return "StructWrap!(" ~ cTypeRemPtr ~ (copyFunction ? ", " ~ copyFunction : "")
        ~ (freeFunction ? ", " ~ freeFunction : "") ~ ")";

    if (kind == TypeKind.Reffed)
      return "ReffedWrap!(" ~ ptrType ~ ", " ~ glibRefFunc ~ ", " ~ glibUnrefFunc ~ ")";

    return "OpaqueWrap!(" ~ ptrType ~ ", " ~ (freeFunction ? freeFunction : "void") ~ ", "
      ~ (copyFunction ? copyFunction : "void") ~ ")";
  }

  // Write class init code
  private void writeInitCode(CodeWriter writer, ModuleType moduleType)
  {
    if (kind == TypeKind.Struct && glibGetType.length == 0) // Struct without a glibGetType function is not a boxed type
      return;

    // Boxed structures with defined structures can be allocated, add ctor without args
    if (kind == TypeKind.Boxed && !ctorFunc && !opaque && !pointer && !fields.empty)
      writer ~= writeBoxedOrWrapCtor!"boxed";

    // Create constructors for Wrap structures without a constructor. C allocated Wrap structures with an init function
    // (gid-init-function) get a constructor which allocates and initializes the C structure instead of field arguments
    // (field defaults would overwrite the initialized values).
    if (kind == TypeKind.Wrap && !ctorFunc && cAllocated && initFunction.length > 0)
      writer ~= ["/**", "    Create a new C allocated `" ~ dName ~ "` initialized with `" ~ initFunction ~ "()`.", "*/",
        "this() nothrow", "{", "auto _ptr = cast(" ~ cTypeRemPtr ~ "*)gMalloc(" ~ cTypeRemPtr ~ ".sizeof);",
        initFunction ~ "(_ptr);", "super(cast(void*)_ptr, Yes.Take);", "}"];
    else if (kind == TypeKind.Wrap && !ctorFunc && !fields.empty)
      writer ~= writeBoxedOrWrapCtor!"wrap";

    with (TypeKind) if (kind.among(Opaque, Reffed, Wrap, Boxed, Object))
      writer ~= ["", "/** */", "this(void* ptr, Flag!\"Take\" take) nothrow", "{", "super(ptr, take);", "}"];

    with (TypeKind) if (kind.among(Opaque, Wrap)) // Borrowed structure constructor (see StructWrap/OpaqueWrap)
      writer ~= ["", "/** */", "this(void* ptr, Object owner) nothrow", "{", "super(ptr, owner);", "}"];

    // Reffed types with a GType (not "intern" fundamental types, which are handled in definitions)
    auto reffedGType = kind == TypeKind.Reffed && !glibGetType.empty && glibGetType != "intern";

    // Boxed types using the BoxedWrap template get _getGType, _gType, _cDup and _cFree from it
    auto boxedGType = kind == TypeKind.Boxed && !usesBoxedWrap;

    with (TypeKind) if (kind.among(Struct, Object) || boxedGType || reffedGType
        || (kind == Interface && moduleType == ModuleType.Iface))
      writer ~= ["", "/** */", "static GType _getGType() nothrow", "{", "import gid.loader : gidSymbolNotFound;",
        "return cast(void function())" ~ glibGetType
        ~ " != &gidSymbolNotFound ? " ~ glibGetType ~ "() : cast(GType)0;", "}"]; // Return 0 if get_type() function was not resolved

    auto overrideStr = (kind.among(TypeKind.Object, TypeKind.Boxed) || reffedGType) ? "override "d : ""d;

    if (kind.among(TypeKind.Struct, TypeKind.Object) || boxedGType || reffedGType)
      writer ~= ["", "/** */", overrideStr ~ "@property GType _gType() nothrow", "{", "return _getGType();", "}"];

    if (boxedGType || kind == TypeKind.Struct) // Boxed copy and free functions require the type's GType
      writer ~= ["", "/** */", "static void* _cDup(void* ptr) nothrow", "{", "return boxedCopy(_getGType, ptr);", "}",
        "", "/** */", "static void _cFree(void* ptr) nothrow", "{", "boxedFree(_getGType, ptr);", "}"];

    dstring[] mapAliases; // MapScope aliases of map functions (see Func.mapScopeName)
    foreach (f; mapScopeFuncs)
    {
      auto name = f.mapScopeName;
      if (!mapAliases.canFind(name))
      {
        mapAliases ~= name;
        writer ~= ["", "/** Scope of a structure mapped by [" ~ f.fullDName ~ "] (see [gid.wrap.MapScope]) */",
          "alias " ~ name ~ " = " ~ f.mapScopeTemplate ~ ";"];
      }
    }

    if (kind.among(TypeKind.Boxed, TypeKind.Object))
      writer ~= ["", "/** Returns `this`, for use in `with` statements. */", overrideStr ~ dType ~ " self() nothrow",
        "{", "return this;", "}"];

    if (kind == TypeKind.Object)
      writer ~= ["", "/**", "    Get builder for [" ~ fullDType ~ "]", "    Returns: New builder object", "*/",
        "static " ~ dType ~ "GidBuilder builder() nothrow", "{", "return new " ~ dType ~ "GidBuilder;", "}"];
  }

  /// Get the active map functions which have their MapScope alias in this class (see Func.mapScopeClass)
  Func[] mapScopeFuncs()
  {
    return chain(functions, repo.globalStruct.functions).filter!(f => f.active == Active.Enabled
      && f.unmapFunc.length > 0 && f.mapScopeClass is this).array;
  }

  // Write a Boxed type constructor with all fields as parameters with default values (optional)
  private dstring writeBoxedOrWrapCtor(string BoxedOrWrap)()
  {
    dstring s = "/**\n    Create a `" ~ dName ~ "` " ~ BoxedOrWrap ~ " type.\n";
    bool paramsShown;

    bool isFieldCandidate(Field f) { return f.active == Active.Enabled && f.writable && !f.arrayFields; }

    foreach (f; fields)
    {
      if (isFieldCandidate(f))
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
      if (isFieldCandidate(f))
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
        {
          if (fieldType.endsWith(']'))
            s ~= fieldType ~ " " ~ f.dName ~ " = (" ~ fieldType ~ ").init"; // Use parenthesis for static array field types (end with [size])
          else
            s ~= fieldType ~ " " ~ f.dName ~ " = " ~ fieldType ~ ".init"; // Otherwise use types .init value
        }
      }
    }

    s ~= ") nothrow\n{\n";
    
    static if (BoxedOrWrap == "boxed")
      s ~= "super(boxedNew!" ~ cType ~ "(_getGType), Yes.Take);\n";

    foreach (f; fields)
      if (isFieldCandidate(f))
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
      if (f.active != Active.Enabled || f.arrayFields) // Skip inactive fields or those which are array lengths
        continue;

      // Wrap structure object fields keep a reference to the D object, since the C field borrows its pointer (unless the
      // field value is owned by a C allocated structure)
      if (kind == TypeKind.Wrap && f.writable && !f.owned
          && f.kind.among(TypeKind.Boxed, TypeKind.Reffed, TypeKind.Object, TypeKind.Interface))
        lines ~= ["", "private " ~ f.fullDType ~ " _" ~ f.dName ~ "Ref; // Keeps borrowed field value alive"];
      else if (kind == TypeKind.Wrap && f.writable && !f.owned && f.kind == TypeKind.String) // Wrap may be borrowed (C memory not scanned by the GC)
        lines ~= ["", "private const(char)* _" ~ f.dName ~ "Ref; // Keeps borrowed GC string alive"];

      assert(!f.directStruct, "Unsupported embedded structure field " ~ f.fullDName.to!string);

      if (f.kind == TypeKind.Callback && !f.typeObject) // Callback function type directly defined in field?
        lines ~= ["", "/** Function alias for field `"~ f.dName ~"` */",
          "alias " ~ f.name.camelCase(true) ~ "FuncType = extern(C) "
          ~ f.callback.getCPrototype ~ ";"]; // Add a type alias, since extern(C) can't be used directly in arg definition

      lines ~= genPropDocs(f, PropMethodType.Getter);

      if (f.kind != TypeKind.Callback)
        lines ~= ["@property " ~ f.fullDType ~ " " ~ f.dName ~ "() nothrow", "{"];

      dstring addrIfNeeded() // Returns an & if field is a direct structure, when we need a pointer to it
      {
        with (TypeKind) return (f.kind.among(StructAlias, Struct, Boxed, Reffed) && f.cType.countStars == 0) ? "&"d : "";
      }

      final switch (f.kind) with (TypeKind)
      {
        case Basic, BasicAlias, Pointer:
          if (f.dType == "bool")
            lines ~= "return cast(bool)" ~ cPtr ~ "." ~ f.dName ~ ";";
          else
            lines ~= "return " ~ cPtr ~ "." ~ f.dName ~ ";";
          break;
        case Enum, Flags:
          lines ~= "return cast(" ~ f.fullDType ~ ")" ~ cPtr ~ "." ~ f.dName ~ ";";
          break;
        case Callback:
          if (f.typeObject) // Callback function is an alias type?
            lines ~= ["@property " ~ f.cType ~ " " ~ f.dName ~ "() nothrow", "{"];
          else // Callback function type is directly defined in field
            lines ~= ["@property " ~ f.name.camelCase(true) ~ "FuncType " ~ f.dName ~ "() nothrow", "{"];

          lines ~= "return " ~ cPtr ~ "." ~ f.dName ~ ";";
          break;
        case Container:
          if (f.containerType == ContainerType.Array)
            lines ~= constructFieldArrayGetter(f);
          else
            lines ~= constructFieldContainerGetter(f);
          break;
        case String, StructAlias, Struct, Object, Boxed, Reffed, Interface:
          lines ~= "return cToD!(" ~ f.fullDType ~ ")(cast(void*)" ~ addrIfNeeded ~ cPtr ~ "." ~ f.dName ~ ");";
          break;
        case Opaque, Wrap:
          auto st = cast(Structure)f.typeObject;
          auto starCount = f.cType.retro.countUntil!(x => x != '*');

          auto ownerArg = st && st.borrowed ? "this"d : "No.Take"d; // Borrowed structures are kept alive by this object

          if (starCount < 1 && !(st && st.pointer)) // pointer typedefs (GdkAtom) are already pointers
            lines ~= "return new " ~ f.fullDType ~ "(cast(" ~ f.cType.stripConst ~ "*)&" ~ cPtr ~ "." ~ f.dName
              ~ ", " ~ ownerArg ~ ");";
          else if (st && st.borrowed) // Null pointer would select the inline structure of a borrowed wrapper
            lines ~= "return " ~ cPtr ~ "." ~ f.dName ~ " ? new " ~ f.fullDType ~ "(cast(" ~ f.cType.stripConst ~ ")"
              ~ cPtr ~ "." ~ f.dName ~ ", this) : null;";
          else
            lines ~= "return new " ~ f.fullDType ~ "(cast(" ~ f.cType.stripConst ~ ")" ~ cPtr ~ "." ~ f.dName
              ~ ", No.Take);";
          break;
        case Unknown, Namespace:
          throw new Exception(
              "Unhandled readable field property type '" ~ f.fullDType.to!string ~ "' (" ~ f.kind.to!string
              ~ ") for struct " ~ fullDType.to!string);
      }

      lines ~= "}";

      if (!f.writable)
        continue;

      lines ~= genPropDocs(f, PropMethodType.Setter);

      if (f.kind != TypeKind.Callback) // Callback setter declaration is specially handled below
        lines ~= ["@property void " ~ f.dName ~ "(" ~ f.fullDType ~ " propval) nothrow", "{"];

      // D memory stored in a borrowed C structure is kept alive only by this wrapper, which the structure may outlive
      // (owned field values are C memory freed by the structure's free function)
      if (borrowed && !f.owned
          && f.kind.among(TypeKind.String, TypeKind.Boxed, TypeKind.Reffed, TypeKind.Object, TypeKind.Interface))
        lines ~= ["if (_refuseBorrowedStore(\"" ~ f.dName ~ "\"))", "  return;", ""];

      final switch (f.kind) with (TypeKind)
      {
        case Basic, BasicAlias:
          lines ~= cPtr ~ "." ~ f.dName ~ " = propval;";
          break;
        case Enum, Flags:
          lines ~= cPtr ~ "." ~ f.dName ~ " = cast(" ~ f.cType ~ ")propval;";
          break;
        case StructAlias, Struct: // Only embedded (non-pointer) structures are writable
          lines ~= cPtr ~ "." ~ f.dName ~ " = cast(" ~ f.cType ~ ")propval;";
          break;
        case Callback:
          if (f.typeObject) // Callback function is an alias type?
            lines ~= ["", "@property void " ~ f.dName ~ "(" ~ f.cType ~ " propval) nothrow", "{"];
          else // Callback function type is directly defined in field
            lines ~= ["", "@property void " ~ f.dName ~ "(" ~ f.name.camelCase(true) ~ "FuncType propval) nothrow", "{"];

          lines ~= cPtr ~ "." ~ f.dName ~ " = propval;";
          break;
        case Container:
          if (f.containerType == ContainerType.Array)
            lines ~= constructFieldArraySetter(f);
          else
            assert(0, "Write of container fields is not supported"); // Verify should prevent this
          break;
        case String:
          if (kind == Wrap && !f.owned) // Borrowed GC string, kept alive by _<field>Ref (a borrowed C structure is not scanned by the GC)
            lines ~= ["_" ~ f.dName ~ "Ref = toCString!(No.Malloc, Yes.Nullable)(propval);",
              cPtr ~ "." ~ f.dName ~ " = cast(" ~ f.cType ~ ")_" ~ f.dName ~ "Ref;"];
          else // Owned field (enabled via definitions), null D strings are stored as NULL
            lines ~= ["cValueFree!(string)(cast(void*)" ~ cPtr ~ "." ~ f.dName ~ ");",
              cPtr ~ "." ~ f.dName ~ " = toCString!(Yes.Malloc, Yes.Nullable)(propval);"];
          break;
        case Boxed, Reffed, Object, Interface:
          if (kind == Wrap && !f.owned) // Borrowed pointer, D object kept alive by _<field>Ref
          {
            if (f.kind == Interface)
              addImport("gobject.object");
 
            auto objExpr = f.kind == Interface ? "(cast(gobject.object.ObjectWrap)propval)"d : "propval"d;
            lines ~= ["_" ~ f.dName ~ "Ref = propval;", cPtr ~ "." ~ f.dName ~ " = propval ? cast(" ~ f.cType ~ ")"
              ~ objExpr ~ "._cPtr(No.Dup) : null;"];
          }
          else // Owned field (enabled via definitions)
            lines ~= ["cValueFree!(" ~ f.fullDType ~ ")(cast(void*)" ~ cPtr ~ "." ~ f.dName ~ ");",
              "dToC(propval, cast(void*)&" ~ cPtr ~ "." ~ f.dName ~ ");"];
          break;
        case Pointer, Opaque, Wrap, Namespace, Unknown:
          throw new Exception("Unhandled writable field property type '" ~ f.fullDType.to!string ~ "' (" ~ f
              .kind.to!string ~ ") for struct " ~ fullDType.to!string);
      }

      lines ~= "}";
    }

    return lines;
  }

  // Construct an array field getter
  private dstring[] constructFieldArrayGetter(Field field)
  {
    auto cPtr = "(cast(" ~ (cType.countStars > 0 ? cTypeRemPtr : cType) ~ "*)this._cPtr)";
    auto elemType = field.elemTypes[0];
    dstring[] lines;
    dstring lengthStr;

    if (field.lengthField) // Array has length field? (the array can be null, regardless of the length)
    {
      lines ~= ["if (!" ~ cPtr ~ "." ~ field.dName ~ ")", "return null;", ""];
      lengthStr = cPtr ~ "." ~ field.lengthField.dName;
    }
    else if (field.fixedSize != ArrayNotFixed) // Array is a fixed size?
      lengthStr = field.fixedSize.to!dstring;
    else if (field.zeroTerminated) // Array is zero terminated?
    {
      lines ~= ["uint len;", "if (" ~ cPtr ~ "." ~ field.dName ~ ")", "{", "for (; " ~ cPtr ~ "."
        ~ field.dName ~ "[len] " ~ (elemType.cType.endsWith("*") ? "!is null"d : "!= 0")
        ~ "; len++)", "{", "}", "}", ""];
      lengthStr = "len";
    }
    else
      assert(0); // This should be prevented by verify()

    dstring declareRetval(dstring elemDType)
    {
      if (field.fixedSize != ArrayNotFixed)
        return elemDType ~ "[" ~ lengthStr ~ "] _retval;";
      else
        return "auto _retval = new " ~ elemDType ~ "[](" ~ lengthStr ~ ");";
    }

    final switch (elemType.kind) with (TypeKind)
    {
      case Basic, BasicAlias, Enum, Flags, StructAlias, Struct, Pointer:
        if (field.dType == "bool")
          lines ~= [declareRetval("bool"), "foreach (i; 0 .. " ~ lengthStr ~ ")", "_retval[i] = " ~ cPtr ~ "."
            ~ field.dName ~ "[i];", "return _retval;"];
        else if (field.fixedSize != ArrayNotFixed)
          lines ~= "return *cast(" ~ field.fullDType ~ "*)&" ~ cPtr ~ "." ~ field.dName ~ ";";
        else if (elemType.kind == Struct) // D Struct types have the C layout
          lines ~= "return (cast(" ~ elemType.fullDType ~ "*)" ~ cPtr ~ "." ~ field.dName ~ ")[0 .. " ~ lengthStr
            ~ "].dup;";
        else
          lines ~= "return " ~ cPtr ~ "." ~ field.dName ~ "[0 .. " ~ lengthStr ~ "].dup;";
        break;
      case String:
        lines ~= [declareRetval("string"), "foreach (i; 0 .. " ~ lengthStr ~ ")", "_retval[i] = " ~ cPtr ~ "."
          ~ field.dName ~ "[i].fromCString;", "return _retval;"];
        break;
      case Opaque, Wrap, Boxed, Reffed:
        auto addrOf = elemType.cType.countStars == 0 ? "&"d : "";
        lines ~= [declareRetval(elemType.fullDType), "foreach (i; 0 .. " ~ lengthStr ~ ")", "_retval[i] = new "
          ~ elemType.fullDType ~ "(cast(void*)" ~ addrOf ~ cPtr ~ "." ~ field.dName ~ "[i], No.Take);",
          "return _retval;"];
        break;
      case Object, Interface:
        addImport("gobject.object");

        lines ~= [declareRetval(elemType.fullDType), "foreach (i; 0 .. " ~ lengthStr ~ ")",
          "_retval[i] = gobject.object.ObjectWrap._getDObject!(" ~ elemType.fullDType ~ ")(cast(void*)" ~ cPtr
          ~ "." ~ field.dName ~ "[i], No.Take);", "return _retval;"];
        break;
      case Unknown, Callback, Container, Namespace:
        assert(0, "Unsupported field array type '" ~ elemType.fullDType.to!string ~ "' (" ~ elemType.kind.to!string
            ~ ") for " ~ field.fullDName.to!string);
    }

    return lines;
  }

  // Construct an array field setter
  private dstring[] constructFieldArraySetter(Field field)
  {
    auto cPtr = "(cast(" ~ (cType.countStars > 0 ? cTypeRemPtr : cType) ~ "*)this._cPtr)";
    auto elemType = field.elemTypes[0];
    dstring[] lines;

    assert(field.fixedSize != ArrayNotFixed, "Write is only supported for fixed size array fields"); // Should be prevented by verify

    auto lengthStr = field.fixedSize.to!dstring;

    final switch (elemType.kind) with (TypeKind)
    {
      case Basic, BasicAlias, Enum, Flags, StructAlias, Struct:
        if (field.dType == "bool")
          lines ~= ["foreach (i; 0 .. " ~ lengthStr ~ ")", cPtr ~ "." ~ field.dName ~ "[i] = propval[i];"];
        else
          lines ~= cPtr ~ "." ~ field.dName ~ " = *cast(" ~ elemType.cType ~ "[" ~ lengthStr ~ "]*)&propval;";
        break;
      case Pointer, String, Opaque, Wrap, Boxed, Reffed, Object, Interface, Unknown, Callback, Container, Namespace:
        assert(0, "Unsupported field array type '" ~ elemType.fullDType.to!string ~ "' (" ~ elemType.kind.to!string
            ~ ") for " ~ field.fullDName.to!string);
    }

    return lines;
  }

  // Construct a container field getter (except array)
  private dstring[] constructFieldContainerGetter(Field field)
  {
    auto cPtr = "(cast(" ~ (cType.countStars > 0 ? cTypeRemPtr : cType) ~ "*)this._cPtr)";
    dstring toDParams;

    final switch (field.containerType) with(ContainerType)
    {
      case ByteArray:
        break;
      case ArrayG, PtrArray, List, SList:
        toDParams = field.elemTypes[0].fullDType;
        break;
      case HashTable:
        toDParams = field.elemTypes[0].fullDType ~ ", " ~ field.elemTypes[1].fullDType;
        break;
      case Array, None:
        assert(0, "Unsupported container type '" ~ field.containerType.to!string
          ~ "' for " ~ field.fullDName.to!string);
    }

    return ["return g" ~ field.containerType.to!dstring ~ "ToD!(" ~ toDParams ~ ")("
      ~ cPtr ~ "." ~ field.dName ~ ");"];
  }

  /// Property type used with genPropDocs
  enum PropMethodType
  {
    Getter, /// A getter property
    Setter, /// A setter property
    Builder, /// A builder construction property
  }

  /**
   * Generate adrdox documentation for a field or property getter/setter
   * Returns: Lines of generated documentation
   */
  private dstring[] genPropDocs(TypeNode node, PropMethodType methodType)
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

    if (methodType == PropMethodType.Getter)
      lines ~= ["/**"d, "    Get `"d ~ name ~ "` " ~ type ~ "."]
        ~ ("    Returns: "d ~ repo.gdocToDDoc(node.docContent, "      ").stripLeft).splitLines;
    else
    {
      lines ~= ["/**"d, "    Set `"d ~ name ~ "` " ~ type ~ ".", "    Params:"]
      ~ ("      propval = "d ~ repo.gdocToDDoc(node.docContent, "        ").stripLeft).splitLines;

      if (methodType == PropMethodType.Builder)
        lines ~= "    Returns: Builder instance for fluent chaining";
    }

    if (!node.docVersion.empty || !node.docDeprecated.empty)
    {
       lines ~= "";

      if (!node.docVersion.empty)
        lines ~= "    Version: " ~ node.docVersion;

      if (!node.docDeprecated.empty)
        lines ~= ("    Deprecated: " ~ repo.gdocToDDoc(node.docDeprecated, "      ").stripLeft).splitLines;
    }

    return lines ~ "*/";
  }

  // Construct struct wrapper property methods
  private dstring[] constructProps(ModuleType moduleType)
  {
    dstring[] lines;

    foreach (p; properties)
    {
      if (p.active != Active.Enabled)
        continue;

      if (p.readable)
      {
        bool outOverrideMethod;

        if (parentStruct)
          if (auto conflictClass = p.findMethodConflict(parentStruct, Yes.Getter, outOverrideMethod))
            if (!outOverrideMethod)
              lines ~= ["", "alias "d ~ p.dName ~ " = " ~ conflictClass.fullDType ~ "." ~ p.dName ~ ";"]; // Add an alias for conflicting methods which don't conform

        lines ~= genPropDocs(p, PropMethodType.Getter);
        lines ~= (outOverrideMethod ? "override "d : ""d) ~ "@property " ~ p.fullDType ~ " " ~ p.dName ~ "() nothrow";

        if (moduleType != ModuleType.Iface)
        {
          Func chkGetter(TypeNode n)
          {
            auto f = cast(Func)n;
            return (f && p.checkGetter(f)) ? f : null;
          }

          auto getter = !p.getter.empty ? chkGetter(dMethodHash.get(repo.defs.symbolName(p.getter.camelCase), null)) // Use getter method for improved performance
            : null;
          if (!getter)
            getter = !p.propGet.empty ? chkGetter(cast(Func)repo.defs.cSymbolHash.get(p.propGet, null)) : null; // Use alternative org.gtk.Property.get attribute as a backup (full C symbol function name)

          if (!getter)
            addImport("gobject.object");

          lines ~= ["{", getter ? ("return " ~ getter.dName ~ "();") : ("return gobject.object.ObjectWrap.getProperty!("
            ~ p.fullDType ~ ")(\"" ~ p.name ~ "\");"), "}"]; // Use getProperty if no getter method
        }
        else
          lines[$ - 1] ~= ";";
      }

      if (!p.writable || p.constructOnly)
        continue;

      bool outOverrideMethod;

      if (parentStruct)
        if (auto conflictClass = p.findMethodConflict(parentStruct, No.Getter, outOverrideMethod))
          if (!outOverrideMethod)
            lines ~= ["", "alias "d ~ p.dName ~ " = " ~ conflictClass.fullDType ~ "." ~ p.dName ~ ";"]; // Add an alias for conflicting methods which don't conform

      lines ~= genPropDocs(p, PropMethodType.Setter);
      lines ~= (outOverrideMethod ? "override "d : ""d) ~ "@property void " ~ p.dName ~ "(" ~ p.fullDType
        ~ " propval) nothrow";

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

        // Simple structure setter parameters which are nullable are passed by pointer
        dstring setterAddr;
        if (setter)
        {
          with (TypeKind) foreach (sp; setter.params)
          {
            if (!sp.isInstanceParam && sp.isDParam)
            {
              if (sp.kind.among(Struct, StructAlias) && sp.direction == ParamDirection.In
                  && (sp.nullable || sp.optional) && sp.cType.countStars == 1)
                setterAddr = "&";
              break;
            }
          }
        }

        lines ~= ["{", setter ? (setter.dName ~ "(" ~ setterAddr ~ "propval);") : ("gobject.object.ObjectWrap.setProperty!("
          ~ p.fullDType ~ ")(\"" ~ p.name ~ "\", propval);"), "}"];  // Use setProperty if no setter method
      }
      else
        lines[$ - 1] ~= ";";
    }

    return lines;
  }

  /**
   * Write a fluent builder sub class.
   * Params:
   *   writer = The code writer
   */
  void writeBuilder(CodeWriter writer, ModuleType moduleType)
  {
    Structure[] objIfaces;

    writer ~= ["", "/// Fluent builder implementation template for [" ~ fullDType ~ "]"];

    if (kind == TypeKind.Interface)
      writer ~= [moduleType == ModuleType.IfaceTemplate ? ("template " ~ dType ~ "GidBuilderT()")
        : ("interface " ~ dType ~ "GidBuilderImpl(T)"), "{"];
    else
    {
      objIfaces = implementStructs.filter!(x => !getIfaceAncestor(x)).array;
      auto parentAndIfaces = ([parentStruct ? (parentStruct.fullDType ~ "GidBuilderImpl!T")
        : "gobject.gid_builder.GidBuilder!T"] ~ objIfaces.map!(x => x.fullDType ~ "GidBuilderImpl!T").array).join(", ");

      writer ~= ["class " ~ dType ~ "GidBuilderImpl(T) : " ~ parentAndIfaces, "{"];

      if (implementStructs.length > 0)
        writer ~= [""d] ~ objIfaces.map!(x => "mixin " ~ x.dType ~ "GidBuilderT!();").array;

      // Is there def file code to insert into this Builder (keyed by module name)?
      if (auto defCode = repo.modDefCode.get(_moduleName ~ "_gid_builder", null))
        if (defCode.inClass.length > 0)
          writer ~= defCode.inClass;
    }

    foreach (p; properties.filter!(p => p.active == Active.Enabled && p.writable)) // Loop over writable properties (also construct-only)
    {
      writer ~= genPropDocs(p, PropMethodType.Builder);
      writer ~= (builderPropOverride(p) ? "override "d : "") ~ "T " ~ p.dName ~ "("
        ~ p.fullDType ~ " propval) nothrow" ~ (moduleType == ModuleType.Iface ? ";"d : ""d);

      if (moduleType != ModuleType.Iface)
        writer ~= ["{", "return setProperty(\"" ~ p.name ~ "\", propval);", "}"];
    }

    writer ~= "}";

    if (moduleType == ModuleType.Normal)
    {
      writer ~= ["", "/// Fluent builder for [" ~ fullDType ~ "]", "final class " ~ dType ~ "GidBuilder : "
        ~ dType ~ "GidBuilderImpl!" ~ dType ~ "GidBuilder", "{", "/**", "    Create object from builder.",
        "    Returns: New object", "*/", dType ~ " build() nothrow", "{", "return new "
        ~ dType ~ "(cast(void*)createGObject(" ~ dType ~ "._getGType), Yes.Take);", "}", "}"];
    }
  }

  // Check if a builder property should be overridden (duplicate write property in a parent class)
  private bool builderPropOverride(Property prop)
  {
    for (auto klass = parentStruct; klass; klass = klass.parentStruct)
    {
      foreach (p; klass.properties.filter!(p => p.active == Active.Enabled && p.writable))
        if (p.dName == prop.dName)
          return true;

      foreach (iface; klass.implementStructs)
        foreach (ip; iface.properties.filter!(p => p.active == Active.Enabled && p.writable))
          if (ip.dName == prop.dName)
            return true;
    }

    return false;
  }

  /**
   * Write a structure definition with struct fields and docs (for "C" structs defined in c/types.d and Struct modules)
   * Params:
   *   writer = Code writer
   *   structMod = true if writing a struct module (D types and no close brace), false otherwise (default)
   */
  void writeStructDef(CodeWriter writer, bool structMod = false)
  { // Recursive function to process embedded struct/union fields
    void recurseStruct(Structure st)
    {
      writer ~= st.genDocs;
      dstring typeName;

      if (structMod)
        typeName = st.dType;
      else if (st is this)
        typeName = st.cType;
      else
        typeName = st.name ? (st.name.camelCase(true) ~ "Type") : null; // Handles anonymous or named embedded struct/unions

      writer ~= [(st.structType == StructType.Union ? "union"d : "struct"d) ~ (typeName ? (" " ~ typeName) : ""), "{"];

      dstring bitsType; // Current bit field type or empty if not active
      ubyte bitsLeft; // Number of bits remaining for this bit field type

      alias BitField = Tuple!(dstring, "name", ubyte, "bits", dstring, "docs");
      BitField[] bitFields;

      void writeBitfield()
      {
        writer ~= "mixin(bitfields!(";

        foreach (i, t; bitFields)
        {
          if (t.docs.length > 0)
            writer ~= t.docs;

          auto lastField = i + 1 == bitFields.length && bitsLeft == 0;
          writer ~= bitsType ~ ", \""d ~ t.name ~ "\", "d ~ t.bits.to!dstring ~ (lastField ? "));"d : ","d);
        }

        if (bitsLeft > 0)
          writer ~= bitsType ~ ", \"\", "d ~ bitsLeft.to!dstring ~ "));"d;

        writer ~= "";

        bitsType.length = 0;
        bitsLeft = 0;
        bitFields.length = 0;
      }

      foreach (fi, f; st.fields)
      {
        auto bitFieldSize = cast(ubyte)(f.bits > 0 ? (BasicTypeSizes.get(f.dType.to!string, 0) * 8) : 0);

        // Just emit a normal field if a bitfield takes up its entire available bit allocation
        if (bitFieldSize > 0 && bitsType.length == 0 && f.bits == bitFieldSize)
          bitFieldSize = 0;

        if (bitFieldSize > 0) // Is this a bit field?
        {
          if (bitsType.length > 0 && (f.dType != bitsType || f.bits > bitsLeft))
            writeBitfield; // Type changed or field doesn't fit, start a new storage unit

          if (bitsType.length == 0) // First field of a bit field?
          {
            bitsType = f.dType;
            bitsLeft = bitFieldSize;
          }

          bitFields ~= BitField(f.dName, f.bits, f.docContent.length > 0 ? f.genDocs : null);
          bitsLeft -= f.bits;
          continue;
        }
        else if (f.bits > 0)
          f.xmlNode.warn("Unsupported bitfield type '" ~ f.dType ~ "'");

        if (bitsType.length > 0) // Write out bitfield when a non-bitfield is found
          writeBitfield;

        writer ~= f.genDocs;

        if (f.directStruct)
          recurseStruct(f.directStruct);
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
        else with (TypeKind) if (structMod && f.kind.among(BasicAlias, Enum, Flags, Struct, StructAlias)) // A regular field (D struct module)
          writer ~= f.dType ~ (f.cType.countStars > 0 ? "*"d : "") ~" " ~ f.dName ~ ";";
        else // A regular field ("C" struct definition)
        {
          if (!f.cType.empty)
            writer ~= f.cType ~ " " ~ f.dName ~ ";";
          else
            f.xmlNode.warn("Struct field is missing c:type attribute");
        }

        if (fi + 1 < st.fields.length)
          writer ~= "";
      }

      if (bitsType.length > 0) // Write out bitfield at the end of the record (if any)
        writeBitfield;

      if (!structMod || st !is this) // Suppress final brace if structMod and st is outermost struct
        writer ~= ["}", ""];

      if (st !is this && typeName)
        writer ~= [typeName ~ " " ~ st.dType ~ ";"]; // dType is the field name
    }

    recurseStruct(this);
  }

  override void toJson(ref JSONValue js)
  {
    super.toJson(js);

    js["structType"] = structType.to!string;
    js["cSymbolPrefix"] = cSymbolPrefix;
    js.jsonSetNonDefault("parentType", parentType);
    if (parentStruct) js["parentStruct"] = parentStruct.fullDType;
    js.jsonSetNonDefault("implements", implements);
    if (implementStructs.length > 0) js.jsonSet("implementStructs", implementStructs.map!(x => x.fullDType).array);
    js.jsonSetNonDefault("prerequisites", prerequisites);
    js.jsonSetNonDefault("functions", functions);
    js.jsonSetNonDefault("signals", signals);
    js.jsonSetNonDefault("fields", fields);
    js.jsonSetNonDefault("properties", properties);
    js.jsonSetNonDefault("moduleName", moduleName);
    js.jsonSetNonDefault("errorQuarks", errorQuarks);
    js.jsonSetNonDefault("abstract", abstract_);
    js.jsonSetNonDefault("deprecated", deprecated_);
    js.jsonSetNonDefault("opaque", opaque);
    js.jsonSetNonDefault("cLayout", cLayout);
    js.jsonSetNonDefault("pointer", pointer);
    js.jsonSetNonDefault("version", version_);
    js.jsonSetNonDefault("deprecatedVersion", deprecatedVersion);
    js.jsonSetNonDefault("glibFundamental", glibFundamental);
    js.jsonSetNonDefault("glibGetType", glibGetType);
    js.jsonSetNonDefault("glibTypeName", glibTypeName);
    js.jsonSetNonDefault("glibGetValueFunc", glibGetValueFunc);
    js.jsonSetNonDefault("glibSetValueFunc", glibSetValueFunc);
    js.jsonSetNonDefault("glibRefFunc", glibRefFunc);
    js.jsonSetNonDefault("glibUnrefFunc", glibUnrefFunc);
    js.jsonSetNonDefault("glibTypeStruct", glibTypeStruct);
    js.jsonSetNonDefault("glibIsGtypeStructFor", glibIsGtypeStructFor);
    js.jsonSetNonDefault("copyFunction", copyFunction);
    js.jsonSetNonDefault("freeFunction", freeFunction);
    js.jsonSetNonDefault("initFunction", initFunction);
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

  dstring borrowAttr; /// Custom "gid-borrow" attribute value ("1" or "0", empty if not set, see borrowed)
  bool cLayout; /// Custom "gid-c-layout" attribute: emit the GIR field layout in c/types.d even when opaque
  bool _isPlainDataSet; /// true when _isPlainData has been set
  bool _isPlainData; /// Cached value indicating if kind.isPlainData or Wrap and all fields are POD
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
  dstring initFunction; /// Custom "gid-init-function" attribute: initializes a new C allocated (cAllocated) Wrap structure
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
  Struct, /// Structure module
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
