module gir.func_writer;

import code_writer;
import defs;
import gir.deleg_writer;
import gir.func;
import gir.param;
import gir.structure;
import gir.type_node;
import import_manager;
import std_includes;
import utils;

/// Function writer class
class FuncWriter
{
  this(Func func)
  {
    this.func = func;
    process();
  }

  // Process the function
  private void process()
  {
    codeTrap("func.write", func.fullName);

    if (!func.isStatic) // Check for conflicting method in ancestor if not a static method
    {
      conflictClass = func.findMethodConflict(null, conflictConforms);

      if (conflictClass && !conflictConforms)
        conflictClass.dType(); // Resolve D type to add it to the active ImportManager
    }

    if (func.isStatic)
      decl ~= "static "; // Function is "static" if it is not a method, constructor, or global function

    if (func.throws)
    { // postCall for exceptions is order sensitive and must be handled before output and return processing
      postCall ~= "if (_err)\nthrow new " ~ func.errorDomain ~ "(_err);\n";
      addImport("glib.error");
    }

    // Write out any C callback embedded functions
    foreach (param; func.params)
    {
      if (param.kind == TypeKind.Callback && !param.isDestroy)
      { // Use a static delegate pointer if there is no closure data argument
        auto staticDelegatePtr = !param.typeObjectRoot || !(cast(Func)param.typeObjectRoot).closureParam;

        if (staticDelegatePtr)
          preCall ~= "static " ~ param.fullDType ~ " _static_" ~ param.dName ~ ";\n\n";

        auto delegWriter = new DelegWriter(param, staticDelegatePtr);
        preCall ~= delegWriter.generate() ~ "\n";

        if (staticDelegatePtr)
        {
          preCall ~= "_static_" ~ param.dName ~ " = " ~ param.dName ~ ";\n";
          postCall ~= "_static_" ~ param.dName ~ " = null;\n"; // Clear the delegate pointer to allow it to be collected
        }
     }
    }

    processReturn();

    if (func.isCtor)
      decl ~= "this(";
    else if (func.shadowsFunc)
      decl ~= func.shadowsFunc.dName ~ "(";
    else
      decl ~= func.dName ~ "(";

    call ~= func.cName ~ "(";

    foreach (param; func.params)
      processParam(param);

    if (func.throws)
    {
      preCall ~= "GError *_err;\n";
      addCallParam("&_err");
    }

    decl ~= ")";
    call ~= ");";
  }

  // Helper to add parameter to call string with comma separator
  private void addCallParam(dstring paramStr)
  {
    if (!call.endsWith('('))
      call ~= ", ";

    call ~= paramStr;
  }

  // Helper to add parameter to decl string with comma separator
  private void addDeclParam(dstring paramStr)
  {
    if (!decl.endsWith('('))
      decl ~= ", ";

    decl ~= paramStr;
  }

  /// Process return value
  private void processReturn()
  {
    auto retVal = func.returnVal;

    if (!retVal || retVal.origDType == "none")
    {
      decl ~= "void ";
      return;
    }
    else if (retVal.lengthArrayParams.length > 0) // Function returns void if return value is an array length
    {
      decl ~= "void ";
      call ~= "auto _ret_length = ";
      return;
    }

    if (retVal.containerType == ContainerType.Array)
    {
      processReturnArray();
      return;
    }
    else if (retVal.containerType != ContainerType.None)
    {
      processReturnContainer();
      return;
    }

    final switch (retVal.kind) with (TypeKind)
    {
      case Basic, BasicAlias:
        decl ~= retVal.fullDType ~ " ";
        preCall ~= retVal.fullDType ~ " _retval;\n";
        call ~= "_retval = ";
        // postCall ~= retVal.dType ~ " _retval = cast(" ~ retVal.dType ~ ")_cretval;\n";
        break;
      case String:
        decl ~= "string ";
        preCall ~= retVal.cType ~ " _cretval;\n";
        call ~= "_cretval = ";
        postCall ~= "string _retval = (cast(const(char)*)_cretval).fromCString("d ~ retVal.fullOwnerFlag ~ ".Free);\n";
        break;
      case Enum, Flags:
        decl ~= retVal.fullDType ~ " ";
        preCall ~= retVal.cType ~ " _cretval;\n";
        call ~= "_cretval = ";
        postCall ~= retVal.fullDType ~ " _retval = cast(" ~ retVal.fullDType ~ ")_cretval;\n";
        break;
      case Simple:
        decl ~= retVal.fullDType ~ " ";
        preCall ~= retVal.cType ~ " _cretval;\n";
        call ~= "_cretval = ";
        postCall ~= retVal.fullDType ~ " _retval;\nif (_cretval)\n_retval = *cast(" ~ retVal.fullDType ~ "*)_cretval;\n";
        break;
      case Callback:
        decl ~= retVal.fullDType ~ "* ";
        call ~= retVal.fullDType ~ "* _retval = ";
        break;
      case Pointer:
        if (!func.isCtor)
          decl ~= retVal.fullDType ~ " ";

        call ~= "auto _retval = ";
        break;
      case Boxed:
        if (!func.isCtor)
          decl ~= retVal.fullDType ~ " ";

        preCall ~= retVal.cType ~ " _cretval;\n";
        call ~= "_cretval = ";

        if (!func.isCtor)
          postCall ~= "auto _retval = _cretval ? new "d ~ retVal.fullDType ~ "(cast(void*)_cretval, "
            ~ retVal.fullOwnerFlag ~ ".Take) : null;\n";
        else // Constructor method
          postCall ~= "this(_cretval, " ~ retVal.fullOwnerFlag ~ ".Take);\n";
        break;
      case Opaque, Wrap, Reffed, Object, Interface:
        if (!func.isCtor)
          decl ~= retVal.fullDType ~ " ";

        preCall ~= retVal.cType ~ " _cretval;\n";
        call ~= "_cretval = ";

        if (!func.isCtor)
        {
          if (retVal.kind == TypeKind.Object || retVal.kind == TypeKind.Interface)
          {
            addImport("gobject.object");
            postCall ~= "auto _retval = gobject.object.ObjectWrap._getDObject!("
              ~ retVal.fullDType ~ ")(cast(" ~ retVal.cType.stripConst ~ ")_cretval, " ~ retVal.fullOwnerFlag
              ~ ".Take);\n";
          }
          else
            postCall ~= "auto _retval = _cretval ? new "
              ~ retVal.fullDType ~ "(cast(" ~ retVal.cType.stripConst ~ ")_cretval, " ~ retVal.fullOwnerFlag
                ~ ".Take) : null;\n";
        }
        else // Constructor method
          postCall ~= "this(_cretval, " ~ retVal.fullOwnerFlag ~ ".Take);\n";
        break;
      case Unknown, Container, Namespace:
        assert(0, "Unsupported return value type '" ~ retVal.fullDType.to!string ~ "' (" ~ retVal.kind.to!string ~ ") for "
            ~ func.fullName.to!string);
    }
  }

  /// Process array return value
  private void processReturnArray()
  {
    auto retVal = func.returnVal;
    auto elemType = retVal.elemTypes[0];
    auto retType = elemType.dType == "char" ? "string" : (elemType.fullDType ~ "[]"); // Use string for char[] with length

    decl ~= retType ~ " ";
    preCall ~= retVal.cType ~ " _cretval;\n";
    call ~= "_cretval = ";
    postCall ~= (elemType.dType == "char" ? "string" : (elemType.fullDType ~ "[]")) ~ " _retval;\n";

    dstring lengthStr;

    postCall ~= "\nif (_cretval)\n{\n";

    if (retVal.lengthParam) // Array has length parameter?
    {
      preCall ~= retVal.lengthParam.fullDType ~ " _cretlength;\n";
      lengthStr = "_cretlength";
    }
    else if (retVal.fixedSize != ArrayNotFixed) // Array is a fixed size?
      lengthStr = retVal.fixedSize.to!dstring;
    else if (retVal.zeroTerminated) // Array is zero terminated?
    {
      postCall ~= "uint _cretlength;\nfor (; _cretval[_cretlength] "d ~ (elemType.cType.endsWith("*")
        ? "!is null"d : "!= 0") ~ "; _cretlength++)\nbreak;\n";
      lengthStr = "_cretlength";
    }
    else
      assert(0, "Function '" ~ func.fullName.to!string ~ "' return array has indeterminate length"); // This should be prevented by defs.fixupRepos()

    if (elemType.kind.among(TypeKind.Basic, TypeKind.BasicAlias, TypeKind.Enum))
      postCall ~= "_retval = cast(" ~ retType ~ ")_cretval[0 .. " ~ lengthStr ~ "].dup;\n";
    else
    {
      postCall ~= "_retval = new " ~ elemType.fullDType ~ "[" ~ lengthStr ~ "];\nforeach (i; 0 .. "
        ~ lengthStr ~ ")\n";

      final switch (elemType.kind) with (TypeKind)
      {
        case String:
          postCall ~= "_retval[i] = _cretval[i].fromCString(" ~ retVal.fullOwnerFlag ~ ".Free);\n";
          break;
        case Enum, Flags:
          postCall ~= "_retval[i] = cast(" ~ elemType.fullDType ~ ")(_cretval[i]);\n";
          break;
        case Simple, Pointer:
          postCall ~= "_retval[i] = _cretval[i];\n";
          break;
        case Opaque, Wrap, Boxed, Reffed:
          postCall ~= "_retval[i] = new " ~ elemType.fullDType ~ "(cast(void*)" ~ (retVal.cType.countStars == 1 ? "&"d : "")
            ~ "_cretval[i], " ~ retVal.fullOwnerFlag ~ ".Take);\n";
          break;
        case Object, Interface:
          addImport("gobject.object");
          postCall ~= "_retval[i] = gobject.object.ObjectWrap._getDObject!(" ~ elemType.fullDType ~ ")(_cretval[i], "
            ~ retVal.fullOwnerFlag ~ ".Take);\n";
          break;
        case Basic, BasicAlias, Callback, Unknown, Container, Namespace:
          assert(0, "Unsupported return value array type '" ~ elemType.fullDType.to!string ~ "' (" ~ elemType
              .kind.to!string ~ ") for " ~ func.fullName.to!string);
      }
    }

    if (retVal.ownership == Ownership.Container || retVal.ownership == Ownership.Full)
      postCall ~= "gFree(cast(void*)_cretval);\n";

    postCall ~= "}\n";
  }

  /// Process a return container (not Array)
  private void processReturnContainer()
  {
    auto retVal = func.returnVal;
    dstring templateArgs;

    if (retVal.containerType == ContainerType.HashTable)
      templateArgs = "!(" ~ retVal.elemTypes[0].fullDType ~ ", " ~ retVal.elemTypes[1].fullDType ~ ", "
        ~ "GidOwnership." ~ retVal.ownership.to!dstring ~ ")";
    else
      templateArgs = retVal.containerType != ContainerType.ByteArray ? ("!(" ~ retVal.elemTypes[0].fullDType
        ~ ", " ~ "GidOwnership." ~ retVal.ownership.to!dstring ~ ")") : "";

    decl ~= retVal.fullDType ~ " ";
    preCall ~= retVal.cType ~ " _cretval;\n";
    call ~= "_cretval = ";
    postCall ~= "auto _retval = g" ~ retVal.containerType.to!dstring ~ "ToD" ~ templateArgs ~ "(cast("
      ~ retVal.cType.stripConst ~ ")_cretval);\n";
  }

  /// Process parameter
  private void processParam(Param param)
  {
    if (param.isInstanceParam) // Instance parameter?
    {
      call ~= "cast(" ~ param.cType ~ ")this._cPtr"d; // Pointer types have the pointer as part of the type
      return;
    }

    if (param.isLengthReturnArray) // Return array length parameter is handled in processReturnArray()
    {
      addCallParam("&_cretlength");
      return;
    }

    if (param.lengthArrayParams.length > 0) // Array length parameter?
    {
      addCallParam((param.direction == ParamDirection.In ? "_"d : "&_"d) ~ param.dName);
      return;
    }

    if (param.containerType == ContainerType.Array) // Array container?
    { // Declare length variable before the array in case it is used by the array
      if (param.lengthParam && param.lengthParam.containerType != ContainerType.Array) // Skip length parameters which are other arrays (a gidgen extension)
      {
        // Only declare length parameter for first array
        if (param == param.lengthParam.lengthArrayParams[0])
          preCall ~= param.lengthParam.fullDType ~ " _" ~ param.lengthParam.dName ~ ";\n";

        if (param.direction != ParamDirection.Out) // Set length if parameter is non-null, handles multiple optional array arguments
          preCall ~= "if (" ~ param.dName ~ ")\n_" ~ param.lengthParam.dName ~ " = cast(" ~ param.lengthParam.fullDType
            ~ ")" ~ param.dName ~ ".length;\n\n";
      }

      if (param.direction == ParamDirection.In)
        processArrayInParam(param);
      else if (param.direction == ParamDirection.Out)
        processArrayOutParam(param);
      else
        assert(0); // Should be prevented by verify()
      return;
    }
    else if (param.containerType != ContainerType.None) // Other type of container?
    {
      if (param.direction == ParamDirection.In)
        processContainerInParam(param);
      else if (param.direction == ParamDirection.Out)
        processContainerOutParam(param);
      else
        assert(0, "InOut container parameter not supported for " ~ param.fullName.to!string);

      return;
    }
    else if (param.isClosure) // Closure data?
    {
      if (param.callbackIndex != NoCallback)
      {
        auto callbackParam = func.params[param.callbackIndex];
        auto freezeDeleg = callbackParam.scope_ != ParamScope.Call;

        // Duplicate delegate to malloc heap memory and pin the context if not Call scope
        if (freezeDeleg)
          preCall ~= "auto _" ~ callbackParam.dName ~ " = " ~ callbackParam.dName ~ " ? freezeDelegate(cast(void*)&"
            ~ callbackParam.dName ~ ") : null;\n";
        else
          preCall ~= "auto _" ~ callbackParam.dName ~ " = " ~ callbackParam.dName ~ " ? cast(void*)&("
            ~ callbackParam.dName ~ ") : null;\n";

        addCallParam("_" ~ callbackParam.dName); // Pass the duplicate pinned delegate as closure data
      }
      else
        addCallParam("null"); // Pass null if there is no callback associated with this closure data

      return;
    }
    else if (param.isDestroy) // Destroy callback?
    {
      auto callbackParam = func.params[param.callbackIndex];
      preCall ~= "GDestroyNotify _" ~ callbackParam.dName ~ "DestroyCB = " ~ callbackParam.dName
        ~ " ? &thawDelegate : null;\n";

      if (param.callbackIndex != NoCallback)
        addCallParam("_" ~ callbackParam.dName ~ "DestroyCB"); // Free the duplicate delegate and unpin the context
      else
        addCallParam("null"); // Pass null if there is no callback associated with this destroy notify

      return;
    }

    final switch (param.kind) with (TypeKind)
    {
      case Basic, BasicAlias:
        addDeclParam(param.directionStr ~ param.fullDType ~ " " ~ param.dName);

        if (param.direction == ParamDirection.In)
          addCallParam(param.dName);
        else
          addCallParam("cast(" ~ param.cType ~ ")&" ~ param.dName);
        break;
      case Enum, Flags:
        addDeclParam(param.directionStr ~ param.fullDType ~ " " ~ param.dName);
        addCallParam((param.direction != ParamDirection.In ? "&"d : "") ~ param.dName);
        break;
      case String:
        addDeclParam(param.directionStr ~ "string " ~ param.dName);

        if (param.direction == ParamDirection.In)
        {
          preCall ~= param.cType ~ " _" ~ param.dName ~ " = " ~ param.dName ~ ".toCString(" ~ param.fullOwnerFlag
            ~ ".Alloc);\n";
          addCallParam("_" ~ param.dName);
        }
        else if (param.direction == ParamDirection.Out)
        {
          preCall ~= "char* _" ~ param.dName ~ ";\n";
          addCallParam("&_" ~ param.dName);
          postCall ~= param.dName ~ " = _" ~ param.dName ~ ".fromCString(" ~ param.fullOwnerFlag ~ ".Free);\n";
        }
        else // InOut
          assert(0, "InOut string arguments not supported"); // FIXME - Does this even exist?

        break;
      case Simple:
        addDeclParam(param.directionStr ~ param.fullDType ~ " " ~ param.dName);
        addCallParam("&" ~ param.dName);
        break;
      case Pointer:
        addDeclParam(param.directionStr ~ param.fullDType ~ " " ~ param.dName);
        addCallParam((param.direction == ParamDirection.Out ? "&"d : ""d) ~ param.dName);
        break;
      case Callback:
        if (cast(Func)param.typeObjectRoot)
        {
          addDeclParam(param.fullDType ~ " " ~ param.dName);
          addCallParam("_" ~ param.dName ~ "CB");
        }
        else
        {
          addDeclParam(param.directionStr ~ param.fullDType ~ " " ~ param.dName);
          addCallParam(param.dName);
        }
        break;
      case Opaque, Wrap, Boxed, Reffed, Object:
        if (param.direction == ParamDirection.In || param.direction == ParamDirection.InOut)
        {
          addDeclParam(param.fullDType ~ " " ~ param.dName);
          addCallParam(param.dName ~ " ? cast(" ~ param.cType ~ ")" ~ param.dName ~ "._cPtr"
            ~ (!param.kind.among(TypeKind.Opaque, TypeKind.Wrap) ? ("(" ~ param.fullOwnerFlag ~ ".Dup)") : "") ~ " : null");
        }
        else if (param.direction == ParamDirection.Out)
        {
          addDeclParam("out " ~ param.fullDType ~ " " ~ param.dName);
          preCall ~= param.cTypeRemPtr ~ " _" ~ param.dName ~ ";\n";
          addCallParam("&_" ~ param.dName);
          postCall ~= param.dName ~ " = " ~ "new " ~ param.fullDType;
          postCall ~= "(cast(void*)" ~ (param.cTypeRemPtr.endsWith('*') ? "_"d : "&_"d) ~ param.dName;
          postCall ~= ", " ~ param.fullOwnerFlag ~ ".Take);\n";
        }
        break;
      case Interface:
        addImport("gobject.object");

        if (param.direction == ParamDirection.In || param.direction == ParamDirection.InOut)
        {
          addImport("gobject.object");
          addDeclParam(param.fullDType ~ " " ~ param.dName);
          addCallParam(param.dName ~ " ? cast(" ~ param.cTypeRemPtr.stripConst ~ "*)(cast(gobject.object.ObjectWrap)"
            ~ param.dName ~ ")._cPtr(" ~ param.fullOwnerFlag ~ ".Dup) : null");
        }
        else if (param.direction == ParamDirection.Out)
        {
          addDeclParam("out " ~ param.fullDType ~ " " ~ param.dName);
          preCall ~= param.cTypeRemPtr ~ " _" ~ param.dName ~ ";\n";
          addCallParam("&_" ~ param.dName);
          postCall ~= param.dName ~ " = gobject.object.ObjectWrap._getDObject!(" ~ param.fullDType ~ ")(_"
            ~ param.dName ~ ", " ~ param.fullOwnerFlag ~ ".Take);\n";
        }
        break;
      case Unknown, Container, Namespace:
        assert(0, "Unsupported parameter type '" ~ param.fullDType.to!string ~ "' (" ~ param.kind.to!string ~ ") for "
            ~ param.fullName.to!string);
    }

    if (param.isOptional) // If parameter is optional, set default value to null (FIXME - Can there be other non-pointer optional types?)
      decl ~= " = null";
  }

  // Process an array input parameter
  private void processArrayInParam(Param param)
  {
    auto elemType = param.elemTypes[0];

    if (elemType.dType == "char") // char[] arrays are sometimes used for strings with a length parameter
      addDeclParam("string " ~ param.dName);
    else
      addDeclParam(elemType.fullDType ~ "[] " ~ param.dName);

    addCallParam("_" ~ param.dName);

    assert(param.ownership == Ownership.None, "Function array parameter " ~ param.fullName.to!string
        ~ " ownership not supported"); // FIXME - Support for ownership Full/Container

    if (param.fixedSize != ArrayNotFixed) // Add an array size assertion if fixed size does not match
      preCall ~= "assert(!" ~ param.dName ~ " || " ~ param.dName ~ ".length == "
        ~ param.fixedSize.to!dstring ~ ");\n";

    final switch (elemType.kind) with (TypeKind)
    {
      case Basic, BasicAlias, Enum, Flags, Simple, Pointer:
        preCall ~= "auto _" ~ param.dName ~ " = cast(" ~ param.cType ~ ")";

        if (param.zeroTerminated) // If zero terminated, append a null or 0 value to the array and use the pointer to pass to the C function call
          preCall ~= "(" ~ param.dName ~ " ~ " ~ (elemType.cType.endsWith("*") ? "null" : elemType.cType ~ ".init")
            ~ ").ptr";
        else
          preCall ~= param.dName ~ ".ptr";

        preCall ~= ";\n";
        break;
      case String:
        preCall ~= elemType.cType ~ "[] _tmp" ~ param.dName ~ ";\n";
        preCall ~= "foreach (s; " ~ param.dName ~ ")\n" ~ "_tmp" ~ param.dName ~ " ~= s.toCString(No.Alloc);\n";

        if (param.zeroTerminated)
          preCall ~= "_tmp" ~ param.dName ~ " ~= null;\n";

        preCall ~= param.cType ~ " _" ~ param.dName ~ " = " ~ "_tmp" ~ param.dName ~ ".ptr" ~ ";\n\n";
        break;
      case Wrap:
        preCall ~= elemType.cTypeRemPtr ~ "[] _tmp" ~ param.dName ~ ";\n";
        preCall ~= "foreach (obj; " ~ param.dName ~ ")\n" ~ "_tmp" ~ param.dName ~ " ~= obj.cInstance;\n";

        if (param.zeroTerminated)
          preCall ~= "_tmp" ~ param.dName ~ " ~= " ~ elemType.cTypeRemPtr ~ "();\n";

        preCall ~= param.cType ~ " _" ~ param.dName ~ " = _tmp" ~ param.dName ~ ".ptr" ~ ";\n\n";
        break;
      case Boxed:
        preCall ~= elemType.cType ~ "[] _tmp" ~ param.dName ~ ";\n";

        preCall ~= "foreach (obj; " ~ param.dName ~ ")\n" ~ "_tmp" ~ param.dName ~ " ~= "
          ~ (elemType.cType.endsWith('*') ? ""d : "*"d) ~ "cast(" ~ elemType.cTypeRemPtr ~ "*)obj._cPtr;\n";

        if (param.zeroTerminated)
          preCall ~= "_tmp" ~ param.dName ~ ".length++;\n";

        preCall ~= param.cType ~ " _" ~ param.dName ~ " = _tmp" ~ param.dName ~ ".ptr" ~ ";\n\n";
        break;
      case Opaque, Reffed, Object:
        preCall ~= elemType.cType ~ "[] _tmp" ~ param.dName ~ ";\n";

        preCall ~= "foreach (obj; " ~ param.dName ~ ")\n" ~ "_tmp" ~ param.dName ~ " ~= obj ? cast("
          ~ elemType.cTypeRemPtr.stripConst ~ "*)obj._cPtr : null;\n";

        if (param.zeroTerminated)
          preCall ~= "_tmp" ~ param.dName ~ " ~= null;\n";

        preCall ~= param.cType ~ " _" ~ param.dName ~ " = cast(" ~ param.cType ~ ")_tmp"
          ~ param.dName ~ ".ptr" ~ ";\n\n";
        break;
      case Interface:
        addImport("gobject.object");
        preCall ~= elemType.cType ~ "[] _tmp" ~ param.dName ~ ";\n";

        preCall ~= "foreach (obj; " ~ param.dName ~ ")\n" ~ "_tmp" ~ param.dName
          ~ " ~= obj ? cast(" ~ elemType.cTypeRemPtr.stripConst ~ "*)(cast(gobject.object.ObjectWrap)obj)._cPtr : null;\n";

        if (param.zeroTerminated)
          preCall ~= "_tmp" ~ param.dName ~ " ~= null;\n";

        preCall ~= param.cType ~ " _" ~ param.dName ~ " = _tmp" ~ param.dName ~ ".ptr" ~ ";\n\n";
        break;
      case Unknown, Callback, Container, Namespace:
        assert(0, "Unsupported parameter array type '" ~ elemType.fullDType.to!string ~ "' (" ~ elemType.kind.to!string
            ~ ") for " ~ param.fullName.to!string);
    }

    if (param.isOptional) // If parameter is optional, set default value to null
      decl ~= " = null";
  }

  // Process an array output parameter
  private void processArrayOutParam(Param param)
  {
    auto elemType = param.elemTypes[0];

    addDeclParam(param.directionStr ~ elemType.fullDType ~ "[] " ~ param.dName);

    dstring lengthStr;

    if (param.lengthParam) // Array has length parameter?
    { // gidgen extention which allows another zero-terminated array to be used for the output array length
      if (param.lengthParam.containerType == ContainerType.Array)
        lengthStr = param.lengthParam.dName ~ ".length";
      else
        lengthStr = "_" ~ param.lengthParam.dName;
    }
    else if (param.lengthReturn) // Array uses return value for length?
      lengthStr = "_ret_length";
    else if (param.fixedSize != ArrayNotFixed) // Array is a fixed size?
      lengthStr = param.fixedSize.to!dstring;
    else if (param.zeroTerminated) // Array is zero terminated?
    {
      postCall ~= "uint _len" ~ param.dName ~ ";\nif (_" ~ param.dName ~ ")\n{\nfor (; _" ~ param.dName
        ~ "[_len" ~ param.dName ~ "] " ~ (elemType.cTypeRemPtr.endsWith("*") ? "!is null"d : "!= 0") ~ "; _len" ~ param.dName
        ~ "++)\n{\n}\n}\n";
      lengthStr = "_len" ~ param.dName;
    }
    else
      assert(0); // This should be prevented by verify()

    final switch (elemType.kind) with (TypeKind)
    {
      case Basic, BasicAlias, Enum, Flags, Simple, Pointer:
        if (!param.callerAllocates)
        {
          preCall ~= param.cTypeRemPtr ~ " _" ~ param.dName ~ ";\n";
          addCallParam("&_" ~ param.dName);
          postCall ~= param.dName ~ ".length = " ~ lengthStr ~ ";\n";
          postCall ~= param.dName ~ "[0 .. $] = (cast(" ~ elemType.fullDType ~ "*)_" ~ param.dName ~ ")[0 .. "
            ~ lengthStr ~ "];\n";

          if (param.ownership != Ownership.None)
            postCall ~= "gFree(cast(void*)_" ~ param.dName ~ ");\n";
        }
        else
          addCallParam(param.dName ~ ".ptr");
        break;
      case String:
        if (!param.callerAllocates)
        {
          preCall ~= param.cTypeRemPtr ~ " _" ~ param.dName ~ ";\n";
          addCallParam("&_" ~ param.dName);
          postCall ~= param.dName ~ ".length = " ~ lengthStr ~ ";\n";
          postCall ~= "foreach (i; 0 .. " ~ lengthStr ~ ")\n" ~ param.dName ~ "[i] = _" ~ param.dName ~ "[i].fromCString("
            ~ param.fullOwnerFlag ~ ".Free);\n";

          if (param.ownership != Ownership.None)
            postCall ~= "gFree(cast(void*)_" ~ param.dName ~ ");\n";
        }
        else
        {
          preCall ~= elemType.cType ~ "[] _" ~ param.dName ~ ";\n";
          preCall ~= "_" ~ param.dName ~ ".length = " ~ lengthStr ~ ";\n";
          addCallParam("_" ~ param.dName ~ ".ptr");
          postCall ~= param.dName ~ ".length = " ~ lengthStr ~ ";\n";
          postCall ~= "foreach (i; 0 .. " ~ lengthStr ~ ")\n" ~ param.dName ~ "[i] = _" ~ param.dName
            ~ "[i].fromCString(" ~ param.fullOwnerFlag ~ ".Free);\n";
        }
        break;
      case Opaque, Wrap, Boxed, Reffed:
        if (!param.callerAllocates)
        {
          preCall ~= param.cTypeRemPtr ~ " _" ~ param.dName ~ ";\n";
          addCallParam("&_" ~ param.dName);
          postCall ~= param.dName ~ ".length = " ~ lengthStr ~ ";\n";
          postCall ~= "foreach (i; 0 .. " ~ lengthStr ~ ")\n" ~ param.dName ~ "[i] = new " ~ elemType.fullDType ~ "(cast(void*)&_"
            ~ param.dName ~ "[i], " ~ param.fullOwnerFlag ~ ".Take);\n";

          if (param.ownership != Ownership.None)
            postCall ~= "gFree(cast(void*)_" ~ param.dName ~ ");\n";
        }
        else
        {
          preCall ~= elemType.cType ~ "[] _" ~ param.dName ~ ";\n";
          preCall ~= "_" ~ param.dName ~ ".length = " ~ lengthStr ~ ";\n";
          addCallParam("_" ~ param.dName ~ ".ptr");
          postCall ~= param.dName ~ ".length = " ~ lengthStr ~ ";\n";
          postCall ~= "foreach (i; 0 .. " ~ lengthStr ~ ")\n" ~ param.dName ~ "[i] = new " ~ elemType.fullDType
            ~ "(cast(void*)&_" ~ param.dName ~ "[i], " ~ param.fullOwnerFlag ~ ".Take);\n";

          if (param.ownership != Ownership.None)
            postCall ~= "gFree(cast(void*)_" ~ param.dName ~ ");\n";
        }
        break;
      case Object, Interface:
        addImport("gobject.object");

        if (!param.callerAllocates)
        {
          preCall ~= param.cTypeRemPtr ~ " _" ~ param.dName ~ ";\n";
          addCallParam("&_" ~ param.dName);
          postCall ~= param.dName ~ ".length = " ~ lengthStr ~ ";\n";

          postCall ~= "foreach (i; 0 .. " ~ lengthStr ~ ")\n" ~ param.dName
            ~ "[i] = gobject.object.ObjectWrap._getDObject!(" ~ elemType.fullDType ~ ")(_" ~ param.dName ~ "[i], "
            ~ param.fullOwnerFlag ~ ".Take);\n";

          if (param.ownership != Ownership.None)
            postCall ~= "gFree(cast(void*)_" ~ param.dName ~ ");\n";
        }
        else
        {
          preCall ~= elemType.cType ~ "[] _" ~ param.dName ~ ";\n";
          preCall ~= "_" ~ param.dName ~ ".length = " ~ lengthStr ~ ";\n";
          addCallParam("_" ~ param.dName ~ ".ptr");
          postCall ~= param.dName ~ ".length = " ~ lengthStr ~ ";\n";

          postCall ~= "foreach (i; 0 .. " ~ lengthStr ~ ")\n" ~ param.dName
            ~ "[i] = gobject.object.ObjectWrap._getDObject!(" ~ elemType.fullDType ~ ")(cast(void*)&_" ~ param.dName
            ~ "[i], " ~ param.fullOwnerFlag ~ ".Take);\n";
        }
        break;
      case Unknown, Callback, Container, Namespace:
        assert(0, "Unsupported parameter array type '" ~ elemType.fullDType.to!string ~ "' (" ~ elemType.kind.to!string
            ~ ") for " ~ param.fullName.to!string);
    }
  }

  // Process a container "in" parameter (except array)
  private void processContainerInParam(Param param)
  {
    dstring templateParams;

    final switch (param.containerType) with(ContainerType)
    {
      case ByteArray:
        break;
      case ArrayG, PtrArray:
        templateParams = "!(" ~ param.elemTypes[0].fullDType  ~ ", " ~ param.zeroTerminated.to!dstring ~ ")";
        break;
      case List, SList:
        templateParams = "!(" ~ param.elemTypes[0].fullDType ~ ")";
        break;
      case HashTable:
        templateParams = "!(" ~ param.elemTypes[0].fullDType ~ ", " ~ param.elemTypes[1].fullDType ~ ")";
        break;
      case Array, None:
        assert(0, "Unsupported 'in' container type '" ~ param.containerType.to!string ~ "' for "
          ~ param.fullName.to!string);
    }

    addDeclParam(param.directionStr ~ param.fullDType ~ " " ~ param.dName);

    preCall ~= "auto _" ~ param.dName ~ " = g" ~ param.containerType.to!dstring ~ "FromD" ~ templateParams ~ "("
      ~ param.dName ~ ");\n";

    if (param.ownership != Ownership.Full)
      preCall ~= "scope(exit) containerFree!(" ~ param.cType ~ ", " ~ param.elemTypes[0].fullDType ~ ", " ~ "GidOwnership."
        ~ param.ownership.to!dstring ~ ")(_" ~ param.dName ~ ");\n";

    addCallParam("_" ~ param.dName);

    if (param.isOptional) // If parameter is optional, set default to null (works for dynamic arrays and associative arrays)
      decl ~= " = null";
  }

  // Process a container "out" parameter (except array)
  private void processContainerOutParam(Param param)
  {
    dstring templateParams;

    final switch (param.containerType) with(ContainerType)
    {
      case ByteArray:
        templateParams = "GidOwnership." ~ param.ownership.to!dstring;
        break;
      case ArrayG, PtrArray, List, SList:
        templateParams = param.elemTypes[0].fullDType  ~ ", " ~ "GidOwnership." ~ param.ownership.to!dstring;
        break;
      case HashTable:
        templateParams = param.elemTypes[0].fullDType ~ ", " ~ param.elemTypes[1].fullDType ~ ", "
          ~ "GidOwnership." ~ param.ownership.to!dstring;
        break;
      case Array, None:
        assert(0, "Unsupported 'out' container type '" ~ param.containerType.to!string ~ "' for "
          ~ param.fullName.to!string);
    }

    addDeclParam(param.directionStr ~ param.fullDType ~ " " ~ param.dName);
    preCall ~= param.cTypeRemPtr ~ " _" ~ param.dName ~ ";\n";
    addCallParam("&_" ~ param.dName);
    postCall ~= param.dName ~ " = g" ~ param.containerType.to!dstring ~ "ToD!(" ~ templateParams ~ ")(_"
      ~ param.dName ~ ");\n";
  }

  /**
   * Write function binding to a CodeWriter.
   * Params:
   *   writer = Code writer to write to.
   *   moduleType = Module file type being written (defaults to ModuleType.Normal)
   */
  void write(CodeWriter writer, ModuleType moduleType = ModuleType.Normal)
  {
    auto isStatic = func.isStatic;

    if (moduleType == ModuleType.IfaceTemplate && isStatic) // Skip static methods in interface template files (implemented in the interface definition file)
      return;

    dstring overrideStr;
    auto parentNode = cast(TypeNode)func.parent;

    if (parentNode && parentNode.kind == TypeKind.Interface && !isStatic) // All interface methods are override
      overrideStr = "override ";
    else if (!isStatic && conflictClass)
    {
      if (!conflictConforms) // Not-identical methods get aliased
        writer ~= ["alias "d ~ func.dName ~ " = " ~ conflictClass.fullDType ~ "."
          ~ func.dName ~ ";", ""];
      else
        overrideStr = "override "; // Conforming methods use override
    }

    writer ~= func.genDocs;

    if (moduleType == ModuleType.Iface && !isStatic) // Interface module and not a static method? (Static methods are implemented in the interface below)
    {
      writer ~= decl ~ ";";
      return;
    }

    // Add "override" for methods of an interface mixin template or if an ancestor/iface has a method with the same name
    writer ~= overrideStr ~ decl;

    writer ~= "{";

    if (preCall.length > 0)
      writer ~= preCall;

    writer ~= call;

    if (postCall.length > 0)
      writer ~= postCall;

    if (func.returnVal && func.returnVal.origDType != "none" && !func.isCtor
        && func.returnVal.lengthArrayParams.length == 0) // Don't return a value for array length return values
      writer ~= "return _retval;";

    writer ~= "}";
  }

  Func func; /// The function object being written
  Structure conflictClass; /// Set to an ancestor class that has a conflicting method
  bool conflictConforms; /// Set to true if the conflicting method conforms to func (override vs alias)
  dstring decl; /// Function declaration
  dstring preCall; /// Pre-call code for call return variable, call output parameter variables, and input variable processing
  dstring call; /// The C function call
  dstring postCall; /// Post-call code for return value processing, output parameter processing, and input variable cleanup
}
