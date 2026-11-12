module gir.func_writer;

import code_writer;
import defs;
import gir.deleg_writer;
import gir.func;
import gir.param;
import gir.structure;
import gir.type_node;
import import_manager;
import line_tracker;
import std_includes;
import utils;

/// Function writer class
class FuncWriter
{
  this(Func func, ModuleType moduleType = ModuleType.Normal)
  {
    this.func = func;
    this.moduleType = moduleType;
    preCall = new LineTracker;
    postCall = new LineTracker;
    process();
  }

  // Process the function
  private void process()
  {
    codeTrap("func.write", func.fullDName);

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
      postCall ~= ["if (_err)", "throw new " ~ func.errorDomain ~ "(_err, Yes.Take);"];
      addImport("glib.error");
    }

    // Write out any C callback embedded functions
    foreach (param; func.params)
    {
      if (param.kind == TypeKind.Callback && !param.isDestroy)
      { // Use a static delegate pointer if there is no closure data argument and scope is Call
        auto staticDelegatePtr = (!param.typeObjectRoot || !(cast(Func)param.typeObjectRoot).hasClosure);

        assert(!staticDelegatePtr || param.scope_ == ParamScope.Call,
          "Static delegate path requires Call scope (Param.verify should have rejected this)");

        if (staticDelegatePtr)
          preCall ~= ["static " ~ param.fullDType ~ " _static_" ~ param.dName ~ ";", ""];

        auto delegWriter = new DelegWriter(param, staticDelegatePtr);
        delegWriter.generate(preCall);

        if (staticDelegatePtr)
        {
          preCall ~= "_static_" ~ param.dName ~ " = " ~ param.dName ~ ";";
          postCall ~= "_static_" ~ param.dName ~ " = null;"; // Clear the delegate pointer to allow it to be collected
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
      preCall ~= "GError *_err;";
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

  // Is a value passed from C to D borrowed from the instance (method with a borrowed structure type and no transfer)
  private bool borrowsFromInstance(TypeNode node)
  {
    auto st = cast(Structure)node.typeObject;
    return st && st.borrowed && node.ownership == Ownership.None && func.funcType == FuncType.Method;
  }

  /// Process return value
  private void processReturn()
  {
    auto retVal = func.returnVal;

    if (!retVal || retVal.origDType == "none" || retVal.active != Active.Enabled)
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

    if (retVal.kind == TypeKind.Callback)
      decl ~= retVal.fullDType ~ "* ";
    else if (!func.isCtor)
      decl ~= retVal.fullDType ~ " ";

    final switch (retVal.kind) with (TypeKind)
    {
      case Basic, BasicAlias:
        preCall ~= retVal.fullDType ~ " _retval;";
        call ~= retVal.dType == "bool" ? "_retval = cast(bool)" : "_retval = ";
        break;
      case String:
        preCall ~= retVal.cType ~ " _cretval;";
        call ~= "_cretval = ";
        postCall ~= "string _retval = (cast(const(char)*)_cretval).fromCString!("d ~ retVal.fullOwnerFlag ~ ".Free);";
        break;
      case Enum, Flags:
        preCall ~= retVal.cType ~ " _cretval;";
        call ~= "_cretval = ";
        postCall ~= retVal.fullDType ~ " _retval = cast(" ~ retVal.fullDType ~ ")_cretval;";
        break;
      case StructAlias, Struct:
        preCall ~= retVal.cType ~ " _cretval;";
        call ~= "_cretval = ";
        postCall ~= [retVal.fullDType ~ " _retval;", "if (_cretval)"];

        if (retVal.ownership == Ownership.Full)
          postCall ~= ["{", "_retval = *cast(" ~ retVal.fullDType ~ "*)_cretval;",
            "cValueFree!(" ~ retVal.fullDType ~ ")(_cretval);", "}"];
        else
          postCall ~= ["_retval = *cast(" ~ retVal.fullDType ~ "*)_cretval;"];
        break;
      case Callback:
        call ~= retVal.fullDType ~ "* _retval = ";
        break;
      case Pointer:
        call ~= "auto _retval = ";
        break;
      case Boxed:
        preCall ~= retVal.cType ~ " _cretval;";
        call ~= "_cretval = ";

        if (!func.isCtor)
          postCall ~= "auto _retval = _cretval ? new "d ~ retVal.fullDType ~ "(cast(void*)_cretval, "
            ~ retVal.fullOwnerFlag ~ ".Take) : null;";
        else // Constructor method
          postCall ~= "this(_cretval, " ~ retVal.fullOwnerFlag ~ ".Take);";
        break;
      case Opaque, Wrap, Reffed, Object, Interface:
        preCall ~= retVal.cType ~ " _cretval;";
        call ~= "_cretval = ";

        if (!func.isCtor)
        {
          if (retVal.kind == TypeKind.Object || retVal.kind == TypeKind.Interface)
          {
            addImport("gobject.object");
            postCall ~= "auto _retval = gobject.object.ObjectWrap._getDObject!("
              ~ retVal.fullDType ~ ")(cast(" ~ retVal.cType.stripConst ~ ")_cretval, " ~ retVal.fullOwnerFlag
              ~ ".Take);";
          }
          else if (retVal.borrowAttr.length > 0) // Borrowed structure owned by a parameter or static (definitions)
          {
            auto owner = func.returnBorrowOwner;
            postCall ~= "auto _retval = _cretval ? new " ~ retVal.fullDType ~ "(cast(" ~ retVal.cType.stripConst
              ~ ")_cretval, " ~ (owner ? owner.dName : "null"d) ~ ") : null;";
          }
          else if (borrowsFromInstance(retVal)) // Borrowed structure owned by the instance
            postCall ~= "auto _retval = _cretval ? new " ~ retVal.fullDType ~ "(cast(" ~ retVal.cType.stripConst
              ~ ")_cretval, this) : null;";
          else
            postCall ~= "auto _retval = _cretval ? new "
              ~ retVal.fullDType ~ "(cast(" ~ retVal.cType.stripConst ~ ")_cretval, " ~ retVal.fullOwnerFlag
                ~ ".Take) : null;";
        }
        else // Constructor method
          postCall ~= "this(_cretval, " ~ retVal.fullOwnerFlag ~ ".Take);";
        break;
      case Unknown, Container, Namespace:
        assert(0, "Unsupported return value type '" ~ retVal.fullDType.to!string ~ "' (" ~ retVal.kind.to!string ~ ") for "
            ~ func.fullDName.to!string);
    }
  }

  /// Process array return value
  private void processReturnArray()
  {
    auto retVal = func.returnVal;
    auto elemType = retVal.elemTypes[0];
    auto retType = elemType.dType == "char" ? "string" : (elemType.fullDType ~ "[]"); // Use string for char[] with length

    decl ~= retType ~ " ";
    preCall ~= retVal.cType ~ " _cretval;";
    call ~= "_cretval = ";
    postCall ~= (elemType.dType == "char" ? "string" : (elemType.fullDType ~ "[]")) ~ " _retval;";

    dstring lengthStr;

    postCall ~= ["", "if (_cretval)", "{"];

    if (retVal.lengthParam && retVal.lengthParam.isArraySize) // Length is an input parameter?
      lengthStr = retVal.lengthParam.dName;
    else if (retVal.lengthParam) // Array has length parameter?
    {
      preCall ~= retVal.lengthParam.fullDType ~ " _cretlength;";
      lengthStr = "_cretlength";
    }
    else if (retVal.fixedSize != ArrayNotFixed) // Array is a fixed size?
      lengthStr = retVal.fixedSize.to!dstring;
    else if (retVal.zeroTerminated) // Array is zero terminated?
    {
      postCall ~= ["uint _cretlength;", "while (_cretval[_cretlength] "d ~ (elemType.cType.endsWith("*")
        ? "!is null"d : "!= 0") ~ ")", "_cretlength++;"];
      lengthStr = "_cretlength";
    }
    else
      assert(0, "Function '" ~ func.fullDName.to!string ~ "' return array has indeterminate length"); // This should be prevented by defs.fixupRepos()

    final switch (elemType.kind) with (TypeKind)
    {
      case TypeKind.Basic, TypeKind.BasicAlias:
        if (elemType.dType == "bool")
        {
          postCall ~= ["_retval = new bool[" ~ lengthStr ~ "];", "foreach (i; 0 .. " ~ lengthStr ~ ")",
            "_retval[i] = cast(bool)_cretval[i];"];
        }
        else // C and D types are compatible, copy array
          postCall ~= "_retval = cast(" ~ retType ~ ")_cretval[0 .. " ~ lengthStr ~ "].dup;";
        break;
      case String:
        postCall ~= ["_retval = new " ~ elemType.fullDType ~ "[" ~ lengthStr ~ "];", "foreach (i; 0 .. "
          ~ lengthStr ~ ")", "_retval[i] = _cretval[i].fromCString!(" ~ retVal.fullOwnerFlag ~ ".Free);"];
        break;
      case Enum, Flags, Struct:
        postCall ~= ["_retval = new " ~ elemType.fullDType ~ "[" ~ lengthStr ~ "];", "foreach (i; 0 .. "
          ~ lengthStr ~ ")", "_retval[i] = cast(" ~ elemType.fullDType ~ ")(_cretval[i]);"];
        break;
      case StructAlias, Pointer:
        postCall ~= ["_retval = new " ~ elemType.fullDType ~ "[" ~ lengthStr ~ "];", "foreach (i; 0 .. "
          ~ lengthStr ~ ")", "_retval[i] = _cretval[i];"];
        break;
      case Opaque, Wrap, Boxed, Reffed:
        postCall ~= ["_retval = new " ~ elemType.fullDType ~ "[" ~ lengthStr ~ "];", "foreach (i; 0 .. "
          ~ lengthStr ~ ")", "_retval[i] = new " ~ elemType.fullDType ~ "(cast(void*)"
          ~ (retVal.arrayPtrDepth == 1 ? "&_cretval[i], No.Take);"d : "_cretval[i], " ~ retVal.fullOwnerFlag
          ~ ".Take);")];
        break;
      case Object, Interface:
        addImport("gobject.object");
        postCall ~= ["_retval = new " ~ elemType.fullDType ~ "[" ~ lengthStr ~ "];", "foreach (i; 0 .. "
          ~ lengthStr ~ ")", "_retval[i] = gobject.object.ObjectWrap._getDObject!(" ~ elemType.fullDType
          ~ ")(_cretval[i], " ~ retVal.fullOwnerFlag ~ ".Take);"];
        break;
      case Callback, Unknown, Container, Namespace:
        assert(0, "Unsupported return value array type '" ~ elemType.fullDType.to!string ~ "' (" ~ elemType
            .kind.to!string ~ ") for " ~ func.fullDName.to!string);
    }

    if (retVal.ownership == Ownership.Container || retVal.ownership == Ownership.Full)
      postCall ~= "gFree(cast(void*)_cretval);";

    postCall ~= "}";
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
    preCall ~= retVal.cType ~ " _cretval;";
    call ~= "_cretval = ";
    postCall ~= "auto _retval = g" ~ retVal.containerType.to!dstring ~ "ToD" ~ templateArgs ~ "(cast("
      ~ retVal.cType.stripConst ~ ")_cretval);";
  }

  /// Process parameter
  private void processParam(Param param)
  {
    if (param.active == Active.Ignored)
    {
      addCallParam(!param.cType.canFind("*") ? (param.cType ~ ".init") : "null");
      return;
    }

    if (param.isInstanceParam) // Instance parameter?
    { // cType pointer types have the "*" as part of the type
      if (moduleType == ModuleType.Struct)
        call ~= param.cType.countStars == 0 ? "*cast(" ~ param.cType ~ "*)&this"d // Struct passed by value
          : "cast(" ~ param.cType ~ ")&this"d; // Struct is compatible with C struct type
      else
        call ~= "cast(" ~ param.cType ~ ")this._cPtr"d // Other types have pointer to C object
          ~ (param.ownership == Ownership.Full ? "(Yes.Dup)"d : ""d); // Callee takes ownership, give it a reference/copy

      return;
    }

    if (param.isArrayLength) // Array length parameter? (return array length is handled in processReturnArray())
    {
      addCallParam(param.isLengthReturnArray ? "&_cretlength"d
        : (param.direction == ParamDirection.In ? "_"d : "&_"d) ~ param.dName);
      return;
    }

    if (param.containerType == ContainerType.Array) // Array container?
    { // Declare length variable before the array in case it is used by the array
      // Skip length parameters which are other arrays (a gidgen extension) or input array sizes
      if (param.lengthParam && param.lengthParam.containerType != ContainerType.Array && !param.lengthParam.isArraySize)
      {
        // Only declare length parameter for first array
        if (param == param.lengthParam.lengthArrayParams[0])
          preCall ~= param.lengthParam.fullDType ~ " _" ~ param.lengthParam.dName ~ ";";

        if (param.direction != ParamDirection.Out) // Set length if parameter is non-null, handles multiple optional array arguments
          preCall ~= ["if (" ~ param.dName ~ ")", "_" ~ param.lengthParam.dName ~ " = cast("
            ~ param.lengthParam.fullDType ~ ")" ~ param.dName ~ ".length;", ""];
      }

      if (param.direction == ParamDirection.In)
        processArrayInParam(param);
      else
        processArrayOutParam(param);
      return;
    }
    else if (param.containerType != ContainerType.None) // Other type of container?
    {
      if (param.direction == ParamDirection.In)
        processContainerInParam(param);
      else
        processContainerOutParam(param);
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
            ~ callbackParam.dName ~ ") : null;";
        else
          preCall ~= "auto _" ~ callbackParam.dName ~ " = " ~ callbackParam.dName ~ " ? cast(void*)&("
            ~ callbackParam.dName ~ ") : null;";

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
        ~ " ? &thawDelegate : null;";

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

        if (param.direction != ParamDirection.In)
        {
          if (param.dType == "bool")
          {
            preCall ~= "gboolean _" ~ param.dName 
              ~ (param.direction == ParamDirection.InOut ? " = " ~ param.dName ~ ";" : ";");
            addCallParam("&_" ~ param.dName);
            postCall ~= param.dName ~ " = cast(bool)_" ~ param.dName ~ ";";
          }
          else
            addCallParam("cast(" ~ param.cType ~ ")&" ~ param.dName);
        }
        else
          addCallParam(param.dName);
        break;
      case Enum, Flags:
        addDeclParam(param.directionStr ~ param.fullDType ~ " " ~ param.dName);
        addCallParam((param.direction != ParamDirection.In ? "&"d : "") ~ param.dName);
        break;
      case String:
        addDeclParam(param.directionStr ~ "string " ~ param.dName);

        if (param.direction == ParamDirection.In)
        {
          preCall ~= param.cType ~ " _" ~ param.dName ~ " = " ~ param.dName ~ ".toCString!(" ~ param.fullOwnerFlag
            ~ ".Malloc, " ~ (param.nullable ? "Yes"d : "No"d) ~ ".Nullable);";
          addCallParam("_" ~ param.dName);
        }
        else if (param.direction == ParamDirection.Out)
        {
          preCall ~= "char* _" ~ param.dName ~ ";";
          addCallParam("&_" ~ param.dName);
          postCall ~= param.dName ~ " = _" ~ param.dName ~ ".fromCString!(" ~ param.fullOwnerFlag ~ ".Free);";
        }
        else // InOut (transfer applies to both directions: with Full the callee frees the input and returns a new string)
        {
          preCall ~= "char* _" ~ param.dName ~ " = cast(char*)" ~ param.dName ~ ".toCString!(" ~ param.fullOwnerFlag
            ~ ".Malloc, " ~ (param.nullable ? "Yes"d : "No"d) ~ ".Nullable);";
          addCallParam("&_" ~ param.dName);
          postCall ~= param.dName ~ " = _" ~ param.dName ~ ".fromCString!(" ~ param.fullOwnerFlag ~ ".Free);";
        }

        break;
      case Struct, StructAlias:
        if (param.direction != ParamDirection.In)
        {
          addDeclParam(param.directionStr ~ param.fullDType ~ " " ~ param.dName);

          if (!param.callerAllocates)
          {
            // Copy for transfer full (Struct kind has a GType and _cDup), callee frees the input and returns a new value
            if (param.direction == ParamDirection.InOut)
              preCall ~= param.cTypeRemPtr ~ " _" ~ param.dName ~ " = cast(" ~ param.cTypeRemPtr ~ ")"
                ~ ((param.ownership == Ownership.Full && param.kind == Struct) ? (param.fullDType ~ "._cDup(cast(void*)&"
                ~ param.dName ~ ")") : ("&" ~ param.dName)) ~ ";";
            else
              preCall ~= param.cTypeRemPtr ~ " _" ~ param.dName ~ ";";

            addCallParam("&_" ~ param.dName);
            postCall ~= "if (_" ~ param.dName ~ ")";

            if (param.ownership == Ownership.Full)
              postCall ~= ["{", param.dName ~ " = *cast(" ~ param.fullDType ~ "*)_" ~ param.dName ~ ";",
                "cValueFree!(" ~ param.fullDType ~ ")(cast(void*)_" ~ param.dName ~ ");", "}"];
            else
              postCall ~= param.dName ~ " = *cast(" ~ param.fullDType ~ "*)_" ~ param.dName ~ ";";
          }
          else
            addCallParam("cast(" ~ param.cType ~ ")&" ~ param.dName);
        }
        else if ((param.nullable || param.optional) && param.cType.countStars == 1)
        {
          addDeclParam(param.fullDType ~ "* " ~ param.dName);

          if (param.ownership == Ownership.Full && param.kind == Struct) // Callee takes ownership, pass a copy
          {
            preCall ~= param.cType ~ " _" ~ param.dName ~ " = " ~ param.dName ~ " ? cast(" ~ param.cType ~ ")"
              ~ param.fullDType ~ "._cDup(cast(void*)" ~ param.dName ~ ") : null;";
            addCallParam("_" ~ param.dName);
          }
          else
            addCallParam("cast(" ~ param.cType ~ ")" ~ param.dName);
        }
        else
        {
          addDeclParam(param.fullDType ~ " " ~ param.dName);

          if (param.ownership == Ownership.Full && param.kind == Struct && param.cType.countStars == 1) // Pass a copy
          {
            preCall ~= param.cType ~ " _" ~ param.dName ~ " = cast(" ~ param.cType ~ ")" ~ param.fullDType
              ~ "._cDup(cast(void*)&" ~ param.dName ~ ");";
            addCallParam("_" ~ param.dName);
          }
          else
            addCallParam("cast(" ~ param.cType ~ ")" ~ (param.cType.countStars == 1 ? "&"d : "") ~ param.dName);
        }

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
        if (param.direction == ParamDirection.InOut && param.cType.countStars == 2)
        {
          addDeclParam("ref " ~ param.fullDType ~ " " ~ param.dName);
          preCall ~= param.cTypeRemPtr.stripConst ~ " _" ~ param.dName ~ " = " ~ param.dName ~ " ? cast("
            ~ param.cTypeRemPtr.stripConst ~ ")" ~ param.dName ~ "._cPtr(" ~ param.fullOwnerFlag ~ ".Dup) : null;";
          addCallParam("&_" ~ param.dName);

          if (param.kind == TypeKind.Object)
          {
            addImport("gobject.object");
            postCall ~= param.dName ~ " = gobject.object.ObjectWrap._getDObject!(" ~ param.fullDType ~ ")(_"
              ~ param.dName ~ ", " ~ param.fullOwnerFlag ~ ".Take);";
          }
          else
            postCall ~= param.dName ~ " = _" ~ param.dName ~ " ? new " ~ param.fullDType ~ "(cast(void*)_"
              ~ param.dName ~ ", " ~ param.fullOwnerFlag ~ ".Take) : null;";
        }
        else if (param.direction == ParamDirection.In || param.direction == ParamDirection.InOut)
        {
          addDeclParam(param.fullDType ~ " " ~ param.dName);
          addCallParam(param.dName ~ " ? cast(" ~ param.cType ~ ")" ~ param.dName ~ "._cPtr"
            ~ (param.canTransferFull ? ("(" ~ param.fullOwnerFlag ~ ".Dup)") : "") ~ " : null");
        }
        else if (func.unmapFunc.length > 0 && param is func.mapParam) // Mapped structure held by a MapScope
        {
          auto source = func.mapSource;
          addDeclParam("out " ~ func.mapScopeClass.fullDType ~ "." ~ func.mapScopeName ~ " " ~ param.dName);
          preCall ~= "auto _" ~ param.dName ~ " = cast(" ~ param.cType ~ ")" ~ param.dName ~ "._begin("
            ~ (source ? source.dName : "this"d) ~ ");";
          addCallParam("_" ~ param.dName);
          postCall ~= param.dName ~ "._mapped = _retval;";
        }
        else // Out
        {
          addDeclParam("out " ~ param.fullDType ~ " " ~ param.dName);

          auto st = cast(Structure)param.typeObject;
          if (st && st.pointer) // Pointer typedef (GdkAtom etc), C param is effectively **
          {
            preCall ~= param.cTypeRemPtr ~ " _" ~ param.dName ~ ";";
            addCallParam("&_" ~ param.dName);
            postCall ~= ["if (_" ~ param.dName ~ ")", param.dName ~ " = new " ~ param.fullDType ~ "(cast(void*)_"
              ~ param.dName ~ ", " ~ param.fullOwnerFlag ~ ".Take);"];
          }
          else if (param.callerAllocates && !param.cTypeRemPtr.endsWith('*')) // Out, caller allocates, and struct pass by value
          {
            if (param.kind == Boxed)
            {
              if (param.isOpaqueBoxedStackOut) // Opaque boxed with gid-c-layout; not used for ordinary boxed outs
              {
                preCall ~= param.cTypeRemPtr ~ " _" ~ param.dName ~ ";";
                addCallParam("&_" ~ param.dName);
                addImport("gid.gid");
                auto wrapOut = param.dName ~ " = new " ~ param.fullDType ~ "(boxedCopy(" ~ param.fullDType
                  ~ "._getGType, &_" ~ param.dName ~ "), Yes.Take);";
                // Only when definitions set gid-valid-if-return; a false return does not generally invalidate outs
                if (param.validIfReturn)
                  postCall ~= ["if (_retval)", wrapOut];
                else
                  postCall ~= wrapOut;
              }
              else
              {
                preCall ~= "auto _" ~ param.dName ~ " = boxedNew!" ~ param.cTypeRemPtr ~ "(" ~ param.fullDType
                  ~ "._getGType);";
                addCallParam("_" ~ param.dName);
                postCall ~= param.dName ~ " = " ~ "new " ~ param.fullDType ~ "(cast(void*)_" ~ param.dName
                  ~ ", Yes.Take);";
              }
            }
            else if (param.kind == Wrap && st && st.initOwnsMembers) // Callee initializes/fills owned C allocated members
            { // Zero initialized GLib allocation, owned by the wrapper (freed with the structure's free function)
              preCall ~= "auto _" ~ param.dName ~ " = cast(" ~ param.cTypeRemPtr ~ "*)gMalloc(" ~ param.cTypeRemPtr
                ~ ".sizeof);";
              addCallParam("_" ~ param.dName);
              addImport("gid.gid");
              postCall ~= param.dName ~ " = new " ~ param.fullDType ~ "(cast(void*)_" ~ param.dName ~ ", Yes.Take);";
            }
            else if (param.kind == Wrap)
            {
              preCall ~= param.cTypeRemPtr ~ " _" ~ param.dName ~ ";";
              addCallParam("&_" ~ param.dName);
              postCall ~= param.dName ~ " = " ~ "new " ~ param.fullDType ~ "(cast(void*)&_" ~ param.dName
                ~ ", No.Take);";

              if (st && st.borrowed && func.funcType == FuncType.Method) // Pointer fields may reference the instance
                postCall ~= param.dName ~ "._owner = this;";
            }
            else
              assert(0, "Unsupported caller allocates out param of type '" ~ param.fullDType.to!string ~ "' ("
                ~ param.kind.to!string ~ ") for " ~ param.fullDName.to!string);
          }
          else // Out pointer to struct or object
          {
            preCall ~= param.cTypeRemPtr ~ " _" ~ param.dName ~ ";";
            addCallParam("&_" ~ param.dName);

            if (param.kind == Object)
            {
              addImport("gobject.object");
              postCall ~= param.dName ~ " = gobject.object.ObjectWrap._getDObject!(" ~ param.fullDType ~ ")(_"
                ~ param.dName ~ ", " ~ param.fullOwnerFlag ~ ".Take);";
            }
            else if (borrowsFromInstance(param)) // Borrowed structure owned by the instance
              postCall ~= ["if (_" ~ param.dName ~ ")", param.dName ~ " = new " ~ param.fullDType ~ "(cast(void*)_"
                ~ param.dName ~ ", this);"];
            else
              postCall ~= ["if (_" ~ param.dName ~ ")", param.dName ~ " = new " ~ param.fullDType ~ "(cast(void*)_"
                ~ param.dName ~ ", " ~ param.fullOwnerFlag ~ ".Take);"];
          }
        }
        break;
      case Interface:
        addImport("gobject.object");

        if (param.direction == ParamDirection.InOut && param.cType.countStars == 2)
        {
          addDeclParam("ref " ~ param.fullDType ~ " " ~ param.dName);
          preCall ~= param.cTypeRemPtr.stripConst ~ " _" ~ param.dName ~ " = " ~ param.dName ~ " ? cast("
            ~ param.cTypeRemPtr.stripConst ~ ")(cast(gobject.object.ObjectWrap)" ~ param.dName ~ ")._cPtr("
            ~ param.fullOwnerFlag ~ ".Dup) : null;";
          addCallParam("&_" ~ param.dName);
          postCall ~= param.dName ~ " = gobject.object.ObjectWrap._getDObject!(" ~ param.fullDType ~ ")(_"
            ~ param.dName ~ ", " ~ param.fullOwnerFlag ~ ".Take);";
        }
        else if (param.direction == ParamDirection.In || param.direction == ParamDirection.InOut)
        {
          addImport("gobject.object");
          addDeclParam(param.fullDType ~ " " ~ param.dName);
          addCallParam(param.dName ~ " ? cast(" ~ param.cTypeRemPtr.stripConst ~ "*)(cast(gobject.object.ObjectWrap)"
            ~ param.dName ~ ")._cPtr(" ~ param.fullOwnerFlag ~ ".Dup) : null");
        }
        else if (param.direction == ParamDirection.Out)
        {
          addDeclParam("out " ~ param.fullDType ~ " " ~ param.dName);
          preCall ~= param.cTypeRemPtr ~ " _" ~ param.dName ~ ";";
          addCallParam("&_" ~ param.dName);
          postCall ~= param.dName ~ " = gobject.object.ObjectWrap._getDObject!(" ~ param.fullDType ~ ")(_"
            ~ param.dName ~ ", " ~ param.fullOwnerFlag ~ ".Take);";
        }
        break;
      case Unknown, Container, Namespace:
        assert(0, "Unsupported parameter type '" ~ param.fullDType.to!string ~ "' (" ~ param.kind.to!string ~ ") for "
            ~ param.fullDName.to!string);
    }

    // Optional parameters get a default value of null. This works because Param.verify() ensures
    // that optional parameters are only allowed for pointer/reference types (String, Callback,
    // Container, Pointer, Opaque, Wrap, Boxed, Reffed, Object, Interface, or void*).
    if (param.isOptional)
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

    with (TypeKind) if (param.ownership == Ownership.Container || (param.ownership == Ownership.Full
        && (elemType.kind.among(Pointer, Opaque) || (elemType.kind == Wrap && !elemType.typeObjectRoot.isPlainData))))
      assert(0, "Function array parameter " ~ param.fullDName.to!string ~ " ownership " ~ param.ownership.to!string
        ~ " not supported");

    // Fixed size arrays are sized by the wrapper, so the C function can't read past the end of the D array: longer
    // arrays are truncated, shorter ones are padded with zeros (empty nullable arrays are passed as null)
    auto fixedSize = param.fixedSize != ArrayNotFixed ? param.fixedSize.to!dstring : null;
    auto nullOk = param.nullable || param.isOptional;

    if (fixedSize)
      preCall ~= "if (" ~ param.dName ~ ".length > " ~ fixedSize ~ ") " ~ param.dName ~ " = " ~ param.dName ~ "[0 .. "
        ~ fixedSize ~ "];";

    auto zeroTerm = param.zeroTerminated ? " + 1"d : "";

    // Length of C arrays allocated for the callee (Ownership Full, gMalloc zeroes the padding of fixed size arrays)
    auto fullLen = !fixedSize ? param.dName ~ ".length" ~ zeroTerm : nullOk ? "(" ~ param.dName ~ ".length ? "
      ~ fixedSize ~ " : 0)" ~ zeroTerm : fixedSize ~ zeroTerm;

    final switch (elemType.kind) with (TypeKind)
    {
      case Basic, BasicAlias, Enum, Flags, StructAlias, Struct, Pointer:
        if (elemType.dType == "bool")
        {
          if (param.ownership == Ownership.None)
            preCall ~= ["auto _" ~ param.dName ~ "_arr = new gboolean[](" ~ param.dName ~ ".length" ~ zeroTerm ~ ");",
              "auto _" ~ param.dName ~ " = " ~ "_" ~ param.dName ~ "_arr.ptr" ~ ";"];
          else // Ownership Full
            preCall ~= ["auto _" ~ param.dName ~ " = cast(gboolean*)gMalloc((" ~ fullLen ~ ") * gboolean.sizeof);"];

          preCall ~= "foreach (i, b; " ~ param.dName ~ ") _" ~ param.dName ~ "[i] = cast(gboolean)b;";
          break;
        }

        if (param.ownership == Ownership.None)
        {
          if (param.zeroTerminated)
            preCall ~= ["auto _" ~ param.dName ~ "_arr = " ~ param.dName ~ " ~ " ~ (elemType.cType.endsWith("*")
              ? "null" : elemType.cType ~ ".init") ~ ";", "auto _" ~ param.dName ~ " = _" ~ param.dName ~ "_arr.ptr;"];
          else
            preCall ~= "auto _" ~ param.dName ~ " = cast(" ~ elemType.cType ~ "*)" ~ param.dName ~ ".ptr;";
        }
        else // Ownership Full
          preCall ~= ["auto _" ~ param.dName ~ " = cast(" ~ param.cType.stripConst ~ ")gMalloc((" ~ fullLen
            ~ ") * (" ~ elemType.cType ~ ").sizeof);", "_" ~ param.dName ~ "[0 .. " ~ param.dName
            ~ ".length] = " ~ (elemType.kind == Struct // D Struct types have the C layout, copy as the C type
            ? "(cast(" ~ elemType.cType ~ "*)" ~ param.dName ~ ".ptr)[0 .. " ~ param.dName ~ ".length];"
            : param.dName ~ "[];")];
        break;
      case String:
        if (param.ownership == Ownership.None)
          preCall ~= ["auto _" ~ param.dName ~ "_arr = new char*[](" ~ param.dName ~ ".length" ~ zeroTerm ~ ");",
            "auto _" ~ param.dName ~ " = cast(" ~ param.cType.stripConst ~ ")" ~ "_" ~ param.dName ~ "_arr.ptr" ~ ";"];
        else // Ownership Full
          preCall ~= ["auto _" ~ param.dName ~ " = cast(char**)gMalloc((" ~ fullLen ~ ") * (char*).sizeof);"];

        auto malloc = param.ownership == Ownership.None ? "No"d : "Yes"d;
        preCall ~= "foreach (i, s; " ~ param.dName ~ ") _" ~ param.dName ~ "[i] = s.toCString!(" ~ malloc
          ~ ".Malloc);";
        break;
      case Wrap:
        if (param.ownership == Ownership.None)
          preCall ~= ["auto _" ~ param.dName ~ "_arr = new " ~ elemType.cType ~ "[](" ~ param.dName ~ ".length"
            ~ zeroTerm ~ ");", "auto _" ~ param.dName ~ " = " ~ "_" ~ param.dName ~ "_arr.ptr" ~ ";"];
        else // Ownership Full
          preCall ~= ["auto _" ~ param.dName ~ " = cast(" ~ param.cType.stripConst ~ ")gMalloc((" ~ fullLen
            ~ ") * (" ~ elemType.cType ~ ").sizeof);"];

        preCall ~= "foreach (i, obj; " ~ param.dName ~ ") _" ~ param.dName ~ "[i] = *cast(" ~ elemType.cType
          ~ "*)obj._cPtr;";

        if (param.ownership == Ownership.None && !param.cType.canFind("const")) // Non-const array, C function may modify the structures, copy them back
          postCall ~= "foreach (i, obj; " ~ param.dName ~ ") *cast(" ~ elemType.cType ~ "*)obj._cPtr = _"
            ~ param.dName ~ "[i];";
        break;
      case Boxed:
        if (param.ownership == Ownership.None)
          preCall ~= ["auto _" ~ param.dName ~ "_arr = new " ~ elemType.cType ~ "[](" ~ param.dName ~ ".length"
            ~ zeroTerm ~ ");", "auto _" ~ param.dName ~ " = " ~ "_" ~ param.dName ~ "_arr.ptr" ~ ";"];
        else // Ownership Full
          preCall ~= ["auto _" ~ param.dName ~ " = cast(" ~ param.cType.stripConst ~ ")gMalloc((" ~ fullLen
            ~ ") * (" ~ elemType.cType ~ ").sizeof);"];

        auto ptr = elemType.cType.endsWith('*') ? ""d : "*"d;
        auto nullval = elemType.cType.endsWith('*') ? "null"d : (elemType.cType ~ ".init");
        auto dup = param.ownership == Ownership.None ? "No"d : "Yes"d;
        preCall ~= "foreach (i, obj; " ~ param.dName ~ ") _" ~ param.dName ~ "[i] = obj ? " ~ ptr ~ "cast("
          ~ elemType.cTypeRemPtr ~ "*)obj._cPtr(" ~ dup ~ ".Dup) : " ~ nullval ~ ";";
        break;
      case Reffed, Object:
        if (param.ownership == Ownership.None)
          preCall ~= ["auto _" ~ param.dName ~ "_arr = new " ~ elemType.cType ~ "[](" ~ param.dName ~ ".length"
            ~ zeroTerm ~ ");", "auto _" ~ param.dName ~ " = cast(" ~ param.cType.stripConst ~ ")" ~ "_" ~ param.dName
            ~ "_arr.ptr" ~ ";"];
        else // Ownership Full
          preCall ~= ["auto _" ~ param.dName ~ " = cast(" ~ param.cType.stripConst ~ ")gMalloc((" ~ fullLen
            ~ ") * (" ~ elemType.cType ~ ").sizeof);"];

        auto dup = param.ownership == Ownership.None ? "No"d : "Yes"d;
        preCall ~= "foreach (i, obj; " ~ param.dName ~ ") _" ~ param.dName ~ "[i] = obj ? cast(" ~ elemType.cType
          ~ ")obj._cPtr(" ~ dup ~ ".Dup) : null;";
        break;
      case Opaque:
        preCall ~= ["auto _" ~ param.dName ~ "_arr = new " ~ elemType.cType ~ "[](" ~ param.dName ~ ".length"
          ~ zeroTerm ~ ");", "auto _" ~ param.dName ~ " = " ~ "_" ~ param.dName ~ "_arr.ptr" ~ ";"];
        preCall ~= "foreach (i, obj; " ~ param.dName ~ ") _" ~ param.dName ~ "[i] = obj ? cast(" ~ elemType.cType
          ~ ")obj._cPtr: null;";
        break;
      case Interface:
        addImport("gobject.object");

        if (param.ownership == Ownership.None)
          preCall ~= ["auto _" ~ param.dName ~ "_arr = new " ~ elemType.cType ~ "[](" ~ param.dName ~ ".length"
            ~ zeroTerm ~ ");", "auto _" ~ param.dName ~ " = " ~ "_" ~ param.dName ~ "_arr.ptr" ~ ";"];
        else // Ownership Full
          preCall ~= ["auto _" ~ param.dName ~ " = cast(" ~ param.cType.stripConst ~ ")gMalloc((" ~ fullLen
            ~ ") * (" ~ elemType.cType ~ ").sizeof);"];

        auto dup = param.ownership == Ownership.None ? "No"d : "Yes"d;
        preCall ~= "foreach (i, obj; " ~ param.dName ~ ") _" ~ param.dName ~ "[i] = obj ? cast("
          ~ elemType.cType ~ ")(cast(gobject.object.ObjectWrap)obj)._cPtr(" ~ dup ~ ".Dup): null;";
        break;
      case Unknown, Callback, Container, Namespace:
        assert(0, "Unsupported parameter array type '" ~ elemType.fullDType.to!string ~ "' (" ~ elemType.kind.to!string
            ~ ") for " ~ param.fullDName.to!string);
    }

    // Short fixed size array passed without an ownership transfer: pass a zero padded copy of the fixed size
    if (fixedSize && param.ownership == Ownership.None)
    { // An empty nullable array is passed as null (the pointer of an empty D slice is not necessarily null)
      preCall ~= "typeof(*_" ~ param.dName ~ ")[" ~ fixedSize ~ "] _" ~ param.dName ~ "_fixed = void;";

      if (nullOk)
        preCall ~= ["if (" ~ param.dName ~ ".length == 0)", "_" ~ param.dName ~ " = null;"];

      preCall ~= [(nullOk ? "else if ("d : "if (") ~ param.dName ~ ".length < " ~ fixedSize ~ ")", "{",
        "(cast(ubyte[])_" ~ param.dName ~ "_fixed[])[] = 0;",
        "_" ~ param.dName ~ "_fixed[0 .. " ~ param.dName ~ ".length] = _" ~ param.dName ~ "[0 .. " ~ param.dName
        ~ ".length];", "_" ~ param.dName ~ " = _" ~ param.dName ~ "_fixed.ptr;", "}"];
    }

    if (param.isOptional) // If parameter is optional, set default value to null
      decl ~= " = null";
  }

  // Process an array output/inout parameter
  private void processArrayOutParam(Param param)
  {
    auto elemType = param.elemTypes[0];

    addDeclParam(param.directionStr ~ elemType.fullDType ~ "[] " ~ param.dName);

    dstring lengthStr;

    if (param.lengthParam) // Array has length parameter?
    { // gidgen extension which allows another zero-terminated array to be used for the output array length
      if (param.lengthParam.containerType == ContainerType.Array)
        lengthStr = param.lengthParam.dName ~ ".length";
      else if (param.lengthParam.isArraySize) // Input size, the array is not set if the function fails
        lengthStr = "(_" ~ param.dName ~ " ? " ~ param.lengthParam.dName ~ " : 0)";
      else
        lengthStr = "_" ~ param.lengthParam.dName;
    }
    else if (param.lengthReturn) // Array uses return value for length?
      lengthStr = "_ret_length";
    else if (param.fixedSize != ArrayNotFixed) // Array is a fixed size?
      lengthStr = param.fixedSize.to!dstring;
    else if (param.zeroTerminated) // Array is zero terminated?
    {
      postCall ~= ["uint _len" ~ param.dName ~ ";", "if (_" ~ param.dName ~ ")", "{", "for (; _" ~ param.dName
        ~ "[_len" ~ param.dName ~ "] " ~ (elemType.cTypeRemPtr.endsWith("*") ? "!is null"d : "!= 0") ~ "; _len"
        ~ param.dName ~ "++)", "{", "}", "}", ""];
      lengthStr = "_len" ~ param.dName;
    }
    else if (param.arrayLengthIndex != ArrayLengthCaller)
      assert(0); // This should be prevented by verify()

    auto inOutFull = param.direction == ParamDirection.InOut && param.ownership == Ownership.Full;

    if (inOutFull) // Callee may free or reallocate the array: pass a GLib owned copy, result is converted back below
    {
      auto n = param.dName;
      auto zeroTerm = param.zeroTerminated ? " + 1"d : "";

      if (elemType.kind == TypeKind.String)
        preCall ~= ["auto _" ~ n ~ " = cast(char**)gMalloc((" ~ n ~ ".length" ~ zeroTerm ~ ") * (char*).sizeof);",
          "foreach (i, s; " ~ n ~ ") _" ~ n ~ "[i] = s.toCString!(Yes.Malloc);"];
      else
      { // Element C type from the array C type (the GIR element c:type of InOut arrays can include the out pointer)
        auto arrCType = param.cTypeRemPtr;
        auto elemCType = arrCType.endsWith("*") ? arrCType[0 .. $ - 1] : elemType.cType;
        preCall ~= ["auto _" ~ n ~ " = cast(" ~ arrCType ~ ")gMalloc((" ~ n ~ ".length" ~ zeroTerm
          ~ ") * (" ~ elemCType ~ ").sizeof);", "_" ~ n ~ "[0 .. " ~ n ~ ".length] = (cast(" ~ elemCType
          ~ "*)" ~ n ~ ".ptr)[0 .. " ~ n ~ ".length];"];
      }
    }

    final switch (elemType.kind) with (TypeKind)
    {
      case Basic, BasicAlias, Enum, Flags, StructAlias, Struct, Pointer:
        if (!param.callerAllocates)
        {
          if (!inOutFull)
            preCall ~= param.cTypeRemPtr ~ " _" ~ param.dName ~ ";";

          addCallParam("&_" ~ param.dName);
          postCall ~= [param.dName ~ ".length = " ~ lengthStr ~ ";", param.dName ~ "[0 .. $] = (cast("
            ~ elemType.fullDType ~ "*)_" ~ param.dName ~ ")[0 .. " ~ lengthStr ~ "];"];

          if (param.ownership != Ownership.None)
            postCall ~= "gFree(cast(void*)_" ~ param.dName ~ ");";
        }
        else // Caller allocated array
        { // Out array with a length parameter which is not another array (gidgen extension): init count to array
          // length (InOut array lengths are already set above)
          if (param.direction == ParamDirection.Out && param.lengthParam
              && param.lengthParam.containerType != ContainerType.Array)
            preCall ~= "_" ~ param.lengthParam.dName ~ " = cast(" ~ param.lengthParam.fullDType ~ ")"
              ~ param.dName ~ ".length;";

          // Fixed size array: the D array is sized by the wrapper (Out arrays are allocated, InOut arrays are
          // truncated or extended with default values), so the C function can't write past its end
          if (param.fixedSize != ArrayNotFixed)
            preCall ~= param.dName ~ ".length = " ~ param.fixedSize.to!dstring ~ ";";

          if (elemType.kind == TypeKind.Struct)
            addCallParam("cast(" ~ param.cType ~ ")" ~ param.dName ~ ".ptr");
          else
            addCallParam(param.dName ~ ".ptr");
        }
        break;
      case String:
        if (!param.callerAllocates)
        {
          if (!inOutFull)
            preCall ~= param.cTypeRemPtr ~ " _" ~ param.dName ~ ";";

          addCallParam("&_" ~ param.dName);
          postCall ~= [param.dName ~ ".length = " ~ lengthStr ~ ";", "foreach (i; 0 .. " ~ lengthStr ~ ")",
            param.dName ~ "[i] = _" ~ param.dName ~ "[i].fromCString!(" ~ param.fullOwnerFlag ~ ".Free);"];

          if (param.ownership != Ownership.None)
            postCall ~= "gFree(cast(void*)_" ~ param.dName ~ ");";
        }
        else
        {
          preCall ~= [elemType.cType ~ "[] _" ~ param.dName ~ ";", "_" ~ param.dName ~ ".length = " ~ lengthStr ~ ";"];
          addCallParam("_" ~ param.dName ~ ".ptr");
          postCall ~= [param.dName ~ ".length = " ~ lengthStr ~ ";", "foreach (i; 0 .. " ~ lengthStr ~ ")", 
            param.dName ~ "[i] = _" ~ param.dName ~ "[i].fromCString!(" ~ param.fullOwnerFlag ~ ".Free);"];
        }
        break;
      case Opaque, Wrap, Boxed, Reffed:
        auto inlineElems = param.arrayPtrDepth == (param.callerAllocates ? 1 : 2);
        auto elemExpr = (inlineElems ? "&_"d : "_"d) ~ param.dName ~ "[i]";
        auto takeStr = inlineElems ? "No.Take"d : param.fullOwnerFlag ~ ".Take";

        if (!param.callerAllocates)
        {
          preCall ~= param.cTypeRemPtr ~ " _" ~ param.dName ~ ";";
          addCallParam("&_" ~ param.dName);
        }
        else
        {
          preCall ~= [elemType.cType ~ "[] _" ~ param.dName ~ ";", "_" ~ param.dName ~ ".length = " ~ lengthStr ~ ";"];
          addCallParam("_" ~ param.dName ~ ".ptr");
        }

        postCall ~= [param.dName ~ ".length = " ~ lengthStr ~ ";", "foreach (i; 0 .. " ~ lengthStr ~ ")",
          param.dName ~ "[i] = new " ~ elemType.fullDType ~ "(cast(void*)" ~ elemExpr ~ ", " ~ takeStr ~ ");"];

        if (!param.callerAllocates && param.ownership != Ownership.None)
          postCall ~= "gFree(cast(void*)_" ~ param.dName ~ ");";
        break;
      case Object, Interface:
        addImport("gobject.object");

        if (!param.callerAllocates)
        {
          preCall ~= param.cTypeRemPtr ~ " _" ~ param.dName ~ ";";
          addCallParam("&_" ~ param.dName);
          postCall ~= [param.dName ~ ".length = " ~ lengthStr ~ ";", "foreach (i; 0 .. " ~ lengthStr ~ ")",
            param.dName ~ "[i] = gobject.object.ObjectWrap._getDObject!(" ~ elemType.fullDType ~ ")(_" ~ param.dName
            ~ "[i], " ~ param.fullOwnerFlag ~ ".Take);"];

          if (param.ownership != Ownership.None)
            postCall ~= "gFree(cast(void*)_" ~ param.dName ~ ");";
        }
        else
        {
          preCall ~= [elemType.cType ~ "[] _" ~ param.dName ~ ";", "_" ~ param.dName ~ ".length = " ~ lengthStr ~ ";"];
          addCallParam("_" ~ param.dName ~ ".ptr");
          postCall ~= [param.dName ~ ".length = " ~ lengthStr ~ ";", "foreach (i; 0 .. " ~ lengthStr ~ ")",
            param.dName ~ "[i] = gobject.object.ObjectWrap._getDObject!(" ~ elemType.fullDType ~ ")(cast(void*)_"
            ~ param.dName ~ "[i], " ~ param.fullOwnerFlag ~ ".Take);"];
        }
        break;
      case Unknown, Callback, Container, Namespace:
        assert(0, "Unsupported parameter array type '" ~ elemType.fullDType.to!string ~ "' (" ~ elemType.kind.to!string
            ~ ") for " ~ param.fullDName.to!string);
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
          ~ param.fullDName.to!string);
    }

    addDeclParam(param.directionStr ~ param.fullDType ~ " " ~ param.dName);

    preCall ~= "auto _" ~ param.dName ~ " = g" ~ param.containerType.to!dstring ~ "FromD" ~ templateParams ~ "("
      ~ param.dName ~ ");";

    if (param.ownership != Ownership.Full)
      preCall ~= "scope(exit) containerFree!(" ~ param.cType.stripConst ~ ", " ~ param.elemTypes[0].fullDType
        ~ ", GidOwnership." ~ param.ownership.to!dstring ~ ")(_" ~ param.dName ~ ");";

    addCallParam("_" ~ param.dName);

    if (param.isOptional) // If parameter is optional, set default to null (works for dynamic arrays and associative arrays)
      decl ~= " = null";
  }

  // Process a container out/inout parameter (except array)
  private void processContainerOutParam(Param param)
  {
    bool isList = param.containerType == ContainerType.List || param.containerType == ContainerType.SList;
    dstring ownershipStr = (param.callerAllocates ? Ownership.Full : param.ownership).to!dstring;
    dstring toDParams;

    final switch (param.containerType) with(ContainerType)
    {
      case ByteArray:
        toDParams = "GidOwnership." ~ ownershipStr;
        break;
      case ArrayG, PtrArray, List, SList:
        toDParams = param.elemTypes[0].fullDType  ~ ", " ~ "GidOwnership." ~ ownershipStr;
        break;
      case HashTable:
        toDParams = param.elemTypes[0].fullDType ~ ", " ~ param.elemTypes[1].fullDType ~ ", " ~ "GidOwnership."
          ~ ownershipStr;
        break;
      case Array, None:
        assert(0, "Unsupported '" ~ param.direction.to!string ~ "' container type '" ~ param.containerType.to!string
          ~ "' for " ~ param.fullDName.to!string);
    }

    addDeclParam(param.directionStr ~ param.fullDType ~ " " ~ param.dName);
    addCallParam(((isList || !param.callerAllocates) ? "&_"d : "_"d) ~ param.dName);

    if (param.callerAllocates)
      preCall ~= ["auto _" ~ param.dName ~ " = g" ~ param.containerType.to!dstring ~ "FromD(" ~ param.dName ~ ");",
        "scope(failure) containerFree!(" ~ (isList ? param.cTypeRemPtr : param.cType) ~ ", "
        ~ param.elemTypes[0].fullDType ~ ", GidOwnership.None)(_" ~ param.dName ~ ");"]; // GidOwnership.None for containerFree frees container and elements
    else
      preCall ~= param.cTypeRemPtr ~ " _" ~ param.dName ~ ";";

    postCall ~= param.dName ~ " = g" ~ param.containerType.to!dstring ~ "ToD!(" ~ toDParams ~ ")(_"
      ~ param.dName ~ ");";
  }

  /**
   * Write function binding to a CodeWriter.
   * Params:
   *   writer = Code writer to write to.
   *   moduleType = Module file type being written (defaults to ModuleType.Normal)
   */
  void write(CodeWriter writer)
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
    writer ~= overrideStr ~ decl ~ (func.throws ? ""d : " nothrow"d); // Add nothrow if the function itself does not throw

    writer ~= "{";

    if (preCall.length > 0)
      writer ~= preCall;

    writer ~= call;

    if (postCall.length > 0)
      writer ~= postCall;

    if (func.returnVal && func.returnVal.origDType != "none" && func.returnVal.active == Active.Enabled && !func.isCtor
        && func.returnVal.lengthArrayParams.length == 0) // Don't return a value for array length return values
      writer ~= "return _retval;";

    writer ~= "}";
  }

  Func func; /// The function object being written
  ModuleType moduleType; /// Module type being written
  Structure conflictClass; /// Set to an ancestor class that has a conflicting method
  bool conflictConforms; /// Set to true if the conflicting method conforms to func (override vs alias)
  dstring decl; /// Function declaration
  dstring call; /// The C function call
  LineTracker preCall; /// Pre-call code for call return variable, call output parameter variables, and input variable processing
  LineTracker postCall; /// Post-call code for return value processing, output parameter processing, and input variable cleanup
}
