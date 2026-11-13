module gir.deleg_writer;

import code_writer;
import defs;
import gir.func;
import gir.param;
import gir.return_value;
import gir.structure;
import gir.type_node;
import import_manager;
import line_tracker;
import std_includes;
import utils;

/// Delegate C callback marshal writer class
class DelegWriter
{
  this(Param delegParam, bool staticDelegatePtr)
  {
    this.delegParam = delegParam;
    callback = cast(Func)delegParam.typeObjectRoot;

    assert(this.callback, "DelegWriter parameter " ~ delegParam.fullDName.to!string ~ " has "
      ~ delegParam.typeObjectRoot.to!string ~ " typeObjectRoot");

    preCall = new LineTracker;
    postCall = new LineTracker;

    process(staticDelegatePtr);
  }

  // Process the delegate parameter
  private void process(bool staticDelegatePtr)
  {
    codeTrap("deleg.write", delegParam.fullDName);

    decl ~= "extern(C) ";

    if (delegParam.scope_ == ParamScope.Async)
      preCall ~= "ptrThawGC(" ~ callback.closureExpr ~ ");"; // If it is an asynchronous callback, delegate can be thawed immediately (will continue to exist so long as it is in use)

    processReturn();

    decl ~= "_" ~ delegParam.dName ~ "Callback(";

    if (!staticDelegatePtr)
    {
      preCall ~= "auto _dlg = cast(" ~ delegParam.fullDType ~ "*)" ~ callback.closureExpr ~ ";";
      call ~= "(*_dlg)(";
    }
    else
      call ~= "_static_" ~ delegParam.dName ~ "("; // Call the static per thread delegate pointer which is used to pass it from the outer function to the C callback

    foreach (param; callback.params)
      processParam(param);

    if (callback.throws)
    {
      addDeclParam("GError **_err");
      addCallParam("_err");
    }

    decl ~= ") nothrow";
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
    auto retVal = callback.returnVal;

    if (!retVal || retVal.origDType == "none")
    {
      decl ~= "void ";
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

    decl ~= retVal.cType ~ " ";

    if (retVal.cType == retVal.fullDType)
    {
      preCall ~= retVal.cType ~ " _retval;";
      call ~= "_retval = ";
      return;
    }

    preCall ~= retVal.fullDType ~ " _dretval;";
    call ~= "_dretval = ";

    final switch (retVal.kind) with (TypeKind)
    {
      case Basic, BasicAlias, Enum, Flags, Pointer:
        postCall ~= "auto _retval = cast(" ~ retVal.cType ~ ")_dretval;";
        break;
      case StructAlias, Struct:
        if (retVal.cType.countStars == 0)
          postCall ~= "auto _retval = cast(" ~ retVal.cType ~ ")_dretval;";
        else // Copy owned by the caller (Struct has _cDup), other ownership is disabled in Func.verify
          postCall ~= "auto _retval = cast(" ~ retVal.cType ~ ")" ~ retVal.fullDType ~ "._cDup(cast(void*)&_dretval);";
        break;
      case String:
        postCall ~= "auto _retval = toCString!(Yes.Malloc, " ~ (retVal.nullable ? "Yes"d : "No"d)
          ~ ".Nullable)(_dretval);";
        break;
      case Opaque, Wrap, Boxed, Reffed, Object:
        postCall ~= "auto _retval = _dretval ? cast(" ~ retVal.cTypeRemPtr ~ "*)_dretval._cPtr(" ~ retVal.fullOwnerFlag
          ~ ".Dup) : null;";
        break;
      case Interface:
        addImport("gobject.object");
        postCall ~= "auto _retval = _dretval ? cast(" ~ retVal.cTypeRemPtr
          ~ "*)(cast(gobject.object.ObjectWrap)_dretval)._cPtr(" ~ retVal.fullOwnerFlag ~ ".Dup) : null;";
        break;
      case Callback, Unknown, Container, Namespace:
        assert(0, "Unsupported delegate return value type '" ~ retVal.fullDType.to!string
          ~ "' (" ~ retVal.kind.to!string ~ ") for " ~ callback.fullDName.to!string);
    }
  }

  /**
   * Generate code converting a D array to a new C array (allocated with gMalloc, the caller takes ownership).
   * Params:
   *   arr = The array type node (return value or parameter)
   *   dVar = D array variable
   *   cVar = Declared C array variable to assign (null if the D array is empty)
   * Returns: Code lines
   */
  private dstring[] dArrayToC(TypeNode arr, dstring dVar, dstring cVar)
  {
    dstring[] lines;
    auto elemType = arr.elemTypes[0];

    dstring cLen = dVar ~ ".length"; // Length of the C array

    // Fixed size array? The C array always has the fixed size: longer D arrays are truncated, shorter ones are padded
    // with zeros (an empty D array is passed as null if the array is nullable)
    if (arr.fixedSize != ArrayNotFixed)
    {
      auto param = cast(Param)arr;
      auto retVal = cast(ReturnValue)arr;
      auto nullable = (param && param.nullable) || (retVal && retVal.nullable);
      auto fixedSize = arr.fixedSize.to!dstring;
      lines ~= "if (" ~ dVar ~ ".length > " ~ fixedSize ~ ") " ~ dVar ~ " = " ~ dVar ~ "[0 .. " ~ fixedSize ~ "];";
      cLen = fixedSize;

      if (!nullable)
        lines ~= ["if (" ~ dVar ~ ".length == 0)", cVar ~ " = cast(typeof(" ~ cVar ~ "))gMalloc(" ~ fixedSize
          ~ (arr.zeroTerminated ? " + 1"d : "") ~ " * (*" ~ cVar ~ ").sizeof);", ""];
    }

    lines ~= ["if (" ~ dVar ~ ".length > 0)", "{"];

    // gMalloc zeroes the memory, which also sets the zero terminator
    lines ~= cVar ~ " = cast(typeof(" ~ cVar ~ "))gMalloc(" ~ (arr.zeroTerminated ? "(" ~ cLen ~ " + 1)"
      : cLen) ~ " * (*" ~ cVar ~ ").sizeof);";

    with (TypeKind) if (!elemType.kind.among(Basic, BasicAlias, Enum, Flags, StructAlias, Struct, Pointer) // Can array be directly copied?
        || elemType.dType == "bool")
    {
      lines ~= ["", "foreach (i; 0 .. " ~ dVar ~ ".length)"];

      final switch (elemType.kind) with (TypeKind)
      {
        case Basic, BasicAlias:
          lines ~= cVar ~ "[i] = " ~ dVar ~ "[i];"; // Convert between bool and gboolean
          break;
        case String:
          lines ~= cVar ~ "[i] = toCString!(Yes.Malloc)(" ~ dVar ~ "[i]);";
          break;
        case Opaque, Wrap, Boxed, Reffed, Object:
          lines ~= cVar ~ "[i] = " ~ dVar ~ "[i] ? cast(typeof(*" ~ cVar ~ "))" ~ dVar ~ "[i]._cPtr(" ~ arr.fullOwnerFlag
            ~ ".Dup) : null;";
          break;
        case Interface:
          addImport("gobject.object");
          lines ~= cVar ~ "[i] = " ~ dVar ~ "[i] ? cast(typeof(*" ~ cVar ~ "))(cast(gobject.object.ObjectWrap)" ~ dVar
            ~ "[i])._cPtr(" ~ arr.fullOwnerFlag ~ ".Dup) : null;";
          break;
        case Enum, Flags, StructAlias, Struct, Pointer, Callback, Unknown, Container, Namespace:
          assert(0, "Unsupported delegate array type '" ~ elemType.fullDType.to!string
            ~ "' (" ~ elemType.kind.to!string ~ ") for " ~ callback.fullDName.to!string);
      }
    }
    else if (elemType.kind == TypeKind.Struct) // D Struct types have the C layout, copy as the C type
      lines ~= cVar ~ "[0 .. " ~ dVar ~ ".length] = (cast(typeof(" ~ cVar ~ "))" ~ dVar ~ ".ptr)[0 .. " ~ dVar
        ~ ".length];";
    else // Data is compatible between D and C types, do array copy
      lines ~= cVar ~ "[0 .. " ~ dVar ~ ".length] = " ~ dVar ~ "[];";

    lines ~= ["}", ""]; // Close of "if (<dVar>.length > 0)" statement
    return lines;
  }

  /// Process array return value
  private void processReturnArray()
  {
    auto retVal = callback.returnVal;

    assert(retVal.ownership == Ownership.Full, "Unsupported delegate return array ownership '"
      ~ retVal.ownership.to!string ~ "' for " ~ callback.fullDName.to!string);

    auto elemType = retVal.elemTypes[0];

    decl ~= retVal.cType ~ " ";
    preCall ~= elemType.fullDType ~ "[] _dretval;";
    call ~= "_dretval = ";
    postCall ~= [retVal.cType ~ " _retval;", ""];

    postCall ~= dArrayToC(retVal, "_dretval", "_retval");

    if (retVal.lengthParam) // Array has length parameter?
      postCall ~= "*" ~ retVal.lengthParam.dName ~ " = cast(typeof(*" ~ retVal.lengthParam.dName ~ "))_dretval.length;";
  }

  /// Template parameters of the g<Container>FromD conversion of a container (not Array) passed from D to C
  private dstring fromDTemplateParams(TypeNode node)
  {
    switch (node.containerType) with(ContainerType)
    {
      case ByteArray:
        return "";
      case ArrayG, PtrArray:
        return "!(" ~ node.elemTypes[0].fullDType  ~ ", " ~ node.zeroTerminated.to!dstring ~ ")";
      case List, SList:
        return "!(" ~ node.elemTypes[0].fullDType ~ ")";
      case HashTable:
        return "!(" ~ node.elemTypes[0].fullDType ~ ", " ~ node.elemTypes[1].fullDType ~ ")";
      default:
        assert(0, "Unsupported delegate container type '" ~ node.containerType.to!string ~ "' for "
          ~ node.fullDName.to!string);
    }
  }

  /// Process a return container (not Array)
  private void processReturnContainer()
  {
    auto retVal = callback.returnVal;
    decl ~= retVal.cType ~ " ";
    preCall ~= retVal.dType ~ " _dretval;";
    call ~= "_dretval = ";
    postCall ~= "auto _retval = g" ~ retVal.containerType.to!dstring ~ "FromD" ~ fromDTemplateParams(retVal)
      ~ "(_dretval);";
  }

  /// Process a container output parameter (not Array), the caller takes ownership (see Func.verify)
  private void processContainerOutParam(Param param)
  {
    addDeclParam(param.cType ~ " " ~ param.dName);
    preCall ~= param.fullDType ~ " _" ~ param.dName ~ ";";
    addCallParam("_" ~ param.dName);
    postCall ~= "*" ~ param.dName ~ " = g" ~ param.containerType.to!dstring ~ "FromD" ~ fromDTemplateParams(param)
      ~ "(_" ~ param.dName ~ ");";
  }

  /// Process parameter
  private void processParam(Param param)
  {
    if (param.isInstanceParam) // Instance parameter or closure data? Skip
      return;

    if (param.isArrayLength) // Array length parameter?
    {
      addDeclParam(param.cType ~ " " ~ param.dName);
      return;
    }

    if (param.containerType == ContainerType.Array) // Array container?
    {
      processArrayParam(param);
      return;
    }
    else if (param.containerType != ContainerType.None) // Other type of container?
    {
      if (param.direction == ParamDirection.In)
        processContainerInParam(param);
      else if (param.direction == ParamDirection.Out)
        processContainerOutParam(param);
      else
          assert(0, "Delegate container " ~ param.directionStr.to!string ~ " parameters of type '"
            ~ param.kind.to!string ~ "' not supported");

      return;
    }

    addDeclParam(param.cType ~ " " ~ param.dName);

    if (param is callback.closureParam) // Closure parameter?
      return;

    // Out pointer target (const is cast away for "const T**" out parameters)
    auto outTarget = param.cType.startsWith("const") ? "cast(" ~ param.cType.stripConst ~ ")" ~ param.dName
      : param.dName;

    final switch (param.kind) with (TypeKind)
    {
      case Basic, BasicAlias, Enum, Flags, Pointer:
        if (param.dType == "bool")
        { // Bool parameters need to be converted between bool and gboolean
          if (param.direction != ParamDirection.In)
          {
            preCall ~= "bool _" ~ param.dName
              ~ (param.direction == ParamDirection.InOut ? " = cast(bool)*" ~ param.dName ~ ";" : ";");
            addCallParam("_" ~ param.dName); // Parameter is "out"
            postCall ~= "*" ~ param.dName ~ " = _" ~  param.dName ~ ";";
          }
          else
            addCallParam("cast(bool)" ~ param.dName);
        }
        else
          addCallParam(param.direction == ParamDirection.In ? param.dName : "*" ~ param.dName);
        break;
      case String:
        if (param.direction == ParamDirection.In)
        {
          preCall ~= "string _" ~ param.dName ~ " = " ~ param.dName ~ ".fromCString!(" ~ param.fullOwnerFlag ~ ".Free);";
          addCallParam("_" ~ param.dName);
        }
        else if (param.direction == ParamDirection.Out)
        {
          preCall ~= "string _" ~ param.dName ~ ";";
          addCallParam("_" ~ param.dName);
          postCall ~= "*" ~ outTarget ~ " = _" ~ param.dName ~ ".toCString!(" ~ param.fullOwnerFlag ~ ".Malloc, "
            ~ (param.nullable ? "Yes"d : "No"d) ~ ".Nullable);";
        }
        else // InOut (transfer applies to both directions: with Full the input is freed and a new string is returned)
        {
          preCall ~= "string _" ~ param.dName ~ " = (*" ~ param.dName ~ ").fromCString!(" ~ param.fullOwnerFlag
            ~ ".Free);";
          addCallParam("_" ~ param.dName);
          postCall ~= "*" ~ outTarget ~ " = _" ~ param.dName ~ ".toCString!(" ~ param.fullOwnerFlag ~ ".Malloc, "
            ~ (param.nullable ? "Yes"d : "No"d) ~ ".Nullable);";
        }
        break;
      case StructAlias, Struct:
        if (param.direction == ParamDirection.Out && param.cType.countStars == 2) // Out pointer to a new structure
        {
          preCall ~= param.fullDType ~ " _" ~ param.dName ~ ";";
          addCallParam("_" ~ param.dName);
          // Copy owned by the caller (Struct has _cDup), other ownership is disabled in Func.verify
          postCall ~= "*" ~ param.dName ~ " = cast(" ~ param.cTypeRemPtr ~ ")" ~ param.fullDType ~ "._cDup(cast(void*)&_"
            ~ param.dName ~ ");";
        }
        else
        {
          // An InOut pointer to a structure pointer is modified in place (pointer and its ownership remain with C)
          addCallParam("*cast(" ~ param.fullDType ~ "*)" ~ (param.cType.countStars == 2 ? "*"d : "") ~ param.dName);

          // Free a structure passed with transfer full after it was copied to the D delegate parameter
          if (param.direction == ParamDirection.In && param.ownership == Ownership.Full && param.kind == Struct
              && param.cType.countStars == 1)
            postCall ~= param.fullDType ~ "._cFree(cast(void*)" ~ param.dName ~ ");";
        }
        break;
      case Opaque, Wrap, Boxed, Reffed, Object, Interface:
        if (param.direction == ParamDirection.In)
        {
          // Named temp when later InOut params borrow from this parameter (gid-borrow)
          auto needOwnerTemp = callback.isParamBorrowOwner(param);

          if (param.kind == TypeKind.Object || param.kind == TypeKind.Interface)
          {
            addImport("gobject.object");
            if (needOwnerTemp)
            {
              preCall ~= "auto _" ~ param.dName ~ " = gobject.object.ObjectWrap._getDObject!(" ~ param.fullDType
                ~ ")(cast(void*)" ~ param.dName ~ ", " ~ param.fullOwnerFlag ~ ".Take);";
              addCallParam("_" ~ param.dName);
            }
            else
              addCallParam("gobject.object.ObjectWrap._getDObject!(" ~ param.fullDType ~ ")(cast(void*)" ~ param.dName
                ~ ", " ~ param.fullOwnerFlag ~ ".Take)");
          }
          else if (param.isBorrowedArg) // Borrowed for the duration of the call (no owner, not copied)
          {
            preCall ~= "auto _" ~ param.dName ~ " = " ~ param.dName ~ " ? new " ~ param.fullDType ~ "(cast(void*)"
              ~ param.dName ~ ", null) : null;";
            addCallParam("_" ~ param.dName);
          }
          else if (needOwnerTemp)
          {
            preCall ~= "auto _" ~ param.dName ~ " = " ~ param.dName ~ " ? new " ~ param.fullDType ~ "(cast(void*)"
              ~ param.dName ~ ", " ~ param.fullOwnerFlag ~ ".Take) : null;";
            addCallParam("_" ~ param.dName);
          }
          else if (param.isInPlaceBoxedArg) // Borrows the C instance during the call, so changes are seen by the caller
          {
            preCall ~= "auto _" ~ param.dName ~ " = " ~ param.dName ~ " ? new " ~ param.fullDType ~ "(cast(void*)"
              ~ param.dName ~ ", Yes.Take)._cBorrow : null;";
            addCallParam("_" ~ param.dName);

            // The wrapper takes a copy of the C instance after the call, in case the delegate kept a reference
            postCall ~= "if (_" ~ param.dName ~ ")\n  _" ~ param.dName ~ "._cUnborrow;";
          }
          else
            addCallParam(param.dName ~ " ? new " ~ param.fullDType ~ "(cast(void*)" ~ param.dName ~ ", "
              ~ param.fullOwnerFlag ~ ".Take) : null");
        }
        else if (param.direction == ParamDirection.Out)
        {
          preCall ~= param.fullDType ~ " _" ~ param.dName ~ ";";
          addCallParam("_" ~ param.dName);

          if (param.cTypeRemPtr.endsWith('*')) // Out pointer value?
            postCall ~= "*" ~ outTarget ~ " = _" ~ param.dName ~ " ? cast(" ~ param.cTypeRemPtr.stripConst ~ ")_"
              ~ param.dName ~ "._cPtr(" ~ param.fullOwnerFlag ~ ".Dup) : null;";
          else // Out pass by value: if the D Out value is null, leave the C value struct alone (assumes the caller initialized it or knows when the value is valid)
            postCall ~= "if (_" ~ param.dName ~ ")\n{\n auto p_ = _" ~ param.dName ~ "._cPtr(Yes.Dup);\n*" ~ param.dName
              ~ " = *cast(" ~ param.cType ~ ")p_;\ngFree(p_);\n}\n";
        }
        else if (param.isBorrowedInOut) // Transfer none Wrap**: borrow from owner, null removes, replace is an error
        {
          auto owner = callback.paramBorrowOwner(param);
          assert(owner, "Borrowed InOut parameter '" ~ param.fullDName.to!string ~ "' has no owner");

          preCall ~= "auto _" ~ param.dName ~ "Orig = *" ~ param.dName ~ ";";
          preCall ~= "auto _" ~ param.dName ~ " = _" ~ param.dName ~ "Orig ? new " ~ param.fullDType
            ~ "(cast(void*)_" ~ param.dName ~ "Orig, _" ~ owner.dName ~ ") : null;";
          addCallParam("_" ~ param.dName);

          addImport("glib.c.functions");
          addImport("glib.c.types");
          // Null clears the slot (C removes the value). A different non-null pointer is a programmer error: log a
          // critical, restore the original, and stop a boolean callback. Ownership of the C structure never moves to D.
          auto post = [
            "if (!_" ~ param.dName ~ ")",
            "  *" ~ outTarget ~ " = null;",
            "else if (_" ~ param.dName ~ "._cPtr !is cast(void*)_" ~ param.dName ~ "Orig)",
            "{",
            "  g_log(\"Gid\", GLogLevelFlags.LevelCritical, \"%s\",",
            "    \"" ~ callback.fullDName ~ " replaced the borrowed " ~ param.dName ~ "\".ptr);",
            "  *" ~ outTarget ~ " = _" ~ param.dName ~ "Orig;",
          ];
          if (callback.returnVal && callback.returnVal.origDType != "none")
            post ~= "  _retval = cast(typeof(_retval))0;";
          post ~= ["}", "else", "  *" ~ outTarget ~ " = _" ~ param.dName ~ "Orig;"];
          postCall ~= post;
        }
        else // InOut pointer with full transfer (see Func.verify): the D object takes the C instance and the C side
        { // receives a new reference or copy of the resulting D object (which may be the same object, another or null)
          if (param.kind == TypeKind.Object || param.kind == TypeKind.Interface)
          {
            addImport("gobject.object");
            preCall ~= "auto _" ~ param.dName ~ " = gobject.object.ObjectWrap._getDObject!(" ~ param.fullDType
              ~ ")(cast(void*)*" ~ param.dName ~ ", " ~ param.fullOwnerFlag ~ ".Take);";
          }
          else
            preCall ~= "auto _" ~ param.dName ~ " = *" ~ param.dName ~ " ? new " ~ param.fullDType ~ "(cast(void*)*"
              ~ param.dName ~ ", " ~ param.fullOwnerFlag ~ ".Take) : null;";

          addCallParam("_" ~ param.dName);

          if (param.kind == TypeKind.Interface)
            postCall ~= "*" ~ outTarget ~ " = _" ~ param.dName ~ " ? cast(" ~ param.cTypeRemPtr.stripConst
              ~ ")(cast(gobject.object.ObjectWrap)_" ~ param.dName ~ ")._cPtr(" ~ param.fullOwnerFlag ~ ".Dup) : null;";
          else
            postCall ~= "*" ~ outTarget ~ " = _" ~ param.dName ~ " ? cast(" ~ param.cTypeRemPtr.stripConst ~ ")_"
              ~ param.dName ~ "._cPtr(" ~ param.fullOwnerFlag ~ ".Dup) : null;";
        }
        break;
      case Callback, Unknown, Container, Namespace:
        assert(0, "Unsupported parameter type '" ~ param.fullDType.to!string ~ "' (" ~ param.kind.to!string ~ ") for "
            ~ callback.fullDName.to!string);
    }
  }

  // Process an array parameter
  private void processArrayParam(Param param)
  {
    // Input arrays with Full or Container ownership are freed after their elements are converted (see Func.verify)
    if ((param.direction == ParamDirection.InOut && param.ownership != Ownership.None)
        || (param.direction == ParamDirection.Out && param.ownership != Ownership.Full))
      assert(0, "Unsupported delegate array parameter direction '" ~ param.direction.to!string
        ~ "' and ownership '" ~ param.ownership.to!string ~ "'");

    auto elemType = param.elemTypes[0];

    addDeclParam(param.cType ~ " " ~ param.dName);
    preCall ~= elemType.fullDType ~ "[] _" ~ param.dName ~ ";";
    addCallParam("_" ~ param.dName);

    // Pre delegate call processing
    if (param.direction == ParamDirection.In || param.direction == ParamDirection.InOut)
    {
      dstring lengthStr;

      if (param.lengthParam) // Array has length parameter?
        lengthStr = param.lengthParam.dName;
      else if (param.fixedSize != ArrayNotFixed) // Array is a fixed size?
        lengthStr = param.fixedSize.to!dstring;
      else if (param.zeroTerminated) // Array is zero terminated?
      {
        preCall ~= ["uint _len" ~ param.dName ~ ";", "if (" ~ param.dName ~ ")", "{", "for (; " ~ param.dName
          ~ "[_len" ~ param.dName ~ "] " ~ (elemType.cType.endsWith("*") ? "!is null"d : "!= 0") ~ "; _len"
          ~ param.dName ~ "++)", "{", "}", "}", ""];
        lengthStr = "_len" ~ param.dName;
      }
      else
        assert(0); // This should be prevented by defs.fixupRepos()

      preCall ~= "_" ~ param.dName ~ ".length = " ~ lengthStr ~ ";";

      final switch (elemType.kind) with (TypeKind)
      {
        case Basic, BasicAlias, Enum, Flags, StructAlias, Pointer:
          if (elemType.dType == "bool") // Convert gboolean to bool
            preCall ~= ["foreach (i; 0 .. " ~ lengthStr ~ ")", "_" ~ param.dName ~ "[i] = cast(bool)"
              ~ param.dName ~ "[i];"];
          else
            preCall ~= "_" ~ param.dName ~ "[0 .. " ~ lengthStr ~ "] = " ~ param.dName
              ~ "[0 .. " ~ lengthStr ~ "];";
          break;
        case Struct:
          preCall ~= "_" ~ param.dName ~ "[0 .. " ~ lengthStr ~ "] = (cast(" ~ elemType.fullDType ~ "*)" ~ param.dName
            ~ ")[0 .. " ~ lengthStr ~ "];";
          break;
        case String:
          preCall ~= ["foreach (i; 0 .. " ~ lengthStr ~ ")", "_" ~ param.dName ~ "[i] = "
            ~ param.dName ~ "[i].fromCString!(" ~ param.fullOwnerFlag ~ ".Free);"];
          break;
        case Opaque, Boxed, Wrap, Reffed:
          if (param.isBorrowedArrayArg && param.arrayPtrDepth == 1) // Borrowed inline structures (no owner, not copied)
            preCall ~= ["foreach (i; 0 .. " ~ lengthStr ~ ")", "_" ~ param.dName ~ "[i] = "
              ~ "new " ~ elemType.fullDType ~ "(cast(void*)&" ~ param.dName ~ "[i], null);"];
          else if (param.isBorrowedArrayArg) // Borrowed array of structure pointers (no owner, not copied)
            preCall ~= ["foreach (i; 0 .. " ~ lengthStr ~ ")", "_" ~ param.dName ~ "[i] = " ~ param.dName
              ~ "[i] ? new " ~ elemType.fullDType ~ "(cast(void*)" ~ param.dName ~ "[i], null) : null;"];
          else if (param.arrayPtrDepth == 1) // Inline structures (not pointers), wrapper copies the structure
            preCall ~= ["foreach (i; 0 .. " ~ lengthStr ~ ")", "_" ~ param.dName ~ "[i] = "
              ~ "new " ~ elemType.fullDType ~ "(cast(" ~ elemType.cType.stripConst ~ "*)&" ~ param.dName
              ~ "[i], No.Take);"];
          else // Array of pointers (also pointer typedefs, such as GdkAtom)
            preCall ~= ["foreach (i; 0 .. " ~ lengthStr ~ ")", "_" ~ param.dName ~ "[i] = "
              ~ "new " ~ elemType.fullDType ~ "(cast(void*)" ~ param.dName ~ "[i], " ~ param.fullOwnerFlag ~ ".Take);"];
          break;
        case Object, Interface:
          addImport("gobject.object");
          preCall ~= ["foreach (i; 0 .. " ~ lengthStr ~ ")", "_" ~ param.dName
            ~ "[i] = gobject.object.ObjectWrap._getDObject!(" ~ elemType.fullDType ~ ")(" ~ param.dName ~ "[i], "
            ~ param.fullOwnerFlag ~ ".Take);"];
          break;
        case Unknown, Callback, Container, Namespace:
          assert(0, "Unsupported parameter array type '" ~ elemType.fullDType.to!string ~ "' (" ~ elemType.kind.to!string
              ~ ") for " ~ callback.fullDName.to!string);
      }

      // Free an input array owned by the callee (elements were taken above for Full ownership)
      if (param.direction == ParamDirection.In && param.ownership != Ownership.None)
        preCall ~= "gFree(cast(void*)" ~ param.dName ~ ");";
    }

    if (param.direction == ParamDirection.Out || param.direction == ParamDirection.InOut)
    {
      if (param.lengthParam) // Array has length parameter?
        postCall ~= "*" ~ param.lengthParam.dName ~ " = cast(typeof(*" ~ param.lengthParam.dName ~ "))_" ~ param.dName
          ~ ".length;";

      // The new C array is owned by the caller (Out arrays require Full ownership, see Func.verify)
      postCall ~= "typeof(*" ~ param.dName ~ ") _" ~ param.dName ~ "C;";
      postCall ~= dArrayToC(param, "_" ~ param.dName, "_" ~ param.dName ~ "C");
      postCall ~= "*" ~ param.dName ~ " = _" ~ param.dName ~ "C;";
    }
  }

  // Process a container input parameter (except array)
  private void processContainerInParam(Param param)
  {
    dstring templateParams;

    switch (param.containerType) with(ContainerType)
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
      default:
        assert(0, "Unsupported 'in' container type '" ~ param.containerType.to!string ~ "' for "
          ~ param.fullDName.to!string);
    }

    addDeclParam(param.cType ~ " " ~ param.dName);
    addCallParam("_" ~ param.dName);
    preCall ~= "auto _" ~ param.dName ~ " = g" ~ param.containerType.to!dstring ~ "ToD!(" ~ templateParams ~ ")("
      ~ param.dName ~ ");";
  }

  /**
   * Generate the delegate C callback function code.
   * Params:
   *   tracker = Line tracker to write generated lines to of callback delegate
   */
  void generate(LineTracker tracker)
  {
    tracker ~= [decl, "{"];

    if (preCall.length > 0)
    {
      tracker ~= preCall;
      tracker ~= "";
    }

    tracker ~= ["try", "{", call, "}", "catch (Exception e)", "{", // Handle exceptions from D callback
      `gidInvokeCallbackExceptionHandler(e, "` ~ callback.fullDName ~ `");`, "}"];

    if (postCall.length > 0)
    {
      tracker ~= postCall;
      tracker ~= "";
    }

    if (callback.returnVal && callback.returnVal.origDType != "none")
      tracker ~= "return _retval;";

    tracker ~= ["}", "auto _" ~ delegParam.dName ~ "CB = " ~ delegParam.dName ~ " ? &_" ~ delegParam.dName
      ~ "Callback : null;"];
  }

  Param delegParam; /// The parameter of the callback delegate
  Func callback; /// The resolved callback delegate type of parameter
  dstring decl; /// Function declaration
  dstring call; /// The C function call
  LineTracker preCall; /// Pre-call code for call return variable, call output parameter variables, and input variable processing
  LineTracker postCall; /// Post-call code for return value processing, output parameter processing, and input variable cleanup
}
