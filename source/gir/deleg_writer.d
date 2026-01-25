module gir.deleg_writer;

import code_writer;
import defs;
import gir.func;
import gir.param;
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
      preCall ~= "ptrThawGC(" ~ callback.closureParam.dName ~ ");"; // If it is an asynchronous callback, delegate can be thawed immediately (will continue to exist so long as it is in use)

    processReturn();

    decl ~= "_" ~ delegParam.dName ~ "Callback(";

    if (!staticDelegatePtr)
    {
      preCall ~= "auto _dlg = cast(" ~ delegParam.fullDType ~ "*)" ~ callback.closureParam.dName ~ ";";
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
      call ~= retVal.cType ~ " _retval = ";
      return;
    }

    preCall ~= retVal.fullDType ~ " _dretval;";
    call ~= "_dretval = ";

    final switch (retVal.kind) with (TypeKind)
    {
      case Basic, BasicAlias, Enum, Flags, StructAlias, Struct, Pointer:
        postCall ~= "auto _retval = cast(" ~ retVal.cType ~ ")_dretval;";
        break;
      case String:
        postCall ~= "auto _retval = _dretval.toCString(Yes.Alloc);";
        break;
      case Opaque, Wrap, Boxed, Reffed, Object:
        postCall ~= "auto _retval = cast(" ~ retVal.cTypeRemPtr ~ "*)_dretval._cPtr(" ~ retVal.fullOwnerFlag ~ ".Dup);";
        break;
      case Interface:
        addImport("gobject.object");
        postCall ~= "auto _retval = cast(" ~ retVal.cTypeRemPtr ~ "*)(cast(gobject.object.ObjectWrap)_dretval)._cPtr("
          ~ retVal.fullOwnerFlag ~ ".Dup);";
        break;
      case Callback, Unknown, Container, Namespace:
        assert(0, "Unsupported delegate return value type '" ~ retVal.fullDType.to!string
          ~ "' (" ~ retVal.kind.to!string ~ ") for " ~ callback.fullDName.to!string);
    }
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

    if (retVal.fixedSize != ArrayNotFixed) // Array is a fixed size? Add an array size assertion.
      postCall ~= "assert(!_dretval || _dretval.length == " ~ retVal.fixedSize.to!dstring
        ~ `, "Delegate '` ~ retVal.fullDType ~ `' should return array of size ` ~ retVal.fixedSize.to!dstring
        ~ ` not " ~ _dretval.length.to!string);`;

    postCall ~= ["if (_dretval.length > 0)", "{"];

    if (retVal.zeroTerminated)
    {
      postCall ~= ["_retval = cast(" ~ retVal.cType ~ ")gMalloc((_dretval.length + 1) * (*_retval).sizeof);",
        "zero(cast(void*)&_retval[_dretval.length], (*_retval).sizeof);"];
    }
    else
      postCall ~= "_retval = cast(" ~ retVal.cType ~ ")gMalloc(_dretval.length * (*_retval).sizeof);";

    with (TypeKind) if (!elemType.kind.among(Basic, BasicAlias, Enum, Flags, StructAlias, Struct, Pointer) // Can array be directly copied?
        || elemType.dType == "bool")
    {
      postCall ~= ["", "foreach (i; 0 .. _dretval.length)"];

      final switch (elemType.kind) with (TypeKind)
      {
        case Basic, BasicAlias:
          postCall ~= "_retval[i] = _dretval[i];"; // Convert between bool and gboolean
          break;
        case String:
          postCall ~= "_retval[i] = _dretval[i].toCString(Yes.Alloc);";
          break;
        case Opaque, Wrap, Boxed, Reffed, Object, Interface:
          postCall ~= "_retval[i] = _dretval[i]._cPtr(" ~ retVal.fullOwnerFlag ~ ".Dup);";
          break;
        case Enum, Flags, StructAlias, Struct, Pointer, Callback, Unknown, Container, Namespace:
          assert(0, "Unsupported delegate return value array type '" ~ elemType.fullDType.to!string
            ~ "' (" ~ elemType.kind.to!string ~ ") for " ~ callback.fullDName.to!string);
      }
    }
    else // Data is compatible between D and C types, do array copy
      postCall ~= "_retval[0 .. _dretval.length] = _dretval[];";

    postCall ~= ["}", ""]; // Close of "if (_dretval.length > 0)" statement

    if (retVal.lengthParam) // Array has length parameter?
      postCall ~= "*" ~ retVal.lengthParam.dName ~ " = cast(typeof(" ~ retVal.lengthParam.dName ~ "))_dretval.length;";
  }

  /// Process a return container (not Array)
  private void processReturnContainer()
  {
    auto retVal = callback.returnVal;
    dstring templateParams;

    switch (retVal.containerType) with(ContainerType)
    {
      case ByteArray:
        break;
      case ArrayG, PtrArray:
        templateParams = "!(" ~ retVal.elemTypes[0].fullDType  ~ ", " ~ retVal.zeroTerminated.to!dstring ~ ")";
        break;
      case List, SList:
        templateParams = "!(" ~ retVal.elemTypes[0].fullDType ~ ")";
        break;
      case HashTable:
        templateParams = "!(" ~ retVal.elemTypes[0].fullDType ~ ", " ~ retVal.elemTypes[1].fullDType ~ ")";
        break;
      default:
        assert(0, "Unsupported delegate return container type '" ~ retVal.containerType.to!string ~ "' for "
          ~ retVal.fullDName.to!string);
    }

    decl ~= retVal.cType ~ " ";
    call ~= "auto _dretval = ";
    postCall ~= "auto _retval = g" ~ retVal.containerType.to!dstring ~ "FromD" ~ templateParams ~ "(_dretval);";
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
      else
          assert(0, "Delegate container " ~ param.directionStr.to!string ~ " parameters of type '"
            ~ param.kind.to!string ~ "' not supported");

      return;
    }

    addDeclParam(param.cType ~ " " ~ param.dName);

    if (param is callback.closureParam) // Closure parameter?
      return;

    final switch (param.kind) with (TypeKind)
    {
      case Basic, BasicAlias, Enum, Flags, Pointer:
        if (param.dType == "bool")
        { // Bool parameters need to be converted between bool and gboolean
          if (param.direction != ParamDirection.In)
          {
            preCall ~= "bool _" ~ param.dName
              ~ (param.direction == ParamDirection.InOut ? " = cast(bool)" ~ param.dName ~ ";" : ";");
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
          preCall ~= "string _" ~ param.dName ~ " = " ~ param.dName ~ ".fromCString(" ~ param.fullOwnerFlag ~ ".Free);";
          addCallParam("_" ~ param.dName);
        }
        else if (param.direction == ParamDirection.Out)
        {
          preCall ~= "string _" ~ param.dName ~ ";";
          addCallParam("_" ~ param.dName);
          postCall ~= "*" ~ param.dName ~ " = _" ~ param.dName ~ ".toCString(" ~ param.fullOwnerFlag ~ ".Alloc);";
        }
        else // InOut
          assert(0, "InOut string arguments not supported"); // FIXME - Does this even exist?
        break;
      case StructAlias, Struct:
        addCallParam("*cast(" ~ param.fullDType ~ "*)" ~ param.dName);
        break;
      case Opaque, Wrap, Boxed, Reffed, Object, Interface:
        if (param.direction == ParamDirection.In)
        {
          if (param.kind == TypeKind.Object || param.kind == TypeKind.Interface)
          {
            addImport("gobject.object");
            addCallParam("gobject.object.ObjectWrap._getDObject!(" ~ param.fullDType ~ ")(cast(void*)" ~ param.dName ~ ", "
              ~ param.fullOwnerFlag ~ ".Take)");
          }
          else
            addCallParam(param.dName ~ " ? new " ~ param.fullDType ~ "(cast(void*)" ~ param.dName ~ ", "
              ~ param.fullOwnerFlag ~ ".Take) : null");
        }
        else if (param.direction == ParamDirection.Out)
        { // FIXME - Not sure if this will work for all cases, also could optimize by allowing C structure to be directly used in D object
          preCall ~= "auto _" ~ param.dName ~ " = new " ~ param.fullDType ~ "(" ~ param.dName ~ ", No.Take);";
          addCallParam("_" ~ param.dName);
          postCall ~= "*" ~ param.dName ~ " = *cast(" ~ param.cType ~ ")_" ~ param.dName ~ "._cPtr;";
        }
        else // InOut
          assert(0, "InOut arguments of type '" ~ param.kind.to!string ~ "' not supported"); // FIXME - Does this even exist?
        break;
      case Callback, Unknown, Container, Namespace:
        assert(0, "Unsupported parameter type '" ~ param.fullDType.to!string ~ "' (" ~ param.kind.to!string ~ ") for "
            ~ callback.fullDName.to!string);
    }
  }

  // Process an array parameter
  private void processArrayParam(Param param)
  {
    if ((param.direction != ParamDirection.Out && param.ownership != Ownership.None)
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
        preCall ~= ["uint _len" ~ param.dName ~ ";", "if (" ~ param.dName ~ ")", "for (; " ~ param.dName
          ~ "[_len" ~ param.dName ~ "] " ~ (elemType.cType.endsWith("*") ? "!is null"d : "!= 0") ~ "; _len"
          ~ param.dName ~ "++)", "break;"];
        lengthStr = "_len" ~ param.dName;
      }
      else
        assert(0); // This should be prevented by defs.fixupRepos()

      preCall ~= "_" ~ param.dName ~ ".length = " ~ lengthStr ~ ";";

      final switch (elemType.kind) with (TypeKind)
      {
        case Basic, BasicAlias, Enum, Flags, StructAlias, Pointer:
          if (param.dType == "bool") // Convert gboolean to bool
            preCall ~= ["foreach (i; 0 .. " ~ lengthStr ~ ")", "_" ~ param.dName ~ "[i] = cast(bool)"
              ~ param.dName ~ "[i];"];
          else
            preCall ~= "_" ~ param.dName ~ "[0 .. " ~ lengthStr ~ "] = " ~ param.dName
              ~ "[0 .. " ~ lengthStr ~ "];";
          break;
        case Struct:
          preCall ~= "_" ~ param.dName ~ "[0 .. " ~ lengthStr ~ "] = cast(" ~ param.fullDType ~ "*)" ~ param.dName
            ~ "[0 .. " ~ lengthStr ~ "];";
          break;
        case String:
          preCall ~= ["foreach (i; 0 .. " ~ lengthStr ~ ")", "_" ~ param.dName ~ "[i] = "
            ~ param.dName ~ "[i].fromCString(" ~ param.fullOwnerFlag ~ ".Free);"];
          break;
        case Opaque, Boxed, Wrap, Reffed:
          preCall ~= ["foreach (i; 0 .. " ~ lengthStr ~ ")", "_" ~ param.dName ~ "[i] = "
            ~ "new " ~ elemType.fullDType ~ "(cast(" ~ elemType.cType.stripConst ~ "*)&" ~ param.dName ~ "[i], "
            ~ param.fullOwnerFlag ~ ".Take);"];
          break;
        case Object, Interface:
          addImport("gobject.object");
          preCall ~= ["foreach (i; 0 .. " ~ lengthStr ~ ")", "_" ~ param.dName
            ~ "[i] = gobject.object.ObjectWrap._getDObject(" ~ param.dName ~ "[i], " ~ param.fullOwnerFlag ~ ".Take);"];
          break;
        case Unknown, Callback, Container, Namespace:
          assert(0, "Unsupported parameter array type '" ~ elemType.fullDType.to!string ~ "' (" ~ elemType.kind.to!string
              ~ ") for " ~ callback.fullDName.to!string);
      }
    }

    if (param.direction == ParamDirection.Out || param.direction == ParamDirection.InOut)
    {
      if (param.lengthParam) // Array has length parameter?
        postCall ~= param.lengthParam.dName ~ " = cast(" ~ param.lengthParam.cType  ~ ")_" ~ param.dName ~ ".length"
          ~ (param.zeroTerminated ? " - 1;"d : ";"d);

      final switch (elemType.kind) with (TypeKind)
      {
        case Basic, String, BasicAlias, Enum, Flags, StructAlias, Struct, Pointer, Opaque, Wrap, Boxed, Reffed,
            Object, Interface:
          postCall ~= param.dName ~ " = arrayDtoC!(" ~ elemType.fullDType ~ ", Yes.Alloc, "
            ~ (param.zeroTerminated ? "Yes"d : "No"d) ~ ".ZeroTerm)(_" ~ param.dName ~ ");";
          break;
        case Unknown, Callback, Container, Namespace:
          assert(0, "Unsupported parameter array type '" ~ elemType.fullDType.to!string ~ "' (" ~ elemType.kind.to!string
            ~ ") for " ~ callback.fullDName.to!string);
      }
    }
  }

  // Process a container input parameter (except array)
  private void processContainerInParam(Param param)
  {
    dstring templateParams;

    switch (param.containerType) with(ContainerType)
    {
      case ByteArray:
        templateParams = param.ownership.to!dstring;
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

    tracker ~= call;

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
