module gir.deleg_writer;

import code_writer;
import defs;
import gir.func;
import gir.param;
import gir.structure;
import gir.type_node;
import std_includes;
import utils;

/// Delegate C callback marshal writer class
class DelegWriter
{
  this(Param delegParam, bool staticDelegatePtr)
  {
    this.delegParam = delegParam;
    callback = cast(Func)delegParam.typeObjectRoot;

    assert(this.callback, "DelegWriter parameter " ~ delegParam.fullName.to!string ~ " has "
      ~ delegParam.typeObjectRoot.to!string ~ " typeObjectRoot");

    process(staticDelegatePtr);
  }

  // Process the delegate parameter
  private void process(bool staticDelegatePtr)
  {
    codeTrap("deleg.write", delegParam.fullDName);

    decl ~= "extern(C) ";

    if (delegParam.scope_ == ParamScope.Async)
      preCall ~= "ptrThawGC(" ~ callback.closureParam.dName ~ ");\n"; // If it is an asynchronous callback, delegate can be thawed immediately (will continue to exist so long as it is in use)

    processReturn();

    decl ~= "_" ~ delegParam.dName ~ "Callback(";

    if (!staticDelegatePtr)
    {
      preCall ~= "auto _dlg = cast(" ~ delegParam.fullDType ~ "*)" ~ callback.closureParam.dName ~ ";\n";
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

    final switch (retVal.kind) with (TypeKind)
    {
      case Basic, BasicAlias, Pointer:
        call ~= retVal.cType ~ " _retval = ";
        break;
      case String:
        preCall ~= retVal.fullDType ~ " _dretval;\n";
        call ~= "_dretval = ";
        postCall ~= retVal.cType ~ " _retval = _dretval.toCString("d ~ retVal.fullOwnerFlag ~ ".Alloc);\n";
        break;
      case Enum, Flags:
        preCall ~= retVal.fullDType ~ " _dretval;\n";
        call ~= "_dretval = ";
        postCall ~= "auto _retval = cast(" ~ retVal.cType ~ ")_dretval;\n";
        break;
      case Simple:
        preCall ~= retVal.fullDType ~ " _dretval;\n";
        call ~= "_dretval = ";
        postCall ~= retVal.cType ~ " _retval;\nif (_dretval)\n_retval = *_dretval;\n";
        break;
      case Opaque, Wrap, Boxed, Reffed, Object:
        preCall ~= retVal.fullDType ~ " _dretval;\n";
        call ~= "_dretval = ";
        postCall ~= retVal.cType ~ " _retval = cast(" ~ retVal.cTypeRemPtr ~ "*)_dretval.cPtr("
          ~ retVal.fullOwnerFlag ~ ".Dup);\n";
        break;
      case Interface:
        auto objectGSym = retVal.repo.resolveSymbol("GObject.ObjectG");
        preCall ~= retVal.fullDType ~ " _dretval;\n";
        call ~= "_dretval = ";
        postCall ~= retVal.cType ~ " _retval = cast(" ~ retVal.cTypeRemPtr ~ "*)(cast(" ~ objectGSym ~ ")_dretval).cPtr("
          ~ retVal.fullOwnerFlag ~ ".Dup);\n";
        break;
      case Callback, Unknown, Container, Namespace:
        assert(0, "Unsupported delegate return value type '" ~ retVal.fullDType.to!string
          ~ "' (" ~ retVal.kind.to!string ~ ") for " ~ callback.fullName.to!string);
    }
  }

  /// Process array return value
  private void processReturnArray()
  {
    auto retVal = callback.returnVal;

    assert(retVal.ownership == Ownership.Full, "Unsupported delegate return array ownership '"
      ~ retVal.ownership.to!string ~ "' for " ~ callback.fullName.to!string);

    auto elemType = retVal.elemTypes[0];

    decl ~= retVal.cType ~ " ";
    preCall ~= elemType.fullDType ~ "[] _dretval;\n";
    call ~= "_dretval = ";
    postCall ~= retVal.cType ~ " _retval;\n\nif (_dretval.length > 0)\n{\n";

    if (retVal.fixedSize != ArrayNotFixed) // Array is a fixed size? Add an array size assertion.
      postCall ~= "assert(!_dretval || _dretval.length == " ~ retVal.fixedSize.to!dstring
        ~ `, "Delegate '` ~ retVal.fullDType ~ `' should return array of size ` ~ retVal.fixedSize.to!dstring
        ~ ` not " ~ _dretval.length.to!string);\n`;

    if (retVal.zeroTerminated)
    {
      postCall ~= "_retval = cast(" ~ retVal.cType ~ ")gMalloc((_dretval.length + 1) * (*_retval).sizeof);\n";
      postCall ~= "zero(cast(void*)&_retval[_dretval.length], (*_retval).sizeof);";
    }
    else
      postCall ~= "_retval = cast(" ~ retVal.cType ~ ")gMalloc(_dretval.length * (*_retval).sizeof);\n";

    if (elemType.kind.among(TypeKind.Basic, TypeKind.BasicAlias, TypeKind.Enum))
      postCall ~= "_retval[0 .. _dretval.length] = _dretval[0 .. _dretval.length];\n";
    else
    {
      postCall ~= "\nforeach (i; 0 .. _dretval.length)\n";

      final switch (elemType.kind) with (TypeKind)
      {
        case String:
          postCall ~= "_retval[i] = _dretval[i].toCString(Yes.Alloc);\n";
          break;
        case Enum, Flags:
          postCall ~= "_retval[i] = cast(" ~ elemType.cType ~ ")_dretval[i];\n";
          break;
        case Simple, Pointer:
          postCall ~= "_retval[i] = _dretval[i];\n";
          break;
        case Opaque, Wrap, Boxed, Reffed, Object, Interface:
          postCall ~= "_retval[i] = _dretval[i].cPtr(" ~ retVal.fullOwnerFlag ~ ".Dup);\n";
          break;
        case Basic, BasicAlias, Callback, Unknown, Container, Namespace:
          assert(0, "Unsupported delegate return value array type '" ~ elemType.fullDType.to!string
            ~ "' (" ~ elemType.kind.to!string ~ ") for " ~ callback.fullName.to!string);
      }
    }

    postCall ~= "}\n\n";

    if (retVal.lengthParam) // Array has length parameter?
      postCall ~= "*" ~ retVal.lengthParam.dName ~ " = cast(typeof(" ~ retVal.lengthParam.dName ~ "))_dretval.length;\n";
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
          ~ retVal.fullName.to!string);
    }

    decl ~= retVal.cType ~ " ";
    call ~= "auto _dretval = ";
    postCall = "auto _retval = g" ~ retVal.containerType.to!dstring ~ "FromD" ~ templateParams ~ "(_dretval);\n";
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
        addCallParam(param.direction == ParamDirection.In ? param.dName : "*" ~ param.dName);
        break;
      case String:
        if (param.direction == ParamDirection.In)
        {
          preCall ~= "string _" ~ param.dName ~ " = " ~ param.dName ~ ".fromCString(" ~ param.fullOwnerFlag ~ ".Free);\n";
          addCallParam("_" ~ param.dName);
        }
        else if (param.direction == ParamDirection.Out)
        {
          preCall ~= "string _" ~ param.dName ~ ";\n";
          addCallParam("_" ~ param.dName);
          postCall ~= "*" ~ param.dName ~ " = _" ~ param.dName ~ ".toCString(" ~ param.fullOwnerFlag ~ ".Alloc);\n";
        }
        else // InOut
          assert(0, "InOut string arguments not supported"); // FIXME - Does this even exist?
        break;
      case Simple:
        addCallParam("*"d ~ param.dName);
        break;
      case Opaque, Wrap, Boxed, Reffed, Object, Interface:
        if (param.direction == ParamDirection.In)
        {
          if (param.kind == TypeKind.Object || param.kind == TypeKind.Interface)
          {
            auto objectGSym = param.repo.resolveSymbol("GObject.ObjectG");
            addCallParam(objectGSym ~ ".getDObject!(" ~ param.fullDType ~ ")(cast(void*)" ~ param.dName ~ ", "
              ~ param.fullOwnerFlag ~ ".Take)");
          }
          else
            addCallParam(param.dName ~ " ? new " ~ param.fullDType ~ "(cast(void*)" ~ param.dName ~ ", "
              ~ param.fullOwnerFlag ~ ".Take) : null");
        }
        else if (param.direction == ParamDirection.Out)
        { // FIXME - Not sure if this will work for all cases, also could optimize by allowing C structure to be directly used in D object
          preCall ~= "auto _" ~ param.dName ~ " = new " ~ param.fullDType ~ "(" ~ param.dName ~ ", No.Take);\n";
          addCallParam("_" ~ param.dName);
          postCall ~= "*" ~ param.dName ~ " = *cast(" ~ param.cType ~ ")_" ~ param.dName ~ ".cPtr;\n";
        }
        else // InOut
          assert(0, "InOut arguments of type '" ~ param.kind.to!string ~ "' not supported"); // FIXME - Does this even exist?
        break;
      case Callback, Unknown, Container, Namespace:
        assert(0, "Unsupported parameter type '" ~ param.fullDType.to!string ~ "' (" ~ param.kind.to!string ~ ") for "
            ~ callback.fullName.to!string);
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
    preCall ~= elemType.fullDType ~ "[] _" ~ param.dName ~ ";\n";
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
        preCall ~= "uint _len" ~ param.dName ~ ";\nif (" ~ param.dName ~ ")\nfor (; " ~ param.dName
          ~ "[_len" ~ param.dName ~ "] " ~ (elemType.cType.endsWith("*") ? "!is null"d : "!= 0") ~ "; _len" ~ param.dName
          ~ "++)\nbreak;\n";
        lengthStr = "_len" ~ param.dName;
      }
      else
        assert(0); // This should be prevented by defs.fixupRepos()

      preCall ~= "_" ~ param.dName ~ ".length = " ~ lengthStr ~ ";\n";

      final switch (elemType.kind) with (TypeKind)
      {
        case Basic, BasicAlias, Enum, Flags, Simple, Pointer:
          preCall ~= "_" ~ param.dName ~ "[0 .. " ~ lengthStr ~ "] = " ~ param.dName
            ~ "[0 .. " ~ lengthStr ~ "];\n";
          break;
        case String:
          preCall ~= "foreach (i; 0 .. " ~ lengthStr ~ ")\n_" ~ param.dName ~ "[i] = "
            ~ param.dName ~ "[i].fromCString(" ~ param.fullOwnerFlag ~ ".Free);\n";
          break;
        case Opaque, Boxed, Wrap, Reffed:
          preCall ~= "foreach (i; 0 .. " ~ lengthStr ~ ")\n_" ~ param.dName ~ "[i] = "
            ~ "new " ~ elemType.fullDType ~ "(cast(" ~ elemType.cType.stripConst ~ "*)&" ~ param.dName ~ "[i]"
            ~ (param.kind != Wrap ? (", " ~ param.fullOwnerFlag ~ ".Take") : "") ~ ");\n";
          break;
        case Object, Interface:
          auto objectGSym = param.repo.resolveSymbol("GObject.ObjectG");
          preCall ~= "foreach (i; 0 .. " ~ lengthStr ~ ")\n_" ~ param.dName ~ "[i] = "
            ~ objectGSym ~ ".getDObject(" ~ param.dName ~ "[i], " ~ param.fullOwnerFlag ~ ".Take);\n";
          break;
        case Unknown, Callback, Container, Namespace:
          assert(0, "Unsupported parameter array type '" ~ elemType.fullDType.to!string ~ "' (" ~ elemType.kind.to!string
              ~ ") for " ~ callback.fullName.to!string);
      }
    }

    if (param.direction == ParamDirection.Out || param.direction == ParamDirection.InOut)
    {
      if (param.lengthParam) // Array has length parameter?
        postCall ~= param.lengthParam.dName ~ " = cast(" ~ param.lengthParam.cType  ~ ")_" ~ param.dName ~ ".length"
          ~ (param.zeroTerminated ? " - 1;\n"d : ";\n"d);

      final switch (elemType.kind) with (TypeKind)
      {
        case Basic, String, BasicAlias, Enum, Flags, Simple, Pointer, Opaque, Wrap, Boxed, Reffed, Object, Interface:
          postCall ~= param.dName ~ " = arrayDtoC!(" ~ elemType.fullDType ~ ", Yes.Alloc, "
            ~ (param.zeroTerminated ? "Yes"d : "No"d) ~ ".ZeroTerm)(_" ~ param.dName ~ ");\n";
          break;
        case Unknown, Callback, Container, Namespace:
          assert(0, "Unsupported parameter array type '" ~ elemType.fullDType.to!string ~ "' (" ~ elemType.kind.to!string
            ~ ") for " ~ callback.fullName.to!string);
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
        templateParams = param.elemTypes[0].dType ~ ", " ~ param.elemTypes[1].dType ~ ", "
          ~ "GidOwnership." ~ param.ownership.to!dstring;
        break;
      default:
        assert(0, "Unsupported 'in' container type '" ~ param.containerType.to!string ~ "' for "
          ~ param.fullName.to!string);
    }

    addDeclParam(param.cType ~ " " ~ param.dName);
    addCallParam("_" ~ param.dName);
    preCall ~= "auto _" ~ param.dName ~ " = g" ~ param.containerType.to!dstring ~ "ToD!(" ~ templateParams ~ ")("
      ~ param.dName ~ ");\n";
  }

  /**
   * Generate the delegate C callback function code.
   * Returns: The generated D code for the C callback delegate function marshaller
   */
  dstring generate()
  {
    dstring output = decl ~ "\n{\n";

    if (preCall.length > 0)
      output ~= preCall ~ "\n";

    output ~= call ~ "\n";

    if (postCall.length > 0)
      output ~= postCall ~ "\n";

    if (callback.returnVal && callback.returnVal.origDType != "none")
      output ~= "return _retval;\n";

    output ~= "}\n";
    output ~= "auto _" ~ delegParam.dName ~ "CB = " ~ delegParam.dName ~ " ? &_" ~ delegParam.dName
      ~ "Callback : null;\n";

    return output;
  }

  Param delegParam; /// The parameter of the callback delegate
  Func callback; /// The resolved callback delegate type of parameter
  dstring decl; /// Function declaration
  dstring preCall; /// Pre-call code for call return variable, call output parameter variables, and input variable processing
  dstring call; /// The C function call
  dstring postCall; /// Post-call code for return value processing, output parameter processing, and input variable cleanup
}
