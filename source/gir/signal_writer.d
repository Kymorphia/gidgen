module gir.signal_writer;

import code_writer;
import defs;
import gir.func;
import gir.param;
import gir.structure;
import gir.type_node;
import std_includes;
import utils;

/// Signal writer class
class SignalWriter
{
  this(Func signal)
  {
    this.signal = signal;

    owningClass = cast(Structure)signal.parent;
    assert(owningClass && owningClass != signal.repo.globalStruct,
      "Signal '" ~ signal.fullName.to!string ~ "' does not have a valid class");

    signal.repo.resolveSymbol("GObject.DClosure");

    process();
  }

  // Process the signal
  private void process()
  {
    auto baseName = signal.dName(true) ~ "Callback";
    connectDecl = "ulong connect" ~ signal.dName(true) ~ "(T)(" ~ (signal.detailed ? "string detail = null, "d : "")
      ~ "T callback, " ~  "Flag!\"After\" after = No.After)\nif (is(T : " ~ baseName ~ "Dlg) || is(T : " ~ baseName
      ~ "Func))";

    preCall ~= "extern(C) void _cmarshal(GClosure* _closure, GValue* _returnValue, uint _nParams,"
      ~ " const(GValue)* _paramVals, void* _invocHint, void* _marshalData)\n{\n";
    preCall ~= "assert(_nParams == " ~ (signal.params.length + 1).to!dstring
      ~ ", \"Unexpected number of signal parameters\");\n";
    preCall ~= "auto _dClosure = cast(DGClosure!T*)_closure;\n";

    processReturn();

    auto instanceParamName = signal.repo.defs.symbolName(owningClass.dType[0].toLower ~ owningClass.dType[1 .. $]);
    preCall ~= "auto " ~ instanceParamName ~ " = getVal!" ~ owningClass.dType ~ "(_paramVals);\n"; // Instance parameter is the first value
    aliasDecl ~= " delegate("; // Replaced in write() for function alias
    call ~= "_dClosure.dlg(";

    foreach (i, param; signal.params)
      processParam(param, i);

    // Add the instance parameter (last)
    addDeclParam(owningClass.dType ~ " " ~ instanceParamName);
    addCallParam(instanceParamName);

    aliasDecl ~= ");";
    call ~= ");";
  }

  // Helper to add parameter to call string with comma separator
  private void addCallParam(dstring paramStr)
  {
    if (!call.endsWith('('))
      call ~= ", ";

    call ~= paramStr;
  }

  // Helper to add parameter to aliasDecl string with comma separator
  private void addDeclParam(dstring paramStr)
  {
    if (!aliasDecl.endsWith('('))
      aliasDecl ~= ", ";

    aliasDecl ~= paramStr;
  }

  /// Process return value
  private void processReturn()
  {
    auto retVal = signal.returnVal;

    if (!retVal || retVal.origDType == "none")
    {
      aliasDecl ~= "void";
      return;
    }

    assert(retVal.containerType == ContainerType.None, "No support for signal container return type '"
      ~ retVal.containerType.to!string ~ "'");

    final switch (retVal.kind) with (TypeKind)
    {
      case Basic, BasicAlias:
        aliasDecl ~= retVal.dType;
        preCall ~= retVal.dType ~ " _retval;\n";
        call ~= "_retval = ";
        break;
      case String:
        aliasDecl ~= "string";
        call ~= "auto _retval = ";
        break;
      case Enum, Flags:
        aliasDecl ~= retVal.dType;
        call ~= "auto _dretval = ";
        postCall ~= retVal.cType ~ " _retval = cast(" ~ retVal.cType ~ ")_dretval;\n";
        break;
      case Boxed:
        aliasDecl ~= retVal.dType;
        call ~= "auto _retval = ";
        break;
      case Wrap, Reffed, Object, Interface:
        aliasDecl ~= retVal.dType;
        call ~= "auto _retval = ";
        break;
      case Simple, Pointer, Callback, Opaque, Unknown, Container, Namespace:
        assert(0, "Unsupported signal return value type '" ~ retVal.dType.to!string ~ "' (" ~ retVal.kind.to!string ~ ") for "
            ~ signal.fullName.to!string);
    }

    postCall ~= "setVal!" ~ retVal.dType ~ "(_returnValue, _retval);\n";
  }

  /// Process parameter
  private void processParam(Param param, ulong paramIndex)
  {
    if (param.containerType == ContainerType.Array) // Array container?
    {
      processArrayParam(param, paramIndex);
      return;
    }

    preCall ~= "auto " ~ param.dName ~ " = getVal!" ~ param.dType ~ "(&_paramVals[" ~ (paramIndex + 1).to!dstring
      ~ "]);\n"; // The parameter index is +1 because the first one is the object instance

    assert(param.containerType == ContainerType.None, "No support for signal container parameter type '"
      ~ param.containerType.to!string ~ "'");

    assert(param.direction == ParamDirection.In, "No support for signal parameter direction '"
      ~ param.direction.to!string ~ "'");

    if (param.isArrayLength) // Array length parameter?
      return;

    addCallParam(param.dName);

    final switch (param.kind) with (TypeKind)
    {
      case Basic, BasicAlias, Enum, Flags, Simple, Pointer, Opaque:
        addDeclParam(param.directionStr ~ param.dType ~ " " ~ param.dName);
        break;
      case String:
        addDeclParam(param.directionStr ~ "string " ~ param.dName);
        break;
      case Wrap, Boxed, Reffed, Object, Interface:
        addDeclParam(param.dType ~ " " ~ param.dName);
        break;
      case Callback, Unknown, Container, Namespace:
        assert(0, "Unsupported signal parameter type '" ~ param.dType.to!string ~ "' (" ~ param.kind.to!string ~ ") for "
            ~ signal.fullName.to!string);
    }
  }

  /// Process array parameter
  private void processArrayParam(Param param, ulong paramIndex)
  {
    if (param.direction != ParamDirection.In || param.ownership != Ownership.None)
      assert(0, "Unsupported delegate array parameter direction '" ~ param.direction.to!string
        ~ "' and ownership '" ~ param.ownership.to!string ~ "'");

    auto elemType = param.elemTypes[0];

    preCall ~= "auto " ~ param.dName ~ " = getVal!(" ~ elemType.cTypeRemPtr ~ "**)(&_paramVals[" ~ (paramIndex + 1).to!dstring ~ "]);\n"; // The parameter index is +1 because the first one is the object instance

    addDeclParam(elemType.dType ~ "[] " ~ param.dName);
    preCall ~= elemType.dType ~ "[] _" ~ param.dName ~ ";\n";
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
        inpProcess ~= "uint _len" ~ param.dName ~ ";\nif (" ~ param.dName ~ ")\nfor (; " ~ param.dName
          ~ "[_len" ~ param.dName ~ "] " ~ (elemType.cType.endsWith("*") ? "!is null"d : "!= 0") ~ "; _len" ~ param.dName
          ~ "++)\nbreak;\n";
        lengthStr = "_len" ~ param.dName;
      }
      else
        assert(0); // This should be prevented by verify

      final switch (elemType.kind) with (TypeKind)
      {
        case Basic, BasicAlias, Enum, Flags, Simple, Pointer:
          inpProcess ~= "_" ~ param.dName ~ " = cast(" ~ elemType.dType ~ "[])" ~ param.dName ~ "[0 .. " ~ lengthStr
            ~ "];\n";
          break;
        case String:
          inpProcess ~= "foreach (i; 0 .. " ~ lengthStr ~ ")\n_" ~ param.dName ~ " ~= " ~ param.dName ~ "[i].fromCString("
            ~ param.fullOwnerFlag ~ ".Free);\n";
          break;
        case Opaque, Boxed, Wrap, Reffed:
          inpProcess ~= "foreach (i; 0 .. " ~ lengthStr ~ ")\n_" ~ param.dName ~ " ~= new " ~ elemType.dType ~ "(cast("
            ~ elemType.cType.stripConst ~ "*)&" ~ param.dName ~ "[i]"
            ~ (param.kind != Wrap ? (", " ~ param.fullOwnerFlag ~ ".Take") : "") ~ ");\n";
          break;
        case Object, Interface:
          auto objectGSym = param.repo.resolveSymbol("GObject.ObjectG");
          inpProcess ~= "foreach (i; 0 .. " ~ lengthStr ~ ")\n_" ~ param.dName ~ " ~= " ~ objectGSym ~ ".getDObject!"
            ~ elemType.dType ~ "(" ~ param.dName ~ "[i], " ~ param.fullOwnerFlag ~ ".Take);\n";
          break;
        case Unknown, Callback, Container, Namespace:
          assert(0, "Unsupported parameter array type '" ~ elemType.dType.to!string ~ "' (" ~ elemType.kind.to!string
              ~ ") for signal " ~ signal.fullName.to!string);
      }
    }
  }

  /**
   * Write signal binding to a CodeWriter.
   * Params:
   *   writer = Code writer to write to.
   *   moduleType = Module file type being written (defaults to ModuleType.Normal)
   */
  void write(CodeWriter writer, ModuleType moduleType = ModuleType.Normal)
  {
    signal.writeDocs(writer);

    auto baseName = signal.dName(true) ~ "Callback";

    // Define a delegate and function alias
    writer ~= ["alias " ~ baseName ~ "Dlg = " ~ aliasDecl, "alias " ~ baseName ~ "Func = "
      ~ aliasDecl.replaceFirst("delegate", "function"), ""];

    writer ~= ["/**", "* Connect to " ~ signal.dName(true) ~ " signal.", "* Params:"];

    if (signal.detailed)
      writer ~= "*   detail = Signal detail or null (default)";

    writer ~= ["*   callback = signal callback delegate or function to connect",
    "*   after = Yes.After to execute callback after default handler, No.After to execute before (default)",
      "* Returns: Signal ID", "*/"];

    if (moduleType == ModuleType.Iface)
    {
      writer ~= connectDecl ~ ";";
      return;
    }

    writer ~= connectDecl;
    writer ~= "{";

    if (!preCall.empty)
      writer ~= preCall;

    if (!inpProcess.empty)
      writer ~= inpProcess;

    writer ~= call;

    if (postCall.length > 0)
      writer ~= postCall;

    writer ~= ["}", "", "auto closure = new DClosure(callback, &_cmarshal);"];
    writer ~= ["return connectSignalClosure(\"" ~ signal.name ~ "\""
      ~ (signal.detailed ? `~ (detail.length ? "::" ~ detail : "")`d : "") ~ ", closure, after);", "}"];
  }

  Func signal; /// The signal object being written
  Structure owningClass; /// The class which owns the signal (parent)
  dstring aliasDecl; /// Delegate/function alias declarations
  dstring connectDecl; /// Signal connect method declaration
  dstring preCall; /// Pre-call code for call return variable, call output parameter variables, and input variable processing
  dstring inpProcess; /// Input processing (after preCall variable assignments and before the delegate call)
  dstring call; /// The D delegate call
  dstring postCall; /// Post-call code for return value processing, output parameter processing, and input variable cleanup
}
