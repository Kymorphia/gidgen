module gir.signal_writer;

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

/// Signal writer class
class SignalWriter
{
  this(Func signal)
  {
    this.signal = signal;

    owningClass = cast(Structure)signal.parent;
    assert(owningClass && owningClass != signal.repo.globalStruct,
      "Signal '" ~ signal.fullDName.to!string ~ "' does not have a valid class");

    addImport("gobject.dclosure");

    preCall = new LineTracker;
    inpProcess = new LineTracker;
    postCall = new LineTracker;
    process();
  }

  // Process the signal
  private void process()
  {
    codeTrap("struct.signal", signal.fullDName);

    processReturn();

    call ~= "_dClosure.cb(_paramTuple[]);";

    foreach (i, param; signal.params)
      processParam(param, i);

    // Add instance parameter
    auto cbIndex = cast(int)callbackTypes.length - 1;
    preCall ~= ["", "static if (Parameters!T.length > " ~ cbIndex.to!dstring ~ ")", // Only process parameters which are included in the callback
      "_paramTuple[" ~ cbIndex.to!dstring ~ "] = getVal!(Parameters!T[" ~ cbIndex.to!dstring
      ~ "])(&_paramVals[0]);"]; // The instance type is the first value in the parameters passed to the C marshal, we make it last though

    callbackTypes ~= CallbackType(owningClass.fullDType, true); // Add the instance parameter type (last)
    callbackProto ~= (callbackProto[$ - 1] != '(' ? ", "d : "") ~ owningClass.fullDType ~ " "
      ~ signal.signalDelegInstanceParam;
    callbackProto ~= ")";
  }

  /// Process return value
  private void processReturn()
  {
    auto retVal = signal.returnVal;

    callbackProto ~= retVal.fullDType ~ " callback(";

    if (!retVal || retVal.origDType == "none")
    {
      callbackTypes ~= CallbackType("void");
      return;
    }

    assert(retVal.containerType == ContainerType.None, "No support for signal container return type '"
      ~ retVal.containerType.to!string ~ "'");

    with (TypeKind) assert(!retVal.kind.among(StructAlias, Struct, Pointer, Callback, Opaque, Unknown,
        Container, Namespace),
      "Unsupported signal return value type '" ~ retVal.fullDType.to!string ~ "' (" ~ retVal.kind.to!string ~ ") for "
      ~ signal.fullDName.to!string);

    with (TypeKind) callbackTypes ~= CallbackType(retVal.fullDType, retVal.kind.among(Object, Interface) != 0);
    call ~= "auto _retval = ";
    postCall ~= ["", "setVal!(" ~ retVal.fullDType ~ ")(_returnValue, _retval);"];
  }

  /// Process parameter
  private void processParam(Param param, ulong paramIndex)
  { // Array container and not string[] (handled below)?
    if (param.containerType == ContainerType.Array)
    { // Arrays of strings are handled below
      if (param.elemTypes[0].dType != "string"d)
      {
        processArrayParam(param, paramIndex);
        return;
      }
    }
    else if (param.containerType != ContainerType.None) // Other container type?
    {
      processContainerParam(param, paramIndex);
      return;
    }

    if (param.isArrayLength) // Array length parameter?
    {
      preCall ~= "auto " ~ param.dName ~ " = getVal!(" ~ param.fullDType ~ ")(&_paramVals["
        ~ (paramIndex + 1).to!dstring ~ "]);"; // The parameter index is +1 because the first one is the object instance
      return;
    }

    with (TypeKind) assert(!param.kind.among(Callback, Unknown, Namespace),
      "Unsupported signal parameter type '" ~ param.fullDType.to!string ~ "' (" ~ param.kind.to!string ~ ") for "
      ~ signal.fullDName.to!string);

    auto cbIndex = cast(int)callbackTypes.length - 1;

    if (param.direction == ParamDirection.In)
      preCall ~= ["", "static if (Parameters!T.length > " ~ cbIndex.to!dstring ~ ")", // Only process parameters which are included in the callback
        "_paramTuple[" ~ cbIndex.to!dstring ~ "] = getVal!(Parameters!T[" ~ cbIndex.to!dstring
        ~ "])(&_paramVals[" ~ (paramIndex + 1).to!dstring ~ "]);"]; // The parameter index is +1 because the first one is the object instance
    else // Out or InOut parameter?
    { // De-referenced value pointer (gpointer GValue)
      auto pointerParam = "*getVal!(Parameters!T[" ~ cbIndex.to!dstring ~ "]*)(&_paramVals["
        ~ (paramIndex + 1).to!dstring ~ "])";

      // Create variable for Out or InOut direction to pass to delegate
      preCall ~= ["", "Parameters!T[" ~ cbIndex.to!dstring ~ "] " ~ param.dName
        ~ (param.direction == ParamDirection.InOut ? (" = " ~ pointerParam ~ ";") : ";")]; // Assign GValue to variable if InOut

      preCall ~= ["", "static if (Parameters!T.length > " ~ cbIndex.to!dstring ~ ")", // Only process parameters which are included in the callback
        "_paramTuple[" ~ cbIndex.to!dstring ~ "] = " ~ param.dName ~ ";"]; // The parameter index is +1 because the first one is the object instance

      postCall ~= ["", "static if (Parameters!T.length > " ~ cbIndex.to!dstring ~ ")", pointerParam ~ " = "
        ~ param.dName ~ ";"];
    }

    with (TypeKind) callbackTypes ~= CallbackType(param.fullDType, param.kind.among(Object, Interface) != 0,
      param.direction);

    callbackProto ~= (callbackProto[$ - 1] != '(' ? ", "d : "") ~ param.directionStr ~ param.fullDType
      ~ " " ~ param.dName;
  }

  /// Process array parameter
  private void processArrayParam(Param param, ulong paramIndex)
  {
    if (param.direction != ParamDirection.In || param.ownership != Ownership.None)
      assert(0, "Unsupported delegate array parameter direction '" ~ param.direction.to!string
        ~ "' and ownership '" ~ param.ownership.to!string ~ "'");

    auto elemType = param.elemTypes[0];
    auto cbIndex = cast(int)callbackTypes.length - 1;

    if (elemType.dType == "char") // Handle char[] arrays specially (usually have an associated length parameter)
    {
      inpProcess ~= ["", "static if (Parameters!T.length > " ~ cbIndex.to!dstring ~ ")"]; // Only process parameters which are included in the callback

      if (param.lengthParam) // Array has length parameter?
        inpProcess ~= ["_paramTuple[" ~ cbIndex.to!dstring ~ "] = getStringWithLength(&_paramVals["
          ~ (paramIndex + 1).to!dstring ~ "], " ~ param.lengthParam.dName ~");"]; // The parameter index is +1 because the first one is the object instance
      else
        inpProcess ~= ["_paramTuple[" ~ cbIndex.to!dstring ~ "] = getVal!(string)(&_paramVals["
          ~ (paramIndex + 1).to!dstring ~ "]);"]; // The parameter index is +1 because the first one is the object instance

      callbackTypes ~= CallbackType("string", false, param.direction);
      callbackProto ~= (callbackProto[$ - 1] != '(' ? ", "d : "") ~ param.directionStr ~ "string " ~ param.dName;
      return;
    }

    inpProcess ~= ["", "static if (Parameters!T.length > " ~ cbIndex.to!dstring ~ ")", // Only process parameters which are included in the callback
      "{", "auto _cArray = getVal!(" ~ elemType.cTypeRemPtr ~ "**)(&_paramVals[" ~ (paramIndex + 1).to!dstring // The parameter index is +1 because the first one is the object instance
      ~ "]);", elemType.fullDType ~ "[] _dArray;"];

    with (TypeKind) callbackTypes ~= CallbackType(param.fullDType, elemType.kind.among(Object, Interface) != 0,
      param.direction);

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
        inpProcess ~= ["uint _len" ~ param.dName ~ ";", "if (_cArray)", "for (; _cArray" ~ "[_len" ~ param.dName ~ "] "
          ~ (elemType.cType.endsWith("*") ? "!is null"d : "!= 0") ~ "; _len" ~ param.dName ~ "++)", "break;"];
        lengthStr = "_len" ~ param.dName;
      }
      else
        assert(0); // This should be prevented by verify

      final switch (elemType.kind) with (TypeKind)
      {
        case Basic, BasicAlias, Enum, Flags, StructAlias, Struct, Pointer:
          inpProcess ~= "_dArray = cast(" ~ elemType.fullDType ~ "[])_cArray[0 .. " ~ lengthStr ~ "];";
          break;
        case String:
          inpProcess ~= ["foreach (i; 0 .. " ~ lengthStr ~ ")", "_dArray ~= _cArray[i].fromCString("
            ~ param.fullOwnerFlag ~ ".Free);"];
          break;
        case Opaque, Boxed, Wrap, Reffed:
          inpProcess ~= ["foreach (i; 0 .. " ~ lengthStr ~ ")", "_dArray ~= new " ~ elemType.fullDType ~ "(cast("
            ~ elemType.cType.stripConst ~ "*)&_cArray[i]" ~ (param.kind != Wrap ? (", " ~ param.fullOwnerFlag
            ~ ".Take") : "") ~ ");"];
          break;
        case Object, Interface:
          addImport("gobject.object");
          inpProcess ~= ["foreach (i; 0 .. " ~ lengthStr ~ ")", "_dArray ~= gobject.object.ObjectWrap._getDObject!("
            ~ elemType.fullDType ~ ")(_cArray[i], " ~ param.fullOwnerFlag ~ ".Take);"];
          break;
        case Unknown, Callback, Container, Namespace:
          assert(0, "Unsupported parameter array type '" ~ elemType.fullDType.to!string ~ "' (" ~ elemType.kind.to!string
              ~ ") for signal " ~ signal.fullDName.to!string);
      }
    }

    inpProcess ~= ["_paramTuple[" ~ paramIndex.to!dstring ~ "] = _dArray;", "}"];
    callbackProto ~= (callbackProto[$ - 1] != '(' ? ", "d : "") ~ param.directionStr ~ param.fullDType
      ~ " " ~ param.dName;
  }

  // Process other container parameters (not arrays)
  private void processContainerParam(Param param, ulong paramIndex)
  {
    if (param.direction != ParamDirection.In)
      assert(0, "Unsupported delegate container '" ~ param.containerType.to!string ~ "' parameter direction '"
        ~ param.direction.to!string ~ "' and ownership '" ~ param.ownership.to!string ~ "'");

    auto cbIndex = cast(int)callbackTypes.length - 1;

    preCall ~= ["", "static if (Parameters!T.length > " ~ cbIndex.to!dstring ~ ")"]; // Only process parameters which are included in the callback

    with (TypeKind) callbackTypes ~= CallbackType(param.fullDType);

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

    preCall ~= "_paramTuple[" ~ paramIndex.to!dstring ~ "] = g" ~ param.containerType.to!dstring ~ "ToD!("
      ~ templateParams ~ ")(cast(" ~ param.containerType.containerTypeCType ~ "*)getVal!(void*)(&_paramVals["
      ~ (paramIndex + 1).to!dstring ~ "]));\n"; // The parameter index is +1 because the first one is the object instance

    callbackProto ~= (callbackProto[$ - 1] != '(' ? ", "d : "") ~ param.directionStr ~ param.fullDType
      ~ " " ~ param.dName;
  }

  /**
   * Write signal binding to a CodeWriter.
   * Params:
   *   writer = Code writer to write to.
   *   moduleType = Module file type being written (defaults to ModuleType.Normal)
   */
  void write(CodeWriter writer, ModuleType moduleType = ModuleType.Normal)
  {
    writer ~= genDocs;
    writer ~= "ulong connect" ~ signal.titleName ~ "(T)(" ~ (signal.detailed ? "string detail = null, "d : "")
      ~ "T callback, Flag!\"After\" after = No.After)" ~ (moduleType == ModuleType.Iface ? ";"d : "");

    if (moduleType == ModuleType.Iface)
      return;

    writer ~= ["if (isCallable!T", "&& is(ReturnType!T " ~ (callbackTypes[0].isObject ? ": "d : "== "d) // Add isCallable and ReturnType constraints
      ~ callbackTypes[0].type ~ ")"];

    dstring[ParamDirection] dirStorage = [ParamDirection.In: "none", ParamDirection.Out: "out_", // Map of ParamDirection enum to ParameterStorageClass values
      ParamDirection.InOut: "ref_"];

    writer ~= callbackTypes[1 .. $].enumerate.map!(t => "&& (Parameters!T.length < " ~ (t.index + 1).to!dstring // Add parameter constraints, either callback does not have parameter or it complies
      ~ " || (ParameterStorageClassTuple!T[" ~ t.index.to!dstring ~ "] == ParameterStorageClass." // Check storage class depending on direction
      ~ dirStorage[t.value.direction] ~ " && is(Parameters!T[" ~ t.index.to!dstring ~ "] " // Check parameter type
      ~ (t.value.isObject ? ": "d : "== "d) ~ t.value.type ~ ")))").array;

    writer ~= ["&& Parameters!T.length < " ~ callbackTypes.length.to!dstring ~ ")", "{"]; // Ensure there aren't more arguments than expected

    writer ~= ["extern(C) void _cmarshal(GClosure* _closure, GValue* _returnValue, uint _nParams," // C marshal function
      ~ " const(GValue)* _paramVals, void* _invocHint, void* _marshalData)", "{",
      "assert(_nParams == " ~ (signal.params.length + 1).to!dstring ~ ", \"Unexpected number of signal parameters\");", // assert C marshal receives expected number of parameters
      "auto _dClosure = cast(DGClosure!T*)_closure;",
      "Tuple!(Parameters!T) _paramTuple;"]; // Create D type parameter tuple

    if (preCall.length > 0)
      writer ~= preCall;

    if (inpProcess.length > 0)
      writer ~= inpProcess;

    writer ~= ["", call];

    if (postCall.length > 0)
      writer ~= postCall;

    writer ~= ["}", "", "auto closure = new DClosure(callback, &_cmarshal);"];
    writer ~= ["return connectSignalClosure(\"" ~ signal.name ~ "\""
      ~ (signal.detailed ? `~ (detail.length ? "::" ~ detail : "")`d : "") ~ ", closure, after);", "}"];
  }

  // Generate documentation for connect template and signal callback
  private dstring[] genDocs()
  {
    auto docs = ["/**", "    Connect to `" ~ signal.titleName ~ "` signal.", ""];

    docs ~= ("    " ~ signal.gdocToDDocFunc(signal.docContent, "      ").stripLeft).splitLines;
    docs ~= ["", "    Params:"];

    if (signal.detailed)
      docs ~= "      detail = Signal detail or null (default)";

    docs ~= "      callback = signal callback delegate or function to connect";

    docs ~= ["", "        $(D " ~ callbackProto ~ ")"];

    docs ~= signal.params.filter!(pa => pa.isDParam).map!(pa => [""d] ~ ("        `" ~ pa.dName ~ "` "
      ~ signal.gdocToDDocFunc(pa.docContent, "          ").stripLeft ~ " (optional)").splitLines).join.array;

    docs ~= ["", "        `" ~ signal.signalDelegInstanceParam ~ "`"
      ~ " the instance the signal is connected to (optional)", ""];

    if (signal.returnVal && signal.returnVal.origDType != "none" && signal.returnVal.lengthArrayParams.length == 0)
      docs ~= ("        `Returns` " ~ signal.gdocToDDocFunc(signal.returnVal.docContent, "          ").stripLeft)
        .splitLines;

    docs ~= ["      after = Yes.After to execute callback after default handler, No.After to execute before (default)",
      "    Returns: Signal ID"];

    if (!signal.docVersion.empty || !signal.docDeprecated.empty)
    {
      docs ~= "";

      if (!signal.docVersion.empty)
        docs ~= "    Version: " ~ signal.docVersion;

      if (!signal.docDeprecated.empty)
        docs ~= ("    Deprecated: " ~ signal.gdocToDDocFunc(signal.docDeprecated, "      ").stripLeft).splitLines;
    }

    return docs ~ "*/";
  }

  // Information on a signal callback type (return value or parameter)
  struct CallbackType
  {
    dstring type; // The D type string
    bool isObject; // true if type is a derivable class
    ParamDirection direction; // Parameter direction (ignored for return values)
  }

  Func signal; /// The signal object being written
  Structure owningClass; /// The class which owns the signal (parent)
  CallbackType[] callbackTypes; /// Array of callback D types, first one is return type
  dstring callbackProto; /// Callback prototype (for docs)
  dstring call; /// The D delegate call
  LineTracker preCall; /// Pre-call code for call return variable, call output parameter variables, and input variable processing
  LineTracker inpProcess; /// Input processing (after preCall variable assignments and before the delegate call)
  LineTracker postCall; /// Post-call code for return value processing, output parameter processing, and input variable cleanup
}
