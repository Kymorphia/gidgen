module gir.param;

import defs;
import gir.func;
import gir.structure;
import gir.type_node;
import std_includes;
import utils;

/// Function parameter
final class Param : TypeNode
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

  /// Get the parameter name formatted in D camelCase
  override dstring dName()
  {
    return repo.defs.symbolName(name.camelCase);
  }

  override @property TypeKind kind()
  {
    auto curKind = super.kind;
    auto tn = cast(TypeNode)typeObject;
    if (curKind != TypeKind.Unknown && curKind != TypeKind.Callback && tn)
      return tn.kind;

    return super.kind;
  }

  override @property void kind(TypeKind kind)
  {
    super.kind(kind);
  }

  /// Get direction string (empty for input, "ref " if callerAllocates=1, "out ", or "inout "). Intended for use directly in code generation.
  dstring directionStr()
  {
    if (direction == ParamDirection.In)
      return "";
    else if (callerAllocates && containerType == ContainerType.Array)
      return "ref ";
    else if (direction == ParamDirection.Out)
      return "out ";
    else
      return "ref ";
  }

  /// Is this parameter a length for a return array and/or one or more array parameters
  @property bool isArrayLength()
  {
    return isLengthReturnArray || lengthArrayParams.length > 0;
  }

  /**
   * Check if a parameter is used by the D binding function
   * Returns: true if the parameter is passed to the D function, false otherwise (instance parameter, array length, closure data, etc)
   */
  @property bool isDParam()
  {
    return !isInstanceParam && !isLengthReturnArray && lengthArrayParams.length == 0 && !isClosure && !isDestroy;
  }

  override void fromXml(XmlNode node)
  {
    super.fromXml(node);

    _name = node.get("name");
    isInstanceParam = node.id == "instance-parameter";
    direction = cast(ParamDirection)ParamDirectionValues.countUntil(node.get("direction"));
    ownership = cast(Ownership)OwnershipValues.countUntil(node.get("transfer-ownership"));
    nullable = node.get("nullable") == "1";
    optional = node.get("optional") == "1";
    allowNone = node.get("allow-none") == "1";
    callerAllocates = node.get("caller-allocates") == "1";
    varargs = node.findChild("varargs") !is null;

    if (auto pVal = "closure" in node.attrs)
      closureIndex = (*pVal).to!int;
    else
      closureIndex = NoClosure;

    if (auto pVal = "destroy" in node.attrs)
      destroyIndex = (*pVal).to!int;
    else
      destroyIndex = NoDestroy;

    scope_ = cast(ParamScope)ParamScopeValues.countUntil(node.get("scope"));
  }

  protected override void fixup()
  {
    if (origDType == "...") // Skip variable args elipsis
      return;

    super.fixup; // Fixup the type node state

    auto func = getParentByType!Func;

    // Convert char** out parameters to output strings, not output array of strings
    if (containerType == ContainerType.Array && direction == ParamDirection.Out && cType.stripConst == "char**")
    {
      containerType = ContainerType.None;
      kind = TypeKind.String;
      elemTypes.length = 0;
    }

    if (lengthParamIndex >= 0)
    {
      if (func.hasInstanceParam) // Instance parameters don't count towards index
        lengthParamIndex++;

      if (lengthParamIndex < func.params.length)
      {
        lengthParam = func.params[lengthParamIndex];

        // gidgen extension which allows other zero terminated arrays to be used for length, should not have lengthArrayParams assigned for the reference array
        if (lengthParam.containerType != ContainerType.Array)
          lengthParam.lengthArrayParams ~= this;
      }
    }
    else if (lengthParamIndex == ArrayLengthReturn) // Array parameter uses return value as length
    {
      lengthReturn = func.returnVal;
      lengthReturn.lengthArrayParams ~= this;
    }

    if (closureIndex != NoClosure)
    {
      if (dType != "void*" && dType != "const(void)*") // Is this the callback? (not the closure data parameter)
      {
        if (func.hasInstanceParam) // Instance parameters don't count towards index
          closureIndex++;

        if (closureIndex < func.params.length)
        {
          auto closureParam = func.params[closureIndex];
          closureParam.isClosure = true;
          closureParam.callbackIndex = cast(int)func.params.countUntil!(x => x is this);
        }
      }
      else // Some closure data parameters reference themselves or the callback, ignore such references, but mark parameter as closure data
      {
        isClosure = true;
        closureIndex = NoClosure;
      }
    }

    if (destroyIndex != NoDestroy)
    {
      if (dType != "DestroyNotify") // Is this the callback? (not destroy notify parameter)
      {
        if (func.hasInstanceParam) // Instance parameters don't count towards index
          destroyIndex++;

        if (destroyIndex < func.params.length)
        {
          auto destroyParam = func.params[destroyIndex];
          destroyParam.isDestroy = true;
          destroyParam.callbackIndex = cast(int)func.params.countUntil!(x => x is this);
        }
      }
      else // Some destroy notify parameters reference themselves or the callback, ignore such references, but mark parameter as a destroy notify
      {
        isDestroy = true;
        destroyIndex = NoDestroy; // Ignore destroy notify parameter references to itself or the callback
      }
    }

    if (cType == "GByteArray*" && direction == ParamDirection.Out) // HACK?: Out GByteArray* is more like InOut
    {
      direction = ParamDirection.InOut;
      info("Using InOut direction for GByteArray* parameter instead of Out");
    }
  }

  protected override void resolve()
  {
    if (origDType == "...") // Skip variable args elipsis
      return;

    super.resolve;
  }

  protected override void verify()
  {
    super.verify;

    auto func = getParentByType!Func;

    if (func.funcType == FuncType.Signal)
    {
      with(TypeKind) if (containerType != ContainerType.None
          && elemTypes[0].kind.among(Unknown, Callback, Container, Namespace))
        throw new Exception("Signal container '" ~ containerType.to!string ~ "' parameter element kind '"
          ~ kind.to!string ~ "' not supported");

      with(TypeKind) if (containerType == ContainerType.None && kind.among(Callback, Unknown, Namespace))
        throw new Exception("Signal parameter '" ~ dType.to!string ~ "' with kind '" ~ kind.to!string ~ "' not supported");

      return;
    }

    if (kind == TypeKind.Unknown)
      throw new Exception("Unresolved type for parameter '" ~ fullDName.to!string ~ "'");

    if (containerType != ContainerType.None)
    {
      if ((direction == ParamDirection.In || direction == ParamDirection.InOut) && ownership != Ownership.None)
        throw new Exception("Container " ~ containerType.to!string ~ " parameter with direction "
          ~ direction.to!string ~ " ownership " ~ ownership.to!string ~ " not supported");
    }

    with (TypeKind) if (containerType == ContainerType.None && kind.among(Basic, BasicAlias, Enum, Flags))
    {
      if (direction == ParamDirection.In && cType.countStars > 0 && cType != "void*" && cType != "const(void)*")
        throw new Exception("Basic input parameter type '" ~ dType.to!string ~ "' has unexpected C type '"
          ~ cType.to!string ~ "'");

      if (direction == ParamDirection.Out && !callerAllocates && cType.countStars == 0)
        throw new Exception("Basic output parameter type '" ~ dType.to!string ~ "' has unexpected C type '"
          ~ cType.to!string ~ "'");

      if (direction == ParamDirection.Out && callerAllocates && cType.countStars == 0)
        throw new Exception("Basic output parameter type '" ~ dType.to!string ~ "' has unexpected C type '"
          ~ cType.to!string ~ "'");
    }

    // Enable callerAllocates for parameters which are output pointers to Struct types
    with (TypeKind) if (direction == ParamDirection.Out && kind.among(Struct, StructAlias)
      && cType.countStars == 1 && !callerAllocates)
    {
      info("Enabling caller allocates for Out parameter '" ~ fullDName.to!string ~ "' with a pointer to a struct type");
      callerAllocates = true;
    }

    if (direction == ParamDirection.InOut)
    {
      if (kind == TypeKind.String)
        throw new Exception("Unsupported string InOut parameter");

      // Ownership other than None does not make sense for InOut parameters
      if (ownership != Ownership.None)
      {
        info("Changing InOut parameter '" ~ fullDName.to!string ~ "' with ownership '" ~ ownership.to!string
          ~ "' to None");
        ownership = Ownership.None;
      }

      // Identify incorrect caller-allocates=0 for structured types, warn, and set callerAllocates to true
      with (TypeKind) if (!callerAllocates && cType.countStars == 1 && kind.typeKindIsStructured)
      {
        info("Changing InOut parameter '" ~ fullDName.to!string ~ "' caller-allocates to true");
        callerAllocates = true;
      }
    }

    if (kind == TypeKind.Boxed && direction == ParamDirection.Out && cType.countStars != 2)
    {
      auto st = cast(Structure)typeObject;

      if (!st || (!st.ctorFunc && (st.opaque || st.pointer || st.fields.empty)))
        throw new Exception("Unsupported boxed type Out parameter of type '" ~ dType.to!string
          ~ "' requiring caller allocation of opaque structure");
    }

    if (containerType == ContainerType.Array) with (ParamDirection)
    {
      if (lengthParamIndex == ArrayLengthReturn && (!lengthReturn || lengthReturn.kind != TypeKind.Basic))
        throw new Exception("Invalid return value for array length");

      if (lengthParamIndex >= 0 && !lengthParam) // Array has invalid length argument?
        throw new Exception("Invalid array length parameter index");

      if (direction == In)
      {
        if (lengthParam && lengthParam.direction != In)
          throw new Exception("Input array has unsupported length parameter direction '"
            ~ lengthParam.direction.to!string ~ "'");
        else if (lengthReturn)
          throw new Exception("Input array cannot specify return value as length");

        if (cType.countStars < 1)
          throw new Exception("Input array has unexpected C type '" ~ cType.to!string ~ "'");
      }
      else if (direction == Out)
      {
        if (callerAllocates)
        {
          if (cType.countStars != 1)
            throw new Exception("Caller allocated output array has unexpected C type '" ~ cType.to!string ~ "'");

          if (lengthParam)
          {
            if (lengthParam.direction == Out)
              throw new Exception("Caller allocated output array has unsupported length parameter direction '"
                ~ lengthParam.direction.to!string ~ "'");
          }
          else if (!lengthReturn && fixedSize == ArrayNotFixed)
            throw new Exception("Caller allocated output array has indeterminate length");
        }
        else // Caller does not allocate
        {
          if (cType.countStars < 2)
            throw new Exception("Callee allocated output array has unexpected C type '" ~ cType.to!string ~ "'");

          // A gidgen GIR extension which allows another zero-terminated array to be used for length
          if (lengthParam && lengthParam.containerType == ContainerType.Array)
          {
            if (!lengthParam.zeroTerminated || lengthParam.direction != In)
              throw new Exception("Using another array for length requires that it is a zero terminated input array");
          }
          else if (lengthParam && lengthParam.direction != Out)
            throw new Exception("Callee allocated output array has unsupported length parameter direction '"
              ~ lengthParam.direction.to!string ~ "'");
        }
      }
      else if (direction == InOut)
      {
        if (!callerAllocates)
          throw new Exception("InOut arrays must be caller allocated");
        else if (ownership != Ownership.None)
          throw new Exception("InOut arrays cannot be ownership " ~ ownership.to!string);
      }
    }
    else if (containerType != ContainerType.None)
    {
      auto stars = cType.countStars;
      auto isList = containerType == ContainerType.List || containerType == ContainerType.SList;

      // See docs/param_validation_table.md
      if ((direction == ParamDirection.In && stars != 1)
          || (direction == ParamDirection.Out && isList && stars != 2)
          || (direction == ParamDirection.Out && !isList && callerAllocates && stars != 1)
          || (direction == ParamDirection.Out && !isList && !callerAllocates && stars != 2)
          || (direction == ParamDirection.InOut && ownership != Ownership.None)
          || (direction == ParamDirection.InOut && !isList && !callerAllocates)
          || (direction == ParamDirection.InOut && isList && stars != 2)
          || (direction == ParamDirection.InOut && !isList && stars != 1))
        throw new Exception("Invalid container param with type '" ~ containerType.to!string ~ "' direction '"
          ~ direction.to!string ~ "' ownership '" ~ ownership.to!string ~ "' callerAllocates="
          ~ callerAllocates.to!string ~ "' and C type '" ~ cType.to!string);
    }

    if (closureIndex != NoClosure)
    {
      if (kind != TypeKind.Callback)
        throw new Exception("Non-callback parameter has closure");

      if (closureIndex >= func.params.length)
        throw new Exception("Invalid closure data parameter index");

      auto closureParam = func.params[closureIndex];

      if (closureParam.direction != ParamDirection.In || closureParam.dType != "void*")
        throw new Exception("Closure data should be a void* input parameter");

      auto closureCount = (cast(Func)typeObjectRoot).params.count!(x => x.isClosure);
      if (closureCount != 1)
        throw new Exception("Parameter callback must have one closure argument");

      if (destroyIndex != NoDestroy)
      {
        if (destroyIndex >= func.params.length)
          throw new Exception("Invalid destroy notify callback parameter index");

        auto destroyParam = func.params[destroyIndex];

        if (destroyParam.direction != ParamDirection.In || !cast(Func)destroyParam.typeObjectRoot
            || (cast(Func)destroyParam.typeObjectRoot).funcType != FuncType.Callback)
          throw new Exception("Unsupported destroy notify callback parameter");
      }
    }
    else if (destroyIndex != NoDestroy)
      throw new Exception("Parameter without a closure has a destroy notify");

    if (auto cb = cast(Func)typeObjectRoot)
      if (!isDestroy && !cb.closureParam && scope_ != ParamScope.Call)
        throw new Exception ("Callback parameters with scope '" ~ scope_.to!string
          ~ "' should have a closure parameter");
  }

  override void toJson(ref JSONValue js)
  {
    super.toJson(js);

    js["name"] = _name;
    js.jsonSetNonDefault("isInstanceParam", isInstanceParam);
    js.jsonSetNonDefault("isLengthReturnArray", isLengthReturnArray);
    js.jsonSetNonDefault("lengthArrayParams", lengthArrayParams);
    js.jsonSetNonDefault("isOptional", isOptional);

    if (direction != ParamDirection.In)
      js["direction"] = direction.to!string;

    js.jsonSetNonDefault("nullable", nullable);
    js.jsonSetNonDefault("optional", optional);
    js.jsonSetNonDefault("allowNone", allowNone);
    js.jsonSetNonDefault("callerAllocates", callerAllocates);
    js.jsonSetNonDefault("varargs", varargs);
    js.jsonSetNonDefault("isClosure", isClosure);
    js.jsonSetNonDefault("isDestroy", isDestroy);
    js.jsonSetNonDefault("closureIndex", closureIndex, NoClosure);
    js.jsonSetNonDefault("destroyIndex", destroyIndex, NoDestroy);
    js.jsonSetNonDefault("callbackIndex", callbackIndex, NoCallback);
    if (scope_ != ParamScope.Unset) js["scope"] = scope_.to!string;
  }

  private dstring _name; /// Name of parameter
  bool isInstanceParam; /// true if this parameter is the instance parameter
  bool isLengthReturnArray; /// true if this is a length parameter for a return array
  bool isOptional; /// Set to true if GIR optional is set and all D params following are also isOptional
  Param[] lengthArrayParams; /// Array parameters which use this one as a length
  ParamDirection direction; /// Parameter direction
  bool nullable; /// Nullable pointer (treated as "optional"), null is always passed as is to C functions
  bool optional; /// Optional pointer
  bool allowNone; /// Allow none (deprecated and replaced with nullable and optional, ignored)
  bool callerAllocates; /// Caller allocates value
  bool varargs; /// Indicates a parameter is a varargs ... elipsis
  bool isClosure; /// Is this a closure user_data parameter?
  bool isDestroy; /// Is this a destroy notification callback parameter?
  int closureIndex = NoClosure; /// Closure parameter index (user data for a callback parameter)
  int destroyIndex = NoDestroy; /// Destroy parameter index (destroy notify for a callback parameter)
  int callbackIndex = NoCallback; /// If isClosure or isDestroy is true then this is the callback parameter index
  ParamScope scope_; /// Scope of the callback closure data
}

/// Direction of a parameter
enum ParamDirection
{
  In = -1, /// Input direction (not actually found in Gir files, since it is the default unspecified value)
  Out, /// Output direction
  InOut, /// Input and output direction
}

immutable dstring[] ParamDirectionValues = ["out", "inout"];

/// Callback parameter closure data scope (how long it should remain frozen, so that it is not collected)
enum ParamScope
{
  Unset = -1, // FIXME - What should the default be?
  Call, /// For the duration of the function call
  Async, /// Until a single call to the callback function (possibly after the function returns)
  Notified, /// Until the destroy notify function is called
  Forever, /// Should be allocated for the duration of the program
}

immutable dstring[] ParamScopeValues = ["call", "async", "notified", "forever"];

enum NoClosure = -1;
enum NoDestroy = -1;
enum NoCallback = -1;
