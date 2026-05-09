module gir.return_value;

import gir.param;
import gir.type_node;
import std_includes;
import utils;

/// Function return value
final class ReturnValue : TypeNode
{
  this(Base parent, XmlNode node)
  {
    super(parent);
    fromXml(node);
  }

  override @property dstring name()
  {
    return "[RETURN]";
  }

  override void fromXml(XmlNode node)
  {
    super.fromXml(node);

    ownership = cast(Ownership)OwnershipValues.countUntil(node.get("transfer-ownership"));
    nullable = node.get("nullable") == "1";
  }

  protected override void resolve()
  {
    super.resolve;

    // Return pointers to basic types which aren't arrays should be treated as pointers
    // Return value pointers to basic types need special handling. When a function returns
    // a pointer to a basic type (e.g., int*, guint*), it's typically returning a reference
    // to data, not a single value. We reclassify these as Pointer kind to generate correct
    // D bindings. A more generic solution would be to handle this at the TypeNode level,
    // but that requires broader refactoring of the type resolution system.
    with(TypeKind) if (containerType == ContainerType.None && cType.countStars > 0
      && kind.among(Basic, BasicAlias, Enum, Flags))
    {
      info("Changing " ~ fullDName.to!string ~ " with C type " ~ cType.to!string ~ " from " ~ kind.to!string ~ " to pointer");
      kind = TypeKind.Pointer;

      if (cType.startsWith("const"))
        _dType = "const("d ~ _dType.stripConstPtr ~ ")"d ~ (cast(dchar)'*').repeat(cType.countStars).to!dstring;
      else
        _dType = _dType.stripConstPtr ~ (cast(dchar)'*').repeat(cType.countStars).to!dstring;
    }
  }

  override void toJson(ref JSONValue js)
  {
    super.toJson(js);
    js.jsonSetNonDefault("nullable", nullable);
  }

  bool nullable; /// Pointer can be null
  Param[] lengthArrayParams; /// Array parameters which use return value as length
}
