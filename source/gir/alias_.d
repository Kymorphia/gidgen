module gir.alias_;

import gir.type_node;
import utils;

/**
 * Gir type alias object.
 * The TypeNode parent stores the aliased type.
 */
final class Alias : TypeNode
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

  override void fromXml(XmlNode node)
  {
    super.fromXml(node);

    origName = node.get("name");
    _name = repo.subTypeStr(origName);

    if (_name == origName)
      _name = _name.normalizeDTypeName();

    cName = node.get("c:type");
  }

  protected override void resolve()
  {
    super.resolve;

    if (typeObject && typeObject.active != Active.Enabled)
      active = typeObject.active;

    typeObject = null; // Alias is itself a type, unset typeObject

    if (kind == TypeKind.Basic)
      kind = TypeKind.BasicAlias;
  }

  /**
   * Get the type object this alias refers to, following chains of aliases.
   * Returns: The aliased type object or null if the alias is of a basic type or the type is not found
   */
  @property TypeNode aliasedType()
  {
    TypeNode tn = this;

    foreach (i; 0 .. 4) // Resolve aliases (up to 4 dereferences)
    {
      auto al = cast(Alias)tn;

      if (!al || !al.typeRepo)
        break;

      tn = al.typeRepo.typeObjectHash.get(al._dType, null);
    }

    return tn !is this ? tn : null;
  }

  override void toJson(ref JSONValue js)
  {
    super.toJson(js);

    js["name"] = _name;
    js["origName"] = origName;
    js["cName"] = cName;
  }

  private dstring _name; /// D type name for the alias (Gir "name" attribute)
  dstring origName; /// Original alias name
  dstring cName; /// C type name for the alias (Gir "c:type" attribute)
}
