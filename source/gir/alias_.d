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
