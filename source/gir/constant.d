module gir.constant;

import gir.type_node;

final class Constant : TypeNode
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

  override @property dstring dName()
  {
    return _name;
  }

  override void fromXml(XmlNode node)
  {
    super.fromXml(node);
    _name = node.get("name");
    cName = node.get("c:type");
    value = node.get("value");
  }

  private dstring _name; /// Name of constant
  dstring cName; /// C name of constant
  dstring value; /// Value of constant
}
