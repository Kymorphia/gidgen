module gir.member;

import gir.type_node;
import utils : camelCase;

/// Member value for Enumeration
final class Member : TypeNode
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
    return _dName;
  }

  override void fromXml(XmlNode node)
  {
    super.fromXml(node);

    _name = node.get("name");
    cName = node.get("c:identifier");
    value = node.get("value");
    glibName = node.get("glib:name");
    glibNick = node.get("glib:nick");
  }

  override void fixup()
  {
    super.fixup;
    _dName = repo.defs.symbolName(_name.camelCase(true));
  }

  private dstring _name; /// Name of the enumeration or bitfield member
  private dstring _dName; /// D enum/flags member identifier (TitleCase)
  dstring cName; /// C name (Gir "c:identifier")
  dstring value; /// The value
  dstring glibName; /// GLib enum/flags name
  dstring glibNick; /// GLib enum/flags nick name
}
