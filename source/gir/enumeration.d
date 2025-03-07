module gir.enumeration;

import gir.base;
import gir.func;
import gir.member;
import gir.type_node;
import utils;

/// Enumeration or bitfield flags
final class Enumeration : TypeNode
{
  this(Base parent, XmlNode node)
  {
    super(parent);
    fromXml(node);
  }

  override @property dstring name()
  {
    return dType;
  }

  override void fromXml(XmlNode node)
  {
    super.fromXml(node);

    bitfield = node.id == "bitfield";

    glibGetType = node.get("glib:get-type");
    glibTypeName = node.get("glib:type-name");
    glibErrorDomain = node.get("glib:error-domain");
    version_ = node.get("version");
  }

  override void fixup()
  {
    kind = bitfield ? TypeKind.Flags : TypeKind.Enum;
    super.fixup;

    Member[dstring] dupCheck; // Duplicate member check

    foreach (m; members)
    {
      m.fixup;

      if (auto dup = m.dName in dupCheck)
      {
        m.active = Active.Ignored;
        dup.xmlNode.warn("Ignoring duplicate enum member '" ~ m.fullDName ~ "'");
        continue;
      }

      dupCheck[dName] = m;
    }
  }

  bool bitfield; /// true if flags bitfield, false for enum
  dstring glibGetType; /// GLib get_type function name
  dstring glibTypeName; /// GLib type name
  dstring glibErrorDomain; /// GLib error domain
  dstring version_; /// Version

  Member[] members; /// Enum/flags members
  Func[] functions; /// Functions
}
