module gir.base;

import code_writer;
public import gir.repo;
import gir.structure;
import std_includes;
public import xml_tree;

private static Base[XmlNode] xmlNodeBaseHash;

abstract class Base
{
  this()
  {
  }

  this(Base parent)
  {
    this.parent = parent;

    this.repo = getParentByType!Repo;
    assert(this.repo !is null);
  }

  @property dstring name()
  {
    return "";
  }

  @property void name(dstring val)
  {
  }

  /// D type name
  @property dstring dName()
  {
    return "";
  }

  /// Full name of object with ancestors separated by periods (GIR namespace)
  dstring fullName()
  {
    dstring full;

    for (auto b = this; b; b = b.parent)
      if (auto s = b.name)
        full = full.length > 0 ? s ~ "." ~ full : s;

    return full;
  }

  /// Full type name of object with ancestors separated by periods (D package directory, module, class, and D type)
  dstring fullDName()
  {
    dstring full;

    for (auto b = this; b; b = b.parent)
      if (auto s = b.dName)
        full = full.length > 0 ? (s ~ "." ~ full) : s;

    return full;
  }

  /// The GError domain type name (exception name)
  dstring errorDomain()
  {
    if (auto st = getParentByType!Structure)
    { // Not global struct and has an error quark function? (hopefully only one)
      if (st != st.repo.globalStruct && !st.errorQuarks.empty)
        return st.dType ~ "Exception";
    }

    return "ErrorWrap"; // Default error domain
  }

  @property XmlNode xmlNode()
  {
    return _node;
  }

  @property void xmlNode(XmlNode node)
  {
    _node = node;
    xmlNodeBaseHash[node] = this;
  }

  void fromXml(XmlNode node)
  {
    this.xmlNode = node;

    // "disable", "ignore", and "unsupported" are gidgen extensions (not part of GIR spec)

    if (node.get("disable") == "1")
      this.active = Active.Disabled;

    if (node.get("ignore") == "1")
      this.active = Active.Ignored;

    if (node.get("unsupported") == "1")
      this.active = Active.Unsupported;
  }

  /**
   * Get an XML selector for this node. Of the form ID[NAME].ID[NAME].. such as class[Widget].method[show]. registry.namespace is not included.
   * Returns: The XML selector or null if there is no XML node associated with the object.
   */
  dstring xmlSelector()
  {
    if (!_node)
      return null;

    dstring s;
    for (auto n = _node; n && n.id != "namespace"; n = n.parent)
      s = n.id ~ ("name" in n.attrs ? "[" ~ n["name"] ~ "]" : "") ~ (s.empty ? "" : "." ~ s);

    return s;
  }

  /**
   * Returns a "file:line " location string from where this node was parsed from or empty string if not set.
   * Returns: Location information in the form of file:line
   */
  string xmlLocation()
  {
    if (!_node || _node.parseFile.empty)
      return "";

    return _node.parseFile ~ (_node.parseLine != 0 ? (":" ~ _node.parseLine.to!string) : "");
  }

  /**
   * Write DDoc documentation for an object to a CodeWriter.
   * Params:
   *   writer = The CodeWriter
   */
  dstring genDocs()
  {
    if (docContent.length == 0)
      return "/** */"; // Add blank docs if none, so that it is still included in generated DDocs

    auto s = "/**\n    "d ~ repo.gdocToDDoc(docContent, "    ").stripLeft ~ "\n";

    if (!docVersion.empty || !docDeprecated.empty)
    {
       s ~= "\n";

      if (!docVersion.empty)
        s ~= "    Version: " ~ docVersion ~ "\n";

      if (!docDeprecated.empty)
        s ~= "    Deprecated: " ~ repo.gdocToDDoc(docDeprecated, "      ").stripLeft ~ "\n";
    }

    return s ~ "*/";
  }

  /**
    * Template to get an Base parent object of a given type, including itself.
    * Params:
    *   T = The expected type of object
    * Returns: The parent object of the given type (can be the object itself) or null
    */
  T getParentByType(T)()
  {
    for (auto n = this; n !is null; n = n.parent)
      if (auto found = cast(T)n)
        return found;

    return null;
  }

  private XmlNode _node; /// The XML node object was created from

  Repo repo; /// Parent repo
  Base parent; /// Parent base object
  Active active; /// Indicates active or inactive state of an object
  dstring[dstring] attributes; /// Gir key/value attributes
  dstring docContent; /// Documentation content
  dstring docFilename; /// Documentation filename
  uint docLine; /// Documentation line number
  dstring docVersion; /// Version of the API where support for this object was added
  dstring sourceFilename; /// Source code filename
  uint sourceLine; /// Source code line number
  dstring docDeprecated; /// Deprecated note documentation
}

/// Indicates active state of an object
enum Active : ubyte
{
  Enabled, /// Object is enabled
  Disabled, /// Object was explicitly disabled
  Ignored, /// Object was explicitly ignored (does not appear in stats table)
  Unsupported, /// Object is not supported by gidgen currently
}

/**
  * Template to get an XmlNode parent object of a given type.
  * Params:
  *   T = The expected type of object
  *   node = The XML node
  * Returns: The parent object of the given type or null
  */
T baseParentFromXmlNode(T)(XmlNode node)
{
  if (auto parent = node.parent)
    if (auto obj = xmlNodeBaseHash.get(parent, null))
      return cast(T)obj;

  return null;
}

/**
  * Template to get an XmlNode parent object of a given type and warn if not found.
  * Params:
  *   T = The expected type of object
  *   node = The XML node
  * Returns: The parent object of the given type or null
  */
T baseParentFromXmlNodeWarn(T)(XmlNode node)
{
  if (auto parent = node.parent)
  {
    if (auto obj = xmlNodeBaseHash.get(parent, null))
    {
      if (auto castObj = cast(T)obj)
        return castObj;

      node.warn(
          "Expected parent node object type '" ~ T.stringof
          ~ "' found '" ~ obj.stringof ~ "'");
    }
    else
      node.warn("No object found for node parent");
  }
  else
    node.warn("Expected node to have a parent");

  return null;
}
