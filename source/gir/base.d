module gir.base;

public import std.json : JSONValue;
import std.string : lastIndexOf;

import code_writer;
public import gir.repo;
import gir.structure;
import std_includes;
public import xml_tree;
import gir.base_range;

private static Base[XmlNode] xmlNodeBaseHash;

abstract class Base
{
  this()
  {
  }

  this(Base parent)
  {
    this.parent = parent;

    if (parent)
    {
      childIndex = cast(uint)parent.children.length;
      parent.children ~= this;
    }

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
    {
      auto s = b.name;

      if (s.length > 0)
        full = full.length > 0 ? s ~ "." ~ full : s;
    }

    return full;
  }

  /// Full type name of object with ancestors separated by periods (D package directory, module, class, and D type)
  dstring fullDName()
  {
    dstring full;

    for (auto b = this; b; b = b.parent)
    {
      auto s = b.dName;

      if (s.length > 0)
        full = full.length > 0 ? (s ~ "." ~ full) : s;
    }

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
   * Get a JSONValue object representing the object's state
   * Returns: JSONValue object
   */
  JSONValue jsObj()
  {
    auto typeName = typeid(this).name;
    auto index = typeName.lastIndexOf('.');
    typeName = typeName[index + 1 .. $]; // Gracefully handles -1 as well (not found)
    JSONValue js = JSONValue(["type" : typeName]);
    toJson(js);
    return js;
  }

  /**
   * Store object state to JSON for troubleshooting and diffing changes.
   * Params:
   *   js = JSON object to populate with object state
   */
  void toJson(ref JSONValue js)
  {
    js["repo"] = repo ? repo.dName : ""d;
    js["parent"] = parent ? parent.fullDName : ""d;
    js["active"] = active.to!string;

    js.jsonSetNonDefault("attributes", attributes);

    if (dumpJsonDocs && docContent.length > 0)
    {
      js["docContent"] = docContent;
      js.jsonSetNonDefault("docFilename", docFilename);
      js["docLine"] = docLine;
    }

    js.jsonSetNonDefault("docVersion", docVersion);
    js.jsonSetNonDefault("docDeprecated", docDeprecated);

    if (sourceFilename.length > 0)
    {
      js["sourceFilename"] = sourceFilename;
      js["sourceLine"] = sourceLine;
    }
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

  /**
   * Get next sibling object.
   * Returns: Next sibling or null
   */
  Base next()
  {
    return (parent && childIndex + 1 < parent.children.length) ? parent.children[childIndex + 1] : null;
  }

  /**
   * Get previous sibling object.
   * Returns: Previous sibling or null
   */
  Base prev()
  {
    return (parent && childIndex > 0) ? parent.children[childIndex - 1] : null;
  }

  /**
   * Return a range that can be used to traverse a Base object node tree.
   * Returns: New forward range
   */
  BaseRange walk()
  {
    return BaseRange(this);
  }

  private XmlNode _node; /// The XML node object was created from

  Repo repo; /// Parent repo
  Base parent; /// Parent base object
  Base[] children; /// Children objects
  uint childIndex; /// Index of a child in its parent (if parent is set)
  Active active; /// Indicates active or inactive state of an object
  dstring[dstring] attributes; /// Gir key/value attributes
  dstring docContent; /// Documentation content
  dstring docFilename; /// Documentation filename
  uint docLine; /// Documentation line number
  dstring docVersion; /// Version of the API where support for this object was added
  dstring sourceFilename; /// Source code filename
  uint sourceLine; /// Source code line number
  dstring docDeprecated; /// Deprecated note documentation

  static bool dumpJsonDocs; /// Set to true to dump JSON docs in toJson
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

/**
 * Helper template to assign a value to a JSONValue object.
 * Params:
 *   T = The value type
 *   js = The JSONValue object to assign to
 *   key = The JSON object key string
 *   value = The value to assign
 */
void jsonSet(T)(ref JSONValue js, string key, T value)
{
  static if (is(T : E[], E) && is(E : Object))
    js[key] = value.map!(x => x.jsObj).array;
  else static if (is(T : Object))
    js[key] = value.jsObj;
  else static if (is(T : dstring[E], E))
    js[key] = value.byPair.map!(p => tuple(p.key.to!string, p.value)).assocArray;
  else
    js[key] = value;
}

/**
 * Helper template to assign a value to a JSONValue object if it isn't a default value.
 * Params:
 *   key = The JSON object key string
 *   T = The value type
 *   js = The JSONValue object to assign to
 *   value = The value to assign
 *   def = The default value (defaults to T.init)
 */
void jsonSetNonDefault(T)(ref JSONValue js, string key, T value, T def = T.init)
{
  if (value != def)
    jsonSet(js, key, value);
}

