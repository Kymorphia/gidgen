module xml_tree;

import core.exception : RangeError;
import std_includes;
import utils;
import xml_patch;

final class XmlTree
{
  this()
  {
    xmlDoc = new XmlNode("xml");
  }

  this(dstring data, string fileName = "<UNKNOWN>")
  {
    this();
    parse(data, fileName);
  }

  void parse(dstring xmlstr, string fileName = "<UNKNOWN>")
  {
    enum State
    {
      Node, // Inside element content (node)
      Element, // Inside an element tag
      Xml, // XML doc <?xml ?> node
    }

    uint lineCount = 1;
    uint charIndex;
    XmlNode[] nodeStack;
    State state;

    void error(string message)
    {
      throw new XmlParseError(message, fileName, lineCount, charIndex);
    }

    void pop(size_t len = 1)
    {
      foreach (i; 0 .. len)
      {
        if (xmlstr[i] == '\n')
        {
          lineCount++;
          charIndex = 0;
        }
        else
          charIndex++;
      }

      xmlstr = xmlstr.drop(len);
    }

    dstring popval(size_t len = 1)
    {
      dstring retval = xmlstr[0 .. len];
      pop(len);
      return retval;
    }

    // Skip whitespace, returns true if there was any
    bool skipWhiteSpace()
    {
      auto len = xmlstr.countUntil!(x => !isWhite(x));

      if (len > 0)
        pop(len);

      return len > 0;
    }

    bool match(dstring m)
    {
      if (xmlstr.startsWith(m))
      {
        pop(m.length);
        return true;
      }
      return false;
    }

    bool parseName(out dstring name)
    {
      if (xmlstr.empty || !isValidElemStartChar(xmlstr[0]))
        return false;

      auto len = xmlstr[1 .. $].countUntil!(x => !isValidElemChar(x));
      name ~= popval(1 + len);
      return true;
    }

    bool parseContent(out dstring val, dchar quotechar = 0)
    {
      if (xmlstr.empty)
        return false;

      auto endChars = quotechar != 0 ? ['<', '>', quotechar] : [cast(dchar)'<', '>'];

      while (!xmlstr.empty && !endChars.canFind(xmlstr[0]))
      {
        if (match("&"))
        {
          if (match("#x"))
          {
            dchar d = 0;

            while (!xmlstr.empty && xmlstr[0] != ';')
            {
              if (xmlstr[0] >= '0' && xmlstr[0] <= '9')
                d = d << 4 | (popval()[0] - '0');
              else if (xmlstr[0] >= 'a' && xmlstr[0] <= 'f')
                d = d << 4 | (popval()[0] - 'a' + 0xa);
              else if (xmlstr[0] >= 'A' && xmlstr[0] <= 'F')
                d = d << 4 | (popval()[0] - 'A' + 0xA);
              else
                error(format("Invalid digit '%s' in XML hex numeric entity",
                    xmlstr[0]));
            }

            if (!match(";"))
              error(
                  "Unexpected EOF while looking for XML hex numeric entity terminator");

            val ~= d;
          }
          else if (match("#"))
          {
            dchar d = 0;

            while (!xmlstr.empty && xmlstr[0] != ';')
            {
              if (xmlstr[0] >= '0' && xmlstr[0] <= '9')
                d = d * 10 + (popval()[0] - '0');
              else
                error(format("Invalid digit '%s' in XML numeric entity", xmlstr[0]));
            }

            if (!match(";"))
              error("Unexpected EOF while looking for XML numeric entity terminator");

            val ~= d;
          }
          else if (match("amp;"))
            val ~= '&';
          else if (match("lt;"))
            val ~= '<';
          else if (match("gt;"))
            val ~= '>';
          else if (match("apos;"))
            val ~= '\'';
          else if (match("quot;"))
            val ~= '"';
          else
            error("Unknown XML character entity");
        }
        else // Not a & character entity
          val ~= popval();
      }

      return true;
    }

    bool parseAttribute(out dstring name, out dstring val)
    {
      if (!parseName(name))
        return false;

      if (!match("="))
        error("Expected '=' delimiter for XML attribute value");

      dchar quotechar;

      if (!xmlstr.empty)
        quotechar = popval()[0];

      if (quotechar != '"' && quotechar != '\'')
        error("Expected XML attribute value quote character");

      parseContent(val, quotechar);

      if (xmlstr.empty)
        error("Unexpected EOF looking for XML attribute value closing quote");

      if (!match([quotechar]))
        error(format("Unexpected character '%s' in XML attribute value", xmlstr[0]));

      return true;
    }

    while (!xmlstr.empty)
    {
      dstring name, val;

      skipWhiteSpace();

      if (state == State.Node && match("</")) // Closing element?
      {
        if (!parseName(name))
          error("Invalid XML element name");

        if (name != nodeStack[$ - 1].id)
          error(format("Unexpected XML closing element '%s', expected '%s'",
              name, nodeStack[$ - 1].id));

        nodeStack = nodeStack[0 .. $ - 1];

        if (!match(">"))
          error("Expected '>' at end of XML closing tag");
      }
      else if (match("<!--")) // Comment
      {
        auto len = xmlstr.countUntil("-->");

        if (len == -1)
          error("Unexpected EOF looking for comment closing");

        pop(len + 3);
      }
      else if (state == State.Node && match("<?xml"))
        state = State.Xml;
      else if (state == State.Node && match("<")) // Opening/standalone element?
      {
        auto parseLine = lineCount;

        if (!parseName(name))
          error("Invalid XML element name");

        nodeStack ~= new XmlNode(name, fileName, parseLine);
        state = State.Element;

        if (nodeStack.length > 1)
        {
          nodeStack[$ - 1].parent = nodeStack[$ - 2];
          nodeStack[$ - 2].children ~= nodeStack[$ - 1];
        }
        else
          root = nodeStack[0];
      }
      else if (state == State.Element && match("/>")) // End of standalone element?
      {
        nodeStack = nodeStack[0 .. $ - 1];
        state = State.Node;
      }
      else if (state == State.Element && match(">")) // End of element?
        state = State.Node;
      else if (state == State.Xml && parseAttribute(name, val)) // <?xml attribute name="value"?
        xmlDoc.attrs[name] = val;
      else if (state == State.Xml && match("?>"))
        state = State.Node;
      else if (state == State.Element && parseAttribute(name, val)) // Attribute name="value"?
        nodeStack[$ - 1].attrs[name] = val;
      else if (state == State.Node && parseContent(val)) // Node content?
      {
        if (!nodeStack.empty)
          nodeStack[$ - 1].content = val;
      }
      else if (!xmlstr.empty)
        error(format("Unexpected character '%s'", xmlstr[0]));
    }

    if (!nodeStack.empty)
      error("Unexpected EOF with unterminated XML elements");
  }

  XmlNode root;
  XmlNode xmlDoc;
}

class XmlNode
{
  this(dstring id, string parseFile = null, uint parseLine = 0)
  {
    this.id = id;
    this.parseFile = parseFile;
    this.parseLine = parseLine;
  }

  this(XmlNode node)
  {
    id = node.id;
    attrs = node.attrs.dup;
    content = node.content;
    parseFile = node.parseFile;
    parseLine = node.parseLine;
  }

  /// Node full name including its ancestry names separated by periods.
  @property dstring fullname()
  {
    dstring s;

    for (auto n = this; n; n = n.parent)
    {
      if (auto nameAttr = "name" in n.attrs)
        s = s.empty ? (n.id ~ "["d ~ *nameAttr ~ "]"d) : (n.id ~ "["d ~ *nameAttr ~ "]"d ~ "."d ~ s);
      else
        s = s.empty ? n.id : n.id ~ "." ~ s;
    }

    return s;
  }

  dstring opIndex(dstring key)
  {
    dstring retval;

    try
      retval = attrs[key];
    catch (RangeError e)
      throw new XmlAttrError(this, key);

    return retval;
  }

  dstring get(dstring key, dstring def = null)
  {
    return attrs.get(key, def);
  }

  /**
   * Find a child by name.
   * Params:
   *   childName = Name of child node to find
   * Returns: The first XmlNode child with childName or null if not found
   */
  XmlNode findChild(dstring childName)
  {
    foreach (child; children)
      if (child.id == childName)
        return child;

    return null;
  }

  /**
   * Find a child by name and throw XmlChildNotFoundError if not found.
   * Params:
   *   childName = Name of child node to find
   * Returns: The first XmlNode child with childName
   */
  XmlNode requireChild(dstring childName)
  {
    if (auto retval = findChild(childName))
      return retval;

    throw new XmlChildNotFoundError(this, childName);
  }

  /**
   * Add a child node (and sub tree) to a node.
   * Params:
   *   child = The child to add
   */
  void addChild(XmlNode child)
  {
    if (child.parent)
      child.unlink;

    children ~= child;
    child.parent = this;
  }

  /**
   * Unlink an XML node from its parent. Does nothing if node has no parent.
   */
  void unlink()
  {
    if (parent)
    {
      auto ndx = parent.children.countUntil!(x => x is this);
      assert(ndx >= 0);
      parent.children = parent.children[0 .. ndx] ~ parent.children[(ndx + 1) .. $];
      parent = null;
    }
  }

  /**
   * Replace a node with a new one. Does nothing if this node has no parent.
   */
  void replace(XmlNode newNode)
  {
    if (parent)
    {
      auto ndx = parent.children.countUntil!(x => x is this);
      assert(ndx >= 0);
      parent.children[ndx] = newNode;
      newNode.parent = parent;
      parent = null;
    }
  }

  /**
   * Perform a deep copy of an XmlNode and its children recursively.
   * Returns: New XmlNode which is a fully duplicated tree.
   */
  XmlNode deepcopy()
  {
    XmlNode root = new XmlNode(this);

    void recurseCopy(XmlNode oldNode, XmlNode newNode)
    {
      foreach (oldChild; oldNode.children)
      {
        auto newChild = new XmlNode(oldChild);
        newNode.addChild(newChild);
        recurseCopy(oldChild, newChild);
      }
    }

    recurseCopy(this, root);

    return root;
  }

  /**
   * Display a warning message with node location details.
   * Params:
   *   msg = The message
   */
  void warn(dstring msg)
  {
    warning(format("%s (node '%s' in file '%s' at line %s)",
        msg, fullname, parseFile, parseLine));
  }

  XmlNode parent; /// Parent XML node
  XmlNode[] children; /// Children nodes
  dstring id; /// XML ID of node
  dstring[dstring] attrs; /// Node attribute values
  dstring content; /// The text content between open and closing element

  string parseFile; /// Name of parsed XML file
  uint parseLine; /// Line number in original XML file that the node name was parsed on
}

class XmlParseError : Exception
{
  this(string msg, string file, size_t line, size_t index)
  {
    this.msg = msg;
    this.file = file;
    this.line = line;
    this.index = index;
    super(toString);
  }

  override string toString() const
  {
    return format("XmlParseError: %s (in file '%s', at line %s, index %s)",
        msg, file, line, index);
  }

  string file;
  size_t line;
  size_t index;
}

/// XML attribute lookup error. Used for catching missing required attributes.
class XmlAttrError : RangeError
{
  this(XmlNode node, dstring attrName)
  {
    super("Attribute " ~ attrName.to!string ~ " not found");
    this.node = node;
    this.attrName = attrName;
  }

  override string toString()
  {
    return format("XmlAttrError: %s (in node %s, in file '%s', line %s)",
        msg, node.fullname, node.parseFile, node.parseLine);
  }

  XmlNode node; /// The XML node
  dstring attrName; /// The attribute name
}

/// XML child node not found error. Used for catching missing child nodes.
class XmlChildNotFoundError : Exception
{
  this(XmlNode node, dstring childName)
  {
    super("Child node " ~ childName.to!string ~ " not found");
    this.node = node;
    this.childName = childName;
  }

  override string toString()
  {
    return format("XmlChildNotFoundError: %s (in node %s, in file '%s', line %s)",
        msg, node.fullname, node.parseFile, node.parseLine);
  }

  XmlNode node; /// The XML node
  dstring childName; /// The name of the child which is missing
}

/**
 * Check if a character is a whitespace character
 * Params:
 *   ch = Character to check
 * Returns: true if whitespace, false otherwise
 */
bool isWhite(dchar ch)
{
  return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r';
}

/**
 * Check if a unicode character is a valid XML element start character (we don't allow any unicode chars, contrary to the standard)
 * Params:
 *   ch = The character
 * Returns: true if valid start character, false otherwise
 */
bool isValidElemStartChar(dchar ch)
{
  return (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || ch == ':' || ch == '_';
}

/**
 * Check if a unicode character is a valid XML element start character (we don't allow any unicode chars, contrary to the standard)
 * Params:
 *   ch = The character
 * Returns: true if valid start character, false otherwise
 */
bool isValidElemChar(dchar ch)
{
  return isValidElemStartChar(ch) || ch == '-' || ch == '.' || (ch >= '0' && ch <= '9');
}

unittest
{
  dstring data = `<?xml version="1.0"?>
<repository version="1.2"
            xmlns="http://www.gtk.org/introspection/core/1.0"
            xmlns:c="http://www.gtk.org/introspection/c/1.0"
            xmlns:glib="http://www.gtk.org/introspection/glib/1.0">
  <package name="glib-2.0"/>
  <c:include name="glib.h"/>
  <namespace name="GLib"
             version="2.0"
             shared-library="libgobject-2.0.so.0,libglib-2.0.so.0"
             c:identifier-prefixes="G"
             c:symbol-prefixes="g,glib">
    <alias name="DateDay" c:type="GDateDay">
      <doc xml:space="preserve"
           filename="glib-2.0.c"
           line="289">Integer representing a day of the month; between 1 and 31.

The %G_DATE_BAD_DAY value represents an invalid day of the month.</doc>
      <source-position filename="gdate.h" line="52"/>
      <type name="guint8" c:type="guint8"/>
    </alias>
    <test>&amp; &lt; &gt; &apos; &quot; &#x31; &#50;</test>
  </namespace>
</repository>
`;

  auto tree = new XmlTree();
  tree.parse(data);

  assert(tree.xmlDoc.attrs["version"] == "1.0");
  assert(tree.root.id == "repository");
  assert(tree.root.attrs["version"] == "1.2");
  assert(tree.root.attrs["xmlns:glib"] == "http://www.gtk.org/introspection/glib/1.0");
  assert(tree.root.children[0].id == "package");
  assert(tree.root.children[0].attrs["name"] == "glib-2.0");
  assert(tree.root.children[2].id == "namespace");
  assert(tree.root.children[2].attrs["name"] == "GLib");
  assert(tree.root.children[2].children[1].id == "test");
  assert(tree.root.children[2].children[1].content == "& < > ' \" 1 2"d);

  dstring noAttrEqual = `<?xml version="1.0"?><repository version`;
  tree = new XmlTree();
  assertThrown(tree.parse(noAttrEqual));
}

// findChild - find child by name, return null for missing
unittest
{
  auto parent = new XmlNode("root"d);
  auto c1 = new XmlNode("alpha"d);
  auto c2 = new XmlNode("beta"d);
  parent.addChild(c1);
  parent.addChild(c2);

  assert(parent.findChild("alpha"d) is c1);
  assert(parent.findChild("beta"d) is c2);
  assert(parent.findChild("missing"d) is null);
}

// requireChild - return found child, throw XmlChildNotFoundError for missing
unittest
{
  auto parent = new XmlNode("root"d);
  auto child = new XmlNode("item"d);
  parent.addChild(child);

  assert(parent.requireChild("item"d) is child);
  assertThrown!XmlChildNotFoundError(parent.requireChild("nope"d));
}

// addChild - add child to node, verify parent and children updated
unittest
{
  auto parent = new XmlNode("root"d);
  auto child = new XmlNode("leaf"d);
  assert(child.parent is null);
  assert(parent.children.length == 0);

  parent.addChild(child);
  assert(parent.children.length == 1);
  assert(parent.children[0] is child);
  assert(child.parent is parent);
}

// unlink - remove child from parent
unittest
{
  auto parent = new XmlNode("root"d);
  auto c1 = new XmlNode("a"d);
  auto c2 = new XmlNode("b"d);
  parent.addChild(c1);
  parent.addChild(c2);

  c1.unlink;
  assert(c1.parent is null);
  assert(parent.children.length == 1);
  assert(parent.children[0] is c2);
}

// replace - replace child node with new one
unittest
{
  auto parent = new XmlNode("root"d);
  auto old = new XmlNode("old"d);
  auto replacement = new XmlNode("new"d);
  parent.addChild(old);

  old.replace(replacement);
  assert(old.parent is null);
  assert(replacement.parent is parent);
  assert(parent.children.length == 1);
  assert(parent.children[0] is replacement);
}

// deepcopy - deep copy is independent of original
unittest
{
  auto root = new XmlNode("root"d);
  auto child = new XmlNode("child"d);
  child.attrs["key"d] = "val"d;
  root.addChild(child);

  auto copy = root.deepcopy;
  assert(copy.id == "root"d);
  assert(copy.children.length == 1);
  assert(copy.children[0].attrs["key"d] == "val"d);

  // Modify original; copy should be unaffected
  child.attrs["key"d] = "changed"d;
  assert(copy.children[0].attrs["key"d] == "val"d);
}

// fullname - test fullname with nested nodes and name attributes
unittest
{
  auto a = new XmlNode("a"d);
  auto b = new XmlNode("b"d);
  b.attrs["name"d] = "myname"d;
  auto c = new XmlNode("c"d);
  a.addChild(b);
  b.addChild(c);

  assert(a.fullname == "a"d);
  assert(b.fullname == "a.b[myname]"d);
  assert(c.fullname == "a.b[myname].c"d);
}

// opIndex - attribute access, throw XmlAttrError for missing
unittest
{
  auto node = new XmlNode("elem"d);
  node.attrs["x"d] = "42"d;

  assert(node["x"d] == "42"d);
  assertThrown!XmlAttrError(node["missing"d]);
}

// get - get with default value for missing keys
unittest
{
  auto node = new XmlNode("elem"d);
  node.attrs["x"d] = "1"d;

  assert(node.get("x"d) == "1"d);
  assert(node.get("missing"d) is null);
  assert(node.get("missing"d, "default"d) == "default"d);
}

// XML comment parsing - comments are skipped
unittest
{
  auto tree = new XmlTree(`<root><!-- a comment --><child/></root>`d);
  assert(tree.root.id == "root"d);
  assert(tree.root.children.length == 1);
  assert(tree.root.children[0].id == "child"d);
}

// XML self-closing elements
unittest
{
  auto tree = new XmlTree(`<root><elem attr="val"/></root>`d);
  assert(tree.root.children[0].id == "elem"d);
  assert(tree.root.children[0].attrs["attr"d] == "val"d);
  assert(tree.root.children[0].children.length == 0);
}

// XML entity parsing - &amp; &lt; &gt; &apos; &quot;
unittest
{
  auto tree = new XmlTree(`<root>&amp; &lt; &gt; &apos; &quot;</root>`d);
  assert(tree.root.content == `& < > ' "`d);
}

// XmlParseError - malformed XML throws
unittest
{
  assertThrown!XmlParseError(new XmlTree("<root><unclosed>"d));
  assertThrown!XmlParseError(new XmlTree("<root>"d));
}
