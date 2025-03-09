module xml_patch;

import std_includes;
import utils;
import xml_tree;

/**
 * XML patching class.
 *
 * Patch syntax
 *
 * XmlSelector:
 *   Provides a way of selecting one or more nodes or attributes
 *   Node IDs are separated by periods: repository.namespace
 *   "name" attributes can be matched in square brackets: function[my_function]
 *   Other attribute values can be matched with ATTR=VAL syntax: function[c:identifier=g_boxed_copy]
 *   Multiple attributes can be matched (logic AND) by seperating them with commas: function[my_function,version=1.22]
 *   When selecting an attribute it follows surrounded by a second set of square brackets: record[my_struct][opaque]
 *   If the selected node does not have any attribute selection criteria it can be empty brackets: namespace[][name]
 *
 *   Wildcards:
 *   To match a node anywhere in the XML doc use '*' as the first node ID component: *.array[ByteArray]
 *   Selectors select a single node/attribute unless wildcards are used
 *   A wildcard node selector is specified by prepending a '*' before the node ID: function[my_function].*parameter.
 *     NOTE: Node ID name wildcards are not used for matching the node ID name, but for indicating that multiple nodes can match.
 *       Pattern matching of node IDs is not supported currently.
 *   A wildcard attribute selector is followed by square brackets to specify the attribute: record[my_record].*parameter[introspectable]
 *   Wildcards can be specified in attribute values by including them in the value: function[new*]
 *
 * XmlTree:
 *   A verbatim XML tree.
 *
 * NOTE: The operator names (set, del, add, rename) are handled externally.
 *
 * Change attribute: set XmlSelector[Attr] AttrValue
 * Change node: set XmlSelector XmlTree
 * Delete attribute: del XmlSelector[Attr]
 * Delete node: del XmlSelector
 * Add node: add XmlSelector XmlTree
 * Rename node: rename XmlSelector new-id
 * Rename attribute: rename XmlSelector[Attr] new-name
 */
class XmlPatch
{
  this()
  {
  }

  /**
   * Parse a set (change) XML patch command.
   * Params:
   *   sel = XML node or attribute selector
   *   val = XML content for a node or attribute value for an attribute
   */
  void parseSetCmd(dstring sel, dstring val)
  {
    op = XmlPatchOp.Set;
    parseSelector(sel);

    if (selAttrId.empty) // Set XML node?
    {
      nodeValue = new XmlTree(val, "<VALUE>");

      if (!nodeValue.root)
        throw new XmlPatchError("Expected XML data but found empty value");
    }
    else // Set XML attribute value
      strValue = val;
  }

  /**
   * Parse a rename XML patch command.
   * Params:
   *   sel = XML node or attribute selector
   *   idName = Node ID or attribute name
   */
  void parseRenameCmd(dstring sel, dstring idName)
  {
    op = XmlPatchOp.Rename;
    parseSelector(sel);
    strValue = idName;
  }

  /**
   * Parse a delete XML patch command.
   * Params:
   *   sel = XML node or attribute selector
   */
  void parseDeleteCmd(dstring sel)
  {
    op = XmlPatchOp.Delete;
    parseSelector(sel);
  }

  /**
   * Parse an add XML node patch command.
   * Params:
   *   sel = XML node or attribute selector to add new node under
   *   xmlVal = The XML tree to parse and add
   */
  void parseAddCmd(dstring sel, dstring xmlVal)
  {
    op = XmlPatchOp.Add;
    parseSelector(sel);
    nodeValue = new XmlTree(xmlVal, "<VALUE>");

    if (!nodeValue.root)
      throw new XmlPatchError("Expected XML data but found empty value");
  }

  /**
   * Parse a patch specification into a patch object.
   * Params:
   *   patchSpec = The path command specification to parse
   */
  void parseSelector(dstring patchSpec)
  { // State machine enum for parts of the patch specification
    enum Part
    {
      NodeName, // XML node name
      AttrNameValOrId, // Attribute "name" value to match or another attribute ID if "=" is present
      AttrId, // Additional attribute ID (ID must be specified)
      AttrVal, // Attribute match value
      AttrEnd, // Attribute [] block has ended (waiting for end of patch spec, '.' node separator, or '[' attribute selector)
      AttrSelId, // Attribute select ID
      End, // Expect end of selector
    }

    Part part;
    ulong lastNdx;
    dstring attrId;
    bool nodeIsWild;

    foreach (i, c; patchSpec)
    {
      final switch (part) with (Part)
      {
        case NodeName: // Expect node name
          if (c == '*') // Wildcard node ID?
          {
            if (lastNdx != i)
              throw new XmlPatchError("Node wildcard '*' character only allowed before node ID");

            lastNdx = i + 1;
            nodeIsWild = true;
          }
          else if (c == '.' || c == '[')
          {
            if (i == lastNdx && !nodeIsWild)
              throw new XmlPatchError("Empty node ID");

            selectors.length++;
            selectors[$ - 1].isWild = nodeIsWild;
            selectors[$ - 1].id = patchSpec[lastNdx .. i];

            if (c == '[')
              part = AttrNameValOrId; // Next part will be a "name" attribute value to match or an attribute ID if '=' is found

            lastNdx = i + 1;
            nodeIsWild = false;
          }
          break;
        case AttrNameValOrId: // Expected "name" attribute match value or an attribute ID
          if (c == '=' || c == ',' || c == ']')
          {
            if (i > lastNdx) // Was string non-empty?
            {
              if (c == '=') // Is an attribute value specified?
              {
                attrId = patchSpec[lastNdx .. i]; // Store attribute ID until value is captured
                part = AttrVal;
              }
              else
              {
                selectors[$ - 1].attrs["name"] = patchSpec[lastNdx .. i]; // Parsed "name" attribute value, set it in the current selector
                part = c == ',' ? AttrId : AttrEnd;
              }

              lastNdx = i + 1;
            }
            else if (c != ']')
              throw new XmlPatchError("Unexpected empty attribute ID");
            else
              part = AttrEnd;
          }
          break;
        case AttrId: // Expected attribute ID
          if (c == '=')
          {
            attrId = patchSpec[lastNdx .. i];
            part = AttrVal;
            lastNdx = i + 1;
          }
          else if (c == ',' || c == ']')
            throw new XmlPatchError("Unexpected attribute ID without a value");
          break;
        case AttrVal: // Expect attribute value
          if (c == ',' || c == ']')
          {
            selectors[$ - 1].attrs[attrId] = patchSpec[lastNdx .. i]; // Parsed attribute value, set it in the current selector
            part = c == ',' ? AttrId : AttrEnd;
            lastNdx = i + 1;
          }
          break;
        case AttrEnd:
          if (c == '[')
            part = AttrSelId;
          else if (c == '.')
            part = NodeName;
          else
            throw new XmlPatchError("Unexpected character at end of attributes selector block");

          lastNdx = i + 1;
          break;
        case AttrSelId:
          if (c == ']')
          {
            selAttrId = patchSpec[lastNdx .. i];
            part = End;
          }
          break;
        case End:
          throw new XmlPatchError("Found unexpected character at end of selector");
      }
    }

    // Process last node ID?
    if (part == Part.NodeName && lastNdx < patchSpec.length)
    {
      selectors.length++;
      selectors[$ - 1].isWild = nodeIsWild;
      selectors[$ - 1].id = patchSpec[lastNdx .. $];
    }
  }

  /**
   * Apply patch to an XML tree.
   * Params:
   *   tree = XML tree
   *   defaultRoot = Default root node or null. If specified then it is used as a reference point if top selector node ID does not match the ID of the root of defaultRoot.
   */
  void apply(XmlTree tree, XmlNode defaultRoot)
  {
    auto nodes = select(tree, defaultRoot, op == XmlPatchOp.Rename); // Select the nodes (rename requires that selected attributes are set)

    if (nodes.length == 0)
      throw new XmlPatchError("XML patch selector '" ~ selectorString.to!string ~ "' did not match");

    foreach (curNode; nodes)
    {
      final switch (op) with (XmlPatchOp)
      {
        case Set:
          if (selAttrId.empty)
            curNode.replace(nodeValue.root.deepcopy); // Replace node with a new deep copy of new node tree
          else
            curNode.attrs[selAttrId] = strValue;
          break;
        case Rename:
          if (selAttrId)
          {
            curNode.attrs[strValue] = curNode.attrs[selAttrId];
            curNode.attrs.remove(selAttrId);
          }
          else
            curNode.id = strValue;
          break;
        case Delete:
          if (selAttrId.empty)
            curNode.unlink;
          else if (selAttrId in curNode.attrs)
            curNode.attrs.remove(selAttrId);
          break;
        case Add:
          curNode.addChild(nodeValue.root);
          break;
      }
    }
  }

  /**
   * Select nodes matching the patch selector.
   * Params:
   *   tree = XML tree
   *   defaultRoot = Default root node or null. If specified then it is used as a reference point if top selector node ID does not match the ID of the root of defaultRoot.
   *   selAttrSet = true if attributes specified by attribute selectors must be set
   * Returns: Array of matching nodes (empty if no matches found)
   */
  XmlNode[] select(XmlTree tree, XmlNode defaultRoot, bool selAttrSet)
  {
    XmlNode[] nodes;

    // Check if a XML node matches a selector
    bool matchSelector(XmlNode node, XmlSelector sel)
    {
      if (!sel.id.empty && node.id != sel.id) // Node ID is empty if wildcard
        return false;

      foreach (a; sel.attrs.byKeyValue)
      {
        if (a.value.canFind('*')) // Wildcard match?
        {
          if (a.key !in node.attrs || !(node[a.key]).matchWild(a.value))
            return false;
        }
        else if (node.get(a.key, "") != a.value)
          return false;
      }

      // Match if selAttrSet is false, there is no selAttrId, or the attribute is set in the node
      return !selAttrSet || selAttrId.empty || selAttrId in node.attrs;
    }

    // Function to recurse XmlNode tree searching for matches to selector criteria
    // Returns true to continue recursion, false if item was found and not wildcard
    void recurseTree(XmlNode node, XmlSelector[] selArray, bool wildNode = false)
    {
    selectorLoop:
      while (true)
      {
        auto sel = selArray[0];

        foreach (child; node.children)
        {
          if (!matchSelector(child, sel))
          {
            if (wildNode) // Recurse into non-matching nodes if we are within a wildcard node
              recurseTree(child, selArray, wildNode);

            continue;
          }

          if (selArray.length == 1) // Do we have a matching node?
          {
            nodes ~= child;

            if (dumpSelectorMatches)
              writeln("Selector '" ~ selectorString(0) ~ "' matched " ~ child.fullname);

            if (!sel.isWild) // First match ends a non-wildcard search
              return;
          }

          if (!sel.isWild) // Not wildcard?
          { // Advance to next selector and descend into matching child
            selArray = selArray[1 .. $];
            node = child;
            continue selectorLoop;
          }

          if (selArray.length > 1) // Recursively process wildcard
            recurseTree(child, selArray[1 .. $], wildNode || (sel.isWild && sel.id.empty)); // Set wildNode if it is already set or this is a wildcard node selector
        }

        break;
      }
    }

    if (!defaultRoot || selectors[0].id == tree.root.id) // No default root or selector matches root ID
    {
      if (matchSelector(tree.root, selectors[0])) // Does root selector match?
      {
        if (selectors.length == 1)
          nodes ~= tree.root;
        else
          recurseTree(tree.root, selectors[1 .. $]);
      }
    }
    else
      recurseTree(defaultRoot, selectors);

    return nodes;
  }

  /**
   * Get a selector string from a patch.
   * Params:
   *   count = Count of selector components to show (0 to show all which is the default)
   * Returns: Selector string
   */
  dstring selectorString(uint count = 0)
  {
    dstring s;

    foreach (sel; selectors)
    {
      if (!s.empty)
        s ~= ".";

      if (sel.isWild)
        s ~= "*";

      s ~= sel.id;

      if (sel.attrs.length > 0)
      {
        s ~= "[";

        if ("name" in sel.attrs)
          s ~= sel.attrs["name"];

        foreach (at; sel.attrs.byKeyValue)
        {
          if (at.key != "name")
          {
            if (s[$ - 1] != '[')
              s ~= ",";

            s ~= at.key ~ "=" ~ at.value;
          }
        }

        s ~= "]";
      }

      if (count != 0)
      {
        count--;

        if (count == 0)
          break;
      }
    }

    return s;
  }

  XmlPatchOp op; /// Patch operation
  XmlSelector[] selectors; /// Selection path
  dstring selAttrId; /// The selected attribute of the node matched by selectors (null if selecting a node)
  dstring strValue; /// Attribute value (if op == Set and selAttrId is set), attribute name (if op == Rename and selAttrId), or node ID (if op == Rename and !selAttrId)
  XmlTree nodeValue; /// Node value (parsed XML tree) or null

  static bool dumpSelectorMatches; /// Enables dumping of XML patch selection matches
}

/// XML selector. All criteria is logically ANDed.
struct XmlSelector
{
  dstring id; /// Node id to match
  dstring[dstring] attrs; /// Attribute values to match (values can contain wildcard string characters)
  bool isWild; /// Wildcard ID
}

/// XML patch operation
enum XmlPatchOp : ubyte
{
  Set, /// Set a node (replace) or attribute
  Rename, /// Rename a node or attribute
  Delete, /// Delete a node or attribute
  Add, /// Add a node
}

/// A patch exception
class XmlPatchError : Exception
{
  this(string msg)
  {
    super(msg);
  }
}
