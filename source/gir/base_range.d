module gir.base_range;

import gir.base;

/// Range for recursively iterating over Base object nodes
struct BaseRange
{
  this(Base root)
  {
    this.root = root;
    this.node = root;
  }

  @property bool empty() const
  {
    return node is null;
  }

  @property auto front()
  {
    return node;
  }

  void popFront()
  {
    if (node.children.length > 0) // Advance to first child if there are any
    {
      node = node.children[0];
      return;
    }
    else if (node.parent) // Find parent with a next child
    {
      for (; node && node !is root; node = node.parent)
      {
        if (node.childIndex + 1 < node.parent.children.length)
        {
          node = node.parent.children[node.childIndex + 1];
          return;
        }
      }
    }

    node = null; // Exhausted tree (should only happen on a single node tree)
  }

  BaseRange save()
  {
    return this;
  }

private:
  Base root;
  Base node;
}
