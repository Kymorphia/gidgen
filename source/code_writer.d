module code_writer;

import std.file : fileRead = read, fileWrite = write;
import std_includes;

/**
 * Code writer object.
 * Automatically indents D code.
 * Provides some file update build optimizations (only writes existing files if changed).
 */
class CodeWriter
{
  this(string fileName, dstring[] content = [])
  {
    this.fileName = fileName;
    lines = content;
  }

  this(string fileName, dstring content)
  {
    this(fileName, content.splitLines);
  }

  CodeWriter opOpAssign(string op)(dstring[] rhs) if (op == "~")
  {
    lines ~= rhs;
    return this;
  }

  CodeWriter opOpAssign(string op)(dstring rhs) if (op == "~")
  { // Remove end newline to have identical behavior of raw text append
    if (rhs.endsWith('\n'))
      rhs = rhs[0 .. $ - 1];

    if (rhs == "")
      return this ~= [""];

    return this ~= rhs.splitLines;
  }

  /**
   * Insert a single line into a code writer.
   * Params:
   *   pos = Position to insert at in the line buffer (< 0 or >= line buffer length to append)
   *   ins = Line to insert
   */
  void insert(int pos, dstring ins)
  {
    insert(pos, [ins]);
  }

  /**
   * Insert lines into a code writer.
   * Params:
   *   pos = Position to insert at in the line buffer (< 0 or >= line buffer length to append)
   *   ins = Slice of lines to insert
   */
  void insert(int pos, dstring[] ins)
  {
    if (pos < 0 || pos > lines.length)
      pos = cast(int)lines.length;

    if (pos < lines.length)
      lines = lines[0 .. pos] ~ ins ~ lines[pos .. $];
    else
      lines = lines[0 .. pos] ~ ins;
  }

  void write()
  {
    if (!exists(fileName.dirName()))
      mkdirRecurse(fileName.dirName());

    dstring content;
    uint indent;
    bool indentStatement; // Used for indenting a single line after a control statement without a open/close brace block

    foreach (line; lines)
    {
      if (!inComment)
      {
        if (line.startsWith("/*")) // Start of multi-line comment?
          inComment = true;
        else if (line.startsWith('}') && indent >= 2) // End brace for a control block?
          indent -= 2;
        else if (line.startsWith('{'))
          indentStatement = false; // Don't add single statement indent if indent statement is followed by an open brace

        auto calcIndent = indent + (indentStatement ? 2 : 0); // Indent statements without braces
        auto stripLine = line.strip;

        if (stripLine.length > 0)
          content ~= (cast(dchar)' ').repeat(calcIndent).array ~ stripLine ~ "\n"d;
        else
          content ~= "\n";
      }
      else
        content ~= (cast(dchar)' ').repeat(indent).array ~ line ~ "\n";

      indentStatement = false;

      if (inComment)
      {
        if (line.endsWith("*/")) // End of multi-line comment?
          inComment = false;
      }
      else if (line.startsWith('{')) // Open brace for a control block increases indent
        indent += 2;
      else if (["else", "for ", "foreach ", "if ", "static if ", "version(", "while "].filter!(x => line.startsWith(x))
          .empty != true)
        indentStatement = true;
    }

    string strContent = content.to!string;

    // Only write file if it has changed, to optimize builds
    if (!exists(fileName) || fileRead(fileName) != strContent)
      fileWrite(fileName, strContent);
  }

  dstring[] lines; // Array of lines

private:
  string fileName; // Name of file the buffer will be written to
  bool inComment; // True if inside a multi-line comment
}
