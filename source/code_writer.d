module code_writer;

import std.algorithm : cmp;
import std.file : fileRead = read, fileWrite = write;
import std_includes;

import line_tracker;

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

  CodeWriter opOpAssign(string op)(dstring[] rhs, string file = __FILE__, size_t line = __LINE__) if (op == "~")
  {
    if (LineTracker.enable)
    {
      auto key = SourceInfo(file, cast(uint)line);

      foreach (i; 1 .. rhs.length + 1)
      {
        auto writeLine = new SourceInfo(fileName, cast(uint)(lines.length + i));
        sourceLineMap[key] ~= writeLine;
        lineTrackerLines ~= writeLine; // Store pointer to output file info to update it on insert
      }
    }

    lines ~= rhs;

    return this;
  }

  CodeWriter opOpAssign(string op)(dstring rhs, string file = __FILE__, size_t line = __LINE__) if (op == "~")
  { // Remove end newline to have identical behavior of raw text append
    if (rhs.endsWith('\n'))
      rhs = rhs[0 .. $ - 1];

    if (rhs == "")
      return this.opOpAssign!"~"([""], file, line);

    return this.opOpAssign!"~"(rhs.splitLines, file, line);
  }

  CodeWriter opOpAssign(string op)(LineTracker tracker) if (op == "~")
  {
    foreach(info; tracker.lines)
      this.opOpAssign!"~"(info.line, info.file, info.lineNumber);

    return this;
  }

  /**
   * Get the length (in lines) of the code writer data.
   * Returns: Number of lines stored
   */
  uint length()
  {
    return cast(uint)lines.length;
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

    if (LineTracker.enable) // Update line tracker output lines if they come after the insert
      foreach (pWriteLine; lineTrackerLines)
        if (pWriteLine.line > pos)
          pWriteLine.line += ins.length;
  }

  /**
   * Return the contents of the last line.
   * Returns: The last line or null if no lines have been added
   */
  dstring lastLine()
  {
    return lines.length > 0 ? lines[$ - 1] : null;
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

  struct SourceInfo
  {
    string file;
    uint line;

    int opCmp(ref const SourceInfo other) const
    {
      auto fileCmp = cmp(file, other.file);
      if (fileCmp != 0) return fileCmp;
      return cast(int)(line - other.line);
    }
  }

  static SourceInfo*[][SourceInfo] sourceLineMap; // Source {file, line} -> Written {fileName, index}[]

private:
  string fileName; // Name of file the buffer will be written to
  bool inComment; // True if inside a multi-line comment
  dstring[] lines; // Array of lines
  SourceInfo*[] lineTrackerLines; // Pointer array of line tracker lines for this CodeWriter stored to sourceLineMap (to update lines on insert)
}
