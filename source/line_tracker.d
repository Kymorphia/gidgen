module line_tracker;

import std.algorithm : endsWith;
import std.string : splitLines;

class LineTracker
{
  LineTracker opOpAssign(string op)(dstring[] lineArray, string file = __FILE__, size_t lineNumber = __LINE__) if (op == "~")
  {
    if (enable)
      foreach(l; lineArray)
        lines ~= LineInfo(l, file, cast(uint)lineNumber);
    else
      foreach(l; lineArray)
        lines ~= LineInfo(l);

    return this;
  }

  LineTracker opOpAssign(string op)(dstring line, string file = __FILE__, size_t lineNumber = __LINE__) if (op == "~")
  { // Remove end newline to have identical behavior of raw text append
    if (line.endsWith('\n'))
      line = line[0 .. $ - 1];

    if (line == "")
      return this.opOpAssign!"~"([""], file, lineNumber);

    return this.opOpAssign!"~"(line.splitLines, file, lineNumber);
  }

  LineTracker opOpAssign(string op)(LineTracker tracker) if (op == "~")
  {
    lines ~= tracker.lines;
    return this;
  }

  uint length()
  {
    return cast(uint)lines.length;
  }

  static bool enable; /// To enable tracking of source code lines

  struct LineInfo
  {
    dstring line;
    string file;
    uint lineNumber;
  }

  LineInfo[] lines;
}
