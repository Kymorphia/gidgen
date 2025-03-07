module utils;

import core.stdc.signal : raise;
public import std.experimental.logger;

public import code_traps;
public import std_includes;

static this()
{
  sharedLog(cast(shared Logger)new GidLogger);
}

/// Create our own logger to strip out some of the extra info
class GidLogger : Logger
{
  this(LogLevel lv = LogLevel.all) @safe
  {
    super(lv);
  }

  override void writeLogMsg(ref LogEntry entry)
  {
    if (entry.logLevel == LogLevel.info)
      writeln(entry.msg);
    else
      writeln(entry.logLevel.to!string.capitalize ~ ": " ~ entry.msg);
  }
}

void breakpoint()
{
  version (Windows) // FIXME
  {
  }
  else
  {
    import core.sys.posix.signal : SIGTRAP;
    raise(SIGTRAP);
  }
}

/**
 * Normalize a D type by removing any trailing "_t" and converting to SnakeCase.
 * Params:
 *   typeStr = The D type string to normalize
 * Returns: Normalized D type string
 */
dstring normalizeDTypeName(dstring typeStr)
{
  return (typeStr.endsWith("_t") ? typeStr[0 .. $ - 2] : typeStr).camelCase(true);
}

/**
 * Changes snake or kabob case to camelCase.
 * Params:
 *   snakeCase = snake_case string
 *   firstUpper = set to true to make first character uppercase, defaults to lowercase
 * Returns: camelCase string
 */
dstring camelCase(dstring snakeCase, bool firstUpper = false)
{
  dstring camelStr;
  bool upperNext = firstUpper;
  ulong lastPos;

  foreach (i, c; snakeCase)
  {
    if (c == '_' || c == '-')
    {
      upperNext = true;
      camelStr ~= snakeCase[lastPos .. i];
      lastPos = i + 1;
    }
    else if (upperNext)
    {
      upperNext = false;
      camelStr ~= c.toUpper;
      lastPos = i + 1;
    }
  }

  if (lastPos == 0)
    return snakeCase;

  if (lastPos < snakeCase.length)
    camelStr ~= snakeCase[lastPos .. $];

  return camelStr;
}

unittest
{
  assert("i_am_snake".camelCase.equal("iAmSnake"));
  assert("i_am__snake".camelCase.equal("iAmSnake"));
  assert("_i_am__snake_".camelCase.equal("IAmSnake"));
  assert("i_am__snake".camelCase(true).equal("IAmSnake"));
  assert("i-am-kabob".camelCase.equal("iAmKabob"));
  assert("i-am--kabob".camelCase.equal("iAmKabob"));
  assert("-i-am--kabob-".camelCase.equal("IAmKabob"));
  assert("i-am--kabob".camelCase(true).equal("IAmKabob"));
  assert("i_am_snake-kabob".camelCase.equal("iAmSnakeKabob"));
  assert("blah".camelCase().equal("blah"));
  assert("blah".camelCase(true).equal("Blah"));
}

/**
 * Convert camelCase or TitleCase to snake_case
 * Returns: The name converted to snake_case
 */
dstring toSnakeCase(dstring name)
{
  import std.array : appender;
  import std.uni : isUpper, toLower;
  auto ap = appender!dstring;
  bool lastIsUp;

  foreach (i, c; name)
  {
    bool isUp = c.isUpper;

    if (i > 0 && !lastIsUp && isUp)
      ap ~= "_";

    ap ~= c.toLower;
    lastIsUp = isUp;
  }

  return ap[];
}

/**
 * Tokenize a type string into words and '*' pointers
 * Params:
 *   type = The type string
 * Returns: 
 */
dstring[] tokenizeType(dstring type)
{
  dstring[] tokens;
  ulong lastStart;

  foreach (i, c; type)
  {
    if (c.among('*', ' ', '(', ')'))
    {
      if (lastStart < i)
        tokens ~= type[lastStart .. i];

      if (c != ' ')
        tokens ~= [c];

      lastStart = i + 1;
    }
  }

  if (lastStart < type.length)
    tokens ~= type[lastStart .. $];

  return tokens;
}

unittest
{
  assert(" const  char  * ".tokenizeType.equal(["const"d, "char", "*"]));
  assert(" const  (char)  *  *".tokenizeType.equal(["const"d, "(", "char", ")", "*", "*"]));
  assert("const char**".tokenizeType.equal(["const"d, "char", "*", "*"]));
  assert("const char".tokenizeType.equal(["const"d, "char"]));
}

/**
 * Strip "const" and parenthesis from a type.
 * Params:
 *   type = The type string
 * Returns: Type string with all "const" and parenthesis stripped out
 */
dstring stripConst(dstring type)
{
  if (!type.startsWith("const")) // Optimization
    return type;

  return tokenizeType(type).filter!(x => !x.among("const"d, "("d, ")"d)).join;
}

/**
 * Strip "const", parenthesis, and pointer '*' from a type.
 * Params:
 *   type = The type string
 * Returns: Type string with all "const" and parenthesis stripped out
 */
dstring stripConstPtr(dstring type)
{
  return tokenizeType(type).filter!(x => !x.among("const"d, "("d, ")"d, "*"d)).join;
}

/**
 * Split a string by whitespace with double quoted substring support.
 * Params:
 *   s = The string to split
 * Returns: A splitter range of the tokenized strings
 */
auto splitQuoted(dstring s)
{
  dchar quoteChar = 0;

  bool doSplit(dchar c)
  {
    if (!quoteChar)
    {
      if (c == '"' || c == '\'')
      {
        quoteChar = c;
        return true;
      }

      return c.isWhite;
    }
    else if (c == quoteChar)
    {
      quoteChar = 0;
      return true;
    }

    return false;
  }

  return splitter!(doSplit)(s).filter!(x => !x.empty);
}

unittest
{
  assert(splitQuoted("a b c  d").equal(["a"d, "b", "c", "d"]));
  assert(splitQuoted("a \"double quoted value\"\t b c  d").equal([
    "a"d, "double quoted value", "b", "c", "d"
  ]));
  assert(splitQuoted("a 'single quoted value'\t b c  d").equal([
    "a"d, "single quoted value", "b", "c", "d"
  ]));
  assert(splitQuoted("a'single quoted value'\tb c  d").equal([
    "a"d, "single quoted value", "b", "c", "d"
  ]));
}

/**
 * Check if a string matches a given pattern with * wildcards.
 * Params:
 *   str = The string to match
 *   pattern = The pattern with * wildcard matching
 * Returns: true if str matches pattern, false otherwise.
 */
bool matchWild(dstring str, dstring pattern)
{
  dstring[] matches = pattern.split('*');

  if (matches.length <= 1) // No wildcards?
    return str == pattern;

  if (!matches[0].empty) // Pattern does not start with wildcard?
  {
    if (!str.startsWith(matches[0]))
      return false;

    str = str[matches[1].length .. $];
    matches = matches[1 .. $];
  }

  while (matches.length > 0)
  {
    if (matches.length == 1 && matches[0].empty) // Wildcard at end of pattern?
      return true;

    auto ndx = str.countUntil(matches[0]);

    if (ndx == -1)
      return false;

    str = str[ndx + matches[0].length .. $];
    matches = matches[1 .. $];
  }

  return str.empty;
}

unittest
{
  assert(matchWild("Hello Beautiful World", "Hello Beautiful World"));
  assert(matchWild("Hello Beautiful World", "Hello*"));
  assert(matchWild("Hello Beautiful World", "*World"));
  assert(matchWild("Hello Beautiful World", "Hello*World"));
  assert(matchWild("Hello Beautiful World", "*Beautiful*"));
  assert(matchWild("Hello Beautiful World", "*Hello*World*"));

  assert(!matchWild("Hello Beautiful World", "Hello"));
  assert(!matchWild("Hello Beautiful World", "*Hello"));
  assert(!matchWild("Hello Beautiful World", "Help*"));
  assert(!matchWild("Hello Beautiful World", "Hello*Full"));
}

/**
 * Count the number of stars at the end of a type string.
 * Params:
 *   type = The type string
 * Returns: Number of stars '*' on the end of the string
 */
int countStars(dstring type)
{
  auto count = type.retro
    .filter!(x => x != ')')
    .countUntil!(x => x != '*');
  return count > 0 ? cast(int)count : 0;
}

import std.range : isInputRange, ElementType;

/**
 * Template to get the front item of a range or a default value if empty.
 * Params:
 *   r = The range
 *   def = The default value
 * Returns: The range front value or def if empty
 */
auto frontIfNotEmpty(R, T)(R r, T def)
  if (isInputRange!R && is(ElementType!R == T))
{
  if (r.empty)
    return def;
  else
    return r.front;
}

/**
 * Escape parenthesis which aren't a part of a reference link of the form [text](url).
 * Params:
 *   s = The string
 * Returns: The string with any wayward non reference link parenthesis escaped with backslashes.
 */
dstring escapeNonRefLinkParens(dstring s)
{
  enum State
  {
    Normal, // Not within a potential ref link
    OpenBracket, // Found a start square bracket
    EndBracket, // Found an end square bracket
    OpenParen, // Found start open parenthesis
  }

  State state;
  dstring escaped;
  dstring possibleRefLink;
  int parenCount;

  with (State) while (s.length > 0) // Loop over characters in string
  {
    auto c = s[0];
    s = s[1 .. $];

    if (c == '[') // Open bracket?
    {
      if (state == Normal)
        state = OpenBracket;

      escaped ~= c;
    }
    else if (c == ']') // Close bracket?
    {
      if (state == OpenBracket)
        state = EndBracket;

      escaped ~= c;
    }
    else if (c == '(') // Open parenthesis?
    {
      if (state == EndBracket)
      {
        state = OpenParen;
        possibleRefLink.length = 0;
        parenCount = 0;
      }
      else // Embedded open parenthesis or open parenthesis not part of a ref link
      {
        if (state == OpenParen) // Count embedded parenthesis, which are escaped
          parenCount++;

        escaped ~= "$(LPAREN)";
      }
    }
    else if (c == ')') // Close parenthesis?
    {
      if (state == OpenParen && parenCount == 0) // The closing parenthesis of a ref link?
      {
        state = Normal;
        escaped ~= "(" ~ possibleRefLink ~ ")";
      }
      else // Close parenthesis for embedded parenthesis pair or a close parenthesis outside of a ref link
      {
        if (state == OpenParen)
          parenCount--;

        escaped ~= "$(RPAREN)";
      }
    }
    else if (c == '\n') // Newline character?
    {
      if (state == OpenParen) // A newline character cancels any possible ref link
        escaped ~= "$(LPAREN)" ~ possibleRefLink;

      escaped ~= '\n';
      state = Normal;
    }
    else // Another character
    {
      if (state == OpenParen)
        possibleRefLink ~= c;
      else
        escaped ~= c;
    }
  }

  if (state == State.OpenParen) // Add any partial potential ref link
    escaped ~= "$(LPAREN)" ~ possibleRefLink;

  return escaped;
}

unittest
{
  dstring testText = r"This is (stuff) with various ) parenthesis ( and a [link description](http://url) and blah()"
    ~ r" with a second [blah blah](https://url.yay) etc";
  dstring escText = r"This is \(stuff\) with various \) parenthesis \( and a [link description](http://url) and blah\(\)"
    ~ r" with a second [blah blah](https://url.yay) etc";
  assert(escapeNonRefLinkParens(testText) == escText);
}

enum MaxListStartIndent = 4;

/**
 * Convert a markdown formatted list (dashes only), with a $(LIST) DDoc adrdox list.
 * Params:
 *   docs = The doc content
 * Returns: The doc content with any markdown lists replaced with a $(LIST)
 */
dstring markdownListToDDoc(dstring docs)
{
  auto lines = docs.split("\n");
  dstring[] retLines;
  uint listIndent;
  dchar listChar = 0;

  outer: foreach (line; lines)
  {
    if (listChar != 0) // In list?
    {
      foreach (i, c; line)
      {
        if (c == listChar) // Another list item?
        {
          retLines ~= (cast(dchar)' ').repeat(listIndent + 2).to!dstring ~ line[0 .. i] ~ "*" ~ line[i + 1 .. $];
          continue outer;
        }

        if (c != ' ' && c != '\t') // Not a space character?  End of list
        {
          listChar = 0;

          auto ndx = cast(int)retLines.length - 1;
          for (; ndx >= 0; ndx--) // Find last non-blank line to insert list close parenthesis after
            if (retLines[ndx].strip.length != 0)
              break;

          retLines = retLines[0 .. ndx + 1] ~ [(cast(dchar)' ').repeat(listIndent).to!dstring ~ ")"]
            ~ retLines[ndx + 1 .. $];
          break;
        }
        else if (i > listIndent)
          break;
      }
    }
    else // Not in list
    {
      foreach (i, c; line) // Loop over characters of line looking for list item char
      {
        if (i > MaxListStartIndent)
          break;

        if (c == '-' || c == '*') // Dash or star are valid
        { // Next char must be space or tab
          if (i + 1 < line.length && (line[i + 1] == ' ' || line[i + 1] == '\t'))
          {
            listChar = c;
            listIndent = cast(uint)i;
            retLines ~= (cast(dchar)' ').repeat(listIndent).to!dstring ~ "$(LIST";
            retLines ~= (cast(dchar)' ').repeat(listIndent + 2).to!dstring ~ line[0 .. i] ~ "*" ~ line[i + 1 .. $];
            continue outer;
          }
        }
        else if (c != ' ' && c != '\t') // If character isn't whitespace, then can't be a list item
          break;
      }
    }

    retLines ~= (listChar != 0 ? (cast(dchar)' ').repeat(listIndent + 2).to!dstring : "") ~ line; // Add additional indent for list lines
  }

  if (listChar != 0)
    retLines ~= (cast(dchar)' ').repeat(listIndent).to!dstring ~ ")";

  return retLines.join("\n");
}
