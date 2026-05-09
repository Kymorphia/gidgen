module code_traps;

import std.regex;
import utils;

/**
 * Code traps provide a means to emit GDB breakpoints based on matching runtime action criteria.
 * These are currently used for breaking on code binding write operations for debugging the binding generation code.
 * Actions have a domain name, such as "ctypes.struct" for the writing of structures to a binding c/types.d file.
 * A regular expression match string is then used to match the object of the action, in this case the structure full D name.
 */

// Map of code traps Domain => Matches[]
private Regex!char[][string] codeTraps;
bool codeTrapsDump;

/**
 * Add a code trap.
 *
 * The match pattern is automatically anchored to the end of the string by appending '$'.
 * This ensures that patterns like "Foo" only match names ending with "Foo" (e.g., "BarFoo")
 * rather than names containing "Foo" anywhere (e.g., "FooBar").
 *
 * To match anywhere in the name, use ".*pattern.*" syntax.
 * To match only the exact name, use "^pattern" (the '$' is added automatically).
 *
 * Params:
 *   action = The action domain name
 *   match = A regular expression to match against the action object name (anchored to end)
 */
void addCodeTrap(string action, string match)
{
  // Anchor pattern to end of string for more predictable matching behavior.
  // This prevents partial matches like "Foo" matching "FooBar" when "BarFoo" was intended.
  codeTraps.require(action, cast(Regex!char[])[]) ~= regex(match ~ "$");
}

/**
 * Identify a code trap action and emit a GDB breakpoint signal if it matches any match criteria.
 * Params:
 *   action = The action domain name
 *   name = The action object name
 */
void codeTrap(string action, lazy dstring name)
{
  if (codeTraps.length == 0 && !codeTrapsDump)
    return;

  auto matchList = codeTraps.get(action, null);

  if (codeTrapsDump)
    writeln("TRAP: ", action, ":", name.to!string);

  if (!matchList)
    return;

  auto nameStr = name.to!string; // Evaluate lazy argument once here and convert it to string

  foreach (i; 0 .. matchList.length)
  {
    if (!nameStr.matchFirst(matchList[i]).empty)
    {
      writeln("TRAP: ", action, ":", nameStr);
      matchList = matchList.remove(i);
      breakpoint;
      return;
    }
  }
}
