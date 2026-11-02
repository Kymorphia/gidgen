/// Minimal text templates: "${name}" placeholders are replaced by values from an associative array
module tmpl;

import std.array : appender;
import std.exception : enforce;
import std.string : indexOf;

/**
 * Fill a template.
 * Params:
 *   text = Template text with ${name} placeholders
 *   vars = Placeholder values
 * Returns: The filled text (an unknown placeholder throws an exception)
 */
string fill(string text, const string[string] vars)
{
  auto result = appender!string;

  while (true)
  {
    auto start = text.indexOf("${");
    if (start < 0)
      break;

    auto end = text[start .. $].indexOf('}');
    enforce(end > 0, "Unterminated placeholder in template: " ~ text);
    auto name = text[start + 2 .. start + end];
    auto val = name in vars;
    enforce(val, "Unknown template placeholder '" ~ name ~ "' in: " ~ text);

    result ~= text[0 .. start];
    result ~= *val;
    text = text[start + end + 1 .. $];
  }

  result ~= text;
  return result[];
}

unittest
{
  assert(fill("a ${x} b ${y}", ["x": "1", "y": "2"]) == "a 1 b 2");
}
