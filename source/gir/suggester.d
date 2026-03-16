module gir.suggester;

import std.algorithm : among, canFind, countUntil, filter, joiner, map;
import std.array : assocArray;
import std.conv : to;
import std.exception : ifThrown;
import std.file : read;
import std.path : buildPath;
import std.string : splitLines, startsWith;
import std.typecons : tuple;

import defs;
import gir.base;
import gir.func;
import gir.param;
import gir.repo;
import gir.return_value;
import gir.structure;
import gir.type_node;
import utils : countStars;

immutable string[string] SuggestionDescriptions = [
  "string-array-null-term" : "Null terminated string arrays",
  "string-length-param" : "Strings with array length parameters",
  "basic-direction-out" : "Output basic type parameters",
  "caller-allocated-out-array": "Caller allocated output array",
];

class Suggester
{
  enum BlacklistFileName = "suggestion_blacklist.txt";

  this(Defs defs)
  { // Generate set of blacklisted suggestions
    blacklistRules = defs.defPaths.map!(path => buildPath(path, BlacklistFileName).read.ifThrown(cast(void[])[])
      .to!string.splitLines.filter!(x => x.startsWith("//!"))).joiner.map!(x => tuple(x, true)).assocArray;

    outer: foreach (node; defs.walk)
    {
      auto typeNode = cast(TypeNode)node;

      for (auto n = node; n; n = n.parent)
        if (n.active == Active.Ignored || n.active == Active.Disabled)
          continue outer;

      // Zero terminate string arrays
      if (typeNode && typeNode.containerType == ContainerType.Array && typeNode.lengthParamIndex == ArrayLengthUnset
          && typeNode.fixedSize == ArrayNotFixed && !typeNode.zeroTerminated
          && typeNode.elemTypes.length > 0 && typeNode.elemTypes[0].kind == TypeKind.String)
        addSug(typeNode.repo, "string-array-null-term", "//!set " ~ typeNode.xmlSelector.to!string
          ~ (cast(ReturnValue)typeNode ? ".return-value.array[][zero-terminated] 1" : ".array[][zero-terminated] 1"));

      if (auto param = cast(Param)node)
        paramSugs(param);
    }
  }

  private void addSug(Repo repo, string name, string rule)
  {
    if (rule !in blacklistRules)
      suggestions[repo][name] ~= rule;
  }

  private void paramSugs(Param param)
  {
    auto func = param.getParentByType!Func;

    // String array length
    if (param.direction == ParamDirection.In && param.kind == TypeKind.String)
    {
      foreach (lenParam; [cast(Param)param.next, cast(Param)param.prev].filter!(x => x !is null))
      {
        if (lenParam.kind == TypeKind.Basic
            && lenParam.direction == ParamDirection.In
            && lenParam.lengthArrayParams.length == 0
            && lenParam.dType.among("int"d, "uint"d, "size_t"d, "ptrdiff_t"d)
            && ["len", "size", "count", "n_"].canFind!(x => lenParam.name.canFind(x)))
        {
          auto lengthIndex = func.params.countUntil(lenParam);

          if (func.hasInstanceParam)
            lengthIndex--;

          addSug(param.repo, "string-length-param", "//!set " ~ param.xmlSelector.to!string
            ~ `.type '<array length="` ~ lengthIndex.to!string ~ `" c:type="` ~ param.origCType.to!string
            ~ `"><type name="char" c:type="char"/></array>'`);
          break;
        }
      }
    }

    // Basic parameters direction=out
    //
    // DISABLED: This heuristic has too many false positives. The assumption that a pointer
    // to a basic type (e.g., int*, uint*) with direction=in should be direction=out is often
    // incorrect. Many C APIs legitimately pass pointers to basic types as input parameters
    // (e.g., for efficiency or to pass optional values). Without more context from the GIR
    // metadata or documentation, we cannot reliably distinguish between:
    //   - True output parameters (caller allocates, callee fills)
    //   - Input pointers to values (caller provides data)
    //   - Arrays without proper array annotation
    //
    // To enable this suggestion for specific cases, users should manually add the appropriate
    // //!set directives in their definition files.
    //
    // with (TypeKind) if (param.containerType == ContainerType.None
    //     && param.kind.among(Basic, BasicAlias, Enum, Flags) && param.direction == ParamDirection.In
    //     && param.cType.countStars > 0 && param.cType != "void*" && param.cType != "const(void)*")
    //   addSug(param.repo, "basic-direction-out", "//!set " ~ param.xmlSelector.to!string ~ "[direction] out");

    // Caller allocated output array
    with (TypeKind) if (param.containerType == ContainerType.Array && param.direction == ParamDirection.Out
        && !param.callerAllocates && param.cType.countStars < 2)
      addSug(param.repo, "caller-allocated-out-array", "//!set " ~ param.xmlSelector.to!string
        ~ "[caller-allocates] 1");
  }

  bool[string] blacklistRules; /// Set of rules to blacklist (keyed by the rule string content)
  string[][string][Repo] suggestions; /// Suggested definitions keyed by type of suggestion description ID and then by Repo
}
