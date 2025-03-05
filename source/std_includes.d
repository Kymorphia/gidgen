module std_includes;

public import core.exception : RangeError;
public import std.algorithm : among, canFind, count, countUntil, each, endsWith, equal, find, filter, fold,
  map, max, remove, sort, splitter, startsWith, uniq;
public import std.array : appender, array, assocArray, byPair, replace, replaceFirst, replicate, split;
public import std.conv : ConvException, to;
public import std.exception : assertThrown;
public import std.file : exists, dirEntries, isFile, mkdirRecurse, readText, SpanMode, write;
public import std.format : format;
public import std.path : baseName, buildPath, dirName, stripExtension;
public import std.range : chain, drop, dropOne, empty, enumerate, front, iota, join, repeat, retro, tee, walkLength;
public import std.regex : Captures, ctRegex, matchAll, matchFirst, replaceAll;
public import std.stdio : stderr, writeln;
public import std.string : capitalize, chomp, splitLines, toLower, toUpper, strip, stripLeft, stripRight;
public import std.traits : EnumMembers;
public import std.typecons : BitFlags, Flag, No, tuple, Yes;
public import std.uni : isWhite;
