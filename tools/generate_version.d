#!/usr/bin/env dub

/+ dub.sdl:
    name "generate_version"
+/

module gidgen.tools.generate_version;

import std.exception : enforce;
import std.file : exists, readText, mkdirRecurse, write;
import std.format : format;
import std.path : buildNormalizedPath, dirName;
import std.process : execute;
import std.stdio : stderr, writeln;
import std.string : strip;

enum rootDirectory = __FILE_FULL_PATH__.dirName.buildNormalizedPath("..");
enum outputFile = rootDirectory.buildNormalizedPath("source", "version_info.d");

void main()
{
  outputFile.dirName.mkdirRecurse();
  outputFile.updateIfChanged(generateModule);
}

string generateModule()
{
  string versionStr = "unknown-version";

  try
  {
    auto result = execute(["git", "-C", rootDirectory, "describe", "--tags", "--dirty", "--always"]);
    enforce(result.status == 0, "Failed to run 'git describe', ensure Git is installed and repo is initialized.");
    versionStr = result.output.strip;
  }
  catch (Exception e)
    stderr.writeln("Failed to generate git repo package version: ", e.message);

  return q"EOS
module version_info;

enum packageVersion = "%s";
EOS".format(versionStr);
}

void updateIfChanged(string path, string content)
{
  if (!path.exists || readText(path) != content)
    write(path, content);
}
