module generator;

import dxml.writer;
import std.array : Appender, appender;
import std.file : writeFile = write;
import std.path : buildPath;

import code_writer;

import common;
import func_param_tests;

/// Generator singleton
Generator genInstance;

static this()
{
  genInstance = new Generator;
}

/// Gidgen integration test generator class
class Generator
{
  enum cIdentPrefix = "Gidg";
  enum cSymPrefix = "gidg";
  enum girPath = "gir";
  enum gidgenTestPath = "gidgen-test";

  this()
  {
    cCow = new CodeWriter(buildPath(gidgenTestPath, "gidgen-test.c"), [
      "#include <glib.h>", "",
    ]);

    dCow = new CodeWriter(buildPath(gidgenTestPath, "gidgen-test.d"), [
      "module gidgen_test;", "",
      "import gidgentest.global;",
    ]);

    girAp = appender!string;
    girAp.writeXMLDecl!string;
    girXml = xmlWriter(girAp);

    girXml.openStartTag("repository");
    girXml.writeAttr("version", "1.2");
    girXml.writeAttr("xmlns", "http://www.gtk.org/introspection/core/1.0");
    girXml.writeAttr("xmlns:c", "http://www.gtk.org/introspection/c/1.0");
    girXml.writeAttr("xmlns:glib", "http://www.gtk.org/introspection/glib/1.0");
    girXml.closeStartTag;

    girXml.openStartTag("include");
    girXml.writeAttr("name", "GLib");
    girXml.writeAttr("version", "2.0");
    girXml.closeStartTag;
    girXml.writeEndTag;

    girXml.openStartTag("include");
    girXml.writeAttr("name", "GObject");
    girXml.writeAttr("version", "2.0");
    girXml.closeStartTag;
    girXml.writeEndTag;

    girXml.openStartTag("package");
    girXml.writeAttr("name", "gidgen-test");
    girXml.closeStartTag;
    girXml.writeEndTag;

    girXml.openStartTag("c:include");
    girXml.writeAttr("name", "gidgen-test.h");
    girXml.closeStartTag;
    girXml.writeEndTag;

    girXml.openStartTag("namespace");
    girXml.writeAttr("name", "GidgenTest");
    girXml.writeAttr("version", "1.0");
    girXml.writeAttr("shared-library", "libgidgen-test.so.0");
    girXml.writeAttr("c:identifier-prefixes", cIdentPrefix);
    girXml.writeAttr("c:symbol-prefixes", cSymPrefix);
    girXml.closeStartTag;
  }

  /// Generate gidgen integration test code
  void generate()
  {
    genFuncParamsTests;

    girXml.writeEndTag;  // namespace
    girXml.writeEndTag;  // repository
  }

  /// Write generated gidgen integration test code
  void write()
  {
    cCow.write;
    dCow.write;
    writeFile(buildPath(girPath, "GidgenTest-1.0.gir"), girAp[]);
  }

  CodeWriter cCow; // C code writer (C library API being bound)
  CodeWriter dCow; // D code writer (unittest D code to exercise the C library API)
  Appender!string girAp; // GIR XML file appender which is used with girXml
  XMLWriter!(Appender!string) girXml; // XML writer (GIR file for the C library API)
}
