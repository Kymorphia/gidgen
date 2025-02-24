# gidgen - GObject Instrospection D Binding Generator

**gidgen** is a GObject to D language (AKA Dlang) binding generator which is part of the **giD** project (pronounced as *giddy*).
The intention of this project is to create high quality D language bindings for libraries with GObject Introspection APIs.

The **gidgen** utility takes XML GObject Introspection Repository (GIR) files and generates D binding packages which can be used with [dub](https://dub.pm/).

The [giD Package Repository](https://github.com/Kymorphia/gid/) hosts the current archive of generated D bindings.
It currently includes bindings for Gtk4 and its dependencies; Vte terminal library, GtkSource code viewer widget, Apache Arrow, and more.
Please consult the documentation there for more information on developing D applications with giD library bindings.

The remainder of this document describes how to use **gidgen** for creating and improving D bindings.

## Features

Some of the features of **gidgen** include:

* The goal is to automatically generate quality bindings based on GIR API definitions, with minimal custom code.
* Handles C callbacks with data "closures" using delegates and intelligent delegate lifecycle management.
* Types:
  * Converts basic types between C and D code.
  * Converts between C zero terminated strings and D strings.
  * Support for GBoxed types.
  * Support for other complex memory managed referenced types, structures, and unions.
  * Arrays of any supported type.
  * Supports strv (`char**`) as `string[]` arrays.
  * Container types are converted between native D dynamic arrays (GArray, GByteArray, GPtrArray, GList, GSList).
  * GHashTable parameters and return values are converted between D associative arrays.
  * Support for getting and setting values of GValue and GVariant using templates and native D types.
  * Throws D exceptions for GErrors.
  * Native C library functions can be called directly if desired and are loaded dynamically at runtime.
* GObject:
  * Wrapping of C GObject and Interface instances.
  * Uses interface proxy objects for interfaces when the GObject type is unknown to D.
  * Each GObject signal has delegate and function callback type aliases and a `connectSignalName()` template.
  * Support for signal "detail" parameters like property names used with the GObject "notify" property.
  * Method aliases are automatically generated for class or interface methods which conflict with ancestor classes.
* Binding Package Definitions:
  * Binding package definition files are just D source code files with additional commands added as specially formatted comments,
    making it easier to edit custom binding D code.
  * Issues with GIR API repository files are corrected using commands in the definition files to patch the XML data,
    providing a simple yet powerful way to fix issues with API definitions and a clean separation with custom binding code.
* Binding Packages:
  * Generates binding package dub.json files.
  * Outputs detailed warnings for GIR API elements which have issues or are not currently supported.
  * Command line options for identifying and resolving GIR issues.
  * Support for debugging gidgen using traps in gdb, to set breakpoints when specific parts of the binding are processed.
* Binding coverage reports:
  * A differentiation is made between intentionally disabled API (Disabled) vs gidgen limitations/GIR issues (Unsupported).
    Disabled items aren't included in the coverage statistics and are either not useful for the binding or are replaced by custom code.
  * The report can be customized to output items of different types and their Active state.

## Command Line Argument Reference

Command line help reference is produced with the `--help` command argument:

```sh
./gidgen --help
GObject Introspection Dlang binding generator
-d           --defs Add a path to a directory of binding definition files or a single file (one or more required)
-g       --gir-path Add a path to search for GIR files (one or more required)
-p       --pkg-path Top-level package binding output path (required)
-s    --subpkg-path Subpackage path to write individual library packages to (required)
         --def-help Display binding definition file command help
        --log-level Log level (all, trace, info, warning, error, critical, fatal, off)
     --log-gir-locs Log GIR file locations in warnings
    --log-code-locs Log code locations in warnings
           --report Output binding coverage statistics (defaults to --report-options AllUnsupported)
      --report-file File to output report to (defaults to stdout if not specified)
   --report-options Customize report output (logically OR'd flags with '|' character, 'help' for flag list)
          --suggest Output definition file command suggestions
   --dump-selectors Dump XML selectors for warnings
      --dump-ctypes Dump all raw C types
      --dump-dtypes Dump all raw D types
       --dump-kinds Dump the list of type kinds
     --dump-matches Dump XML patch selector matches
       --dump-traps Dump code trap actions
             --trap Add gdb breakpoint 'action:regex', action: domain (help to list), regex: pattern to match
-h           --help This help information.
```

### Basic Commands

These are basic commands which are used for general use cases.

* **-d, --defs**: Add a path to a directory of giD binding definition files or a single file. Option is required and can be specified multiple times.
* **-g, --gir-path**: Add a path to the GIR search path. This option is required and can be specified multiple times.
* **--log-level**: Set the output log level. Can be one of: all, trace, info, warning, error, critical, fatal, or off.
  The default value is **warning**. A value of **info** can be used to display useful information about automatic binding decisions,
  which are potentially problematic.

### Report Commands

A binding coverage report can be created and is configurable with the following options:

* **--report** Generate a coverage report
* **--report-file** Specify a file to write the report to. Uses stdout by default if not specified.
* **--report-options** Can be used to customize the report output.
  Consists of one or more flags which are logically OR'd with the '|' pipe character.
  The default is **AllUnsupported** if not specified.

#### Report Options

The available report options can be output by specifying **--report-options help** which will display:

```sh
./gidgen --report-options help
Report options help (values can be logically OR'd with '|'):
Summary        Generate a binding coverage summary table
Enabled        Show Enabled item names
Disabled       Show Disabled item names
Ignored        Show Ignored item names
Unsupported    Show Unsupported item names
Structs        Show Structure/Class names
Funcs          Show Function/Method names
Signals        Show Signal names
Fields         Show Field names
All            Generate a full report
AllEnabled     Generate a report of all Enabled items
AllDisabled    Generate a report of all Disabled items
AllIgnored     Generate a report of all Ignored items
AllUnsupported Generate a report of all Unsupported items (default)
```

The **Summary** flag outputs a table of coverage statistics, described in more detail below.

There are four flags which determine the active state of items whose names will be output:

* **Enabled** Displays names of items which are enabled and present in the generated binding.
* **Disabled** Displays names of items which are intentionally disabled, not due to gidgen limitations or GIR issues.
* **Ignored** Displays names of items which are intentionally ignored and not considered to be relevant to D bindings.
* **Unsupported** Displays names of items which are inactive because of a limitation in gidgen or an issue with the GIR specification.

Four of the flags determine the types of items whose names will be output:

* **Structs** Structured data, including: structs, classes, interfaces, and unions. Correspond to a single module in the binding package.
* **Funcs** Functions including: global functions, static class functions, and methods.
* **Signals** GObject signals.
* **Fields** Structure fields, which includes: structs, classes, and unions.

The remaining flags are for convenience and are combinations of the previous flags, including:

* **All** All of the above flags, which creates a full report.
* **AllEnabled** All four of the item type flags and the Enabled flag, which outputs the names of all enabled items which are output in the binding.
* **AllDisabled** All four of the item type flags and the Disabled flag, which outputs the names of all of the explicitly disabled items.
* **AllIgnored** All four of the item type flags and the Ignored flag, which outputs the names of all of the ignored items.
* **AllUnsupported** All four of the item type flags and the Unsupported flag, which outputs the names of all of the unsupported API items which are not covered.
  These count against the total coverage scores.

#### Summary

Example summary output:
```sh
[Summary]
Package    |       Structs        |        Funcs         |       Signals        |        Fields        |
           |  Act  Dis  Uns  Perc |  Act  Dis  Uns  Perc |  Act  Dis  Uns  Perc |  Act  Dis  Uns  Perc |
GLib       |   86    8    0   100 | 1358  623   10  99.3 |    0    0    0   100 |  213   23   35  85.9 |
GModule    |    3    0    0   100 |    8    6    0   100 |    0    0    0   100 |    0    0    0   100 |
GObject    |   64    2    0   100 |  306  158    3  99.0 |    3    0    0   100 |  231    8   10  95.9 |
Gdk        |   80    0    0   100 |  478   34    5  99.0 |   33    0    2  94.3 |   32    0    1  97.0 |
GdkPixbuf  |   16    0    0   100 |   95   12    2  97.9 |    4    0    0   100 |   42    4    2  95.5 |
Gid        |    3    0    0   100 |    0    0    0   100 |    0    0    0   100 |    0    0    0   100 |
Gio        |  374    2    0   100 | 1729  162   17  99.0 |   80    0    1  98.8 |  885  195   32  96.5 |
Graphene   |   21    0    0   100 |  367   43    0   100 |    0    0    0   100 |   40    0    0   100 |
Gsk        |   57    0    0   100 |  266   24    4  98.5 |    0    0    0   100 |   13    0    2  86.7 |
Gtk        |  542    0    0   100 | 3593  115   10  99.7 |  342    1    3  99.1 |  601   95    9  98.5 |
HarfBuzz   |   35    0    0   100 |  407   52   42  90.6 |    0    0    0   100 |   71   28   11  86.6 |
Pango      |   54    0    0   100 |  388   44   12  97.0 |    0    0    0   100 |  162   11    8  95.3 |
PangoCairo |    4    0    0   100 |   26    5    0   100 |    0    0    0   100 |    0    0    0   100 |
cairo      |   20    0    1  95.2 |  327   56    3  99.1 |    0    0    0   100 |   38    0    1  97.4 |
freetype2  |    5    0    0   100 |    0    0    0   100 |    0    0    0   100 |    0    0    0   100 |
```

The **Package** column contains the package name. Additional columns are shown for the enabled item types (Structs, Funcs, Signals, Fields).
Each of the additional columns contains sub columns for the number of Active (Act), Disabled (Dis), and Unsupported (Uns) items.
The final sub-column contains the percentage coverage, which is the count of Active items from the total Active and Unsupported items.
Disabled and Ignored items are not included in the coverage percentage calculation as they are not considered to be a part of the API binding.

### Binding Debugging Commands

The following commands are useful for improving and troubleshooting bindings.

* **--def-help** Display giD binding definition file help.
* **--log-gir-locs** Log GIR file locations in warnings. This is useful for locating API definitions in GIR files which have issues.
* **--suggest** Outputs automated suggestions of XML patch commands which might be applicable. They should be reviewed for correctness.
* **--dump-selectors** Dumps `set` command XML selectors for GIR warnings as a convenient start point for defining XML patches.
* **--dump-ctypes** Display all C types from all packages.
* **--dump-dtypes** Display all D types from all packages and their designated type kinds.
* **--dump-kinds** Display the list of type kinds which are used for classifying types for binding generation.
  Type kinds: Unknown, Basic, String, BasicAlias, Enum, Flags, Callback, Container, Simple, Pointer, Opaque, Wrap, Boxed, Reffed, Object, Interface, Namespace
* **--dump-matches** Dumps XML selector matches. Useful for troubleshooting XML patch commands in package definition files.

### gidgen Program Debugging Commands

These commands are useful for debugging the gidgen CLI program itself and pausing execution in GDB at specific points of binding processing.

* **--log-code-locs** Log code locations in warnings. Useful for locating the source code which resulted in a particular warning (can be sorted to group similar issues).
* **--dump-traps** Dump all binding generation traps, which GDB breakpoints can be set on.
* **--trap REGEX** Add a binding generation trap matching REGEX, which will cause a GDB debugger breakpoint.

## Binding Definition File Reference

Binding definition files are just D language files with a .d extension, but which can contain special giD command comments.
These files control various aspects of binding generation and can also contain custom D binding code.

There are 3 scopes of binding definition files, defined below:
* **global.d** - This specially named file defines global commands which are relevant to all binding libraries.
* **Namespace.d** - There is usually one file per library (namespace), which is used for commands and global binding code for the library.
  For example: `GLib.d`.
* **Namespace-Class.d** - Files for classes/structures of a library can also be defined and these take the name of the namespace and class/structure separated by a dash.
  These files contain commands and custom code related to the specific class or structure. These are primarily used for organization purposes,
  since commands and custom code for classes/structs can also be defined in the library definition file as well.
  For example: `GObject-Value.d`

### Command Syntax

* Commands are prefixed with `//!`.
* giD comments are prefixed with `//#` and aren't output to binding code.
* Strings can be single or double quoted.
* Some commands support multi-line values using opening and close braces within giD comment lines (seldom used).

### Command Reference

Binding definition file command reference can be output with the `--def-help` command line option.
This outputs the following:

```sh
./gidgen --def-help
giD binding definition command help
Commands are prefixed with '//!'.
giD comments are prefixed with '//#' and aren't output to binding code.
Strings can be single or double quoted.
Some commands support multi-line values using opening and close braces within giD comment lines ('Block' flag).
Commands indicating 'Repo' in parenthesis require a repo to have been specified, 'Class' requires a class (or struct).
Commands:

add <XmlSelect> <AttributeValue | Xml> - Add an XML attribute or node (Block)
class <Class> - Select the current structure/class (Repo)
del <XmlSelect> - Delete an XML attribute or node
import <Import> - Add a D import (Class)
info <name> <value> - Set JSON dub info for repo or master package (name, description, copyright, authors, license), multiple authors values can be given
inhibit [nothing imports init funcs] - Inhibit generation of certain module code (space separated flags) (Class)
kind <TypeName> <TypeKind> - Override a type kind (Repo)
merge <Namespace> - Merge current repo into the package identified by Namespace (Repo)
namespace <Namespace> - Create a repository from a namespace instead of a Gir file
rename <XmlSelect> <AttributeName | XmlNodeId> - Rename an XML attribute or node ID
repo <RepoName> - Specify the Gir repository name to load
reserved <Word> - Identify a reserved word, an underscore will be appended to it
set <XmlSelect> <AttributeValue | Xml> - Set an XML attribute or node (Block)
subtype <FromTypeName> <ToTypeName> - Substitute a type name (D and C types)
subctype <FromTypeName> <ToTypeName> - Substitute a C type name
subdtype <FromTypeName> <ToTypeName> - Substitute a D type name
```
These commands are described in more detail in the following sections.

### GIR XML Patching Commands

There are often issues with GIR XML API definition files.
To correct such issues the XML data can be patched using `add`, `del`, `rename`, or `set` commands.
Sometimes these commands are also used for customizing behavior of the resulting bindings.

These commands use an XML selector to find a single XML element/attribute or match multiple ones using wildcards.

#### Gidgen GIR Extensions

Gidgen supports a number of extensions which are used for additional configuration of the resulting bindings.

The following XML attributes can be specified on API items and are set to a value of 1 to enable them:
* **disable** - Disable an API item, which should be addressed through gidgen improvements or custom binding code.
* **ignore** - Also disables an API item, but is used for marking APIs as not useful for D bindings.
* **unsupported** - API item as unsupported. These are identified candidates for future gidgen improvements.
  Not normally specified directly, but detected by gidgen.

**Additional features:**

* A method or function return value can be an output array parameter length, by specifying `length="-1"` in the XML attributes for the array.
* Input array parameters can be used as the length of an output array parameter or return value,
  by setting the `length` XML attribute of the array to the relevant parameter index (subtract 1 if there is a instance parameter).

#### XML selector reference

* Provides a way of selecting one or more nodes or attributes
* Node IDs are separated by periods: `repository.namespace`
* "name" attributes can be matched in square brackets: `function[my_function]`
* Other attribute values can be matched with ATTR=VAL syntax: `function[c:identifier=g_boxed_copy]`
* Multiple attributes can be matched (logic AND) by seperating them with commas: `function[my_function,version=1.22]`
* When selecting an attribute it follows surrounded by a second set of square brackets: `record[my_struct][opaque]`
* If the selected node does not have any attribute selection criteria it can be empty brackets: `namespace[][name]`
* Wildcards:
  * Selectors select a single node/attribute unless wildcards are used
  * To match a node anywhere in the XML doc use '*' as the first node ID component: `*.array[ByteArray]`
  * A wildcard node selector is specified by prepending a '*' before the node ID: `function[my_function].*parameter`
  * A wildcard attribute selector is followed by square brackets to specify the attribute: `record[my_record].*parameter[introspectable]`
  * Wildcards can be specified in attribute values by including them in the value: `function[new*]`

#### Command Reference

Additional details on the XML patching commands are below:

* **add** - For adding XML elements. The selector defines the node or nodes under which to add the XML,
  which is defined as the second parameter. For example: `//!add repository '<include name="GLib" version="2.0"/>'`
* **del** - For deleting XML elements or attributes. The selector defines the node(s) or attribute(s) to delete.
  For example: `//!del '*class[glib:get-type=intern][glib:get-type]'` which deletes all attributes named **glib:get-type**
  that are defined within an element named **class** that has the value **intern**.
* **rename** - Rename XML elements. For example: `//!rename record[Variant] class` would rename the **record** element
  with the name **Variant** to be a **class** instead of a **record**.
* **set** - Set the value of XML elements or attributes. For example: `//!set record[IOChannel][opaque] 1`
  is used for setting IOChannel to be opaque.

### Binding Scope Commands

Some commands are used for defining the current scope of other commands.
These include: `repo`, `namespace`, and `class`.

The `repo` command is used for processing a GIR XML file to create bindings from.
It is usually the first command in a library definition file and instructs gidgen to process a GIR file and create bindings for it.
It takes a single argument which is the GIR filename without the **.gir** extension.
For example: `//!repo Gtk-4.0`

The `namespace` command is used instead of the `repo` command to define a new namespace.
This can be used for defining a custom library/namespace which is not represented by a GIR file.
One example is the Gid namespace which is used for binding support code and is declared like: `//!namespace Gid`.

The `class` command can be used following either a `repo` or `namespace` command and selects the current class or structure which other commands act upon.

### Binding Behavior Commands

There are several commands which modify the behavior of binding generation. These are described in more detail below:
* **import** - Add a D import to the list of imports for the current class/structure.
* **info** - Used for defining values in dub.json package files. It takes a name, which is one of: name, description, copyright, authors, or license.
  The second parameter is the value to assign. The **authors** info value can be assigned multiple times, to be used when there are multiple authors.
  Example: `//!info description "GObject introspection D binding repository"`
* **inhibit** - This command is used to inhibit automatic code generation for modules and takes one or more space separated values from:
  imports, init, or funcs. These values inhibit code generation for imports, class init methods, and methods/functions respectively.
* **kind** - Override the **kind** of a type. gidgen automatically determines what kind a type is (Basic, String, Object, etc).
  This command takes a type identifier followed by the kind label, which can be obtained from executing `./gidgen --dump-kinds`.
  For example: `//!kind OptionEntry Simple`
* **merge** - This causes the current repo to be merged with another named repo identified by it's namespace.
  The giD Package Repository distribution use this for merging GLib, GObject, and Gid into a single library to resolve mutual dependency issues.
  For example: `//!merge GLib`
* **reserved** - Identify a reserved word, which will cause any instances of it in binding symbols to have an underscore appended to it.
  This is primarily used for identifying all D language reserved words. For example: `//!reserved version`.
* **subtype** - Used for renaming (substituting) a type name. This command applies for both D and C types.
  Example: `//!subtype "unsigned char" ubyte`.
* **subctype** - This command is used for renaming C types only. For example: `//!subctype GObject ObjectC`.
* **subdtype** - Used for renaming D types only. For example `//!subdtype Object ObjectG`.

## What about GtkD and gir-to-d?

The author of this project made an effort to get [GtkD](https://gtkd.org/) and [gir-to-d](https://github.com/gtkd-developers/gir-to-d/tree/master)
to generate a more complete GTK4 binding.
However, after much work it was decided that efforts would be better spent to create a new GObject Introspection binding utility for this purpose.
This project is the result of that effort.
