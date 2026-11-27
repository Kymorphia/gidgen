/// GIR names of binding types (_dGirType), from the @gidGir attributes and the GIR namespace constants gidgen writes
module feat_gir_names;

import gid.core.basictypes : gidGir;
import gid.gidgenfeat1.c.types : GidgenFeatPoint;
import gid.gidgenfeat1.child_iface : ChildIface;
import gid.gidgenfeat1.field_kinds : FieldKinds;
import gid.gidgenfeat1.object : FeatObject = ObjectWrap;
import gid.gidgenfeat1.types : Color, Point, Style;
import gid.glib2.bytes : Bytes;
import gid.glib2.error : ErrorWrap;
import gid.glib2.types : ChecksumType;
import gid.glib2.variant : Variant;
import gid.gobject2.dig : _dGirType;
import gid.gobject2.object : ObjectWrap;
import gid.gobject2.param_spec : ParamSpec;
import gid.gobject2.types : ParamFlags;
import gid.gobject2.value : Value;
import Shim1 = gid.shimlib1.widget;
import Shim2 = gid.shimlib2.widget;
import ShimDefault = gid.shimlib.widget;
import std.traits : getUDAs;

/// Namespace constants
unittest
{
  static assert(imported!"gid.gobject2.c.types"._gidGirNamespace == "GObject");
  static assert(imported!"gid.gobject2.c.types"._gidGirVersion == "2.0");
  static assert(imported!"gid.glib2.c.types"._gidGirNamespace == "GLib"); // Not hidden by the public import
  static assert(imported!"gid.gidgenfeat1.c.types"._gidGirVersion == "1.0");
  static assert(imported!"gid.shimlib1.c.types"._gidGirVersion == "1.0");
  static assert(imported!"gid.shimlib2.c.types"._gidGirVersion == "2.0");
}

/// Classes, interfaces, structures and their class or interface structures
unittest
{
  static assert(_dGirType!FeatObject.fullName == "GidgenFeat.Object"); // Renamed D class
  static assert(_dGirType!FeatObject.typeStruct == "ObjectClass");
  static assert(_dGirType!ChildIface.typeStruct == "ChildIfaceInterface");
  static assert(_dGirType!ChildIface.fullName == "GidgenFeat.ChildIface");
  static assert(_dGirType!FieldKinds.fullName == "GidgenFeat.FieldKinds");
  static assert(_dGirType!ObjectWrap.fullName == "GObject.Object");
  static assert(_dGirType!ObjectWrap.namespace == "GObject" && _dGirType!ObjectWrap.nsVersion == "2.0");
  static assert(_dGirType!ObjectWrap.name == "Object" && _dGirType!ObjectWrap.typeStruct == "ObjectClass");
  static assert(_dGirType!ParamSpec.fullName == "GObject.ParamSpec");
  static assert(_dGirType!Value.fullName == "GObject.Value");
  static assert(_dGirType!ErrorWrap.fullName == "GLib.Error" && _dGirType!ErrorWrap.typeStruct is null);
  static assert(_dGirType!Bytes.fullName == "GLib.Bytes");
  static assert(_dGirType!Variant.fullName == "GLib.Variant");
}

/// Enumerations, flags and C structure aliases (the attribute is on the C declaration of c/types.d)
unittest
{
  static assert(_dGirType!Color.fullName == "GidgenFeat.Color");
  static assert(_dGirType!Style.fullName == "GidgenFeat.Style"); // Flags
  static assert(_dGirType!Point.fullName == "GidgenFeat.Point"); // StructAlias
  static assert(is(Point == GidgenFeatPoint) && getUDAs!(GidgenFeatPoint, gidGir)[0] == gidGir("Point"));
  static assert(_dGirType!ChecksumType.fullName == "GLib.ChecksumType");
  static assert(_dGirType!ParamFlags.fullName == "GObject.ParamFlags");
}

/// Versioned majors and unversioned shims resolve to the defining module
unittest
{
  static assert(_dGirType!(Shim1.Widget).fullName == "ShimLib.Widget" && _dGirType!(Shim1.Widget).nsVersion == "1.0");
  static assert(_dGirType!(Shim2.Widget).nsVersion == "2.0");
  static assert(_dGirType!(ShimDefault.Widget).fullName == "ShimLib.Widget"
    && _dGirType!(ShimDefault.Widget).nsVersion == "2.0"); // Through the shim of the latest major
}

/// Types which are not binding types are rejected at compile time
unittest
{
  static class NotBound {}
  static assert(!__traits(compiles, _dGirType!NotBound.fullName));
  static assert(!__traits(compiles, _dGirType!int.fullName));

  static class DSubclass : ObjectWrap {} // Attributes are not inherited
  static assert(!__traits(compiles, _dGirType!DSubclass.fullName));
}
