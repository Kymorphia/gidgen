/// GObject interfaces defined in D with GObjectInterface
module feat_dinterface;

import gid.core.utils : GidCallbackExceptionInfo, setCallbackExceptionHandler;
import gidgenfeat.c.functions;
import gidgenfeat.c.types;
import gidgenfeat.iface;
import gidgenfeat.iface_mixin; // Interface mixins are imported without selection, for their public imports
import gid.glib2.c.functions : g_free;
import gid.gobject2.c.functions;
import gid.gobject2.c.types;
import gid.gobject2.object : GObjectInterface, GObjectSubclass, ObjectWrap, gSignal, gVirtual;
import gid.gobject2.types : GTypeEnum;
import std.algorithm : canFind;
import std.string : fromStringz, toStringz;
import std.typecons : No, Yes;

/// Enum parameter and return type
enum DShapeKind
{
  Round,
  Square,
}

/// Interface with methods of the supported kinds
interface DShape
{
  mixin GObjectInterface!"GidgenTestDShape";

  @gVirtual double vfuncArea();
  @gVirtual bool vfuncContains(double x, double y);
  @gVirtual DShapeKind vfuncKind();
  @gVirtual void vfuncDescribe(string text, ObjectWrap other);
  @gVirtual long vfuncThrows();
}

/// Interface extending another one (its prerequisite)
interface DNamed : DShape
{
  mixin GObjectInterface; // Default GType name

  @gVirtual int vfuncTag(string text);
}

/// Interface without methods
interface DMarker
{
  mixin GObjectInterface!"GidgenTestDMarker";
}

/// D class implementing D interfaces
class DSquare : ObjectWrap, DNamed, DMarker
{
  mixin GObjectSubclass!"GidgenTestDSquare";

  double side = 2;
  string seen;
  ObjectWrap seenObj;

  double vfuncArea()
  {
    return side * side;
  }

  bool vfuncContains(double x, double y)
  {
    return x >= 0 && y >= 0 && x <= side && y <= side;
  }

  DShapeKind vfuncKind()
  {
    return DShapeKind.Square;
  }

  void vfuncDescribe(string text, ObjectWrap other)
  {
    seen = text;
    seenObj = other;
  }

  long vfuncThrows()
  {
    throw new Exception("boom");
  }

  int vfuncTag(string text)
  {
    return cast(int)text.length;
  }
}

/// Subclass overriding an interface method of its parent
class DBigSquare : DSquare
{
  mixin GObjectSubclass!"GidgenTestDBigSquare";

  override double vfuncArea()
  {
    return super.vfuncArea() * 10;
  }
}

/// Class implementing a D interface and a C interface
class DBoth : ObjectWrap, DShape, Iface
{
  mixin GObjectSubclass!"GidgenTestDBoth";
  mixin IfaceT!();

  double vfuncArea() { return 1; }
  bool vfuncContains(double x, double y) { return false; }
  DShapeKind vfuncKind() { return DShapeKind.Round; }
  void vfuncDescribe(string text, ObjectWrap other) {}
  long vfuncThrows() { return 0; }
}

private GType[] prerequisites(GType t)
{
  uint n;
  auto p = g_type_interface_prerequisites(t, &n);
  auto r = p[0 .. n].dup;
  g_free(p);
  return r;
}

/// Interface GTypes, prerequisites and interface struct layout
unittest
{
  auto shape = DShape._getGType;
  auto named = DNamed._getGType;
  assert(shape && named && DShape._getGType == shape);
  assert(g_type_fundamental(shape) == GTypeEnum.Interface);
  assert(g_type_name(shape).fromStringz == "GidgenTestDShape");
  assert(g_type_name(named).fromStringz == "Dfeat_dinterface_DNamed");
  // Slots follow the GTypeInterface in order of declaration: the D implementation is in the last slot of DShape
  auto iface = g_type_default_interface_ref(shape);
  assert(iface !is null && (cast(GTypeInterface*)iface).gType == shape);
  g_type_default_interface_unref(iface);
  assert(prerequisites(shape) == [cast(GType)GTypeEnum.Object]);
  auto namedPrereqs = prerequisites(named);
  assert(namedPrereqs.length == 2 && namedPrereqs.canFind(cast(GType)GTypeEnum.Object) && namedPrereqs.canFind(shape));

  auto sq = DSquare._getGType;
  assert(g_type_is_a(sq, shape) && g_type_is_a(sq, named) && g_type_is_a(sq, DMarker._getGType));
  assert(g_type_is_a(DBigSquare._getGType, named));
  assert(g_type_is_a(DBoth._getGType, shape) && g_type_is_a(DBoth._getGType, Iface._getGType));
}

/// Methods are called through the interface struct
unittest
{
  auto sq = new DSquare;
  DShape shape = sq;
  assert(shape.area == 4);
  assert(shape.contains(1, 1) && !shape.contains(3, 1));
  assert(shape.kind == DShapeKind.Square);
  auto other = new DSquare;
  shape.describe("hi", other);
  assert(sq.seen == "hi" && sq.seenObj is other);
  shape.describe(null, null);
  assert(sq.seen is null && sq.seenObj is null);

  DNamed named = sq;
  assert(named.tag("four") == 4 && named.area == 4);

  DShape big = new DBigSquare;
  assert(big.area == 40, "Override in a D subclass");

  auto both = new DBoth;
  assert((cast(DShape)both).area == 1 && (cast(DShape)both).kind == DShapeKind.Round);

  // C call of the slot
  alias AreaFn = extern(C) double function(void*);
  auto iface = g_type_interface_peek((cast(GTypeInstance*)(cast(ObjectWrap)big)._cPtr).gClass, DShape._getGType);
  auto area = *cast(AreaFn*)(cast(ubyte*)iface + GTypeInterface.sizeof);
  assert(area((cast(ObjectWrap)big)._cPtr) == 40);
  alias ThrowsFn = extern(C) long function(void*);
  auto throws = *cast(ThrowsFn*)(cast(ubyte*)iface + GTypeInterface.sizeof + 4 * (void*).sizeof);
  alias KindFn = extern(C) int function(void*);
  auto kind = *cast(KindFn*)(cast(ubyte*)iface + GTypeInterface.sizeof + 2 * (void*).sizeof);
  assert(kind((cast(ObjectWrap)big)._cPtr) == DShapeKind.Square && throws !is null);
  auto namedIface = g_type_interface_peek((cast(GTypeInstance*)(cast(ObjectWrap)big)._cPtr).gClass, DNamed._getGType);
  alias TagFn = extern(C) int function(void*, const(char)*);
  auto tag = *cast(TagFn*)(cast(ubyte*)namedIface + GTypeInterface.sizeof);
  assert(tag((cast(ObjectWrap)big)._cPtr, "abc") == 3, "DNamed has its own interface struct with its own methods");
}

private __gshared int exceptionCount;

private void countException(Exception e, GidCallbackExceptionInfo info) nothrow
{
  exceptionCount++;
}

/// Exceptions don't escape to the caller, which gets the default value
unittest
{
  DShape shape = new DSquare;
  setCallbackExceptionHandler(&countException);
  scope(exit) setCallbackExceptionHandler(null);
  exceptionCount = 0;
  assert(shape.throws == 0 && exceptionCount == 1);
}

// C implementation of DShape and DNamed
private extern(C) double cArea(GObject*) nothrow { return 7.5; }
private extern(C) int cContains(GObject*, double x, double y) nothrow { return x == y; }
private extern(C) int cKind(GObject*) nothrow { return DShapeKind.Round; }
private extern(C) int cTag(GObject*, const(char)* text) nothrow { return text ? 100 + cast(int)text.fromStringz.length : -1; }

private size_t slotOffset(size_t i) nothrow
{
  return GTypeInterface.sizeof + i * (void*).sizeof;
}

private extern(C) void cShapeInit(void* iface, void*) nothrow
{
  *cast(void**)(cast(ubyte*)iface + slotOffset(0)) = cast(void*)&cArea;
  *cast(void**)(cast(ubyte*)iface + slotOffset(1)) = cast(void*)&cContains;
  *cast(void**)(cast(ubyte*)iface + slotOffset(2)) = cast(void*)&cKind;
  // describe and throws are left unset
}

private extern(C) void cNamedInit(void* iface, void*) nothrow
{
  *cast(void**)(cast(ubyte*)iface + slotOffset(0)) = cast(void*)&cTag;
}

private extern(C) double cBigArea(GObject* obj) nothrow
{
  auto iface = g_type_interface_peek((cast(GTypeInstance*)obj).gClass, DShape._getGType);
  auto parentIface = g_type_interface_peek_parent(iface);
  alias AreaFn = extern(C) double function(GObject*) nothrow;
  return (*cast(AreaFn*)(cast(ubyte*)parentIface + slotOffset(0)))(obj) + 1000; // Chain up to the D implementation
}

private extern(C) void cBigShapeInit(void* iface, void*) nothrow
{
  *cast(void**)(cast(ubyte*)iface + slotOffset(0)) = cast(void*)&cBigArea;
}

/// A GObject type that implements the interfaces in C is wrapped as the D interface, which calls the C methods
unittest
{
  GTypeQuery q;
  g_type_query(GTypeEnum.Object, &q);
  auto ctype = g_type_register_static_simple(GTypeEnum.Object, "GidgenTestCShape", q.classSize, null, q.instanceSize,
    null, cast(GTypeFlags)0);
  GInterfaceInfo shapeInfo = GInterfaceInfo(&cShapeInit, null, null);
  GInterfaceInfo namedInfo = GInterfaceInfo(&cNamedInit, null, null);
  g_type_add_interface_static(ctype, DShape._getGType, &shapeInfo);
  g_type_add_interface_static(ctype, DNamed._getGType, &namedInfo);

  auto cobj = g_object_new(ctype, null);
  auto named = ObjectWrap._getDObject!DNamed(cobj, Yes.Take);
  assert(named !is null);
  assert(named.area == 7.5 && named.contains(2, 2) && !named.contains(1, 2) && named.kind == DShapeKind.Round);
  assert(named.tag("ab") == 102 && named.tag(null) == -1);
  named.describe("x", null); // Not implemented: does nothing
  assert(named.throws == 0, "Not implemented: default value");
  assert(named.vfuncArea == 7.5, "The vfunc method of the wrapper calls the C implementation");
  assert(ObjectWrap._getDObject!DShape(cobj, No.Take) is cast(DShape)named, "Same D object");

  // Object that does not implement the interface: methods return default values
  auto plain = g_object_new(GTypeEnum.Object, null);
  auto notShape = ObjectWrap._getDObject!DShape(plain, Yes.Take);
  assert(notShape.area == 0);
}

/// A C type derived from a D class overrides an interface method and chains up to the D implementation
unittest
{
  auto parent = DSquare._getGType;
  GTypeQuery q;
  g_type_query(parent, &q);
  auto ctype = g_type_register_static_simple(parent, "GidgenTestCBigSquare", q.classSize, null, q.instanceSize, null,
    cast(GTypeFlags)0);
  GInterfaceInfo info = GInterfaceInfo(&cBigShapeInit, null, null);
  g_type_add_interface_static(ctype, DShape._getGType, &info);

  auto cobj = g_object_new(ctype, null);
  auto shape = ObjectWrap._getDObject!DShape(cobj, Yes.Take);
  assert(cast(DSquare)shape !is null, "D object of the nearest D class");
  assert(shape.area == 1004, "C override chains up to the D implementation");
  assert(shape.kind == DShapeKind.Square, "Not overridden: D implementation");
}

/// Invalid declarations are rejected at compile time
unittest
{
  assert(__traits(compiles, {
    interface Good { mixin GObjectInterface!"GidgenTestGoodDIface"; @gVirtual int vfuncOk(string s); }
  }));
  assert(!__traits(compiles, {
    static class NotIface : ObjectWrap { mixin GObjectInterface!"GidgenTestBadDIface1"; }
  }), "Not an interface");
  assert(!__traits(compiles, {
    interface BadName { mixin GObjectInterface!"GidgenTestBadDIface2"; @gVirtual int doIt(); }
  }), "Name must start with vfunc");
  assert(!__traits(compiles, {
    interface BadSignal { mixin GObjectInterface!"GidgenTestBadDIface3"; @gSignal void changed(); }
  }), "No signals");
  assert(!__traits(compiles, {
    interface BadBase : Iface { mixin GObjectInterface!"GidgenTestBadDIface4"; }
    BadBase._getGType();
  }), "Only D-defined interfaces as bases");
}
