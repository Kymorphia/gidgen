/// New virtual methods declared with @gVirtual on GObjectSubclass classes
module feat_virtual;

import core.thread : Thread;
import gid.gidgenfeat1.object : FeatObject = ObjectWrap;
import gid.gobject2.c.functions;
import gid.gobject2.c.types;
import gid.core.utils : GidCallbackExceptionInfo, setCallbackExceptionHandler;
import gid.gobject2.object : GObjectSubclass, ObjectWrap, gVirtual;
import std.conv : to;
import std.typecons : No, Yes;

/// Enum parameter and return type
enum VColor
{
  Red,
  Green,
  Blue,
}

/// Enum with a 64 bit base type
enum VBig : ulong
{
  Small = 1,
  Large = 0x1_0000_0000UL,
}

/// Class with a new virtual method of each supported kind
class VObj : FeatObject
{
  mixin GObjectSubclass!"GidgenTestVObj";

  int pings;
  string seen;
  FeatObject seenObj;

  @gVirtual protected void vfuncPing()
  {
    pings++;
  }

  @gVirtual protected int vfuncAdd(int a, int b)
  {
    return a + b;
  }

  @gVirtual protected bool vfuncFlip(bool val)
  {
    return !val;
  }

  @gVirtual protected double vfuncNums(byte a, ubyte b, short c, ushort d, int e, uint f, long g, ulong h, float i, double j)
  {
    return a + b + c + d + e + f + g + h + i + j;
  }

  @gVirtual protected byte vfuncRetByte(byte x) { return cast(byte)(x + 1); }
  @gVirtual protected ubyte vfuncRetUByte(ubyte x) { return cast(ubyte)(x + 1); }
  @gVirtual protected short vfuncRetShort(short x) { return cast(short)(x + 1); }
  @gVirtual protected ushort vfuncRetUShort(ushort x) { return cast(ushort)(x + 1); }
  @gVirtual protected uint vfuncRetUInt(uint x) { return x + 1; }
  @gVirtual protected long vfuncRetLong(long x) { return x + 1; }
  @gVirtual protected ulong vfuncRetULong(ulong x) { return x + 1; }
  @gVirtual protected float vfuncRetFloat(float x) { return x + 0.5f; }
  @gVirtual protected double vfuncRetDouble(double x) { return x + 0.5; }

  @gVirtual protected VColor vfuncNext(VColor c)
  {
    return c == VColor.Blue ? VColor.Red : cast(VColor)(c + 1);
  }

  @gVirtual protected VBig vfuncBig(VBig v)
  {
    return v == VBig.Small ? VBig.Large : VBig.Small;
  }

  @gVirtual protected void vfuncDescribe(string text, FeatObject obj, string nothing)
  {
    seen = text ~ (nothing is null ? "|null" : "|set");
    seenObj = obj;
  }

  @gVirtual protected int vfuncThrows()
  {
    throw new Exception("boom");
  }
}

/// Subclass that overrides vfuncs and chains up, and declares a new one
class VChild : VObj
{
  mixin GObjectSubclass!"GidgenTestVChild";

  override protected void vfuncPing()
  {
    super.vfuncPing();
    pings += 10;
  }

  override protected int vfuncAdd(int a, int b)
  {
    return super.vfuncAdd(a, b) * 10;
  }

  override protected void vfuncDescribe(string text, FeatObject obj, string nothing)
  {
    super.vfuncDescribe(text ~ "!", obj, nothing);
  }

  @gVirtual protected int vfuncExtra(int x)
  {
    return x + 100;
  }
}

/// Grand child with no overrides and no new vfuncs
class VGrandChild : VChild
{
  mixin GObjectSubclass!"GidgenTestVGrandChild";
}

/// Grand child that overrides a vfunc of the child that was declared by the child
class VOther : VChild
{
  mixin GObjectSubclass!"GidgenTestVOther";

  override protected int vfuncExtra(int x)
  {
    return super.vfuncExtra(x) + 1000;
  }
}

private size_t classSize(GType t) nothrow
{
  GTypeQuery q;
  g_type_query(t, &q);
  return q.classSize;
}

private size_t instanceSize(GType t) nothrow
{
  GTypeQuery q;
  g_type_query(t, &q);
  return q.instanceSize;
}

/// The class struct is the class struct of the parent followed by one pointer per vfunc in order of declaration
unittest
{
  enum ptrSize = (void*).sizeof;
  auto feat = classSize(FeatObject._getGType);
  auto vobj = classSize(VObj._getGType);
  assert(vobj == feat + 17 * ptrSize, "VObj declares 17 vfuncs: " ~ vobj.to!string ~ " " ~ feat.to!string);
  assert(classSize(VChild._getGType) == vobj + ptrSize);
  assert(classSize(VGrandChild._getGType) == vobj + ptrSize, "No new vfuncs: same class size as the parent");
  assert(classSize(VOther._getGType) == vobj + ptrSize);
  assert(instanceSize(VObj._getGType) == instanceSize(FeatObject._getGType));
}

/// The default implementations are called through the generated public methods
unittest
{
  auto obj = new VObj;

  obj.ping;
  assert(obj.pings == 1);
  assert(obj.add(2, 3) == 5);
  assert(obj.flip(true) == false && obj.flip(false) == true);
  assert(obj.nums(1, 2, 3, 4, 5, 6, 7, 8, 9.5f, 10.25) == 55.75);

  assert(obj.retByte(-3) == -2);
  assert(obj.retUByte(254) == 255);
  assert(obj.retShort(-300) == -299);
  assert(obj.retUShort(65534) == 65535);
  assert(obj.retUInt(uint.max - 1) == uint.max);
  assert(obj.retLong(long.min) == long.min + 1);
  assert(obj.retULong(ulong.max - 1) == ulong.max);
  assert(obj.retFloat(1.0f) == 1.5f);
  assert(obj.retDouble(1.0) == 1.5);

  assert(obj.next(VColor.Red) == VColor.Green && obj.next(VColor.Blue) == VColor.Red);
  assert(obj.big(VBig.Small) == VBig.Large && obj.big(VBig.Large) == VBig.Small);

  auto other = new FeatObject;
  obj.describe("hello", other, null);
  assert(obj.seen == "hello|null" && obj.seenObj is other, "Objects are wrapped to the same D object");
  obj.describe("again", null, "x");
  assert(obj.seen == "again|set" && obj.seenObj is null);
}

/// D overrides are used, also through a base class reference, and super chains up
unittest
{
  auto child = new VChild;
  VObj asBase = child;

  asBase.ping;
  assert(child.pings == 11);
  assert(asBase.add(2, 3) == 50);
  assert(child.extra(1) == 101);
  assert(asBase.flip(true) == false, "Not overridden: inherited default");

  asBase.describe("x", null, null);
  assert(child.seen == "x!|null");

  auto grand = new VGrandChild;
  assert(grand.add(1, 1) == 20 && grand.extra(1) == 101);
  grand.ping;
  assert(grand.pings == 11);

  auto other = new VOther;
  assert(other.extra(1) == 1101 && other.add(1, 1) == 20);
}

/// The slot of the class struct holds a function that calls the D object
unittest
{
  auto child = new VChild;
  auto klass = (cast(GTypeInstance*)child._cPtr).gClass;
  enum ptrSize = (void*).sizeof;
  auto base = classSize(FeatObject._getGType);

  alias AddFn = extern(C) int function(void*, int, int);
  auto add = *cast(AddFn*)(cast(ubyte*)klass + base + 1 * ptrSize);
  assert(add !is null && add(child._cPtr, 4, 5) == 90);

  alias ExtraFn = extern(C) int function(void*, int);
  auto extra = *cast(ExtraFn*)(cast(ubyte*)klass + classSize(VObj._getGType));
  assert(extra(child._cPtr, 2) == 102);
}

private __gshared GType cType;
private __gshared int cAdds;

private size_t slot(size_t index) nothrow
{
  return classSize(FeatObject._getGType) + index * (void*).sizeof;
}

private extern(C) int cAdd(void* self, int a, int b) nothrow
{
  cAdds++;
  auto parentClass = g_type_class_peek(VObj._getGType);
  alias AddFn = extern(C) int function(void*, int, int) nothrow;
  auto parentAdd = *cast(AddFn*)(cast(ubyte*)parentClass + slot(1));
  return parentAdd(self, a, b) + 1000; // Chain up to the implementation of the parent class
}

private extern(C) VColor cNext(void*, VColor) nothrow
{
  return VColor.Blue;
}

private extern(C) void cClassInit(void* klass, void* data) nothrow
{
  *cast(void**)(cast(ubyte*)klass + slot(1)) = cast(void*)&cAdd;
  *cast(void**)(cast(ubyte*)klass + slot(13)) = cast(void*)&cNext;
}

/// A type derived from the D type in C style overrides the vfuncs by writing to the class struct
unittest
{
  auto vobj = VObj._getGType;
  cType = g_type_register_static_simple(vobj, "GidgenTestVObjC", cast(uint)classSize(vobj), &cClassInit,
    cast(uint)instanceSize(vobj), null, cast(GTypeFlags)0);
  assert(cType);

  auto cobj = cast(GObject*)g_object_new(cType, null);
  auto obj = ObjectWrap._getDObject!VObj(cobj, Yes.Take);
  assert(obj !is null && obj.classinfo is VObj.classinfo, "Instance bound to the D class of the nearest D type");

  cAdds = 0;
  assert(obj.add(2, 3) == 1005 && cAdds == 1, "C override is called by the invoker, and chains up to the D default");
  assert(obj.next(VColor.Red) == VColor.Blue, "C override of an enum vfunc");
  obj.ping;
  assert(obj.pings == 1, "Not overridden by C: D default");
  assert(obj.flip(true) == false);

  // Object of the D type is not affected by the override of the C type
  auto plain = new VObj;
  assert(plain.add(2, 3) == 5);
}

/// Calling the slot from a thread that was not created by D works
unittest
{
  auto child = new VChild;
  int result;
  auto klass = (cast(GTypeInstance*)child._cPtr).gClass;
  alias AddFn = extern(C) int function(void*, int, int);
  auto fn = *cast(AddFn*)(cast(ubyte*)klass + slot(1));
  auto t = new Thread({ result = fn(child._cPtr, 6, 4); });
  t.start;
  t.join;
  assert(result == 100);
}

private __gshared int exceptionCount;

private void countException(Exception e, GidCallbackExceptionInfo info) nothrow
{
  exceptionCount++;
}

/// Exceptions in an implementation don't escape to the caller of the vfunc, which gets the default value of the type
unittest
{
  auto obj = new VObj;
  setCallbackExceptionHandler(&countException);
  scope(exit) setCallbackExceptionHandler(null);
  exceptionCount = 0;

  assert(obj.throws == 0);
  assert(exceptionCount == 1);
}

/// Invalid declarations are rejected at compile time
unittest
{
  assert(__traits(compiles, {
    static class Good : ObjectWrap
    {
      mixin GObjectSubclass!"GidgenTestGoodV";
      @gVirtual protected int vfuncOk(string s, VColor c, ObjectWrap o) { return 0; }
    }
  }));

  assert(!__traits(compiles, {
    static class BadName : ObjectWrap
    {
      mixin GObjectSubclass!"GidgenTestBadVName";
      @gVirtual protected void doIt() {}
    }
  }), "Name must start with vfunc and an upper case letter");

  assert(!__traits(compiles, {
    static class BadReturn : ObjectWrap
    {
      mixin GObjectSubclass!"GidgenTestBadVReturn";
      @gVirtual protected string vfuncName() { return null; }
    }
    BadReturn.vfuncName;
  }), "Strings can't be returned");

  assert(!__traits(compiles, {
    static class BadParam : ObjectWrap
    {
      mixin GObjectSubclass!"GidgenTestBadVParam";
      @gVirtual protected void vfuncRef(ref int x) {}
    }
  }), "ref parameters");

  assert(!__traits(compiles, {
    static class BadType : ObjectWrap
    {
      mixin GObjectSubclass!"GidgenTestBadVType";
      @gVirtual protected void vfuncArr(int[] x) {}
    }
  }), "Arrays");
}
