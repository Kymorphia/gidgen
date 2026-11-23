/// GidgenFeat virtual method override tests: D overrides installed in the class structure of D subclass GTypes
module feat_vfunc;

import gid.core.utils : GidCallbackExceptionInfo, setCallbackExceptionHandler;
import gid.gidgenfeat1.c.functions;
import gid.gidgenfeat1.c.types;
import gid.gidgenfeat1.global : liveCount;
import gid.gidgenfeat1.object : FeatObject = ObjectWrap;
import gid.gidgenfeat1.types : Counter;
import gid.glib2.error : ErrorWrap;
import gid.glib2.c.functions : g_error_free, g_quark_from_string;
import gid.gobject2.c.functions;
import gid.gobject2.c.types;
import GObj = gid.gobject2.object;
import gid.gobject2.object : GObjectSubclass;
import std.string : toStringz;
import std.typecons : No;

/// GType of a C instance
GType instType(void* cObj)
{
  return (cast(GTypeInstance*)cObj).gClass.gType;
}

/// D subclass overriding most of the FeatObject vfunc methods, with the default GType name
class DObj : FeatObject
{
  mixin GObjectSubclass;

  FeatObject[] absorbed;
  bool constructedBound; // vfuncConstructed was called with the D object bound to the C instance
  bool ctorRan; // Set by the constructor body

  this(int value)
  {
    super(); // Creates the C instance with the DObj GType
    ctorRan = true;
    setValue(value);
  }

  override protected void vfuncConstructed()
  {
    constructedBound = _cPtr !is null && !ctorRan; // Called by g_object_new before the constructor body
    super.vfuncConstructed();
  }

  override protected int vfuncCompute(int x)
  {
    return super.vfuncCompute(x) + 1; // Chain up to the C implementation
  }

  override protected void vfuncAbsorb(FeatObject other)
  {
    absorbed ~= other; // Keep a D reference to the object parameter
    super.vfuncAbsorb(other);
  }

  override protected bool vfuncDivide(int divisor, out int quotient, out int remainder)
  {
    if (divisor == 0)
      return false;

    quotient = 100 + getValue / divisor;
    remainder = getValue % divisor;
    return true;
  }

  override protected int vfuncHook()
  {
    throw new Exception("hook failed");
  }

  override protected bool vfuncCheck(int value)
  {
    if (value == 13)
      throw new ErrorWrap(g_quark_from_string("feat-vfunc-test"), 42, "unlucky");

    if (value == 7)
      throw new Exception("plain exception");

    return super.vfuncCheck(value);
  }
}

/// Second level D subclass with its own GType and an explicit GType name
class DChild : DObj
{
  mixin GObjectSubclass!"GidgenTestDChild";

  this(int value)
  {
    super(value);
  }

  override protected int vfuncCompute(int x)
  {
    return super.vfuncCompute(x) * 10; // Chains up through DObj to the C implementation
  }
}

/// D subclass overriding a single vfunc method, with a default constructor (used for C created instances)
class DPartial : FeatObject
{
  mixin GObjectSubclass;

  this(int value = 3)
  { // Implicit super() creates the C instance with the DPartial GType
    setValue(value);
  }

  override protected int vfuncHook()
  {
    return 5;
  }
}

/// D subclass whose GType name collides with an existing GType (FeatObject)
class DCollide : FeatObject
{
  mixin GObjectSubclass!"GidgenFeatObject";

  this()
  {
    super();
  }
}

/// Two D classes forced to the same type name
class DSameA : FeatObject
{
  mixin GObjectSubclass!"GidgenTestSameName";

  this()
  {
    super();
  }
}

class DSameB : FeatObject
{
  mixin GObjectSubclass!"GidgenTestSameName";

  this()
  {
    super();
  }
}

/// D subclass of a D subclass without its own GType (uses the DPartial GType)
class DPartialPlain : DPartial
{
  this()
  {
    super(4);
  }
}

private __gshared int exceptionCount;

private void countException(Exception e, GidCallbackExceptionInfo info) nothrow
{
  exceptionCount++;
}

/// C calls of virtual methods reach the D overrides, which chain up to the C implementations
unittest
{
  auto start = liveCount(Counter.Object);
  auto obj = new DObj(6);
  assert(obj.compute(7) == 43, "C invoker reaches the D override, which chains up to the C implementation");

  int quotient, remainder;
  assert(obj.divide(4, quotient, remainder) && quotient == 101 && remainder == 2, "out parameters and return value");
  assert(!obj.divide(0, quotient, remainder));

  auto other = FeatObject.newWithValue(4);
  obj.absorb(other); // Object parameter (transfer none)
  assert(obj.absorbed.length == 1 && obj.absorbed[0] is other, "object parameter maps to the existing D object");
  assert(other.getRefCount == 1, "object parameter reference count");
  assert(obj.getValue == 10);

  auto child = new DChild(6);
  assert(child.compute(7) == 430, "two level D override chain up");
  child.absorb(other); // Inherited DObj override installed in the DChild class
  assert(child.absorbed.length == 1 && child.getValue == 10);

  obj.absorbed = null;
  child.absorbed = null;
  destroy(obj);
  destroy(child);
  destroy(other);
  assert(liveCount(Counter.Object) == start, "objects leaked");
}

/// Exceptions in D overrides are caught, throwing virtual methods return them as GErrors
unittest
{
  auto obj = new DObj(1);

  setCallbackExceptionHandler(&countException);
  scope(exit) setCallbackExceptionHandler(null);
  exceptionCount = 0;
  assert(obj.hook == 0, "default return value when the override throws");
  assert(exceptionCount == 1, "exception handler called");

  assert(obj.check(5));

  try
  {
    obj.check(-1); // D override chains up to the C implementation which sets a GError
    assert(0, "check(-1) should throw");
  }
  catch (ErrorWrap e)
    assert(e.message == "negative value");

  try
  {
    obj.check(13); // ErrorWrap thrown by the D override
    assert(0, "check(13) should throw");
  }
  catch (ErrorWrap e)
    assert(e.code == 42 && e.domain == g_quark_from_string("feat-vfunc-test") && e.message == "unlucky");

  try
  {
    obj.check(7); // Other exception thrown by the D override
    assert(0, "check(7) should throw");
  }
  catch (ErrorWrap e)
    assert(e.domain == g_quark_from_string("gid-exception") && e.message == "plain exception");

  assert(exceptionCount == 1, "GError exceptions do not call the exception handler");
  destroy(obj);
}

/// Virtual methods which are not overridden keep the C implementation
unittest
{
  auto obj = new DPartial(3);
  assert(obj.hook == 5, "overridden optional vfunc");

  auto dClass = cast(GidgenFeatObjectClass*)g_type_class_peek(DPartial._getGType);
  auto cClass = cast(GidgenFeatObjectClass*)g_type_class_peek(FeatObject._getGType);
  assert(dClass.compute is cClass.compute && dClass.absorb is cClass.absorb && dClass.divide is cClass.divide
    && dClass.check is cClass.check, "slots of vfunc methods which are not overridden are untouched");
  assert(dClass.hook !is null && cClass.hook is null);

  assert(obj.compute(4) == 12);
  destroy(obj);
}

/// GType registration: names, parents, D subclass flags, and one GType per D class
unittest
{
  import std.string : fromStringz;

  assert(g_type_name(DObj._getGType).fromStringz == "Dfeat_vfunc_DObj", "default GType name");
  assert(DObj._getGType == DObj._getGType, "registered once");
  assert(g_type_parent(DObj._getGType) == FeatObject._getGType, "parent is the bound type");
  assert(g_type_name(DChild._getGType).fromStringz == "GidgenTestDChild", "explicit GType name");
  assert(g_type_parent(DChild._getGType) == DObj._getGType, "parent of a D subclass of a D subclass");
  assert(DPartialPlain._getGType == DPartial._getGType, "D subclass without the mixin uses the base GType");

  GTypeQuery dQuery, cQuery;
  g_type_query(DChild._getGType, &dQuery);
  g_type_query(FeatObject._getGType, &cQuery);
  assert(dQuery.classSize == cQuery.classSize && dQuery.instanceSize == cQuery.instanceSize, "parent sizes");

  auto obj = new DChild(2);
  assert(instType(obj._cPtr) == DChild._getGType && obj._gType == DChild._getGType);
  assert(obj._gTypeName == "GidgenTestDChild");
  assert(obj.constructedBound && obj.ctorRan, "vfuncConstructed reaches the D object during g_object_new");
  assert(obj.getRefCount == 1, "only the toggle reference remains after construction");

  auto plain = new DPartialPlain;
  assert(instType(plain._cPtr) == DPartial._getGType && plain.hook == 5 && plain.getValue == 4);

  destroy(obj);
  destroy(plain);
}

private int collideCriticals;

private extern(C) void countCollideCritical(const(char)* domain, GLogLevelFlags level, const(char)* message, void* data)
  nothrow
{
  collideCriticals++;
}

/// A GType name which is already used is not registered (no suffix), the first of two classes with the same name
/// still works, and constructing the failed class does not create a GObject
unittest
{
  import gid.glib2.c.functions : g_log_remove_handler, g_log_set_always_fatal, g_log_set_handler;
  import gid.glib2.c.types : GLogLevelFlags;
  import std.string : fromStringz;

  auto oldFatal = g_log_set_always_fatal(GLogLevelFlags.LevelError | GLogLevelFlags.FlagFatal);
  auto handler = g_log_set_handler("Gid", GLogLevelFlags.LevelCritical, &countCollideCritical, null);
  scope(exit)
  {
    g_log_remove_handler("Gid", handler);
    g_log_set_always_fatal(oldFatal);
  }

  gidgen_feat_object_get_type();
  assert(DCollide._getGType == 0, "colliding name is not registered");
  assert(collideCriticals == 1);
  assert(DCollide._getGType == 0 && collideCriticals == 1, "critical logged once");

  auto bad = new DCollide; // Does not create a FeatObject
  assert(bad._cPtr is null);
  assert(collideCriticals == 2, "construction logs a critical and returns no instance");

  auto aType = DSameA._getGType;
  assert(aType != 0 && g_type_name(aType).fromStringz == "GidgenTestSameName", "first class registers the name");
  auto a = new DSameA;
  assert(instType(a._cPtr) == aType);

  assert(DSameB._getGType == 0 && collideCriticals == 3, "second class with the same name fails once");
  assert(DSameB._getGType == 0 && collideCriticals == 3, "not logged again");
  auto b = new DSameB;
  assert(b._cPtr is null && collideCriticals == 4);

  auto a2 = new DSameA;
  assert(instType(a2._cPtr) == aType, "first class still works");

  destroy(a);
  destroy(a2);
}

/// Instances of D subclass GTypes created from C get a D object in instance_init
unittest
{
  auto start = liveCount(Counter.Object);

  // D class with a default constructor: the constructor runs and binds the C instance
  auto cObj = cast(GidgenFeatObject*)g_object_new_with_properties(DPartial._getGType, 0, null, null);
  auto dObj = cast(DPartial)GObj.ObjectWrap._vfuncDObject(cObj);
  assert(dObj && dObj._cPtr is cObj, "D object bound in instance_init");
  assert(gidgen_feat_object_get_value(cObj) == 3, "D default constructor ran");
  assert(gidgen_feat_object_hook(cObj) == 5, "C call reaches the D override");
  assert(GObj.ObjectWrap._getDObject!FeatObject(cObj, No.Take) is dObj, "wrapping returns the bound D object");
  assert((cast(GObject*)cObj).refCount == 2, "C reference and toggle reference");
  g_object_unref(cast(GObject*)cObj); // D object is now the only owner
  destroy(dObj);

  // D class without a default constructor: D object created without running a constructor
  auto cObj2 = cast(GidgenFeatObject*)g_object_new_with_properties(DChild._getGType, 0, null, null);
  auto dObj2 = cast(DChild)GObj.ObjectWrap._vfuncDObject(cObj2);
  assert(dObj2 && !dObj2.ctorRan, "D object created without a constructor");
  assert(dObj2.constructedBound, "vfuncConstructed reaches the D object of a C created instance");
  gidgen_feat_object_set_value(cObj2, 3);
  assert(gidgen_feat_object_compute(cObj2, 5) == 160, "C call reaches the DChild override chain");
  g_object_unref(cast(GObject*)cObj2);
  destroy(dObj2);

  assert(liveCount(Counter.Object) == start, "objects leaked");
}

/// D subclass with a default constructor (so that C created instances get a D object) overriding vfuncs of C classes
class DDefault : FeatObject
{
  mixin GObjectSubclass!"GidgenTestDDefault";

  int constructedCalls;
  int absorbCalls;

  override protected void vfuncConstructed()
  {
    constructedCalls++;
    super.vfuncConstructed(); // Chain up to the implementation of the nearest ancestor which is not a D type
  }

  override protected int vfuncCompute(int x)
  {
    return super.vfuncCompute(x) + 1;
  }

  override protected void vfuncAbsorb(FeatObject other)
  {
    absorbCalls++;
    super.vfuncAbsorb(other);
  }

  override protected bool vfuncCheck(int value)
  {
    return value != 7 && super.vfuncCheck(value);
  }
}

/// Second level of DDefault
class DDefaultChild : DDefault
{
  mixin GObjectSubclass!"GidgenTestDDefaultChild";

  override protected int vfuncCompute(int x)
  {
    return super.vfuncCompute(x) * 10;
  }
}

private __gshared int cConstructedCalls;
private __gshared int cComputeCalls;
private __gshared int cCheckCalls;
private __gshared int cAbsorbCalls;

// Class of the parent type of the type of an instance
private void* parentClassOf(void* inst) nothrow
{
  return g_type_class_peek_parent((cast(GTypeInstance*)inst).gClass);
}

private extern(C) void cConstructed(GObject* obj) nothrow
{
  alias Fn = extern(C) void function(GObject*) nothrow;
  cConstructedCalls++;
  (cast(Fn)(cast(GObjectClass*)parentClassOf(obj)).constructed)(obj); // Chain up to the D type
}

private extern(C) int cCompute(GidgenFeatObject* self, int x) nothrow
{
  alias Fn = extern(C) int function(GidgenFeatObject*, int) nothrow;
  cComputeCalls++;
  return (cast(Fn)(cast(GidgenFeatObjectClass*)parentClassOf(self)).compute)(self, x) + 1000;
}

private extern(C) int cCheck(GidgenFeatObject* self, int value, GError** error) nothrow
{
  alias Fn = extern(C) int function(GidgenFeatObject*, int, GError**) nothrow;
  cCheckCalls++;
  return (cast(Fn)(cast(GidgenFeatObjectClass*)parentClassOf(self)).check)(self, value, error);
}

private extern(C) void cAbsorb(GidgenFeatObject* self, GidgenFeatObject* other) nothrow
{
  alias Fn = extern(C) void function(GidgenFeatObject*, GidgenFeatObject*) nothrow;
  cAbsorbCalls++;
  (cast(Fn)(cast(GidgenFeatObjectClass*)parentClassOf(self)).absorb)(self, other);
}

private extern(C) void cDerivedClassInit(void* klass, void* data) nothrow
{
  (cast(GObjectClass*)klass).constructed = cast(typeof(GObjectClass.constructed))&cConstructed;
  auto fc = cast(GidgenFeatObjectClass*)klass;
  fc.compute = cast(typeof(fc.compute))&cCompute;
  fc.check = cast(typeof(fc.check))&cCheck;
  fc.absorb = cast(typeof(fc.absorb))&cAbsorb;
}

private GType registerCType(string name, GType parent)
{
  GTypeQuery q;
  g_type_query(parent, &q);
  return g_type_register_static_simple(parent, name.toStringz, q.classSize, &cDerivedClassInit, q.instanceSize, null,
    cast(GTypeFlags)0);
}

/// A type registered in C code from a D type calls the D overrides, which chain up to the nearest non D type
unittest
{
  auto plain = new FeatObject;
  plain.setValue(3);
  auto base = gidgen_feat_object_compute(cast(GidgenFeatObject*)plain._cPtr, 5); // C implementation: value * x
  assert(base == 15);

  foreach (isChild; [false, true])
  {
    auto dType = isChild ? DDefaultChild._getGType : DDefault._getGType;
    // Plain instances of the D types (no C subclass) use the D overrides
    auto d = cast(GidgenFeatObject*)g_object_new(dType, "value", 3, null);
    auto dObj = cast(DDefault)GObj.ObjectWrap._vfuncDObject(d);
    assert(dObj !is null && dObj.constructedCalls == 1);
    auto expected = isChild ? (base + 1) * 10 : base + 1;
    assert(gidgen_feat_object_compute(d, 5) == expected);
    g_object_unref(cast(GObject*)d);

    auto cType = registerCType(isChild ? "GidgenTestCDDefaultChild" : "GidgenTestCDDefault", dType);
    assert(cType);

    cConstructedCalls = cComputeCalls = cCheckCalls = cAbsorbCalls = 0;
    auto c = cast(GidgenFeatObject*)g_object_new(cType, "value", 3, null);
    assert(cConstructedCalls == 1, "C override of constructed called once and chained up without recursion");
    auto cObj = cast(DDefault)GObj.ObjectWrap._vfuncDObject(c);
    assert(cObj !is null && cObj.constructedCalls == 1, "D override of constructed called by the C override");
    assert(g_type_is_a((cast(GTypeInstance*)c).gClass.gType, dType));

    assert(gidgen_feat_object_compute(c, 5) == expected + 1000, "C, then D overrides, then the C implementation");
    assert(cComputeCalls == 1);

    GError* err;
    assert(gidgen_feat_object_check(c, 4, &err) && err is null);
    assert(!gidgen_feat_object_check(c, 7, &err) && err is null, "D override rejects 7 before the C implementation");
    assert(!gidgen_feat_object_check(c, -1, &err) && err !is null, "C implementation of the base class chained to");
    g_error_free(err);
    assert(cCheckCalls == 3);

    auto other = new FeatObject;
    other.setValue(4);
    gidgen_feat_object_absorb(c, cast(GidgenFeatObject*)other._cPtr);
    assert(cAbsorbCalls == 1 && cObj.absorbCalls == 1);
    assert(gidgen_feat_object_get_value(c) == 7, "C implementation of absorb chained to");
    g_object_unref(cast(GObject*)c);
  }
}
