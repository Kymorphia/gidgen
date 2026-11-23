/// GidgenFeat GObject class tests (constructors, wrapper identity, signals, interfaces, virtual methods)
module feat_object;

import gid.gidgenfeat1.global : liveCount;
import gid.gidgenfeat1.iface;
import gid.gidgenfeat1.object : FeatObject = ObjectWrap;
import gid.gidgenfeat1.types : Counter;

/// Constructors and builder
unittest
{
  auto start = liveCount(Counter.Object);
  auto obj = new FeatObject;
  assert(obj.getRefCount == 1, "new object ref count");

  auto withValue = FeatObject.newWithValue(3);
  assert(withValue.getValue == 3 && withValue.getRefCount == 1);

  auto built = FeatObject.builder.value(11).name("built").build;
  assert(built.getValue == 11 && built.getName == "built");

  destroy(obj);
  destroy(withValue);
  destroy(built);
  assert(liveCount(Counter.Object) == start);
}

/// The same C instance maps to the same D object
unittest
{
  auto start = liveCount(Counter.Object);
  auto none = FeatObject.returnNone; // Library keeps a singleton reference
  auto none2 = FeatObject.returnNone;
  assert(none is none2, "same C instance should map to the same D object");
  destroy(none);
  FeatObject.resetSingleton;
  assert(liveCount(Counter.Object) == start, "singleton leaked");
}

/// Signal handler with fewer parameters than the signal
unittest
{
  auto obj = new FeatObject;
  int got = -1, count;
  obj.connectChanged((int v) { got = v; count++; });
  obj.connectChanged(() { count++; });
  obj.emitChanged(42);
  assert(got == 42 && count == 2);
  destroy(obj);
}

/// Interface implemented by a class
unittest
{
  auto obj = FeatObject.newWithValue(2);
  Iface iface = obj;
  assert(iface.getId == 200);
  destroy(obj);
}

/// Virtual method implemented in C (D overrides are not supported yet)
unittest
{
  auto obj = FeatObject.newWithValue(6);
  assert(obj.compute(7) == 42);
  destroy(obj);
}

/// Virtual methods: generated vfunc* methods call the C implementation of the instance's class
unittest
{
  import gid.gidgenfeat1.c.functions : gidgen_feat_child_object_new, gidgen_feat_object_new_with_value;
  import gid.gidgenfeat1.child_object : ChildObject;
  import std.typecons : Yes;

  // D subclasses which expose the protected vfunc methods (they have no D subclass GType with installed overrides,
  // so only D calls dispatch to the overrides, see feat_vfunc for installed overrides)
  static class VfuncObject : FeatObject
  {
    this(int value)
    {
      super(gidgen_feat_object_new_with_value(value), Yes.Take);
    }

    override protected int vfuncCompute(int x)
    {
      return super.vfuncCompute(x) + 1; // Chain up to the C implementation
    }

    int callCompute(int x) => vfuncCompute(x);
    void callAbsorb(FeatObject other) => vfuncAbsorb(other);
    bool callDivide(int divisor, out int quotient, out int remainder) => vfuncDivide(divisor, quotient, remainder);
    int callHook() => vfuncHook;
  }

  static class VfuncChild : ChildObject
  {
    this(int value)
    {
      super(gidgen_feat_child_object_new(value), Yes.Take);
    }

    void callAbsorb(FeatObject other) => vfuncAbsorb(other);
    int callHook() => vfuncHook;
  }

  auto obj = new VfuncObject(6);
  assert(obj.callCompute(7) == 43, "D override chained up to the C compute implementation");
  assert(obj.compute(7) == 42, "invoker calls the C implementation");

  auto other = FeatObject.newWithValue(4);
  obj.callAbsorb(other); // Void virtual method with an object parameter
  assert(obj.getValue == 10);

  int quotient, remainder;
  assert(obj.callDivide(3, quotient, remainder) && quotient == 3 && remainder == 1, "out parameters and return value");
  assert(!obj.callDivide(0, quotient, remainder));

  assert(obj.callHook == 0, "NULL C implementation returns a default value");

  auto child = new VfuncChild(1);
  child.callAbsorb(other); // Uses the C class implementation of the instance's actual type (GidgenFeatChildObject)
  assert(child.getValue == 9);
  assert(child.callHook == 7);

  destroy(obj);
  destroy(other);
  destroy(child);
}
