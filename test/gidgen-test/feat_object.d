/// GidgenFeat GObject class tests (constructors, wrapper identity, signals, interfaces, virtual methods)
module feat_object;

import gidgenfeat.global : liveCount;
import gidgenfeat.iface;
import gidgenfeat.object : FeatObject = ObjectWrap;
import gidgenfeat.types : Counter;

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
