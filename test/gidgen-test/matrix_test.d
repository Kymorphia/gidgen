/**
 * Generic GidgenMatrix unittest, written once for all matrix cells.
 *
 * The cells are listed in the generated matrix_manifest.d (from matrix-gen's type and position tables). For each
 * cell the gidgen generated D function is looked up in gidgenmatrix.global by name, the D value type `T` is taken
 * from the function signature with std.traits, and the function is called according to the cell's position. The C
 * side verifies received values (failures are counted by the library) and returns known values which are checked
 * here. Live counters of the instance test types verify that nothing is leaked or freed too often.
 */
module matrix_test;

import std.algorithm : startsWith;
import std.array : join;
import std.meta : Filter;
import std.traits : isDynamicArray, Parameters, ReturnType;
import std.conv : to;

import glib.c.functions : g_quark_from_string, g_set_error_literal;
import glib.c.types : GError, GLogLevelFlags;
import gobject.object : ObjectWrap;
import gidgenmatrix.global;
import gidgenmatrix.types : Counter;
import matrix_manifest;

/// Destroy a D value which wraps a C instance, or the elements of an array or associative array of them (no-op for
/// other values)
private void release(T)(ref T v)
{
  static if (is(T == class))
  {
    if (v)
      destroy(v);
  }
  else static if (is(T : E[], E) || is(T : E[K], E, K))
  {
    static if (is(E == class))
      foreach (ref e; v)
        release(e);
  }
}

/// Create a container element from its integer value (instance, structure or enum)
private E makeElem(E)(int v)
{
  static if (is(E == class))
    return new E(v);
  else static if (is(E == struct))
    return E(v);
  else
    return cast(E)v;
}

/// Check if a container element has an integer value (see makeElem)
private bool elemEq(E)(E e, int v)
{
  static if (is(E == class))
    if (!e)
      return false;

  static if (__traits(hasMember, E, "getValue"))
    return e.getValue == v;
  else static if (__traits(hasMember, E, "value"))
    return e.value == v;
  else
    return e == v;
}

/// Create an array or associative array of elements from their integer values (container type table entries)
private T makeElems(T, V)(V vals)
{
  T result;

  static if (is(T : E[], E))
  {
    foreach (v; vals)
      result ~= makeElem!E(v);
  }
  else static if (is(T : E[K], E, K))
  {
    foreach (k, v; vals)
      result[k] = makeElem!E(v);
  }

  return result;
}

/// Compare an array or associative array of elements with their integer values
private bool elemsEq(T, V)(T a, V vals)
{
  if (a.length != vals.length)
    return false;

  foreach (k, v; vals)
  {
    static if (is(T : E[K], E, K) && !is(T : E[], E))
    {
      if (k !in a)
        return false;
    }

    if (!elemEq(a[k], v))
      return false;
  }

  return true;
}

/// What the delegate of a cb_inout cell does with the value (GM_INOUT_* in gidgen-matrix-private.h)
private enum InOutMode : int
{
  Replace = 0, /// Replace it with the In value
  Keep = 1, /// Keep it
  SetNull = 2, /// Set it to null
}

/// Live instance count of the cell type's C instances (0 for types without a counter)
private int typeLive(MatrixCell c)()
{
  static if (c.counter.length)
    return liveCount(mixin("Counter." ~ c.counter));
  else
    return 0;
}

/**
 * Check the ownership of a value received from C (out, inout and return values), base is the live count before the
 * value was received. Full values are taken without a copy or an added reference, borrowed (transfer none) values
 * are copied or referenced while the library keeps its own (see KEEP in gidgen-matrix-private.h), except for
 * opaque records which are borrowed. Value structs hold no C instance, so only the library's kept one remains.
 */
private void checkReceived(MatrixCell c, T)(T a, int base)
{
  enum full = c.transfer == "full";

  static if (c.own == "Ref")
  {
    assert(typeLive!c - base == 1, c.func ~ ": received object live count " ~ (typeLive!c - base).to!string);
    assert(a.getRefCount == (full ? 1 : 2), c.func ~ ": received object ref count " ~ a.getRefCount.to!string);
  }
  else static if (c.own == "Copy")
    assert(typeLive!c - base == (full ? 1 : 2), c.func ~ ": received record live count " ~ (typeLive!c - base).to!string);
  else static if (c.own == "Borrow")
    assert(typeLive!c - base == 1, c.func ~ ": received opaque live count " ~ (typeLive!c - base).to!string);
  else static if (c.own == "Value")
    assert(typeLive!c - base == (full ? 0 : 1), c.func ~ ": received struct live count " ~ (typeLive!c - base).to!string);
}

/**
 * Check the ownership of value v after it was passed to C or stored in a field or property (stored), base is the
 * live count before v was created. Passed values must keep their own instance (the callee takes a reference or copy
 * for transfer full), stored values are referenced or copied by the record or object.
 */
private void checkPassed(MatrixCell c, T)(T v, int base, bool stored)
{
  static if (c.own == "Ref")
    assert(v.getRefCount == (stored ? 2 : 1), c.func ~ ": passed object ref count " ~ v.getRefCount.to!string);
  else static if (c.own == "Copy" || c.own == "Borrow")
    assert(typeLive!c - base == (stored ? 2 : 1), c.func ~ ": passed record live count " ~ (typeLive!c - base).to!string);
  else static if (c.own == "Value")
    assert(typeLive!c - base == (stored ? 1 : 0), c.func ~ ": passed struct live count " ~ (typeLive!c - base).to!string);
}

/**
 * Arguments of a function with an array size parameter: the array argument for the array parameter, the size for
 * the integral parameter and isNull for the leading null flag (nullable cells).
 */
private string sizeArgs(Ps...)(string array)
{
  string[] args;
  static foreach (i, P; Ps)
  {
    static if (is(P == bool))
      args ~= "isNull";
    else static if (isDynamicArray!P)
      args ~= array;
    else
      args ~= "cast(Params[" ~ i.to!string ~ "])len";
  }
  return args.join(", ");
}

/**
 * Test a structure borrowed from the fields record (see the borrow positions in position_table.d): the D wrapper must
 * point at the C structure, writes must reach it and it must keep the record alive after the record's D object is
 * dropped, until the wrapper is dropped too. Separate function frames keep dropped references off the scanned stack.
 */
private void testBorrow(MatrixCell c, T)()
{
  import core.memory : GC;
  import gidgenmatrix.types : Counter;

  auto base = liveCount(Counter.Fields);
  auto a = getBorrowed!(c, T);
  GC.collect;
  assert(liveCount(Counter.Fields) == base + 1, c.func ~ ": owning record not kept alive by the borrowed structure");
  assert(a.value == mixin(c.makeOut).value, c.func ~ ": borrowed structure value invalid after its owner was dropped");
  a = null;
  clearStack;
  GC.collect;
  assert(liveCount(Counter.Fields) == base, c.func ~ ": owning record not freed after the borrowed structure");
}

/// Get a borrowed structure from a new fields record (dropped on return) and check identity and writes (see testBorrow)
private T getBorrowed(MatrixCell c, T)()
{
  import gidgenmatrix.fields : Fields;

  auto o = new Fields;
  T a;

  static if (c.pos == "borrow_field")
    a = mixin("o." ~ c.member);
  else static if (c.pos == "borrow_out")
    mixin("o." ~ c.func ~ "(a);");
  else static if (c.pos == "borrow_owner_ret") // Function (not a method) with an owner parameter
    a = mixin(c.func ~ "(o)");
  else
    a = mixin("o." ~ c.func ~ "()");

  assert(a._cPtr is o.addr(c.member), c.func ~ ": borrowed structure is not the C structure");
  testRefusedStore!c(a);
  a.value = mixin(c.makeIn).value;

  static if (c.pos == "borrow_field") // C checks the In value and sets the Out value
    mixin("o." ~ c.func ~ "();");
  else
  {
    assert((cast(typeof(T._cInstance)*)o.addr(c.member)).value == a.value, c.func ~ ": write did not reach C");
    a.value = mixin(c.makeOut).value;
  }

  assert(a.value == mixin(c.makeOut).value, c.func ~ ": C write not visible through the borrowed structure");
  return a;
}

/// Test a caller allocated out structure filled by a method: the copy keeps the instance alive (see fill_out_ca)
private void testFillOut(MatrixCell c, T)()
{
  import core.memory : GC;

  auto base = liveCount(Counter.Fields);
  auto a = getFilled!(c, T);
  GC.collect;
  assert(liveCount(Counter.Fields) == base + 1, c.func ~ ": instance not kept alive by the filled structure");
  a = null;
  clearStack;
  GC.collect;
  assert(liveCount(Counter.Fields) == base, c.func ~ ": instance not freed after the filled structure");
}

/// Fill a structure from a new fields record (dropped on return, see testFillOut)
private T getFilled(MatrixCell c, T)()
{
  import gidgenmatrix.fields : Fields;

  auto o = new Fields;
  T a;
  mixin("o." ~ c.func ~ "(a);");
  assert(mixin(c.eqOut) && !a._cBorrowed && a._owner is o, c.func ~ ": unexpected filled structure");
  return a;
}

/**
 * Test a map position: the MapScope holds the mapped structure, keeps the source fields record alive while mapped
 * and unmaps it once, on explicit unmap or at scope exit (the C unmap fails a check if called twice).
 */
private void testMap(MatrixCell c)()
{
  import core.memory : GC;

  auto maps = liveCount(Counter.Maps);
  auto fields = liveCount(Counter.Fields);
  mapExplicit!c(maps, fields); // Separate function frames keep dropped references off the scanned stack
  mapScoped!c();
  assert(liveCount(Counter.Maps) == maps, c.func ~ ": not unmapped at scope exit");
  clearStack;
  GC.collect;
  assert(liveCount(Counter.Fields) == fields, c.func ~ ": source not freed after unmap");
}

/// MapScope type of a map position cell
private template MapScopeOf(MatrixCell c)
{
  import gidgenmatrix.fields : Fields;

  import std.uni : toUpper;

  static if (c.pos == "map_method") // Fields has map functions of several types: "<FuncName>Scope" aliases
    alias MapScopeOf = mixin("Fields." ~ c.func[0 .. 1].toUpper ~ c.func[1 .. $] ~ "Scope");
  else // Global map function: "MapScope" alias of the mapped structure class
    alias MapScopeOf = Parameters!(__traits(getMember, gidgenmatrix.global, c.inFunc))[$ - 1].MapScope;
}

/// Map a new fields record with a map position cell
private bool doMap(MatrixCell c)(out MapScopeOf!c m)
{
  import gidgenmatrix.fields : Fields;

  auto o = new Fields;

  static if (c.pos == "map_method")
    return mixin("o." ~ c.func ~ "(m)");
  else
    return mixin(c.func ~ "(o, m)");
}

/// Map with explicit unmap (see testMap)
private void mapExplicit(MatrixCell c)(int maps, int fields)
{
  import core.memory : GC;

  MapScopeOf!c m;
  assert(doMap!c(m) && m.mapped, c.func ~ ": map failed");
  {
    auto a = m.value;
    assert(mixin(c.eqOut), c.func ~ ": unexpected mapped value");
  }
  assert(liveCount(Counter.Maps) == maps + 1, c.func ~ ": not mapped");
  clearStack;
  GC.collect;
  assert(liveCount(Counter.Fields) == fields + 1, c.func ~ ": source not kept alive while mapped");
  m.unmap;
  assert(!m.mapped && liveCount(Counter.Maps) == maps, c.func ~ ": not unmapped");
  m.unmap; // No second unmap (the C unmap check would fail)
}

/// Map and unmap at scope exit (see testMap)
private void mapScoped(MatrixCell c)()
{
  MapScopeOf!c m;
  assert(doMap!c(m), c.func ~ ": map failed");
}

/// Count of critical "Gid" log messages (see refusedStoreHandler)
private __gshared int refusedStores;

private extern(C) void refusedStoreHandler(const(char)* domain, GLogLevelFlags level, const(char)* msg, void* data) nothrow
{
  refusedStores++;
}

/**
 * A borrowed structure must refuse setters which store D memory (only kept alive by the wrapper, which the C structure
 * may outlive): a critical message is logged and the C field is left unchanged.
 */
private void testRefusedStore(MatrixCell c, T)(T a)
{
  import glib.c.functions : g_log_remove_handler, g_log_set_always_fatal, g_log_set_handler;

  static if (__traits(compiles, a.name = ""))
  {
    auto cStruct = cast(typeof(T._cInstance)*)a._cPtr;
    auto cName = cStruct.name;
    auto count = refusedStores;
    auto fatalMask = g_log_set_always_fatal(GLogLevelFlags.LevelError); // Not fatal with G_DEBUG=fatal-criticals
    auto id = g_log_set_handler("Gid", GLogLevelFlags.LevelCritical, &refusedStoreHandler, null);
    a.name = "refused";
    g_log_remove_handler("Gid", id);
    g_log_set_always_fatal(fatalMask);
    assert(cStruct.name is cName && refusedStores == count + 1, c.func ~ ": borrowed structure stored D memory");
  }
}

/// Check if T is a C allocated Wrap class with an init function constructor (gid-init-function, no field arguments)
private enum isInitWrap(T) = is(T == class) && __traits(hasMember, T, "_cAllocated") && hasNoArgCtor!T;

private template hasNoArgCtor(T)
{
  static if (__traits(hasMember, T, "__ctor"))
  {
    enum hasNoArgCtor = ()
    {
      bool found;
      static foreach (ctor; __traits(getOverloads, T, "__ctor"))
        found = found || Parameters!ctor.length == 0;
      return found;
    }();
  }
  else
    enum hasNoArgCtor = false;
}

/// Create a C allocated Wrap with an init function (see isInitWrap) and set its value and owned name field
private T makeInitWrap(T)(int v)
{
  auto a = new T;
  assert(a._cOwned && !a._cBorrowed && a.name == "init", T.stringof ~ ": constructor did not initialize a C allocated instance");
  a.value = v;
  a.name = "iw";
  return a;
}

/**
 * Owned string fields of a C allocated Wrap (see isInitWrap): setters store C copies freed by the structure (also
 * through a borrowed wrapper, which must not refuse them), copies are deep and independent.
 */
private void testOwnedFields(MatrixCell c, T)(T a)
{
  static if (isInitWrap!T && __traits(compiles, a.name = ""))
  {
    auto cStruct = cast(typeof(T._cInstance)*)a._cPtr;
    auto gcName = "owned".dup;
    a.name = cast(string)gcName;
    assert(cStruct.name !is gcName.ptr && a.name == "owned",
      c.func ~ ": owned field setter did not store a C copy");

    auto b = new T(a._cPtr, cast(Object)a); // Borrowed wrapper of the same C structure
    b.name = "borrowed";
    assert(a.name == "borrowed", c.func ~ ": owned field set through a borrowed wrapper not stored");
    b.name = null;
    assert(cStruct.name is null && a.name is null, c.func ~ ": owned field not cleared");

    a.name = "orig";
    auto d = a._dup;
    assert(d._cOwned && d._cPtr !is a._cPtr && d.name == "orig" && d.value == a.value, c.func ~ ": dup is not a deep copy");
    d.name = "changed";
    assert(a.name == "orig", c.func ~ ": dup shares the owned field");
    release(d);

    static if (__traits(hasMember, T, "copy")) // C copy function bound as a method returns an owned deep copy
    {
      auto e = a.copy;
      assert(e._cOwned && e._cPtr !is a._cPtr && e.name == "orig", c.func ~ ": copy is not an owned deep copy");
      release(e);
    }

    release(b);
  }
}

/// Overwrite unused stack below the caller with zeros, so stale references aren't found by the conservative GC
private void clearStack()
{
  ubyte[4096] buf = 0;
  import core.volatile : volatileStore;
  volatileStore(&buf[0], 0);
}

/// Test a single matrix cell
private void testCell(MatrixCell c)()
{
  static if (c.pos.startsWith("self_") || c.pos.startsWith("borrow_") || c.pos == "map_method" || c.pos == "fill_out_ca") // Methods of the type (c.member) or the
    // fields record (c.func), T is the D value type of the type's in cell
    alias T = Parameters!(__traits(getMember, gidgenmatrix.global, c.inFunc))[$ - 1];
  else
  {
    alias fn = __traits(getMember, gidgenmatrix.global, c.func);
    alias Params = Parameters!fn;
  }

  static if (c.nullable)
    enum nullArgs = "false, ";
  else
    enum nullArgs = "";

  static if (c.pos == "in")
  { // Pass the In value, it must still be valid afterwards (D keeps its own reference for transfer full)
    alias T = Params[$ - 1];
    auto base = typeLive!c;
    T v = mixin(c.makeIn);
    mixin("fn(" ~ nullArgs ~ "v);");
    { auto a = v; assert(mixin(c.eqIn), c.func ~ ": in value modified"); }
    checkPassed!c(v, base, false);
    static if (c.nullable)
    {
      fn(true, null);
      fn(true); // Trailing nullable parameters default to null
    }
    testOwnedFields!(c, T)(v);
    release(v);

    static if (is(T == class) && c.transfer == "full" && !c.nullable)
    { // Steal the C instance of a new D value: owning wrappers hand over their instance and are left empty, GObject
      // wrappers return a new reference (see GidWrap._cSteal), freeing the stolen instance releases it
      auto stealBase = typeLive!c;
      T s = mixin(c.makeIn);
      void* p = s._cSteal;
      assert(p !is null, c.func ~ ": stole null instance");
      static if (is(T : ObjectWrap))
        assert(s._cPtr is p, c.func ~ ": object wrapper changed by steal");
      else
        assert(s._cPtr is null, c.func ~ ": wrapper not empty after steal");
      T._cFree(p);
      release(s);
      assert(typeLive!c == stealBase, c.func ~ ": steal live count " ~ (typeLive!c - stealBase).to!string);
    }
  }
  else static if (c.pos == "out" || c.pos == "out_ca")
  { // Receive the Out value
    alias T = Params[$ - 1];
    T a;
    static if (c.pos == "out_ca" && isDynamicArray!T) // Caller allocated arrays are sized by the caller
      a.length = mixin(c.makeOut).length;
    auto base = typeLive!c;
    mixin("fn(" ~ nullArgs ~ "a);");
    assert(mixin(c.eqOut), c.func ~ ": unexpected out value");
    static if (c.pos == "out")
      checkReceived!c(a, base);
    static if (c.pos == "out_ca" && isInitWrap!T) // Callee filled C allocated structure taken by the wrapper
      assert(a._cOwned && !a._cBorrowed && !a._owner, c.func ~ ": caller allocated structure not owned by the wrapper");
    release(a);

    static if (c.nullable)
    {
      T n;
      fn(true, n);
      assert(n is null, c.func ~ ": expected null out value");
    }
  }
  else static if (c.pos == "inout_ca")
  { // Pass the In value which is replaced with the Out value in place
    alias T = Params[$ - 1];
    T a = mixin(c.makeIn);
    mixin("fn(a);");
    assert(mixin(c.eqOut), c.func ~ ": unexpected inout value");
    release(a);
  }
  else static if (c.pos == "inout")
  { // Pass the In value and receive the Out value in the same variable
    alias T = Params[$ - 1];
    T orig = mixin(c.makeIn);
    T a = orig;
    auto base = typeLive!c;
    mixin("fn(" ~ nullArgs ~ "a);");
    assert(mixin(c.eqOut), c.func ~ ": unexpected inout value");
    checkReceived!c(a, base);
    release(a);
    release(orig);

    static if (c.nullable)
    {
      T n = null;
      fn(true, n);
      assert(n is null, c.func ~ ": expected null inout value");
    }
  }
  else static if (c.pos == "ret_static")
  { // Receive the library kept Ret value twice, D has its own copies or references which are independent of the
    // library's instance
    alias T = ReturnType!fn;
    auto base = typeLive!c;
    T first = fn();
    T second = fn();
    { auto a = first; assert(mixin(c.eqRet), c.func ~ ": unexpected return value"); }
    { auto a = second; assert(mixin(c.eqRet), c.func ~ ": unexpected second return value"); }

    static if (is(T : ObjectWrap))
      assert(first is second, c.func ~ ": same C instance returned a different D object");

    static if (c.own == "Ref")
      assert(first.getRefCount == (is(T : ObjectWrap) ? 2 : 3), c.func ~ ": ref count "
        ~ first.getRefCount.to!string);
    else static if (c.own == "Copy")
      assert(typeLive!c - base == 3, c.func ~ ": record live count " ~ (typeLive!c - base).to!string);
    else static if (c.own == "Borrow" || c.own == "Value")
      assert(typeLive!c - base == 1, c.func ~ ": live count " ~ (typeLive!c - base).to!string);

    release(first);
    release(second);
    { T a = fn(); assert(mixin(c.eqRet), c.func ~ ": library value invalid after D released its values"); release(a); }
  }
  else static if (c.pos == "self_full" || c.pos == "self_value")
  { // Call a method of the In value which takes the instance with transfer full or by value, it must still be valid
    T v = mixin(c.makeIn);
    auto base = typeLive!c - 1;
    mixin("v." ~ c.member ~ "();");
    { auto a = v; assert(mixin(c.eqIn), c.func ~ ": instance modified"); }
    checkPassed!c(v, base, false);
    release(v);
  }
  else static if (c.pos == "self_borrow" || c.pos.startsWith("borrow_"))
    testBorrow!(c, T);
  else static if (c.pos == "cb_inout")
  { // The delegate receives the Out value (null for nullable cells with isNull) and replaces it with the In value,
    // keeps it or sets it to null (nullable cells), the C function checks the resulting value (see InOutMode)
    alias T = Parameters!(Params[$ - 1])[0];
    enum nullable = c.nullable;
    int calls;

    foreach (isNull; nullable ? [false, true] : [false])
    {
      foreach (mode; nullable ? [InOutMode.Replace, InOutMode.Keep, InOutMode.SetNull]
        : [InOutMode.Replace, InOutMode.Keep])
      {
        releaseKept; // Release the library value of a previous call
        auto base = typeLive!c;
        T v = mixin(c.makeIn);
        auto receiveBase = typeLive!c;
        T keptVal;

        void dlg(ref T a)
        {
          static if (nullable)
            assert(isNull ? a is null : mixin(c.eqOut), c.func ~ ": unexpected callback value");
          else
            assert(mixin(c.eqOut), c.func ~ ": unexpected callback value");

          static if (is(T == class)) // Value structs are modified in place (C keeps the instance)
            if (!isNull)
              checkReceived!c(a, receiveBase);

          final switch (mode)
          {
            case InOutMode.Replace:
              release(a);
              a = v;
              break;
            case InOutMode.Keep:
              keptVal = a;
              break;
            case InOutMode.SetNull:
              static if (nullable)
              {
                release(a);
                a = null;
              }
              break;
          }

          calls++;
        }

        static if (nullable)
          fn(isNull, mode, &dlg);
        else
          fn(mode, &dlg);

        // The C function released its value (a reference or copy of the D value), D still owns its own instances
        static if (c.own == "Ref")
          assert(!keptVal || keptVal.getRefCount == 1, c.func ~ ": kept object ref count " ~ keptVal.getRefCount.to!string);
        release(keptVal);
        checkPassed!c(v, base + (c.transfer == "none" && c.own == "Value" ? 1 : 0), false);
        release(v);
        releaseKept; // Release the library's transfer none value
        assert(typeLive!c == base, c.func ~ ": callback inout live count " ~ (typeLive!c - base).to!string);
      }
    }

    assert(calls == (nullable ? 6 : 2), c.func ~ ": callback not called");
  }
  else static if (c.pos == "ret_len_in" || c.pos == "out_len_in")
  { // Receive an array with a size passed by the caller (an empty or null array for nullable cells with isNull)
    static if (c.pos == "ret_len_in")
      alias T = ReturnType!fn;
    else
      alias T = Filter!(isDynamicArray, Params)[0];

    T e = mixin(c.pos == "ret_len_in" ? c.makeRet : c.makeOut);
    immutable valLen = e.length; // Array size passed to the function
    release(e);

    foreach (isNull; c.nullable ? [false, true] : [false])
    {
      auto len = isNull ? 0 : valLen;
      auto base = typeLive!c;

      static if (c.pos == "ret_len_in")
        mixin("T a = fn(" ~ sizeArgs!Params("") ~ ");");
      else
      {
        T a;
        mixin("fn(" ~ sizeArgs!Params("a") ~ ");");
      }

      if (isNull)
        assert(a.length == 0, c.func ~ ": expected empty array");
      else
      {
        static if (c.pos == "ret_len_in")
          assert(mixin(c.eqRet), c.func ~ ": unexpected return value");
        else
          assert(mixin(c.eqOut), c.func ~ ": unexpected out value");
        checkReceived!c(a, base);
      }
      release(a);
    }
  }
  else static if (c.pos == "ret")
  { // Receive the Ret value
    alias T = ReturnType!fn;
    auto base = typeLive!c;
    mixin("T a = fn(" ~ nullArgs ~ ");");
    assert(mixin(c.eqRet), c.func ~ ": unexpected return value");
    checkReceived!c(a, base);
    release(a);

    static if (c.nullable)
      assert(fn(true) is null, c.func ~ ": expected null return value");
  }
  else static if (c.pos == "field" || c.pos == "field_fixed" || c.pos == "field_embed" || c.pos == "prop"
    || c.pos == "prop_acc")
  { // Set the record field or object property to the In value, the C function checks it and replaces it with the
    // Out value which is then read back
    // (nullable cells are additionally run with null as the In value)
    alias O = Params[$ - 1];
    auto o = new O();
    alias T = typeof(mixin("o." ~ c.member));

    foreach (isNull; c.nullable ? [false, true] : [false])
    {
      auto base = typeLive!c;
      T v = isNull ? T.init : mixin(c.makeIn);
      mixin("o." ~ c.member ~ " = v;");

      if (!isNull)
      {
        checkPassed!c(v, base, true);
        static if (is(T : ObjectWrap)) // The getter returns the same D object (GObject wrappers are unique)
          assert(mixin("o." ~ c.member) is v, c.func ~ ": " ~ c.pos ~ " getter returned a different D object");
      }

      mixin("fn(" ~ (c.nullable ? "isNull, " : "") ~ "o);");
      { T a = mixin("o." ~ c.member); assert(mixin(c.eqOut), c.func ~ ": unexpected " ~ c.pos ~ " value"); release(a); }
      release(v);
    }

    release(o);
  }
  else static if (c.pos == "field_array")
  { // Read the array field set by the C function (read-only, the length is another field)
    alias O = Params[$ - 1];
    auto o = new O();
    fn(o);
    auto base = typeLive!c;
    auto a = mixin("o." ~ c.member);
    assert(mixin(c.eqOut), c.func ~ ": unexpected field array value");
    checkReceived!c(a, base);
    release(a);
    release(o);
  }
  else static if (c.pos == "cb_arg" || c.pos == "cb_notified" || c.pos == "cb_async")
  { // C calls the delegate with the Out value (nullable cells are additionally called with null)
    alias T = Parameters!(Params[$ - 1])[0];
    int calls;
    mixin("fn(" ~ nullArgs ~ "(T a) { assert(mixin(c.eqOut), c.func ~ \": unexpected callback value\"); release(a); calls++; });");
    static if (c.nullable)
      fn(true, (T a) { assert(a is null, c.func ~ ": expected null callback value"); calls++; });
    assert(calls == (c.nullable ? 2 : 1), c.func ~ ": callback not called");
  }
  else static if (c.pos == "fill_out_ca")
    testFillOut!(c, T);
  else static if (c.pos == "map_method" || c.pos == "map_func")
    testMap!c;
  else static if (c.pos == "static_borrow")
  { // Static C structure: borrowed without an owner (the same C structure each call), a dup is an owned copy
    auto a = fn();
    assert(mixin(c.eqRet), c.func ~ ": unexpected static value");
    assert(a._cBorrowed && !a._owner && a._cPtr is fn()._cPtr, c.func ~ ": static structure is not borrowed");
    auto kept = a._dup;
    assert(!kept._cBorrowed && kept._cPtr !is a._cPtr && kept.value == a.value, c.func ~ ": dup is not an owned copy");

    // A D constructed (inline) structure is copied inline (not by the copy function), keeping the source alive
    alias T = ReturnType!fn;
    T local = mixin(c.makeIn);
    auto localDup = local._dup;
    assert(!localDup._cBorrowed && !localDup._cOwned && localDup._owner is local
      && localDup._cPtr !is local._cPtr && localDup.value == local.value && localDup.name == "pw",
      c.func ~ ": dup of a D structure is not an inline copy");
  }
  else static if (c.pos == "cb_borrow" || c.pos == "sig_borrow")
  { // C passes a borrowed structure (Out value) which D writes the In value to (checked by C) and dups
    alias O = Params[0];
    alias T = Parameters!(mixin(c.inFunc))[$ - 1];
    T kept;
    auto dg = (T a) {
      assert(mixin(c.eqOut), c.func ~ ": unexpected borrowed value");
      assert(a._cBorrowed && !a._owner, c.func ~ ": argument is not borrowed without an owner");
      a.value = mixin(c.makeIn).value;
      kept = a._dup;
      assert(!kept._cBorrowed && kept._cPtr !is a._cPtr, c.func ~ ": dup is not an owned copy");
    };

    static if (c.pos == "cb_borrow")
      fn(dg);
    else
    {
      auto o = new O();
      mixin("o." ~ c.member ~ "(dg);");
      fn(o);
    }

    assert(kept && kept.value == mixin(c.makeIn).value, c.func ~ ": dup did not survive the C structure");

    static if (T._cAllocated) // Deep copy (copy function), its pointer fields are valid after the original was freed
      assert(kept._cOwned && kept.name == "pw", c.func ~ ": dup is not a deep copy");
  }
  else static if (c.pos == "cb_ret")
  { // The delegate returns the In value (nullable cells additionally return null)
    alias T = ReturnType!(Params[$ - 1]);
    T v = mixin(c.makeIn);
    mixin("fn(" ~ nullArgs ~ "() => v);");
    static if (c.nullable)
      fn(true, () => cast(T)null);
    release(v);
  }
  else static if (c.pos == "cb_out")
  { // The delegate sets its out parameter to the In value (nullable cells additionally set null)
    alias T = Parameters!(Params[$ - 1])[0];
    T v = mixin(c.makeIn);
    mixin("fn(" ~ nullArgs ~ "(out T a) { a = v; });");
    static if (c.nullable)
      fn(true, (out T a) { a = null; });
    release(v);
  }
  else static if (c.pos == "sig_arg")
  { // The C function emits the passed In value with the signal connected here
    alias O = Params[0];
    alias T = Params[$ - 1];
    auto o = new O();
    int calls;
    bool expectNull;
    mixin("o." ~ c.member ~ "((T a) { assert(expectNull ? a is T.init : mixin(c.eqIn), c.func ~ \": unexpected signal value\"); release(a); calls++; });");
    T v = mixin(c.makeIn);
    fn(o, v);
    release(v);

    static if (c.nullable)
    {
      expectNull = true;
      fn(o, null);
    }

    assert(calls == (c.nullable ? 2 : 1), c.func ~ ": signal handler not called");
    release(o);
  }
  else static if (c.pos == "sig_ret")
  { // The signal handler returns the Ret value which the C function returns (nullable: and then null)
    alias O = Params[0];
    alias T = ReturnType!fn;
    auto o = new O();
    bool returnNull;
    T v = mixin(c.makeRet);
    mixin("o." ~ c.member ~ "(() => returnNull ? T.init : v);");
    { T a = fn(o); assert(mixin(c.eqRet), c.func ~ ": unexpected signal return value"); release(a); }

    static if (c.nullable)
    {
      returnNull = true;
      assert(fn(o) is null, c.func ~ ": expected null signal return value");
    }

    release(v);
    release(o);
  }
  else static if (c.pos == "prop_ro" || c.pos == "prop_wo" || c.pos == "prop_co")
  { // Read-only: C sets the Out value, D reads it. Write-only: D sets the In value, C checks it. Construct-only: C
    // constructs an object with the Out value, D reads it.
    alias T = Parameters!(__traits(getMember, gidgenmatrix.global, c.inFunc))[$ - 1];
    static if (c.pos == "prop_co")
      auto o = fn();
    else
    {
      alias O = Params[$ - 1];
      auto o = new O();
    }

    static if (c.pos == "prop_wo")
    {
      static assert(!__traits(compiles, { T a = mixin("o." ~ c.member); }), c.func ~ ": write-only property readable");
      T v = mixin(c.makeIn);
      mixin("o." ~ c.member ~ " = v;");
      fn(o);
      release(v);
    }
    else
    {
      static assert(!__traits(compiles, { T v; mixin("o." ~ c.member ~ " = v;"); }), c.func ~ ": property writable");
      static if (c.pos == "prop_ro")
        fn(o);
      { T a = mixin("o." ~ c.member); assert(mixin(c.eqOut), c.func ~ ": unexpected property value"); release(a); }
    }

    release(o);
  }
  else static if (c.pos == "cb_throws")
  { // The delegate returns the In value, then sets a GError (raw GError** passed to throwing delegates)
    alias Dg = Params[$ - 1];
    alias T = ReturnType!Dg;
    int calls;
    T v = mixin(c.makeIn);
    fn((GError** err) {
      if (calls++ == 0)
        return v;
      g_set_error_literal(err, g_quark_from_string("gidgen-matrix"), 1, "matrix error");
      return T.init;
    });
    assert(calls == 2, c.func ~ ": callback not called twice");
  }
  else static if (c.pos == "sig_out" || c.pos == "sig_inout")
  { // The C function emits a pointer to its value (inout: the Out value) which the handler sets to the In value
    alias O = Params[0];
    auto o = new O();
    int calls;
    alias T = Parameters!(__traits(getMember, gidgenmatrix.global, c.inFunc))[$ - 1];
    mixin("o." ~ c.member ~ "((" ~ (c.pos == "sig_out" ? "out" : "ref") ~ " T a) { static if (c.pos == \"sig_inout\") assert(mixin(c.eqOut), c.func"
      ~ " ~ \": unexpected signal value\"); a = mixin(c.makeIn); calls++; });");
    fn(o);
    assert(calls == 1, c.func ~ ": signal handler not called");
    release(o);
  }
  else static if (c.pos == "cb_inout_borrow")
  { // Borrowed InOut Wrap**: keep the same pointer or set null (remove). Owner is a Fields wrapper.
    import core.memory : GC;
    import gidgenmatrix.fields : Fields;

    alias T = Parameters!(Params[$ - 1])[1];
    int calls;

    foreach (mode; [InOutMode.Keep, InOutMode.SetNull])
    {
      void dlg(Fields owner, ref T a)
      {
        assert(owner !is null, c.func ~ ": missing owner");
        assert(a !is null && a._cBorrowed && a._owner is owner, c.func ~ ": value is not borrowed from owner");
        assert(mixin(c.eqOut), c.func ~ ": unexpected borrowed value");

        if (mode == InOutMode.SetNull)
          a = null;

        calls++;
      }

      fn(mode, &dlg);
    }

    assert(calls == 2, c.func ~ ": callback not called");
    // Owner is Boxed: transfer-none trampoline copies it; collect so the copies are freed before the live check
    GC.collect;
    GC.minimize;
  }
  else
    static assert(false, "Unhandled matrix position " ~ c.pos);
}

/// Run every matrix cell, checking for C side failures and leaked/over-freed instances after each.
/// Set the environment variable MATRIX_VERBOSE to print each cell name before it is run (to locate crashes).
unittest
{
  import std.process : environment;
  import std.stdio : stderr;

  auto verbose = environment.get("MATRIX_VERBOSE") !is null;

  static foreach (c; matrixCells)
  {{
    if (verbose)
    {
      stderr.writeln("matrix cell: ", c.func);
      stderr.flush;
    }

    releaseKept;
    int[Counter.Last] startLive;
    foreach (i, ref start; startLive)
      start = liveCount(cast(Counter)i);
    auto startFail = failureCount;

    testCell!c;

    releaseKept;
    assert(failureCount == startFail, c.func ~ ": " ~ lastFailure);
    foreach (i, start; startLive)
      assert(liveCount(cast(Counter)i) == start, c.func ~ ": " ~ (cast(Counter)i).to!string ~ " live count "
        ~ (liveCount(cast(Counter)i) - start).to!string);
  }}
}

/// Sanity check of the failure detection itself: a wrong input value must be recorded as a failure by the C side
unittest
{
  auto start = failureCount;
  inInt(0); // Expects -1
  assert(failureCount == start + 1, "matrix C value checks are not working");
}
