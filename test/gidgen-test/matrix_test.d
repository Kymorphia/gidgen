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
import std.traits : isDynamicArray, Parameters, ReturnType;
import std.conv : to;

import glib.c.functions : g_quark_from_string, g_set_error_literal;
import glib.c.types : GError;
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

/// Test a single matrix cell
private void testCell(MatrixCell c)()
{
  static if (c.pos.startsWith("self_")) // Methods of the type (c.member), T is the D value type of the type's in cell
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
    release(v);
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
  else static if (c.pos == "cb_inout")
  { // The delegate receives the Out value and replaces it with the In value
    alias T = Parameters!(Params[$ - 1])[0];
    T v = mixin(c.makeIn);
    int calls;
    fn((ref T a) { assert(mixin(c.eqOut), c.func ~ ": unexpected callback value"); release(a); a = v; calls++; });
    assert(calls == 1, c.func ~ ": callback not called");
    release(v);
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
  else static if (c.pos == "cb_arg" || c.pos == "cb_notified" || c.pos == "cb_async")
  { // C calls the delegate with the Out value (nullable cells are additionally called with null)
    alias T = Parameters!(Params[$ - 1])[0];
    int calls;
    mixin("fn(" ~ nullArgs ~ "(T a) { assert(mixin(c.eqOut), c.func ~ \": unexpected callback value\"); release(a); calls++; });");
    static if (c.nullable)
      fn(true, (T a) { assert(a is null, c.func ~ ": expected null callback value"); calls++; });
    assert(calls == (c.nullable ? 2 : 1), c.func ~ ": callback not called");
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
