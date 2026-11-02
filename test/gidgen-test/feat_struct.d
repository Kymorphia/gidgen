/// GidgenFeat structure field kinds tests
module feat_struct;

import gidgenfeat.types : Point;

/// Structure field kinds not covered by the matrix field cells: bit fields, array with a length field, zero
/// terminated array, fixed size structure array, callback fields and an untyped pointer field
unittest
{
  import gidgenfeat.field_kinds;

  auto fk = new FieldKinds;
  fk.fill;
  assert(fk.value == 1 && fk.flagA == 1 && fk.flagB == 5);
  assert(fk.nums == [5, 6, 7], "array field with a length field");
  assert(fk.names == ["x", "y"], "zero terminated array field");
  assert(fk.pts == [Point(8, 9), Point(10, 11)], "fixed size structure array field");
  assert(fk.func !is null && fk.func()(3) == 6, "callback typedef field");
  assert(fk.inlineFunc()(3, 4) == 7, "inline callback field");
  assert(fk.data is null);
  assert(fk.describe == "1 1 5 8,9 10,11 6 7");

  fk.value = 20;
  fk.flagA = 0;
  fk.flagB = 2;
  fk.pts = [Point(1, 2), Point(3, 4)];
  fk.func = null;
  assert(fk.describe == "20 0 2 1,2 3,4 -1 7");

  int x;
  fk.data = &x;
  assert(fk.data is &x, "untyped pointer field");
  fk.data = null;
}
