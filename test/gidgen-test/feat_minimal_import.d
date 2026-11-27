/// @gSignal connect methods must compile when the user module does not import gulong.
module feat_minimal_import;

import gid.gobject2.object : GObjectSubclass, ObjectWrap, gSignal;

class MinimalSig : ObjectWrap
{
  mixin GObjectSubclass!"GidgenTestMinimalSig";

  @gSignal void ping() {}
}

unittest
{
  auto obj = new MinimalSig;
  int n;
  obj.connectPing({ n++; });
  obj.emitPing;
  assert(n == 1);
}
