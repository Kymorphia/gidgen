/// GIR export across D namespaces: the "widgets" library, with a GIR version (see feat_gir_mixer)
module feat_gir_widgets;

import gid.gobject2.object : GInterfaceExport, GObjectExport, GObjectNamespace, ObjectWrap, gMethod, gProperty,
  gVirtual;

mixin GObjectNamespace!("GidgenWidgets", "1.0");

/// Exported interface used by the other namespace
interface Turnable
{
  mixin GInterfaceExport;

  @gVirtual int vfuncTurn(int steps);
}

/// Exported class used by the other namespace
class Knob : ObjectWrap
{
  mixin GObjectExport;

  @gProperty double level = 0.5;

  @gVirtual protected double vfuncScaled(double factor) { return level * factor; }
}
