/**
 * D shared library exporting types of the GidgenMixer namespace which use the types of the GidgenExport namespace
 * (libgidgen-export.so, export_lib.d): the GIR includes GidgenExport-1.0 and refers to GidgenExport.Virt.
 */
module export_mixer;

import export_lib : ExportShape, ExportVirt;
import gid.gobject2.object : GirInfo, GObjectExport, GObjectGirMain, GObjectNamespace, ObjectWrap, gMethod, gProperty,
  gSignal;

mixin GObjectNamespace!("GidgenMixer", "1.0");

// The gir configuration writes GidgenMixer-1.0.gir (the version comes from GObjectNamespace)
version (GidGir) mixin GObjectGirMain!(GirInfo("GidgenMixer", null, "libgidgen-mixer.so"), export_mixer);

/// GidgenMixerChannel: methods, a property and a signal with GidgenExport types
class Channel : ObjectWrap
{
  mixin GObjectExport;

  @gProperty ExportVirt knob;

  this()
  {
    super();
    knob = new ExportVirt;
    knob.title = "gain";
  }

  @gMethod ExportVirt getGainKnob()
  {
    return knob;
  }

  @gMethod void setGainKnob(ExportVirt k)
  {
    knob = k;
    emitKnobChanged(k);
  }

  @gMethod int sidesOf(ExportShape shape)
  {
    return shape.sides;
  }

  @gSignal void knobChanged(ExportVirt k) {}
}

/// GidgenMixerFancyVirt, a subclass of GidgenExportVirt
class FancyVirt : ExportVirt
{
  mixin GObjectExport;

  override protected int vfuncScale(int x)
  {
    return x * 3;
  }
}
