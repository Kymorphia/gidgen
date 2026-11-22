/*
 * ShimLib - Tiny C test library which is built twice (SHIMLIB_MAJOR 1 and 2) to the libraries libshimlib-1.so.0 and
 * libshimlib-2.so.0. Both majors have the same C symbol names and GType names, as GTK 3 and GTK 4 do, so tests can
 * check that giD binds, links, and maps classes of two majors of a library which are loaded in the same process.
 * The GType names are registered by the first call of a get_type() function, a second library registering the same
 * name is an error, so a test must call the get_type() function of only one major for each GType name. The two
 * classes (Widget and Gadget) are independent of each other, so a test can register one from each major.
 */

#include <glib-object.h>

#ifndef SHIMLIB_MAJOR
#error "SHIMLIB_MAJOR must be defined"
#endif

typedef struct { GObject parent_instance; } ShimLibWidget;
typedef struct { GObjectClass parent_class; } ShimLibWidgetClass;
typedef struct { GObject parent_instance; } ShimLibGadget;
typedef struct { GObjectClass parent_class; } ShimLibGadgetClass;

GType shim_lib_widget_get_type (void);
ShimLibWidget *shim_lib_widget_new (void);
gint shim_lib_widget_get_major (ShimLibWidget *widget);
GType shim_lib_gadget_get_type (void);
ShimLibGadget *shim_lib_gadget_new (void);
gint shim_lib_gadget_get_major (ShimLibGadget *gadget);

G_DEFINE_TYPE (ShimLibWidget, shim_lib_widget, G_TYPE_OBJECT)
G_DEFINE_TYPE (ShimLibGadget, shim_lib_gadget, G_TYPE_OBJECT)

static void
shim_lib_widget_finalize (GObject *object)
{
  G_OBJECT_CLASS (shim_lib_widget_parent_class)->finalize (object);
}

static void
shim_lib_widget_class_init (ShimLibWidgetClass *klass)
{
  G_OBJECT_CLASS (klass)->finalize = shim_lib_widget_finalize; /* Differs from the parent class, identifies this library */
}

static void
shim_lib_widget_init (ShimLibWidget *widget)
{
}

static void
shim_lib_gadget_finalize (GObject *object)
{
  G_OBJECT_CLASS (shim_lib_gadget_parent_class)->finalize (object);
}

static void
shim_lib_gadget_class_init (ShimLibGadgetClass *klass)
{
  G_OBJECT_CLASS (klass)->finalize = shim_lib_gadget_finalize;
}

static void
shim_lib_gadget_init (ShimLibGadget *gadget)
{
}

ShimLibWidget *
shim_lib_widget_new (void)
{
  return g_object_new (shim_lib_widget_get_type (), NULL);
}

gint
shim_lib_widget_get_major (ShimLibWidget *widget)
{
  return SHIMLIB_MAJOR;
}

ShimLibGadget *
shim_lib_gadget_new (void)
{
  return g_object_new (shim_lib_gadget_get_type (), NULL);
}

gint
shim_lib_gadget_get_major (ShimLibGadget *gadget)
{
  return SHIMLIB_MAJOR;
}
