/*
 * C program test of GObjectExport: a pure C program (no D main) using GObject types exported by the D shared library
 * libgidgen-export.so (source/export_lib.d). Built twice: linked against the library (direct calls of the exported
 * get_type functions) and with -DUSE_DLOPEN (the library is loaded with dlopen and its types are found by name).
 */
#include <dlfcn.h>
#include <glib-object.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "gidgen-feat.h"
#include "gidgen_export.h" /* C header generated with the GIR (gidgen-export-gir) */
#include "gidgen_mixer.h" /* Header of another D library using the GidgenExport types (includes gidgen_export.h) */

#define CHECK(cond) do { if (!(cond)) { fprintf (stderr, "%s:%d: check failed: %s\n", __FILE__, __LINE__, #cond); \
  exit (1); } } while (0)

/* Casts to the C types of the generated header (its checked cast macros call the get_type functions directly, which
 * the dlopen build doesn't link) */
#define VIRT(obj) ((GidgenExportVirt *) (obj))
#define SHAPE(obj) ((GidgenExportShape *) (obj))
#define AUTO(obj) ((GidgenExportAuto *) (obj))

/* Hand written helpers of the test library (not in the GIR) */
static void (*collect) (void);
static int (*live_dobjects) (void);
static void (*emit_bumped) (void *obj, int amount);
/* Exported C entry points of the D types (GObjectExport and GInterfaceExport), typed by the generated header */
#define EXPORT_FN(name, sym) static __typeof__ (sym) *name
EXPORT_FN (obj_get_type, gidgen_export_obj_get_type);
EXPORT_FN (child_get_type, gidgen_export_child_get_type);
EXPORT_FN (floating_get_type, gidgen_export_floating_type);
EXPORT_FN (virt_get_type, gidgen_export_virt_get_type);
EXPORT_FN (virt_child_get_type, gidgen_export_virt_child_get_type);
EXPORT_FN (shape_get_type, gidgen_export_shape_get_type);
EXPORT_FN (obj_new, gidgen_export_obj_new);
EXPORT_FN (floating_new, gidgen_export_floating_new);
EXPORT_FN (virt_new, gidgen_export_virt_new);
EXPORT_FN (virt_scale, gidgen_export_virt_scale); /* Invokers of @gVirtual methods */
EXPORT_FN (virt_join, gidgen_export_virt_join);
EXPORT_FN (shape_sides, gidgen_export_shape_sides);
EXPORT_FN (shape_label, gidgen_export_shape_label);
EXPORT_FN (virt_new_with_title, gidgen_export_virt_new_with_title); /* @gConstructor, @gMethod */
EXPORT_FN (virt_describe, gidgen_export_virt_describe);
EXPORT_FN (virt_divide, gidgen_export_virt_divide);
EXPORT_FN (virt_stats, gidgen_export_virt_stats);
EXPORT_FN (virt_abi_version, gidgen_export_virt_abi_version);
EXPORT_FN (auto_get_type, gidgen_export_auto_get_type); /* GObjectExportAll, no attributes */
EXPORT_FN (auto_add, gidgen_export_auto_add);
EXPORT_FN (auto_twice, gidgen_export_auto_twice);

/* C implementation of the D-defined interface GidgenExportShape (interface struct of the generated header) */
static gint
c_sides (GidgenExportShape *self)
{
  return 3;
}

static void
c_shape_init (gpointer iface, gpointer data)
{
  ((GidgenExportShapeInterface *) iface)->sides = c_sides;
}

/* C subclass of GidgenExportVirt overriding scale, chaining up to the parent implementation. The class struct of the
 * generated header has the new virtual methods (@gVirtual) after the class struct of the parent type. */
static gint
c_scale (GidgenExportVirt *self, gint x)
{
  GidgenExportVirtClass *parent = g_type_class_peek_parent (GIDGEN_EXPORT_VIRT_GET_CLASS (self));

  return parent->scale (self, x) + 100;
}

/* C subclass of ExportObj, a D type that overrides C-declared vfuncs (constructed, compute) */
static int c_compute_calls, c_constructed_calls;

static int
c_obj_compute (GidgenFeatObject *self, gint x)
{
  GidgenFeatObjectClass *parent = g_type_class_peek_parent (G_OBJECT_GET_CLASS (self));

  c_compute_calls++;
  return parent->compute (self, x) + 100;
}

static void
c_obj_constructed (GObject *self)
{
  GObjectClass *parent = g_type_class_peek_parent (G_OBJECT_GET_CLASS (self));

  c_constructed_calls++;
  parent->constructed (self); /* D override of the type, which chains up to GObject */
}

static void
c_obj_class_init (gpointer klass, gpointer data)
{
  ((GObjectClass *) klass)->constructed = c_obj_constructed;
  ((GidgenFeatObjectClass *) klass)->compute = c_obj_compute;
}

static void
c_class_init (gpointer klass, gpointer data)
{
  ((GidgenExportVirtClass *) klass)->scale = c_scale;
}

static void
on_bumped (GObject *obj, int amount, const char *who, gpointer data)
{
  (*((int *) data)) += amount;
}

static char *
on_greet_after (GObject *obj, const char *name, gpointer data)
{
  (*((int *) data))++;
  return g_strdup_printf ("after %s", name);
}

static void
on_tagged (GObject *obj, const char *text, gpointer data)
{
  (*((int *) data))++;
}

static void
on_prop_notify (GObject *obj, GParamSpec *pspec, gpointer data)
{
  (*((int *) data))++;
}

#ifdef USE_DLOPEN
static void
load_library (void)
{
  void *lib;

  CHECK (g_type_from_name ("GidgenExportObj") == 0); /* Not loaded yet */
  lib = dlopen ("libgidgen-export.so", RTLD_NOW | RTLD_LOCAL); /* Library constructor initializes the D runtime */
  if (!lib)
    fprintf (stderr, "dlopen: %s\n", dlerror ());
  CHECK (lib != NULL);
  CHECK (g_type_from_name ("GidgenExportObj") == 0); /* GTypes are registered lazily by get_type */

  obj_get_type = (__typeof__ (obj_get_type)) dlsym (lib, "gidgen_export_obj_get_type");
  child_get_type = (__typeof__ (child_get_type)) dlsym (lib, "gidgen_export_child_get_type");
  floating_get_type = (__typeof__ (floating_get_type)) dlsym (lib, "gidgen_export_floating_type");
  virt_get_type = (__typeof__ (virt_get_type)) dlsym (lib, "gidgen_export_virt_get_type");
  virt_child_get_type = (__typeof__ (virt_child_get_type)) dlsym (lib, "gidgen_export_virt_child_get_type");
  collect = (void (*) (void)) dlsym (lib, "gidgen_export_collect");
  live_dobjects = (int (*) (void)) dlsym (lib, "gidgen_export_live_dobjects");
  emit_bumped = (void (*) (void *, int)) dlsym (lib, "gidgen_export_emit_bumped");
  shape_get_type = (__typeof__ (shape_get_type)) dlsym (lib, "gidgen_export_shape_get_type");
  obj_new = (__typeof__ (obj_new)) dlsym (lib, "gidgen_export_obj_new");
  floating_new = (__typeof__ (floating_new)) dlsym (lib, "gidgen_export_floating_new");
  virt_new = (__typeof__ (virt_new)) dlsym (lib, "gidgen_export_virt_new");
  virt_scale = (__typeof__ (virt_scale)) dlsym (lib, "gidgen_export_virt_scale");
  virt_join = (__typeof__ (virt_join)) dlsym (lib, "gidgen_export_virt_join");
  shape_sides = (__typeof__ (shape_sides)) dlsym (lib, "gidgen_export_shape_sides");
  shape_label = (__typeof__ (shape_label)) dlsym (lib, "gidgen_export_shape_label");
  virt_new_with_title = (__typeof__ (virt_new_with_title)) dlsym (lib, "gidgen_export_virt_new_with_title");
  virt_describe = (__typeof__ (virt_describe)) dlsym (lib, "gidgen_export_virt_describe");
  virt_divide = (__typeof__ (virt_divide)) dlsym (lib, "gidgen_export_virt_divide");
  virt_stats = (__typeof__ (virt_stats)) dlsym (lib, "gidgen_export_virt_stats");
  virt_abi_version = (__typeof__ (virt_abi_version)) dlsym (lib, "gidgen_export_virt_abi_version");
  auto_get_type = (__typeof__ (auto_get_type)) dlsym (lib, "gidgen_export_auto_get_type");
  auto_add = (__typeof__ (auto_add)) dlsym (lib, "gidgen_export_auto_add");
  auto_twice = (__typeof__ (auto_twice)) dlsym (lib, "gidgen_export_auto_twice");
  CHECK (obj_get_type && child_get_type && floating_get_type && collect && live_dobjects && emit_bumped
         && virt_get_type && virt_child_get_type && shape_get_type && obj_new && floating_new && virt_new
         && virt_scale && virt_join && shape_sides && shape_label && virt_new_with_title && virt_describe && virt_divide
         && virt_stats && virt_abi_version && auto_get_type && auto_add && auto_twice);
  CHECK (dlsym (lib, "gidgen_export_auto_level") == NULL); /* @property accessors are properties only */
  /* Invokers are exported by the type which declares the @gVirtual method only */
  CHECK (dlsym (lib, "gidgen_export_virt_child_scale") == NULL);
  CHECK (dlsym (lib, "gidgen_export_scale") == NULL); /* Former hand written helper */
  /* The library is not closed: GTypes are registered statically */
}
#else
void gidgen_export_collect (void); /* Hand written helpers, the exported types are declared by gidgen_export.h */
int gidgen_export_live_dobjects (void);
void gidgen_export_emit_bumped (void *obj, int amount);

static void
load_library (void)
{
  obj_get_type = gidgen_export_obj_get_type;
  child_get_type = gidgen_export_child_get_type;
  floating_get_type = gidgen_export_floating_type;
  virt_get_type = gidgen_export_virt_get_type;
  virt_child_get_type = gidgen_export_virt_child_get_type;
  collect = gidgen_export_collect;
  live_dobjects = gidgen_export_live_dobjects;
  emit_bumped = gidgen_export_emit_bumped;
  shape_get_type = gidgen_export_shape_get_type;
  obj_new = gidgen_export_obj_new;
  floating_new = gidgen_export_floating_new;
  virt_new = gidgen_export_virt_new;
  virt_scale = gidgen_export_virt_scale;
  virt_join = gidgen_export_virt_join;
  shape_sides = gidgen_export_shape_sides;
  shape_label = gidgen_export_shape_label;
  virt_new_with_title = gidgen_export_virt_new_with_title;
  virt_describe = gidgen_export_virt_describe;
  virt_divide = gidgen_export_virt_divide;
  virt_stats = gidgen_export_virt_stats;
  virt_abi_version = gidgen_export_virt_abi_version;
  auto_get_type = gidgen_export_auto_get_type;
  auto_add = gidgen_export_auto_add;
  auto_twice = gidgen_export_auto_twice;
}
#endif

static void
weak_notify (gpointer data, GObject *where_the_object_was)
{
  (*(int *) data)++;
}

/* Exercise an exported object from C */
static void
check_object (GidgenFeatObject *obj, int child)
{
  gint value = 0, bumps = 0, total = 0;
  GError *error = NULL;

  /* D-declared signals: emitted from C by name and from D, D default handler, from threads created by C too */
  g_signal_connect (obj, "bumped", G_CALLBACK (on_bumped), &bumps);
  g_signal_emit_by_name (obj, "bumped", 2, "C");
  emit_bumped (obj, 3);
  CHECK (bumps == 5);
  g_object_get (G_OBJECT (obj), "bump-total", &total, NULL);
  CHECK (total == 5);
  g_signal_handlers_disconnect_by_data (obj, &bumps);

  CHECK (strcmp (gidgen_feat_object_get_name (obj), "constructed-bound") == 0); /* D constructed override, D object bound */
  CHECK (gidgen_feat_object_compute (obj, 2) == 83 + (child ? 1000 : 0)); /* Class vfunc override (inherited by child) */
  CHECK (gidgen_feat_iface_get_id (GIDGEN_FEAT_IFACE (obj)) == 4242); /* Interface vfunc override */

  gidgen_feat_object_set_value (obj, 5);
  CHECK (gidgen_feat_iface_lookup (GIDGEN_FEAT_IFACE (obj), 3, &value, &error) && value == 8); /* Chain up to C */
  CHECK (!gidgen_feat_iface_lookup (GIDGEN_FEAT_IFACE (obj), 13, &value, &error)); /* D exception -> GError */
  CHECK (error && error->code == 13 && strcmp (error->message, "unlucky") == 0);
  g_clear_error (&error);
  CHECK (gidgen_feat_object_divide (obj, 2, &value, &value)); /* Not overridden: C implementation */
}

typedef struct
{
  int ok;
  int weak_count;
} ThreadData;

/* Thread created by C, which is not attached to the D runtime: creates objects and exits before the next GC */
static void *
thread_func (void *data)
{
  ThreadData *td = data;
  int i;

  for (i = 0; i < 20; i++)
    {
      GidgenFeatObject *obj = g_object_new (i % 2 ? child_get_type () : obj_get_type (), NULL);

      check_object (obj, i % 2);
      g_object_weak_ref (G_OBJECT (obj), weak_notify, &td->weak_count);
      g_object_unref (obj); /* Last C reference, the D object is collected later on the main thread */
    }

  td->ok = 1;
  return NULL;
}

int
main (void)
{
  GType obj_type, child_type, floating_type;
  GidgenFeatObject *obj;
  GObject *floating;
  int weak_count = 0, start, i;
  pthread_t threads[4];
  ThreadData td[4];

  alarm (60); /* A hanging GC (exited attached thread) fails the test */
  load_library ();

  obj_type = obj_get_type ();
  child_type = child_get_type ();
  floating_type = floating_get_type ();
  CHECK (obj_type && child_type && floating_type);
  CHECK (obj_get_type () == obj_type); /* Registered once */
  CHECK (g_type_from_name ("GidgenExportObj") == obj_type);
  CHECK (g_type_from_name ("GidgenExportChild") == child_type);
  CHECK (g_type_from_name ("GidgenExportFloating") == floating_type);
  CHECK (g_type_parent (obj_type) == GIDGEN_FEAT_TYPE_OBJECT);
  CHECK (g_type_parent (child_type) == obj_type);
  CHECK (g_type_is_a (child_type, GIDGEN_FEAT_TYPE_IFACE));
  CHECK (g_type_is_a (floating_type, G_TYPE_INITIALLY_UNOWNED));

  start = live_dobjects ();

  /* Main thread: objects created from C get their D objects, unref -> GC -> D destructor -> finalize */
  obj = g_object_new (obj_type, NULL);
  check_object (obj, 0);
  CHECK (live_dobjects () == start + 1);
  CHECK (G_OBJECT (obj)->ref_count == 2); /* C reference and the D object toggle reference */
  g_object_weak_ref (G_OBJECT (obj), weak_notify, &weak_count);
  g_object_unref (obj);
  CHECK (weak_count == 0); /* D object still alive until collected */
  obj = NULL;
  for (i = 0; i < 5 && weak_count == 0; i++)
    collect ();
  CHECK (weak_count == 1);
  CHECK (live_dobjects () == start);

  /* D-declared signals: names, flags, return values, details, action signals, inherited by the child type */
  {
    int bumps = 0, total = 0, after_calls = 0, tagged_a = 0, tagged_any = 0;
    char *greeting = NULL;
    GSignalQuery query;
    guint id;

    id = g_signal_lookup ("bumped", obj_type);
    CHECK (id != 0 && g_signal_lookup ("bumped", child_type) == id);
    CHECK (g_signal_lookup ("child-event", obj_type) == 0);
    g_type_class_ref (child_type);
    CHECK (g_signal_lookup ("child-event", child_type) != 0);
    g_signal_query (id, &query);
    CHECK (query.itype == obj_type && query.n_params == 2 && query.param_types[0] == G_TYPE_INT
           && query.param_types[1] == G_TYPE_STRING && query.return_type == G_TYPE_NONE
           && (query.signal_flags & G_SIGNAL_RUN_LAST));
    g_signal_query (g_signal_lookup ("tagged", obj_type), &query);
    CHECK (query.signal_flags & G_SIGNAL_DETAILED);
    g_signal_query (g_signal_lookup ("reset", obj_type), &query);
    CHECK (query.signal_flags & G_SIGNAL_ACTION);
    g_signal_query (g_signal_lookup ("greet", obj_type), &query);
    CHECK (query.return_type == G_TYPE_STRING && query.n_params == 1);

    obj = g_object_new (child_type, NULL);
    g_signal_connect (obj, "bumped", G_CALLBACK (on_bumped), &bumps);
    g_signal_emit_by_name (obj, "bumped", 4, "child");
    g_object_get (G_OBJECT (obj), "bump-total", &total, NULL);
    CHECK (bumps == 4 && total == 4);
    g_signal_emit_by_name (obj, "child-event", 1);

    g_signal_emit_by_name (obj, "greet", "C", &greeting); /* D default handler runs last */
    CHECK (greeting && strcmp (greeting, "hello C") == 0);
    g_free (greeting);
    g_signal_connect_after (obj, "greet", G_CALLBACK (on_greet_after), &after_calls);
    g_signal_emit_by_name (obj, "greet", "C", &greeting);
    CHECK (greeting && strcmp (greeting, "after C") == 0 && after_calls == 1);
    g_free (greeting);

    g_signal_connect (obj, "tagged::a", G_CALLBACK (on_tagged), &tagged_a);
    g_signal_connect (obj, "tagged", G_CALLBACK (on_tagged), &tagged_any);
    g_signal_emit_by_name (obj, "tagged::a", "x");
    g_signal_emit_by_name (obj, "tagged::b", "x");
    CHECK (tagged_a == 1 && tagged_any == 2);

    g_signal_emit_by_name (obj, "reset"); /* Action signal: D default handler */
    g_object_get (G_OBJECT (obj), "bump-total", &total, NULL);
    CHECK (total == 0);

    g_object_unref (obj);
    obj = NULL;
    for (i = 0; i < 5 && live_dobjects () != start; i++)
      collect ();
    CHECK (live_dobjects () == start);
  }

  /* D-declared properties: g_object_new / g_object_set / g_object_get, and notify */
  {
    int count = 0, construct_val = 0, notify_count = 0;
    char *title = NULL;
    obj = g_object_new (obj_type, "count", 5, "construct-val", 70, NULL);
    g_object_get (G_OBJECT (obj), "count", &count, "title", &title, "construct-val", &construct_val, NULL);
    CHECK (count == 5);
    CHECK (title && strcmp (title, "hi") == 0);
    CHECK (construct_val == 70);
    g_free (title);
    g_signal_connect (obj, "notify", G_CALLBACK (on_prop_notify), &notify_count);
    g_object_set (G_OBJECT (obj), "count", 9, "title", "z", NULL);
    g_object_get (G_OBJECT (obj), "count", &count, "title", &title, NULL);
    CHECK (count == 9 && notify_count >= 2);
    CHECK (title && strcmp (title, "z") == 0);
    g_free (title);
    notify_count = 0;
    g_object_set (G_OBJECT (obj), "count", 9, NULL); /* same value: no notify */
    CHECK (notify_count == 0);
    g_object_set (G_OBJECT (obj), "count", 10, NULL);
    g_object_get (G_OBJECT (obj), "count", &count, NULL);
    CHECK (count == 10 && notify_count == 1);
    notify_count = 0;
    g_object_set (G_OBJECT (obj), "noisy", 3, NULL); /* notifyAlways: same value still notifies */
    CHECK (notify_count == 1);
    g_object_set (G_OBJECT (obj), "noisy", 4, NULL);
    CHECK (notify_count == 2);
    g_object_unref (obj);
    obj = NULL;
    for (i = 0; i < 5 && live_dobjects () != start; i++)
      collect ();
    CHECK (live_dobjects () == start);
  }

  obj = g_object_new (child_type, NULL);
  check_object (obj, 1);
  g_object_weak_ref (G_OBJECT (obj), weak_notify, &weak_count);
  g_clear_object (&obj);
  for (i = 0; i < 5 && weak_count == 1; i++)
    collect ();
  CHECK (weak_count == 2);

  /* Floating reference of a GInitiallyUnowned subclass is kept for C and sunk by it */
  floating = g_object_new (floating_type, NULL);
  CHECK (g_object_is_floating (floating));
  g_object_ref_sink (floating);
  CHECK (!g_object_is_floating (floating) && floating->ref_count == 2);
  g_object_weak_ref (floating, weak_notify, &weak_count);
  g_object_unref (floating);
  for (i = 0; i < 5 && weak_count == 2; i++)
    collect ();
  CHECK (weak_count == 3);
  CHECK (live_dobjects () == start);

  /* Exported constructors (<prefix>_<type>_new): new instances with their D objects, transfer full (floating for a
   * GInitiallyUnowned subclass) */
  {
    GObject *nobj;
    int construct_val = 0, new_weak = 0;

    nobj = G_OBJECT (obj_new ());
    CHECK (G_OBJECT_TYPE (nobj) == obj_type && nobj->ref_count == 2 && !g_object_is_floating (nobj));
    CHECK (live_dobjects () == start + 1);
    g_object_get (nobj, "construct-val", &construct_val, NULL);
    CHECK (construct_val == 10); /* Construct property default */
    g_object_weak_ref (nobj, weak_notify, &new_weak);
    g_object_unref (nobj);
    for (i = 0; i < 5 && new_weak == 0; i++)
      collect ();
    CHECK (new_weak == 1);

    nobj = G_OBJECT (floating_new ());
    CHECK (G_OBJECT_TYPE (nobj) == floating_type && g_object_is_floating (nobj));
    g_object_ref_sink (nobj);
    g_object_weak_ref (nobj, weak_notify, &new_weak);
    g_object_unref (nobj);
    for (i = 0; i < 5 && new_weak == 1; i++)
      collect ();
    CHECK (new_weak == 2);
    CHECK (live_dobjects () == start);
  }

  /* New virtual methods: slots follow the parent class struct, D defaults and overrides, C overrides */
  {
    GTypeQuery query;
    GType c_obj_type, c_child_type;
    GObject *vobj;
    char *title = NULL;

    g_type_query (virt_get_type (), &query);
    CHECK (query.class_size == sizeof (GidgenExportVirtClass));
    g_type_query (virt_child_get_type (), &query);
    CHECK (query.class_size == sizeof (GidgenExportVirtClass)); /* No new vfuncs in the child */

    vobj = g_object_new (virt_get_type (), NULL);
    CHECK (((GidgenExportVirtClass *) G_OBJECT_GET_CLASS (vobj))->scale (VIRT (vobj), 5) == 10);
    ((GidgenExportVirtClass *) G_OBJECT_GET_CLASS (vobj))->join (VIRT (vobj), "a", "b");
    g_object_get (vobj, "title", &title, NULL);
    CHECK (strcmp (title, "ab") == 0);
    g_free (title);
    CHECK (virt_scale (VIRT (vobj), 5) == 10);
    virt_join (VIRT (vobj), "c", "d"); /* String arguments through the invoker */
    g_object_get (vobj, "title", &title, NULL);
    CHECK (strcmp (title, "cd") == 0);
    g_free (title);
    g_object_unref (vobj);

    vobj = G_OBJECT (virt_new ()); /* Exported constructor */
    CHECK (G_TYPE_CHECK_INSTANCE_TYPE (vobj, virt_get_type ()) && G_OBJECT_TYPE (vobj) == virt_get_type ());
    CHECK (vobj->ref_count == 2 && !g_object_is_floating (vobj)); /* C reference and the D object toggle reference */
    CHECK (virt_scale (VIRT (vobj), 7) == 14);
    g_object_unref (vobj);

    vobj = g_object_new (virt_child_get_type (), NULL);
    /* D override chains up */
    CHECK (((GidgenExportVirtClass *) G_OBJECT_GET_CLASS (vobj))->scale (VIRT (vobj), 5) == 11);
    CHECK (virt_scale (VIRT (vobj), 5) == 11);
    g_object_unref (vobj);

    c_obj_type = g_type_register_static_simple (virt_get_type (), "CExportObj", sizeof (GidgenExportVirtClass),
                                                c_class_init, query.instance_size, NULL, 0);
    c_child_type = g_type_register_static_simple (virt_child_get_type (), "CExportChild",
                                                  sizeof (GidgenExportVirtClass), c_class_init, query.instance_size, NULL, 0);

    vobj = g_object_new (c_obj_type, NULL);
    /* C override chains up to D */
    CHECK (((GidgenExportVirtClass *) G_OBJECT_GET_CLASS (vobj))->scale (VIRT (vobj), 5) == 110);
    CHECK (virt_scale (VIRT (vobj), 5) == 110); /* D invoker uses the C override */
    g_object_unref (vobj);

    vobj = g_object_new (c_child_type, NULL);
    CHECK (virt_scale (VIRT (vobj), 5) == 111); /* C chains up to the D override, which chains up to the D default */
    g_object_unref (vobj);
  }

  /* Exported methods (@gMethod), constructors (@gConstructor) and functions: strings, NULL, GError, out values */
  {
    GObject *vobj;
    GidgenExportVirt *same = NULL;
    GError *error = NULL;
    char *str, *upper = NULL, **chars = NULL;
    int length = 0, vweak = 0;

    vobj = G_OBJECT (virt_new_with_title ("T"));
    CHECK (G_OBJECT_TYPE (vobj) == virt_get_type () && vobj->ref_count == 2);
    str = virt_describe (VIRT (vobj), "Mr");
    CHECK (strcmp (str, "Mr T") == 0);
    g_free (str); /* Transfer full */
    str = virt_describe (VIRT (vobj), NULL); /* @gNullable */
    CHECK (strcmp (str, "T") == 0);
    g_free (str);

    CHECK (virt_divide (VIRT (vobj), 7, 2, &error) == 3 && error == NULL);
    CHECK (virt_divide (VIRT (vobj), 7, 0, &error) == 0);
    CHECK (error && error->domain == g_quark_from_string ("gidgen-export") && error->code == 3
           && strcmp (error->message, "division by zero") == 0);
    g_clear_error (&error);
    CHECK (virt_divide (VIRT (vobj), 7, -1, &error) == 0);
    CHECK (error && error->domain == g_quark_from_string ("gid-d-exception-quark") && error->code == 0
           && strcmp (error->message, "negative divisor") == 0);
    g_clear_error (&error);
    CHECK (virt_divide (VIRT (vobj), 7, 0, NULL) == 0); /* Error ignored */

    virt_stats (VIRT (vobj), "abc", &length, &upper, &chars, &same);
    CHECK (length == 3 && strcmp (upper, "ABC") == 0 && g_strv_length (chars) == 3 && strcmp (chars[2], "c") == 0);
    CHECK ((GObject *) same == vobj && vobj->ref_count == 3);
    g_free (upper);
    g_strfreev (chars);
    g_object_unref (same);
    virt_stats (VIRT (vobj), "", NULL, NULL, NULL, NULL); /* Out values are optional */

    CHECK (virt_abi_version () == 3);

    g_object_weak_ref (vobj, weak_notify, &vweak);
    g_object_unref (vobj);
    for (i = 0; i < 5 && vweak == 0; i++)
      collect ();
    CHECK (vweak == 1);
  }

  /* Auto-exported type (GObjectExportAll): methods, functions and properties without attributes */
  {
    GObject *aobj;
    GParamSpec *pspec;
    int level = 0;

    aobj = g_object_new (auto_get_type (), NULL);
    pspec = g_object_class_find_property (G_OBJECT_GET_CLASS (aobj), "level");
    CHECK (pspec && (pspec->flags & G_PARAM_READWRITE) == G_PARAM_READWRITE);
    g_object_set (aobj, "level", 5, NULL);
    g_object_get (aobj, "level", &level, NULL);
    CHECK (level == 5 && auto_add (AUTO (aobj), 2) == 7 && auto_twice (4) == 8);
    g_object_unref (aobj);
  }

  /* Interface defined in D: implemented by a D class and by a C type, called from C and from D */
  {
    GType shape_type, c_shape_type;
    GTypeQuery query;
    GObject *sobj;
    GidgenExportShapeInterface *iface;
    char *title = NULL;
    GInterfaceInfo info = { c_shape_init, NULL, NULL };

    shape_type = shape_get_type (); /* Exported get_type of the D interface */
    CHECK (shape_type && G_TYPE_IS_INTERFACE (shape_type));
    CHECK (g_type_from_name ("GidgenExportShape") == shape_type && shape_get_type () == shape_type);
    CHECK (g_type_is_a (virt_get_type (), shape_type) && g_type_is_a (virt_child_get_type (), shape_type));

    sobj = g_object_new (virt_child_get_type (), NULL);
    iface = G_TYPE_INSTANCE_GET_INTERFACE (sobj, shape_type, GidgenExportShapeInterface);
    CHECK (iface->sides (SHAPE (sobj)) == 4);
    iface->label (SHAPE (sobj), "x");
    g_object_get (sobj, "title", &title, NULL);
    CHECK (strcmp (title, "label x") == 0);
    g_free (title);
    CHECK (shape_sides (SHAPE (sobj)) == 4); /* Invoker through the interface struct */
    shape_label (SHAPE (sobj), "y");
    g_object_get (sobj, "title", &title, NULL);
    CHECK (strcmp (title, "label y") == 0);
    g_free (title);
    g_object_unref (sobj);

    g_type_query (G_TYPE_OBJECT, &query);
    c_shape_type = g_type_register_static_simple (G_TYPE_OBJECT, "CExportShape", query.class_size, NULL,
                                                  query.instance_size, NULL, 0);
    g_type_add_interface_static (c_shape_type, shape_type, &info);
    sobj = g_object_new (c_shape_type, NULL);
    CHECK (shape_sides (SHAPE (sobj)) == 3); /* The invoker calls the C implementation */
    g_object_unref (sobj);
  }

#ifndef USE_DLOPEN
  /* Type macros and structs of the generated header */
  {
    GidgenExportVirt *v = gidgen_export_virt_new ();
    GidgenExportShape *shape = GIDGEN_EXPORT_SHAPE (v);
    GTypeQuery query;

    CHECK (GIDGEN_EXPORT_TYPE_VIRT == virt_get_type () && GIDGEN_EXPORT_IS_VIRT (v) && !GIDGEN_EXPORT_IS_AUTO (v));
    CHECK (GIDGEN_EXPORT_IS_VIRT_CLASS (GIDGEN_EXPORT_VIRT_GET_CLASS (v)));
    CHECK (GIDGEN_EXPORT_VIRT_GET_CLASS (v)->scale (v, 3) == 6);
    CHECK (GIDGEN_EXPORT_VIRT_CLASS (g_type_class_peek (GIDGEN_EXPORT_TYPE_VIRT))->scale
           == GIDGEN_EXPORT_VIRT_GET_CLASS (v)->scale);
    CHECK (GIDGEN_EXPORT_IS_SHAPE (shape)
           && GIDGEN_EXPORT_SHAPE_GET_IFACE (shape)->sides (shape) == gidgen_export_shape_sides (shape));
    CHECK (GIDGEN_EXPORT_TYPE_FLOATING == floating_get_type () && GIDGEN_EXPORT_TYPE_CHILD == child_get_type ());
    g_object_unref (v);

    g_type_query (GIDGEN_EXPORT_TYPE_CHILD, &query); /* The structs have the sizes of the registered types */
    CHECK (query.instance_size == sizeof (GidgenExportChild) && query.class_size == sizeof (GidgenExportChildClass));
    g_type_query (GIDGEN_EXPORT_TYPE_VIRT_CHILD, &query);
    CHECK (query.instance_size == sizeof (GidgenExportVirtChild)
           && query.class_size == sizeof (GidgenExportVirtChildClass));
    CHECK (sizeof (GidgenMixerFancyVirtClass) == sizeof (GidgenExportVirtClass));
  }
#endif

  /* C subclasses of D types overriding vfuncs of C classes: chaining up from D (super) does not call the C type again */
  {
    GTypeQuery query;
    GType types[2] = { obj_type, child_type };
    GidgenFeatObject *cobj;
    int j;

    for (j = 0; j < 2; j++)
      {
        GType ctype;

        g_type_query (types[j], &query);
        ctype = g_type_register_static_simple (types[j], j ? "CExportChildOfD" : "CExportObjOfD", query.class_size,
                                               c_obj_class_init, query.instance_size, NULL, 0);
        c_compute_calls = c_constructed_calls = 0;
        cobj = g_object_new (ctype, NULL);
        CHECK (c_constructed_calls == 1);
        CHECK (strcmp (gidgen_feat_object_get_name (cobj), "constructed-bound") == 0); /* D constructed ran */
        /* C (+100), D ExportChild (+1000, child only), D ExportObj (x * 3 + 77), no C chain-up recursion */
        CHECK (gidgen_feat_object_compute (cobj, 5) == 15 + 77 + (j ? 1000 : 0) + 100);
        CHECK (c_compute_calls == 1);
        g_object_unref (cobj);
      }
  }

  /* Short lived threads created by C, which exit before the GC runs */
  for (i = 0; i < 4; i++)
    {
      td[i].ok = 0;
      td[i].weak_count = 0;
      CHECK (pthread_create (&threads[i], NULL, thread_func, &td[i]) == 0);
    }

  for (i = 0; i < 4; i++)
    {
      pthread_join (threads[i], NULL);
      CHECK (td[i].ok);
    }

  for (i = 0; i < 10 && live_dobjects () != start; i++)
    collect (); /* Would hang if exited threads were still attached to the D runtime */

  CHECK (live_dobjects () == start);
  for (i = 0; i < 4; i++)
    CHECK (td[i].weak_count == 20);

  printf ("export test passed%s\n",
#ifdef USE_DLOPEN
          " (dlopen)"
#else
          " (linked)"
#endif
          );
  return 0;
}
