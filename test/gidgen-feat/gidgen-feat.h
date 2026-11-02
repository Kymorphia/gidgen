/*
 * GidgenFeat - Hand written C test library for gidgen feature integration tests.
 *
 * The GIR file for this library is generated with g-ir-scanner from the GObject Introspection annotations in
 * gidgen-feat.c, so that it matches what real world libraries produce. The D unittests in gidgen-test/feat_*.d
 * exercise the generated D binding. Live instance counters allow the tests to verify that ownership transfers do
 * not leak or double free.
 */

#ifndef GIDGEN_FEAT_H
#define GIDGEN_FEAT_H

#include <glib-object.h>

G_BEGIN_DECLS

/* Live instance counters */

typedef enum
{
  GIDGEN_FEAT_COUNTER_OBJECT,
  GIDGEN_FEAT_COUNTER_DESTROY_NOTIFY,
  GIDGEN_FEAT_COUNTER_NO_COPY,
  GIDGEN_FEAT_COUNTER_LAST, /*< skip >*/
} GidgenFeatCounter;

gint gidgen_feat_live_count (GidgenFeatCounter counter);

/* Enums and flags */

typedef enum
{
  GIDGEN_FEAT_COLOR_RED,
  GIDGEN_FEAT_COLOR_GREEN,
  GIDGEN_FEAT_COLOR_BLUE = 10,
} GidgenFeatColor;

typedef enum
{
  GIDGEN_FEAT_STYLE_NONE = 0,
  GIDGEN_FEAT_STYLE_BOLD = 1 << 0,
  GIDGEN_FEAT_STYLE_ITALIC = 1 << 1,
  GIDGEN_FEAT_STYLE_UNDERLINE = 1 << 2,
} GidgenFeatStyle;

#define GIDGEN_FEAT_TYPE_COLOR (gidgen_feat_color_get_type ())
#define GIDGEN_FEAT_TYPE_STYLE (gidgen_feat_style_get_type ())

GType gidgen_feat_color_get_type (void) G_GNUC_CONST;
GType gidgen_feat_style_get_type (void) G_GNUC_CONST;
GidgenFeatColor gidgen_feat_color_next (GidgenFeatColor color);
GidgenFeatStyle gidgen_feat_style_toggle (GidgenFeatStyle style, GidgenFeatStyle toggle);

/* Errors */

#define GIDGEN_FEAT_ERROR (gidgen_feat_error_quark ())
GQuark gidgen_feat_error_quark (void);

typedef enum
{
  GIDGEN_FEAT_ERROR_FAILED,
  GIDGEN_FEAT_ERROR_INVALID,
} GidgenFeatError;

gboolean gidgen_feat_may_fail (gboolean fail, GError **error);
gchar *gidgen_feat_may_fail_string (gboolean fail, GError **error);

/* Interface */

#define GIDGEN_FEAT_TYPE_IFACE (gidgen_feat_iface_get_type ())
G_DECLARE_INTERFACE (GidgenFeatIface, gidgen_feat_iface, GIDGEN_FEAT, IFACE, GObject)

struct _GidgenFeatIfaceInterface
{
  GTypeInterface parent_iface;

  gint (*get_id) (GidgenFeatIface *self);
};

gint gidgen_feat_iface_get_id (GidgenFeatIface *self);

/* Object */

#define GIDGEN_FEAT_TYPE_OBJECT (gidgen_feat_object_get_type ())
G_DECLARE_DERIVABLE_TYPE (GidgenFeatObject, gidgen_feat_object, GIDGEN_FEAT, OBJECT, GObject)

struct _GidgenFeatObjectClass
{
  GObjectClass parent_class;

  gint (*compute) (GidgenFeatObject *self, gint x);

  gpointer padding[8];
};

GidgenFeatObject *gidgen_feat_object_new (void);
GidgenFeatObject *gidgen_feat_object_new_with_value (gint value);
gint gidgen_feat_object_get_value (GidgenFeatObject *self);
void gidgen_feat_object_set_value (GidgenFeatObject *self, gint value);
const gchar *gidgen_feat_object_get_name (GidgenFeatObject *self);
void gidgen_feat_object_set_name (GidgenFeatObject *self, const gchar *name);
guint gidgen_feat_object_get_ref_count (GidgenFeatObject *self);
void gidgen_feat_object_emit_changed (GidgenFeatObject *self, gint value);
GidgenFeatObject *gidgen_feat_object_return_none (void);
void gidgen_feat_object_reset_singleton (void);
gint gidgen_feat_object_compute (GidgenFeatObject *self, gint x);

/* Property and method name conflicts */

#define GIDGEN_FEAT_TYPE_CONFLICT (gidgen_feat_conflict_get_type ())
G_DECLARE_DERIVABLE_TYPE (GidgenFeatConflict, gidgen_feat_conflict, GIDGEN_FEAT, CONFLICT, GObject)

struct _GidgenFeatConflictClass
{
  GObjectClass parent_class;
};

GidgenFeatConflict *gidgen_feat_conflict_new (void);
gint gidgen_feat_conflict_level (GidgenFeatConflict *self);
gint gidgen_feat_conflict_clash (GidgenFeatConflict *self);
gint gidgen_feat_conflict_inherited (GidgenFeatConflict *self);

#define GIDGEN_FEAT_TYPE_CONFLICT_CHILD (gidgen_feat_conflict_child_get_type ())
G_DECLARE_FINAL_TYPE (GidgenFeatConflictChild, gidgen_feat_conflict_child, GIDGEN_FEAT, CONFLICT_CHILD,
                      GidgenFeatConflict)

GidgenFeatConflictChild *gidgen_feat_conflict_child_new (void);

/* Callbacks */

typedef gint (*GidgenFeatIntFunc) (gint value, gpointer user_data);

void gidgen_feat_set_notified (GidgenFeatIntFunc func, gpointer user_data, GDestroyNotify notify);
gint gidgen_feat_call_notified (gint value);
void gidgen_feat_clear_notified (void);

/* Opaque type with a free function only (not copyable) */

typedef struct _GidgenFeatNoCopy GidgenFeatNoCopy;

void gidgen_feat_no_copy_create (gint value, GidgenFeatNoCopy **self);
void gidgen_feat_no_copy_free (GidgenFeatNoCopy *self);
gint gidgen_feat_no_copy_get_value (GidgenFeatNoCopy *self);
void gidgen_feat_no_copy_set_value (GidgenFeatNoCopy *self, gint value);
GidgenFeatNoCopy *gidgen_feat_no_copy_return_none (void);
void gidgen_feat_no_copy_reset_singleton (void);
gint gidgen_feat_no_copy_take (GidgenFeatNoCopy *self);

/* Plain C structure (no GType) used by the field kinds structure */

typedef struct
{
  gint x;
  gint y;
} GidgenFeatPoint;

/* Structure with field kinds which can't be expressed by the matrix field cells */

typedef gint (*GidgenFeatFieldFunc) (gint value);

/**
 * GidgenFeatFieldKinds:
 * @value: Basic field
 * @flag_a: 1 bit field
 * @flag_b: 3 bit field
 * @nums: (array length=n_nums): Integer array with a length field
 * @n_nums: Length of @nums
 * @names: (array zero-terminated=1): Zero terminated string array
 * @pts: Inline fixed size array of plain structures
 * @func: Callback field of a callback typedef
 * @inline_func: Callback field with an inline callback type
 * @data: Untyped pointer field
 * @pptr: Pointer to pointer field
 * @strs: String array field without array annotation
 *
 * Structure with field kinds which aren't covered by the matrix field cells: embedded anonymous structure and
 * union, bit fields, array with a length field, fixed size structure array, callbacks, pointer fields and private
 * fields.
 */
typedef struct
{
  gint value;
  struct
  {
    gint a;
    gint b;
  } inner;
  union
  {
    gint i;
    gfloat f;
  } u;
  guint flag_a : 1;
  guint flag_b : 3;
  gint *nums;
  guint n_nums;
  gchar **names;
  GidgenFeatPoint pts[2];
  GidgenFeatFieldFunc func;
  gint (*inline_func) (gint x, gint y);
  gpointer data;
  gint **pptr;
  gchar **strs;

  /*< private >*/
  gint priv;
} GidgenFeatFieldKinds;

void gidgen_feat_field_kinds_fill (GidgenFeatFieldKinds *fk);
gchar *gidgen_feat_field_kinds_describe (const GidgenFeatFieldKinds *fk);

G_END_DECLS

#endif /* GIDGEN_FEAT_H */
