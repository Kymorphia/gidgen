/*
 * GidgenFeat - Hand written C test library for gidgen feature integration tests.
 * See gidgen-feat.h for details.
 */

#include "gidgen-feat.h"

static gint live_counts[GIDGEN_FEAT_COUNTER_LAST];

/**
 * gidgen_feat_live_count:
 * @counter: The counter to get
 *
 * Get the number of live instances for a counter. For %GIDGEN_FEAT_COUNTER_DESTROY_NOTIFY it is the number of
 * pending destroy notify callbacks (callbacks which have been set, but not yet destroyed).
 *
 * Returns: The live count
 */
gint
gidgen_feat_live_count (GidgenFeatCounter counter)
{
  return g_atomic_int_get (&live_counts[counter]);
}

#define COUNT_INC(counter) g_atomic_int_inc (&live_counts[counter])
#define COUNT_DEC(counter) g_atomic_int_dec_and_test (&live_counts[counter])

/* ---- Enums and flags ---- */

/**
 * gidgen_feat_color_next:
 * @color: A color
 *
 * Returns: The next color (wraps around)
 */
GidgenFeatColor
gidgen_feat_color_next (GidgenFeatColor color)
{
  switch (color)
    {
    case GIDGEN_FEAT_COLOR_RED: return GIDGEN_FEAT_COLOR_GREEN;
    case GIDGEN_FEAT_COLOR_GREEN: return GIDGEN_FEAT_COLOR_BLUE;
    default: return GIDGEN_FEAT_COLOR_RED;
    }
}

/**
 * gidgen_feat_style_toggle:
 * @style: Style flags
 * @toggle: Flags to toggle
 *
 * Returns: @style with @toggle flags toggled
 */
GidgenFeatStyle
gidgen_feat_style_toggle (GidgenFeatStyle style, GidgenFeatStyle toggle)
{
  return style ^ toggle;
}

/* ---- Errors ---- */

G_DEFINE_QUARK (gidgen-feat-error-quark, gidgen_feat_error)

/**
 * gidgen_feat_may_fail:
 * @fail: %TRUE to fail
 * @error: Return location for an error
 *
 * Returns: %TRUE on success, %FALSE on error
 */
gboolean
gidgen_feat_may_fail (gboolean fail, GError **error)
{
  if (fail)
    {
      g_set_error (error, GIDGEN_FEAT_ERROR, GIDGEN_FEAT_ERROR_INVALID, "Failed as requested");
      return FALSE;
    }

  return TRUE;
}

/**
 * gidgen_feat_may_fail_string:
 * @fail: %TRUE to fail
 * @error: Return location for an error
 *
 * Returns: (transfer full): A string on success, %NULL on error
 */
gchar *
gidgen_feat_may_fail_string (gboolean fail, GError **error)
{
  if (fail)
    {
      g_set_error (error, GIDGEN_FEAT_ERROR, GIDGEN_FEAT_ERROR_FAILED, "String failure");
      return NULL;
    }

  return g_strdup ("success");
}

/* ---- Interface ---- */

G_DEFINE_INTERFACE (GidgenFeatIface, gidgen_feat_iface, G_TYPE_OBJECT)

static void
gidgen_feat_iface_default_init (GidgenFeatIfaceInterface *iface)
{
}

/**
 * gidgen_feat_iface_get_id:
 * @self: Interface instance
 *
 * Returns: The ID
 */
gint
gidgen_feat_iface_get_id (GidgenFeatIface *self)
{
  return GIDGEN_FEAT_IFACE_GET_IFACE (self)->get_id (self);
}

/* ---- Object ---- */

typedef struct
{
  gint value;
  gchar *name;
} GidgenFeatObjectPrivate;

enum
{
  PROP_0,
  PROP_VALUE,
  PROP_NAME,
  N_PROPS
};

enum
{
  SIGNAL_CHANGED,
  N_SIGNALS
};

static GParamSpec *obj_props[N_PROPS];
static guint obj_signals[N_SIGNALS];

static void gidgen_feat_object_iface_init (GidgenFeatIfaceInterface *iface);

G_DEFINE_TYPE_WITH_CODE (GidgenFeatObject, gidgen_feat_object, G_TYPE_OBJECT,
                         G_ADD_PRIVATE (GidgenFeatObject)
                         G_IMPLEMENT_INTERFACE (GIDGEN_FEAT_TYPE_IFACE, gidgen_feat_object_iface_init))

static gint
gidgen_feat_object_iface_get_id (GidgenFeatIface *iface)
{
  return gidgen_feat_object_get_value (GIDGEN_FEAT_OBJECT (iface)) * 100;
}

static void
gidgen_feat_object_iface_init (GidgenFeatIfaceInterface *iface)
{
  iface->get_id = gidgen_feat_object_iface_get_id;
}

static gint
gidgen_feat_object_real_compute (GidgenFeatObject *self, gint x)
{
  return gidgen_feat_object_get_value (self) * x;
}

static void
gidgen_feat_object_finalize (GObject *object)
{
  GidgenFeatObjectPrivate *priv = gidgen_feat_object_get_instance_private (GIDGEN_FEAT_OBJECT (object));
  g_free (priv->name);
  COUNT_DEC (GIDGEN_FEAT_COUNTER_OBJECT);
  G_OBJECT_CLASS (gidgen_feat_object_parent_class)->finalize (object);
}

static void
gidgen_feat_object_get_property (GObject *object, guint prop_id, GValue *value, GParamSpec *pspec)
{
  GidgenFeatObjectPrivate *priv = gidgen_feat_object_get_instance_private (GIDGEN_FEAT_OBJECT (object));

  switch (prop_id)
    {
    case PROP_VALUE:
      g_value_set_int (value, priv->value);
      break;
    case PROP_NAME:
      g_value_set_string (value, priv->name);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
    }
}

static void
gidgen_feat_object_set_property (GObject *object, guint prop_id, const GValue *value, GParamSpec *pspec)
{
  GidgenFeatObject *self = GIDGEN_FEAT_OBJECT (object);

  switch (prop_id)
    {
    case PROP_VALUE:
      gidgen_feat_object_set_value (self, g_value_get_int (value));
      break;
    case PROP_NAME:
      gidgen_feat_object_set_name (self, g_value_get_string (value));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
    }
}

static void
gidgen_feat_object_class_init (GidgenFeatObjectClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->finalize = gidgen_feat_object_finalize;
  klass->compute = gidgen_feat_object_real_compute;
  object_class->get_property = gidgen_feat_object_get_property;
  object_class->set_property = gidgen_feat_object_set_property;

  /**
   * GidgenFeatObject:value:
   *
   * Integer value property.
   */
  obj_props[PROP_VALUE] = g_param_spec_int ("value", NULL, NULL, G_MININT, G_MAXINT, 0,
                                            G_PARAM_READWRITE | G_PARAM_EXPLICIT_NOTIFY | G_PARAM_STATIC_STRINGS);

  /**
   * GidgenFeatObject:name:
   *
   * String name property.
   */
  obj_props[PROP_NAME] = g_param_spec_string ("name", NULL, NULL, NULL,
                                              G_PARAM_READWRITE | G_PARAM_EXPLICIT_NOTIFY | G_PARAM_STATIC_STRINGS);

  g_object_class_install_properties (object_class, N_PROPS, obj_props);

  /**
   * GidgenFeatObject::changed:
   * @self: The object
   * @value: The new value
   *
   * Emitted by gidgen_feat_object_emit_changed().
   */
  obj_signals[SIGNAL_CHANGED] = g_signal_new ("changed", G_TYPE_FROM_CLASS (klass), G_SIGNAL_RUN_LAST, 0,
                                              NULL, NULL, NULL, G_TYPE_NONE, 1, G_TYPE_INT);
}

static void
gidgen_feat_object_init (GidgenFeatObject *self)
{
  COUNT_INC (GIDGEN_FEAT_COUNTER_OBJECT);
}

/**
 * gidgen_feat_object_new:
 *
 * Returns: (transfer full): New object
 */
GidgenFeatObject *
gidgen_feat_object_new (void)
{
  return g_object_new (GIDGEN_FEAT_TYPE_OBJECT, NULL);
}

/**
 * gidgen_feat_object_new_with_value:
 * @value: Initial value
 *
 * Returns: (transfer full): New object
 */
GidgenFeatObject *
gidgen_feat_object_new_with_value (gint value)
{
  return g_object_new (GIDGEN_FEAT_TYPE_OBJECT, "value", value, NULL);
}

/**
 * gidgen_feat_object_get_value:
 * @self: Object
 *
 * Returns: The value
 */
gint
gidgen_feat_object_get_value (GidgenFeatObject *self)
{
  GidgenFeatObjectPrivate *priv = gidgen_feat_object_get_instance_private (self);
  return priv->value;
}

/**
 * gidgen_feat_object_set_value:
 * @self: Object
 * @value: The value
 */
void
gidgen_feat_object_set_value (GidgenFeatObject *self, gint value)
{
  GidgenFeatObjectPrivate *priv = gidgen_feat_object_get_instance_private (self);

  if (priv->value != value)
    {
      priv->value = value;
      g_object_notify_by_pspec (G_OBJECT (self), obj_props[PROP_VALUE]);
    }
}

/**
 * gidgen_feat_object_get_name:
 * @self: Object
 *
 * Returns: (transfer none) (nullable): The name
 */
const gchar *
gidgen_feat_object_get_name (GidgenFeatObject *self)
{
  GidgenFeatObjectPrivate *priv = gidgen_feat_object_get_instance_private (self);
  return priv->name;
}

/**
 * gidgen_feat_object_set_name:
 * @self: Object
 * @name: (nullable): The name
 */
void
gidgen_feat_object_set_name (GidgenFeatObject *self, const gchar *name)
{
  GidgenFeatObjectPrivate *priv = gidgen_feat_object_get_instance_private (self);

  if (g_set_str (&priv->name, name))
    g_object_notify_by_pspec (G_OBJECT (self), obj_props[PROP_NAME]);
}

/**
 * gidgen_feat_object_get_ref_count:
 * @self: Object
 *
 * Returns: The current GObject reference count of the object
 */
guint
gidgen_feat_object_get_ref_count (GidgenFeatObject *self)
{
  return g_atomic_int_get ((gint *)&G_OBJECT (self)->ref_count);
}

/**
 * gidgen_feat_object_emit_changed:
 * @self: Object
 * @value: Value to emit with the changed signal
 */
void
gidgen_feat_object_emit_changed (GidgenFeatObject *self, gint value)
{
  g_signal_emit (self, obj_signals[SIGNAL_CHANGED], 0, value);
}

static GidgenFeatObject *object_singleton;

/**
 * gidgen_feat_object_return_none:
 *
 * Returns: (transfer none): Singleton object owned by the library (value = 7)
 */
GidgenFeatObject *
gidgen_feat_object_return_none (void)
{
  if (!object_singleton)
    object_singleton = gidgen_feat_object_new_with_value (7);

  return object_singleton;
}

/**
 * gidgen_feat_object_reset_singleton:
 *
 * Release the library's reference to the singleton object returned by gidgen_feat_object_return_none().
 */
void
gidgen_feat_object_reset_singleton (void)
{
  g_clear_object (&object_singleton);
}

/* ---- Callbacks ---- */

/**
 * GidgenFeatIntFunc:
 * @value: Value passed to the callback
 * @user_data: (closure): User data
 *
 * Returns: Callback result
 */

static GidgenFeatIntFunc notified_func;
static gpointer notified_data;
static GDestroyNotify notified_notify;

/**
 * gidgen_feat_clear_notified:
 *
 * Clear the callback set with gidgen_feat_set_notified(), calling its destroy notify.
 */
void
gidgen_feat_clear_notified (void)
{
  if (notified_notify)
    {
      notified_notify (notified_data);
      COUNT_DEC (GIDGEN_FEAT_COUNTER_DESTROY_NOTIFY);
    }

  notified_func = NULL;
  notified_data = NULL;
  notified_notify = NULL;
}

/**
 * gidgen_feat_set_notified:
 * @func: (scope notified) (closure user_data) (destroy notify): Callback to store
 * @user_data: User data
 * @notify: Destroy notify for @user_data
 *
 * Store a callback which can be called with gidgen_feat_call_notified(). Any previous callback is cleared.
 */
void
gidgen_feat_set_notified (GidgenFeatIntFunc func, gpointer user_data, GDestroyNotify notify)
{
  gidgen_feat_clear_notified ();
  notified_func = func;
  notified_data = user_data;
  notified_notify = notify;

  if (notify)
    COUNT_INC (GIDGEN_FEAT_COUNTER_DESTROY_NOTIFY);
}

/**
 * gidgen_feat_call_notified:
 * @value: Value to pass to the stored callback
 *
 * Returns: The result of the callback or -1 if none is set
 */
gint
gidgen_feat_call_notified (gint value)
{
  return notified_func ? notified_func (value, notified_data) : -1;
}

/* ---- Non-copyable opaque type ---- */

/**
 * GidgenFeatNoCopy: (free-func gidgen_feat_no_copy_free)
 *
 * Opaque type with a free function and no copy function or GType.
 */

struct _GidgenFeatNoCopy
{
  gint value;
};

/**
 * gidgen_feat_no_copy_create:
 * @value: Initial value
 * @self: (out) (transfer full): New instance (g-ir-scanner does not allow transfer full returns of non-copyable
 *   records, but out parameters are accepted)
 */
void
gidgen_feat_no_copy_create (gint value, GidgenFeatNoCopy **self)
{
  *self = g_new0 (GidgenFeatNoCopy, 1);
  (*self)->value = value;
  COUNT_INC (GIDGEN_FEAT_COUNTER_NO_COPY);
}

/**
 * gidgen_feat_no_copy_free:
 * @self: Instance
 */
void
gidgen_feat_no_copy_free (GidgenFeatNoCopy *self)
{
  g_free (self);
  COUNT_DEC (GIDGEN_FEAT_COUNTER_NO_COPY);
}

/**
 * gidgen_feat_no_copy_get_value:
 * @self: Instance
 *
 * Returns: The value
 */
gint
gidgen_feat_no_copy_get_value (GidgenFeatNoCopy *self)
{
  return self->value;
}

/**
 * gidgen_feat_no_copy_set_value:
 * @self: Instance
 * @value: The value
 */
void
gidgen_feat_no_copy_set_value (GidgenFeatNoCopy *self, gint value)
{
  self->value = value;
}

static GidgenFeatNoCopy *no_copy_singleton;

/**
 * gidgen_feat_no_copy_return_none:
 *
 * Returns: (transfer none): Singleton instance owned by the library (value = 13)
 */
GidgenFeatNoCopy *
gidgen_feat_no_copy_return_none (void)
{
  if (!no_copy_singleton)
    gidgen_feat_no_copy_create (13, &no_copy_singleton);

  return no_copy_singleton;
}

/**
 * gidgen_feat_no_copy_reset_singleton:
 *
 * Free the singleton returned by gidgen_feat_no_copy_return_none().
 */
void
gidgen_feat_no_copy_reset_singleton (void)
{
  g_clear_pointer (&no_copy_singleton, gidgen_feat_no_copy_free);
}

/**
 * gidgen_feat_no_copy_take:
 * @self: (transfer full): Instance which the function takes ownership of and frees
 *
 * Returns: The value of @self
 */
gint
gidgen_feat_no_copy_take (GidgenFeatNoCopy *self)
{
  gint value = self->value;
  gidgen_feat_no_copy_free (self);
  return value;
}

/* ---- Plain C structure ---- */

/**
 * GidgenFeatPoint:
 * @x: X coordinate
 * @y: Y coordinate
 *
 * Plain C structure with basic fields and no GType.
 */

static gint
field_kinds_double (gint value)
{
  return value * 2;
}

static gint
field_kinds_add (gint x, gint y)
{
  return x + y;
}

/**
 * gidgen_feat_field_kinds_fill:
 * @fk: Field kinds structure
 *
 * Fill all fields with test values (pointer fields point to static data).
 */
void
gidgen_feat_field_kinds_fill (GidgenFeatFieldKinds *fk)
{
  static gint nums[] = { 5, 6, 7 };
  static gchar *names[] = { "x", "y", NULL };

  fk->value = 1;
  fk->inner.a = 2;
  fk->inner.b = 3;
  fk->u.i = 4;
  fk->flag_a = 1;
  fk->flag_b = 5;
  fk->nums = nums;
  fk->n_nums = G_N_ELEMENTS (nums);
  fk->names = names;
  fk->pts[0] = (GidgenFeatPoint){ 8, 9 };
  fk->pts[1] = (GidgenFeatPoint){ 10, 11 };
  fk->func = field_kinds_double;
  fk->inline_func = field_kinds_add;
  fk->data = NULL;
  fk->pptr = NULL;
  fk->strs = NULL;
  fk->priv = 12;
}

/**
 * gidgen_feat_field_kinds_describe:
 * @fk: Field kinds structure
 *
 * Returns: (transfer full): Description of the basic, bit and fixed size array fields and the callback results
 */
gchar *
gidgen_feat_field_kinds_describe (const GidgenFeatFieldKinds *fk)
{
  return g_strdup_printf ("%d %u %u %d,%d %d,%d %d %d", fk->value, fk->flag_a, fk->flag_b, fk->pts[0].x, fk->pts[0].y,
                          fk->pts[1].x, fk->pts[1].y, fk->func ? fk->func (3) : -1,
                          fk->inline_func ? fk->inline_func (3, 4) : -1);
}

/* ---- Property and method name conflicts ---- */

enum { CONFLICT_PROP_0, CONFLICT_PROP_LEVEL, CONFLICT_PROP_CLASH, N_CONFLICT_PROPS };

G_DEFINE_TYPE (GidgenFeatConflict, gidgen_feat_conflict, G_TYPE_OBJECT)

static void
gidgen_feat_conflict_get_property (GObject *object, guint prop_id, GValue *value, GParamSpec *pspec)
{
  switch (prop_id)
    {
    case CONFLICT_PROP_LEVEL:
      g_value_set_int (value, 1);
      break;
    case CONFLICT_PROP_CLASH:
      g_value_set_int (value, 2);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
    }
}

static void
gidgen_feat_conflict_class_init (GidgenFeatConflictClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->get_property = gidgen_feat_conflict_get_property;

  /**
   * GidgenFeatConflict:level: (getter level)
   *
   * Property with a getter method of the same name.
   */
  g_object_class_install_property (object_class, CONFLICT_PROP_LEVEL,
    g_param_spec_int ("level", NULL, NULL, 0, 100, 0, G_PARAM_READABLE | G_PARAM_STATIC_STRINGS));

  /**
   * GidgenFeatConflict:clash:
   *
   * Property which conflicts with a method of the same name (not its getter).
   */
  g_object_class_install_property (object_class, CONFLICT_PROP_CLASH,
    g_param_spec_int ("clash", NULL, NULL, 0, 100, 0, G_PARAM_READABLE | G_PARAM_STATIC_STRINGS));
}

static void
gidgen_feat_conflict_init (GidgenFeatConflict *self)
{
}

/**
 * gidgen_feat_conflict_new:
 *
 * Returns: (transfer full): A new conflict object
 */
GidgenFeatConflict *
gidgen_feat_conflict_new (void)
{
  return g_object_new (GIDGEN_FEAT_TYPE_CONFLICT, NULL);
}

/**
 * gidgen_feat_conflict_level: (get-property level)
 * @self: The object
 *
 * Returns: The level property value
 */
gint
gidgen_feat_conflict_level (GidgenFeatConflict *self)
{
  return 1;
}

/**
 * gidgen_feat_conflict_clash:
 * @self: The object
 *
 * Returns: 20 (differs from the clash property value)
 */
gint
gidgen_feat_conflict_clash (GidgenFeatConflict *self)
{
  return 20;
}

/**
 * gidgen_feat_conflict_inherited:
 * @self: The object
 *
 * Returns: 30
 */
gint
gidgen_feat_conflict_inherited (GidgenFeatConflict *self)
{
  return 30;
}

struct _GidgenFeatConflictChild
{
  GidgenFeatConflict parent_instance;
};

enum { CONFLICT_CHILD_PROP_0, CONFLICT_CHILD_PROP_INHERITED, N_CONFLICT_CHILD_PROPS };

G_DEFINE_FINAL_TYPE (GidgenFeatConflictChild, gidgen_feat_conflict_child, GIDGEN_FEAT_TYPE_CONFLICT)

static void
gidgen_feat_conflict_child_get_property (GObject *object, guint prop_id, GValue *value, GParamSpec *pspec)
{
  if (prop_id == CONFLICT_CHILD_PROP_INHERITED)
    g_value_set_int (value, 3);
  else
    G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
}

static void
gidgen_feat_conflict_child_class_init (GidgenFeatConflictChildClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->get_property = gidgen_feat_conflict_child_get_property;

  /**
   * GidgenFeatConflictChild:inherited:
   *
   * Property with the name of a parent class method.
   */
  g_object_class_install_property (object_class, CONFLICT_CHILD_PROP_INHERITED,
    g_param_spec_int ("inherited", NULL, NULL, 0, 100, 0, G_PARAM_READABLE | G_PARAM_STATIC_STRINGS));
}

static void
gidgen_feat_conflict_child_init (GidgenFeatConflictChild *self)
{
}

/**
 * gidgen_feat_conflict_child_new:
 *
 * Returns: (transfer full): A new conflict child object
 */
GidgenFeatConflictChild *
gidgen_feat_conflict_child_new (void)
{
  return g_object_new (GIDGEN_FEAT_TYPE_CONFLICT_CHILD, NULL);
}

/* ---- Enum and flags GTypes ---- */

GType
gidgen_feat_color_get_type (void)
{
  static gsize type_id = 0;
  static const GEnumValue values[] = {
    { GIDGEN_FEAT_COLOR_RED, "GIDGEN_FEAT_COLOR_RED", "red" },
    { GIDGEN_FEAT_COLOR_GREEN, "GIDGEN_FEAT_COLOR_GREEN", "green" },
    { GIDGEN_FEAT_COLOR_BLUE, "GIDGEN_FEAT_COLOR_BLUE", "blue" },
    { 0, NULL, NULL }
  };

  if (g_once_init_enter (&type_id))
    g_once_init_leave (&type_id, g_enum_register_static (g_intern_static_string ("GidgenFeatColor"), values));

  return type_id;
}

GType
gidgen_feat_style_get_type (void)
{
  static gsize type_id = 0;
  static const GFlagsValue values[] = {
    { GIDGEN_FEAT_STYLE_NONE, "GIDGEN_FEAT_STYLE_NONE", "none" },
    { GIDGEN_FEAT_STYLE_BOLD, "GIDGEN_FEAT_STYLE_BOLD", "bold" },
    { GIDGEN_FEAT_STYLE_ITALIC, "GIDGEN_FEAT_STYLE_ITALIC", "italic" },
    { GIDGEN_FEAT_STYLE_UNDERLINE, "GIDGEN_FEAT_STYLE_UNDERLINE", "underline" },
    { 0, NULL, NULL }
  };

  if (g_once_init_enter (&type_id))
    g_once_init_leave (&type_id, g_flags_register_static (g_intern_static_string ("GidgenFeatStyle"), values));

  return type_id;
}

/* ---- Object virtual method ---- */

/**
 * gidgen_feat_object_compute:
 * @self: Object
 * @x: Value
 *
 * Calls the compute virtual method (default implementation returns value * @x).
 *
 * Returns: Result
 */
gint
gidgen_feat_object_compute (GidgenFeatObject *self, gint x)
{
  return GIDGEN_FEAT_OBJECT_GET_CLASS (self)->compute (self, x);
}

/* ---- Interface with a prerequisite interface ---- */

G_DEFINE_INTERFACE_WITH_CODE (GidgenFeatChildIface, gidgen_feat_child_iface, G_TYPE_OBJECT,
                              g_type_interface_add_prerequisite (g_define_type_id, GIDGEN_FEAT_TYPE_IFACE))

static void
gidgen_feat_child_iface_default_init (GidgenFeatChildIfaceInterface *iface)
{
}

/**
 * gidgen_feat_child_iface_get_level:
 * @self: Interface instance
 *
 * Returns: The level
 */
gint
gidgen_feat_child_iface_get_level (GidgenFeatChildIface *self)
{
  return GIDGEN_FEAT_CHILD_IFACE_GET_IFACE (self)->get_level (self);
}

#ifndef __GI_SCANNER__
/* Private object type (not in the GIR) implementing GidgenFeatChildIface and its GidgenFeatIface prerequisite, the
 * D binding uses the interface proxy object for it */

#define GIDGEN_FEAT_TYPE_HIDDEN (gidgen_feat_hidden_get_type ())
G_DECLARE_FINAL_TYPE (GidgenFeatHidden, gidgen_feat_hidden, GIDGEN_FEAT, HIDDEN, GObject)

struct _GidgenFeatHidden
{
  GObject parent_instance;
  gint value;
};

static void gidgen_feat_hidden_iface_init (GidgenFeatIfaceInterface *iface);
static void gidgen_feat_hidden_child_iface_init (GidgenFeatChildIfaceInterface *iface);

G_DEFINE_TYPE_WITH_CODE (GidgenFeatHidden, gidgen_feat_hidden, G_TYPE_OBJECT,
                         G_IMPLEMENT_INTERFACE (GIDGEN_FEAT_TYPE_IFACE, gidgen_feat_hidden_iface_init)
                         G_IMPLEMENT_INTERFACE (GIDGEN_FEAT_TYPE_CHILD_IFACE, gidgen_feat_hidden_child_iface_init))

static gint
gidgen_feat_hidden_get_id (GidgenFeatIface *iface)
{
  return GIDGEN_FEAT_HIDDEN (iface)->value * 100;
}

static void
gidgen_feat_hidden_iface_init (GidgenFeatIfaceInterface *iface)
{
  iface->get_id = gidgen_feat_hidden_get_id;
}

static gint
gidgen_feat_hidden_get_level (GidgenFeatChildIface *iface)
{
  return GIDGEN_FEAT_HIDDEN (iface)->value;
}

static void
gidgen_feat_hidden_child_iface_init (GidgenFeatChildIfaceInterface *iface)
{
  iface->get_level = gidgen_feat_hidden_get_level;
}

static void
gidgen_feat_hidden_finalize (GObject *object)
{
  COUNT_DEC (GIDGEN_FEAT_COUNTER_OBJECT);
  G_OBJECT_CLASS (gidgen_feat_hidden_parent_class)->finalize (object);
}

static void
gidgen_feat_hidden_class_init (GidgenFeatHiddenClass *klass)
{
  G_OBJECT_CLASS (klass)->finalize = gidgen_feat_hidden_finalize;
}

static void
gidgen_feat_hidden_init (GidgenFeatHidden *self)
{
  COUNT_INC (GIDGEN_FEAT_COUNTER_OBJECT);
}
#endif

/**
 * gidgen_feat_new_hidden_child:
 * @value: Value of the object (the level, the ID is 100 times the value)
 *
 * Create an object of a private type (not in the GIR) which implements #GidgenFeatChildIface.
 *
 * Returns: (transfer full): The new object
 */
GidgenFeatChildIface *
gidgen_feat_new_hidden_child (gint value)
{
  GidgenFeatHidden *self = g_object_new (GIDGEN_FEAT_TYPE_HIDDEN, NULL);
  self->value = value;
  return GIDGEN_FEAT_CHILD_IFACE (self);
}

/* ---- Object subclass implementing GidgenFeatChildIface ---- */

struct _GidgenFeatChildObject
{
  GidgenFeatObject parent_instance;
};

static void gidgen_feat_child_object_child_iface_init (GidgenFeatChildIfaceInterface *iface);

G_DEFINE_TYPE_WITH_CODE (GidgenFeatChildObject, gidgen_feat_child_object, GIDGEN_FEAT_TYPE_OBJECT,
                         G_IMPLEMENT_INTERFACE (GIDGEN_FEAT_TYPE_CHILD_IFACE, gidgen_feat_child_object_child_iface_init))

static gint
gidgen_feat_child_object_get_level (GidgenFeatChildIface *iface)
{
  return gidgen_feat_object_get_value (GIDGEN_FEAT_OBJECT (iface)) * 10;
}

static void
gidgen_feat_child_object_child_iface_init (GidgenFeatChildIfaceInterface *iface)
{
  iface->get_level = gidgen_feat_child_object_get_level;
}

static void
gidgen_feat_child_object_class_init (GidgenFeatChildObjectClass *klass)
{
}

static void
gidgen_feat_child_object_init (GidgenFeatChildObject *self)
{
}

/**
 * gidgen_feat_child_object_new:
 * @value: Object value
 *
 * Returns: (transfer full): A new child object
 */
GidgenFeatChildObject *
gidgen_feat_child_object_new (gint value)
{
  return g_object_new (GIDGEN_FEAT_TYPE_CHILD_OBJECT, "value", value, NULL);
}
