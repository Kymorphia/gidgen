/*
 * GidgenMatrix support - Hand written part of the GidgenMatrix test library. See gidgen-matrix-support.h.
 */

#include <string.h>

#include "gidgen-matrix-private.h"

/* ---- Live counters and failure recording ---- */

static gint live_counts[GIDGEN_MATRIX_COUNTER_LAST];
static gint failure_count;
static gchar *last_failure;
static gpointer kept_ptr;
static GDestroyNotify kept_free;

/**
 * gidgen_matrix_live_count:
 * @counter: The counter to get
 *
 * Returns: Number of live instances of a test type
 */
gint
gidgen_matrix_live_count (GidgenMatrixCounter counter)
{
  return g_atomic_int_get (&live_counts[counter]);
}

/**
 * gidgen_matrix_failure_count:
 *
 * Returns: Number of failed value checks in matrix functions so far
 */
gint
gidgen_matrix_failure_count (void)
{
  return failure_count;
}

/**
 * gidgen_matrix_last_failure:
 *
 * Returns: (nullable): Description of the last failed value check or %NULL if none
 */
const gchar *
gidgen_matrix_last_failure (void)
{
  return last_failure;
}

/**
 * gidgen_matrix_release_kept:
 *
 * Release the value kept alive for the last transfer none out or return value (if any).
 */
void
gidgen_matrix_release_kept (void)
{
  gpointer ptr = kept_ptr;
  kept_ptr = NULL;

  if (ptr)
    kept_free (ptr);
}

void
gidgen_matrix_fail (const gchar *func, const gchar *what)
{
  failure_count++;
  g_free (last_failure);
  last_failure = g_strdup_printf ("%s: CHECK (%s) failed", func, what);
}

gpointer
gidgen_matrix_keep (gpointer ptr, GDestroyNotify free_func)
{
  gidgen_matrix_release_kept ();
  kept_ptr = ptr;
  kept_free = free_func;
  return ptr;
}

/* The value kept by the last KEEP (or NULL if it was released) */
gpointer
gidgen_matrix_kept (void)
{
  return kept_ptr;
}

/* ---- Enum and flags ---- */

GType
gidgen_matrix_fruit_get_type (void)
{
  static gsize type_id = 0;
  static const GEnumValue values[] = {
    { GIDGEN_MATRIX_FRUIT_APPLE, "GIDGEN_MATRIX_FRUIT_APPLE", "apple" },
    { GIDGEN_MATRIX_FRUIT_BANANA, "GIDGEN_MATRIX_FRUIT_BANANA", "banana" },
    { GIDGEN_MATRIX_FRUIT_CHERRY, "GIDGEN_MATRIX_FRUIT_CHERRY", "cherry" },
    { 0, NULL, NULL }
  };

  if (g_once_init_enter (&type_id))
    g_once_init_leave (&type_id, g_enum_register_static (g_intern_static_string ("GidgenMatrixFruit"), values));

  return type_id;
}

GType
gidgen_matrix_bits_get_type (void)
{
  static gsize type_id = 0;
  static const GFlagsValue values[] = {
    { GIDGEN_MATRIX_BITS_NONE, "GIDGEN_MATRIX_BITS_NONE", "none" },
    { GIDGEN_MATRIX_BITS_A, "GIDGEN_MATRIX_BITS_A", "a" },
    { GIDGEN_MATRIX_BITS_B, "GIDGEN_MATRIX_BITS_B", "b" },
    { GIDGEN_MATRIX_BITS_C, "GIDGEN_MATRIX_BITS_C", "c" },
    { 0, NULL, NULL }
  };

  if (g_once_init_enter (&type_id))
    g_once_init_leave (&type_id, g_flags_register_static (g_intern_static_string ("GidgenMatrixBits"), values));

  return type_id;
}

/* ---- GObject class ---- */

/**
 * GidgenMatrixObj:
 *
 * GObject class test type with an integer value.
 */
struct _GidgenMatrixObj
{
  GObject parent_instance;
  gint value;
};

G_DEFINE_FINAL_TYPE (GidgenMatrixObj, gidgen_matrix_obj, G_TYPE_OBJECT)

static void
gidgen_matrix_obj_finalize (GObject *object)
{
  g_atomic_int_add (&live_counts[GIDGEN_MATRIX_COUNTER_OBJ], -1);
  G_OBJECT_CLASS (gidgen_matrix_obj_parent_class)->finalize (object);
}

static void
gidgen_matrix_obj_class_init (GidgenMatrixObjClass *klass)
{
  G_OBJECT_CLASS (klass)->finalize = gidgen_matrix_obj_finalize;
}

static void
gidgen_matrix_obj_init (GidgenMatrixObj *self)
{
  g_atomic_int_inc (&live_counts[GIDGEN_MATRIX_COUNTER_OBJ]);
}

/**
 * gidgen_matrix_obj_new:
 * @value: The value
 *
 * Returns: (transfer full): A new object
 */
GidgenMatrixObj *
gidgen_matrix_obj_new (gint value)
{
  GidgenMatrixObj *self = g_object_new (GIDGEN_MATRIX_TYPE_OBJ, NULL);
  self->value = value;
  return self;
}

/**
 * gidgen_matrix_obj_get_value:
 * @self: The object
 *
 * Returns: The value
 */
gint
gidgen_matrix_obj_get_value (GidgenMatrixObj *self)
{
  return self->value;
}

/**
 * gidgen_matrix_obj_get_ref_count:
 * @self: The object
 *
 * Returns: The reference count of the object
 */
guint
gidgen_matrix_obj_get_ref_count (GidgenMatrixObj *self)
{
  return g_atomic_int_get ((gint *)&G_OBJECT (self)->ref_count);
}

/* ---- Boxed record ---- */

G_DEFINE_BOXED_TYPE (GidgenMatrixBox, gidgen_matrix_box, gidgen_matrix_box_copy, gidgen_matrix_box_free)

/**
 * gidgen_matrix_box_new:
 * @value: The value
 *
 * Returns: (transfer full): A new boxed instance
 */
GidgenMatrixBox *
gidgen_matrix_box_new (gint value)
{
  GidgenMatrixBox *box = g_new0 (GidgenMatrixBox, 1);
  box->value = value;
  g_atomic_int_inc (&live_counts[GIDGEN_MATRIX_COUNTER_BOX]);
  return box;
}

/**
 * gidgen_matrix_box_copy:
 * @box: The box to copy
 *
 * Returns: (transfer full): A copy of @box
 */
GidgenMatrixBox *
gidgen_matrix_box_copy (const GidgenMatrixBox *box)
{
  return gidgen_matrix_box_new (box->value);
}

/**
 * gidgen_matrix_box_free:
 * @box: The box to free
 */
void
gidgen_matrix_box_free (GidgenMatrixBox *box)
{
  g_atomic_int_add (&live_counts[GIDGEN_MATRIX_COUNTER_BOX], -1);
  g_free (box);
}

/* ---- Boxed record with a pointer field ---- */

G_DEFINE_BOXED_TYPE (GidgenMatrixRec, gidgen_matrix_rec, gidgen_matrix_rec_copy, gidgen_matrix_rec_free)

/**
 * gidgen_matrix_rec_new:
 * @value: The value
 *
 * Returns: (transfer full): A new record with name "rec"
 */
GidgenMatrixRec *
gidgen_matrix_rec_new (gint value)
{
  GidgenMatrixRec *rec = g_new0 (GidgenMatrixRec, 1);
  rec->value = value;
  rec->name = g_strdup ("rec");
  g_atomic_int_inc (&live_counts[GIDGEN_MATRIX_COUNTER_REC]);
  return rec;
}

/**
 * gidgen_matrix_rec_copy:
 * @rec: The record to copy
 *
 * Returns: (transfer full): A copy of @rec
 */
GidgenMatrixRec *
gidgen_matrix_rec_copy (const GidgenMatrixRec *rec)
{
  GidgenMatrixRec *copy = gidgen_matrix_rec_new (rec->value);
  g_free (copy->name);
  copy->name = g_strdup (rec->name);
  return copy;
}

/**
 * gidgen_matrix_rec_free:
 * @rec: The record to free
 */
void
gidgen_matrix_rec_free (GidgenMatrixRec *rec)
{
  g_atomic_int_add (&live_counts[GIDGEN_MATRIX_COUNTER_REC], -1);
  g_free (rec->name);
  g_free (rec);
}

/* ---- Reference counted fundamental type ---- */

/**
 * GidgenMatrixReffed: (ref-func gidgen_matrix_reffed_ref) (unref-func gidgen_matrix_reffed_unref)
 *
 * Reference counted fundamental type test type with an integer value.
 */
struct _GidgenMatrixReffed
{
  GTypeInstance parent_instance;
  gint ref_count;
  gint value;
};

typedef struct
{
  GTypeClass parent_class;
} GidgenMatrixReffedClass;

GType
gidgen_matrix_reffed_get_type (void)
{
  static gsize type_id = 0;

  if (g_once_init_enter (&type_id))
  {
    static const GTypeFundamentalInfo finfo = {
      G_TYPE_FLAG_CLASSED | G_TYPE_FLAG_INSTANTIATABLE | G_TYPE_FLAG_DERIVABLE | G_TYPE_FLAG_DEEP_DERIVABLE
    };
    static const GTypeInfo info = {
      sizeof (GidgenMatrixReffedClass), NULL, NULL, NULL, NULL, NULL, sizeof (GidgenMatrixReffed), 0, NULL, NULL
    };
    g_once_init_leave (&type_id, g_type_register_fundamental (g_type_fundamental_next (),
      g_intern_static_string ("GidgenMatrixReffed"), &info, &finfo, 0));
  }

  return type_id;
}

/**
 * gidgen_matrix_reffed_new:
 * @value: The value
 *
 * Returns: (transfer full): A new instance with a reference count of 1
 */
GidgenMatrixReffed *
gidgen_matrix_reffed_new (gint value)
{
  GidgenMatrixReffed *self = (GidgenMatrixReffed *)g_type_create_instance (GIDGEN_MATRIX_TYPE_REFFED);
  self->ref_count = 1;
  self->value = value;
  g_atomic_int_inc (&live_counts[GIDGEN_MATRIX_COUNTER_REFFED]);
  return self;
}

/**
 * gidgen_matrix_reffed_ref:
 * @self: The instance
 *
 * Returns: (transfer full): @self
 */
GidgenMatrixReffed *
gidgen_matrix_reffed_ref (GidgenMatrixReffed *self)
{
  g_atomic_int_inc (&self->ref_count);
  return self;
}

/**
 * gidgen_matrix_reffed_unref:
 * @self: (transfer full): The instance
 */
void
gidgen_matrix_reffed_unref (GidgenMatrixReffed *self)
{
  if (g_atomic_int_dec_and_test (&self->ref_count))
  {
    g_type_free_instance ((GTypeInstance *)self);
    g_atomic_int_add (&live_counts[GIDGEN_MATRIX_COUNTER_REFFED], -1);
  }
}

/**
 * gidgen_matrix_reffed_get_value:
 * @self: The instance
 *
 * Returns: The value
 */
gint
gidgen_matrix_reffed_get_value (GidgenMatrixReffed *self)
{
  return self->value;
}

/**
 * gidgen_matrix_reffed_get_ref_count:
 * @self: The instance
 *
 * Returns: The reference count of the instance
 */
guint
gidgen_matrix_reffed_get_ref_count (GidgenMatrixReffed *self)
{
  return g_atomic_int_get (&self->ref_count);
}

/* ---- Container helpers (see gidgen-matrix-private.h) ---- */

static gpointer
gm_elem_new (GmKind kind, gintptr v)
{
  switch (kind)
  {
    case GM_STR: return g_strdup ((const gchar *)v);
    case GM_OBJ: return gidgen_matrix_obj_new ((gint)v);
    case GM_REC: return gidgen_matrix_rec_new ((gint)v);
    case GM_REFFED: return gidgen_matrix_reffed_new ((gint)v);
    default: return GINT_TO_POINTER (v);
  }
}

static gboolean
gm_elem_eq (GmKind kind, gconstpointer elem, gintptr v)
{
  switch (kind)
  {
    case GM_STR: return g_strcmp0 (elem, (const gchar *)v) == 0;
    case GM_OBJ: return GIDGEN_MATRIX_IS_OBJ ((gpointer)elem) && gidgen_matrix_obj_get_value ((GidgenMatrixObj *)elem) == v;
    case GM_REC: return elem && ((const GidgenMatrixRec *)elem)->value == v
      && g_strcmp0 (((const GidgenMatrixRec *)elem)->name, "rec") == 0;
    case GM_REFFED: return elem && gidgen_matrix_reffed_get_value ((GidgenMatrixReffed *)elem) == v;
    default: return GPOINTER_TO_INT (elem) == v;
  }
}

static GDestroyNotify
gm_elem_free (GmKind kind)
{
  switch (kind)
  {
    case GM_STR: return g_free;
    case GM_OBJ: return g_object_unref;
    case GM_REC: return (GDestroyNotify)gidgen_matrix_rec_free;
    case GM_REFFED: return (GDestroyNotify)gidgen_matrix_reffed_unref;
    default: return NULL;
  }
}

#define GM_LIST_FUNCS(prefix, Type, type_prefix) \
  Type * \
  gm_##prefix##_new (GmKind kind, const GmVals *gv) \
  { \
    gsize n = gv->n; const gintptr *vals = gv->v; \
    Type *list = NULL; \
    for (gsize i = n; i > 0; i--) \
      list = type_prefix##_prepend (list, gm_elem_new (kind, vals[i - 1])); \
    return list; \
  } \
  gboolean \
  gm_##prefix##_eq (GmKind kind, const Type *list, const GmVals *gv) \
  { \
    gsize n = gv->n; const gintptr *vals = gv->v; \
    gsize i = 0; \
    for (; list; list = list->next, i++) \
      if (i >= n || !gm_elem_eq (kind, list->data, vals[i])) \
        return FALSE; \
    return i == n; \
  } \
  void gm_##prefix##_free_str (Type *list) { type_prefix##_free_full (list, g_free); } \
  void gm_##prefix##_free_obj (Type *list) { type_prefix##_free_full (list, g_object_unref); } \
  void gm_##prefix##_free_rec (Type *list) { type_prefix##_free_full (list, gm_elem_free (GM_REC)); } \
  void gm_##prefix##_free_reffed (Type *list) { type_prefix##_free_full (list, gm_elem_free (GM_REFFED)); }

GM_LIST_FUNCS (list, GList, g_list)
GM_LIST_FUNCS (slist, GSList, g_slist)

GPtrArray *
gm_ptrarray_new (GmKind kind, const GmVals *gv)
{
  gsize n = gv->n; const gintptr *vals = gv->v;
  GPtrArray *array = g_ptr_array_new_with_free_func (gm_elem_free (kind));
  for (gsize i = 0; i < n; i++)
    g_ptr_array_add (array, gm_elem_new (kind, vals[i]));
  return array;
}

gboolean
gm_ptrarray_eq (GmKind kind, const GPtrArray *array, const GmVals *gv)
{
  gsize n = gv->n; const gintptr *vals = gv->v;
  if (!array || array->len != n)
    return FALSE;
  for (gsize i = 0; i < n; i++)
    if (!gm_elem_eq (kind, array->pdata[i], vals[i]))
      return FALSE;
  return TRUE;
}

/* An owned array created by D may not have an element free function, set it so that the elements are freed */
void gm_ptrarray_free_str (GPtrArray *array) { g_ptr_array_set_free_func (array, g_free); g_ptr_array_unref (array); }
void gm_ptrarray_free_obj (GPtrArray *array) { g_ptr_array_set_free_func (array, g_object_unref); g_ptr_array_unref (array); }
void gm_ptrarray_free_rec (GPtrArray *array) { g_ptr_array_set_free_func (array, gm_elem_free (GM_REC)); g_ptr_array_unref (array); }
void gm_ptrarray_free_reffed (GPtrArray *array) { g_ptr_array_set_free_func (array, gm_elem_free (GM_REFFED)); g_ptr_array_unref (array); }

GArray *
gm_garray_new (GmKind kind, const GmVals *gv)
{
  gsize n = gv->n; const gintptr *vals = gv->v;
  GArray *array = g_array_sized_new (FALSE, FALSE, sizeof (gint), n);
  for (gsize i = 0; i < n; i++)
  {
    gint v = (gint)vals[i];
    g_array_append_val (array, v);
  }
  return array;
}

gboolean
gm_garray_eq (GmKind kind, const GArray *array, const GmVals *gv)
{
  gsize n = gv->n; const gintptr *vals = gv->v;
  if (!array || array->len != n || g_array_get_element_size ((GArray *)array) != sizeof (gint))
    return FALSE;
  for (gsize i = 0; i < n; i++)
    if (g_array_index (array, gint, i) != vals[i])
      return FALSE;
  return TRUE;
}

void gm_garray_free_int (GArray *array) { g_array_unref (array); }

gpointer
gm_zt_new (GmKind kind, const GmVals *gv)
{
  gsize n = gv->n; const gintptr *vals = gv->v;
  gpointer *array = g_new0 (gpointer, n + 1);
  for (gsize i = 0; i < n; i++)
    array[i] = gm_elem_new (kind, vals[i]);
  return array;
}

gboolean
gm_zt_eq (GmKind kind, gconstpointer array, const GmVals *gv)
{
  gsize n = gv->n; const gintptr *vals = gv->v;
  const gpointer *a = array;
  gsize i = 0;
  if (!a)
    return FALSE;
  for (; a[i]; i++)
    if (i >= n || !gm_elem_eq (kind, a[i], vals[i]))
      return FALSE;
  return i == n;
}

void gm_zt_free_str (gpointer array) { g_strfreev (array); }

void
gm_zt_free_obj (gpointer array)
{
  for (gpointer *a = array; *a; a++)
    g_object_unref (*a);
  g_free (array);
}

gpointer
gm_carray_new (GmKind kind, const GmVals *gv)
{
  gsize n = gv->n; const gintptr *vals = gv->v;
  if (kind == GM_INT)
  {
    gint *array = g_new0 (gint, n + 1);
    for (gsize i = 0; i < n; i++)
      array[i] = (gint)vals[i];
    return array;
  }
  return gm_zt_new (kind, gv);
}

/* Fill a caller allocated integer array of length n (which must be the number of values) */
void
gm_carray_fill (GmKind kind, gpointer array, gsize n, const GmVals *gv)
{
  g_assert (kind == GM_INT && array && n == gv->n);
  for (gsize i = 0; i < n; i++)
    ((gint *)array)[i] = (gint)gv->v[i];
}

gboolean
gm_carray_eq (GmKind kind, gconstpointer array, gsize n, const GmVals *gv)
{
  if (!array || n != gv->n)
    return FALSE;
  for (gsize i = 0; i < n; i++)
    if (kind == GM_INT ? ((const gint *)array)[i] != (gint)gv->v[i]
        : !gm_elem_eq (kind, ((const gpointer *)array)[i], gv->v[i]))
      return FALSE;
  return TRUE;
}

void gm_carray_free_int (gpointer array) { g_free (array); }
void gm_carray_free_str (gpointer array) { g_strfreev (array); }
void gm_carray_free_obj (gpointer array) { gm_zt_free_obj (array); }
void gm_carray_freen_int (gpointer array, gsize n) { g_free (array); }

void
gm_carray_freen_str (gpointer array, gsize n)
{
  for (gsize i = 0; array && i < n; i++)
    g_free (((gpointer *)array)[i]);
  g_free (array);
}

void
gm_carray_freen_obj (gpointer array, gsize n)
{
  for (gsize i = 0; array && i < n; i++)
    g_object_unref (((gpointer *)array)[i]);
  g_free (array);
}

GHashTable *
gm_hash_new (GmKind kind, const GmVals *gv)
{
  gsize n = gv->n; const gintptr *vals = gv->v;
  GHashTable *hash = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, gm_elem_free (kind));
  for (gsize i = 0; i + 1 < n; i += 2)
    g_hash_table_insert (hash, g_strdup ((const gchar *)vals[i]), gm_elem_new (kind, vals[i + 1]));
  return hash;
}

gboolean
gm_hash_eq (GmKind kind, GHashTable *hash, const GmVals *gv)
{
  gsize n = gv->n; const gintptr *vals = gv->v;
  if (!hash || g_hash_table_size (hash) != n / 2)
    return FALSE;
  for (gsize i = 0; i + 1 < n; i += 2)
  {
    gpointer v;
    if (!g_hash_table_lookup_extended (hash, (gconstpointer)vals[i], NULL, &v) || !gm_elem_eq (kind, v, vals[i + 1]))
      return FALSE;
  }
  return TRUE;
}

/* An owned hash table created by D may not have key/value free functions, so the entries are freed explicitly
 * (stealing them first, in case the table does have free functions) */
static void
gm_hash_free (GHashTable *hash, GDestroyNotify value_free)
{
  GHashTableIter iter;
  gpointer k, v;
  g_hash_table_iter_init (&iter, hash);
  while (g_hash_table_iter_next (&iter, &k, &v))
  {
    g_hash_table_iter_steal (&iter);
    g_free (k);

    if (value_free)
      value_free (v);
  }
  g_hash_table_unref (hash);
}

void gm_hash_free_str (GHashTable *hash) { gm_hash_free (hash, g_free); }
void gm_hash_free_obj (GHashTable *hash) { gm_hash_free (hash, g_object_unref); }
void gm_hash_free_int (GHashTable *hash) { gm_hash_free (hash, NULL); }

/* ---- Plain structure ---- */

GidgenMatrixPlain *
gm_plain_new (gint value)
{
  GidgenMatrixPlain *plain = g_new (GidgenMatrixPlain, 1);
  plain->value = value;
  return plain;
}

GidgenMatrixPlainWrap *
gm_plainwrap_new (gint value)
{
  GidgenMatrixPlainWrap *pw = g_new (GidgenMatrixPlainWrap, 1);
  pw->value = value;
  pw->name = g_strdup ("pw");
  return pw;
}

GidgenMatrixPlainWrap *
gm_plainwrap_copy (const GidgenMatrixPlainWrap *pw)
{
  GidgenMatrixPlainWrap *copy = gm_plainwrap_new (pw->value);
  g_free (copy->name);
  copy->name = g_strdup (pw->name);
  return copy;
}

void
gm_plainwrap_free (GidgenMatrixPlainWrap *pw)
{
  g_free (pw->name);
  g_free (pw);
}

/* ---- Opaque record ---- */

/**
 * GidgenMatrixOpaque: (copy-func gidgen_matrix_opaque_copy) (free-func gidgen_matrix_opaque_free)
 *
 * Opaque record test type with copy and free functions and no GType.
 */
struct _GidgenMatrixOpaque
{
  gint value;
};

/**
 * gidgen_matrix_opaque_new:
 * @value: The value
 *
 * Returns: (transfer full): A new instance
 */
GidgenMatrixOpaque *
gidgen_matrix_opaque_new (gint value)
{
  GidgenMatrixOpaque *self = g_new (GidgenMatrixOpaque, 1);
  self->value = value;
  g_atomic_int_inc (&live_counts[GIDGEN_MATRIX_COUNTER_OPAQUE]);
  return self;
}

/**
 * gidgen_matrix_opaque_copy:
 * @self: The instance to copy
 *
 * Returns: (transfer full): A copy of @self
 */
GidgenMatrixOpaque *
gidgen_matrix_opaque_copy (GidgenMatrixOpaque *self)
{
  return gidgen_matrix_opaque_new (self->value);
}

/**
 * gidgen_matrix_opaque_free:
 * @self: The instance to free
 */
void
gidgen_matrix_opaque_free (GidgenMatrixOpaque *self)
{
  g_atomic_int_add (&live_counts[GIDGEN_MATRIX_COUNTER_OPAQUE], -1);
  g_free (self);
}

/**
 * gidgen_matrix_opaque_get_value:
 * @self: The instance
 *
 * Returns: The value
 */
gint
gidgen_matrix_opaque_get_value (GidgenMatrixOpaque *self)
{
  return self->value;
}

/* ---- GBytes and GValue helpers ---- */

GBytes *
gm_bytes_new (const gchar *str)
{
  return g_bytes_new (str, strlen (str));
}

gboolean
gm_bytes_eq (GBytes *bytes, const gchar *str)
{
  gsize size;
  gconstpointer data = g_bytes_get_data (bytes, &size);
  return size == strlen (str) && memcmp (data, str, size) == 0;
}

GValue *
gm_value_new (gint value)
{
  GValue *v = g_new0 (GValue, 1);
  gm_value_fill (v, value);
  return v;
}

GValue *
gm_value_copy (const GValue *value)
{
  GValue *v = g_new0 (GValue, 1);
  g_value_init (v, G_VALUE_TYPE (value));
  g_value_copy (value, v);
  return v;
}

void
gm_value_free (GValue *value)
{
  g_value_unset (value);
  g_free (value);
}

/* Set a zero initialized or initialized GValue to an int value */
void
gm_value_fill (GValue *value, gint v)
{
  if (G_IS_VALUE (value))
    g_value_unset (value);

  g_value_init (value, G_TYPE_INT);
  g_value_set_int (value, v);
}
