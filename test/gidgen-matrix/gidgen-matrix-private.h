/*
 * Private helpers used by the generated matrix functions (not introspected, not passed to g-ir-scanner).
 */

#pragma once

#include "gidgen-matrix-support.h"

void gidgen_matrix_fail (const gchar *func, const gchar *what);
gpointer gidgen_matrix_keep (gpointer ptr, GDestroyNotify free_func);
gpointer gidgen_matrix_kept (void);

/* Check a condition in a generated function, recording a failure (and continuing) if it is false */
#define CHECK(cond) \
  G_STMT_START { if (!(cond)) gidgen_matrix_fail (G_STRFUNC, #cond); } G_STMT_END

/* Keep a pointer alive (owned by the library) until the next KEEP or gidgen_matrix_release_kept(), used for
 * transfer none out and return values which have to stay valid after the function returns */
#define KEEP(ptr, free_func) gidgen_matrix_keep ((ptr), (GDestroyNotify)(free_func))

/* Container helpers used by the container type table entries. Element values are passed as an array of gintptr
 * (integers, or string pointers for GM_STR), objects are created with the integer value. Hash table values are
 * key/value pairs (keys are strings). */

typedef enum { GM_INT, GM_STR, GM_OBJ, GM_REC, GM_REFFED } GmKind;

/* Plain structure helpers (transfer full plain structures are freed with g_free()) */
GidgenMatrixPlain *gm_plain_new (gint value);
GidgenMatrixPlainWrap *gm_plainwrap_new (gint value);
GidgenMatrixPlainWrap *gm_plainwrap_copy (const GidgenMatrixPlainWrap *pw);
void gm_plainwrap_free (GidgenMatrixPlainWrap *pw);

/* GBytes (string contents without the terminator), GValue (holding an int) helpers */
GBytes *gm_bytes_new (const gchar *str);
gboolean gm_bytes_eq (GBytes *bytes, const gchar *str);
GValue *gm_value_new (gint value);
GValue *gm_value_copy (const GValue *value);
void gm_value_free (GValue *value);
void gm_value_fill (GValue *value, gint v);

/* Element values (a single argument, so that it can be used in parenthesized template expressions) */
typedef struct { gsize n; const gintptr *v; } GmVals;

GList *gm_list_new (GmKind kind, const GmVals *gv);
gboolean gm_list_eq (GmKind kind, const GList *list, const GmVals *gv);
void gm_list_free_str (GList *list);
void gm_list_free_obj (GList *list);
void gm_list_free_rec (GList *list);
void gm_list_free_reffed (GList *list);
GSList *gm_slist_new (GmKind kind, const GmVals *gv);
gboolean gm_slist_eq (GmKind kind, const GSList *list, const GmVals *gv);
void gm_slist_free_str (GSList *list);
void gm_slist_free_obj (GSList *list);
void gm_slist_free_rec (GSList *list);
void gm_slist_free_reffed (GSList *list);
GPtrArray *gm_ptrarray_new (GmKind kind, const GmVals *gv);
gboolean gm_ptrarray_eq (GmKind kind, const GPtrArray *array, const GmVals *gv);
void gm_ptrarray_free_str (GPtrArray *array);
void gm_ptrarray_free_obj (GPtrArray *array);
void gm_ptrarray_free_rec (GPtrArray *array);
void gm_ptrarray_free_reffed (GPtrArray *array);
GArray *gm_garray_new (GmKind kind, const GmVals *gv);
gboolean gm_garray_eq (GmKind kind, const GArray *array, const GmVals *gv);
void gm_garray_free_int (GArray *array);
gpointer gm_zt_new (GmKind kind, const GmVals *gv);
gboolean gm_zt_eq (GmKind kind, gconstpointer array, const GmVals *gv);
void gm_zt_free_str (gpointer array);
void gm_zt_free_obj (gpointer array);
/* C arrays with a separate length (allocated with a terminating 0/NULL element so the *_free_* functions don't
 * need the length, the *_freen_* functions free arrays allocated elsewhere using the length) */
gpointer gm_carray_new (GmKind kind, const GmVals *gv);
void gm_carray_fill (GmKind kind, gpointer array, gsize n, const GmVals *gv);
gboolean gm_carray_eq (GmKind kind, gconstpointer array, gsize n, const GmVals *gv);
void gm_carray_free_int (gpointer array);
void gm_carray_free_str (gpointer array);
void gm_carray_free_obj (gpointer array);
void gm_carray_freen_int (gpointer array, gsize n);
void gm_carray_freen_str (gpointer array, gsize n);
void gm_carray_freen_obj (gpointer array, gsize n);
GHashTable *gm_hash_new (GmKind kind, const GmVals *gv);
gboolean gm_hash_eq (GmKind kind, GHashTable *hash, const GmVals *gv);
void gm_hash_free_str (GHashTable *hash);
void gm_hash_free_obj (GHashTable *hash);
void gm_hash_free_int (GHashTable *hash);
