/**
 * Templates of the aggregate test types which are assembled from the position table sections of all cells:
 * a record with a field per type (GidgenMatrixFields), a GObject with a property per type (GidgenMatrixProps), a
 * GObject with signals (GidgenMatrixSignals), callback typedefs and the GidgenMatrix gidgen definitions file.
 *
 * Placeholders are section names (see Section in position_table.d), each one is the concatenation of the section's
 * text of all cells.
 */
module aggregates;

/// All section names (each must be used in one of the templates below)
immutable string[] sectionNames = ["fieldsMember", "fieldsDoc", "fieldsCopy", "fieldsFree", "propsMember",
  "propsEnum", "propsInstall", "propsSet", "propsGet", "propsFree", "propsProtos", "propsMethods", "fieldsAddr", "sigEnum", "sigNew", "sigDocs", "cbTypedef",
  "mapProtos", "mapFuncs", "defs"];

/// C header part, before the cell function prototypes
enum headerTemplate = `/* Record with a field of each type */

#define GIDGEN_MATRIX_TYPE_FIELDS (gidgen_matrix_fields_get_type ())

typedef struct _GidgenMatrixFields GidgenMatrixFields;

/**
 * GidgenMatrixFields:
${fieldsDoc} *
 * Record with a field of each matrix type (owns its pointer fields).
 */
struct _GidgenMatrixFields
{
${fieldsMember}};

GType gidgen_matrix_fields_get_type (void) G_GNUC_CONST;
GidgenMatrixFields *gidgen_matrix_fields_new (void);
GidgenMatrixFields *gidgen_matrix_fields_copy (const GidgenMatrixFields *rec);
void gidgen_matrix_fields_free (GidgenMatrixFields *rec);
gpointer gidgen_matrix_fields_addr (GidgenMatrixFields *rec, const gchar *member);

/* GObject with a property of each type */

#define GIDGEN_MATRIX_TYPE_PROPS (gidgen_matrix_props_get_type ())
G_DECLARE_FINAL_TYPE (GidgenMatrixProps, gidgen_matrix_props, GIDGEN_MATRIX, PROPS, GObject)

GidgenMatrixProps *gidgen_matrix_props_new (void);
${propsProtos}
${mapProtos}/* GObject with signals */

#define GIDGEN_MATRIX_TYPE_SIGNALS (gidgen_matrix_signals_get_type ())
G_DECLARE_FINAL_TYPE (GidgenMatrixSignals, gidgen_matrix_signals, GIDGEN_MATRIX, SIGNALS, GObject)

GidgenMatrixSignals *gidgen_matrix_signals_new (void);

/* Callback types */

${cbTypedef}`;

/// C source part, before the cell functions
enum sourceTemplate = `/* ---- Fields record ---- */

G_DEFINE_BOXED_TYPE (GidgenMatrixFields, gidgen_matrix_fields, gidgen_matrix_fields_copy, gidgen_matrix_fields_free)

/**
 * gidgen_matrix_fields_new:
 *
 * Returns: (transfer full): A new zero initialized fields record
 */
GidgenMatrixFields *
gidgen_matrix_fields_new (void)
{
  gm_live_add (GIDGEN_MATRIX_COUNTER_FIELDS, 1);
  return g_new0 (GidgenMatrixFields, 1);
}

/**
 * gidgen_matrix_fields_copy:
 * @rec: The record to copy
 *
 * Returns: (transfer full): A copy of @rec
 */
GidgenMatrixFields *
gidgen_matrix_fields_copy (const GidgenMatrixFields *rec)
{
  GidgenMatrixFields *copy = g_new (GidgenMatrixFields, 1);
  *copy = *rec;
${fieldsCopy}  gm_live_add (GIDGEN_MATRIX_COUNTER_FIELDS, 1);
  return copy;
}

/**
 * gidgen_matrix_fields_free:
 * @rec: The record to free
 */
void
gidgen_matrix_fields_free (GidgenMatrixFields *rec)
{
${fieldsFree}  g_free (rec);
  gm_live_add (GIDGEN_MATRIX_COUNTER_FIELDS, -1);
}

/**
 * gidgen_matrix_fields_addr:
 * @rec: The fields record
 * @member: D name of a borrowed structure field
 *
 * Returns: (transfer none) (nullable): C address of the structure of a borrowed structure field
 */
gpointer
gidgen_matrix_fields_addr (GidgenMatrixFields *rec, const gchar *member)
{
${fieldsAddr}  return NULL;
}

/* ---- Properties object ---- */

struct _GidgenMatrixProps
{
  GObject parent_instance;
${propsMember}};

enum
{
  PROP_0,
${propsEnum}  N_PROPS
};

static GParamSpec *props[N_PROPS];

G_DEFINE_FINAL_TYPE (GidgenMatrixProps, gidgen_matrix_props, G_TYPE_OBJECT)

static void
gidgen_matrix_props_set_property (GObject *object, guint prop_id, const GValue *value, GParamSpec *pspec)
{
  GidgenMatrixProps *self = GIDGEN_MATRIX_PROPS (object);

  switch (prop_id)
  {
${propsSet}    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
  }
}

static void
gidgen_matrix_props_get_property (GObject *object, guint prop_id, GValue *value, GParamSpec *pspec)
{
  GidgenMatrixProps *self = GIDGEN_MATRIX_PROPS (object);

  switch (prop_id)
  {
${propsGet}    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
  }
}

static void
gidgen_matrix_props_finalize (GObject *object)
{
  GidgenMatrixProps *self = GIDGEN_MATRIX_PROPS (object);
${propsFree}  (void)self;
  G_OBJECT_CLASS (gidgen_matrix_props_parent_class)->finalize (object);
}

static void
gidgen_matrix_props_class_init (GidgenMatrixPropsClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->set_property = gidgen_matrix_props_set_property;
  object_class->get_property = gidgen_matrix_props_get_property;
  object_class->finalize = gidgen_matrix_props_finalize;
${propsInstall}  g_object_class_install_properties (object_class, N_PROPS, props);
}

static void
gidgen_matrix_props_init (GidgenMatrixProps *self)
{
}

/**
 * gidgen_matrix_props_new:
 *
 * Returns: (transfer full): A new properties object
 */
GidgenMatrixProps *
gidgen_matrix_props_new (void)
{
  return g_object_new (GIDGEN_MATRIX_TYPE_PROPS, NULL);
}

${propsMethods}
${mapFuncs}/* ---- Signals object ---- */

struct _GidgenMatrixSignals
{
  GObject parent_instance;
};

enum
{
${sigEnum}  N_SIGNALS
};

static guint matrix_signals[N_SIGNALS];

G_DEFINE_FINAL_TYPE (GidgenMatrixSignals, gidgen_matrix_signals, G_TYPE_OBJECT)

${sigDocs}static void
gidgen_matrix_signals_class_init (GidgenMatrixSignalsClass *klass)
{
${sigNew}}

static void
gidgen_matrix_signals_init (GidgenMatrixSignals *self)
{
}

/**
 * gidgen_matrix_signals_new:
 *
 * Returns: (transfer full): A new signals object
 */
GidgenMatrixSignals *
gidgen_matrix_signals_new (void)
{
  return g_object_new (GIDGEN_MATRIX_TYPE_SIGNALS, NULL);
}

/* ---- Cell functions ---- */

`;

/// gidgen definitions file for the GidgenMatrix repository
enum defsTemplate = `// Generated by matrix-gen from the type and position tables, do not edit
//!gir GidgenMatrix-1.0
//!info description "D binding for the GidgenMatrix generated type/position matrix test library"

${defs}`;
