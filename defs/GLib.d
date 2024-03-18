//!repo GLib-2.0

//# Not introspectable, but could be implemented manually
//!set function[base64_encode_close][introspectable] 0
//!set function[base64_encode_step][introspectable] 0
//!set function[prefix_error_literal][introspectable] 0
//!set function[strfreev][introspectable] 0
//!set function[test_add_data_func_full][introspectable] 0
//!set function[unichar_to_utf8][introspectable] 0
//!set record[Variant].constructor[new_from_data][introspectable] 0
//!set record[Variant].function[parse][introspectable] 0

//# Disable ByteArray functions and methods, since this type is handled by a custom range type
//!set record[ByteArray].*function[][disable] 1

//# Disable problematic functions (memory allocation issues)
//!set record[Bytes].constructor[new_take][disable] 1
//!set function[base64_decode_inplace][disable] 1

//# Disable functions which don't have proper closures (solutions may be possible)
//!set function[atexit][disable] 1
//!set function[test_add_func][disable] 1
//!set function[test_queue_destroy][disable] 1
//!set record[Tree].constructor[new_full][disable] 1

//# Error conflicts with the base D Error type, rename to ErrorG
//!subtype Error ErrorG

//# Override PollFD type kind to be a simple struct, not Boxed
//!kind PollFD Simple

//# We use native associative arrays, disable HashTable bindings
//!set record[HashTable][disable] 1
//!set record[HashTableIter][disable] 1

//# Fix Dir by specifying g_dir_close as the free function and g_dir_open as the constructor
//!set record[Dir][free-function] g_dir_close
//!set record[Dir].method[close][disable] 1
//!set record[Dir].function[open][introspectable] 1
//!rename record[Dir].function[open] constructor
//!set record[Dir].constructor[open].return-value[][transfer-ownership] full

//# Disable binding of unuseful and problematic structures
//!set record[TrashStack][disable] 1

//# Disable Queue fields
//!set record[Queue].field[head][disable] 1
//!set record[Queue].field[tail][disable] 1

//# Disable TestLogMsg fields
//!set record[TestLogMsg].*field[][disable] 1

//# Disable datalist functions
//!set *function[datalist_*][disable] 1

//# Remove problematic type
//!del union[DoubleIEEE754]

//# Remove problematic type
//!del union[FloatIEEE754]

//# Change IConv from a record to a void* alias
//!del record[IConv]
//!add repository.namespace '<alias name="IConv" c:type="GIConv"><type name="void*" c:type="void*"/></alias>'

//# IOChannel should be opaque
//!set record[IOChannel][opaque] 1

//# Set field callback parameters to out
//!set record[IOFuncs].field[io_read].callback[io_read].parameters.parameter[bytes_read][direction] out
//!set record[IOFuncs].field[io_write].callback[io_write].parameters.parameter[bytes_written][direction] out
//!set record[SourceFuncs].field[prepare].callback[prepare].parameters.parameter[timeout_][direction] out

//# FIXME - This contains a union which isn't currently supported
//!set record[VariantDict][opaque] 1

//# These arrays should have zero-terminated set
//!set function[build_filenamev].parameters.parameter[args].array[][zero-terminated] 1
//!set function[build_pathv].parameters.parameter[args].array[][zero-terminated] 1
//!set function[environ_getenv].parameters.parameter[envp].array[][zero-terminated] 1
//!set function[environ_setenv].parameters.parameter[envp].array[][zero-terminated] 1
//!set function[environ_setenv].return-value.array[][zero-terminated] 1
//!set function[environ_unsetenv].parameters.parameter[envp].array[][zero-terminated] 1
//!set function[environ_unsetenv].return-value.array[][zero-terminated] 1
//!set function[get_environ].return-value.array[][zero-terminated] 1
//!set function[get_filename_charsets].parameters.parameter[filename_charsets].array[][zero-terminated] 1
//!set function[get_language_names].return-value.array[][zero-terminated] 1
//!set function[get_language_names_with_category].return-value.array[][zero-terminated] 1
//!set function[get_locale_variants].return-value.array[][zero-terminated] 1
//!set function[get_system_config_dirs].return-value.array[][zero-terminated] 1
//!set function[get_system_data_dirs].return-value.array[][zero-terminated] 1
//!set function[listenv].return-value.array[][zero-terminated] 1
//!set function[spawn_async].parameters.parameter[argv].array[][zero-terminated] 1
//!set function[spawn_async].parameters.parameter[envp].array[][zero-terminated] 1
//!set function[spawn_async_with_fds].parameters.parameter[argv].array[][zero-terminated] 1
//!set function[spawn_async_with_fds].parameters.parameter[envp].array[][zero-terminated] 1
//!set function[spawn_async_with_pipes_and_fds].parameters.parameter[argv].array[][zero-terminated] 1
//!set function[spawn_async_with_pipes_and_fds].parameters.parameter[envp].array[][zero-terminated] 1
//!set function[spawn_async_with_pipes].parameters.parameter[argv].array[][zero-terminated] 1
//!set function[spawn_async_with_pipes].parameters.parameter[envp].array[][zero-terminated] 1
//!set function[spawn_sync].parameters.parameter[argv].array[][zero-terminated] 1
//!set function[spawn_sync].parameters.parameter[envp].array[][zero-terminated] 1
//!set function[strsplit].return-value.array[][zero-terminated] 1
//!set function[strsplit_set].return-value.array[][zero-terminated] 1
//!set function[str_tokenize_and_fold].return-value.array[][zero-terminated] 1
//!set function[str_tokenize_and_fold].parameters.parameter[ascii_alternates].array[][zero-terminated] 1
//!set record[KeyFile].method[get_groups].return-value.array[][zero-terminated] 1
//!set record[KeyFile].method[get_keys].return-value.array[][zero-terminated] 1
//!set record[KeyFile].method[load_from_dirs].parameters.parameter[search_dirs].array[][zero-terminated] 1
//!set record[MatchInfo].method[fetch_all].return-value.array[][zero-terminated] 1
//!set record[OptionContext].method[add_main_entries].parameters.parameter[entries].array[][zero-terminated] 1
//!set record[OptionContext].method[parse_strv].parameters.parameter[arguments].array[][zero-terminated] 1
//!set record[OptionGroup].method[add_entries].parameters.parameter[entries].array[][zero-terminated] 1
//!set record[StrvBuilder].method[addv].parameters.parameter[value].array[][zero-terminated] 1
//!set record[StrvBuilder].method[end].return-value.array[][zero-terminated] 1
//!set record[Uri].function[list_extract_uris].return-value.array[][zero-terminated] 1
//!set record[Regex].method[split].return-value.array[][zero-terminated] 1
//!set record[Regex].method[split_full].return-value.array[][zero-terminated] 1
//!set record[Regex].function[split_simple].return-value.array[][zero-terminated] 1

//# Change arrays of chars to strings
//!set function[spawn_command_line_sync].parameters.parameter[standard_error].array '<type name="utf8" c:type="char**"/>'
//!set function[spawn_command_line_sync].parameters.parameter[standard_output].array '<type name="utf8" c:type="char**"/>'
//!set function[spawn_sync].parameters.parameter[standard_error].array '<type name="utf8" c:type="char**"/>'
//!set function[spawn_sync].parameters.parameter[standard_output].array '<type name="utf8" c:type="char**"/>'
//!set record[Variant].method[get_bytestring].return-value.array '<type name="string" c:type="const char*"/>'
//!set record[Variant].constructor[new_bytestring].parameters.parameter[string].array '<type name="string" c:type="const char*"/>'

//# Missing array argument length
//!set record[IOChannel].method[write_chars].parameters.parameter[buf].array[][length] 1

//# Add missing free functions
//!set record[AsyncQueue][free-function] g_async_queue_unref
//!set record[Hmac][free-function] g_hmac_unref
//!set record[List][free-function] g_list_free
//!set record[OptionContext][free-function] g_option_context_free
//!set record[Queue][free-function] g_queue_free
//!set record[Rand][free-function] g_rand_free
//!set record[Scanner][free-function] g_scanner_destroy
//!set record[Sequence][free-function] g_sequence_free
//!set record[SList][free-function] g_slist_free
//!set record[StringChunk][free-function] g_string_chunk_free
//!set record[StrvBuilder][free-function] g_strv_builder_unref
//!set record[TestCase][free-function] g_test_case_free
//!set record[TestSuite][free-function] g_test_suite_free
//!set record[VariantIter][free-function] g_variant_iter_free

//# Change Variant to a class, set to glib:fundamental, and add ref and unref functions
//!rename record[Variant] class
//!set class[Variant][glib:fundamental] 1
//!set class[Variant][glib:ref-func] g_variant_ref
//!set class[Variant][glib:unref-func] g_variant_unref

//# Disable deprecated g_variant_parser_get_error_quark since it confuses the exception generator
//!set class[Variant].function[parser_get_error_quark][disable] 1

//# Add ref/unref functions to VariantBuilder
//!set record[VariantBuilder][glib:ref-func] g_variant_builder_ref
//!set record[VariantBuilder][glib:unref-func] g_variant_builder_unref
//!set record[VariantBuilder].union[u][private] 1

//# Add missing parameter direction "out"
//!set record[IOChannel].method[read].parameters.parameter[bytes_read][direction] out
//!set record[IOChannel].method[read_line_string].parameters.parameter[terminator_pos][direction] out
//!set record[IOChannel].method[write].parameters.parameter[bytes_written][direction] out

//# Fix string array parameters with lengths to be array of chars not array of strings
//!set record[Regex].method[match_all_full].parameters.parameter[string].array.type[][name] char
//!set record[Regex].method[match_full].parameters.parameter[string].array.type[][name] char
//!set record[Regex].method[replace_literal].parameters.parameter[string].array.type[][name] char
//!set record[Regex].method[replace].parameters.parameter[string].array.type[][name] char
//!set record[Regex].method[split_full].parameters.parameter[string].array.type[][name] char

//# Set proper direction of pointer parameters
//!set function[atomic_int_add].parameters.parameter[atomic][direction] inout
//!set function[atomic_int_and].parameters.parameter[atomic][direction] inout
//!set function[atomic_int_compare_and_exchange_full].parameters.parameter[atomic][direction] inout
//!set function[atomic_int_compare_and_exchange].parameters.parameter[atomic][direction] inout
//!set function[atomic_int_dec_and_test].parameters.parameter[atomic][direction] inout
//!set function[atomic_int_exchange_and_add].parameters.parameter[atomic][direction] inout
//!set function[atomic_int_exchange].parameters.parameter[atomic][direction] inout
//!set function[atomic_int_get].parameters.parameter[atomic][direction] out
//!set function[atomic_int_inc].parameters.parameter[atomic][direction] inout
//!set function[atomic_int_or].parameters.parameter[atomic][direction] inout
//!set function[atomic_int_set].parameters.parameter[atomic][direction] inout
//!set function[atomic_int_xor].parameters.parameter[atomic][direction] inout
//!set function[atomic_ref_count_compare].parameters.parameter[arc][direction] inout
//!set function[atomic_ref_count_dec].parameters.parameter[arc][direction] inout
//!set function[atomic_ref_count_inc].parameters.parameter[arc][direction] inout
//!set function[atomic_ref_count_init].parameters.parameter[arc][direction] inout
//!set function[bit_lock].parameters.parameter[address][direction] inout
//!set function[bit_trylock].parameters.parameter[address][direction] inout
//!set function[bit_unlock].parameters.parameter[address][direction] inout
//!set function[nullify_pointer].parameters.parameter[nullify_location][direction] out
//!set function[ref_count_compare].parameters.parameter[rc][direction] inout
//!set function[ref_count_dec].parameters.parameter[rc][direction] inout
//!set function[ref_count_inc].parameters.parameter[rc][direction] inout
//!set function[ref_count_init].parameters.parameter[rc][direction] inout
//!set record[Timer].method[elapsed].parameters.parameter[microseconds][direction] out

//# Fix or add missing array information
//!set function[assertion_message_cmpstrv].parameters.parameter[arg1].type '<array zero-terminated="1" c:type="const char* const*"><type name="utf8" c:type="const char*"/></array>'
//!set function[assertion_message_cmpstrv].parameters.parameter[arg2].type '<array zero-terminated="1" c:type="const char* const*"><type name="utf8" c:type="const char*"/></array>'
//!set function[slice_get_config_state].return-value.type '<array length="2" zero-terminated="0" c:type="const gint64*"><type name="gint64" c:type="gint64"/></array>'
//!set function[slice_get_config_state].parameters.parameter[n_values][direction] out
//!set function[strjoinv].parameters.parameter[str_array].type '<array zero-terminated="1" c:type="gchar**"><type name="utf8" c:type="gchar*"/></array>'
//!set function[strv_contains].parameters.parameter[strv].type '<array zero-terminated="1" c:type="const gchar* const*"><type name="utf8" c:type="const gchar*"/></array>'
//!set function[strv_equal].parameters.parameter[strv1].type '<array zero-terminated="1" c:type="const gchar* const*"><type name="utf8" c:type="const gchar*"/></array>'
//!set function[strv_equal].parameters.parameter[strv2].type '<array zero-terminated="1" c:type="const gchar* const*"><type name="utf8" c:type="const gchar*"/></array>'
//!set function[strv_length].parameters.parameter[str_array].type '<array zero-terminated="1" c:type="const gchar* const*"><type name="utf8" c:type="const gchar*"/></array>'
//!set function[ucs4_to_utf16].return-value.type '<array zero-terminated="1" c:type="gunichar2*"><type name="guint16" c:type="gunichar2"/></array>'
//!set function[unicode_canonical_decomposition].return-value.type '<array length="1" zero-terminated="0" c:type="gunichar*"><type name="gunichar" c:type="gunichar"/></array>'
//!set function[unicode_canonical_decomposition].parameters.parameter[result_len][direction] out
//!set function[utf16_to_ucs4].return-value.type '<array length="3" zero-terminated="1" c:type="gunichar*"><type name="gunichar" c:type="gunichar"/></array>'
//!set function[utf8_to_ucs4].return-value.type '<array length="3" zero-terminated="1" c:type="gunichar*"><type name="gunichar" c:type="gunichar"/></array>'
//!set function[utf8_to_ucs4_fast].return-value.type '<array length="2" zero-terminated="1" c:type="gunichar*"><type name="gunichar" c:type="gunichar"/></array>'
//!set function[utf8_to_ucs4_fast].parameters.parameter[str].type '<array length="1" zero-terminated="1" c:type="const gchar*"><type name="gchar" c:type="gchar"/></array>'
//!set function[utf8_to_utf16].return-value.type '<array length="3" zero-terminated="1" c:type="gunichar2*"><type name="guint16" c:type="gunichar2"/></array>'
//!set record[IOChannel].method[read].parameters.parameter[buf].type '<array length="1" zero-terminated="0" c:type="guint8*"><type name="guint8" c:type="guint8"/></array>'
//!set record[IOChannel].method[read].parameters.parameter[buf][caller-allocates] 1
//!set record[IOChannel].method[read].parameters.parameter[buf][direction] out
//!set record[Rand].method[set_seed_array].parameters.parameter[seed].type '<array length="1" zero-terminated="0" c:type="const guint32*"><type name="guint32" c:type="guint32"/></array>'
//!set record[TestLogBuffer].method[push].parameters.parameter[bytes].type '<array length="0" zero-terminated="0" c:type="const guint8*"><type name="guint8" c:type="guint8"/></array>'
//!set record[StrvBuilder].method[end].return-value.array '<array zero-terminated="1" c:type="char**"><type name="utf8" c:type="char*"/></array>'

//# Set writable to false as it should be
//!set record[Node].field[children][writable] 0
//!set record[Node].field[next][writable] 0
//!set record[Node].field[parent][writable] 0
//!set record[Node].field[prev][writable] 0
//!set record[Scanner].field[config][writable] 0
//!set record[Scanner].field[next_value][writable] 0
//!set record[Scanner].field[value][writable] 0

//# Add missing closure parameter designations
//!set callback[IOFunc].parameters.parameter[data][closure] 2
//!set callback[SpawnChildSetupFunc].parameters.parameter[data][closure] 0
//!set callback[ThreadFunc].parameters.parameter[data][closure] 0
//!set callback[TranslateFunc].parameters.parameter[data][closure] 1

//# Set proper callback scopes
//!set record[Queue].method[clear_full].parameters.parameter[free_func][scope] call
//!set record[Queue].method[free_full].parameters.parameter[free_func][scope] call
//!set function[atomic_rc_box_release_full].parameters.parameter[clear_func][scope] call
//!set function[rc_box_release_full].parameters.parameter[clear_func][scope] call

//!class global

  /**
   * Template to convert a GHashTable to a D associative array.
   * Params:
   *   K = The key D type
   *   V = The value D type
   *   owned = Set to true if caller takes ownership of hash (frees it), false to leave it alone (default)
   * Returns: The D associative array which is a copy of the data in hash
   */
  V[K] hashTableToMap(K, V, bool owned = false)(GHashTable* hash)
  {
    GHashTableIter iter;
    void* key;
    void* value;
    V[K] map;

    for (g_hash_table_iter_init(&iter, hash); g_hash_table_iter_next(&iter, &key, &value); )
      map[containerGetItem!K(key)] = containerGetItem!V(value);

    static if (owned)
      g_hash_table_unref(hash);

    return map;
  }

  /**
   * Convert a D map to a GHashTable.
   * Params:
   *   K = The key type
   *   V = The value type
   *   map = The D map
   * Returns: A newly allocated GHashTable.
   */
  GHashTable* mapToHashTable(K, V)(V[K] map)
  {
    GDestroyNotify valDestroyFunc;

    static if (is(V : ObjectG))
      valDestroyFunc = g_object_unref;
    else static if (is(V : string))
      valDestroyFunc = g_free;

    static if (is(K : string))
      g_hash_table_new_full(g_str_hash, g_str_equal, g_free, valDestroyFunc);
    else
      g_hash_table_new_full(g_direct_hash, g_direct_equal, null, valDestroyFunc);
  }

  import GObject.ObjectG;
  import GLib.Boxed;

  /**
  * Template to get a D item from a container C data item. Used internally for binding containers.
  * Params:
  *   T = The D item type
  *   data = The container C data pointer
  * Returns: The D item which is a copy of the C item
  */
  T containerGetItem(T)(void* data)
  {
    static if (is(T : ObjectG) || is(T == interface) || is(T : Boxed))
      return ObjectG.getDObject!T(data, false);
    else static if (is(T : string))
      return fromCString(cast(const(char)*)data, false);
    else static if (__traits(compiles, new T(data, false)))
      return new T(data, false);
    else static if (is(T : void*))
      return data;
    else
      assert(0);
  }

  /**
   * Template to copy a container C item to another C item. Used internally for binding containers.
   * Params:
   *   T = The D item type
   *   data = The container C data pointer
   * Returns: A copy of the C item
   */
  void* containerCopyItem(T)(void* data)
  {
    if (!data)
      return null;

    static if (is(T : ObjectG) || is(T == interface))
      return ObjectG.ref_(data);
    else static if (is(T : Boxed))
      return Boxed.boxCopy!T(data);
    else static if (is(T : string))
      return g_strdup(cast(const(char)*)data);
    else static if (__traits(compiles, T.ref_(data)))
      return T.ref_(data);
    else static if (is(T : void*))
      return data;
    else
      assert(0);
  }

  /**
   * Free a container C item. Used internally for binding containers.
   * Params:
   *   T = The D item type
   *   data = The container C data pointer
   */
  void containerFreeItem(T)(void* data)
  {
    static if (is(T : ObjectG) || is(T == interface))
      ObjectG.unref(data);
    else static if (is(T : Boxed))
      Boxed.boxFree!T(data);
    else static if (is(T : string))
      g_free(data);
    else static if (__traits(compiles, T.unref(data)))
      T.unref(data);
    else static if (is(T : void*))
    {
    }
    else
      assert(0);
  }

//# g_markup_parse_context_new() has unnecessary closure and destroy notify, override the new() method
//!set record[MarkupParseContext].constructor[new][disable] 1
//!class MarkupParseContext

  this(MarkupParser parser, MarkupParseFlags flags)
  {
    GMarkupParseContext* _cretval;
    GMarkupParseFlags _flags = cast(GMarkupParseFlags)cast(uint)flags;
    _cretval = g_markup_parse_context_new(parser ? parser.cPtr!GMarkupParser : null, _flags, null, null);
    this(&_cretval, true);
  }

//# g_option_group_new() has unnecessary closure and destroy notify, override the new() method
//!set record[OptionGroup].constructor[new][disable] 1
//!class OptionGroup

  this(string name, string description, string helpDescription)
  {
    GOptionGroup* _cretval;
    const(char)* _name = name.toCString(false);
    const(char)* _description = description.toCString(false);
    const(char)* _helpDescription = helpDescription.toCString(false);
    _cretval = g_option_group_new(_name, _description, _helpDescription, null, null);
    this(&_cretval, true);
  }

//# Use a custom free function since g_thread_pool_free takes 3 arguments
//!set record[ThreadPool][free-function] freePool
//!class ThreadPool

  private static void freePool(GThreadPool* pool)
  {
    g_thread_pool_free(pool, true, false); // immediate, wait
  }
