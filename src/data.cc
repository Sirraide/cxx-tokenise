#include <data.hh>

std::unordered_set<std::string_view> keywords{
    "alignas",
    "alignof",
    "and",
    "and_eq",
    "asm",
    "auto",
    "bitand",
    "bitor",
    "bool",
    "break",
    "case",
    "catch",
    "char",
    "char16_t",
    "char32_t",
    "char8_t",
    "class",
    "co_await",
    "co_return",
    "co_yield",
    "compl",
    "concept",
    "const",
    "const_cast",
    "consteval",
    "constexpr",
    "constinit",
    "continue",
    "decltype",
    "default",
    "delete",
    "do",
    "double",
    "dynamic_cast",
    "else",
    "enum",
    "explicit",
    "export",
    "extern",
    "false",
    "final",
    "float",
    "for",
    "friend",
    "goto",
    "if",
    "import",
    "inline",
    "int",
    "long",
    "module",
    "mutable",
    "namespace",
    "new",
    "noexcept",
    "not",
    "not_eq",
    "nullptr",
    "operator",
    "or",
    "or_eq",
    "override",
    "private",
    "protected",
    "public",
    "register",
    "reinterpret_cast",
    "requires",
    "return",
    "short",
    "signed",
    "sizeof",
    "static",
    "static_assert",
    "static_cast",
    "struct",
    "switch",
    "template",
    "this",
    "thread_local",
    "throw",
    "true",
    "try",
    "typedef",
    "typeid",
    "typename",
    "union",
    "unsigned",
    "using",
    "virtual",
    "void",
    "volatile",
    "wchar_t",
    "while",
    "xor",
    "xor_eq",
};

std::unordered_set<std::string_view> known_namespaces{
    "chr",
    "chrono",
    "clang",
    "filesystem",
    "fmt",
    "fs",
    "llvm",
    "ranges",
    "rgs",
    "rng",
    "std",
};

std::unordered_set<std::string_view> known_std_classes{
    "add_const",
    "add_cv",
    "add_lvalue_reference",
    "add_pointer",
    "add_rvalue_reference",
    "add_volatile",
    "adopt_lock_t",
    "aligned_storage",
    "aligned_union",
    "alignment_of",
    "allocator",
    "allocator_traits",
    "annotate_base",
    "any",
    "array",
    "associative_tag",
    "atomic",
    "atomic_flag",
    "atomic_ref",
    "auto_ptr",
    "auto_ptr_ref",
    "back_insert_iterator",
    "bad_alloc",
    "bad_any_cast",
    "bad_cast",
    "bad_exception",
    "bad_function_call",
    "bad_optional_access",
    "bad_typeid",
    "bad_weak_ptr",
    "balanced_quicksort_tag",
    "balanced_tag",
    "bases",
    "basic_branch",
    "basic_branch_tag",
    "basic_filebuf",
    "basic_fstream",
    "basic_hash_table",
    "basic_hash_tag",
    "basic_ifstream",
    "basic_invalidation_guarantee",
    "basic_ios",
    "basic_iostream",
    "basic_istream",
    "basic_istringstream",
    "basic_ofstream",
    "basic_ostream",
    "basic_ostringstream",
    "basic_regex",
    "basic_streambuf",
    "basic_string",
    "basic_stringbuf",
    "basic_stringstream",
    "basic_string_view",
    "bernoulli_distribution",
    "bidirectional_iterator_tag",
    "binary_compose",
    "binary_function",
    "binary_heap",
    "binary_heap_const_iterator_",
    "binary_heap_point_const_iterator_",
    "binary_heap_tag",
    "binary_negate",
    "binder1st",
    "binder2nd",
    "binomial_distribution",
    "binomial_heap",
    "binomial_heap_base",
    "binomial_heap_tag",
    "bin_search_tree_const_it_",
    "bin_search_tree_const_node_it_",
    "bin_search_tree_it_",
    "bin_search_tree_node_it_",
    "bin_search_tree_traits",
    "bitmap_allocator",
    "bitset",
    "bool_set",
    "branch_policy",
    "cauchy_distribution",
    "cc_hash_max_collision_check_resize_trigger",
    "cc_hash_table",
    "cc_hash_tag",
    "cc_ht_map",
    "character",
    "char_traits",
    "chi_squared_distribution",
    "codecvt",
    "codecvt_base",
    "codecvt_byname",
    "collate",
    "collate_byname",
    "common_iterator",
    "common_type",
    "compare_three_way_result",
    "complex",
    "cond_dealtor",
    "conditional",
    "condition_base",
    "condition_variable",
    "condition_variable_any",
    "constant_binary_fun",
    "constant_size_blocks_tag",
    "constant_unary_fun",
    "constant_void_fun",
    "const_mem_fun1_ref_t",
    "const_mem_fun1_t",
    "const_mem_fun_ref_t",
    "const_mem_fun_t",
    "container_base_dispatch",
    "container_error",
    "container_tag",
    "container_traits",
    "container_traits_base",
    "contiguous_iterator_tag",
    "counted_iterator",
    "ctype",
    "ctype_base",
    "ctype_byname",
    "dangling",
    "debug_allocator",
    "decay",
    "decimal128",
    "decimal32",
    "decimal64",
    "deduce",
    "default_comb_hash_fn",
    "default_delete",
    "default_eq_fn",
    "default_hash_fn",
    "default_parallel_tag",
    "default_probe_fn",
    "default_resize_policy",
    "default_sentinel_t",
    "default_trie_access_traits",
    "default_update_policy",
    "defer_lock_t",
    "deque",
    "destroying_delete_t",
    "direct_bases",
    "direct_mask_range_hashing",
    "direct_mod_range_hashing",
    "directory_entry",
    "directory_iterator",
    "discard_block_engine",
    "discrete_distribution",
    "divides",
    "domain_error",
    "dumnode_const_iterator",
    "duration",
    "duration_values",
    "dynamic_bitset",
    "empty_view",
    "enable_if",
    "enable_shared_from_this",
    "enc_filebuf",
    "encoding_char_traits",
    "encoding_state",
    "entry_cmp",
    "entry_pred",
    "eq_by_less",
    "equal_split_tag",
    "equal_to",
    "error_category",
    "error_code",
    "error_condition",
    "exact_tag",
    "exception",
    "exception_ptr",
    "exponential_distribution",
    "extent",
    "extreme_value_distribution",
    "file_status",
    "filesystem_error",
    "find_tag",
    "fisher_f_distribution",
    "forced_error",
    "forward_iterator_tag",
    "forward_list",
    "fpos",
    "free_list",
    "from_chars_result",
    "front_insert_iterator",
    "function",
    "future",
    "future_error",
    "gamma_distribution",
    "geometric_distribution",
    "gp_hash_table",
    "gp_hash_tag",
    "gp_ht_map",
    "gps_clock",
    "greater",
    "greater_equal",
    "growing_blocks_tag",
    "gslice",
    "gslice_array",
    "hash",
    "hash_eq_fn",
    "hash_exponential_size_policy",
    "hash_load_check_resize_trigger",
    "hash_load_check_resize_trigger_size_base",
    "hash_map",
    "hash_multimap",
    "hash_multiset",
    "hash_prime_size_policy",
    "hash_set",
    "hash_standard_resize_policy",
    "has_unique_object_representations",
    "has_virtual_destructor",
    "hh_mm_ss",
    "identity",
    "independent_bits_engine",
    "indirect_array",
    "initializer_list",
    "in_place_t",
    "input_iterator_tag",
    "insert_error",
    "insert_iterator",
    "integer_sequence",
    "integral_constant",
    "invalid_argument",
    "invoke_result",
    "ios_base",
    "is_abstract",
    "is_aggregate",
    "is_arithmetic",
    "is_array",
    "is_assignable",
    "is_base_of",
    "is_bind_expression",
    "is_bounded_array",
    "is_class",
    "is_compound",
    "is_const",
    "is_constructible",
    "is_copy_assignable",
    "is_copy_constructible",
    "is_default_constructible",
    "is_destructible",
    "is_empty",
    "is_enum",
    "is_error_code_enum",
    "is_error_condition_enum",
    "is_final",
    "is_floating_point",
    "is_function",
    "is_fundamental",
    "is_integral",
    "is_invocable",
    "is_invocable_r",
    "is_layout_compatible",
    "is_literal_type",
    "is_lvalue_reference",
    "is_member_function_pointer",
    "is_member_object_pointer",
    "is_member_pointer",
    "is_move_assignable",
    "is_move_constructible",
    "is_nothrow_assignable",
    "is_nothrow_constructible",
    "is_nothrow_convertible",
    "is_nothrow_copy_assignable",
    "is_nothrow_copy_constructible",
    "is_nothrow_default_constructible",
    "is_nothrow_destructible",
    "is_nothrow_invocable",
    "is_nothrow_invocable_r",
    "is_nothrow_move_assignable",
    "is_nothrow_move_constructible",
    "is_nothrow_swappable",
    "is_nothrow_swappable_with",
    "is_null_pointer",
    "is_object",
    "is_placeholder",
    "is_pod",
    "is_pointer",
    "is_pointer_interconvertible_base_of",
    "is_polymorphic",
    "is_reference",
    "is_rvalue_reference",
    "is_same",
    "is_scalar",
    "is_signed",
    "is_standard_layout",
    "is_swappable",
    "is_swappable_with",
    "istreambuf_iterator",
    "istream_iterator",
    "is_trivial",
    "is_trivially_assignable",
    "is_trivially_constructible",
    "is_trivially_copyable",
    "is_trivially_copy_assignable",
    "is_trivially_copy_constructible",
    "is_trivially_default_constructible",
    "is_trivially_destructible",
    "is_trivially_move_assignable",
    "is_trivially_move_constructible",
    "is_unbounded_array",
    "is_union",
    "is_unsigned",
    "is_void",
    "is_volatile",
    "iterator",
    "iterator_traits",
    "join_error",
    "jthread",
    "left_child_next_sibling_heap",
    "left_child_next_sibling_heap_const_iterator_",
    "left_child_next_sibling_heap_node_",
    "left_child_next_sibling_heap_node_point_const_iterator_",
    "length_error",
    "less",
    "less_equal",
    "limit_condition",
    "linear_congruential_engine",
    "linear_probe_fn",
    "list",
    "list_update",
    "list_update_tag",
    "locale",
    "lock_guard",
    "logical_and",
    "logical_not",
    "logical_or",
    "logic_error",
    "lognormal_distribution",
    "lu_counter_metadata",
    "lu_counter_policy",
    "lu_counter_policy_base",
    "lu_map",
    "lu_move_to_front_policy",
    "make_signed",
    "make_unsigned",
    "malloc_allocator",
    "map",
    "mask_array",
    "mask_based_range_hashing",
    "match_results",
    "maybe_null_type",
    "mem_fun1_ref_t",
    "mem_fun1_t",
    "mem_fun_ref_t",
    "mem_fun_t",
    "memory_resource",
    "mersenne_twister_engine",
    "messages",
    "messages_base",
    "messages_byname",
    "minus",
    "mod_based_range_hashing",
    "modulus",
    "money_base",
    "money_get",
    "moneypunct",
    "moneypunct_byname",
    "money_put",
    "monotonic_buffer_resource",
    "move_iterator",
    "move_only_function",
    "multimap",
    "multiplies",
    "multiset",
    "multiway_mergesort_exact_tag",
    "multiway_mergesort_sampling_tag",
    "multiway_mergesort_tag",
    "mutex",
    "negate",
    "negative_binomial_distribution",
    "nested_exception",
    "new_allocator",
    "normal_distribution",
    "nostopstate_t",
    "not_equal_to",
    "no_throw_copies",
    "null_node_update",
    "nullopt_t",
    "null_type",
    "numeric_limits",
    "num_get",
    "numpunct",
    "numpunct_byname",
    "num_put",
    "omp_loop_static_tag",
    "omp_loop_tag",
    "once_flag",
    "optional",
    "ostreambuf_iterator",
    "ostream_iterator",
    "ostream_joiner",
    "out_of_range",
    "output_iterator_tag",
    "overflow_error",
    "ov_tree_map",
    "ov_tree_node_const_it_",
    "ov_tree_node_it_",
    "ov_tree_tag",
    "owner_less",
    "packaged_task",
    "pair",
    "pairing_heap",
    "pairing_heap_tag",
    "parallel_tag",
    "path",
    "pat_trie_base",
    "pat_trie_map",
    "pat_trie_tag",
    "piecewise_constant_distribution",
    "piecewise_construct_t",
    "piecewise_linear_distribution",
    "plus",
    "pointer_to_binary_function",
    "pointer_to_unary_function",
    "pointer_traits",
    "point_invalidation_guarantee",
    "poisson_distribution",
    "polymorphic_allocator",
    "pool_options",
    "priority_queue",
    "priority_queue_tag",
    "probe_fn_base",
    "project1st",
    "project2nd",
    "promise",
    "propagate_const",
    "quadratic_probe_fn",
    "queue",
    "quicksort_tag",
    "random_access_iterator_tag",
    "random_condition",
    "random_device",
    "ranged_hash_fn",
    "ranged_probe_fn",
    "range_error",
    "range_invalidation_guarantee",
    "rank",
    "ratio",
    "ratio_equal",
    "ratio_greater",
    "ratio_greater_equal",
    "ratio_less",
    "ratio_less_equal",
    "ratio_not_equal",
    "raw_storage_iterator",
    "rb_tree",
    "rb_tree_map",
    "rb_tree_node_",
    "rb_tree_tag",
    "rc",
    "rc_binomial_heap",
    "rc_binomial_heap_tag",
    "rebind_traits",
    "recursive_directory_iterator",
    "recursive_init_error",
    "recursive_mutex",
    "recursive_timed_mutex",
    "reference_wrapper",
    "regex_error",
    "regex_iterator",
    "regex_token_iterator",
    "regex_traits",
    "remove_all_extents",
    "remove_const",
    "remove_cv",
    "remove_extent",
    "remove_pointer",
    "remove_reference",
    "remove_volatile",
    "resize_error",
    "resize_policy",
    "result_of",
    "reverse_iterator",
    "rope",
    "runtime_error",
    "sample_probe_fn",
    "sample_ranged_hash_fn",
    "sample_ranged_probe_fn",
    "sample_range_hashing",
    "sample_resize_policy",
    "sample_resize_trigger",
    "sample_size_policy",
    "sample_tree_node_update",
    "sample_trie_access_traits",
    "sample_trie_node_update",
    "sample_update_policy",
    "sampling_tag",
    "scoped_allocator_adaptor",
    "scoped_lock",
    "seed_seq",
    "select1st",
    "select2nd",
    "select_value_type",
    "sequence_tag",
    "sequential_tag",
    "set",
    "shared_future",
    "shared_lock",
    "shared_mutex",
    "shared_ptr",
    "shared_timed_mutex",
    "shuffle_order_engine",
    "single_view",
    "slice",
    "slice_array",
    "slist",
    "source_location",
    "space_info",
    "splay_tree_map",
    "splay_tree_node_",
    "splay_tree_tag",
    "stack",
    "stdio_filebuf",
    "stdio_sync_filebuf",
    "steady_clock",
    "stop_callback",
    "stop_source",
    "stop_token",
    "stored_data",
    "stored_hash",
    "stored_value",
    "string_tag",
    "student_t_distribution",
    "sub_match",
    "subrange",
    "subtractive_rng",
    "subtract_with_carry_engine",
    "synchronized_pool_resource",
    "synth_access_traits",
    "system_clock",
    "system_error",
    "tai_clock",
    "temporary_buffer",
    "thin_heap",
    "thin_heap_tag",
    "thread",
    "throw_allocator_base",
    "throw_allocator_limit",
    "throw_allocator_random",
    "throw_value_base",
    "throw_value_limit",
    "throw_value_random",
    "time_base",
    "timed_mutex",
    "time_get",
    "time_get_byname",
    "time_point",
    "time_put",
    "time_put_byname",
    "to_chars_result",
    "treat_as_floating_point",
    "tree",
    "tree_metadata_helper",
    "tree_node_metadata_dispatch",
    "tree_order_statistics_node_update",
    "tree_tag",
    "tree_traits",
    "trie",
    "trie_metadata_helper",
    "trie_node_metadata_dispatch",
    "trie_order_statistics_node_update",
    "trie_policy_base",
    "trie_prefix_search_node_update",
    "trie_string_access_traits",
    "trie_tag",
    "trie_traits",
    "trivial_iterator_tag",
    "try_to_lock_t",
    "tuple",
    "tuple_element",
    "tuple_size",
    "type_index",
    "type_info",
    "types_traits",
    "tzdb_list",
    "unary_compose",
    "unary_function",
    "unary_negate",
    "unbalanced_tag",
    "underflow_error",
    "underlying_type",
    "uniform_int_distribution",
    "uniform_real_distribution",
    "unique_lock",
    "unique_ptr",
    "unordered_map",
    "unordered_multimap",
    "unordered_multiset",
    "unordered_set",
    "unsynchronized_pool_resource",
    "unwrap_ref_decay",
    "unwrap_reference",
    "uses_allocator",
    "utc_clock",
    "valarray",
    "vector",
    "view_base",
    "view_interface",
    "wbuffer_convert",
    "weak_ptr",
    "weibull_distribution",
    "wstring_convert",
};

std::unordered_set<std::string_view> known_std_functions{
    "accumulate",
    "acos",
    "acosh",
    "addressof",
    "adjacent_difference",
    "adjacent_find",
    "advance",
    "align",
    "all_of",
    "any_of",
    "arg",
    "asin",
    "asinh",
    "assoc_laguerref",
    "assoc_laguerrel",
    "assoc_legendref",
    "assoc_legendrel",
    "assume_aligned",
    "async",
    "atan",
    "atanh",
    "atomic_compare_exchange_strong",
    "atomic_compare_exchange_strong_explicit",
    "atomic_compare_exchange_weak",
    "atomic_compare_exchange_weak_explicit",
    "atomic_exchange",
    "atomic_exchange_explicit",
    "atomic_is_lock_free",
    "atomic_load",
    "atomic_load_explicit",
    "atomic_store",
    "atomic_store_explicit",
    "back_inserter",
    "begin",
    "betaf",
    "betal",
    "binary_search",
    "bind",
    "bind1st",
    "bind2nd",
    "bind_front",
    "bit_cast",
    "bit_ceil",
    "bit_floor",
    "bit_width",
    "boolalpha",
    "call_once",
    "cbegin",
    "cend",
    "clamp",
    "comp_ellint_1f",
    "comp_ellint_1l",
    "comp_ellint_2f",
    "comp_ellint_2l",
    "comp_ellint_3f",
    "comp_ellint_3l",
    "conj",
    "const_pointer_cast",
    "copy",
    "copy_backward",
    "copy_if",
    "cos",
    "cosh",
    "count",
    "count_if",
    "countl_one",
    "countl_zero",
    "countr_one",
    "countr_zero",
    "crbegin",
    "crend",
    "current_exception",
    "cyl_bessel_if",
    "cyl_bessel_il",
    "cyl_bessel_jf",
    "cyl_bessel_jl",
    "cyl_bessel_kf",
    "cyl_bessel_kl",
    "cyl_neumannf",
    "cyl_neumannl",
    "data",
    "dec",
    "declare_no_pointers",
    "declare_reachable",
    "declval",
    "defaultfloat",
    "dynamic_pointer_cast",
    "ellint_1f",
    "ellint_1l",
    "ellint_2f",
    "ellint_2l",
    "ellint_3f",
    "ellint_3l",
    "empty",
    "end",
    "endl",
    "ends",
    "equal",
    "equal_range",
    "exchange",
    "exclusive_scan",
    "exp",
    "expintf",
    "expintl",
    "fill",
    "fill_n",
    "find",
    "find_end",
    "find_first_of",
    "find_if",
    "find_if_not",
    "fixed",
    "flush",
    "for_each",
    "for_each_n",
    "forward",
    "forward_as_tuple",
    "from_chars",
    "front_inserter",
    "future_category",
    "generate",
    "generate_canonical",
    "generate_n",
    "generic_category",
    "get",
    "getline",
    "get_money",
    "get_new_handler",
    "get_pointer_safety",
    "get_temporary_buffer",
    "get_terminate",
    "get_time",
    "get_unexpected",
    "has_facet",
    "has_single_bit",
    "hermitef",
    "hermitel",
    "hex",
    "hexfloat",
    "includes",
    "inclusive_scan",
    "inner_product",
    "inplace_merge",
    "inserter",
    "internal",
    "invoke",
    "iota",
    "isalnum",
    "isalpha",
    "isblank",
    "iscntrl",
    "is_constant_evaluated",
    "is_corresponding_member",
    "isdigit",
    "isgraph",
    "is_heap",
    "is_heap_until",
    "islower",
    "is_partitioned",
    "is_permutation",
    "is_pointer_interconvertible_with_class",
    "isprint",
    "ispunct",
    "is_sorted",
    "is_sorted_until",
    "isspace",
    "isupper",
    "isxdigit",
    "iter_swap",
    "kill_dependency",
    "laguerref",
    "laguerrel",
    "launder",
    "left",
    "legendref",
    "legendrel",
    "lexicographical_compare",
    "lock",
    "log",
    "log10",
    "lower_bound",
    "make_any",
    "make_error_code",
    "make_error_condition",
    "make_exception_ptr",
    "make_heap",
    "make_reverse_iterator",
    "make_tuple",
    "max_element",
    "mem_fn",
    "merge",
    "min_element",
    "minmax",
    "minmax_element",
    "mismatch",
    "move",
    "move_backward",
    "move_if_noexcept",
    "next_permutation",
    "noboolalpha",
    "none_of",
    "norm",
    "noshowbase",
    "noshowpoint",
    "noshowpos",
    "noskipws",
    "not1",
    "not2",
    "nounitbuf",
    "nouppercase",
    "nth_element",
    "oct",
    "partial_sort",
    "partial_sort_copy",
    "partial_sum",
    "partition",
    "partition_copy",
    "partition_point",
    "polar",
    "popcount",
    "pop_heap",
    "prev_permutation",
    "ptr_fun",
    "push_heap",
    "put_money",
    "put_time",
    "quoted",
    "random_shuffle",
    "rbegin",
    "reduce",
    "regex_match",
    "regex_replace",
    "regex_search",
    "remove",
    "remove_copy",
    "remove_copy_if",
    "remove_if",
    "rend",
    "replace",
    "replace_copy",
    "replace_copy_if",
    "replace_if",
    "resetiosflags",
    "rethrow_exception",
    "rethrow_if_nested",
    "return_temporary_buffer",
    "reverse",
    "reverse_copy",
    "riemann_zetaf",
    "riemann_zetal",
    "right",
    "rotate",
    "rotate_copy",
    "rotl",
    "rotr",
    "scientific",
    "search_n",
    "setbase",
    "set_difference",
    "setfill",
    "set_intersection",
    "setiosflags",
    "set_new_handler",
    "setprecision",
    "set_symmetric_difference",
    "set_terminate",
    "set_unexpected",
    "set_union",
    "setw",
    "showbase",
    "showpoint",
    "showpos",
    "shuffle",
    "sin",
    "sinh",
    "size",
    "skipws",
    "sort",
    "sort_heap",
    "sph_besself",
    "sph_bessell",
    "sph_legendref",
    "sph_legendrel",
    "sph_neumannf",
    "sph_neumannl",
    "sqrt",
    "stable_partition",
    "stable_sort",
    "static_pointer_cast",
    "swap_ranges",
    "system_category",
    "tan",
    "tanh",
    "terminate",
    "throw_with_nested",
    "tie",
    "to_address",
    "tolower",
    "toupper",
    "transform",
    "transform_exclusive_scan",
    "transform_inclusive_scan",
    "transform_reduce",
    "try_lock",
    "tuple_cat",
    "uncaught_exception",
    "uncaught_exceptions",
    "undeclare_no_pointers",
    "undeclare_reachable",
    "unexpected",
    "uninitialized_copy",
    "uninitialized_default_construct",
    "uninitialized_default_construct_n",
    "uninitialized_fill",
    "uninitialized_fill_n",
    "uninitialized_move",
    "uninitialized_move_n",
    "uninitialized_value_construct",
    "uninitialized_value_construct_n",
    "unique",
    "unique_copy",
    "unitbuf",
    "upper_bound",
    "uppercase",
    "use_facet",
    "ws",
};

std::unordered_set<std::string_view> known_std_typedefs{
    "add_const_t",
    "add_cv_t",
    "add_lvalue_reference_t",
    "add_pointer_t",
    "add_rvalue_reference_t",
    "add_volatile_t",
    "aligned_storage_t",
    "atomic_bool",
    "atomic_char",
    "atomic_char16_t",
    "atomic_char32_t",
    "atomic_int",
    "atomic_int16_t",
    "atomic_int32_t",
    "atomic_int64_t",
    "atomic_int8_t",
    "atomic_int_fast16_t",
    "atomic_int_fast32_t",
    "atomic_int_fast64_t",
    "atomic_int_fast8_t",
    "atomic_int_least16_t",
    "atomic_int_least32_t",
    "atomic_int_least64_t",
    "atomic_int_least8_t",
    "atomic_intmax_t",
    "atomic_intptr_t",
    "atomic_llong",
    "atomic_long",
    "atomic_ptrdiff_t",
    "atomic_schar",
    "atomic_short",
    "atomic_size_t",
    "atomic_uchar",
    "atomic_uint",
    "atomic_uint16_t",
    "atomic_uint32_t",
    "atomic_uint64_t",
    "atomic_uint8_t",
    "atomic_uint_fast16_t",
    "atomic_uint_fast32_t",
    "atomic_uint_fast64_t",
    "atomic_uint_fast8_t",
    "atomic_uint_least16_t",
    "atomic_uint_least32_t",
    "atomic_uint_least64_t",
    "atomic_uint_least8_t",
    "atomic_uintmax_t",
    "atomic_uintptr_t",
    "atomic_ullong",
    "atomic_ulong",
    "atomic_ushort",
    "atomic_wchar_t",
    "bool_constant",
    "common_type_t",
    "compare_three_way_result_t",
    "conditional_t",
    "cregex_token_iterator",
    "csub_match",
    "decay_t",
    "enable_if_t",
    "false_type",
    "filebuf",
    "fstream",
    "ifstream",
    "index_sequence",
    "index_sequence_for",
    "invoke_result_t",
    "ios",
    "iostream",
    "istream",
    "istringstream",
    "make_index_sequence",
    "make_integer_sequence",
    "make_signed_t",
    "make_unsigned_t",
    "minstd_rand",
    "minstd_rand0",
    "mt19937",
    "mt19937_64",
    "new_handler",
    "ofstream",
    "ostream",
    "ostringstream",
    "projected",
    "ratio_add",
    "ratio_divide",
    "ratio_multiply",
    "ratio_subtract",
    "regex",
    "remove_all_extents_t",
    "remove_const_t",
    "remove_cv_t",
    "remove_cvref_t",
    "remove_extent_t",
    "remove_pointer_t",
    "remove_reference_t",
    "remove_volatile_t",
    "result_of_t",
    "sregex_token_iterator",
    "ssub_match",
    "streambuf",
    "streamoff",
    "streampos",
    "streamsize",
    "string",
    "string_view",
    "stringbuf",
    "stringstream",
    "terminate_handler",
    "true_type",
    "type_identity_t",
    "u16streampos",
    "u32streampos",
    "u16string",
    "u16string_view",
    "u32string",
    "u32string_view",
    "underlying_type_t",
    "unexpected_handler",
    "wcregex_token_iterator",
    "wcsub_match",
    "wfilebuf",
    "wfstream",
    "wifstream",
    "wios",
    "wiostream",
    "wistream",
    "wistringstream",
    "wofstream",
    "wostream",
    "wostringstream",
    "wregex",
    "wsregex_token_iterator",
    "wssub_match",
    "wstreambuf",
    "wstreampos",
    "wstring",
    "wstringbuf",
    "wstringstream",
};
