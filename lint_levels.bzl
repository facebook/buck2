# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# buildifier: keep sorted
CLIPPY_ALLOW = [
    "clippy::arc-with-non-send-sync",  # Needs triage, see 'dashmap_directory_interner.rs:39:20' (`DashMap` is not `Send` or `Sync`)
    "clippy::await_holding_lock",  # FIXME new in Rust 1.74
    "clippy::bool-assert-comparison",  # Sometimes more clear to write it this way
    "clippy::bool-to-int-with-if",  # Using if branches to return 1 or 0 is valid, but this complains that we should use `int::from`, which is arguably less clear
    "clippy::cognitive_complexity",  # This is an arbitrary linter
    "clippy::collapsible-else-if",  # Sometimes nesting better expresses intent
    "clippy::collapsible-if",  # Sometimes nesting better expresses intent
    "clippy::comparison_chain",  # Generates worse code and harder to read
    "clippy::comparison_to_empty",  # x == "" is clearer than x.is_empty()
    "clippy::derive_partial_eq_without_eq",  # In generated protobuf code
    "clippy::disallowed_names",  # Not using foo, bar, baz in test data is silly
    "clippy::enum-variant-names",  # Sometimes you do want the same prefixes
    "clippy::implicit-hasher",  # Makes code more complex for little benefit
    "clippy::len-without-is-empty",  # len() == 0 is perfectly clear
    "clippy::manual-range-contains",  # a <= b && b <= c is way clearer than (a..=c).contains(&b)
    "clippy::many_single_char_names",  # match(a,b,c,d,e) sometimes makes sense
    "clippy::match-like-matches-macro",  # Using matches! is sometimes clearer, sometimes not
    "clippy::match-wild-err-arm",  # Seems reasonable to panic on Err(_)
    "clippy::missing-safety-doc",  # Documentation should be tailored to the reader, not the linter
    "clippy::mut_from_ref",  # Tries to check soundness, which Rust already does
    "clippy::naive-bytecount",  # Requires an extra dependency for marginal gains.
    "clippy::needless_borrows_for_generic_args",  # FIXME new in Rust 1.74
    "clippy::needless_collect",  # False positives: doesn't understand lifetimes, or e.g. DoubleEndedIterator.
    "clippy::needless_lifetimes",  # This is throwing false positives
    "clippy::needless_pass_by_ref_mut",  # Mostly identifies cases where we are accepting `&mut T` because we logically accept a mut reference but don't technically require it (i.e. we want the api to enforce the caller has a mut ref, but we don't technically need it).
    "clippy::needless_raw_string_hashes",  # False positives
    "clippy::needless_update",  # Our RE structs have slightly different definitions in internal and OSS.
    "clippy::new_without_default",  # Default is not always useful
    "clippy::non_canonical_partial_ord_impl",  # Almost exclusively identifies cases where a type delegates ord/partial ord to something else (including Derivative-derived PartialOrd) and in that case being explicit about that delegation is better than following some canonical partialord impl.
    "clippy::single_match",  # Sometimes a single match looks good
    "clippy::too_many_arguments",  # This is an arbitrary limit set on number of arguments and not always useful
    "clippy::type_complexity",  # This is an arbitrary limit set on number of type parameterizations and not always useful
    "clippy::unnecessary-wraps",  # Sometimes unnecessary wraps provide the right API
    "clippy::unwrap-or-default",  # Defaults aren't always more clear as it removes the type information when reading code
    "clippy::useless_conversion",  # Removed all obvious but there are some reports I'm unclear how to fix
    "clippy::wrong_self_convention",  # These rules are useless pedantry
]

# buildifier: keep sorted
CLIPPY_DENY = [
    "clippy::all",
    "clippy::await_holding_lock",
    "clippy::await_holding_refcell_ref",
    "clippy::dbg_macro",
    "clippy::debug_assert_with_mut_call",
    "clippy::empty_enum",
    "clippy::filter_map_next",
    "clippy::flat_map_option",
    "clippy::from_iter_instead_of_collect",
    "clippy::large_stack_arrays",
    "clippy::linkedlist",
    "clippy::macro_use_imports",
    "clippy::maybe_infinite_iter",
    "clippy::mut_mut",
    "clippy::needless_borrow",
    "clippy::needless_continue",
    "clippy::needless_range_loop",
    "clippy::nonstandard_macro_braces",
    "clippy::rc_mutex",
    "clippy::ref_option_ref",
    "clippy::rest_pat_in_fully_bound_structs",
    "clippy::same_functions_in_if_condition",
    "clippy::str_to_string",
    "clippy::string_to_string",
    "clippy::todo",
    "clippy::trivially_copy_pass_by_ref",
    "clippy::tuple_array_conversions",
    "clippy::unnecessary-literal-unwrap",  # TBD if this should be CLIPPY_ALLOW
    "clippy::useless-vec",  # TBD if this should be CLIPPY_ALLOW
    "clippy::useless_transmute",
    "clippy::verbose_file_reads",
    "let_underscore_drop",
    "unused_extern_crates",
]

# buildifier: keep sorted
CLIPPY_AUTOFIX = [
    # Only add machine-fixable warnings in this list, or we'll see them all
    # the time in CI.
    "clippy::cloned_instead_of_copied",
    "clippy::inconsistent_struct_constructor",
    "clippy::inefficient_to_string",
    "clippy::let_unit_value",
    "clippy::map_flatten",
    "clippy::map_unwrap_or",
    "clippy::needless_bitwise_bool",
    "clippy::needless_borrow",
    "clippy::range_minus_one",
    "clippy::unwrap_or_default",
    "clippy::useless-conversion",
]
