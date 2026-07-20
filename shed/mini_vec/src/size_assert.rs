/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Provides a number of size assertion macros.
//!
//! These macros are similar to ones already available in various ecosystem crates, but - aside from
//! some light API adjustments - are distinguished in that they are automatically disabled when this
//! crate's pointer packing is disabled, ensuring that it doesn't introduce compilation failures.

// The macros below expand to an *item* (`static_assertions::assert_eq_size!` produces a `const _`),
// so they can be invoked at module level as a drop-in for `assert_eq_size!`. Do not wrap the
// expansion in a block `{ ... }` - that turns it into a block expression that is only valid in
// statement position.

/// Assert that the provided type has size equal to the specified number of pointers.
#[cfg(all(not(mini_vec_no_ptr_packing), target_pointer_width = "64"))]
pub macro words_of_type($ty:ty, $w:literal) {
    $crate::size_assert::__macro_refs::static_assertions::assert_eq_size!($ty, [usize; $w]);
}

// Note that it's very intentional that the `cfg` is outside the macro, not inside it. That way it
// only needs to be set when building this crate, not whichever crate uses this one.

/// Does nothing in this configuration
#[cfg(not(all(not(mini_vec_no_ptr_packing), target_pointer_width = "64")))]
pub macro words_of_type($($t:tt)*) {}

/// Assert that the two provided types have equal size.
///
/// Prefer [`words_of_type`] when asserting against a fixed number of pointers; reach for this when
/// the point is that one type is layout-compatible with another (e.g. a newtype around `Arc<str>`).
#[cfg(all(not(mini_vec_no_ptr_packing), target_pointer_width = "64"))]
pub macro same_size($a:ty, $b:ty) {
    $crate::size_assert::__macro_refs::static_assertions::assert_eq_size!($a, $b);
}

/// Does nothing in this configuration
#[cfg(not(all(not(mini_vec_no_ptr_packing), target_pointer_width = "64")))]
pub macro same_size($($t:tt)*) {}

/// Assert that the future returned by the given async fn has size equal to the specified number of
/// pointers.
///
/// The function's arguments should be written as `_`; use like:
///
/// ```ignore
/// size_assert::words_of_async_fn_future!(my_async_fn, (_, _, _), 5);
/// ```
#[cfg(all(not(mini_vec_no_ptr_packing), target_pointer_width = "64"))]
pub macro words_of_async_fn_future($f:path, ($($arg:tt),* $(,)?), $w:literal) {
    const _: () = {
        // The body is only ever type-checked, never run: the `panic!()` placeholders construct a
        // value of the future's type without needing real arguments, and the size check fires at
        // compile time regardless of the function being called.
        #[allow(unused, clippy::diverging_sub_expression)]
        fn assert() {
            $crate::size_assert::__macro_refs::static_assertions::assert_eq_size_ptr!(
                &$f($($crate::size_assert::__macro_refs::panic_placeholder!($arg)),*),
                &[0usize; $w]
            );
        }
    };
}

/// Does nothing in this configuration
#[cfg(not(all(not(mini_vec_no_ptr_packing), target_pointer_width = "64")))]
pub macro words_of_async_fn_future($($t:tt)*) {}

#[allow(missing_docs)]
#[doc(hidden)]
pub mod __macro_refs {
    pub use static_assertions;

    /// Expands to a `panic!()` (a value of type `!`), discarding `$arg`.
    ///
    /// Used to fill in async fn arguments when only the size of the returned future matters: `!` coerces
    /// to any argument type, and a `tt` binding is needed to drive the argument repetition.
    pub macro panic_placeholder($arg:tt) {
        ::std::panic!()
    }
}

mod __compile_test {
    async fn three_args(_a: u8, _b: u16, _c: u32) -> u64 {
        0
    }

    super::words_of_type!(usize, 1);
    super::same_size!(usize, u64);
    super::words_of_async_fn_future!(three_args, (_, _, _), 1);
}
