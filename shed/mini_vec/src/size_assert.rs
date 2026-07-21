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
/// Each argument should be written as `_`, which expands to a `panic!()` placeholder of the right
/// type. When that placeholder is not enough to determine the future's type - e.g. the function is
/// generic over a closure type that must be inferred from the argument - write that argument as an
/// explicit expression instead:
///
/// ```ignore
/// size_assert::words_of_async_fn_future!(my_async_fn, (_, |x| panic!(), _), 5);
/// ```
///
/// Like the `_` placeholders, explicit arguments are only ever type-checked, never run, so they may
/// themselves be (or contain) `panic!()`.
#[cfg(all(not(mini_vec_no_ptr_packing), target_pointer_width = "64"))]
pub macro words_of_async_fn_future($f:path, ($($arg:tt)*), $w:literal) {
    const _: () = {
        // The body is only ever type-checked, never run: the placeholders construct a value of the
        // future's type without needing real arguments, and the size check fires at compile time
        // regardless of the function being called.
        #[allow(unused, clippy::diverging_sub_expression)]
        fn assert() {
            $crate::size_assert::__macro_refs::assert_async_fn_future_size!(($f) ($w) () $($arg)*);
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

    /// Emits the size assertion for [`super::words_of_async_fn_future`].
    ///
    /// This is a tt-muncher over the comma-separated argument list: it rewrites each `_` to a
    /// `panic!()` placeholder, passes any other argument through verbatim, and accumulates the
    /// results until the list is exhausted, at which point it emits the assertion. The muncher is
    /// necessary because neither fragment type covers the whole list on its own: `_` is not an
    /// `expr` (ruling out `$(:expr),*`), while explicit arguments may be multi-token expressions
    /// (ruling out `$(:tt),*`).
    pub macro assert_async_fn_future_size {
        // All arguments consumed: emit the assertion.
        (($f:path) ($w:literal) ($($arg:expr,)*)) => {
            $crate::size_assert::__macro_refs::static_assertions::assert_eq_size_ptr!(
                &$f($($arg),*),
                &[0usize; $w]
            );
        },
        // `_` placeholder (optionally followed by a comma and more arguments).
        (($f:path) ($w:literal) ($($arg:expr,)*) _ $(, $($rest:tt)*)?) => {
            $crate::size_assert::__macro_refs::assert_async_fn_future_size!(
                ($f) ($w) ($($arg,)* ::std::panic!(),) $($($rest)*)?
            );
        },
        // Explicit expression argument (optionally followed by a comma and more arguments).
        (($f:path) ($w:literal) ($($arg:expr,)*) $next:expr $(, $($rest:tt)*)?) => {
            $crate::size_assert::__macro_refs::assert_async_fn_future_size!(
                ($f) ($w) ($($arg,)* $next,) $($($rest)*)?
            );
        },
    }
}

mod __compile_test {
    async fn three_args(_a: u8, _b: u16, _c: u32) -> u64 {
        0
    }

    super::words_of_type!(usize, 1);
    super::same_size!(usize, u64);
    super::words_of_async_fn_future!(three_args, (_, _, _), 1);
    // Mix `_` placeholders with an explicit, multi-token expression argument.
    super::words_of_async_fn_future!(three_args, (_, 1u16 + 1u16, _), 1);
}
