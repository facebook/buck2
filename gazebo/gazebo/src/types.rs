/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Operations working on Rust types.

// If we need more type operations we should probably model them on
// https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Type-Equality.html

/// A trait witnessing that two types are equal.
///
/// For example:
///
/// ```
/// use gazebo::types::TEq;
/// fn foo<A : TEq<String>>(x: A) -> String {
///    x.teq()
/// }
/// ```
///
/// You should not make any further implementations of this trait.
///
/// Originally taken from a request to have Rust allow equality constraints in
/// traits, from [Issue 20041](https://github.com/rust-lang/rust/issues/20041#issuecomment-414551783).
pub trait TEq<T> {
    /// Convert between two equal types.
    fn teq(self) -> T;
    /// Convert between references to two equal types.
    fn teq_ref(&self) -> &T;
    /// Convert between mutable references to two equal types.
    fn teq_mut(&mut self) -> &mut T;
}

impl<T> TEq<T> for T {
    fn teq(self) -> Self {
        self
    }
    fn teq_ref(&self) -> &Self {
        self
    }
    fn teq_mut(&mut self) -> &mut T {
        self
    }
}
