/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Standard functions. Usually imported with `use gazebo::prelude::*`.
//!
//! Contains:
//!
//! * Extension methods for [`str`](str) and slice/[`Vec`](Vec).
//! * Exports the [`Dupe` trait](Dupe).
//! * Defines derive macros such as [`Dupe_`](Dupe_).
//!
//! The derivation macros appended with underscore are like the normal
//! derivations, but don't require the trait on any argument types.
//! For example, given the type:
//!
//! ```rust
//! # use gazebo::prelude::*;
//! # use std::sync::Arc;
//! #[derive(Clone_)]
//! struct Foo<T>(Arc<T>);
//! ```
//!
//! It is possible to use `derive(Clone)`, but that would require that
//! `T` implements [`Clone`](Clone), which is unnecessary. Using
//! [`Clone_`](Clone_) removes that constraint.
pub use gazebo_derive::Clone_;
pub use gazebo_derive::Copy_;
pub use gazebo_derive::Default_;

pub use crate::dupe::Dupe;
pub use crate::dupe::Dupe_;
pub use crate::ext::iter::IterDuped;
pub use crate::ext::iter::IterExt;
pub use crate::ext::iter::IterOwned;
pub use crate::ext::option::OptionExt;
pub use crate::ext::option::OptionRefExt;
pub use crate::ext::str::StrExt;
pub use crate::ext::vec::SliceClonedExt;
pub use crate::ext::vec::SliceCopiedExt;
pub use crate::ext::vec::SliceDupedExt;
pub use crate::ext::vec::SliceExt;
pub use crate::ext::vec::VecExt;

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use super::*;

    struct NoTraits();

    #[derive(Clone_)]
    struct Foo<A> {
        foo: Arc<A>,
    }

    #[test]
    fn test() {
        let x = Foo {
            foo: Arc::new(NoTraits()),
        };
        let x2 = x.clone();
        // Now make it clear to clippy that all those clones were important
        std::mem::drop(x2);
        std::mem::drop(x);
    }
}
