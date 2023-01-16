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
//! * Defines [`Default_`] macro.
//!
//! The derivation macros appended with underscore are like the normal
//! derivations, but don't require the trait on any argument types.
//! For example, given the type:
//!
//! ```rust
//! # use gazebo::prelude::*;
//! #[derive(Default_)]
//! struct Foo<T>(std::marker::PhantomData<T>);
//! ```
pub use gazebo_derive::Default_;

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

    use dupe::Clone_;

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
