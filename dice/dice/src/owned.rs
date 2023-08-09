/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Provides a pointer that is owned uniquely by a holder, which can handout an un-lifetimed reference.
//! The reference can only be accessed via a lambda providing an optional access based on whether
//! the reference is still valid.

use std::ops::Deref;
use std::sync::Arc;
use std::sync::Weak;

use allocative::Allocative;
use dupe::Clone_;
use dupe::Dupe_;

/// Owns 'T' uniquely.
/// Note that it is up to the user the ensure there is no race between dropping the 'Owned' value
/// and any 'Ref' accesses.
#[derive(Allocative)]
pub struct Owned<T>(Arc<T>);

impl<T> Deref for Owned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// Reference that has no compile time lifetimes and isn't guaranteed to be alive
#[derive(Allocative, Clone_, Dupe_)]
pub struct Ref<T>(Weak<T>);

impl<T> Owned<T> {
    pub fn new(t: T) -> Self {
        Owned(Arc::new(t))
    }

    pub fn as_ref(&self) -> Ref<T> {
        Ref(Arc::downgrade(&self.0))
    }

    /// Effectively drops the 'Owned' and gets a unique ownership to the underlying data.
    /// Note that it is up to the user the ensure there is no race between calling `into_inner`
    /// and any 'Ref' accesses.
    pub fn unwrap_inner(self) -> anyhow::Result<T> {
        Arc::into_inner(self.0).ok_or_else(|| anyhow::anyhow!("Some reference is using it"))
    }
}

impl<T> Ref<T> {
    pub fn maybe_access<U>(&self, f: impl FnOnce(&T) -> U) -> Option<U> {
        self.0.upgrade().map(|t| f(&*t))
    }

    /// gets the reference. Ensures that the reference is alive at time of access, but does not
    /// prevent the `Owned` from being dropped while the returned reference is alive
    pub unsafe fn deref(&self) -> Option<&T> {
        if Weak::strong_count(&self.0) > 0 {
            Some(unsafe { &*Weak::as_ptr(&self.0) })
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::owned::Owned;

    #[test]
    fn valid_references() {
        let owner = Owned::new(());

        let reference = owner.as_ref();
        assert!(reference.maybe_access(|_x| ()).is_some());

        let _inner = owner.unwrap_inner().unwrap();

        assert!(
            reference
                .maybe_access(|_x| panic!("no reference"))
                .is_none()
        );
    }
}
