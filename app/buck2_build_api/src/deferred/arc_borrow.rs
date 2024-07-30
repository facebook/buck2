/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::marker::PhantomData;
use std::mem;
use std::ops::Deref;
use std::sync::Arc;

use dupe::Dupe;

/// Like `triomphe::ArcBorrow`, but works with DST.
pub struct ArcBorrow<'a, T: ?Sized> {
    data: *const T,
    _phantom: PhantomData<&'a T>,
}

unsafe impl<'a, T: ?Sized> Send for ArcBorrow<'a, T> where Arc<T>: Send {}
unsafe impl<'a, T: ?Sized> Sync for ArcBorrow<'a, T> where Arc<T>: Sync {}

impl<'a, T: ?Sized> Copy for ArcBorrow<'a, T> {}

impl<'a, T: ?Sized> Clone for ArcBorrow<'a, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, T: ?Sized> Dupe for ArcBorrow<'a, T> {}

impl<'a, T: ?Sized> Deref for ArcBorrow<'a, T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &T {
        ArcBorrow::get(*self)
    }
}

impl<'a, T: ?Sized> ArcBorrow<'a, T> {
    #[inline]
    pub fn borrow(arc: &'a Arc<T>) -> Self {
        ArcBorrow {
            data: Arc::as_ptr(arc),
            _phantom: PhantomData,
        }
    }

    /// Upgrade to `Arc`.
    #[inline]
    pub(crate) fn clone_arc(borrow: Self) -> Arc<T> {
        // Documentation of `Arc::from_raw` says we can call `from_raw` on a pointer
        // previously constructed by `Arc::into_raw`.
        // This is not the case here, but we are fine.
        let arc = unsafe { Arc::from_raw(borrow.data) };
        // Bump the refcount.
        mem::forget(arc.clone());
        arc
    }

    /// Get the reference with the lifetime of the borrow.
    #[inline]
    pub(crate) fn get(borrow: Self) -> &'a T {
        unsafe { &*borrow.data }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use crate::deferred::arc_borrow::ArcBorrow;

    #[test]
    fn test_refcount() {
        let arc = Arc::new(1);
        assert_eq!(1, Arc::strong_count(&arc));
        let borrow = ArcBorrow::borrow(&arc);
        assert_eq!(1, Arc::strong_count(&arc));
        let arc_cloned = ArcBorrow::clone_arc(borrow);
        assert_eq!(2, Arc::strong_count(&arc_cloned));
        drop(arc);
        assert_eq!(1, Arc::strong_count(&arc_cloned));
    }
}
