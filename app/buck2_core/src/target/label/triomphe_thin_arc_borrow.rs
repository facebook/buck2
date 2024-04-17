/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::marker::PhantomData;
use std::mem::ManuallyDrop;
use std::ptr::NonNull;

use dupe::Clone_;
use dupe::Copy_;
use dupe::Dupe_;

/// Like `triomphe::ArcBorrow`, but for `triomphe::ThinArc`.
#[derive(Copy_, Clone_, Dupe_)]
pub(crate) struct ThinArcBorrow<'a, H, T> {
    /// `ThinArc` without holding a reference counter.
    ptr: NonNull<()>,
    /// There's no `ThinArcBorrow`, use `ArcBorrow` because they are similar.
    _marker: PhantomData<triomphe::ArcBorrow<'a, (H, T)>>,
}

impl<'a, H, T> ThinArcBorrow<'a, H, T> {
    /// Borrow.
    pub(crate) fn borrow(arc: &'a triomphe::ThinArc<H, T>) -> ThinArcBorrow<'a, H, T> {
        ThinArcBorrow {
            ptr: NonNull::new(triomphe::ThinArc::as_ptr(arc) as *mut _).unwrap(),
            _marker: PhantomData,
        }
    }

    /// Obtain a temporary reference to the `ThinArc`.
    pub(crate) fn with_arc<R>(self, mut f: impl FnMut(&triomphe::ThinArc<H, T>) -> R) -> R {
        // Tricky part: we create a `ThinArc` without incrementing the reference counter
        // (which must be already >= 1 by the contract of `ThinArcBorrow`).
        // And we put it into `ManuallyDrop` to prevent reference counter decrement.
        unsafe {
            let arc = ManuallyDrop::new(triomphe::ThinArc::from_raw(self.ptr.as_ptr() as *const _));
            f(&arc)
        }
    }

    /// Upgrade to `triomphe::ThinArc`.
    pub(crate) fn to_owned(self) -> triomphe::ThinArc<H, T> {
        self.with_arc(|arc| arc.clone())
    }

    /// Create from a raw pointer produced by `triomphe::ThinArc::into_raw`.
    pub(crate) unsafe fn from_raw(ptr: *const ()) -> Self {
        ThinArcBorrow {
            ptr: NonNull::new(ptr as *mut _).unwrap(),
            _marker: PhantomData,
        }
    }
}
