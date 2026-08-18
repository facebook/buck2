/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
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

    /// Create from a raw pointer produced by `triomphe::ThinArc::into_raw`.
    pub(crate) unsafe fn from_raw(ptr: *const ()) -> Self {
        ThinArcBorrow {
            ptr: NonNull::new(ptr as *mut _).unwrap(),
            _marker: PhantomData,
        }
    }

    /// Raw pointer identity of the underlying allocation.
    pub(crate) fn as_ptr(self) -> *const () {
        self.ptr.as_ptr()
    }
}
