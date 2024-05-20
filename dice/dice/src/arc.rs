/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ops::Deref;
use std::ptr;

use allocative::Allocative;
use dupe::Dupe;
use lock_free_hashtable::atomic_value::AtomicValue;

#[derive(Debug, Eq, PartialEq, Hash, Allocative)]
pub(crate) struct Arc<T>(triomphe::Arc<T>);

impl<T> Arc<T> {
    #[inline]
    pub(crate) fn new(value: T) -> Self {
        Arc(triomphe::Arc::new(value))
    }

    #[inline]
    #[cfg(test)]
    pub(crate) fn ptr_eq(this: &Self, other: &Self) -> bool {
        triomphe::Arc::ptr_eq(&this.0, &other.0)
    }
}

impl<T: Clone> Arc<T> {
    #[inline]
    pub(crate) fn make_mut(&mut self) -> &mut T {
        triomphe::Arc::make_mut(&mut self.0)
    }
}

impl<T> Clone for Arc<T> {
    #[inline]
    fn clone(&self) -> Self {
        Arc(self.0.clone())
    }
}

impl<T> Dupe for Arc<T> {}

impl<T> Deref for Arc<T> {
    type Target = triomphe::Arc<T>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> AtomicValue for Arc<T> {
    type Raw = *const T;
    type Ref<'a> = &'a T where Self: 'a;

    #[inline]
    fn null() -> Self::Raw {
        ptr::null()
    }

    #[inline]
    fn is_null(this: Self::Raw) -> bool {
        this.is_null()
    }

    #[inline]
    fn into_raw(this: Self) -> Self::Raw {
        triomphe::Arc::into_raw(this.0)
    }

    #[inline]
    unsafe fn from_raw(raw: Self::Raw) -> Self {
        Arc(triomphe::Arc::from_raw(raw))
    }

    #[inline]
    unsafe fn deref<'a>(raw: Self::Raw) -> Self::Ref<'a> {
        &*raw
    }
}
