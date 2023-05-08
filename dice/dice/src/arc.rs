/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ops::Deref;

use allocative::Allocative;
use dupe::Dupe;

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
