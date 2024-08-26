/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::marker::PhantomData;

use allocative::Allocative;
use derivative::Derivative;
use dupe::Clone_;
use dupe::Dupe_;

use crate::deferred::key::DeferredKey;

/// A value to be stored in 'Provider' fields representing an asynchronously computed value
#[derive(Clone_, Dupe_, derive_more::Display, Derivative, Allocative)]
#[derivative(Debug, Eq, Hash, PartialEq)]
#[display("{}", key)]
#[allocative(bound = "")]
#[repr(C)]
pub struct DeferredData<T> {
    key: DeferredKey,
    // by invariant of construction, the key is uniquely identifying of the 'DeferredData'
    #[derivative(Debug = "ignore", Hash = "ignore", PartialEq = "ignore")]
    p: PhantomData<T>,
}

impl<T> DeferredData<T> {
    pub fn unchecked_new(key: DeferredKey) -> Self {
        Self {
            key,
            p: PhantomData,
        }
    }

    pub fn unchecked_new_ref(key: &DeferredKey) -> &Self {
        // SAFETY: `repr(C)`.
        unsafe { &*(key as *const DeferredKey as *const Self) }
    }

    #[inline]
    pub fn into_deferred_key(self) -> DeferredKey {
        self.key
    }

    pub fn deferred_key(&self) -> &DeferredKey {
        &self.key
    }
}
