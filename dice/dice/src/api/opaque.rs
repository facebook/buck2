/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::fmt::Debug;
use std::fmt::Formatter;

use crate::api::key::Key;
use crate::impls::opaque::OpaqueValueModern;
use crate::opaque::OpaqueValueImpl;

/// Computed value which is not directly visible to user.
///
/// The value can be accessed only via "projection" operation,
/// so projection result is recorded as a dependency
/// of a computation which requested the opaqued value,
/// but the opaque value key is not.
pub struct OpaqueValue<K: Key> {
    pub(crate) implementation: OpaqueValueImpl<K>,
}

impl<K> Debug for OpaqueValue<K>
where
    K: Key,
    K::Value: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.implementation.fmt(f)
    }
}

impl<K: Key> OpaqueValue<K> {
    pub(crate) fn new(implementation: OpaqueValueImpl<K>) -> Self {
        Self { implementation }
    }

    pub(crate) fn unpack_modern(&self) -> Option<&OpaqueValueModern<K>> {
        match &self.implementation {
            OpaqueValueImpl::Modern(v) => Some(v),
        }
    }

    pub(crate) fn into_modern(self) -> Option<OpaqueValueModern<K>> {
        match self.implementation {
            OpaqueValueImpl::Modern(v) => Some(v),
        }
    }
}
