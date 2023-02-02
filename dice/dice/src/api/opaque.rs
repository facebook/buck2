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

use crate::api::error::DiceResult;
use crate::api::key::Key;
use crate::opaque::OpaqueValueImpl;
use crate::ProjectionKey;

/// Computed value which is not directly visible to user.
///
/// The value can be accessed only via "projection" operation,
/// so projection result is recorded as a dependency
/// of a computation which requested the opaqued value,
/// but the opaque value key is not.
pub struct OpaqueValue<'a, K: Key> {
    pub(crate) implementation: OpaqueValueImpl<'a, K>,
}

impl<'a, K> Debug for OpaqueValue<'a, K>
where
    K: Key,
    K::Value: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.implementation.fmt(f)
    }
}

impl<'a, K: Key> OpaqueValue<'a, K> {
    pub(crate) fn new(implementation: OpaqueValueImpl<'a, K>) -> Self {
        Self { implementation }
    }

    pub fn projection<P>(&self, projection_key: &P) -> DiceResult<P::Value>
    where
        P: ProjectionKey<DeriveFromKey = K>,
    {
        self.implementation.projection(projection_key)
    }
}
