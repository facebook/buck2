/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#[cfg(test)]
mod tests;

use std::fmt;
use std::fmt::Debug;
use std::fmt::Formatter;

use crate::api::error::DiceResult;
use crate::api::key::Key;
use crate::api::projection::ProjectionKey;
use crate::impls::opaque::OpaqueValueModern;
use crate::legacy::opaque::OpaqueValueImplLegacy;

/// Computed value which is not directly visible to user.
///
/// The value can be accessed only via "projection" operation,
/// so projection result is recorded as a dependency
/// of a computation which requested the opaqued value,
/// but the opaque value key is not.
pub(crate) enum OpaqueValueImpl<'a, K: Key> {
    Legacy(OpaqueValueImplLegacy<'a, K>),
    Modern(OpaqueValueModern<K>),
}

impl<'a, K> Debug for OpaqueValueImpl<'a, K>
where
    K: Key,
    K::Value: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            OpaqueValueImpl::Legacy(delegate) => delegate.fmt(f),
            OpaqueValueImpl::Modern(delegate) => delegate.fmt(f),
        }
    }
}

impl<'a, K: Key> OpaqueValueImpl<'a, K> {
    pub(crate) fn projection<P>(&self, projection_key: &P) -> DiceResult<P::Value>
    where
        P: ProjectionKey<DeriveFromKey = K>,
    {
        match self {
            OpaqueValueImpl::Legacy(delegate) => delegate.projection(projection_key),
            OpaqueValueImpl::Modern(delegate) => delegate.projection(projection_key),
        }
    }
}
