/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::marker::PhantomData;

use crate::api::error::DiceResult;
use crate::api::key::Key;
use crate::api::projection::ProjectionKey;

#[derive(Debug)]
pub(crate) struct OpaqueValueModern<K>(PhantomData<K>);

impl<K> OpaqueValueModern<K>
where
    K: Key,
{
    pub(crate) fn projection<P>(&self, _projection_key: &P) -> DiceResult<P::Value>
    where
        P: ProjectionKey<DeriveFromKey = K>,
    {
        unimplemented!("todo")
    }

    /// Get a value and record parent computation dependency on `K`.
    pub(crate) fn into_value(self) -> K::Value {
        unimplemented!("todo")
    }
}
