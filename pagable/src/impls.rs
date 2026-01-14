/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

mod arc;
mod collections;
mod serde;
mod std;
mod tuples;

use relative_path::RelativePathBuf;

use crate::__internal::serde::Deserialize;
use crate::__internal::serde::Serializer;
use crate::PagableDeserialize;
use crate::PagableDeserializer;
use crate::PagableSerialize;
use crate::PagableSerializer;

impl PagableSerialize for RelativePathBuf {
    fn pagable_serialize<S: PagableSerializer>(&self, serializer: &mut S) -> crate::Result<()> {
        Ok(serializer.serde().serialize_str(self.as_str())?)
    }
}

impl<'de> PagableDeserialize<'de> for RelativePathBuf {
    fn pagable_deserialize<D: PagableDeserializer<'de>>(
        deserializer: &mut D,
    ) -> crate::Result<Self> {
        Ok(RelativePathBuf::from(String::deserialize(
            deserializer.serde(),
        )?))
    }
}
