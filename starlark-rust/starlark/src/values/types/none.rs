/*
 * Copyright 2019 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//! The `None` type.
use std::hash::Hasher;

use derive_more::Display;
use gazebo::{any::ProvidesStaticType, prelude::*};
use serde::{Serialize, Serializer};

use crate::{
    collections::{StarlarkHashValue, StarlarkHasher},
    private::Private,
    values::{
        basic::StarlarkValueBasic, AllocFrozenValue, AllocValue, FrozenHeap, FrozenValue, Heap,
        StarlarkValue, UnpackValue, Value,
    },
};

/// Define the None type, use [`NoneType`] in Rust.
#[derive(Debug, Clone, Dupe, ProvidesStaticType, Display)]
#[display(fmt = "None")]
pub struct NoneType;

impl NoneType {
    /// The result of `type(None)`.
    pub const TYPE: &'static str = "NoneType";
}

/// Define the NoneType type
impl<'v> StarlarkValue<'v> for NoneType {
    starlark_type!(NoneType::TYPE);

    fn is_special(_: Private) -> bool
    where
        Self: Sized,
    {
        true
    }

    fn equals(&self, other: Value) -> anyhow::Result<bool> {
        // We always compare pointers before calling `equals`,
        // so if we are here, the other is definitely not `None`.
        debug_assert!(!other.is_none());
        Ok(false)
    }

    fn to_bool(&self) -> bool {
        false
    }
    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        // just took the result of hash(None) in macos python 2.7.10 interpreter.
        hasher.write_u64(9_223_380_832_852_120_682);
        Ok(())
    }
}

impl<'v> StarlarkValueBasic<'v> for NoneType {
    fn get_hash(&self) -> StarlarkHashValue {
        // Just a random number.
        StarlarkHashValue::new_unchecked(0xf9c2263d)
    }
}

impl<'v> AllocValue<'v> for NoneType {
    fn alloc_value(self, _heap: &'v Heap) -> Value<'v> {
        Value::new_none()
    }
}

impl Serialize for NoneType {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_none()
    }
}

impl AllocFrozenValue for NoneType {
    fn alloc_frozen_value(self, _heap: &FrozenHeap) -> FrozenValue {
        FrozenValue::new_none()
    }
}

/// Equivalent of a Rust [`Option`], where [`None`] is encoded as [`NoneType`]. Useful for its [`UnpackValue`] instance.
#[derive(Debug, Eq, PartialEq, Copy, Clone, Dupe)]
pub enum NoneOr<T> {
    /// Starlark `None`.
    None,
    /// Not `None`.
    Other(T),
}

impl<T> NoneOr<T> {
    /// Convert the [`NoneOr`] to a real Rust [`Option`].
    pub fn into_option(self) -> Option<T> {
        match self {
            Self::None => None,
            Self::Other(x) => Some(x),
        }
    }

    /// Is the value a [`NoneOr::None`].
    pub fn is_none(&self) -> bool {
        matches!(self, NoneOr::None)
    }
}

impl<'v, T: UnpackValue<'v>> UnpackValue<'v> for NoneOr<T> {
    fn expected() -> String {
        format!("None or {}", T::expected())
    }

    fn unpack_value(value: Value<'v>) -> Option<Self> {
        if value.is_none() {
            Some(NoneOr::None)
        } else {
            T::unpack_value(value).map(NoneOr::Other)
        }
    }
}
