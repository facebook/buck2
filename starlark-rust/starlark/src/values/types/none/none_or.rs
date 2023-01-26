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

use dupe::Dupe;

use crate::values::type_repr::StarlarkTypeRepr;
use crate::values::UnpackValue;
use crate::values::Value;

/// Equivalent of a Rust [`Option`], where `None`
/// is encoded as [`NoneType`](crate::values::none::NoneType).
/// Useful for its [`UnpackValue`] instance.
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

impl<'v, T: UnpackValue<'v>> StarlarkTypeRepr for NoneOr<T> {
    fn starlark_type_repr() -> String {
        Option::<T>::starlark_type_repr()
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
