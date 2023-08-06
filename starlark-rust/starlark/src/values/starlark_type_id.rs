/*
 * Copyright 2018 The Starlark in Rust Authors.
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

use allocative::Allocative;
use dupe::Dupe;

use crate::values::layout::const_type_id::ConstTypeId;
use crate::values::StarlarkValue;

/// Identifier of a starlark type.
///
/// This is different from `TypeId` of `StarlarkValue` implementation:
/// multiple Rust types can share the same `StarlarkTypeId`.
/// For example, mutable and frozen list share the same `StarlarkTypeId`.
#[derive(Eq, PartialEq, Hash, Copy, Clone, Dupe, Debug, Allocative)]
pub(crate) struct StarlarkTypeId(ConstTypeId);

impl StarlarkTypeId {
    #[inline]
    pub(crate) const fn from_type_id(type_id: ConstTypeId) -> StarlarkTypeId {
        StarlarkTypeId(type_id)
    }

    #[inline]
    pub(crate) const fn of<'v, T: StarlarkValue<'v>>() -> StarlarkTypeId {
        StarlarkTypeId::of_canonical::<T::Canonical>()
    }

    #[inline]
    pub(crate) const fn of_canonical<'v, T: StarlarkValue<'v>>() -> StarlarkTypeId {
        // We should check that `T::Canonical` is `T`,
        // but we cannot do it until `TypeId::of` is const.
        StarlarkTypeId(ConstTypeId::of::<T::StaticType>())
    }
}
