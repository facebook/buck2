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

use num_bigint::BigInt;
use num_traits::Num;

use crate::values::type_repr::StarlarkTypeRepr;
use crate::values::types::bigint::StarlarkBigInt;
use crate::values::AllocFrozenValue;
use crate::values::AllocValue;
use crate::values::FrozenHeap;
use crate::values::FrozenValue;
use crate::values::Heap;
use crate::values::UnpackValue;
use crate::values::Value;
use crate::values::ValueLike;

#[derive(Debug, thiserror::Error)]
enum StarlarkIntError {
    #[error("Cannot parse `{0}` as an integer in base {1}")]
    CannotParse(String, u32),
}

#[derive(Debug, Clone, Eq, PartialEq, derive_more::Display)]
pub(crate) enum StarlarkInt {
    Small(i32),
    Big(StarlarkBigInt),
}

pub(crate) enum StarlarkIntRef<'v> {
    Small(i32),
    Big(&'v StarlarkBigInt),
}

impl StarlarkInt {
    pub(crate) fn from_str_radix(s: &str, base: u32) -> anyhow::Result<StarlarkInt> {
        if let Ok(i) = i32::from_str_radix(s, base) {
            Ok(StarlarkInt::Small(i))
        } else {
            match BigInt::from_str_radix(s, base) {
                Ok(i) => Ok(StarlarkBigInt::try_from_bigint(i)),
                Err(_) => Err(StarlarkIntError::CannotParse(s.to_owned(), base).into()),
            }
        }
    }
}

impl StarlarkTypeRepr for StarlarkInt {
    fn starlark_type_repr() -> String {
        StarlarkBigInt::starlark_type_repr()
    }
}

impl<'v> AllocValue<'v> for StarlarkInt {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        match self {
            StarlarkInt::Small(i) => heap.alloc(i),
            StarlarkInt::Big(i) => heap.alloc(i),
        }
    }
}

impl AllocFrozenValue for StarlarkInt {
    fn alloc_frozen_value(self, heap: &FrozenHeap) -> FrozenValue {
        match self {
            StarlarkInt::Small(i) => heap.alloc(i),
            StarlarkInt::Big(i) => heap.alloc(i),
        }
    }
}

impl<'v> StarlarkTypeRepr for StarlarkIntRef<'v> {
    fn starlark_type_repr() -> String {
        StarlarkInt::starlark_type_repr()
    }
}

impl<'v> UnpackValue<'v> for StarlarkIntRef<'v> {
    fn unpack_value(value: Value<'v>) -> Option<Self> {
        if let Some(i) = i32::unpack_value(value) {
            Some(StarlarkIntRef::Small(i))
        } else {
            value.downcast_ref().map(StarlarkIntRef::Big)
        }
    }
}
