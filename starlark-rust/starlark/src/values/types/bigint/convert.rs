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

use crate::values::type_repr::StarlarkTypeRepr;
use crate::values::types::bigint::StarlarkBigInt;
use crate::values::AllocFrozenValue;
use crate::values::AllocValue;
use crate::values::FrozenHeap;
use crate::values::FrozenValue;
use crate::values::Heap;
use crate::values::UnpackValue;
use crate::values::Value;

impl StarlarkTypeRepr for u32 {
    fn starlark_type_repr() -> String {
        i32::starlark_type_repr()
    }
}

impl<'v> AllocValue<'v> for u32 {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        match i32::try_from(self) {
            Ok(x) => Value::new_int(x),
            Err(_) => StarlarkBigInt::alloc_bigint(self.into(), heap),
        }
    }
}

impl AllocFrozenValue for u32 {
    fn alloc_frozen_value(self, heap: &FrozenHeap) -> FrozenValue {
        match i32::try_from(self) {
            Ok(x) => FrozenValue::new_int(x),
            Err(_) => StarlarkBigInt::alloc_bigint_frozen(self.into(), heap),
        }
    }
}

impl StarlarkTypeRepr for u64 {
    fn starlark_type_repr() -> String {
        i32::starlark_type_repr()
    }
}

impl<'v> AllocValue<'v> for u64 {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        match i32::try_from(self) {
            Ok(x) => Value::new_int(x),
            Err(_) => StarlarkBigInt::alloc_bigint(self.into(), heap),
        }
    }
}

impl AllocFrozenValue for u64 {
    fn alloc_frozen_value(self, heap: &FrozenHeap) -> FrozenValue {
        match i32::try_from(self) {
            Ok(x) => FrozenValue::new_int(x),
            Err(_) => StarlarkBigInt::alloc_bigint_frozen(self.into(), heap),
        }
    }
}

impl StarlarkTypeRepr for i64 {
    fn starlark_type_repr() -> String {
        i32::starlark_type_repr()
    }
}

impl<'v> AllocValue<'v> for i64 {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        match i32::try_from(self) {
            Ok(x) => Value::new_int(x),
            Err(_) => StarlarkBigInt::alloc_bigint(self.into(), heap),
        }
    }
}

impl AllocFrozenValue for i64 {
    fn alloc_frozen_value(self, heap: &FrozenHeap) -> FrozenValue {
        match i32::try_from(self) {
            Ok(x) => FrozenValue::new_int(x),
            Err(_) => StarlarkBigInt::alloc_bigint_frozen(self.into(), heap),
        }
    }
}

impl StarlarkTypeRepr for usize {
    fn starlark_type_repr() -> String {
        i32::starlark_type_repr()
    }
}

impl<'v> AllocValue<'v> for usize {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        match i32::try_from(self) {
            Ok(x) => Value::new_int(x),
            Err(_) => StarlarkBigInt::alloc_bigint(self.into(), heap),
        }
    }
}

impl AllocFrozenValue for usize {
    fn alloc_frozen_value(self, heap: &FrozenHeap) -> FrozenValue {
        match i32::try_from(self) {
            Ok(x) => FrozenValue::new_int(x),
            Err(_) => StarlarkBigInt::alloc_bigint_frozen(self.into(), heap),
        }
    }
}

impl StarlarkTypeRepr for isize {
    fn starlark_type_repr() -> String {
        i32::starlark_type_repr()
    }
}

impl<'v> AllocValue<'v> for isize {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        match i32::try_from(self) {
            Ok(x) => Value::new_int(x),
            Err(_) => StarlarkBigInt::alloc_bigint(self.into(), heap),
        }
    }
}

impl AllocFrozenValue for isize {
    fn alloc_frozen_value(self, heap: &FrozenHeap) -> FrozenValue {
        match i32::try_from(self) {
            Ok(x) => FrozenValue::new_int(x),
            Err(_) => StarlarkBigInt::alloc_bigint_frozen(self.into(), heap),
        }
    }
}

impl<'v> UnpackValue<'v> for u32 {
    fn unpack_value(value: Value<'v>) -> Option<u32> {
        value.unpack_integer()
    }
}

impl<'v> UnpackValue<'v> for u64 {
    fn unpack_value(value: Value<'v>) -> Option<u64> {
        value.unpack_integer()
    }
}

impl<'v> UnpackValue<'v> for i64 {
    fn unpack_value(value: Value<'v>) -> Option<i64> {
        value.unpack_integer()
    }
}

impl<'v> UnpackValue<'v> for usize {
    fn unpack_value(value: Value<'v>) -> Option<usize> {
        value.unpack_integer()
    }
}

impl<'v> UnpackValue<'v> for isize {
    fn unpack_value(value: Value<'v>) -> Option<isize> {
        value.unpack_integer()
    }
}
