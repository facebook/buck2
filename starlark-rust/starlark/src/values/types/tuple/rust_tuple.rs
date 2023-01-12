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

//! Bindings to/from Rust tuple types.

use crate::values::tuple::Tuple;
use crate::values::type_repr::StarlarkTypeRepr;
use crate::values::AllocValue;
use crate::values::Heap;
use crate::values::UnpackValue;
use crate::values::Value;

impl<'v, T1: AllocValue<'v>> AllocValue<'v> for (T1,) {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_tuple(&[self.0.alloc_value(heap)])
    }
}

impl<'v, T1: AllocValue<'v>, T2: AllocValue<'v>> AllocValue<'v> for (T1, T2) {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_tuple(&[self.0.alloc_value(heap), self.1.alloc_value(heap)])
    }
}

impl<'v, T1: AllocValue<'v>, T2: AllocValue<'v>, T3: AllocValue<'v>> AllocValue<'v>
    for (T1, T2, T3)
{
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_tuple(&[
            self.0.alloc_value(heap),
            self.1.alloc_value(heap),
            self.2.alloc_value(heap),
        ])
    }
}

impl<T1: StarlarkTypeRepr, T2: StarlarkTypeRepr> StarlarkTypeRepr for (T1, T2) {
    fn starlark_type_repr() -> String {
        format!(
            "({}, {})",
            T1::starlark_type_repr(),
            T2::starlark_type_repr()
        )
    }
}

impl<T1: StarlarkTypeRepr> StarlarkTypeRepr for (T1,) {
    fn starlark_type_repr() -> String {
        format!("({},)", T1::starlark_type_repr())
    }
}

impl<T1: StarlarkTypeRepr, T2: StarlarkTypeRepr, T3: StarlarkTypeRepr> StarlarkTypeRepr
    for (T1, T2, T3)
{
    fn starlark_type_repr() -> String {
        format!(
            "({}, {}, {})",
            T1::starlark_type_repr(),
            T2::starlark_type_repr(),
            T3::starlark_type_repr()
        )
    }
}

impl<'v, T1: UnpackValue<'v>, T2: UnpackValue<'v>> UnpackValue<'v> for (T1, T2) {
    fn expected() -> String {
        format!("tuple ({}, {})", T1::expected(), T2::expected())
    }

    fn unpack_value(value: Value<'v>) -> Option<Self> {
        let t = Tuple::from_value(value)?;
        if t.len() != 2 {
            return None;
        }
        Some((
            T1::unpack_value(t.content()[0])?,
            T2::unpack_value(t.content()[1])?,
        ))
    }
}
