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

use std::marker;
use std::marker::PhantomData;

use starlark_map::small_map::SmallMap;

use crate::values::structs::value::Struct;
use crate::values::structs::StructRef;
use crate::values::type_repr::StarlarkTypeRepr;
use crate::values::StringValue;
use crate::values::UnpackValue;
use crate::values::Value;
use crate::values::ValueOf;

/// Like [`ValueOf`](crate::values::ValueOf), but only validates value types; does not construct
/// or store a map.
#[derive(Debug)]
pub struct StructOf<'v, V: UnpackValue<'v>> {
    value: ValueOf<'v, StructRef<'v>>,
    _marker: PhantomData<V>,
}

impl<'v, V: UnpackValue<'v>> StarlarkTypeRepr for StructOf<'v, V> {
    fn starlark_type_repr() -> String {
        Struct::TYPE.to_owned()
    }
}

impl<'v, V: UnpackValue<'v>> UnpackValue<'v> for StructOf<'v, V> {
    fn expected() -> String {
        format!("struct with fields of type {}", V::expected())
    }

    fn unpack_value(value: Value<'v>) -> Option<StructOf<'v, V>> {
        let value = ValueOf::<StructRef>::unpack_value(value)?;
        for (_k, v) in value.typed.iter() {
            // Validate field types
            V::unpack_value(v)?;
        }
        Some(StructOf {
            value,
            _marker: marker::PhantomData,
        })
    }
}

impl<'v, V: UnpackValue<'v>> StructOf<'v, V> {
    /// Get the actual value this `StructOf` wraps.
    pub fn to_value(&self) -> Value<'v> {
        self.value.value
    }

    /// Get untyped struct reference.
    fn as_struct(&self) -> &StructRef<'v> {
        &self.value.typed
    }

    /// Collect field structs.
    pub fn to_map(&self) -> SmallMap<StringValue<'v>, V> {
        self.as_struct()
            .iter()
            .map(|(k, v)| (k, V::unpack_value(v).expect("validated at construction")))
            .collect()
    }
}
