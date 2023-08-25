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

use std::fmt;
use std::fmt::Display;

use allocative::Allocative;
use derivative::Derivative;
use either::Either;
use starlark_derive::starlark_value;
use starlark_derive::Coerce;
use starlark_derive::Freeze;
use starlark_derive::Trace;
use starlark_map::StarlarkHasher;

use crate as starlark;
use crate::__derive_refs::serde;
use crate::any::ProvidesStaticType;
use crate::starlark_complex_value;
use crate::starlark_complex_values;
use crate::values::enumeration::enum_type::EnumType;
use crate::values::enumeration::enum_type::FrozenEnumType;
use crate::values::exported_name::ExportedName;
use crate::values::Heap;
use crate::values::StarlarkValue;
use crate::values::Value;
use crate::values::ValueLike;

/// A value from an enumeration.
#[derive(
    Clone,
    Derivative,
    Trace,
    Coerce,
    Freeze,
    ProvidesStaticType,
    Allocative
)]
#[repr(C)]
#[derivative(Debug)]
pub struct EnumValueGen<V> {
    // Must ignore value.typ or type.elements, since they are circular
    #[derivative(Debug = "ignore")]
    pub(crate) typ: V, // Must be EnumType it points back to (so it can get the type)
    pub(crate) value: V,   // The value of this enumeration
    pub(crate) index: i32, // The index in the enumeration
}

impl<V: Display> Display for EnumValueGen<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.fmt(f)
    }
}

starlark_complex_values!(EnumType);
starlark_complex_value!(pub EnumValue);

impl<'v, V: ValueLike<'v>> EnumValueGen<V> {
    /// The result of calling `type()` on an enum value.
    pub const TYPE: &'static str = "enum";

    fn get_enum_type(&self) -> Either<&'v EnumType<'v>, &'v FrozenEnumType> {
        // Safe to unwrap because we always ensure typ is EnumType
        EnumType::from_value(self.typ.to_value()).unwrap()
    }
}

#[starlark_value(type = EnumValue::TYPE)]
impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for EnumValueGen<V>
where
    Self: ProvidesStaticType<'v>,
{
    fn matches_type(&self, ty: &str) -> bool {
        if ty == EnumValue::TYPE {
            return true;
        }
        match self.get_enum_type() {
            Either::Left(x) => x.typ.equal_to(ty),
            Either::Right(x) => x.typ.equal_to(ty),
        }
    }

    fn equals(&self, other: Value<'v>) -> anyhow::Result<bool> {
        match EnumValue::from_value(other) {
            Some(other) if self.typ.equals(other.typ)? => Ok(self.index == other.index),
            _ => Ok(false),
        }
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        self.value.write_hash(hasher)
    }

    fn get_attr(&self, attribute: &str, heap: &'v Heap) -> Option<Value<'v>> {
        match attribute {
            "index" => Some(heap.alloc(self.index)),
            "value" => Some(self.value.to_value()),
            _ => None,
        }
    }

    fn dir_attr(&self) -> Vec<String> {
        vec!["index".to_owned(), "value".to_owned()]
    }
}

impl<'v, V: ValueLike<'v>> serde::Serialize for EnumValueGen<V> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.value.serialize(serializer)
    }
}
