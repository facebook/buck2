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

//! A `record` type, comprising of a fixed set of fields.
//!
//! Calling `record()` produces a `RecordType`. Calling `RecordType` produces a [`Record`].
//! The field names of the record are only stored once, potentially reducing memory usage.
//! Created in Starlark using the `record()` function, which accepts keyword arguments.
//! The keys become field names, and values are the types. Calling the resulting
//! function produces an actual record.
//!
//! ```
//! # starlark::assert::is_true(r#"
//! IpAddress = record(host=str.type, port=int.type)
//! rec = IpAddress(host="localhost", port=80)
//! rec.port == 80
//! # "#);
//! ```
//!
//! It is also possible to use `field(type, default)` type to give defaults:
//!
//! ```
//! # starlark::assert::is_true(r#"
//! IpAddress = record(host=str.type, port=field(int.type, 80))
//! rec = IpAddress(host="localhost")
//! rec.port == 80
//! # "#);
//! ```

pub(crate) mod field;
pub(crate) mod record_type;

use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;

use allocative::Allocative;
use display_container::fmt_keyed_container;
use either::Either;
use serde::Serialize;
use starlark_derive::starlark_value;

use crate as starlark;
use crate::any::ProvidesStaticType;
use crate::coerce::Coerce;
use crate::collections::Hashed;
use crate::collections::SmallMap;
use crate::collections::StarlarkHasher;
use crate::starlark_complex_value;
use crate::values::comparison::equals_slice;
use crate::values::function::FUNCTION_TYPE;
use crate::values::record::field::FieldGen;
use crate::values::record::record_type::record_fields;
use crate::values::record::record_type::FrozenRecordType;
use crate::values::record::record_type::RecordType;
use crate::values::types::exported_name::ExportedName;
use crate::values::Freeze;
use crate::values::Heap;
use crate::values::StarlarkValue;
use crate::values::Trace;
use crate::values::Value;
use crate::values::ValueLike;

/// An actual record.
#[derive(Clone, Debug, Trace, Coerce, Freeze, ProvidesStaticType, Allocative)]
#[repr(C)]
pub struct RecordGen<V> {
    typ: V, // Must be RecordType
    values: Box<[V]>,
}

impl<'v, V: ValueLike<'v>> Display for RecordGen<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt_keyed_container(f, "record(", ")", "=", self.iter())
    }
}

starlark_complex_value!(pub Record);

impl<'v, V: ValueLike<'v>> RecordGen<V> {
    /// `type(x)` for records.
    pub const TYPE: &'static str = "record";

    fn get_record_type(&self) -> Either<&'v RecordType<'v>, &'v FrozenRecordType> {
        // Safe to unwrap because we always ensure typ is RecordType
        RecordType::from_value(self.typ.to_value()).unwrap()
    }

    fn get_record_fields(&self) -> &'v SmallMap<String, FieldGen<Value<'v>>> {
        record_fields(self.get_record_type())
    }

    /// Iterate over the elements in the record.
    pub fn iter<'a>(&'a self) -> impl ExactSizeIterator<Item = (&'v str, V)> + 'a
    where
        'v: 'a,
    {
        self.get_record_fields()
            .keys()
            .map(String::as_str)
            .zip(self.values.iter().copied())
    }
}

#[starlark_value(type = Record::TYPE)]
impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for RecordGen<V>
where
    Self: ProvidesStaticType<'v>,
{
    fn matches_type(&self, ty: &str) -> bool {
        if ty == Record::TYPE {
            return true;
        }
        match self.get_record_type() {
            Either::Left(x) => x.typ.equal_to(ty),
            Either::Right(x) => x.typ.equal_to(ty),
        }
    }

    fn equals(&self, other: Value<'v>) -> anyhow::Result<bool> {
        match Record::from_value(other) {
            Some(other) if self.typ.equals(other.typ)? => {
                equals_slice(&self.values, &other.values, |x, y| x.equals(*y))
            }
            _ => Ok(false),
        }
    }

    fn get_attr(&self, attribute: &str, heap: &'v Heap) -> Option<Value<'v>> {
        self.get_attr_hashed(Hashed::new(attribute), heap)
    }

    fn get_attr_hashed(&self, attribute: Hashed<&str>, _heap: &'v Heap) -> Option<Value<'v>> {
        let i = self.get_record_fields().get_index_of_hashed(attribute)?;
        Some(self.values[i].to_value())
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        self.typ.write_hash(hasher)?;
        for v in &*self.values {
            v.write_hash(hasher)?;
        }
        Ok(())
    }

    fn dir_attr(&self) -> Vec<String> {
        self.get_record_fields().keys().cloned().collect()
    }
}

impl<'v, V: ValueLike<'v>> Serialize for RecordGen<V> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.collect_map(self.iter())
    }
}
