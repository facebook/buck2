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
//! Calling `record()` produces a [`RecordType`]. Calling [`RecordType`] produces a [`Record`].
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

use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;

use allocative::Allocative;
use display_container::fmt_keyed_container;
use dupe::Dupe;
use either::Either;
use serde::Serialize;
use starlark_derive::NoSerialize;
use starlark_derive::StarlarkDocs;

use crate as starlark;
use crate::any::ProvidesStaticType;
use crate::coerce::coerce;
use crate::coerce::Coerce;
use crate::collections::Hashed;
use crate::collections::SmallMap;
use crate::collections::StarlarkHasher;
use crate::eval::Arguments;
use crate::eval::Evaluator;
use crate::eval::ParametersSpec;
use crate::starlark_complex_value;
use crate::starlark_complex_values;
use crate::starlark_type;
use crate::values::comparison::equals_slice;
use crate::values::function::FUNCTION_TYPE;
use crate::values::types::exported_name::ExportedName;
use crate::values::types::exported_name::FrozenExportedName;
use crate::values::types::exported_name::MutableExportedName;
use crate::values::typing::TypeCompiled;
use crate::values::Freeze;
use crate::values::Freezer;
use crate::values::FrozenValue;
use crate::values::Heap;
use crate::values::StarlarkValue;
use crate::values::Trace;
use crate::values::Value;
use crate::values::ValueLike;

/// The result of `field()`.
#[derive(
    Clone,
    Debug,
    Dupe,
    Trace,
    Freeze,
    NoSerialize,
    ProvidesStaticType,
    StarlarkDocs,
    Allocative
)]
#[starlark_docs(builtin = "extension")]
pub struct FieldGen<V> {
    pub(crate) typ: TypeCompiled<V>,
    default: Option<V>,
}

impl<'v, V: ValueLike<'v>> Display for FieldGen<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "field(")?;
        Display::fmt(&self.typ, f)?;
        if let Some(d) = &self.default {
            write!(f, ", ")?;
            Display::fmt(d, f)?;
        }
        write!(f, ")")
    }
}

// Manual because no instance for Option<V>
unsafe impl<From: Coerce<To>, To> Coerce<FieldGen<To>> for FieldGen<From> {}

/// The result of `record()`, being the type of records.
#[derive(
    Debug,
    Trace,
    NoSerialize,
    ProvidesStaticType,
    StarlarkDocs,
    Allocative
)]
#[starlark_docs(builtin = "extension")]
pub struct RecordTypeGen<V, Typ: ExportedName> {
    typ: Typ,
    /// The V is the type the field must satisfy (e.g. `"string"`)
    fields: SmallMap<String, FieldGen<V>>,
    /// Creating these on every invoke is pretty expensive (profiling shows)
    /// so compute them in advance and cache.
    parameter_spec: ParametersSpec<FrozenValue>,
}

impl<'v, V: ValueLike<'v>, Typ: ExportedName> Display for RecordTypeGen<V, Typ> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt_keyed_container(f, "record(", ")", "=", &self.fields)
    }
}

/// Type of a record in a heap.
pub type RecordType<'v> = RecordTypeGen<Value<'v>, MutableExportedName>;
/// Type of a record in a frozen heap.
pub type FrozenRecordType = RecordTypeGen<FrozenValue, FrozenExportedName>;

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

starlark_complex_value!(pub(crate) Field);
starlark_complex_values!(RecordType);
starlark_complex_value!(pub Record);

impl<V> FieldGen<V> {
    pub(crate) fn new(typ: TypeCompiled<V>, default: Option<V>) -> Self {
        Self { typ, default }
    }
}

fn record_fields<'v>(
    x: Either<&'v RecordType<'v>, &'v FrozenRecordType>,
) -> &'v SmallMap<String, FieldGen<Value<'v>>> {
    x.either(|x| &x.fields, |x| coerce(&x.fields))
}

impl<'v> RecordType<'v> {
    pub(crate) fn new(fields: SmallMap<String, FieldGen<Value<'v>>>) -> Self {
        let parameter_spec = Self::make_parameter_spec(&fields);
        Self {
            typ: MutableExportedName::default(),
            fields,
            parameter_spec,
        }
    }

    fn make_parameter_spec(
        fields: &SmallMap<String, FieldGen<Value<'v>>>,
    ) -> ParametersSpec<FrozenValue> {
        let mut parameters = ParametersSpec::with_capacity("record".to_owned(), fields.len());
        parameters.no_more_positional_args();
        for (name, field) in fields {
            if field.default.is_some() {
                parameters.optional(name);
            } else {
                parameters.required(name);
            }
        }
        parameters.finish()
    }
}

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

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for FieldGen<V>
where
    Self: ProvidesStaticType<'v>,
{
    starlark_type!("field");

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        self.typ.write_hash(hasher)?;
        self.default.is_some().hash(hasher);
        if let Some(d) = self.default {
            d.write_hash(hasher)?;
        }
        Ok(())
    }
}

impl<'v> Freeze for RecordType<'v> {
    type Frozen = FrozenRecordType;
    fn freeze(self, freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        Ok(FrozenRecordType {
            typ: self.typ.freeze(freezer)?,
            fields: self.fields.freeze(freezer)?,
            parameter_spec: self.parameter_spec,
        })
    }
}

impl<'v, Typ: Allocative + 'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for RecordTypeGen<V, Typ>
where
    Self: ProvidesStaticType<'v>,
    FieldGen<V>: ProvidesStaticType<'v>,
    Typ: ExportedName,
{
    starlark_type!(FUNCTION_TYPE);

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        for (name, typ) in &self.fields {
            name.hash(hasher);
            // No need to hash typ.1, since it was computed from typ.0
            typ.write_hash(hasher)?;
        }
        Ok(())
    }

    fn invoke(
        &self,
        me: Value<'v>,
        args: &Arguments<'v, '_>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let this = me;

        self.parameter_spec
            .parser(args, eval, |mut param_parser, eval| {
                let fields = record_fields(RecordType::from_value(this).unwrap());
                let mut values = Vec::with_capacity(fields.len());
                for (name, field) in fields.iter() {
                    let value = match field.default {
                        None => {
                            let v: Value = param_parser.next(name)?;
                            field.typ.check_type(v, Some(name))?;
                            v
                        }
                        Some(default) => {
                            let v: Option<Value> = param_parser.next_opt(name)?;
                            match v {
                                None => default,
                                Some(v) => {
                                    field.typ.check_type(v, Some(name))?;
                                    v
                                }
                            }
                        }
                    };
                    values.push(value);
                }
                Ok(eval.heap().alloc_complex(Record {
                    typ: this,
                    values: values.into_boxed_slice(),
                }))
            })
    }

    fn dir_attr(&self) -> Vec<String> {
        vec!["type".to_owned()]
    }

    fn has_attr(&self, attribute: &str, _heap: &'v Heap) -> bool {
        attribute == "type"
    }

    fn get_attr(&self, attribute: &str, heap: &'v Heap) -> Option<Value<'v>> {
        if attribute == "type" {
            Some(
                heap.alloc(
                    self.typ
                        .borrow()
                        .as_ref()
                        .map_or(Record::TYPE, |s| s.as_str()),
                ),
            )
        } else {
            None
        }
    }

    fn equals(&self, other: Value<'v>) -> anyhow::Result<bool> {
        fn eq<'v>(
            a: &RecordTypeGen<impl ValueLike<'v>, impl ExportedName>,
            b: &RecordTypeGen<impl ValueLike<'v>, impl ExportedName>,
        ) -> anyhow::Result<bool> {
            if a.typ.borrow() != b.typ.borrow() {
                return Ok(false);
            };
            if a.fields.len() != b.fields.len() {
                return Ok(false);
            };
            for ((k1, t1), (k2, t2)) in a.fields.iter().zip(b.fields.iter()) {
                // We require that the types and defaults are both equal.
                if k1 != k2
                    || !t1.typ.to_value().equals(t2.typ.to_value())?
                    || t1.default.map(ValueLike::to_value) != t2.default.map(ValueLike::to_value)
                {
                    return Ok(false);
                }
            }
            Ok(true)
        }

        match RecordType::from_value(other) {
            Some(Either::Left(other)) => eq(self, other),
            Some(Either::Right(other)) => eq(self, other),
            _ => Ok(false),
        }
    }

    fn export_as(&self, variable_name: &str, _eval: &mut Evaluator<'v, '_>) {
        self.typ.try_export_as(variable_name);
    }
}

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for RecordGen<V>
where
    Self: ProvidesStaticType<'v>,
{
    starlark_type!(Record::TYPE);

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
