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

use std::{
    cell::RefCell,
    fmt,
    fmt::{Debug, Display},
    hash::Hash,
};

use either::Either;
use gazebo::{
    any::ProvidesStaticType,
    cell::AsARef,
    coerce::{coerce, Coerce},
    prelude::*,
};
use serde::Serialize;

use crate::{
    self as starlark,
    collections::{Hashed, SmallMap, StarlarkHasher},
    eval::{Arguments, Evaluator, ParametersSpec},
    values::{
        comparison::equals_slice, display::display_keyed_container, function::FUNCTION_TYPE,
        typing::TypeCompiled, Freeze, Freezer, FrozenValue, Heap, StarlarkValue, Trace, Value,
        ValueLike,
    },
};

/// The result of `field()`.
#[derive(Clone, Debug, Dupe, Trace, Freeze, NoSerialize, ProvidesStaticType)]
pub struct FieldGen<V> {
    pub(crate) typ: V,
    default: Option<V>,
}

impl<V: Display> Display for FieldGen<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "field(")?;
        self.typ.fmt(f)?;
        if let Some(d) = &self.default {
            write!(f, ", ")?;
            d.fmt(f)?;
        }
        write!(f, ")")
    }
}

// Manual because no instance for Option<V>
unsafe impl<From: Coerce<To>, To> Coerce<FieldGen<To>> for FieldGen<From> {}

/// The result of `record()`, being the type of records.
#[derive(Debug, Trace, NoSerialize, ProvidesStaticType)]
pub struct RecordTypeGen<V, Typ> {
    /// The name of this type, e.g. MyRecord
    /// Either `Option<String>` or a `RefCell` thereof.
    typ: Typ,
    /// The V is the type the field must satisfy (e.g. `"string"`)
    fields: SmallMap<String, (FieldGen<V>, TypeCompiled)>,
    /// Creating these on every invoke is pretty expensive (profiling shows)
    /// so compute them in advance and cache.
    parameter_spec: ParametersSpec<FrozenValue>,
}

impl<V: Display, Typ> Display for RecordTypeGen<V, Typ> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_keyed_container(
            f,
            "record(",
            ")",
            "=",
            self.fields.iter().map(|(name, typ)| (name, &typ.0)),
        )
    }
}

/// Type of a record in a heap.
pub type RecordType<'v> = RecordTypeGen<Value<'v>, RefCell<Option<String>>>;
/// Type of a record in a frozen heap.
pub type FrozenRecordType = RecordTypeGen<FrozenValue, Option<String>>;

/// An actual record.
#[derive(Clone, Debug, Trace, Coerce, Freeze, ProvidesStaticType)]
#[repr(C)]
pub struct RecordGen<V> {
    typ: V, // Must be RecordType
    values: Vec<V>,
}

impl<'v, V: ValueLike<'v>> Display for RecordGen<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_keyed_container(f, "record(", ")", "=", self.iter())
    }
}

starlark_complex_value!(pub(crate) Field);
starlark_complex_values!(RecordType);
starlark_complex_value!(pub Record);

impl<V> FieldGen<V> {
    pub(crate) fn new(typ: V, default: Option<V>) -> Self {
        Self { typ, default }
    }
}

fn record_fields<'v>(
    x: Either<&'v RecordType<'v>, &'v FrozenRecordType>,
) -> &'v SmallMap<String, (FieldGen<Value<'v>>, TypeCompiled)> {
    x.either(|x| &x.fields, |x| coerce(&x.fields))
}

impl<'v> RecordType<'v> {
    pub(crate) fn new(fields: SmallMap<String, (FieldGen<Value<'v>>, TypeCompiled)>) -> Self {
        let parameter_spec = Self::make_parameter_spec(&fields);
        Self {
            typ: RefCell::new(None),
            fields,
            parameter_spec,
        }
    }

    fn make_parameter_spec(
        fields: &SmallMap<String, (FieldGen<Value<'v>>, TypeCompiled)>,
    ) -> ParametersSpec<FrozenValue> {
        let mut parameters = ParametersSpec::with_capacity("record".to_owned(), fields.len());
        parameters.no_more_positional_args();
        for (name, field) in fields {
            if field.0.default.is_some() {
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

    fn get_record_fields(&self) -> &'v SmallMap<String, (FieldGen<Value<'v>>, TypeCompiled)> {
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
    Self: ProvidesStaticType,
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
        let mut fields = SmallMap::with_capacity(self.fields.len());
        for (k, t) in self.fields.into_iter_hashed() {
            fields.insert_hashed(k, (t.0.freeze(freezer)?, t.1));
        }
        Ok(FrozenRecordType {
            typ: self.typ.into_inner(),
            fields,
            parameter_spec: self.parameter_spec,
        })
    }
}

impl<'v, Typ: 'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for RecordTypeGen<V, Typ>
where
    Self: ProvidesStaticType,
    FieldGen<V>: ProvidesStaticType,
    Typ: AsARef<Option<String>> + Debug,
{
    starlark_type!(FUNCTION_TYPE);

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        for (name, typ) in &self.fields {
            name.hash(hasher);
            // No need to hash typ.1, since it was computed from typ.0
            typ.0.write_hash(hasher)?;
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
                    match field.0.default {
                        None => {
                            let v: Value = param_parser.next(name)?;
                            v.check_type_compiled(field.0.typ, &field.1, Some(name))?;
                            values.push(v);
                        }
                        Some(default) => {
                            let v: Option<Value> = param_parser.next_opt(name)?;
                            match v {
                                None => values.push(default),
                                Some(v) => {
                                    v.check_type_compiled(field.0.typ, &field.1, Some(name))?;
                                    values.push(v);
                                }
                            }
                        }
                    }
                }
                Ok(eval.heap().alloc_complex(Record { typ: this, values }))
            })
    }

    fn extra_memory(&self) -> usize {
        // We don't capture the memory beneath the TypeCompiled, since we don't know how big
        // those closures are.
        let typ = AsARef::as_aref(&self.typ);
        typ.as_ref().map_or(0, |s| s.capacity()) + self.fields.extra_memory()
    }

    fn dir_attr(&self) -> Vec<String> {
        vec!["type".to_owned()]
    }

    fn has_attr(&self, attribute: &str) -> bool {
        attribute == "type"
    }

    fn get_attr(&self, attribute: &str, heap: &'v Heap) -> Option<Value<'v>> {
        if attribute == "type" {
            Some(
                heap.alloc(
                    AsARef::as_aref(&self.typ)
                        .as_deref()
                        .unwrap_or(Record::TYPE),
                ),
            )
        } else {
            None
        }
    }

    fn equals(&self, other: Value<'v>) -> anyhow::Result<bool> {
        fn eq<'v>(
            a: &RecordTypeGen<impl ValueLike<'v>, impl AsARef<Option<String>>>,
            b: &RecordTypeGen<impl ValueLike<'v>, impl AsARef<Option<String>>>,
        ) -> anyhow::Result<bool> {
            if AsARef::as_aref(&a.typ) != AsARef::as_aref(&b.typ) {
                return Ok(false);
            };
            if a.fields.len() != b.fields.len() {
                return Ok(false);
            };
            for ((k1, t1), (k2, t2)) in a.fields.iter().zip(b.fields.iter()) {
                // We require that the types and defaults are both equal.
                if k1 != k2
                    || !t1.0.typ.equals(t2.0.typ.to_value())?
                    || t1.0.default.map(ValueLike::to_value)
                        != t2.0.default.map(ValueLike::to_value)
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
        if let Some(typ) = AsARef::as_ref_cell(&self.typ) {
            let mut typ = typ.borrow_mut();
            if typ.is_none() {
                *typ = Some(variable_name.to_owned())
            }
        }
    }
}

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for RecordGen<V>
where
    Self: ProvidesStaticType,
{
    starlark_type!(Record::TYPE);

    fn matches_type(&self, ty: &str) -> bool {
        if ty == Record::TYPE {
            return true;
        }
        match self.get_record_type() {
            Either::Left(x) => Some(ty) == x.typ.borrow().as_deref(),
            Either::Right(x) => Some(ty) == x.typ.as_deref(),
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
        for v in &self.values {
            v.write_hash(hasher)?;
        }
        Ok(())
    }

    fn has_attr(&self, attribute: &str) -> bool {
        self.get_record_fields().contains_key(attribute)
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
