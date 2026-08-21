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
use crate::collections::Hashed;
use crate::collections::SmallMap;
use crate::collections::StarlarkHasher;
use crate::starlark_complex_value_branded;
use crate::typing::Ty;
use crate::values::FreezeBranded;
use crate::values::Heap;
use crate::values::StarlarkPagable;
use crate::values::StarlarkValue;
use crate::values::Trace;
use crate::values::UnpackValue;
use crate::values::Value;
use crate::values::ValueLike;
use crate::values::ValueTyped;
use crate::values::comparison::equals_slice;
use crate::values::record::field::Field;
use crate::values::record::record_type::AnyRecordType;
use crate::values::record::record_type::RecordTypeError;
use crate::values::record::record_type::RecordTypeGen;
use crate::values::record::record_type::RecordVariant;
use crate::values::record::record_type::record_fields;
use crate::values::types::type_instance_id::TypeInstanceId;

#[derive(Debug, thiserror::Error)]
enum RecordError {
    #[error("Record type has {fields} fields, but {values} values were provided")]
    WrongNumberOfValues { fields: usize, values: usize },
}

/// An actual record.
#[derive(
    Clone,
    Debug,
    Trace,
    FreezeBranded,
    ProvidesStaticType,
    Allocative,
    StarlarkPagable
)]
#[repr(C)]
pub struct Record<'v> {
    pub(crate) typ: Value<'v>, // Must be RecordType
    pub(crate) values: Box<[Value<'v>]>,
}

impl<'v> Display for Record<'v> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = self.record_type_name().unwrap_or("anon");
        fmt_keyed_container(f, &format!("record[{name}]("), ")", "=", self.iter())
    }
}

starlark_complex_value_branded!(pub Record);

impl<'v> Record<'v> {
    /// `type(x)` for records.
    pub const TYPE: &'static str = "record";

    /// Creates a new record of type `typ`.
    ///
    /// `typ` must already have a name assigned, which happens when the record type
    /// is exported: assigned to a global variable in Starlark, or passed to
    /// [`Value::export_as`](crate::values::Value::export_as) from Rust.
    /// `values` must contain one value per field of `typ`, in field declaration
    /// order; defaults are not applied. Each value is checked against its
    /// field's type.
    pub fn new<V: RecordVariant>(
        typ: ValueTyped<'v, RecordTypeGen<'v, V>>,
        values: Box<[Value<'v>]>,
    ) -> crate::Result<Self> {
        if typ.as_ref().ty_record_data.get_ty().is_none() {
            return Err(crate::Error::new_other(
                RecordTypeError::RecordTypeNotAssigned,
            ));
        }
        let fields = &typ.as_ref().fields;
        if fields.len() != values.len() {
            return Err(crate::Error::new_other(RecordError::WrongNumberOfValues {
                fields: fields.len(),
                values: values.len(),
            }));
        }
        for ((name, field), value) in fields.iter().zip(&values) {
            field.typ.check_type(*value, Some(name))?;
        }
        Ok(Self {
            typ: typ.to_value(),
            values,
        })
    }

    fn get_record_type(&self) -> AnyRecordType<'v> {
        // Safe to unwrap because we always ensure typ is RecordType
        AnyRecordType::unpack_value_err(self.typ.to_value()).unwrap()
    }

    fn record_type_name(&self) -> Option<&'v str> {
        match self.get_record_type() {
            Either::Left(x) => Some(&x.ty_record_data.get_ty()?.name),
            Either::Right(x) => Some(&x.ty_record_data.get_ty()?.name),
        }
    }

    pub(crate) fn record_type_id(&self) -> TypeInstanceId {
        match self.get_record_type() {
            Either::Left(x) => x.id,
            Either::Right(x) => x.id,
        }
    }

    fn get_record_fields(&self) -> &'v SmallMap<String, Field<'v>> {
        record_fields(self.get_record_type())
    }

    /// Iterate over the elements in the record.
    pub fn iter<'a>(&'a self) -> impl ExactSizeIterator<Item = (&'v str, Value<'v>)> + 'a
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
impl<'v> StarlarkValue<'v> for Record<'v> {
    fn equals(&self, other: Value<'v>) -> crate::Result<bool> {
        match Record::from_value(other) {
            Some(other) if self.typ.equals(other.typ)? => {
                equals_slice(&self.values, &other.values, |x, y| x.equals(*y))
            }
            _ => Ok(false),
        }
    }

    fn get_attr(&self, attribute: &str, heap: Heap<'v>) -> Option<Value<'v>> {
        self.get_attr_hashed(Hashed::new(attribute), heap)
    }

    fn get_attr_hashed(&self, attribute: Hashed<&str>, _heap: Heap<'v>) -> Option<Value<'v>> {
        let i = self.get_record_fields().get_index_of_hashed(attribute)?;
        Some(self.values[i].to_value())
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> crate::Result<()> {
        self.typ.write_hash(hasher)?;
        for v in &*self.values {
            v.write_hash(hasher)?;
        }
        Ok(())
    }

    fn dir_attr(&self) -> Vec<String> {
        self.get_record_fields().keys().cloned().collect()
    }

    fn typechecker_ty(&self) -> Option<Ty> {
        Some(
            self.get_record_type()
                .either(|r| r.instance_ty(), |r| r.instance_ty()),
        )
    }
}

impl<'v> Serialize for Record<'v> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.collect_map(self.iter())
    }
}

#[cfg(test)]
mod tests {
    use starlark_map::small_map::SmallMap;

    use crate::environment::Module;
    use crate::eval::Evaluator;
    use crate::typing::Ty;
    use crate::values::Heap;
    use crate::values::ValueTyped;
    use crate::values::layout::heap::heap_type::StarlarkTestHeapName;
    use crate::values::record::field::Field;
    use crate::values::record::instance::Record;
    use crate::values::record::record_type::FrozenRecordType;
    use crate::values::record::record_type::RecordType;
    use crate::values::types::type_instance_id::TypeInstanceId;
    use crate::values::typing::type_compiled::compiled::TypeCompiled;

    fn unnamed_ip_address_record_type<'v>(heap: Heap<'v>) -> ValueTyped<'v, RecordType<'v>> {
        let mut fields = SmallMap::new();
        fields.insert(
            "host".to_owned(),
            Field::new(TypeCompiled::from_ty(&Ty::string(), heap), None),
        );
        fields.insert(
            "port".to_owned(),
            Field::new(TypeCompiled::from_ty(&Ty::int(), heap), None),
        );
        ValueTyped::new_err(heap.alloc(RecordType::new(fields, TypeInstanceId::r#gen())))
            .expect("just allocated a RecordType")
    }

    fn ip_address_record_type<'v>(module: &Module<'v>) -> ValueTyped<'v, RecordType<'v>> {
        let typ = unnamed_ip_address_record_type(module.heap());
        let mut eval = Evaluator::new(module);
        typ.to_value()
            .export_as("IpAddress", &mut eval)
            .expect("assigning a name to a record type does not fail");
        typ
    }

    #[test]
    fn test_new_creates_record() {
        Module::with_temp_heap(|module| {
            let heap = module.heap();
            let typ = ip_address_record_type(&module);
            assert_eq!(2, typ.as_ref().len(), "`IpAddress` has two fields");
            assert!(!typ.as_ref().is_empty(), "`IpAddress` has fields");

            let record = heap.alloc(
                Record::new(typ, Box::new([heap.alloc("localhost"), heap.alloc(80)]))
                    .expect("values match the field types"),
            );
            assert_eq!(
                Some("localhost"),
                record
                    .get_attr("host", heap)
                    .expect("record attribute access does not fail")
                    .expect("`host` is a field of the record")
                    .unpack_str(),
                "constructed record should expose its fields"
            );

            let annotation = TypeCompiled::new(typ.to_value(), heap)
                .expect("an exported record type is a valid type annotation");
            assert!(
                annotation.matches(record),
                "constructed record should match its own type annotation"
            );
        });
    }

    #[test]
    fn test_new_rejects_wrong_number_of_values() {
        Module::with_temp_heap(|module| {
            let heap = module.heap();
            let typ = ip_address_record_type(&module);

            assert!(
                Record::new(typ, Box::new([heap.alloc("localhost")])).is_err(),
                "one value for two fields should be rejected"
            );
        });
    }

    #[test]
    fn test_new_rejects_ill_typed_values() {
        Module::with_temp_heap(|module| {
            let heap = module.heap();
            let typ = ip_address_record_type(&module);

            assert!(
                Record::new(typ, Box::new([heap.alloc(1), heap.alloc(80)])).is_err(),
                "an int is not a valid `host`"
            );
        });
    }

    #[test]
    fn test_new_rejects_unnamed_record_type() {
        Module::with_temp_heap(|module| {
            let heap = module.heap();
            let typ = unnamed_ip_address_record_type(heap);

            assert!(
                Record::new(typ, Box::new([heap.alloc("localhost"), heap.alloc(80)])).is_err(),
                "records of a type that was never assigned a name cannot be created"
            );
        });
    }

    #[test]
    fn test_new_with_frozen_record_type() {
        let frozen_module = Module::with_temp_heap(|module| {
            let typ = ip_address_record_type(&module);
            module.set("IpAddress", typ.to_value());
            module
                .freeze_named(StarlarkTestHeapName::frozen_heap_name())
                .expect("freezing a module holding a record type succeeds")
        });

        Module::with_temp_heap(|module| {
            let heap = module.heap();
            let typ = frozen_module
                .get_option_owned("IpAddress")
                .expect("symbol lookup does not fail")
                .expect("`IpAddress` was exported")
                .add_to_heap(heap);
            let typ = ValueTyped::<FrozenRecordType>::new_err(typ)
                .expect("the frozen module symbol is a frozen record type");

            let record = heap.alloc(
                Record::new(typ, Box::new([heap.alloc("localhost"), heap.alloc(80)]))
                    .expect("values match the field types"),
            );
            assert_eq!(
                Some(80),
                record
                    .get_attr("port", heap)
                    .expect("record attribute access does not fail")
                    .expect("`port` is a field of the record")
                    .unpack_i32(),
                "record built against a frozen record type should expose its fields"
            );
        });
    }

    #[test]
    fn test_new_requires_values_for_defaulted_fields() {
        Module::with_temp_heap(|module| {
            let heap = module.heap();
            let mut fields = SmallMap::new();
            fields.insert(
                "host".to_owned(),
                Field::new(TypeCompiled::from_ty(&Ty::string(), heap), None),
            );
            fields.insert(
                "port".to_owned(),
                Field::new(
                    TypeCompiled::from_ty(&Ty::int(), heap),
                    Some(heap.alloc(80)),
                ),
            );
            let typ = ValueTyped::<RecordType>::new_err(
                heap.alloc(RecordType::new(fields, TypeInstanceId::r#gen())),
            )
            .expect("just allocated a RecordType");
            let mut eval = Evaluator::new(&module);
            typ.to_value()
                .export_as("IpAddress", &mut eval)
                .expect("assigning a name to a record type does not fail");

            assert!(
                Record::new(typ, Box::new([heap.alloc("localhost")])).is_err(),
                "defaults are not applied, so a value for `port` is still required"
            );
            assert!(
                Record::new(typ, Box::new([heap.alloc("localhost"), heap.alloc(8080)])).is_ok(),
                "explicitly passing a value for the defaulted field works"
            );
        });
    }
}
