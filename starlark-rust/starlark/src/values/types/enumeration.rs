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

//! Fixed set enumerations, with runtime checking of validity.
//!
//! Calling `enum()` produces an [`EnumType`]. Calling the [`EnumType`] creates an [`EnumValue`].
//!
//! The implementation ensures that each value of the enumeration is only stored once,
//! so they may also provide (modest) memory savings. Created in starlark with the
//! `enum` function:
//!
//! ```
//! # starlark::assert::pass(r#"
//! Colors = enum("Red", "Green", "Blue")
//! val = Colors("Red")
//! assert_eq(val.value, "Red")
//! assert_eq(val.index, 0)
//! assert_eq(Colors[0], val)
//! assert_eq(Colors.type, "Colors")
//! assert_eq([v.value for v in Colors], ["Red", "Green", "Blue"])
//! # "#);
//! ```

use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;

use allocative::Allocative;
use derivative::Derivative;
use display_container::display_container;
use either::Either;
use serde::Serialize;
use starlark_derive::starlark_module;
use starlark_derive::NoSerialize;
use starlark_derive::StarlarkDocs;
use starlark_map::Equivalent;
use thiserror::Error;

use crate::any::ProvidesStaticType;
use crate::coerce::Coerce;
use crate::collections::SmallMap;
use crate::collections::StarlarkHasher;
use crate::environment::Methods;
use crate::environment::MethodsBuilder;
use crate::environment::MethodsStatic;
use crate::eval::Arguments;
use crate::eval::Evaluator;
use crate::values::function::FUNCTION_TYPE;
use crate::values::index::convert_index;
use crate::values::types::exported_name::ExportedName;
use crate::values::types::exported_name::FrozenExportedName;
use crate::values::types::exported_name::MutableExportedName;
use crate::values::Freeze;
use crate::values::FrozenValue;
use crate::values::Heap;
use crate::values::StarlarkValue;
use crate::values::Trace;
use crate::values::Value;
use crate::values::ValueLike;
use crate::{self as starlark};

#[derive(Error, Debug)]
enum EnumError {
    #[error("enum values must all be distinct, but repeated `{0}`")]
    DuplicateEnumValue(String),
    #[error("Unknown enum element `{0}`, given to `{1}`")]
    InvalidElement(String, String),
}

/// The type of an enumeration, created by `enum()`.
#[derive(
    Clone,
    Debug,
    Trace,
    Coerce,
    Freeze,
    NoSerialize,
    ProvidesStaticType,
    StarlarkDocs,
    Allocative
)]
#[starlark_docs(builtin = "extension")]
#[repr(C)]
// Deliberately store fully populated values
// for each entry, so we can produce enum values with zero allocation.
pub struct EnumTypeGen<V, Typ: ExportedName> {
    typ: Typ,
    // The key is the value of the enumeration
    // The value is a value of type EnumValue
    elements: SmallMap<V, V>,
}

impl<V: Display, Typ: ExportedName> Display for EnumTypeGen<V, Typ> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_container(f, "enum(", ")", self.elements.iter().map(|(k, _v)| k))
    }
}

/// Unfrozen enum type.
pub type EnumType<'v> = EnumTypeGen<Value<'v>, MutableExportedName>;
/// Frozen enum type.
pub type FrozenEnumType = EnumTypeGen<FrozenValue, FrozenExportedName>;

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
    typ: V, // Must be EnumType it points back to (so it can get the type)
    value: V,   // The value of this enumeration
    index: i32, // The index in the enumeration
}

impl<V: Display> Display for EnumValueGen<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.fmt(f)
    }
}

starlark_complex_values!(EnumType);
starlark_complex_value!(pub EnumValue);

impl<'v> EnumType<'v> {
    pub(crate) fn new(elements: Vec<Value<'v>>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        // We are constructing the enum and all elements in one go.
        // They both point at each other, which adds to the complexity.
        let typ = heap.alloc(EnumType {
            typ: MutableExportedName::default(),
            elements: SmallMap::new(),
        });

        let mut res = SmallMap::with_capacity(elements.len());
        for (i, x) in elements.iter().enumerate() {
            let v = heap.alloc(EnumValue {
                typ,
                index: i as i32,
                value: *x,
            });
            if res.insert_hashed(x.get_hashed()?, v).is_some() {
                return Err(EnumError::DuplicateEnumValue(x.to_string()).into());
            }
        }

        // Here we tie the cycle
        let t = typ.downcast_ref::<EnumType>().unwrap();
        #[allow(clippy::cast_ref_to_mut)]
        unsafe {
            // To tie the cycle we can either have an UnsafeCell or similar, or just mutate in place.
            // Since we only do the tie once, better to do the mutate in place.
            // Safe because we know no one else has a copy of this reference at this point.
            *(&t.elements as *const SmallMap<Value<'v>, Value<'v>>
                as *mut SmallMap<Value<'v>, Value<'v>>) = res;
        }
        Ok(typ)
    }
}

impl<'v, V: ValueLike<'v>> EnumValueGen<V> {
    /// The result of calling `type()` on an enum value.
    pub const TYPE: &'static str = "enum";

    fn get_enum_type(&self) -> Either<&'v EnumType<'v>, &'v FrozenEnumType> {
        // Safe to unwrap because we always ensure typ is EnumType
        EnumType::from_value(self.typ.to_value()).unwrap()
    }
}

impl<'v, Typ, V> EnumTypeGen<V, Typ>
where
    Value<'v>: Equivalent<V>,
    Typ: ExportedName,
    V: ValueLike<'v> + 'v,
{
    pub(crate) fn construct(&self, val: Value<'v>) -> anyhow::Result<V> {
        match self.elements.get_hashed_by_value(val.get_hashed()?) {
            Some(v) => Ok(*v),
            None => Err(EnumError::InvalidElement(val.to_str(), self.to_string()).into()),
        }
    }
}

impl<'v, Typ: Allocative + 'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for EnumTypeGen<V, Typ>
where
    Self: ProvidesStaticType,
    Typ: ExportedName,
    Value<'v>: Equivalent<V>,
{
    starlark_type!(FUNCTION_TYPE);

    fn invoke(
        &self,
        _me: Value<'v>,
        args: &Arguments<'v, '_>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        args.no_named_args()?;
        let val = args.positional1(eval.heap())?;
        Ok(self.construct(val)?.to_value())
    }

    fn length(&self) -> anyhow::Result<i32> {
        Ok(self.elements.len() as i32)
    }

    fn at(&self, index: Value, _heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let i = convert_index(index, self.elements.len() as i32)? as usize;
        // Must be in the valid range since convert_index checks that, so just unwrap
        Ok(self.elements.get_index(i).map(|x| *x.1).unwrap().to_value())
    }

    fn iterate<'a>(
        &'a self,
        _heap: &'v Heap,
    ) -> anyhow::Result<Box<dyn Iterator<Item = Value<'v>> + 'a>>
    where
        'v: 'a,
    {
        Ok(Box::new(self.elements.values().map(|x| x.to_value())))
    }

    fn with_iterator(
        &self,
        _heap: &'v Heap,
        f: &mut dyn FnMut(&mut dyn Iterator<Item = Value<'v>>) -> anyhow::Result<()>,
    ) -> anyhow::Result<()> {
        f(&mut self.elements.values().map(|x| x.to_value()))
    }

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(enum_type_methods)
    }

    fn equals(&self, other: Value<'v>) -> anyhow::Result<bool> {
        fn eq<'v>(
            a: &EnumTypeGen<impl ValueLike<'v>, impl ExportedName>,
            b: &EnumTypeGen<impl ValueLike<'v>, impl ExportedName>,
        ) -> anyhow::Result<bool> {
            if a.typ.borrow() != b.typ.borrow() {
                return Ok(false);
            }
            if a.elements.len() != b.elements.len() {
                return Ok(false);
            }
            for (k1, k2) in a.elements.keys().zip(b.elements.keys()) {
                if !k1.to_value().equals(k2.to_value())? {
                    return Ok(false);
                }
            }
            Ok(true)
        }

        match EnumType::from_value(other) {
            Some(Either::Left(other)) => eq(self, other),
            Some(Either::Right(other)) => eq(self, other),
            _ => Ok(false),
        }
    }

    fn export_as(&self, variable_name: &str, _eval: &mut Evaluator<'v, '_>) {
        self.typ.try_export_as(variable_name);
    }
}

#[starlark_module]
fn enum_type_methods(builder: &mut MethodsBuilder) {
    #[starlark(attribute)]
    fn r#type<'v>(this: Value, heap: &Heap) -> anyhow::Result<Value<'v>> {
        let this = EnumType::from_value(this).unwrap();
        let typ = match this {
            Either::Left(x) => x.typ.borrow(),
            Either::Right(x) => x.typ.borrow(),
        };
        Ok(heap.alloc(typ.as_ref().map_or(EnumValue::TYPE, |n| n.as_str())))
    }

    fn values<'v>(this: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let this = EnumType::from_value(this).unwrap();
        match this {
            Either::Left(x) => Ok(heap.alloc_list_iter(x.elements.keys().copied())),
            Either::Right(x) => Ok(heap.alloc_list_iter(x.elements.keys().map(|x| x.to_value()))),
        }
    }
}

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for EnumValueGen<V>
where
    Self: ProvidesStaticType,
{
    starlark_type!(EnumValue::TYPE);

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

    fn get_attr(&self, attribute: &str, _heap: &'v Heap) -> Option<Value<'v>> {
        match attribute {
            "index" => Some(Value::new_int(self.index)),
            "value" => Some(self.value.to_value()),
            _ => None,
        }
    }

    fn dir_attr(&self) -> Vec<String> {
        vec!["index".to_owned(), "value".to_owned()]
    }
}

impl<'v, V: ValueLike<'v>> Serialize for EnumValueGen<V> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.value.serialize(serializer)
    }
}
