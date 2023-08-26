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

use std::cell::UnsafeCell;
use std::fmt;
use std::fmt::Display;

use allocative::Allocative;
use display_container::fmt_container;
use either::Either;
use starlark_derive::starlark_module;
use starlark_derive::starlark_value;
use starlark_derive::Coerce;
use starlark_derive::Freeze;
use starlark_derive::NoSerialize;
use starlark_derive::StarlarkDocs;
use starlark_derive::Trace;
use starlark_map::small_map::SmallMap;
use starlark_map::Equivalent;

use crate as starlark;
use crate::any::ProvidesStaticType;
use crate::environment::Methods;
use crate::environment::MethodsBuilder;
use crate::environment::MethodsStatic;
use crate::eval::Arguments;
use crate::eval::Evaluator;
use crate::values::enumeration::EnumValue;
use crate::values::exported_name::ExportedName;
use crate::values::exported_name::FrozenExportedName;
use crate::values::exported_name::MutableExportedName;
use crate::values::function::FUNCTION_TYPE;
use crate::values::index::convert_index;
use crate::values::FrozenValue;
use crate::values::Heap;
use crate::values::StarlarkValue;
use crate::values::Value;
use crate::values::ValueLike;

#[derive(thiserror::Error, Debug)]
enum EnumError {
    #[error("enum values must all be distinct, but repeated `{0}`")]
    DuplicateEnumValue(String),
    #[error("Unknown enum element `{0}`, given to `{1}`")]
    InvalidElement(String, String),
}

/// The type of an enumeration, created by `enum()`.
#[derive(
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
    pub(crate) typ: Typ,
    // The key is the value of the enumeration
    // The value is a value of type EnumValue
    #[allocative(skip)] // TODO(nga): do not skip.
    elements: UnsafeCell<SmallMap<V, V>>,
}

unsafe impl<V: Send, Typ: ExportedName + Send> Send for EnumTypeGen<V, Typ> {}
unsafe impl<V: Sync, Typ: ExportedName + Sync> Sync for EnumTypeGen<V, Typ> {}

impl<V: Display, Typ: ExportedName> Display for EnumTypeGen<V, Typ> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt_container(f, "enum(", ")", self.elements().iter().map(|(k, _v)| k))
    }
}

/// Unfrozen enum type.
pub type EnumType<'v> = EnumTypeGen<Value<'v>, MutableExportedName>;
/// Frozen enum type.
pub type FrozenEnumType = EnumTypeGen<FrozenValue, FrozenExportedName>;

impl<'v> EnumType<'v> {
    pub(crate) fn new(elements: Vec<Value<'v>>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        // We are constructing the enum and all elements in one go.
        // They both point at each other, which adds to the complexity.
        let typ = heap.alloc(EnumType {
            typ: MutableExportedName::default(),
            elements: UnsafeCell::new(SmallMap::new()),
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
        unsafe {
            // SAFETY: we own unique reference to `t`.
            *t.elements.get() = res;
        }
        Ok(typ)
    }
}

impl<V, Typ: ExportedName> EnumTypeGen<V, Typ> {
    pub(crate) fn elements(&self) -> &SmallMap<V, V> {
        // Safe because we never mutate the elements after construction.
        unsafe { &*self.elements.get() }
    }
}

impl<'v, Typ, V> EnumTypeGen<V, Typ>
where
    Value<'v>: Equivalent<V>,
    Typ: ExportedName,
    V: ValueLike<'v> + 'v,
{
    pub(crate) fn construct(&self, val: Value<'v>) -> anyhow::Result<V> {
        match self.elements().get_hashed_by_value(val.get_hashed()?) {
            Some(v) => Ok(*v),
            None => Err(EnumError::InvalidElement(val.to_str(), self.to_string()).into()),
        }
    }
}

#[starlark_value(type = FUNCTION_TYPE)]
impl<'v, Typ: Allocative + 'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for EnumTypeGen<V, Typ>
where
    Self: ProvidesStaticType<'v>,
    Typ: ExportedName,
    Value<'v>: Equivalent<V>,
{
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
        Ok(self.elements().len() as i32)
    }

    fn at(&self, index: Value, _heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let i = convert_index(index, self.elements().len() as i32)? as usize;
        // Must be in the valid range since convert_index checks that, so just unwrap
        Ok(self
            .elements()
            .get_index(i)
            .map(|x| *x.1)
            .unwrap()
            .to_value())
    }

    unsafe fn iterate(&self, me: Value<'v>, _heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        Ok(me)
    }

    unsafe fn iter_size_hint(&self, index: usize) -> (usize, Option<usize>) {
        debug_assert!(index <= self.elements().len());
        let rem = self.elements().len() - index;
        (rem, Some(rem))
    }

    unsafe fn iter_next(&self, index: usize, _heap: &'v Heap) -> Option<Value<'v>> {
        self.elements().values().nth(index).map(|v| v.to_value())
    }

    unsafe fn iter_stop(&self) {}

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(enum_type_methods)
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
            Either::Left(x) => Ok(heap.alloc_list_iter(x.elements().keys().copied())),
            Either::Right(x) => Ok(heap.alloc_list_iter(x.elements().keys().map(|x| x.to_value()))),
        }
    }
}
