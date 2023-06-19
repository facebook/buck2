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
use std::fmt::Formatter;
use std::hash::Hash;
use std::hash::Hasher;

use allocative::Allocative;
use anyhow::Context;
use dupe::Dupe;
use starlark_derive::starlark_module;
use starlark_map::StarlarkHasher;
use thiserror::Error;

use crate as starlark;
use crate::any::ProvidesStaticType;
use crate::coerce::Coerce;
use crate::environment::GlobalsBuilder;
use crate::environment::Methods;
use crate::environment::MethodsBuilder;
use crate::environment::MethodsStatic;
use crate::private::Private;
use crate::slice_vec_ext::SliceExt;
use crate::starlark_type;
use crate::values::dict::Dict;
use crate::values::dict::DictRef;
use crate::values::layout::avalue::alloc_static;
use crate::values::layout::avalue::AValueImpl;
use crate::values::layout::avalue::Basic;
use crate::values::layout::heap::repr::AValueRepr;
use crate::values::list::ListRef;
use crate::values::none::NoneType;
use crate::values::type_repr::StarlarkTypeRepr;
use crate::values::types::tuple::value::Tuple;
use crate::values::types::tuple::value::TupleGen;
use crate::values::AllocValue;
use crate::values::Demand;
use crate::values::Freeze;
use crate::values::FrozenHeap;
use crate::values::FrozenValue;
use crate::values::Heap;
use crate::values::NoSerialize;
use crate::values::StarlarkValue;
use crate::values::Trace;
use crate::values::Value;
use crate::values::ValueLike;

#[derive(Debug, Error)]
enum TypingError {
    /// The value does not have the specified type
    #[error("Value `{0}` of type `{1}` does not match the type annotation `{2}` for {3}")]
    TypeAnnotationMismatch(String, String, String, String),
    /// The given type annotation does not represent a type
    #[error("Type `{0}` is not a valid type annotation")]
    InvalidTypeAnnotation(String),
    /// The given type annotation does not exist, but the user might have forgotten quotes around
    /// it
    #[error(r#"Found `{0}` instead of a valid type annotation. Perhaps you meant `"{1}"`?"#)]
    PerhapsYouMeant(String, String),
    #[error("Value of type `{1}` does not match type `{2}`: {0}")]
    ValueDoesNotMatchType(String, &'static str, String),
}

trait TypeCompiledImpl<'v>: Allocative + Display + Debug + 'v {
    fn matches(&self, value: Value<'v>) -> bool;
    fn is_wildcard(&self) -> bool {
        false
    }
    fn to_frozen(&self, heap: &FrozenHeap) -> TypeCompiled<FrozenValue>;
}

unsafe impl<'v> ProvidesStaticType<'v> for &'v dyn TypeCompiledImpl<'v> {
    type StaticType = &'static dyn TypeCompiledImpl<'static>;
}

#[derive(Debug, Trace, Freeze, Allocative, ProvidesStaticType, NoSerialize)]
struct TypeCompiledImplAsStarlarkValue<T>(T);

impl<T> TypeCompiledImplAsStarlarkValue<T>
where
    TypeCompiledImplAsStarlarkValue<T>: StarlarkValue<'static>,
{
    const fn alloc_static(imp: T) -> AValueRepr<AValueImpl<Basic, Self>> {
        alloc_static(Basic, TypeCompiledImplAsStarlarkValue(imp))
    }
}

impl<'v, T> StarlarkValue<'v> for TypeCompiledImplAsStarlarkValue<T>
where
    T: TypeCompiledImpl<'v> + Hash + Eq,
    Self: ProvidesStaticType<'v>,
{
    starlark_type!("eval_type");

    fn type_matches_value(&self, value: Value<'v>, _private: Private) -> bool {
        self.0.matches(value)
    }

    fn provide(&'v self, demand: &mut Demand<'_, 'v>) {
        demand.provide_value::<&'v dyn TypeCompiledImpl<'v>>(&self.0);
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        Hash::hash(&self.0, hasher);
        Ok(())
    }

    fn equals(&self, other: Value<'v>) -> anyhow::Result<bool> {
        let Some(other) = other.downcast_ref::<Self>() else {
            return Ok(false);
        };
        Ok(self.0 == other.0)
    }

    fn get_methods() -> Option<&'static Methods>
    where
        Self: Sized,
    {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(type_compiled_methods)
    }
}

impl<'v, T: TypeCompiledImpl<'v>> Display for TypeCompiledImplAsStarlarkValue<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "eval_type({})", self.0)
    }
}

#[starlark_module]
fn type_compiled_methods(methods: &mut MethodsBuilder) {
    /// True iff the value matches this type.
    fn matches<'v>(this: Value<'v>, value: Value<'v>) -> anyhow::Result<bool> {
        Ok(this.get_ref().type_matches_value(value))
    }

    /// Error if the value does not match this type.
    fn check_matches<'v>(this: Value<'v>, value: Value<'v>) -> anyhow::Result<NoneType> {
        if !this.get_ref().type_matches_value(value) {
            return Err(TypingError::ValueDoesNotMatchType(
                value.to_repr(),
                value.get_type(),
                TypeCompiled(this).to_string(),
            )
            .into());
        }
        Ok(NoneType)
    }
}

#[starlark_module]
pub(crate) fn register_eval_type(globals: &mut GlobalsBuilder) {
    /// Create a runtime type object which can be used to check if a value matches the given type.
    fn eval_type<'v>(
        #[starlark(require = pos)] ty: Value<'v>,
        heap: &'v Heap,
    ) -> anyhow::Result<TypeCompiled<Value<'v>>> {
        TypeCompiled::new(ty, heap)
    }
}

#[derive(
    Debug,
    Allocative,
    Freeze,
    Trace,
    Clone,
    Copy,
    Dupe,
    Coerce,
    ProvidesStaticType
)]
#[repr(transparent)]
pub(crate) struct TypeCompiled<V>(
    /// `V` is a starlark value which implements `type_matches_value`.
    /// Such values are not visible to the user.
    V,
);

impl<'v, V: ValueLike<'v>> Display for TypeCompiled<V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.downcast() {
            Ok(t) => Display::fmt(t, f),
            Err(_) => {
                // This is unreachable, but we should not panic in `Display`.
                Display::fmt(&self.0, f)
            }
        }
    }
}

impl<V> StarlarkTypeRepr for TypeCompiled<V> {
    fn starlark_type_repr() -> String {
        "eval_type.type".to_owned()
    }
}

impl<'v, V: ValueLike<'v>> AllocValue<'v> for TypeCompiled<V> {
    fn alloc_value(self, _heap: &'v Heap) -> Value<'v> {
        self.0.to_value()
    }
}

impl<'v, V: ValueLike<'v>> TypeCompiled<V> {
    fn downcast(self) -> anyhow::Result<&'v dyn TypeCompiledImpl<'v>> {
        self.to_value()
            .0
            .request_value::<&dyn TypeCompiledImpl>()
            .context("Not TypeCompiledImpl (internal error)")
    }

    pub(crate) fn matches(&self, value: Value<'v>) -> bool {
        self.0.to_value().get_ref().type_matches_value(value)
    }

    pub(crate) fn type_is_wildcard(self) -> bool {
        self.downcast().unwrap().is_wildcard()
    }

    #[cold]
    #[inline(never)]
    fn check_type_error(self, value: Value<'v>, arg_name: Option<&str>) -> anyhow::Result<()> {
        Err(TypingError::TypeAnnotationMismatch(
            value.to_str(),
            value.get_type().to_owned(),
            self.to_string(),
            match arg_name {
                None => "return type".to_owned(),
                Some(x) => format!("argument `{}`", x),
            },
        )
        .into())
    }

    pub(crate) fn check_type(self, value: Value<'v>, arg_name: Option<&str>) -> anyhow::Result<()> {
        if self.matches(value) {
            Ok(())
        } else {
            self.check_type_error(value, arg_name)
        }
    }

    pub(crate) fn to_value(self) -> TypeCompiled<Value<'v>> {
        TypeCompiled(self.0.to_value())
    }

    pub(crate) fn write_hash(self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        self.to_value().0.write_hash(hasher)
    }

    pub(crate) fn equals(self, other: Self) -> anyhow::Result<bool> {
        self.to_value().0.equals(other.to_value().0)
    }
}

impl<'v, V: ValueLike<'v>> Hash for TypeCompiled<V> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self.0.to_value().get_hash() {
            Ok(h) => h.hash(state),
            Err(_) => {
                // Unreachable, but we should not panic in `Hash`.
            }
        }
    }
}

impl<'v, V: ValueLike<'v>> PartialEq for TypeCompiled<V> {
    #[allow(clippy::manual_unwrap_or)]
    fn eq(&self, other: &Self) -> bool {
        match self.0.to_value().equals(other.0.to_value()) {
            Ok(b) => b,
            Err(_) => {
                // Unreachable, but we should not panic in `PartialEq`.
                false
            }
        }
    }
}

impl<'v, V: ValueLike<'v>> Eq for TypeCompiled<V> {}

impl<'v, V: ValueLike<'v>> TypeCompiled<V> {
    fn type_anything() -> TypeCompiled<V> {
        #[derive(
            Eq,
            PartialEq,
            Hash,
            Allocative,
            Debug,
            derive_more::Display,
            ProvidesStaticType
        )]
        #[display(fmt = "\"\"")]
        struct Anything;

        impl<'v> TypeCompiledImpl<'v> for Anything {
            fn matches(&self, _value: Value<'v>) -> bool {
                true
            }

            fn is_wildcard(&self) -> bool {
                true
            }

            fn to_frozen(&self, _heap: &FrozenHeap) -> TypeCompiled<FrozenValue> {
                TypeCompiled::type_anything()
            }
        }

        static ANYTHING: AValueRepr<AValueImpl<Basic, TypeCompiledImplAsStarlarkValue<Anything>>> =
            TypeCompiledImplAsStarlarkValue::alloc_static(Anything);

        TypeCompiled(V::from_frozen_value(FrozenValue::new_repr(&ANYTHING)))
    }

    fn type_none() -> TypeCompiled<V> {
        #[derive(
            Eq,
            PartialEq,
            Hash,
            Allocative,
            Debug,
            derive_more::Display,
            ProvidesStaticType
        )]
        #[display(fmt = "None")]
        struct IsNone;

        impl<'v> TypeCompiledImpl<'v> for IsNone {
            fn matches(&self, value: Value<'v>) -> bool {
                value.is_none()
            }

            fn to_frozen(&self, _heap: &FrozenHeap) -> TypeCompiled<FrozenValue> {
                TypeCompiled::type_none()
            }
        }

        static IS_NONE: AValueRepr<AValueImpl<Basic, TypeCompiledImplAsStarlarkValue<IsNone>>> =
            TypeCompiledImplAsStarlarkValue::alloc_static(IsNone);

        TypeCompiled(V::from_frozen_value(FrozenValue::new_repr(&IS_NONE)))
    }

    fn type_string() -> TypeCompiled<V> {
        #[derive(
            Eq,
            PartialEq,
            Hash,
            Allocative,
            Debug,
            derive_more::Display,
            ProvidesStaticType
        )]
        #[display(fmt = "str.type")]
        struct IsString;

        impl<'v> TypeCompiledImpl<'v> for IsString {
            fn matches(&self, value: Value<'v>) -> bool {
                value.unpack_str().is_some() || value.get_ref().matches_type("string")
            }

            fn to_frozen(&self, _heap: &FrozenHeap) -> TypeCompiled<FrozenValue> {
                TypeCompiled::type_string()
            }
        }

        static IS_STRING: AValueRepr<AValueImpl<Basic, TypeCompiledImplAsStarlarkValue<IsString>>> =
            TypeCompiledImplAsStarlarkValue::alloc_static(IsString);

        TypeCompiled(V::from_frozen_value(FrozenValue::new_repr(&IS_STRING)))
    }

    fn type_int() -> TypeCompiled<V> {
        #[derive(
            Eq,
            PartialEq,
            Hash,
            Allocative,
            Debug,
            derive_more::Display,
            ProvidesStaticType
        )]
        #[display(fmt = "int.type")]
        struct IsInt;

        impl<'v> TypeCompiledImpl<'v> for IsInt {
            fn matches(&self, value: Value<'v>) -> bool {
                value.unpack_inline_int().is_some() || value.get_ref().matches_type("int")
            }

            fn to_frozen(&self, _heap: &FrozenHeap) -> TypeCompiled<FrozenValue> {
                TypeCompiled::type_int()
            }
        }

        static IS_INT: AValueRepr<AValueImpl<Basic, TypeCompiledImplAsStarlarkValue<IsInt>>> =
            TypeCompiledImplAsStarlarkValue::alloc_static(IsInt);

        TypeCompiled(V::from_frozen_value(FrozenValue::new_repr(&IS_INT)))
    }

    fn type_bool() -> TypeCompiled<V> {
        #[derive(
            Eq,
            PartialEq,
            Hash,
            Allocative,
            Debug,
            derive_more::Display,
            ProvidesStaticType
        )]
        #[display(fmt = "bool.type")]
        struct IsBool;

        impl<'v> TypeCompiledImpl<'v> for IsBool {
            fn matches(&self, value: Value<'v>) -> bool {
                value.unpack_bool().is_some() || value.get_ref().matches_type("bool")
            }

            fn to_frozen(&self, _heap: &FrozenHeap) -> TypeCompiled<FrozenValue> {
                TypeCompiled::type_bool()
            }
        }

        static IS_BOOL: AValueRepr<AValueImpl<Basic, TypeCompiledImplAsStarlarkValue<IsBool>>> =
            TypeCompiledImplAsStarlarkValue::alloc_static(IsBool);

        TypeCompiled(V::from_frozen_value(FrozenValue::new_repr(&IS_BOOL)))
    }

    fn type_list() -> TypeCompiled<V> {
        #[derive(
            Eq,
            PartialEq,
            Hash,
            Allocative,
            Debug,
            derive_more::Display,
            ProvidesStaticType
        )]
        #[display(fmt = "[\"\"]")]
        struct IsList;

        impl<'v> TypeCompiledImpl<'v> for IsList {
            fn matches(&self, value: Value<'v>) -> bool {
                ListRef::from_value(value).is_some()
            }

            fn to_frozen(&self, _heap: &FrozenHeap) -> TypeCompiled<FrozenValue> {
                TypeCompiled::type_list()
            }
        }

        static IS_LIST: AValueRepr<AValueImpl<Basic, TypeCompiledImplAsStarlarkValue<IsList>>> =
            TypeCompiledImplAsStarlarkValue::alloc_static(IsList);

        TypeCompiled(V::from_frozen_value(FrozenValue::new_repr(&IS_LIST)))
    }

    fn type_dict() -> TypeCompiled<V> {
        #[derive(
            Eq,
            PartialEq,
            Hash,
            Allocative,
            Debug,
            derive_more::Display,
            ProvidesStaticType
        )]
        #[display(fmt = "{{\"\": \"\"}}")]
        struct IsDict;

        impl<'v> TypeCompiledImpl<'v> for IsDict {
            fn matches(&self, value: Value<'v>) -> bool {
                DictRef::from_value(value).is_some()
            }

            fn to_frozen(&self, _heap: &FrozenHeap) -> TypeCompiled<FrozenValue> {
                TypeCompiled::type_dict()
            }
        }

        static IS_DICT: AValueRepr<AValueImpl<Basic, TypeCompiledImplAsStarlarkValue<IsDict>>> =
            TypeCompiledImplAsStarlarkValue::alloc_static(IsDict);

        TypeCompiled(V::from_frozen_value(FrozenValue::new_repr(&IS_DICT)))
    }

    pub(crate) fn to_frozen(self, heap: &FrozenHeap) -> TypeCompiled<FrozenValue> {
        if let Some(v) = self.0.to_value().unpack_frozen() {
            TypeCompiled(v)
        } else {
            self.to_value().downcast().unwrap().to_frozen(heap)
        }
    }
}

// These functions are small, but are deliberately out-of-line so we get better
// information in profiling about the origin of these closures
impl<'v> TypeCompiled<Value<'v>> {
    fn type_concrete(t: &str, heap: &'v Heap) -> TypeCompiled<Value<'v>> {
        #[derive(
            Eq,
            PartialEq,
            Hash,
            Allocative,
            Debug,
            derive_more::Display,
            Trace,
            Freeze,
            ProvidesStaticType,
            Clone
        )]
        #[display(fmt = "\"{}\"", _0)]
        struct IsConcrete(String);

        impl<'v> TypeCompiledImpl<'v> for IsConcrete {
            fn matches(&self, value: Value<'v>) -> bool {
                value.get_ref().matches_type(&self.0)
            }

            fn to_frozen(&self, heap: &FrozenHeap) -> TypeCompiled<FrozenValue> {
                TypeCompiled(
                    heap.alloc_simple(TypeCompiledImplAsStarlarkValue(IsConcrete::clone(self))),
                )
            }
        }

        TypeCompiled(heap.alloc_simple(TypeCompiledImplAsStarlarkValue(IsConcrete(t.to_owned()))))
    }

    pub(crate) fn type_list_of(
        t: TypeCompiled<Value<'v>>,
        heap: &'v Heap,
    ) -> TypeCompiled<Value<'v>> {
        #[derive(
            Allocative,
            Debug,
            derive_more::Display,
            Trace,
            Freeze,
            ProvidesStaticType
        )]
        #[display(fmt = "[{}]", _0)]
        struct IsListOf<V>(TypeCompiled<V>);

        impl<V> PartialEq for IsListOf<V>
        where
            TypeCompiled<V>: PartialEq,
        {
            fn eq(&self, other: &Self) -> bool {
                self.0.eq(&other.0)
            }
        }

        impl<V> Eq for IsListOf<V> where TypeCompiled<V>: Eq {}

        impl<V> Hash for IsListOf<V>
        where
            TypeCompiled<V>: Hash,
        {
            fn hash<H: Hasher>(&self, state: &mut H) {
                self.0.hash(state)
            }
        }

        impl<'v, V: ValueLike<'v>> TypeCompiledImpl<'v> for IsListOf<V>
        where
            Self: ProvidesStaticType<'v>,
        {
            fn matches(&self, value: Value<'v>) -> bool {
                match ListRef::from_value(value) {
                    None => false,
                    Some(list) => list.iter().all(|v| self.0.matches(v)),
                }
            }

            fn to_frozen(&self, heap: &FrozenHeap) -> TypeCompiled<FrozenValue> {
                TypeCompiled(heap.alloc_simple(TypeCompiledImplAsStarlarkValue(IsListOf(
                    self.0.to_frozen(heap),
                ))))
            }
        }

        TypeCompiled(heap.alloc_complex(TypeCompiledImplAsStarlarkValue(IsListOf(t))))
    }

    fn type_any_of_two(
        t1: TypeCompiled<Value<'v>>,
        t2: TypeCompiled<Value<'v>>,
        heap: &'v Heap,
    ) -> TypeCompiled<Value<'v>> {
        #[derive(
            Allocative,
            Debug,
            derive_more::Display,
            Trace,
            Freeze,
            ProvidesStaticType
        )]
        #[display(fmt = "[{}, {}]", _0, _1)]
        struct IsAnyOfTwo<V>(TypeCompiled<V>, TypeCompiled<V>);

        impl<V> Hash for IsAnyOfTwo<V>
        where
            TypeCompiled<V>: Hash,
        {
            fn hash<H: Hasher>(&self, state: &mut H) {
                self.0.hash(state);
                self.1.hash(state);
            }
        }

        impl<V> PartialEq for IsAnyOfTwo<V>
        where
            TypeCompiled<V>: PartialEq,
        {
            fn eq(&self, other: &Self) -> bool {
                self.0.eq(&other.0) && self.1.eq(&other.1)
            }
        }

        impl<V> Eq for IsAnyOfTwo<V> where TypeCompiled<V>: Eq {}

        impl<'v, V: ValueLike<'v>> TypeCompiledImpl<'v> for IsAnyOfTwo<V>
        where
            Self: ProvidesStaticType<'v>,
        {
            fn matches(&self, value: Value<'v>) -> bool {
                self.0.matches(value) || self.1.matches(value)
            }

            fn to_frozen(&self, heap: &FrozenHeap) -> TypeCompiled<FrozenValue> {
                TypeCompiled(
                    heap.alloc_simple(TypeCompiledImplAsStarlarkValue(IsAnyOfTwo(
                        self.0.to_frozen(heap),
                        self.1.to_frozen(heap),
                    ))),
                )
            }
        }

        TypeCompiled(heap.alloc_complex(TypeCompiledImplAsStarlarkValue(IsAnyOfTwo(t1, t2))))
    }

    pub(crate) fn type_any_of(
        ts: Vec<TypeCompiled<Value<'v>>>,
        heap: &'v Heap,
    ) -> TypeCompiled<Value<'v>> {
        #[derive(Allocative, Debug, Trace, Freeze, ProvidesStaticType)]
        struct IsAnyOf<V>(Vec<TypeCompiled<V>>);

        impl<V> Hash for IsAnyOf<V>
        where
            TypeCompiled<V>: Hash,
        {
            fn hash<H: Hasher>(&self, state: &mut H) {
                self.0.hash(state);
            }
        }

        impl<V> PartialEq for IsAnyOf<V>
        where
            TypeCompiled<V>: PartialEq,
        {
            fn eq(&self, other: &Self) -> bool {
                self.0.eq(&other.0)
            }
        }

        impl<V> Eq for IsAnyOf<V> where TypeCompiled<V>: Eq {}

        impl<'v, V: ValueLike<'v>> Display for IsAnyOf<V> {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                display_container::fmt_container(f, "[", "]", &self.0)
            }
        }

        impl<'v, V: ValueLike<'v>> TypeCompiledImpl<'v> for IsAnyOf<V>
        where
            Self: ProvidesStaticType<'v>,
        {
            fn matches(&self, value: Value<'v>) -> bool {
                self.0.iter().any(|t| t.matches(value))
            }

            fn to_frozen(&self, heap: &FrozenHeap) -> TypeCompiled<FrozenValue> {
                TypeCompiled(heap.alloc_simple(TypeCompiledImplAsStarlarkValue(IsAnyOf(
                    self.0.iter().map(|t| t.to_frozen(heap)).collect(),
                ))))
            }
        }

        TypeCompiled(heap.alloc_complex(TypeCompiledImplAsStarlarkValue(IsAnyOf(ts))))
    }

    pub(crate) fn type_dict_of(
        kt: TypeCompiled<Value<'v>>,
        vt: TypeCompiled<Value<'v>>,
        heap: &'v Heap,
    ) -> TypeCompiled<Value<'v>> {
        #[derive(
            Allocative,
            Debug,
            derive_more::Display,
            Trace,
            Freeze,
            ProvidesStaticType
        )]
        #[display(fmt = "{{{}: {}}}", _0, _1)]
        struct IsDictOf<V>(TypeCompiled<V>, TypeCompiled<V>);

        impl<V> Hash for IsDictOf<V>
        where
            TypeCompiled<V>: Hash,
        {
            fn hash<H: Hasher>(&self, state: &mut H) {
                self.0.hash(state);
                self.1.hash(state);
            }
        }

        impl<V> PartialEq for IsDictOf<V>
        where
            TypeCompiled<V>: PartialEq,
        {
            fn eq(&self, other: &Self) -> bool {
                self.0.eq(&other.0) && self.1.eq(&other.1)
            }
        }

        impl<V> Eq for IsDictOf<V> where TypeCompiled<V>: Eq {}

        impl<'v, V: ValueLike<'v>> TypeCompiledImpl<'v> for IsDictOf<V>
        where
            Self: ProvidesStaticType<'v>,
        {
            fn matches(&self, value: Value<'v>) -> bool {
                match DictRef::from_value(value) {
                    None => false,
                    Some(dict) => dict
                        .iter()
                        .all(|(k, v)| self.0.matches(k) && self.1.matches(v)),
                }
            }

            fn to_frozen(&self, heap: &FrozenHeap) -> TypeCompiled<FrozenValue> {
                TypeCompiled(heap.alloc_simple(TypeCompiledImplAsStarlarkValue(IsDictOf(
                    self.0.to_frozen(heap),
                    self.1.to_frozen(heap),
                ))))
            }
        }

        TypeCompiled(heap.alloc_complex(TypeCompiledImplAsStarlarkValue(IsDictOf(kt, vt))))
    }

    pub(crate) fn type_tuple_of(
        ts: Vec<TypeCompiled<Value<'v>>>,
        heap: &'v Heap,
    ) -> TypeCompiled<Value<'v>> {
        #[derive(Allocative, Debug, Trace, Freeze, ProvidesStaticType)]
        struct IsTupleOf<V>(Vec<TypeCompiled<V>>);

        impl<V> Hash for IsTupleOf<V>
        where
            TypeCompiled<V>: Hash,
        {
            fn hash<H: Hasher>(&self, state: &mut H) {
                self.0.hash(state);
            }
        }

        impl<V> PartialEq for IsTupleOf<V>
        where
            TypeCompiled<V>: PartialEq,
        {
            fn eq(&self, other: &Self) -> bool {
                self.0.eq(&other.0)
            }
        }

        impl<V> Eq for IsTupleOf<V> where TypeCompiled<V>: Eq {}

        impl<'v, V: ValueLike<'v>> Display for IsTupleOf<V> {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                if self.0.len() == 1 {
                    display_container::fmt_container(f, "(", ",)", [&self.0[0]])
                } else {
                    display_container::fmt_container(f, "(", ")", &self.0)
                }
            }
        }

        impl<'v, V: ValueLike<'v>> TypeCompiledImpl<'v> for IsTupleOf<V>
        where
            Self: ProvidesStaticType<'v>,
        {
            fn matches(&self, value: Value<'v>) -> bool {
                match Tuple::from_value(value) {
                    Some(v) if v.len() == self.0.len() => {
                        v.iter().zip(self.0.iter()).all(|(v, t)| t.matches(v))
                    }
                    _ => false,
                }
            }

            fn to_frozen(&self, heap: &FrozenHeap) -> TypeCompiled<FrozenValue> {
                TypeCompiled(heap.alloc_simple(TypeCompiledImplAsStarlarkValue(IsTupleOf(
                    self.0.iter().map(|t| t.to_frozen(heap)).collect(),
                ))))
            }
        }

        TypeCompiled(heap.alloc_complex(TypeCompiledImplAsStarlarkValue(IsTupleOf(ts))))
    }

    /// Types that are `""` or start with `"_"` are wildcard - they match everything.
    pub(crate) fn is_wildcard(x: &str) -> bool {
        x == "" || x.starts_with('_')
    }

    pub(crate) fn is_wildcard_value(x: Value) -> bool {
        x.unpack_str().map(TypeCompiled::is_wildcard) == Some(true)
    }

    /// For `p: "xxx"`, parse that `"xxx"` as type.
    pub(crate) fn from_str(t: &str, heap: &'v Heap) -> TypeCompiled<Value<'v>> {
        if TypeCompiled::is_wildcard(t) {
            TypeCompiled::type_anything()
        } else {
            match t {
                "string" => TypeCompiled::type_string(),
                "int" => TypeCompiled::type_int(),
                "bool" => TypeCompiled::type_bool(),
                t => TypeCompiled::type_concrete(t, heap),
            }
        }
    }

    fn from_tuple(
        t: &TupleGen<Value<'v>>,
        heap: &'v Heap,
    ) -> anyhow::Result<TypeCompiled<Value<'v>>> {
        let ts = t.content().try_map(|t| TypeCompiled::new(*t, heap))?;
        Ok(TypeCompiled::type_tuple_of(ts, heap))
    }

    /// Parse `[t1, t2, ...]` as type.
    fn from_list(t: &ListRef<'v>, heap: &'v Heap) -> anyhow::Result<TypeCompiled<Value<'v>>> {
        match t.len() {
            0 => Err(TypingError::InvalidTypeAnnotation(t.to_string()).into()),
            1 => {
                // Must be a list with all elements of this type
                let t = *t.first().unwrap();
                if TypeCompiled::is_wildcard_value(t) {
                    // Any type - so avoid the inner iteration
                    Ok(TypeCompiled::type_list())
                } else {
                    let t = TypeCompiled::new(t, heap)?;
                    Ok(TypeCompiled::type_list_of(t, heap))
                }
            }
            2 => {
                // A union type, can match either - special case of the arbitrary choice to go slightly faster
                let t1 = TypeCompiled::new(t[0], heap)?;
                let t2 = TypeCompiled::new(t[1], heap)?;
                Ok(TypeCompiled::type_any_of_two(t1, t2, heap))
            }
            _ => {
                // A union type, can match any
                let ts = t[..].try_map(|t| TypeCompiled::new(*t, heap))?;
                Ok(TypeCompiled::type_any_of(ts, heap))
            }
        }
    }

    fn from_dict(t: DictRef<'v>, heap: &'v Heap) -> anyhow::Result<TypeCompiled<Value<'v>>> {
        // Dictionary with a single element
        fn unpack_singleton_dictionary<'v>(x: &Dict<'v>) -> Option<(Value<'v>, Value<'v>)> {
            if x.len() == 1 { x.iter().next() } else { None }
        }

        if let Some((tk, tv)) = unpack_singleton_dictionary(&t) {
            if TypeCompiled::is_wildcard_value(tk) && TypeCompiled::is_wildcard_value(tv) {
                Ok(TypeCompiled::type_dict())
            } else {
                // Dict of the form {k: v} must all match the k/v types
                let tk = TypeCompiled::new(tk, heap)?;
                let tv = TypeCompiled::new(tv, heap)?;
                Ok(TypeCompiled::type_dict_of(tk, tv, heap))
            }
        } else {
            // Dict type with zero or multiple fields is not allowed
            Err(TypingError::InvalidTypeAnnotation(t.to_string()).into())
        }
    }

    pub(crate) fn new(ty: Value<'v>, heap: &'v Heap) -> anyhow::Result<Self> {
        if let Some(s) = ty.unpack_str() {
            Ok(TypeCompiled::from_str(s, heap))
        } else if ty.is_none() {
            Ok(TypeCompiled::type_none())
        } else if let Some(t) = Tuple::from_value(ty) {
            TypeCompiled::from_tuple(t, heap)
        } else if let Some(t) = ListRef::from_value(ty) {
            TypeCompiled::from_list(t, heap)
        } else if let Some(t) = DictRef::from_value(ty) {
            TypeCompiled::from_dict(t, heap)
        } else if ty.request_value::<&dyn TypeCompiledImpl>().is_some() {
            Ok(TypeCompiled(ty))
        } else {
            Err(invalid_type_annotation(ty, heap).into())
        }
    }
}

fn invalid_type_annotation<'v>(ty: Value<'v>, heap: &'v Heap) -> TypingError {
    if let Some(name) = ty
        .get_attr("type", heap)
        .ok()
        .flatten()
        .and_then(|v| v.unpack_str())
    {
        TypingError::PerhapsYouMeant(ty.to_str(), name.into())
    } else {
        TypingError::InvalidTypeAnnotation(ty.to_str())
    }
}

#[cfg(test)]
mod tests {
    use crate::assert;
    use crate::values::typing::TypeCompiled;
    use crate::values::Heap;

    #[test]
    fn test_types() {
        let a = assert::Assert::new();
        a.is_true(
            r#"
def f(i: int.type) -> bool.type:
    return i == 3
f(8) == False"#,
        );

        // If the types are either malformed or runtime errors, it should fail
        a.fail("def f(i: made_up):\n pass", "Variable");
        a.fail(
            "def f(i: fail('bad')):\n pass",
            "call expression is not allowed in type expression",
        );

        // Type errors should be caught in arguments
        a.fails(
            "def f(i: bool.type):\n pass\nf(1)",
            &["type annotation", "`1`", "`int`", "`bool.type`", "`i`"],
        );
        // Type errors should be caught when the user forgets quotes around a valid type
        a.fail("def f(v: bool):\n pass\n", r#"Perhaps you meant `"bool"`"#);
        a.fails(
            r#"Foo = record(value=int.type)
def f(v: bool.type) -> Foo:
    return Foo(value=1)"#,
            &[r#"record(value=field(int.type))"#],
        );
        a.fails(
            r#"Bar = enum("bar")
def f(v: Bar):
  pass"#,
            &[r#"enum("bar")"#],
        );
        // Type errors should be caught in return positions
        a.fails(
            "def f() -> bool.type:\n return 1\nf()",
            &["type annotation", "`1`", "`bool.type`", "`int`", "return"],
        );
        // And for functions without return
        a.fails(
            "def f() -> bool.type:\n pass\nf()",
            &["type annotation", "`None`", "`bool.type`", "return"],
        );
        // And for functions that return None implicitly or explicitly
        a.fails(
            "def f() -> None:\n return True\nf()",
            &["type annotation", "`None`", "`bool`", "return"],
        );
        a.pass("def f() -> None:\n pass\nf()");

        // The following are all valid types
        a.all_true(
            r#"
is_type(1, int.type)
is_type(True, bool.type)
is_type(True, "")
is_type(None, None)
is_type(assert_type, "function")
is_type([], [int.type])
is_type([], [""])
is_type([1, 2, 3], [int.type])
is_type(None, [None, int.type])
is_type('test', [int.type, str.type])
is_type(('test', None), (str.type, None))
is_type({"test": 1, "more": 2}, {str.type: int.type})
is_type({1: 1, 2: 2}, {int.type: int.type})

not is_type(1, None)
not is_type((1, 1), str.type)
not is_type('test', [int.type, bool.type])
not is_type([1,2,None], [int.type])
not is_type({"test": 1, 8: 2}, {str.type: int.type})
not is_type({"test": 1, "more": None}, {str.type: int.type})

is_type(1, "")
is_type([1,2,"test"], ["_a"])
"#,
        );

        // Checking types fails for invalid types
        a.fail("is_type(None, is_type)", "not a valid type");
        a.fail("is_type(None, [])", "not a valid type");
        a.fail("is_type(None, {'1': '', '2': ''})", "not a valid type");
        a.fail("is_type({}, {1: 'string', 2: 'bool'})", "not a valid type");

        // Should check the type of default parameters that aren't used
        a.fail(
            r#"
def foo(f: int.type = None):
    pass
"#,
            "`None` of type `NoneType` does not match the type annotation `int.type`",
        );
    }

    #[test]
    fn test_type_compiled_display() {
        fn t(expected: &str, ty0: &str) {
            let ty = assert::pass(ty0);
            let heap = Heap::new();
            let ty = unsafe { ty.unchecked_frozen_value() }.to_value();
            let ty = TypeCompiled::new(ty, &heap).unwrap();
            assert_eq!(expected, ty.to_string(), "for `{}`", ty0);
        }

        t("\"\"", "\"\"");
        t("\"list\"", "list.type");
        t("[\"\"]", "[\"\"]");
        t("None", "None");
        t("[\"a\", \"b\"]", "[\"a\", \"b\"]");
    }

    #[test]
    fn test_type_compiled_starlark_api() {
        assert::eq("\"eval_type(int.type)\"", "repr(eval_type(int.type))");
        assert::is_true("eval_type(int.type).matches(1)");
        assert::is_true("not eval_type(int.type).matches([])");
        assert::pass("eval_type(int.type).check_matches(1)");
        assert::fail(
            "eval_type(int.type).check_matches([])",
            "Value of type `list` does not match type `int.type`: []",
        );
    }

    #[test]
    fn test_type_compiled_can_be_used_in_function_signature() {
        assert::pass(
            r#"
ty = eval_type(int.type)
def f(x: ty):
    pass

f(1)
"#,
        );
        assert::fail(
            r#"
ty = eval_type(int.type)
def f(x: ty):
    pass

f("x")
"#,
            "Value `x` of type `string` does not match the type annotation `int.type` for argument `x`",
        );
    }
}
