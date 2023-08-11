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
use cmp_any::PartialEqAny;
use dupe::Dupe;
use starlark_derive::starlark_module;
use starlark_derive::starlark_value;
use starlark_map::StarlarkHasher;
use thiserror::Error;

use crate as starlark;
use crate::any::ProvidesStaticType;
use crate::coerce::Coerce;
use crate::environment::Methods;
use crate::environment::MethodsBuilder;
use crate::environment::MethodsStatic;
use crate::private::Private;
use crate::slice_vec_ext::SliceExt;
use crate::slice_vec_ext::VecExt;
use crate::typing::basic::TyBasic;
use crate::typing::custom::TyCustom;
use crate::typing::starlark_value::TyStarlarkValue;
use crate::typing::Ty;
use crate::values::dict::Dict;
use crate::values::dict::DictRef;
use crate::values::layout::avalue::alloc_static;
use crate::values::layout::avalue::AValueImpl;
use crate::values::layout::avalue::Basic;
use crate::values::layout::heap::repr::AValueRepr;
use crate::values::list::ListRef;
use crate::values::none::NoneType;
use crate::values::type_repr::StarlarkTypeRepr;
use crate::values::types::int_or_big::StarlarkIntRef;
use crate::values::types::tuple::value::Tuple;
use crate::values::types::tuple::value::TupleGen;
use crate::values::typing::type_compiled::factory::TypeCompiledFactory;
use crate::values::AllocValue;
use crate::values::Demand;
use crate::values::Freeze;
use crate::values::FrozenHeap;
use crate::values::FrozenValue;
use crate::values::Heap;
use crate::values::NoSerialize;
use crate::values::StarlarkValue;
use crate::values::Trace;
use crate::values::UnpackValue;
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

pub(crate) trait TypeCompiledImpl:
    Allocative + Debug + Clone + Eq + Hash + Sized + Send + Sync + 'static
{
    fn matches(&self, value: Value) -> bool;
    fn is_wildcard(&self) -> bool {
        false
    }
}

trait TypeCompiledDyn: Debug + Allocative + Send + Sync + 'static {
    fn as_ty_dyn(&self) -> &Ty;
    fn matches_dyn(&self, value: Value) -> bool;
    fn is_runtime_wildcard_dyn(&self) -> bool;
    fn to_frozen_dyn(&self, heap: &FrozenHeap) -> TypeCompiled<FrozenValue>;
    fn patch_ty_dyn<'v>(&self, ty: Ty, heap: &'v Heap) -> TypeCompiled<Value<'v>>;

    fn eq_token(&self) -> PartialEqAny;
    fn hash_code(&self) -> u64;

    fn to_box(&self) -> TypeCompiledBox;
}

impl<T> TypeCompiledDyn for TypeCompiledImplAsStarlarkValue<T>
where
    T: TypeCompiledImpl,
{
    fn as_ty_dyn(&self) -> &Ty {
        &self.ty
    }
    fn matches_dyn(&self, value: Value) -> bool {
        self.type_compiled_impl.matches(value)
    }
    fn is_runtime_wildcard_dyn(&self) -> bool {
        self.type_compiled_impl.is_wildcard()
    }
    fn to_frozen_dyn(&self, heap: &FrozenHeap) -> TypeCompiled<FrozenValue> {
        TypeCompiled(heap.alloc_simple::<TypeCompiledImplAsStarlarkValue<T>>(Self::clone(self)))
    }
    fn patch_ty_dyn<'v>(&self, ty: Ty, heap: &'v Heap) -> TypeCompiled<Value<'v>> {
        TypeCompiled::alloc(self.type_compiled_impl.clone(), ty, heap)
    }

    fn eq_token(&self) -> PartialEqAny {
        PartialEqAny::new::<Self>(self)
    }
    fn hash_code(&self) -> u64 {
        let mut hasher = StarlarkHasher::new();
        self.type_compiled_impl.hash(&mut hasher);
        hasher.finish()
    }

    fn to_box(&self) -> TypeCompiledBox {
        TypeCompiledBox(Box::new(self.clone()))
    }
}

#[derive(Allocative, Debug)]
struct TypeCompiledBox(Box<dyn TypeCompiledDyn>);

impl Clone for TypeCompiledBox {
    fn clone(&self) -> Self {
        self.0.to_box()
    }
}

impl PartialEq for TypeCompiledBox {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq_token() == other.0.eq_token()
    }
}
impl Eq for TypeCompiledBox {}

impl Hash for TypeCompiledBox {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash_code().hash(state)
    }
}

// TODO(nga): derive.
unsafe impl<'v> ProvidesStaticType<'v> for &'v dyn TypeCompiledDyn {
    type StaticType = &'static dyn TypeCompiledDyn;
}

#[derive(
    Clone,
    Eq,
    PartialEq,
    Debug,
    Allocative,
    ProvidesStaticType,
    NoSerialize
)]
struct TypeCompiledImplAsStarlarkValue<T: 'static> {
    type_compiled_impl: T,
    ty: Ty,
}

impl<T> TypeCompiledImplAsStarlarkValue<T>
where
    TypeCompiledImplAsStarlarkValue<T>: StarlarkValue<'static>,
{
    const fn alloc_static(imp: T, ty: Ty) -> AValueRepr<AValueImpl<Basic, Self>> {
        alloc_static(
            Basic,
            TypeCompiledImplAsStarlarkValue {
                type_compiled_impl: imp,
                ty,
            },
        )
    }
}

#[starlark_value(type = "eval_type")]
impl<'v, T: 'static> StarlarkValue<'v> for TypeCompiledImplAsStarlarkValue<T>
where
    T: TypeCompiledImpl,
{
    fn type_matches_value(&self, value: Value<'v>, _private: Private) -> bool {
        self.type_compiled_impl.matches(value)
    }

    fn provide(&'v self, demand: &mut Demand<'_, 'v>) {
        demand.provide_ref_static::<dyn TypeCompiledDyn>(self);
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        Hash::hash(&self.type_compiled_impl, hasher);
        Ok(())
    }

    fn equals(&self, other: Value<'v>) -> anyhow::Result<bool> {
        let Some(other) = other.downcast_ref::<Self>() else {
            return Ok(false);
        };
        Ok(self.type_compiled_impl == other.type_compiled_impl)
    }

    fn eval_type(&self) -> Option<Ty> {
        // `TypeCompiled::new` handles this type explicitly,
        // but implement this function to make proc-macro generate `bit_or`.
        // Also safer to be explicit here.
        Some(self.ty.clone())
    }

    fn get_methods() -> Option<&'static Methods>
    where
        Self: Sized,
    {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(type_compiled_methods)
    }
}

impl<T: TypeCompiledImpl> Display for TypeCompiledImplAsStarlarkValue<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.ty)
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

/// Wrapper for a [`Value`] that acts like a runtime type matcher.
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
pub struct TypeCompiled<V>(
    /// `V` is `TypeCompiledImplAsStarlarkValue`.
    V,
);

impl<'v, V: ValueLike<'v>> Display for TypeCompiled<V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.downcast() {
            Ok(t) => Display::fmt(&t.as_ty_dyn(), f),
            Err(_) => {
                // This is unreachable, but we should not panic in `Display`.
                Display::fmt(&self.0, f)
            }
        }
    }
}

impl<V> StarlarkTypeRepr for TypeCompiled<V> {
    fn starlark_type_repr() -> Ty {
        Ty::name("eval_type")
    }
}

impl<'v, V: ValueLike<'v>> AllocValue<'v> for TypeCompiled<V> {
    fn alloc_value(self, _heap: &'v Heap) -> Value<'v> {
        self.0.to_value()
    }
}

impl<'v, V: ValueLike<'v>> TypeCompiled<V> {
    fn downcast(self) -> anyhow::Result<&'v dyn TypeCompiledDyn> {
        self.to_value()
            .0
            .request_value::<&dyn TypeCompiledDyn>()
            .context("Not TypeCompiledImpl (internal error)")
    }

    pub(crate) fn matches(&self, value: Value<'v>) -> bool {
        self.0.to_value().get_ref().type_matches_value(value)
    }

    pub(crate) fn as_ty(&self) -> &'v Ty {
        self.downcast().unwrap().as_ty_dyn()
    }

    /// True if `TypeCompiled` matches any type at runtime.
    /// However, compile-time/lint typechecker may still check the type.
    pub(crate) fn is_runtime_wildcard(self) -> bool {
        self.downcast().unwrap().is_runtime_wildcard_dyn()
    }

    fn to_box_dyn(&self) -> TypeCompiledBox {
        self.downcast().unwrap().to_box()
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

    pub(crate) fn to_inner(self) -> V {
        self.0
    }

    pub(crate) fn write_hash(self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        self.to_value().0.write_hash(hasher)
    }

    // Dead code, but may become useful in the future.
    pub(crate) fn _equals(self, other: Self) -> anyhow::Result<bool> {
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
            Clone,
            Copy,
            Dupe,
            Eq,
            PartialEq,
            Hash,
            Allocative,
            Debug,
            ProvidesStaticType
        )]
        struct Anything;

        impl TypeCompiledImpl for Anything {
            fn matches(&self, _value: Value) -> bool {
                true
            }

            fn is_wildcard(&self) -> bool {
                true
            }
        }

        static ANYTHING: AValueRepr<AValueImpl<Basic, TypeCompiledImplAsStarlarkValue<Anything>>> =
            TypeCompiledImplAsStarlarkValue::alloc_static(Anything, Ty::any());

        TypeCompiled(V::from_frozen_value(FrozenValue::new_repr(&ANYTHING)))
    }

    pub(crate) fn type_none() -> TypeCompiled<V> {
        #[derive(
            Clone,
            Copy,
            Dupe,
            Eq,
            PartialEq,
            Hash,
            Allocative,
            Debug,
            ProvidesStaticType
        )]
        struct IsNone;

        impl TypeCompiledImpl for IsNone {
            fn matches(&self, value: Value) -> bool {
                value.is_none()
            }
        }

        static IS_NONE: AValueRepr<AValueImpl<Basic, TypeCompiledImplAsStarlarkValue<IsNone>>> =
            TypeCompiledImplAsStarlarkValue::alloc_static(IsNone, Ty::none());

        TypeCompiled(V::from_frozen_value(FrozenValue::new_repr(&IS_NONE)))
    }

    pub(crate) fn type_string() -> TypeCompiled<V> {
        #[derive(
            Clone,
            Copy,
            Dupe,
            Eq,
            PartialEq,
            Hash,
            Allocative,
            Debug,
            ProvidesStaticType
        )]
        struct IsString;

        impl TypeCompiledImpl for IsString {
            fn matches(&self, value: Value) -> bool {
                value.unpack_str().is_some()
            }
        }

        static IS_STRING: AValueRepr<AValueImpl<Basic, TypeCompiledImplAsStarlarkValue<IsString>>> =
            TypeCompiledImplAsStarlarkValue::alloc_static(IsString, Ty::string());

        TypeCompiled(V::from_frozen_value(FrozenValue::new_repr(&IS_STRING)))
    }

    pub(crate) fn type_int() -> TypeCompiled<V> {
        #[derive(
            Clone,
            Copy,
            Dupe,
            Eq,
            PartialEq,
            Hash,
            Allocative,
            Debug,
            ProvidesStaticType
        )]
        struct IsInt;

        impl TypeCompiledImpl for IsInt {
            fn matches(&self, value: Value) -> bool {
                StarlarkIntRef::unpack_value(value).is_some()
            }
        }

        static IS_INT: AValueRepr<AValueImpl<Basic, TypeCompiledImplAsStarlarkValue<IsInt>>> =
            TypeCompiledImplAsStarlarkValue::alloc_static(IsInt, Ty::int());

        TypeCompiled(V::from_frozen_value(FrozenValue::new_repr(&IS_INT)))
    }

    pub(crate) fn type_bool() -> TypeCompiled<V> {
        #[derive(
            Clone,
            Copy,
            Dupe,
            Eq,
            PartialEq,
            Hash,
            Allocative,
            Debug,
            ProvidesStaticType
        )]
        struct IsBool;

        impl TypeCompiledImpl for IsBool {
            fn matches(&self, value: Value) -> bool {
                value.unpack_bool().is_some()
            }
        }

        static IS_BOOL: AValueRepr<AValueImpl<Basic, TypeCompiledImplAsStarlarkValue<IsBool>>> =
            TypeCompiledImplAsStarlarkValue::alloc_static(IsBool, Ty::bool());

        TypeCompiled(V::from_frozen_value(FrozenValue::new_repr(&IS_BOOL)))
    }

    pub(crate) fn to_frozen(self, heap: &FrozenHeap) -> TypeCompiled<FrozenValue> {
        if let Some(v) = self.0.to_value().unpack_frozen() {
            TypeCompiled(v)
        } else {
            self.to_value().downcast().unwrap().to_frozen_dyn(heap)
        }
    }
}

// These functions are small, but are deliberately out-of-line so we get better
// information in profiling about the origin of these closures
impl<'v> TypeCompiled<Value<'v>> {
    pub(crate) fn alloc(
        type_compiled_impl: impl TypeCompiledImpl,
        ty: Ty,
        heap: &'v Heap,
    ) -> TypeCompiled<Value<'v>> {
        TypeCompiled(heap.alloc_simple(TypeCompiledImplAsStarlarkValue {
            type_compiled_impl,
            ty,
        }))
    }

    /// Replace `ty` field, keep the matcher.
    fn patch_ty(self, ty: Ty, heap: &'v Heap) -> TypeCompiled<Value<'v>> {
        if self.as_ty() == &ty {
            // Optimization, semantically identical to the branch below.
            self.to_value()
        } else {
            self.downcast().unwrap().patch_ty_dyn(ty, heap)
        }
    }

    fn type_concrete(t: &str, heap: &'v Heap) -> TypeCompiled<Value<'v>> {
        #[derive(
            Eq,
            PartialEq,
            Hash,
            Allocative,
            Debug,
            Trace,
            Freeze,
            ProvidesStaticType,
            Clone
        )]
        struct IsConcrete(String);

        impl TypeCompiledImpl for IsConcrete {
            fn matches(&self, value: Value) -> bool {
                value.get_ref().matches_type(&self.0)
            }
        }

        let ty = Ty::name(t);
        Self::alloc(IsConcrete(t.to_owned()), ty, heap)
    }

    fn ty_custom(ty_custom: &TyCustom, heap: &'v Heap) -> TypeCompiled<Value<'v>> {
        let ty = Ty::basic(TyBasic::Custom(ty_custom.clone()));
        ty_custom.matcher(TypeCompiledFactory::new(ty, heap))
    }

    fn type_list(heap: &'v Heap) -> TypeCompiled<Value<'v>> {
        #[derive(
            Clone,
            Copy,
            Dupe,
            Eq,
            PartialEq,
            Hash,
            Allocative,
            Debug,
            ProvidesStaticType
        )]
        struct IsList;

        impl TypeCompiledImpl for IsList {
            fn matches(&self, value: Value) -> bool {
                ListRef::from_value(value).is_some()
            }
        }

        Self::alloc(IsList, Ty::any_list(), heap)
    }

    fn type_dict(heap: &'v Heap) -> TypeCompiled<Value<'v>> {
        #[derive(
            Clone,
            Copy,
            Dupe,
            Eq,
            PartialEq,
            Hash,
            Allocative,
            Debug,
            ProvidesStaticType
        )]
        struct IsDict;

        impl TypeCompiledImpl for IsDict {
            fn matches(&self, value: Value) -> bool {
                DictRef::from_value(value).is_some()
            }
        }

        Self::alloc(IsDict, Ty::any_dict(), heap)
    }

    pub(crate) fn type_list_of(
        t: TypeCompiled<Value<'v>>,
        heap: &'v Heap,
    ) -> TypeCompiled<Value<'v>> {
        let ty = Ty::list(t.as_ty().clone());
        if t.is_runtime_wildcard() {
            return TypeCompiled::<Value>::type_list(heap).patch_ty(ty, heap);
        }

        #[derive(Clone, Allocative, Eq, PartialEq, Hash, Debug, ProvidesStaticType)]
        struct IsListOf(TypeCompiledBox);

        impl TypeCompiledImpl for IsListOf {
            fn matches(&self, value: Value) -> bool {
                match ListRef::from_value(value) {
                    None => false,
                    Some(list) => list.iter().all(|v| self.0.0.matches_dyn(v)),
                }
            }
        }

        Self::alloc(IsListOf(t.to_box_dyn()), ty, heap)
    }

    pub(crate) fn type_any_of_two(
        t0: TypeCompiled<Value<'v>>,
        t1: TypeCompiled<Value<'v>>,
        heap: &'v Heap,
    ) -> TypeCompiled<Value<'v>> {
        if t0.is_runtime_wildcard() || t1.is_runtime_wildcard() {
            let ts = Ty::union2(t0.as_ty().clone(), t1.as_ty().clone());
            return TypeCompiled::<Value>::type_anything().patch_ty(ts, heap);
        }

        #[derive(Eq, PartialEq, Hash, Clone, Allocative, Debug, ProvidesStaticType)]
        struct IsAnyOfTwo(TypeCompiledBox, TypeCompiledBox);

        impl TypeCompiledImpl for IsAnyOfTwo {
            fn matches(&self, value: Value) -> bool {
                self.0.0.matches_dyn(value) || self.1.0.matches_dyn(value)
            }
        }

        Self::alloc(
            IsAnyOfTwo(t0.to_box_dyn(), t1.to_box_dyn()),
            Ty::union2(t0.as_ty().clone(), t1.as_ty().clone()),
            heap,
        )
    }

    pub(crate) fn type_any_of(
        ts: Vec<TypeCompiled<Value<'v>>>,
        heap: &'v Heap,
    ) -> TypeCompiled<Value<'v>> {
        if ts.iter().any(|t| t.is_runtime_wildcard()) {
            let ts = Ty::unions(ts.map(|t| t.as_ty().clone()));
            return TypeCompiled::<Value>::type_anything().patch_ty(ts, heap);
        } else if ts.len() == 1 {
            return ts.into_iter().next().unwrap();
        } else if ts.len() == 2 {
            let mut it = ts.into_iter();
            let t0 = it.next().unwrap();
            let t1 = it.next().unwrap();
            assert!(it.next().is_none());
            return Self::type_any_of_two(t0, t1, heap);
        }

        #[derive(Eq, PartialEq, Hash, Clone, Allocative, Debug, ProvidesStaticType)]
        struct IsAnyOf(Vec<TypeCompiledBox>);

        impl TypeCompiledImpl for IsAnyOf {
            fn matches(&self, value: Value) -> bool {
                self.0.iter().any(|t| t.0.matches_dyn(value))
            }
        }

        let ty = Ty::unions(ts.map(|t| t.as_ty().clone()));
        Self::alloc(IsAnyOf(ts.into_map(|t| t.to_box_dyn())), ty, heap)
    }

    pub(crate) fn type_dict_of(
        kt: TypeCompiled<Value<'v>>,
        vt: TypeCompiled<Value<'v>>,
        heap: &'v Heap,
    ) -> TypeCompiled<Value<'v>> {
        let ty = Ty::dict(kt.as_ty().clone(), vt.as_ty().clone());
        if kt.is_runtime_wildcard() && vt.is_runtime_wildcard() {
            return TypeCompiled::<Value>::type_dict(heap).patch_ty(ty, heap);
        }

        #[derive(Eq, PartialEq, Hash, Clone, Allocative, Debug, ProvidesStaticType)]
        struct IsDictOf(TypeCompiledBox, TypeCompiledBox);

        impl TypeCompiledImpl for IsDictOf {
            fn matches(&self, value: Value) -> bool {
                match DictRef::from_value(value) {
                    None => false,
                    Some(dict) => dict
                        .iter()
                        .all(|(k, v)| self.0.0.matches_dyn(k) && self.1.0.matches_dyn(v)),
                }
            }
        }

        Self::alloc(IsDictOf(kt.to_box_dyn(), vt.to_box_dyn()), ty, heap)
    }

    pub(crate) fn type_tuple_of(
        ts: Vec<TypeCompiled<Value<'v>>>,
        heap: &'v Heap,
    ) -> TypeCompiled<Value<'v>> {
        #[derive(Eq, PartialEq, Hash, Clone, Allocative, Debug, ProvidesStaticType)]
        struct IsTupleOf(Vec<TypeCompiledBox>);

        impl TypeCompiledImpl for IsTupleOf {
            fn matches(&self, value: Value) -> bool {
                match Tuple::from_value(value) {
                    Some(v) if v.len() == self.0.len() => {
                        v.iter().zip(self.0.iter()).all(|(v, t)| t.0.matches_dyn(v))
                    }
                    _ => false,
                }
            }
        }

        let ty = Ty::tuple(ts.map(|t| t.as_ty().clone()));
        Self::alloc(IsTupleOf(ts.into_map(|t| t.to_box_dyn())), ty, heap)
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
                "NoneType" => TypeCompiled::type_none(),
                "list" => TypeCompiled::type_list(heap),
                "dict" => TypeCompiled::type_dict(heap),
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
        match t.content() {
            [] => Err(TypingError::InvalidTypeAnnotation(t.to_string()).into()),
            [t] => {
                let t = TypeCompiled::new(*t, heap)?;
                Ok(TypeCompiled::type_list_of(t, heap))
            }
            ts => {
                // A union type, can match any
                let ts = ts.try_map(|t| TypeCompiled::new(*t, heap))?;
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
                Ok(TypeCompiled::type_dict(heap))
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

    fn iter(item: &Ty, heap: &'v Heap) -> Self {
        #[derive(Copy, Clone, Dupe, Debug, Hash, Eq, PartialEq, Allocative)]
        struct IterableMatcher;

        impl TypeCompiledImpl for IterableMatcher {
            fn matches(&self, value: Value) -> bool {
                TyStarlarkValue::is_iterable(&value.vtable().starlark_value)
            }
        }

        TypeCompiled::alloc(IterableMatcher, Ty::iter(item.clone()), heap)
    }

    fn from_ty_basic(ty: &TyBasic, heap: &'v Heap) -> Self {
        match ty {
            TyBasic::Any => TypeCompiled::type_anything(),
            TyBasic::Name(name) => TypeCompiled::from_str(name.as_str(), heap),
            TyBasic::StarlarkValue(x) => x.type_compiled(heap),
            TyBasic::List(item) => {
                let item = TypeCompiled::from_ty(item, heap);
                TypeCompiled::type_list_of(item, heap)
            }
            TyBasic::Tuple(xs) => {
                let xs = xs.map(|x| TypeCompiled::from_ty(x, heap));
                TypeCompiled::type_tuple_of(xs, heap)
            }
            TyBasic::Dict(k_v) => {
                let (k, v) = &**k_v;
                let k = TypeCompiled::from_ty(k, heap);
                let v = TypeCompiled::from_ty(v, heap);
                TypeCompiled::type_dict_of(k, v, heap)
            }
            TyBasic::Iter(item) => TypeCompiled::iter(item, heap),
            TyBasic::Custom(custom) => TypeCompiled::ty_custom(custom, heap),
        }
    }

    pub(crate) fn from_ty(ty: &Ty, heap: &'v Heap) -> Self {
        TypeCompiled::type_any_of(
            ty.iter_union()
                .map(|t| TypeCompiled::from_ty_basic(t, heap)),
            heap,
        )
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
        } else if ty.request_value::<&dyn TypeCompiledDyn>().is_some() {
            // This branch is optimization: `TypeCompiledAsStarlarkValue` implements `eval_type`,
            // but this branch avoids copying the type.
            Ok(TypeCompiled(ty))
        } else if let Some(ty) = ty.get_ref().eval_type() {
            Ok(TypeCompiled::from_ty(&ty, heap))
        } else {
            Err(invalid_type_annotation(ty, heap).into())
        }
    }
}

impl TypeCompiled<FrozenValue> {
    pub(crate) fn new_frozen(ty: FrozenValue, frozen_heap: &FrozenHeap) -> anyhow::Result<Self> {
        // TODO(nga): trip to a heap is not free.
        let heap = Heap::new();
        let ty = TypeCompiled::new(ty.to_value(), &heap)?;
        Ok(ty.to_frozen(frozen_heap))
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
