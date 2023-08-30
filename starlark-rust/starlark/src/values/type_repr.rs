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

//! Trait and default implementations of a trait that will show starlark type annotations for a
//! given type.

use std::marker::PhantomData;

use either::Either;

use crate::typing::Ty;
use crate::values::none::NoneType;
use crate::values::string::StarlarkStr;
use crate::values::Heap;
use crate::values::StarlarkValue;
use crate::values::Value;

/// Provides a starlark type representation, even if StarlarkValue is not implemented.
pub trait StarlarkTypeRepr {
    /// The representation of a type that a user would use verbatim in starlark type annotations
    fn starlark_type_repr() -> Ty;
}

/// A dict used just for display purposes.
///
/// `DictOf` requires `Unpack` to be implemented, and `Dict` does not take type parameters so
/// we need something for documentation generation.
pub struct DictType<K: StarlarkTypeRepr, V: StarlarkTypeRepr> {
    k: PhantomData<K>,
    v: PhantomData<V>,
}

impl<K: StarlarkTypeRepr, V: StarlarkTypeRepr> StarlarkTypeRepr for DictType<K, V> {
    fn starlark_type_repr() -> Ty {
        Ty::dict(K::starlark_type_repr(), V::starlark_type_repr())
    }
}

impl<'v, T: StarlarkValue<'v> + ?Sized> StarlarkTypeRepr for T {
    fn starlark_type_repr() -> Ty {
        Self::get_type_starlark_repr()
    }
}

impl StarlarkTypeRepr for String {
    fn starlark_type_repr() -> Ty {
        StarlarkStr::starlark_type_repr()
    }
}

impl StarlarkTypeRepr for &str {
    fn starlark_type_repr() -> Ty {
        StarlarkStr::starlark_type_repr()
    }
}

impl<T: StarlarkTypeRepr> StarlarkTypeRepr for Option<T> {
    fn starlark_type_repr() -> Ty {
        Either::<NoneType, T>::starlark_type_repr()
    }
}

impl<T: StarlarkTypeRepr> StarlarkTypeRepr for Vec<T> {
    fn starlark_type_repr() -> Ty {
        Ty::list(T::starlark_type_repr())
    }
}

impl<TLeft: StarlarkTypeRepr, TRight: StarlarkTypeRepr> StarlarkTypeRepr for Either<TLeft, TRight> {
    fn starlark_type_repr() -> Ty {
        Ty::union2(TLeft::starlark_type_repr(), TRight::starlark_type_repr())
    }
}

/// Derive macros generate a reference to this method to be able to get the `type_repr` of types
/// they can't name
#[doc(hidden)]
pub fn type_repr_from_attr_impl<'v, T: StarlarkTypeRepr>(
    _f: fn(Value<'v>, &'v Heap) -> anyhow::Result<T>,
) -> Ty {
    T::starlark_type_repr()
}
