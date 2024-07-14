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
pub use starlark_derive::StarlarkTypeRepr;

use crate::typing::Ty;
use crate::values::dict::UnpackDictEntries;
use crate::values::none::NoneType;
use crate::values::string::str_type::StarlarkStr;
use crate::values::Heap;
use crate::values::StarlarkValue;
use crate::values::UnpackAndDiscard;
use crate::values::UnpackValue;
use crate::values::Value;

/// Provides a starlark type representation, even if StarlarkValue is not implemented.
///
/// # Derive
///
/// There is `#[derive(StarlarkTypeRepr)]` for enums, for example:
///
/// ```
/// use starlark::values::type_repr::StarlarkTypeRepr;
///
/// #[derive(StarlarkTypeRepr)]
/// enum IntOrString {
///     Int(i32),
///     String(String),
/// }
/// ```
///
/// It emits type `int | str`.
///
/// This derive is useful in combination with derive of [`UnpackValue`].
pub trait StarlarkTypeRepr {
    /// Different Rust type representing the same Starlark Type.
    ///
    /// For example, `bool` and `StarlarkBool` Rust types represent the same Starlark type `bool`.
    ///
    /// Formal requirement: `Self::starlark_type_repr() == Self::Canonical::starlark_type_repr()`.
    ///
    /// If unsure, it is safe to put `= Self` here.
    /// When [`associated_type_defaults`](https://github.com/rust-lang/rust/issues/29661)
    /// is stabilized, this will be the default.
    type Canonical: StarlarkTypeRepr;

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
    type Canonical = DictType<K::Canonical, V::Canonical>;

    fn starlark_type_repr() -> Ty {
        Ty::dict(K::starlark_type_repr(), V::starlark_type_repr())
    }
}

impl<'v, K: UnpackValue<'v>, V: UnpackValue<'v>> UnpackValue<'v> for DictType<K, V> {
    type Error = Either<K::Error, V::Error>;

    fn unpack_value_impl(value: Value<'v>) -> Result<Option<Self>, Self::Error> {
        match UnpackDictEntries::<UnpackAndDiscard<K>, UnpackAndDiscard<V>>::unpack_value_impl(
            value,
        ) {
            Ok(Some(_)) => Ok(Some(DictType {
                k: PhantomData,
                v: PhantomData,
            })),
            Ok(None) => Ok(None),
            Err(e) => Err(e),
        }
    }
}

impl<'v, T: StarlarkValue<'v> + ?Sized> StarlarkTypeRepr for T {
    type Canonical = Self;

    fn starlark_type_repr() -> Ty {
        Self::get_type_starlark_repr()
    }
}

impl StarlarkTypeRepr for String {
    type Canonical = StarlarkStr;

    fn starlark_type_repr() -> Ty {
        StarlarkStr::starlark_type_repr()
    }
}

impl StarlarkTypeRepr for &str {
    type Canonical = StarlarkStr;

    fn starlark_type_repr() -> Ty {
        StarlarkStr::starlark_type_repr()
    }
}

impl<T: StarlarkTypeRepr> StarlarkTypeRepr for Option<T> {
    type Canonical = <Either<NoneType, T> as StarlarkTypeRepr>::Canonical;

    fn starlark_type_repr() -> Ty {
        Either::<NoneType, T>::starlark_type_repr()
    }
}

impl<T: StarlarkTypeRepr> StarlarkTypeRepr for Vec<T> {
    type Canonical = Vec<T::Canonical>;

    fn starlark_type_repr() -> Ty {
        Ty::list(T::starlark_type_repr())
    }
}

impl<TLeft: StarlarkTypeRepr, TRight: StarlarkTypeRepr> StarlarkTypeRepr for Either<TLeft, TRight> {
    type Canonical = Either<TLeft::Canonical, TRight::Canonical>;

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

#[cfg(test)]
mod tests {
    use crate::tests::util::TestComplexValue;
    use crate::util::non_static_type_id::non_static_type_id;
    use crate::values::type_repr::StarlarkTypeRepr;
    use crate::values::FrozenValue;
    use crate::values::Value;

    #[test]
    fn test_canonical_for_complex_value() {
        // TODO(nga): `StarlarkTypeRepr::Canonical` should be equal.
        assert_ne!(
            non_static_type_id::<<TestComplexValue<Value> as StarlarkTypeRepr>::Canonical>(),
            non_static_type_id::<<TestComplexValue<FrozenValue> as StarlarkTypeRepr>::Canonical>(),
        );
    }
}
