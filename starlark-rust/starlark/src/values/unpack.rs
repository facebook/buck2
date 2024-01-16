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

//! Parameter conversion utilities for `starlark_module` macros.

use std::ops::Deref;

use dupe::Dupe;
use either::Either;

use crate::typing::Ty;
use crate::values::type_repr::StarlarkTypeRepr;
use crate::values::AllocValue;
use crate::values::Heap;
use crate::values::Value;
use crate::values::ValueError;

/// How to convert a [`Value`] to a Rust type. Required for all arguments in
/// a [`#[starlark_module]`](macro@crate::starlark_module) definition.
///
/// Note for simple references it often can be implemented with `#[starlark_value(UnpackValue)]`,
/// for example:
///
/// ```
/// # use allocative::Allocative;
/// # use starlark::any::ProvidesStaticType;
/// # use starlark::values::{NoSerialize, StarlarkValue, starlark_value};
///
/// #[derive(
///     Debug,
///     derive_more::Display,
///     Allocative,
///     NoSerialize,
///     ProvidesStaticType
/// )]
/// struct MySimpleValue;
///
/// #[starlark_value(type = "MySimpleValue", UnpackValue, StarlarkTypeRepr)]
/// impl<'v> StarlarkValue<'v> for MySimpleValue {}
/// ```
///
/// Whereas for types that aren't also [`StarlarkValue`](crate::values::StarlarkValue) you can define:
///
/// ```
/// # use either::Either;
/// # use starlark::typing::Ty;
/// # use starlark::values::{UnpackValue, Value};
/// # use starlark::values::type_repr::StarlarkTypeRepr;
///
/// struct BoolOrInt(i32);
///
/// impl StarlarkTypeRepr for BoolOrInt {
///     fn starlark_type_repr() -> Ty {
///         Either::<bool, i32>::starlark_type_repr()
///     }
/// }
///
/// impl<'v> UnpackValue<'v> for BoolOrInt {
///     fn unpack_value(value: Value<'v>) -> Option<Self> {
///         if let Some(x) = value.unpack_bool() {
///             Some(BoolOrInt(x as i32))
///         } else {
///             value.unpack_i32().map(BoolOrInt)
///         }
///     }
/// }
/// ```
pub trait UnpackValue<'v>: Sized + StarlarkTypeRepr {
    /// Description of values acceptable by `unpack_value`, e. g. `list or str`.
    fn expected() -> String {
        Self::starlark_type_repr().to_string()
    }

    /// Given a [`Value`], try and unpack it into the given type, which may involve some element of conversion.
    fn unpack_value(value: Value<'v>) -> Option<Self>;

    /// Unpack a value, but return error instead of `None` if unpacking fails.
    fn unpack_value_err(value: Value<'v>) -> anyhow::Result<Self> {
        #[derive(thiserror::Error, Debug)]
        #[error("Expected `{0}`, but got `{1}`")]
        struct Error(String, &'static str);

        Self::unpack_value(value).ok_or_else(|| Error(Self::expected(), value.get_type()).into())
    }

    /// Unpack value, but instead of `None` return error about incorrect argument type.
    #[inline]
    fn unpack_param(value: Value<'v>) -> anyhow::Result<Self> {
        #[cold]
        fn error<'v, U: UnpackValue<'v>>(value: Value<'v>) -> anyhow::Error {
            ValueError::IncorrectParameterTypeWithExpected(
                U::expected(),
                value.get_type().to_owned(),
            )
            .into()
        }

        Self::unpack_value(value).ok_or_else(|| error::<Self>(value))
    }

    /// Unpack value, but instead of `None` return error about incorrect named argument type.
    #[inline]
    fn unpack_named_param(value: Value<'v>, param_name: &str) -> anyhow::Result<Self> {
        #[cold]
        fn error<'v, U: UnpackValue<'v>>(value: Value<'v>, param_name: &str) -> anyhow::Error {
            ValueError::IncorrectParameterTypeNamedWithExpected(
                param_name.to_owned(),
                U::expected(),
                value.get_type().to_owned(),
            )
            .into()
        }

        Self::unpack_value(value).ok_or_else(|| error::<Self>(value, param_name))
    }
}

impl<'v> UnpackValue<'v> for Value<'v> {
    fn expected() -> String {
        "Value".to_owned()
    }

    fn unpack_value(value: Value<'v>) -> Option<Self> {
        Some(value)
    }
}

/// A wrapper that keeps the original value on the heap for use elsewhere,
/// and also, when unpacked, unpacks the value to validate it is of
/// the correct type. Has an [`UnpackValue`] instance, so often used as
/// an argument to [`#[starlark_module]`](macro@crate::starlark_module) defined
/// functions.
///
/// Two container specializations of this are [`ListOf`](crate::values::list::ListOf)
/// and [`DictOf`](crate::values::dict::DictOf), which
/// validate the types of their containers on unpack, but do not store the
/// resulting Vec/Map
#[derive(Debug, Copy, Clone, Dupe)]
pub struct ValueOf<'v, T: UnpackValue<'v>> {
    /// The original [`Value`] on the same heap.
    pub value: Value<'v>,
    /// The value that was unpacked.
    pub typed: T,
}

impl<'v, T: UnpackValue<'v>> Deref for ValueOf<'v, T> {
    type Target = Value<'v>;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<'v, T: UnpackValue<'v>> StarlarkTypeRepr for ValueOf<'v, T> {
    fn starlark_type_repr() -> Ty {
        T::starlark_type_repr()
    }
}

impl<'v, T: UnpackValue<'v>> UnpackValue<'v> for ValueOf<'v, T> {
    fn expected() -> String {
        T::expected()
    }

    fn unpack_value(value: Value<'v>) -> Option<Self> {
        let typed = T::unpack_value(value)?;
        Some(Self { value, typed })
    }
}

impl<'v, T: UnpackValue<'v>> AllocValue<'v> for ValueOf<'v, T> {
    fn alloc_value(self, _heap: &'v Heap) -> Value<'v> {
        self.value
    }
}

impl<'v, TLeft: UnpackValue<'v>, TRight: UnpackValue<'v>> UnpackValue<'v>
    for Either<TLeft, TRight>
{
    fn expected() -> String {
        format!("either {} or {}", TLeft::expected(), TRight::expected())
    }

    // Only implemented for types that implement [`UnpackValue`]. Nonsensical for other types.
    fn unpack_value(value: Value<'v>) -> Option<Self> {
        if let Some(left) = TLeft::unpack_value(value) {
            Some(Self::Left(left))
        } else {
            TRight::unpack_value(value).map(Self::Right)
        }
    }
}
