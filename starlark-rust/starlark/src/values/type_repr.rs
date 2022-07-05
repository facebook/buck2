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

use either::Either;

use crate::values::none::NoneType;
use crate::values::StarlarkValue;

/// Provides a starlark type representation, even if StarlarkValue is not implemented.
pub trait StarlarkTypeRepr {
    /// The representation of a type that a user would use verbatim in starlark type annotations
    fn starlark_type_repr() -> String;
}

impl<'v, T: StarlarkValue<'v>> StarlarkTypeRepr for T {
    fn starlark_type_repr() -> String {
        Self::get_type_starlark_repr()
    }
}

impl StarlarkTypeRepr for String {
    fn starlark_type_repr() -> String {
        str::starlark_type_repr()
    }
}

impl StarlarkTypeRepr for str {
    fn starlark_type_repr() -> String {
        // Here because str is not Sized so StarlarkValue doesn't
        // implement get_type_starlark_repr
        "str.type".to_owned()
    }
}

impl StarlarkTypeRepr for &str {
    fn starlark_type_repr() -> String {
        str::starlark_type_repr()
    }
}

impl<T: StarlarkTypeRepr> StarlarkTypeRepr for Option<T> {
    fn starlark_type_repr() -> String {
        format!(
            "[{}, {}]",
            NoneType::starlark_type_repr(),
            T::starlark_type_repr()
        )
    }
}

impl<T: StarlarkTypeRepr> StarlarkTypeRepr for Vec<T> {
    fn starlark_type_repr() -> String {
        format!("[{}]", T::starlark_type_repr())
    }
}
impl<TLeft: StarlarkTypeRepr, TRight: StarlarkTypeRepr> StarlarkTypeRepr for Either<TLeft, TRight> {
    fn starlark_type_repr() -> String {
        format!(
            "[{}, {}]",
            TLeft::starlark_type_repr(),
            TRight::starlark_type_repr()
        )
    }
}
