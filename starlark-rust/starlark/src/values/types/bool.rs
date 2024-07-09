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

//! The boolean type (`False` and `True`).
//!
//! Can be created with [`new_bool`](Value::new_bool) and unwrapped with [`unpack_bool`](Value::unpack_bool).
//! Unlike most Starlark values, these aren't actually represented on the [`Heap`], but as special values.

use std::cmp::Ordering;
use std::convert::Infallible;
use std::fmt;
use std::fmt::Display;
use std::hash::Hasher;

use allocative::Allocative;
use serde::Serialize;
use starlark_derive::starlark_value;
use starlark_derive::StarlarkDocs;

use crate as starlark;
use crate::any::ProvidesStaticType;
use crate::collections::StarlarkHashValue;
use crate::collections::StarlarkHasher;
use crate::private::Private;
use crate::typing::Ty;
use crate::values::layout::avalue::alloc_static;
use crate::values::layout::avalue::AValueBasic;
use crate::values::layout::avalue::AValueImpl;
use crate::values::layout::heap::repr::AValueRepr;
use crate::values::type_repr::StarlarkTypeRepr;
use crate::values::AllocFrozenValue;
use crate::values::AllocValue;
use crate::values::FrozenHeap;
use crate::values::FrozenValue;
use crate::values::Heap;
use crate::values::StarlarkValue;
use crate::values::UnpackValue;
use crate::values::Value;
use crate::values::ValueError;

/// The result of calling `type()` on booleans.
pub const BOOL_TYPE: &str = "bool";

/// `bool` value.
#[derive(ProvidesStaticType, Debug, Serialize, StarlarkDocs, Allocative)]
#[starlark_docs(builtin = "standard")]
#[serde(transparent)]
pub struct StarlarkBool(pub(crate) bool);

impl Display for StarlarkBool {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0 {
            write!(f, "True")
        } else {
            write!(f, "False")
        }
    }
}

pub(crate) static VALUE_FALSE_TRUE: [AValueRepr<AValueImpl<AValueBasic<StarlarkBool>>>; 2] = [
    alloc_static(StarlarkBool(false)),
    alloc_static(StarlarkBool(true)),
];

impl<'v> AllocValue<'v> for bool {
    fn alloc_value(self, _heap: &'v Heap) -> Value<'v> {
        Value::new_bool(self)
    }
}

impl AllocFrozenValue for bool {
    fn alloc_frozen_value(self, _heap: &FrozenHeap) -> FrozenValue {
        FrozenValue::new_bool(self)
    }
}

impl StarlarkTypeRepr for bool {
    type Canonical = <StarlarkBool as StarlarkTypeRepr>::Canonical;

    fn starlark_type_repr() -> Ty {
        StarlarkBool::get_type_starlark_repr()
    }
}

impl UnpackValue<'_> for bool {
    type Error = Infallible;

    fn unpack_value_impl(value: Value) -> Result<Option<Self>, Self::Error> {
        Ok(value.unpack_bool())
    }
}

/// Define the bool type
#[starlark_value(type = BOOL_TYPE)]
impl<'v> StarlarkValue<'v> for StarlarkBool {
    fn is_special(_: Private) -> bool
    where
        Self: Sized,
    {
        true
    }

    fn collect_repr(&self, s: &mut String) {
        // repr() for bool is quite hot, so optimise it
        if self.0 {
            s.push_str("True")
        } else {
            s.push_str("False")
        }
    }

    fn to_bool(&self) -> bool {
        self.0
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> crate::Result<()> {
        hasher.write_u8(if self.0 { 1 } else { 0 });
        Ok(())
    }

    fn get_hash(&self, _private: Private) -> crate::Result<StarlarkHashValue> {
        // These constants are just two random numbers.
        Ok(StarlarkHashValue::new_unchecked(if self.0 {
            0xa4acba08
        } else {
            0x71e8ba71
        }))
    }

    fn compare(&self, other: Value) -> crate::Result<Ordering> {
        if let Some(other) = other.unpack_bool() {
            Ok(self.0.cmp(&other))
        } else {
            ValueError::unsupported_with(self, "<>", other)
        }
    }

    fn typechecker_ty(&self) -> Option<Ty> {
        Some(Ty::bool())
    }

    fn get_type_starlark_repr() -> Ty {
        Ty::bool()
    }
}
