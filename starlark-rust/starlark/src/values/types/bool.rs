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
use std::fmt::Display;
use std::fmt::{self};
use std::hash::Hasher;

use gazebo::any::ProvidesStaticType;
use serde::Serialize;

use crate::collections::StarlarkHashValue;
use crate::collections::StarlarkHasher;
use crate::private::Private;
use crate::values::basic::StarlarkValueBasic;
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

// We have to alias bool so we can have a Display that uses True/False.
#[derive(ProvidesStaticType, Debug, Serialize)]
#[serde(transparent)]
pub(crate) struct StarlarkBool(pub(crate) bool);

impl Display for StarlarkBool {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0 {
            write!(f, "True")
        } else {
            write!(f, "False")
        }
    }
}

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

impl UnpackValue<'_> for bool {
    fn expected() -> String {
        "bool".to_owned()
    }

    fn unpack_value(value: Value) -> Option<Self> {
        value.unpack_bool()
    }
}

/// Define the bool type
impl StarlarkValue<'_> for StarlarkBool {
    starlark_type!(BOOL_TYPE);

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

    fn to_int(&self) -> anyhow::Result<i32> {
        Ok(if self.0 { 1 } else { 0 })
    }
    fn to_bool(&self) -> bool {
        self.0
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        hasher.write_u8(if self.0 { 1 } else { 0 });
        Ok(())
    }

    fn equals(&self, other: Value) -> anyhow::Result<bool> {
        // We always compare values for pointer equality before calling `equals`,
        // and there are only two instances of `StarlarkBool`.
        // So if we are here, values are definitely not equal.
        debug_assert!(!matches!(other.unpack_bool(), Some(other) if other == self.0));
        Ok(false)
    }

    fn compare(&self, other: Value) -> anyhow::Result<Ordering> {
        if let Some(other) = other.unpack_bool() {
            Ok(self.0.cmp(&other))
        } else {
            ValueError::unsupported_with(self, "<>", other)
        }
    }
}

impl<'v> StarlarkValueBasic<'v> for StarlarkBool {
    fn get_hash(&self) -> StarlarkHashValue {
        // These constants are just two random numbers.
        StarlarkHashValue::new_unchecked(if self.0 { 0xa4acba08 } else { 0x71e8ba71 })
    }
}
