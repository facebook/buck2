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

//! Special value which holds a reference to actual value.
//! This is used to implement variable capture by nested functions.
//!
//! `Value` holding `ValueCaptured` is equivalent to `Box<Option<Value>>`.

use std::cell::Cell;

use derive_more::Display;
use gazebo::any::ProvidesStaticType;
use gazebo::prelude::*;

use crate as starlark;
use crate::values::Freeze;
use crate::values::Freezer;
use crate::values::FrozenValue;
use crate::values::StarlarkValue;
use crate::values::Value;
use crate::values::ValueLike;

#[derive(Debug, Trace, ProvidesStaticType, Display, NoSerialize)]
#[display(fmt = "{:?}", self)] // This type should never be user visible
#[repr(transparent)]
pub(crate) struct ValueCaptured<'v>(pub Cell<Option<Value<'v>>>);

#[derive(Debug, ProvidesStaticType, Display, NoSerialize)]
#[display(fmt = "{:?}", self)] // Type is not user visible
#[repr(transparent)]
pub(crate) struct FrozenValueCaptured(Option<FrozenValue>);

impl<'v> StarlarkValue<'v> for ValueCaptured<'v> {
    starlark_type!("value_captured");
}

impl<'v> StarlarkValue<'v> for FrozenValueCaptured {
    starlark_type!("value_captured");
}

impl<'v> ValueCaptured<'v> {
    pub(crate) fn set(&self, value: Value<'v>) {
        debug_assert!(value.downcast_ref::<ValueCaptured>().is_none());
        debug_assert!(value.downcast_ref::<FrozenValueCaptured>().is_none());
        self.0.set(Some(value));
    }
}

impl<'v> Freeze for ValueCaptured<'v> {
    type Frozen = FrozenValueCaptured;

    fn freeze(self, freezer: &Freezer) -> anyhow::Result<FrozenValueCaptured> {
        Ok(FrozenValueCaptured(
            self.0.get().into_try_map(|v| freezer.freeze(v))?,
        ))
    }
}

pub(crate) fn value_captured_get<'v>(value_captured: Value<'v>) -> Option<Value<'v>> {
    if let Some(value_captured) = value_captured.unpack_frozen() {
        value_captured
            .downcast_ref::<FrozenValueCaptured>()
            .expect("not a ValueCaptured")
            .0
            .map(|v| v.to_value())
    } else {
        value_captured
            .downcast_ref::<ValueCaptured>()
            .expect("not a ValueCaptured")
            .0
            .get()
    }
}
