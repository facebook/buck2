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

use gazebo::dupe::Dupe;

use crate::eval::runtime::call_stack::FrozenFileSpan;
use crate::eval::Arguments;
use crate::eval::Evaluator;
use crate::values::layout::vtable::AValueDyn;
use crate::values::stack_guard;
use crate::values::FrozenRef;
use crate::values::FrozenValue;
use crate::values::Value;

/// `FrozenValue` which is not `i32` or `str`.
#[derive(Copy, Clone, Dupe, Debug, derive_more::Display)]
#[display(fmt = "{}", .0)]
pub(crate) struct FrozenValueNotSpecial(FrozenValue);

impl FrozenValueNotSpecial {
    #[inline]
    pub(crate) fn new(value: FrozenValue) -> Option<FrozenValueNotSpecial> {
        if value.is_str() || value.unpack_int().is_some() {
            None
        } else {
            Some(FrozenValueNotSpecial(value))
        }
    }

    #[inline]
    pub(crate) fn to_frozen_value(self) -> FrozenValue {
        self.0
    }

    #[inline]
    pub(crate) fn to_value<'v>(self) -> Value<'v> {
        self.0.to_value()
    }

    #[inline]
    fn get_ref<'v>(self) -> AValueDyn<'v> {
        // SAFETY: we checked in constructor that it is not a str or i32.
        unsafe { self.0.0.unpack_ptr_no_int_no_str_unchecked().unpack() }
    }

    pub(crate) fn equals(self, other: Value) -> anyhow::Result<bool> {
        if self.to_value().ptr_eq(other) {
            Ok(true)
        } else {
            let _guard = stack_guard::stack_guard()?;
            self.get_ref().equals(other)
        }
    }

    pub(crate) fn invoke_method<'v>(
        self,
        this: Value<'v>,
        location: FrozenRef<'static, FrozenFileSpan>,
        args: &Arguments<'v, '_>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        eval.with_call_stack(self.to_value(), Some(location), |eval| {
            self.get_ref()
                .invoke_method(self.to_value(), this, args, eval)
        })
    }
}
