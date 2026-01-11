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

//! Handle special "unbound" globals: methods or attributes.

use std::fmt;
use std::fmt::Debug;
use std::fmt::Formatter;

use crate::eval::Arguments;
use crate::eval::Evaluator;
use crate::eval::runtime::frame_span::FrameSpan;
use crate::values::FrozenRef;
use crate::values::FrozenValue;
use crate::values::FrozenValueTyped;
use crate::values::Heap;
use crate::values::Value;
use crate::values::ValueLike;
use crate::values::function::BoundMethodGen;
use crate::values::function::NativeAttribute;
use crate::values::function::NativeMethod;

/// A value or an unbound method or unbound attribute.
#[derive(Clone)]
pub(crate) enum UnboundValue {
    /// A method with `this` unbound.
    Method(FrozenValueTyped<'static, NativeMethod>),
    /// An attribute with `this` unbound.
    Attr(FrozenValueTyped<'static, NativeAttribute>),
}

impl Debug for UnboundValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("MaybeUnboundValue").finish_non_exhaustive()
    }
}

impl UnboundValue {
    #[inline]
    pub(crate) fn to_frozen_value(&self) -> FrozenValue {
        match self {
            UnboundValue::Method(m) => m.to_frozen_value(),
            UnboundValue::Attr(a) => a.to_frozen_value(),
        }
    }

    /// Bind this object to given `this` value.
    #[inline]
    pub(crate) fn bind<'v>(&self, this: Value<'v>, heap: Heap<'v>) -> crate::Result<Value<'v>> {
        match self {
            UnboundValue::Method(m) => {
                Ok(heap.alloc_complex(BoundMethodGen::new(this.to_value(), *m)))
            }
            UnboundValue::Attr(a) => a.invoke(this, heap),
        }
    }

    #[inline]
    pub(crate) fn invoke_method<'v>(
        &self,
        this: Value<'v>,
        span: FrozenRef<'static, FrameSpan>,
        args: &Arguments<'v, '_>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> crate::Result<Value<'v>> {
        eval.with_call_stack(
            self.to_frozen_value().to_value(),
            Some(span),
            |eval| match self {
                UnboundValue::Method(m) => m.function.invoke(eval, this, args),
                UnboundValue::Attr(a) => a.invoke(this, eval.heap()),
            },
        )
    }
}
