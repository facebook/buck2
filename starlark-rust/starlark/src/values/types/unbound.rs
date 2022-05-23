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

use crate::values::{
    function::{BoundMethodGen, NativeAttribute, NativeMethod},
    layout::value_not_special::FrozenValueNotSpecial,
    FrozenValueTyped, Heap, Value, ValueLike,
};

/// A value or an unbound method or unbound attribute.
pub(crate) enum MaybeUnboundValue {
    /// A method with `this` unbound.
    Method(FrozenValueTyped<'static, NativeMethod>),
    /// An attribute with `this` unbound.
    Attr(FrozenValueTyped<'static, NativeAttribute>),
}

impl MaybeUnboundValue {
    /// Bind this object to given `this` value.
    pub(crate) fn bind<'v>(self, this: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        match self {
            MaybeUnboundValue::Method(m) => {
                Ok(heap.alloc_complex(BoundMethodGen::new(this.to_value(), m)))
            }
            MaybeUnboundValue::Attr(a) => a.call(this, heap),
        }
    }
}

impl MaybeUnboundValue {
    /// Split into variants.
    #[allow(clippy::same_functions_in_if_condition)] // False positive
    pub(crate) fn new(value: FrozenValueNotSpecial) -> MaybeUnboundValue {
        // TODO(nga): this can be a little faster if we do downcast of `FrozenValueNotSpecial`
        //   instead of converting it to `FrozenValue` first.
        if let Some(method) = FrozenValueTyped::new(value.to_frozen_value()) {
            MaybeUnboundValue::Method(method)
        } else if let Some(attr) = FrozenValueTyped::new(value.to_frozen_value()) {
            MaybeUnboundValue::Attr(attr)
        } else {
            unreachable!("not a member: {}", value);
        }
    }
}
