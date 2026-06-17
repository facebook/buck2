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

use allocative::Allocative;
use dupe::Dupe;

use crate::eval::Arguments;
use crate::eval::Evaluator;
use crate::pagable::StarlarkDeserialize;
use crate::pagable::StarlarkDeserializeContext;
use crate::pagable::StarlarkSerialize;
use crate::pagable::StarlarkSerializeContext;
use crate::values::FrozenValueTyped;
use crate::values::Value;
use crate::values::function::NativeFunc;
use crate::values::function::NativeFunction;

/// Pointer to a native function optimized for bytecode execution.
#[derive(Copy, Clone, Dupe, Allocative)]
pub(crate) struct BcNativeFunction {
    fun: FrozenValueTyped<'static, NativeFunction>,
    /// Copy function here from `fun` to avoid extra dereference when calling.
    imp: &'static NativeFunc,
}

// Only the frozen value is on the wire; `imp` is recomputed from `fun` on
// deserialize via `BcNativeFunction::new`.
impl StarlarkSerialize for BcNativeFunction {
    fn starlark_serialize(&self, ctx: &mut dyn StarlarkSerializeContext) -> crate::Result<()> {
        self.fun.starlark_serialize(ctx)
    }
}

impl StarlarkDeserialize for BcNativeFunction {
    fn starlark_deserialize(ctx: &mut dyn StarlarkDeserializeContext<'_>) -> crate::Result<Self> {
        let fun = FrozenValueTyped::<'static, NativeFunction>::starlark_deserialize(ctx)?;
        Ok(BcNativeFunction::new(fun))
    }
}

impl BcNativeFunction {
    pub(crate) fn new(fun: FrozenValueTyped<'static, NativeFunction>) -> BcNativeFunction {
        BcNativeFunction {
            fun,
            imp: &fun.as_ref().function,
        }
    }

    #[inline]
    pub(crate) fn fun(&self) -> FrozenValueTyped<'static, NativeFunction> {
        self.fun
    }

    #[inline]
    pub(crate) fn to_value<'v>(&self) -> Value<'v> {
        self.fun.to_value()
    }

    #[inline]
    pub(crate) fn invoke<'v>(
        &self,
        args: &Arguments<'v, '_>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> crate::Result<Value<'v>> {
        self.imp.invoke(eval, args)
    }
}
