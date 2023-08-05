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

use starlark_derive::starlark_module;

use crate as starlark;
use crate::environment::GlobalsBuilder;
use crate::values::typing::type_compiled::compiled::TypeCompiled;
use crate::values::Heap;
use crate::values::Value;

#[starlark_module]
pub(crate) fn register_eval_type(globals: &mut GlobalsBuilder) {
    /// Create a runtime type object which can be used to check if a value matches the given type.
    fn eval_type<'v>(
        #[starlark(require = pos)] ty: Value<'v>,
        heap: &'v Heap,
    ) -> anyhow::Result<TypeCompiled<Value<'v>>> {
        TypeCompiled::new(ty, heap)
    }

    /// Check if a value matches the given type.
    fn isinstance<'v>(
        #[starlark(require = pos)] value: Value<'v>,
        #[starlark(require = pos)] ty: Value<'v>,
        heap: &'v Heap,
    ) -> anyhow::Result<bool> {
        Ok(TypeCompiled::new(ty, heap)?.matches(value))
    }
}
