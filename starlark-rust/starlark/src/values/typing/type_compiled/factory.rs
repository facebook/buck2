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

use crate::typing::Ty;
use crate::values::typing::type_compiled::compiled::TypeCompiled;
use crate::values::typing::type_compiled::compiled::TypeCompiledImpl;
use crate::values::Heap;
use crate::values::Value;

/// Convert `TypeCompiledImpl` into `TypeCompiled`.
/// This is used to convert custom types to runtime types.
pub struct TypeCompiledFactory<'v> {
    heap: &'v Heap,
    ty: Ty,
}

impl<'v> TypeCompiledFactory<'v> {
    pub(crate) fn new(ty: Ty, heap: &'v Heap) -> TypeCompiledFactory<'v> {
        TypeCompiledFactory { heap, ty }
    }

    pub(crate) fn alloc(self, matcher: impl TypeCompiledImpl) -> TypeCompiled<Value<'v>> {
        TypeCompiled::alloc(matcher, self.ty, self.heap)
    }
}
