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

use crate::{collections::StarlarkHashValue, values::StarlarkValue};

/// Implemented by "basic" (non-heap allocated) types like `int` or `NoneType`.
pub(crate) trait StarlarkValueBasic<'v>: StarlarkValue<'v> {
    fn get_hash(&self) -> StarlarkHashValue;
}
