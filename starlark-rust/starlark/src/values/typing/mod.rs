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

//! Typechecker-related types.

pub(crate) mod any;
pub(crate) mod callable;
pub(crate) mod globals;
pub(crate) mod iter;
pub mod macro_refs;
pub(crate) mod never;
pub(crate) mod type_compiled;

pub use crate::values::typing::callable::StarlarkCallable;
pub use crate::values::typing::iter::StarlarkIter;
pub use crate::values::typing::never::StarlarkNever;
