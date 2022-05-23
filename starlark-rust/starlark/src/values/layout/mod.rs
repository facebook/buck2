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

// Possible optimisations:
// Encoding none, bool etc in the pointer of frozen value

mod arena;
mod avalue;
mod const_frozen_string;
mod fast_cell;
pub(crate) mod heap;
pub(crate) mod identity;
mod pointer;
pub(crate) mod static_string;
pub(crate) mod typed;
pub(crate) mod value;
pub(crate) mod value_captured;
pub(crate) mod value_not_special;
pub(crate) mod vtable;
