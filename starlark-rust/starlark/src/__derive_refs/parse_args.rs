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

use std::cell::Cell;

use crate::eval::Arguments;
use crate::eval::ParametersSpec;
use crate::values::FrozenValue;
use crate::values::Heap;
use crate::values::Value;

/// Collect `N` arguments.
///
/// This function is called by generated code.
#[inline]
pub fn parse_signature<'v, const N: usize>(
    parser: &ParametersSpec<FrozenValue>,
    args: &Arguments<'v, '_>,
    heap: &'v Heap,
) -> crate::Result<[Cell<Option<Value<'v>>>; N]> {
    parser.collect_into(args, heap)
}

/// Parse positional-only arguments, all required.
#[inline(always)]
pub fn parse_positional_required<'v, const N: usize>(
    args: &Arguments<'v, '_>,
    heap: &'v Heap,
) -> crate::Result<[Value<'v>; N]> {
    args.no_named_args()?;
    args.positional(heap)
}

/// Parse positional-only arguments, required and optional.
#[inline(always)]
pub fn parse_positional<'v, const R: usize, const O: usize>(
    args: &Arguments<'v, '_>,
    heap: &'v Heap,
) -> crate::Result<([Value<'v>; R], [Option<Value<'v>>; O])> {
    args.no_named_args()?;
    args.optional(heap)
}
