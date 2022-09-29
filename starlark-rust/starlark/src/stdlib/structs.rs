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

//! Implementation of `struct` function.
use crate as starlark;
use crate::environment::GlobalsBuilder;
use crate::eval::Arguments;
use crate::values::structs::Struct;
use crate::values::Heap;

#[starlark_module]
pub fn global(builder: &mut GlobalsBuilder) {
    #[starlark(type = Struct::TYPE)]
    fn r#struct<'v>(args: &Arguments<'v, '_>, heap: &'v Heap) -> anyhow::Result<Struct<'v>> {
        args.no_positional_args(heap)?;
        // TODO(nga): missing optimization: practically most `struct` invocations are
        //   performed with fixed named arguments, e.g. `struct(a = 1, b = 2)`.
        //   In this case we can avoid allocating the map, but instead
        //   allocate field index once at compilation time and store field values in a vector.
        Ok(Struct::new(args.names_map()?))
    }
}
