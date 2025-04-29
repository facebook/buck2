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

use starlark_derive::starlark_module;

use crate as starlark;
use crate::assert::Assert;
use crate::environment::GlobalsBuilder;

#[starlark_module]
fn generic_builder<T: Default, U>(globals: &mut GlobalsBuilder)
where
    U: std::fmt::Display + Default,
{
    const MY_STR: &str = &U::default().to_string();
}

#[test]
fn test_generic_builder() {
    let mut a = Assert::new();
    a.globals_add(generic_builder::<u8, u8>);
    a.eq("\"0\"", "MY_STR");
}
