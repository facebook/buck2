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

use crate as starlark;
use crate::assert::Assert;
use crate::environment::GlobalsBuilder;

const FOO_TYPE: &str = "Foo";

#[starlark_module]
fn type_annotation_functions(globals: &mut GlobalsBuilder) {
    #[starlark(type = FOO_TYPE)]
    fn foo(x: i32) -> anyhow::Result<i32> {
        Ok(x)
    }
}

#[test]
fn test_type_annotation() {
    let mut a = Assert::new();
    a.globals_add(type_annotation_functions);
    a.eq("'Foo'", "foo.type");
}
