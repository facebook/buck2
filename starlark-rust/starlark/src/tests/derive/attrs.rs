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

use allocative::Allocative;
use derive_more::Display;
use gazebo::any::ProvidesStaticType;

use crate as starlark;
use crate::assert::Assert;
use crate::values::StarlarkAttrs;
use crate::values::StarlarkValue;

#[test]
fn test_derive_attrs() {
    #[derive(
        Debug,
        StarlarkAttrs,
        Display,
        ProvidesStaticType,
        NoSerialize,
        Allocative
    )]
    #[display(fmt = "{:?}", self)]
    struct Example {
        hello: String,
        #[starlark(skip)]
        answer: i64,
        #[starlark(clone)]
        nested: Nested,
    }
    starlark_simple_value!(Example);
    impl<'v> StarlarkValue<'v> for Example {
        starlark_type!("example");
        starlark_attrs!();
    }

    #[derive(
        Debug,
        Clone,
        StarlarkAttrs,
        Display,
        ProvidesStaticType,
        NoSerialize,
        Allocative
    )]
    #[display(fmt = "{}", foo)]
    struct Nested {
        foo: String,
    }
    starlark_simple_value!(Nested);
    impl<'v> StarlarkValue<'v> for Nested {
        starlark_type!("nested");
        starlark_attrs!();
    }

    let mut a = Assert::new();
    a.globals_add(|gb| {
        gb.set(
            "example",
            Example {
                hello: "world".to_owned(),
                answer: 42,
                nested: Nested {
                    foo: "bar".to_owned(),
                },
            },
        )
    });
    a.eq("example.hello", "\"world\"");
    a.eq("dir(example)", "[\"hello\", \"nested\"]");
    a.is_true("not hasattr(example, \"answer\")");
    a.eq("example.nested.foo", "\"bar\"");
}
