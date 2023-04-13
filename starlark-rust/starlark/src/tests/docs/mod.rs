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
use serde::Serialize;
use starlark_derive::starlark_module;

use crate as starlark;
use crate::any::ProvidesStaticType;
use crate::assert;
use crate::docs::DocItem;
use crate::environment::GlobalsBuilder;
use crate::environment::Methods;
use crate::environment::MethodsBuilder;
use crate::environment::MethodsStatic;
use crate::starlark_type;
use crate::tests::docs::golden::docs_golden_test;
use crate::values::StarlarkValue;
use crate::values::Value;

mod golden;
mod rustdocs;

const STARLARK_CODE: &str = r#"
"""
This is the summary of the module's docs

Some extra details can go here,
    and indentation is kept as expected
"""

def f1(a, b: "string", c:"int" = 5, *, d:"string" = "some string", **kwargs) -> ["string"]:
    """
    Summary line goes here

    Args:
        a: The docs for a
        b: The docs for b
        c: The docs for c, but these
           go onto two lines
        **kwargs: Docs for the keyword args

    Returns:
        A string repr of the args
    """
    return [str((a, b, c, d, repr(kwargs)))]

def f2(a, *args: ["string"]):
    """
    This is a function with *args, and no return type

    Args:
        *args: Only doc this arg
    """
    return None

def f3(a: "string") -> "string":
    return a

def f4(a: "string") -> "string":
    """ This is a docstring with no 'Args:' section """
    return a

# Not public, so shouldn't show up
def _do_not_export():
    pass
"#;

/// These are where the module docs go
#[starlark_module]
fn module(builder: &mut GlobalsBuilder) {
    const MAGIC: i32 = 42;

    /// Docs for func1
    ///
    /// # Arguments
    ///     * `foo`: Docs for foo
    ///
    /// # Returns
    /// The string 'func1'
    fn func1(foo: String) -> anyhow::Result<String> {
        let _ignore = foo;
        Ok("func1".to_owned())
    }

    fn func2() -> anyhow::Result<String> {
        Ok("func2".to_owned())
    }

    /// A function with only positional arguments.
    fn func3(
        #[starlark(require = pos)] a1: i32,
        #[starlark(require = pos)] a2: Option<i32>,
        #[starlark(require = pos, default = 1)] step: i32,
    ) -> anyhow::Result<String> {
        let _x = (a1, a2, step);
        Ok("func3".to_owned())
    }
}

#[derive(ProvidesStaticType, Debug, Display, Allocative, Serialize)]
#[display(format = "obj")]
struct Obj;

impl<'v> StarlarkValue<'v> for Obj {
    starlark_type!("obj");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(object)
    }
}

/// These are where the module docs go
#[starlark_module]
fn object(builder: &mut MethodsBuilder) {
    /// Docs for attr1
    #[starlark(attribute)]
    fn attr1<'v>(this: Value<'v>) -> anyhow::Result<String> {
        Ok("attr1".to_owned())
    }

    #[starlark(attribute)]
    fn attr2<'v>(this: Value<'v>) -> anyhow::Result<String> {
        Ok("attr2".to_owned())
    }

    /// Docs for func1
    ///
    /// # Arguments
    ///     * `foo`: Docs for foo
    ///
    /// # Returns
    /// The string 'func1'
    fn func1<'v>(this: Value<'v>, foo: String) -> anyhow::Result<String> {
        let _ignore = (this, foo);
        Ok("func1".to_owned())
    }

    fn func2<'v>(this: Value<'v>) -> anyhow::Result<String> {
        let _ = this;
        Ok("func2".to_owned())
    }
}

#[test]
fn golden_docs_starlark() {
    let res = docs_golden_test(
        "starlark",
        DocItem::Module(assert::pass_module(STARLARK_CODE).documentation()),
    );
    assert!(!res.contains("_do_not_export"))
}

#[test]
fn golden_docs_module() {
    docs_golden_test(
        "module",
        GlobalsBuilder::new().with(module).build().documentation(),
    );
}

#[test]
fn golden_docs_object() {
    docs_golden_test("object", Obj.documentation().unwrap());
}
