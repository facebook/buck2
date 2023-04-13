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

use std::collections::HashMap;

use starlark_derive::starlark_module;
use starlark_map::small_map::SmallMap;

use crate as starlark;
use crate::assert;
use crate::docs::DocItem;
use crate::docs::DocMember;
use crate::environment::GlobalsBuilder;
use crate::eval::Arguments;
use crate::eval::Evaluator;
use crate::values::none::NoneType;
use crate::values::Heap;
use crate::values::StringValue;
use crate::values::Value;

#[starlark_module]
#[allow(unused_variables)] // Since this is for a test
fn globals(builder: &mut GlobalsBuilder) {
    fn simple(
        arg_int: i32,
        arg_bool: bool,
        arg_vec: Vec<&str>,
        arg_dict: SmallMap<String, (bool, i32)>,
    ) -> anyhow::Result<NoneType> {
        unimplemented!()
    }

    fn default_arg<'v>(
        arg1: Option<Value<'v>>,
        #[starlark(default = NoneType)] arg2: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Vec<String>> {
        unimplemented!()
    }

    fn args_kwargs<'v>(
        #[starlark(args)] args: Vec<Value<'v>>,
        #[starlark(kwargs)] kwargs: Value<'v>,
    ) -> anyhow::Result<NoneType> {
        unimplemented!()
    }

    #[starlark(return_type = "\"output\"")]
    fn custom_types<'v>(
        arg1: StringValue<'v>,
        #[starlark(type = "\"input\"")] arg2: Value<'v>,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        unimplemented!()
    }

    fn pos_named(
        #[starlark(require = pos)] arg1: i32,
        #[starlark(require = named)] arg2: i32,
    ) -> anyhow::Result<i32> {
        unimplemented!()
    }

    fn with_arguments(args: &Arguments) -> anyhow::Result<i32> {
        unimplemented!()
    }
}

/// Test that a Rust starlark_module produces the right documentation.
#[test]
fn test_rustdoc() {
    let got = GlobalsBuilder::new().with(globals).build();
    let expected = assert::pass_module(
        r#"
def args_kwargs(*args, **kwargs: "") -> None: pass
def custom_types(arg1: str.type, arg2: "input") -> "output": pass
def default_arg(arg1: [None, ""] = None, arg2: "" = None) -> [str.type]: pass
def pos_named(arg1: int.type, *, arg2: int.type) -> int.type: pass
def simple(arg_int: int.type, arg_bool: bool.type, arg_vec: [str.type], arg_dict: {str.type: (bool.type, int.type)}) -> None: pass
def with_arguments(*args, **kwargs) -> int.type: pass
"#,
    );

    fn unpack(x: DocItem) -> HashMap<String, DocItem> {
        match x {
            DocItem::Module(obj) => obj
                .members
                .into_iter()
                .filter_map(|(name, member)| match member {
                    DocMember::Property(_) => None,
                    DocMember::Function(f) => Some((name, DocItem::Function(f))),
                })
                .collect(),
            _ => HashMap::new(),
        }
    }

    fn cleanup_types(x: &str) -> String {
        x.replace("\\\"int\\\"", "int.type")
            .replace("\\\"bool\\\"", "bool.type")
            .replace("\\\"string\\\"", "str.type")
            .replace("Some(DocType { raw_type: \"\\\"\\\"\" })", "None")
    }

    let expected = expected.documentation().members;
    let got = unpack(got.documentation());
    assert_eq!(expected.len(), got.len());
    for (name, expected1) in expected.iter() {
        let got1 = got.get(name).unwrap();
        assert_eq!(
            cleanup_types(&format!("{:?}", expected1)),
            cleanup_types(&format!("{:?}", got1)),
            "Function {}",
            name
        );
    }
}
