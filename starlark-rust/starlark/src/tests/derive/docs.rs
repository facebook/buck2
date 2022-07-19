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

use derive_more::Display;
use gazebo::any::ProvidesStaticType;
use starlark_derive::StarlarkDocs;

use crate as starlark;
use crate::environment::Methods;
use crate::environment::MethodsBuilder;
use crate::environment::MethodsStatic;
use crate::values::docs::get_registered_docs;
use crate::values::docs::DocItem;
use crate::values::docs::DocString;
use crate::values::docs::DocStringKind;
use crate::values::docs::Member;
use crate::values::StarlarkValue;

/// Main module docs
#[starlark_module]
fn object_docs_1(_: &mut MethodsBuilder) {
    /// Returns the string "foo"
    #[starlark(attribute)]
    fn foo(this: &TestExample) -> anyhow::Result<String> {
        Ok("foo".to_owned())
    }
}

#[derive(Debug, Display, ProvidesStaticType, NoSerialize, StarlarkDocs)]
struct TestExample {}

starlark_simple_value!(TestExample);

impl<'v> StarlarkValue<'v> for TestExample {
    starlark_type!("TestExample");

    fn get_methods() -> Option<&'static Methods>
    where
        Self: Sized,
    {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(object_docs_1)
    }
}

#[test]
fn test_derive_docs() {
    let docs = get_registered_docs()
        .into_iter()
        .find(|d| d.id.name == "TestExample")
        .unwrap();
    let obj = match docs.item {
        DocItem::Object(o) => o,
        _ => panic!("Expected object as docitem"),
    };

    assert_eq!(
        DocString::from_docstring(DocStringKind::Rust, "Main module docs"),
        obj.docs
    );
    assert_eq!(
        DocString::from_docstring(DocStringKind::Rust, "Returns the string \"foo\""),
        obj.members
            .iter()
            .find_map(|(name, m)| match m {
                Member::Property(p) if name == "foo" => Some(p.docs.clone()),
                _ => None,
            })
            .unwrap()
    );
    assert!(docs.custom_attrs.is_empty());
}

/// Main module docs
#[starlark_module]
fn object_docs_2(_: &mut MethodsBuilder) {}

#[derive(Debug, Display, ProvidesStaticType, NoSerialize, StarlarkDocs)]
#[starlark_docs_attrs(key = "value", key2 = "value2")]
struct TestAttrExample {}

starlark_simple_value!(TestAttrExample);

impl<'v> StarlarkValue<'v> for TestAttrExample {
    starlark_type!("TestAttrExample");

    fn get_methods() -> Option<&'static Methods>
    where
        Self: Sized,
    {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(object_docs_2)
    }
}

#[test]
fn test_derive_docs_custom_attrs() {
    let docs = get_registered_docs()
        .into_iter()
        .find(|d| d.id.name == "TestAttrExample")
        .unwrap();
    let expected_attrs = hashmap! {
        "key".to_owned()=> "value".to_owned(),
        "key2".to_owned()=> "value2".to_owned(),
    };
    assert_eq!(expected_attrs, docs.custom_attrs);
}
