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

use allocative::Allocative;
use derive_more::Display;
use maplit::hashmap;
use serde::Serialize;
use serde::Serializer;
use starlark_derive::starlark_module;
use starlark_derive::Freeze;
use starlark_derive::NoSerialize;
use starlark_derive::StarlarkDocs;
use starlark_derive::Trace;

use crate as starlark;
use crate::any::ProvidesStaticType;
use crate::coerce::Coerce;
use crate::docs::get_registered_starlark_docs;
use crate::docs::DocItem;
use crate::docs::DocMember;
use crate::docs::DocString;
use crate::docs::DocStringKind;
use crate::environment::Methods;
use crate::environment::MethodsBuilder;
use crate::environment::MethodsStatic;
use crate::starlark_complex_value;
use crate::starlark_simple_value;
use crate::starlark_type;
use crate::values::StarlarkValue;
use crate::values::ValueLike;
use crate::wasm::is_wasm;

/// Main module docs
#[starlark_module]
fn object_docs_1(_: &mut MethodsBuilder) {
    /// Returns the string "foo"
    #[starlark(attribute)]
    fn foo(this: &TestExample) -> anyhow::Result<String> {
        Ok("foo".to_owned())
    }
}

#[derive(
    Debug,
    Display,
    ProvidesStaticType,
    NoSerialize,
    StarlarkDocs,
    Allocative
)]
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

#[derive(
    Clone,
    Debug,
    Coerce,
    Display,
    Trace,
    Freeze,
    ProvidesStaticType,
    StarlarkDocs,
    Allocative
)]
#[repr(C)]
struct ComplexTestExampleGen<V>(V);

impl<V> Serialize for ComplexTestExampleGen<V>
where
    V: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.0.serialize(serializer)
    }
}

starlark_complex_value!(ComplexTestExample);

impl<'v, T: ValueLike<'v> + 'v + ProvidesStaticType<'v>> StarlarkValue<'v>
    for ComplexTestExampleGen<T>
where
    Self: ProvidesStaticType<'v>,
{
    starlark_type!("ComplexTestExample");

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
    if is_wasm() {
        // `inventory` doesn't work on wasm.
        return;
    }

    let docs = get_registered_starlark_docs()
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
                DocMember::Property(p) if name == "foo" => Some(p.docs.clone()),
                _ => None,
            })
            .unwrap()
    );
    assert!(docs.custom_attrs.is_empty());
}

#[test]
fn test_derive_docs_on_complex_values() {
    if is_wasm() {
        // `inventory` doesn't work on wasm.
        return;
    }

    let complex_docs = get_registered_starlark_docs()
        .into_iter()
        .find(|d| d.id.name == "ComplexTestExample")
        .unwrap();
    let complex_obj = match complex_docs.item {
        DocItem::Object(o) => o,
        _ => panic!("Expected object as docitem"),
    };

    assert_eq!(
        DocString::from_docstring(DocStringKind::Rust, "Main module docs"),
        complex_obj.docs
    );
    assert_eq!(
        DocString::from_docstring(DocStringKind::Rust, "Returns the string \"foo\""),
        complex_obj
            .members
            .iter()
            .find_map(|(name, m)| match m {
                DocMember::Property(p) if name == "foo" => Some(p.docs.clone()),
                _ => None,
            })
            .unwrap()
    );
    assert!(complex_docs.custom_attrs.is_empty());
}

/// Main module docs
#[starlark_module]
fn object_docs_2(_: &mut MethodsBuilder) {}

#[derive(
    Debug,
    Display,
    ProvidesStaticType,
    NoSerialize,
    StarlarkDocs,
    Allocative
)]
#[starlark_docs(key = "value", key2 = "value2")]
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
    if is_wasm() {
        // `inventory` doesn't work on wasm.
        return;
    }

    let docs = get_registered_starlark_docs()
        .into_iter()
        .find(|d| d.id.name == "TestAttrExample")
        .unwrap();
    let expected_attrs = hashmap! {
        "key".to_owned()=> "value".to_owned(),
        "key2".to_owned()=> "value2".to_owned(),
    };
    assert_eq!(expected_attrs, docs.custom_attrs);
}
