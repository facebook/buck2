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

use std::collections::HashMap;

use once_cell::sync::Lazy;

use crate::docs::get_registered_starlark_docs;
use crate::environment::Globals;
use crate::syntax::AstModule;
use crate::syntax::Dialect;
use crate::typing::oracle::traits::TypingOracle;
use crate::typing::Approximation;
use crate::typing::Interface;
use crate::typing::OracleDocs;
use crate::typing::OracleNoBuiltins;
use crate::typing::Param;
use crate::typing::Ty;
use crate::typing::TypeMap;

fn mk_oracle() -> impl TypingOracle {
    static ORACLE: Lazy<Vec<Box<dyn TypingOracle + Send + Sync + 'static>>> = Lazy::new(|| {
        let mut docs = OracleDocs::new(&get_registered_starlark_docs());
        docs.add_object(&Globals::standard().documentation());
        vec![Box::new(docs), Box::new(OracleNoBuiltins)]
    });
    &*ORACLE
}

#[test]
fn test_oracle() {
    let o = mk_oracle();
    // TODO(ndmitchell): In both cases these _should_ be positional only, but the docs don't currently record that
    assert_eq!(
        o.attribute(&Ty::string(), "removeprefix"),
        Some(Ok(Ty::function(
            vec![Param::pos_or_name("prefix", Ty::string())],
            Ty::string()
        )))
    );
    assert_eq!(
        o.builtin("hash"),
        Some(Ok(Ty::function(
            vec![Param::pos_or_name("a", Ty::string())],
            Ty::int()
        )))
    );
    assert_eq!(o.builtin("not_a_symbol"), Some(Err(())))
}

fn typecheck(
    code: &str,
    loads: &HashMap<String, Interface>,
) -> (Vec<anyhow::Error>, TypeMap, Interface, Vec<Approximation>) {
    AstModule::parse("filename", code.to_owned(), &Dialect::Extended)
        .unwrap()
        .typecheck(&mk_oracle(), loads)
}

#[test]
fn test_success() {
    let (errs, _, interface, approx) = typecheck(
        r#"
def foo(x: str.type) -> str.type:
    return x.removeprefix("test")
y = hash(foo("magic"))
   "#,
        &HashMap::new(),
    );
    assert!(approx.is_empty());
    assert!(errs.is_empty());
    assert_eq!(interface.get("y").unwrap(), &Ty::int());
}

#[test]
fn test_failure() {
    let (errs, _, _, approx) = typecheck(
        r#"
hash(1)
   "#,
        &HashMap::new(),
    );
    assert!(approx.is_empty());
    assert_eq!(errs.len(), 1);
    assert_eq!(
        format!("{:#}", errs[0]),
        r#"Expected type `"string"` but got `"int"`, at filename:2:1-8"#
    );
}

#[test]
fn test_load() {
    let (errs, _, interface, approx) = typecheck(
        r#"
def foo(x: [bool.type]) -> str.type:
    return "test"
   "#,
        &HashMap::new(),
    );
    assert!(approx.is_empty());
    assert!(errs.is_empty());

    let (errs, _, interface, approx) = typecheck(
        r#"
load("foo.bzl", "foo")
res = [foo([])]
   "#,
        &hashmap!["foo.bzl".to_owned() => interface],
    );
    assert!(approx.is_empty());
    assert!(errs.is_empty());
    assert_eq!(interface.get("res").unwrap(), &Ty::list(Ty::string()));
}
