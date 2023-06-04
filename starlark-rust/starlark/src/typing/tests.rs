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
use std::fmt::Display;

use once_cell::sync::Lazy;

use crate::stdlib::LibraryExtension;
use crate::syntax::AstModule;
use crate::syntax::Dialect;
use crate::typing::Interface;
use crate::typing::OracleNoBuiltins;
use crate::typing::OracleStandard;
use crate::typing::Param;
use crate::typing::Ty;
use crate::typing::TypingOracle;

fn mk_oracle() -> impl TypingOracle {
    static ORACLE: Lazy<Vec<Box<dyn TypingOracle + Send + Sync + 'static>>> = Lazy::new(|| {
        let standard = OracleStandard::new(LibraryExtension::all());
        vec![Box::new(standard), Box::new(OracleNoBuiltins)]
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

#[derive(Default)]
struct TypeCheck {
    expect_errors: Vec<String>,
    expect_approximations: Vec<String>,
    expect_interface: Vec<(String, Ty)>,
    loads: HashMap<String, Interface>,
}

impl TypeCheck {
    fn new() -> Self {
        Self::default()
    }

    fn error(mut self, x: &str) -> Self {
        self.expect_errors.push(x.to_owned());
        self
    }

    fn ty(mut self, name: &str, ty: Ty) -> Self {
        self.expect_interface.push((name.to_owned(), ty));
        self
    }

    fn load(mut self, file: &str, interface: Interface) -> Self {
        self.loads.insert(file.to_owned(), interface);
        self
    }

    fn check(&self, code: &str) -> Interface {
        fn assert_list<T: Display>(got: Vec<T>, want: &[String]) {
            let mut got = got.iter().map(|x| format!("{x:#}")).collect::<Vec<_>>();
            let mut good = got.len() == want.len();
            for w in want {
                let before = got.len();
                got.retain(|x| !x.contains(w));
                good = good && before == got.len() + 1;
            }
            assert!(good, "Wanted {want:?}, got {got:?}");
        }

        let (errors, _, interface, approximations) =
            AstModule::parse("filename", code.to_owned(), &Dialect::Extended)
                .unwrap()
                .typecheck(&mk_oracle(), &self.loads);
        assert_list(approximations, &self.expect_approximations);
        assert_list(errors, &self.expect_errors);
        for (k, v) in &self.expect_interface {
            assert_eq!(interface.get(k), Some(v));
        }
        interface
    }
}

#[test]
fn test_success() {
    TypeCheck::default().ty("y", Ty::int()).check(
        r#"
def foo(x: str.type) -> str.type:
    return x.removeprefix("test")
y = hash(foo("magic"))
   "#,
    );
}

#[test]
fn test_failure() {
    TypeCheck::new()
        .error(r#"Expected type `"string"` but got `"int"`, at filename:1:1-8"#)
        .check(r#"hash(1)"#);
}

#[test]
fn test_load() {
    let interface = TypeCheck::new().check(
        r#"
def foo(x: [bool.type]) -> str.type:
    return "test"
   "#,
    );
    TypeCheck::new()
        .load("foo.bzl", interface)
        .ty("res", Ty::list(Ty::string()))
        .check(
            r#"
load("foo.bzl", "foo")
res = [foo([])]
"#,
        );
}

/// Test things that have previous claimed incorrectly they were type errors
#[test]
fn test_false_negative() {
    TypeCheck::new().check(r#"fail("Expected variable expansion in string: `{}`".format("x"))"#);
}

#[test]
fn test_type_kwargs() {
    TypeCheck::new()
        .error(r#"Expected type `{"string": ""}` but got `{"int": "string"}`, at filename:4:7-15"#)
        .check(
            r#"
def foo(**kwargs):
    pass
foo(**{1: "x"})
"#,
        );
}
