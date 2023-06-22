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

use maplit::btreemap;
use once_cell::sync::Lazy;

use crate::codemap::ResolvedFileSpan;
use crate::environment::Globals;
use crate::eval::compiler::EvalException;
use crate::stdlib::LibraryExtension;
use crate::syntax::AstModule;
use crate::syntax::Dialect;
use crate::typing::oracle::traits::OracleNoAttributes;
use crate::typing::oracle::traits::OracleNoBuiltins;
use crate::typing::oracle::traits::OracleSeq;
use crate::typing::oracle::traits::TypingAttr;
use crate::typing::ty::TyStruct;
use crate::typing::Interface;
use crate::typing::OracleDocs;
use crate::typing::OracleStandard;
use crate::typing::Param;
use crate::typing::Ty;
use crate::typing::TyFunction;
use crate::typing::TypingOracle;

fn mk_oracle() -> impl TypingOracle {
    static ORACLE: Lazy<OracleSeq<Box<dyn TypingOracle + Send + Sync + 'static>>> =
        Lazy::new(|| {
            let standard = OracleStandard::new(LibraryExtension::all());
            OracleSeq(vec![
                Box::new(standard),
                Box::new(OracleNoBuiltins),
                Box::new(OracleNoAttributes),
            ])
        });
    &*ORACLE
}

#[test]
fn test_oracle() {
    let o = mk_oracle();

    let mut b = OracleDocs::new();
    b.add_module(&Globals::extended().documentation());

    assert_eq!(
        o.attribute(&Ty::string(), TypingAttr::Regular("removeprefix")),
        Some(Ok(Ty::function(
            vec![Param::pos_only(Ty::string())],
            Ty::string()
        )))
    );
    assert_eq!(
        b.builtin("hash"),
        Ok(Ty::function(vec![Param::pos_only(Ty::string())], Ty::int()))
    );
    assert_eq!(
        b.builtin("any"),
        Ok(Ty::function(
            vec![Param::pos_only(Ty::iter(Ty::Any))],
            Ty::bool()
        ))
    );
    assert_eq!(
        b.builtin("fail"),
        Ok(Ty::function(vec![Param::args(Ty::Any)], Ty::Never))
    );
    assert_eq!(b.builtin("not_a_symbol"), Err(()));

    fn get_type(x: &Result<Ty, ()>) -> &str {
        match x {
            Ok(Ty::Function(TyFunction { type_attr, .. })) => type_attr.as_str(),
            _ => "",
        }
    }

    assert_eq!(get_type(&b.builtin("int")), "int");
    assert_eq!(get_type(&b.builtin("str")), "string");
    assert_eq!(get_type(&b.builtin("list")), "list");
    assert_eq!(get_type(&b.builtin("hash")), "");
}

#[derive(Default)]
struct TypeCheck {
    expect_errors: Vec<(String, ResolvedFileSpan)>,
    expect_approximations: Vec<String>,
    expect_interface: Vec<(String, Ty)>,
    loads: HashMap<String, Interface>,
}

impl TypeCheck {
    fn new() -> Self {
        Self::default()
    }

    fn error(mut self, err: &str, loc: &str) -> Self {
        self.expect_errors
            .push((err.to_owned(), ResolvedFileSpan::testing_parse(loc)));
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
            assert!(good, "Wanted:\n{want:?}\nGot:\n{got:?}");
        }

        fn assert_error_list(mut got: Vec<anyhow::Error>, want: &[(String, ResolvedFileSpan)]) {
            let mut good = got.len() == want.len();
            for (expect_err, expect_loc) in want {
                let before = got.len();
                got.retain(|x| {
                    !(x.to_string().contains(expect_err)
                        && EvalException::testing_loc(x) == *expect_loc)
                });
                good = good && before == got.len() + 1;
            }
            assert!(good, "Wanted:\n{want:?}\nGot:\n{got:?}");
        }

        let (errors, _, interface, approximations) =
            AstModule::parse("filename", code.to_owned(), &Dialect::Extended)
                .unwrap()
                .typecheck(&mk_oracle(), &Globals::extended(), &self.loads);
        assert_list(approximations, &self.expect_approximations);
        assert_error_list(errors, &self.expect_errors);
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
        .error(
            r#"Expected type `str.type` but got `int.type`"#,
            "filename:1:1-8",
        )
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
        .error(
            r#"Expected type `{str.type: ""}` but got `{int.type: str.type}`"#,
            "filename:4:7-15",
        )
        .check(
            r#"
def foo(**kwargs):
    pass
foo(**{1: "x"})
"#,
        );
}

#[test]
fn test_dot_type() {
    TypeCheck::new().check(
        r#"
def foo(x: list.type) -> bool.type:
    return type(x) == list.type
foo([1,2,3])
"#,
    );
    TypeCheck::new()
        .error(
            r#"Expected type `[""]` but got `bool.type`"#,
            "filename:4:1-10",
        )
        .error(
            r#"Expected type `["never"]` but got `str.type`"#,
            "filename:3:12-25",
        )
        .check(
            r#"
def foo(x: list.type) -> bool.type:
    return type(x) == []
foo(True)
"#,
        );
}

#[test]
fn test_special_function_zip() {
    TypeCheck::new()
        .ty(
            "x",
            Ty::list(Ty::Tuple(vec![Ty::int(), Ty::bool(), Ty::string()])),
        )
        .check(
            r#"
x = zip([1,2], [True, False], ["a", "b"])
"#,
        );
}

#[test]
fn test_special_function_struct() {
    TypeCheck::new()
        .ty(
            "x",
            Ty::Struct(TyStruct {
                fields: btreemap! {"a".to_owned() => Ty::int(), "b".to_owned() => Ty::string()},
                extra: false,
            }),
        )
        .check(
            r#"
x = struct(a = 1, b = "test")
"#,
        );
}
