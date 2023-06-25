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
use std::fmt::Write;

use once_cell::sync::Lazy;

use crate::environment::Globals;
use crate::stdlib::LibraryExtension;
use crate::syntax::AstModule;
use crate::syntax::Dialect;
use crate::tests::golden_test_template::golden_test_template;
use crate::typing::oracle::traits::OracleNoAttributes;
use crate::typing::oracle::traits::OracleSeq;
use crate::typing::oracle::traits::TypingAttr;
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
            OracleSeq(vec![Box::new(standard), Box::new(OracleNoAttributes)])
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
    expect_interface: Vec<String>,
    loads: HashMap<String, Interface>,
}

impl TypeCheck {
    fn new() -> Self {
        Self::default()
    }

    fn ty(mut self, name: &str) -> Self {
        self.expect_interface.push(name.to_owned());
        self
    }

    fn load(mut self, file: &str, interface: Interface) -> Self {
        self.loads.insert(file.to_owned(), interface);
        self
    }

    fn check(&self, test_name: &str, code: &str) -> Interface {
        let (errors, _, interface, approximations) =
            AstModule::parse("filename", code.to_owned(), &Dialect::Extended)
                .unwrap()
                .typecheck(&mk_oracle(), &Globals::extended(), &self.loads);

        let mut output = String::new();
        writeln!(output, "Code:").unwrap();
        writeln!(output, "{}", code.trim()).unwrap();
        if errors.is_empty() {
            writeln!(output).unwrap();
            writeln!(output, "No errors.").unwrap();
        } else {
            for error in errors {
                writeln!(output).unwrap();
                writeln!(output, "Error:").unwrap();
                // Note we are using `:#` here instead of `:?` because
                // `:?` includes rust backtrace.
                // The issue: https://github.com/dtolnay/anyhow/issues/300
                writeln!(output, "{}", format!("{:#}", error).trim_end()).unwrap();
            }
        }

        if !approximations.is_empty() {
            writeln!(output).unwrap();
            writeln!(output, "Approximations:").unwrap();
            for appox in approximations {
                writeln!(output, "{}", appox).unwrap();
            }
        }

        if !self.expect_interface.is_empty() {
            writeln!(output).unwrap();
            writeln!(output, "Interfaces:").unwrap();
            for k in &self.expect_interface {
                let intf = interface.get(k).expect("no interface for key");
                writeln!(output, "{}: {}", k, intf).unwrap();
            }
        }

        golden_test_template(&format!("src/typing/golden/{}.golden", test_name), &output);

        interface
    }
}

#[test]
fn test_success() {
    TypeCheck::default().ty("y").check(
        "success",
        r#"
def foo(x: str.type) -> str.type:
    return x.removeprefix("test")
y = hash(foo("magic"))
   "#,
    );
}

#[test]
fn test_failure() {
    TypeCheck::new().check("failure", r#"hash(1)"#);
}

#[test]
fn test_load() {
    let interface = TypeCheck::new().check(
        "load_0",
        r#"
def foo(x: [bool.type]) -> str.type:
    return "test"
   "#,
    );
    TypeCheck::new().load("foo.bzl", interface).ty("res").check(
        "load_1",
        r#"
load("foo.bzl", "foo")
res = [foo([])]
"#,
    );
}

/// Test things that have previous claimed incorrectly they were type errors
#[test]
fn test_false_negative() {
    TypeCheck::new().check(
        "false_negative",
        r#"fail("Expected variable expansion in string: `{}`".format("x"))"#,
    );
}

#[test]
fn test_type_kwargs() {
    TypeCheck::new().check(
        "type_kwargs",
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
        "dot_type_0",
        r#"
def foo(x: list.type) -> bool.type:
    return type(x) == list.type
foo([1,2,3])
"#,
    );
    TypeCheck::new().check(
        "dot_type_1",
        r#"
def foo(x: list.type) -> bool.type:
    return type(x) == []
foo(True)
"#,
    );
}

#[test]
fn test_special_function_zip() {
    TypeCheck::new().ty("x").check(
        "zip",
        r#"
x = zip([1,2], [True, False], ["a", "b"])
"#,
    );
}

#[test]
fn test_special_function_struct() {
    TypeCheck::new().ty("x").check(
        "struct",
        r#"
x = struct(a = 1, b = "test")
"#,
    );
}

#[test]
fn test_call_callable() {
    TypeCheck::new().check(
        "call_callable",
        r#"
def foo(x: "function"):
    x()
"#,
    );
}

#[test]
fn test_call_not_callable() {
    TypeCheck::new().check(
        "call_not_callable",
        r#"
def foo(x: [""]):
    x()
"#,
    );
}

#[test]
fn test_call_unknown() {
    TypeCheck::new().check(
        "call_unknown",
        r#"
def foo(x: "unknown"):
    x()
"#,
    );
}

#[test]
fn test_call_callable_or_not_callable() {
    TypeCheck::new().check(
        "call_callable_or_not_callable",
        r#"
def foo(x: ["function", str.type], y: [str.type, "function"]):
    x()
    y()
"#,
    );
}

#[test]
fn test_call_callable_or_unknown() {
    TypeCheck::new().check(
        "call_callable_or_unknown",
        r#"
def foo(x: ["function", "unknown"], y: ["unknown", "function"]):
    x()
    y()
"#,
    );
}

#[test]
fn test_call_not_callable_or_unknown() {
    TypeCheck::new().check(
        "call_not_callable_or_unknown",
        r#"
def foo(x: [str.type, "unknown"], y: ["unknown", str.type]):
    x()
    y()
"#,
    );
}

#[test]
fn test_tuple() {
    TypeCheck::new().check(
        "tuple",
        r#"
def empty_tuple_fixed_name() -> (): return tuple()
def empty_tuple_name_fixed() -> tuple.type: return ()
"#,
    );
}
