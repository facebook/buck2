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

use dupe::Dupe;
use once_cell::sync::Lazy;
use starlark_derive::starlark_module;
use starlark_syntax::golden_test_template::golden_test_template;

use crate as starlark;
use crate::assert::Assert;
use crate::environment::FrozenModule;
use crate::environment::GlobalsBuilder;
use crate::environment::Module;
use crate::eval::runtime::file_loader::ReturnOwnedFileLoader;
use crate::eval::Evaluator;
use crate::syntax::AstModule;
use crate::syntax::Dialect;
use crate::typing::interface::Interface;
use crate::typing::oracle::traits::OracleNoAttributes;
use crate::typing::AstModuleTypecheck;
use crate::typing::TypingOracle;
use crate::values::none::NoneType;
use crate::values::typing::StarlarkIter;
use crate::values::Value;
use crate::values::ValueOfUnchecked;

fn mk_oracle() -> impl TypingOracle {
    static ORACLE: Lazy<Box<dyn TypingOracle + Send + Sync + 'static>> =
        Lazy::new(|| Box::new(OracleNoAttributes));
    &*ORACLE
}

#[derive(Default)]
struct TypeCheck {
    expect_types: Vec<String>,
    loads: HashMap<String, (Interface, FrozenModule)>,
}

#[starlark_module]
fn register_typecheck_globals(globals: &mut GlobalsBuilder) {
    fn accepts_iterable<'v>(
        #[starlark(require = pos)] xs: ValueOfUnchecked<'v, StarlarkIter<Value<'v>>>,
    ) -> anyhow::Result<NoneType> {
        let _ = xs;
        Ok(NoneType)
    }
}

impl TypeCheck {
    fn new() -> Self {
        Self::default()
    }

    fn ty(mut self, name: &str) -> Self {
        self.expect_types.push(name.to_owned());
        self
    }

    fn load(mut self, file: &str, interface: Interface, module: FrozenModule) -> Self {
        self.loads.insert(file.to_owned(), (interface, module));
        self
    }

    fn mk_file_loader(&self) -> ReturnOwnedFileLoader {
        let modules = self
            .loads
            .iter()
            .map(|(name, (_, module))| (name.clone(), module.dupe()))
            .collect();
        ReturnOwnedFileLoader { modules }
    }

    fn check(&self, test_name: &str, code: &str) -> (Interface, FrozenModule) {
        let globals = GlobalsBuilder::extended()
            .with(register_typecheck_globals)
            .build();
        // `AstModule` is not `Clone`. Parse twice.
        let ast0 = AstModule::parse("filename", code.to_owned(), &Dialect::Extended).unwrap();
        let ast1 = AstModule::parse("filename", code.to_owned(), &Dialect::Extended).unwrap();
        let (errors, typemap, interface, approximations) = ast0.typecheck(
            &mk_oracle(),
            &globals,
            &self
                .loads
                .iter()
                .map(|(name, (intf, _))| (name.clone(), intf.clone()))
                .collect(),
        );

        let mut output = String::new();
        writeln!(output, "Code:").unwrap();
        writeln!(output, "{}", code.trim()).unwrap();
        if errors.is_empty() {
            writeln!(output).unwrap();
            writeln!(output, "No errors.").unwrap();
        } else {
            for error in &errors {
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

        if !self.expect_types.is_empty() {
            writeln!(output).unwrap();
            writeln!(output, "Types:").unwrap();
            for k in &self.expect_types {
                let types = typemap.find_bindings_by_name(k);
                match types.as_slice() {
                    [ty] => writeln!(output, "{}: {}", k, ty).unwrap(),
                    [] => panic!("Type not found for {}", k),
                    [_, _, ..] => panic!("Multiple types found for {}", k),
                }
            }
        }

        let loader = self.mk_file_loader();
        let module = {
            writeln!(output).unwrap();
            writeln!(output, "Compiler typechecker (eval):").unwrap();
            let module = Module::new();
            let mut eval = Evaluator::new(&module);

            eval.set_loader(&loader);

            eval.enable_static_typechecking(true);
            let eval_result = eval.eval_module(ast1, &globals);
            match &eval_result {
                Ok(_) => writeln!(output, "No errors.").unwrap(),
                Err(err) => writeln!(output, "{}", err).unwrap(),
            }

            if eval_result.is_ok() != errors.is_empty() {
                writeln!(output).unwrap();
                writeln!(output, "Compiler typechecker and eval results mismatch.").unwrap();
            }

            // Help borrow checker.
            drop(eval);

            module.freeze().unwrap()
        };

        golden_test_template(&format!("src/typing/golden/{}.golden", test_name), &output);

        (interface, module)
    }
}

#[test]
fn test_success() {
    TypeCheck::default().ty("y").check(
        "success",
        r#"
def foo(x: str) -> str:
    return x.removeprefix("test")

def bar():
    y = hash(foo("magic"))
"#,
    );
}

#[test]
fn test_failure() {
    TypeCheck::new().check(
        "failure",
        r#"
def test():
    hash(1)
"#,
    );
}

#[test]
fn test_load() {
    let (interface, module) = TypeCheck::new().check(
        "load_0",
        r#"
def foo(x: list[bool]) -> str:
    return "test"
   "#,
    );
    TypeCheck::new()
        .load("foo.bzl", interface, module)
        .ty("res")
        .check(
            "load_1",
            r#"
load("foo.bzl", "foo")
def test():
    res = [foo([])]
"#,
        );
}

/// Test things that have previous claimed incorrectly they were type errors
#[test]
fn test_false_negative() {
    TypeCheck::new().check(
        "false_negative",
        r#"
def test():
    fail("Expected variable expansion in string: `{}`".format("x"))
"#,
    );
}

#[test]
fn test_type_kwargs() {
    TypeCheck::new().check(
        "type_kwargs",
        r#"
def foo(**kwargs):
    pass

def bar():
    foo(**{1: "x"})
"#,
    );
}

#[test]
fn test_types_of_args_kwargs() {
    TypeCheck::new().ty("args").ty("kwargs").check(
        "types_of_args_kwargs",
        r#"
def foo(*args: str, **kwargs: int):
    pass
"#,
    );
}

#[test]
fn test_dot_type() {
    TypeCheck::new().check(
        "dot_type_0",
        r#"
def foo(x: list) -> bool:
    return type(x) == type(list)

def bar():
    foo([1,2,3])
"#,
    );
    TypeCheck::new().check(
        "dot_type_1",
        r#"
def foo(x: list) -> bool:
    return type(x) == []

def bar():
    foo(True)
"#,
    );
}

#[test]
fn test_special_function_zip() {
    TypeCheck::new().ty("x").check(
        "zip",
        r#"
def test():
    x = zip([1,2], [True, False], ["a", "b"])
"#,
    );
}

#[test]
fn test_special_function_struct() {
    TypeCheck::new().ty("x").check(
        "struct",
        r#"
def test():
    x = struct(a = 1, b = "test")
"#,
    );
}

#[test]
fn test_call_callable() {
    TypeCheck::new().check(
        "call_callable",
        r#"
def foo(x: typing.Callable):
    x()
"#,
    );
}

#[test]
fn test_call_not_callable() {
    TypeCheck::new().check(
        "call_not_callable",
        r#"
def foo(x: list):
    x()
"#,
    );
}

#[test]
fn test_call_unknown() {
    TypeCheck::new().check(
        "call_unknown",
        r#"
# @starlark-rust: allow_string_literals_in_type_expr

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
def foo(x: [typing.Callable, str], y: [str, typing.Callable]):
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
# @starlark-rust: allow_string_literals_in_type_expr

def foo(x: [typing.Callable, "unknown"], y: ["unknown", typing.Callable]):
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
# @starlark-rust: allow_string_literals_in_type_expr

def foo(x: [str, "unknown"], y: ["unknown", str]):
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
def empty_tuple_name_fixed() -> tuple: return ()
"#,
    );
}

#[test]
fn test_tuple_ellipsis() {
    TypeCheck::new().check(
        "tuple_ellipsis",
        r#"
def f(t: tuple[int, ...]) -> int:
    return t[0]

def g():
    # Good.
    f((1, 2, 3))

    # Bad.
    f((1, "x"))
"#,
    );
}

#[test]
fn test_test_new_syntax_without_dot_type() {
    TypeCheck::new().check(
        "new_syntax_without_dot_type",
        r#"
def foo(x: str): pass

def bar():
    # good
    foo("test")

    # bad
    foo(1)
"#,
    );
}

#[test]
fn test_calls() {
    TypeCheck::new().check(
        "calls",
        r#"
def f(y): pass

def g():
    # Extra parameter.
    f(1, 2)

    # Not enough parameters.
    f()
"#,
    );
}

#[test]
fn test_list_append() {
    TypeCheck::new().ty("x").check(
        "list_append",
        r#"
def test():
    # Type of `x` should be inferred as list of either `int` or `str`.
    x = []
    x.append(1)
    x.append("")
"#,
    );
}

#[test]
fn test_list_append_bug() {
    // TODO(nga): fix.
    TypeCheck::new().ty("x").check(
        "list_append_bug",
        r#"
def test():
    x = []
    x.append(x)
"#,
    );
}

#[test]
fn test_list_function() {
    TypeCheck::new().ty("x").check(
        "list_function",
        r#"
def test():
    x = list([1, 2])
"#,
    );
}

#[test]
fn test_list_less() {
    TypeCheck::new().check(
        "list_less",
        r#"
def test(x: list[str], y: list[str]) -> bool:
    return x < y
"#,
    );
}

#[test]
fn test_list_bin_op() {
    TypeCheck::new().ty("x").ty("y").ty("z").check(
        "list_bin_op",
        r#"
def test(a: list[str]):
    x = a + a
    y = a * 3
    z = 3 * a
"#,
    );
}

#[test]
fn test_accepts_iterable() {
    TypeCheck::new().check(
        "accepts_iterable",
        r#"
def test():
    accepts_iterable([1, ()])
"#,
    );

    let mut a = Assert::new();
    a.globals_add(register_typecheck_globals);
    a.pass("accepts_iterable([1, ()])");
}

#[test]
fn test_dict_bug() {
    // TODO(nga): figure out how to fix it.
    //   Type of `y` should be inferred to `str`.
    TypeCheck::new().ty("y").check(
        "dict_bug",
        r#"
def test():
    x = {}
    x.setdefault(33, "x")
    y = x[44]
"#,
    );
}

#[test]
fn test_dict_lookup_by_never() {
    TypeCheck::new().check(
        "dict_never_key",
        r#"
# We use `typing.Never` when expression is an error,
# or it is a type parameter of empty list for example.
# Dict lookup by never should not be an error.
def test(d: dict[typing.Any, str], x: typing.Never):
    y = d[x]
"#,
    );
}

#[test]
fn test_new_list_dict_syntax() {
    TypeCheck::new().ty("x").check(
        "new_list_dict_syntax",
        r#"
def new_list_dict_syntax(d: dict[str, int]) -> list[str]:
    return list(d.keys())

def test():
    # Check type is properly parsed from the function return type.
    x = new_list_dict_syntax({"a": 1, "b": 2})
"#,
    );
}

#[test]
fn test_new_list_dict_syntax_as_value() {
    // TODO(nga): fix.
    TypeCheck::new().ty("x").ty("y").check(
        "new_list_dict_syntax_as_value",
        r#"
def test():
    x = list[str]
    y = dict[int, bool]
"#,
    );
}

#[test]
fn test_int_plus_float() {
    TypeCheck::new().ty("x").check(
        "int_plus_float",
        r#"
def test():
    x = 1 + 1.0
"#,
    );
}

#[test]
fn test_int_bitor_float() {
    TypeCheck::new().ty("x").check(
        "int_bitor_float",
        r#"
def test():
    x = 0x60000000000000000000000 | 1.0
"#,
    );
}

#[test]
fn test_int_mul_list() {
    // TODO(nga): fix.
    TypeCheck::new().ty("x").check(
        "int_mul_list",
        r#"
def test():
    x = 1 * ["a"]
"#,
    );
}

#[test]
fn test_un_op() {
    TypeCheck::new().ty("x").ty("y").ty("z").check(
        "un_op",
        r#"
def test():
    # Good.
    x = -1
    # Bad.
    y = ~True
    # Union good and bad.
    z = -(1 if True else "")
"#,
    );
}

#[test]
fn test_union() {
    TypeCheck::new().check(
        "union",
        r#"
def func_which_returns_union(p) -> str | int:
    if p == 56:
        return "a"
    elif p == 57:
        return 1
    else:
        return []
"#,
    );
}

#[test]
fn test_type_alias() {
    TypeCheck::new().ty("x").check(
        "type_alias",
        r#"
MyList = list[int]

def f(x: MyList):
    pass
"#,
    );
}

#[test]
fn test_incorrect_type_dot() {
    TypeCheck::new().check(
        "incorrect_type_dot",
        r#"
def foo(x: list.foo.bar):
    pass
"#,
    );
}

#[test]
fn test_never_call_bug() {
    TypeCheck::new().ty("y").check(
        "never_call_bug",
        r#"
def foo(x: typing.Never):
    y = x(1)
"#,
    );
}

#[test]
fn test_methods_work_for_ty_starlark_value() {
    TypeCheck::new().ty("x").check(
        "methods_work_for_ty_starlark_value",
        r#"
def test(s: str):
    x = s.startswith("a")
"#,
    );
}
