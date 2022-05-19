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

//! Test docstrings.

use crate::{
    assert,
    assert::Assert,
    environment::{Module, ModuleDocs},
    values::{docs::DocStringKind, StarlarkValue, Value},
};

#[test]
fn test_def_docstring_parses() -> anyhow::Result<()> {
    use crate::values::docs::{DocItem, DocString, Function, Param, Return, Type};

    let fun = assert::pass(
        r#"
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


    # Function with: no doc string, with a docstring and no args:, one with *args, one with no return type
(f1, f2, f3, f4)
"#,
    );
    let env = Module::new();
    let f1 = fun
        .value()
        .at(Value::new_int(0), env.heap())?
        .get_ref()
        .documentation();
    let f2 = fun
        .value()
        .at(Value::new_int(1), env.heap())?
        .get_ref()
        .documentation();
    let f3 = fun
        .value()
        .at(Value::new_int(2), env.heap())?
        .get_ref()
        .documentation();
    let f4 = fun
        .value()
        .at(Value::new_int(3), env.heap())?
        .get_ref()
        .documentation();

    let expected_f1 = Some(DocItem::Function(Function {
        docs: DocString::from_docstring(DocStringKind::Starlark, r#"Summary line goes here"#),
        params: vec![
            Param::Arg {
                name: "a".to_owned(),
                docs: DocString::from_docstring(DocStringKind::Starlark, "The docs for a"),
                typ: None,
                default_value: None,
            },
            Param::Arg {
                name: "b".to_owned(),
                docs: DocString::from_docstring(DocStringKind::Starlark, "The docs for b"),
                typ: Some(Type {
                    raw_type: "\"string\"".to_owned(),
                }),
                default_value: None,
            },
            Param::Arg {
                name: "c".to_owned(),
                docs: DocString::from_docstring(
                    DocStringKind::Starlark,
                    "The docs for c, but these\ngo onto two lines",
                ),
                typ: Some(Type {
                    raw_type: "\"int\"".to_owned(),
                }),
                default_value: Some("5".to_owned()),
            },
            Param::NoArgs,
            Param::Arg {
                name: "d".to_owned(),
                docs: None,
                typ: Some(Type {
                    raw_type: "\"string\"".to_owned(),
                }),
                default_value: Some("\"some string\"".to_owned()),
            },
            Param::Kwargs {
                name: "**kwargs".to_owned(),
                docs: DocString::from_docstring(
                    DocStringKind::Starlark,
                    "Docs for the keyword args",
                ),
                typ: None,
            },
        ],
        ret: Return {
            docs: DocString::from_docstring(DocStringKind::Starlark, "A string repr of the args"),
            typ: Some(Type {
                raw_type: r#"["string"]"#.to_owned(),
            }),
        },
    }));

    let expected_f2 = Some(DocItem::Function(Function {
        docs: DocString::from_docstring(
            DocStringKind::Starlark,
            r#"This is a function with *args, and no return type"#,
        ),
        params: vec![
            Param::Arg {
                name: "a".to_owned(),
                docs: None,
                typ: None,
                default_value: None,
            },
            Param::Args {
                name: "*args".to_owned(),
                docs: DocString::from_docstring(DocStringKind::Starlark, "Only doc this arg"),
                typ: Some(Type {
                    raw_type: "[\"string\"]".to_owned(),
                }),
            },
        ],
        ret: Return {
            docs: None,
            typ: None,
        },
    }));

    let expected_f3 = Some(DocItem::Function(Function {
        docs: None,
        params: vec![Param::Arg {
            name: "a".to_owned(),
            docs: None,
            typ: Some(Type {
                raw_type: "\"string\"".to_owned(),
            }),
            default_value: None,
        }],
        ret: Return {
            docs: None,
            typ: Some(Type {
                raw_type: "\"string\"".to_owned(),
            }),
        },
    }));

    let expected_f4 = Some(DocItem::Function(Function {
        docs: DocString::from_docstring(
            DocStringKind::Starlark,
            "This is a docstring with no 'Args:' section",
        ),
        params: vec![Param::Arg {
            name: "a".to_owned(),
            docs: None,
            typ: Some(Type {
                raw_type: "\"string\"".to_owned(),
            }),
            default_value: None,
        }],
        ret: Return {
            docs: None,
            typ: Some(Type {
                raw_type: "\"string\"".to_owned(),
            }),
        },
    }));

    assert_eq!(expected_f1, f1);
    assert_eq!(expected_f2, f2);
    assert_eq!(expected_f3, f3);
    assert_eq!(expected_f4, f4);

    Ok(())
}

#[test]
fn test_module_docstring_parses() {
    use crate::values::docs::{DocItem, DocString, Module};

    let m1 = assert::pass_module(
        r#"
"""
This is the summary of the module's docs

Some extra details can go here,
    and indentation is kept as expected
"""
def f1():
    """ This is a function summary """
    pass

# This function has no docstring
def f2():
    pass
"#,
    );
    let m2 = assert::pass_module(
        r#"
x = ""
"""
This comes after another statement, so is not a docstring
"""
def f1():
    pass
"#,
    );
    let m3 = assert::pass_module(
        r#"
# This module has no docstring
def f1():
    pass
"#,
    );

    let m1_docs = m1.documentation();
    let m2_docs = m2.documentation();
    let m3_docs = m3.documentation();

    let expected_m1 = Some(DocItem::Module(Module {
        docs: DocString::from_docstring(
            DocStringKind::Starlark,
            r"This is the summary of the module's docs

Some extra details can go here,
    and indentation is kept as expected",
        ),
    }));

    assert_eq!(expected_m1, m1_docs);
    assert_eq!(None, m2_docs);
    assert_eq!(None, m3_docs);
}

#[test]
fn test_module_docs_return() {
    use crate::values::docs::{DocItem, DocString, Function, Module, Return};

    let mut a = Assert::new();
    let mod_a = r#"
def a():
    """ This is a function summary that should not re-export """
    pass
"#;
    a.module("a", mod_a);

    let m1 = a.pass_module(
        r#"
"""
This is the summary of the module's docs

Some extra details can go here,
    and indentation is kept as expected
"""
def f1():
    """ This is a function summary """
    pass

# This function has no docstring
def f2():
    pass

# This is not public, so it should not be exported in docs
def _f3():
    pass
"#,
    );
    let m2 = assert::pass_module(
        r#"
x = ""
"""
This comes after another statement, so is not a docstring
"""
def f1():
    pass
"#,
    );
    let m3 = assert::pass_module(
        r#"
# This module has no docstring
def f1():
    pass
"#,
    );

    let m1_docs = m1.module_documentation();
    let m2_docs = m2.module_documentation();
    let m3_docs = m3.module_documentation();

    let empty_function = Some(DocItem::Function(Function {
        docs: None,
        params: vec![],
        ret: Return {
            docs: None,
            typ: None,
        },
    }));

    let expected_m1 = ModuleDocs {
        module: Some(DocItem::Module(Module {
            docs: DocString::from_docstring(
                DocStringKind::Starlark,
                r"This is the summary of the module's docs

Some extra details can go here,
    and indentation is kept as expected",
            ),
        })),
        members: hashmap! {
            "f1".to_owned() => Some(DocItem::Function(Function {
                docs: DocString::from_docstring(DocStringKind::Starlark, "This is a function summary"),
                params: vec![],
                ret: Return {
                    docs: None,
                    typ: None,
                },
            })),
            "f2".to_owned() => empty_function.clone(),
        },
    };

    let expected_m2 = ModuleDocs {
        module: None,
        // Note that the "x" value here is the documentation for the string type, not
        // for a SPECIFIC string.
        members: hashmap! {
            "x".to_owned() =>  "blah".documentation(),
            "f1".to_owned() => empty_function.clone(),
        },
    };

    let expected_m3 = ModuleDocs {
        module: None,
        members: hashmap! {
            "f1".to_owned() => empty_function,
        },
    };

    assert_eq!(expected_m1, m1_docs);
    assert_eq!(expected_m2, m2_docs);
    assert_eq!(expected_m3, m3_docs);
}
