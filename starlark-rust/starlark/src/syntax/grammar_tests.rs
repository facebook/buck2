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

use gazebo::prelude::*;

use crate::{assert, assert::Assert, syntax::ast::Stmt};

#[test]
fn test_empty() {
    assert_eq!(assert::parse("\n"), "");
    assert_eq!(assert::parse(""), "");
    assert_eq!(assert::parse("    "), "");
    assert_eq!(assert::parse("    \n"), "");
    // Empty trailing bits
    assert_eq!(assert::parse("x = 1\n   "), "x = 1\n");
    assert_eq!(assert::parse("x = 1\n   \n"), "x = 1\n");
}

#[test]
fn test_top_level_comment() {
    assert_eq!(assert::parse("# Test"), "");
}

#[test]
fn test_top_level_load() {
    let want = "load(\"//top/level/load.bzl\"top-level = \"top-level\")\n";
    assert_eq!(
        assert::parse("\nload(\"//top/level/load.bzl\", \"top-level\")\n"),
        want
    );
    assert_eq!(
        assert::parse("\nload(\"//top/level/load.bzl\", \"top-level\")"),
        want
    );
    assert_eq!(
        assert::parse("\nload(\n  \"//top/level/load.bzl\",\n  \"top-level\",\n)\n"),
        want
    );
}

#[test]
fn test_top_level_assignation() {
    assert_eq!(
        assert::parse("\n_ASSIGNATION = 'top-level'\n"),
        "_ASSIGNATION = \"top-level\"\n"
    );
}

#[test]
fn test_top_level_docstring() {
    assert_eq!(
        assert::parse("\n\"\"\"Top-level docstring\"\"\"\n"),
        "\"Top-level docstring\"\n"
    );
}

#[test]
fn test_top_level_def() {
    assert_eq!(
        assert::parse("def toto():\n  pass\n"),
        "def toto():\n  pass\n"
    );
    let mut a = Assert::new();
    a.dialect_set(|x| x.enable_def = false);
    a.parse_fail("!def toto():\n  pass\n!");
    // no new line at end of file
    assert_eq!(
        assert::parse("def toto():\n  pass"),
        "def toto():\n  pass\n"
    );
    assert_eq!(
        assert::parse("def toto():\n  pass\ndef titi(): return 1"),
        "def toto():\n  pass\ndef titi():\n  return 1\n"
    );
    assert_eq!(
        assert::parse("def toto():\n  pass\n\ndef titi(): return 1"),
        "def toto():\n  pass\ndef titi():\n  return 1\n"
    );
    assert_eq!(assert::parse("def t():\n\n  pass"), "def t():\n  pass\n");
}

#[test]
fn test_top_level_statements() {
    let mut a = Assert::new();
    a.dialect_set(|x| x.enable_top_level_stmt = false);
    a.parse_fail("x = 1\n!if x == 1:\n  x = 2\n!x = 3");
    a.parse_fail("x = 1\n!for x in []:\n   pass\n!");
    assert_eq!(a.parse("pass"), "pass\n");

    assert_eq!(
        assert::parse("if x == 1:\n  x = 2"),
        "if (x == 1):\n  x = 2\n"
    );
    assert_eq!(
        assert::parse("for x in []:\n   pass\n"),
        "for x in []:\n  pass\n"
    );
    assert_eq!(assert::parse("pass"), "pass\n");
}

#[test]
fn test_top_level_def_with_docstring() {
    assert_eq!(
        assert::parse(
            "\"\"\"Top-level docstring\"\"\"

def toto():
  pass
"
        ),
        "\"Top-level docstring\"\ndef toto():\n  pass\n"
    );
}

#[test]
fn test_ifelse() {
    assert_eq!(
        assert::parse("def d():\n  if True:\n    a\n  else:\n    b"),
        "def d():\n  if True:\n    a\n  else:\n    b\n"
    );
}

#[test]
fn test_kwargs_passing() {
    assert_eq!(
        assert::parse("f(x, *a, **b); f(x, *a, **{a:b}); f(x, *[a], **b)"),
        "f(x, *a, **b)\nf(x, *a, **{a: b})\nf(x, *[a], **b)\n"
    );
}

#[test]
fn test_unary_op() {
    assert_eq!(assert::parse("a = -1"), "a = -1\n");
    assert_eq!(assert::parse("a = +1"), "a = +1\n");
    assert_eq!(assert::parse("a = -a"), "a = -a\n");
    assert_eq!(assert::parse("a = +a"), "a = +a\n");
}

#[test]
fn test_tuples() {
    assert_eq!(assert::parse("a = (-1)"), "a = -1\n"); // Not a tuple
    assert_eq!(assert::parse("a = (+1,)"), "a = (+1,)\n"); // But this is one
    assert_eq!(assert::parse("a = ()"), "a = ()\n");
}

#[test]
fn test_return() {
    assert_eq!(
        assert::parse("def fn(): return 1"),
        "def fn():\n  return 1\n"
    );
    assert_eq!(
        assert::parse("def fn(): return a()"),
        "def fn():\n  return a()\n"
    );
    assert_eq!(assert::parse("def fn(): return"), "def fn():\n  return\n");
}

// Regression test for https://github.com/google/starlark-rust/issues/44.
#[test]
fn test_optional_whitespace() {
    assert_eq!(assert::parse("6 or()"), "(6 or ())\n");
    assert_eq!(assert::parse("6or()"), "(6 or ())\n");
}

// Regression test for https://github.com/google/starlark-rust/issues/56.
#[test]
fn test_optional_whitespace_after_0() {
    assert_eq!(assert::parse("0in[1,2,3]"), "(0 in [1, 2, 3])\n");
}

#[test]
fn test_blank_line() {
    assert_eq!(
        assert::parse(
            r#"
def foo():
  bar

  bar
"#
        ),
        "def foo():\n  bar\n  bar\n"
    )
}

#[test]
fn test_fncall_span() {
    let content = r#"def fn(a):
  fail(a)

fn(1)

fail(2)
"#;
    let ast = assert::parse_ast(content);
    match &ast.statement.node {
        Stmt::Statements(xs) => {
            let lines = xs.map(|x| ast.codemap.resolve_span(x.span).begin_line);
            assert_eq!(lines, vec![0, 3, 5])
        }
        _ => panic!("Expected to parse as statements"),
    }
}

#[test]
fn test_comprehension() {
    assert_eq!(
        assert::parse("[x for x in range(12) if x % 2 == 0 if x % 3 == 0]"),
        "[x for x in range(12) if ((x % 2) == 0) if ((x % 3) == 0)]\n"
    );
    assert_eq!(
        assert::parse("[x + 7 for x in range(12) for y in range(10)]"),
        "[(x + 7) for x in range(12) for y in range(10)]\n"
    );
}

#[test]
fn test_lambda() {
    assert_eq!(
        assert::parse("x = lambda y: y + 1"),
        "x = (lambda y: (y + 1))\n"
    );
    let mut a = Assert::new();
    a.dialect_set(|x| x.enable_lambda = false);
    a.parse_fail("x = !lambda y: y + 1!");
    assert_eq!(
        assert::parse("(lambda y: x == 1)(1)"),
        "(lambda y: (x == 1))(1)\n"
    );
    assert_eq!(
        assert::parse("(lambda x: x or 1)(1)"),
        "(lambda x: (x or 1))(1)\n"
    );
    assert_eq!(
        assert::parse("f = lambda x, y: x * y"),
        "f = (lambda x, y: (x * y))\n"
    );
    assert_eq!(assert::parse("lambda x: True"), "(lambda x: True)\n");
    assert_eq!(assert::parse("lambda: True"), "(lambda : True)\n");
    assert_eq!(
        assert::parse("f(lambda x, y=1, *args, **kwargs: x + y + z)"),
        "f((lambda x, y = 1, *args, **kwargs: ((x + y) + z)))\n"
    );
    assert_eq!(
        assert::parse("[x for x in [1, 2] if (lambda : 3 if True else 4)]"),
        "[x for x in [1, 2] if (lambda : (3 if True else 4))]\n"
    );
    // Note that Python3 and Go Starlark can both parse the line below,
    // but we can't, and the official Python3 grammar doesn't appear to be
    // able to either (we follow it in this position). Fortunately, its
    // a mostly meaningless program.
    // [x for x in [1, 2] if lambda : None]
}

#[test]
fn test_nested_def() {
    assert_eq!(
        assert::parse("def foo(x):\n  def bar(y): return y\n  return bar(x)"),
        "def foo(x):\n  def bar(y):\n    return y\n  return bar(x)\n"
    );
}

#[test]
fn test_op_associativity() {
    // Normal operators are left associative
    assert_eq!(
        assert::parse("1 or 2 or 3 or 4"),
        "(((1 or 2) or 3) or 4)\n"
    );
    assert_eq!(
        assert::parse("1 and 2 and 3 and 4"),
        "(((1 and 2) and 3) and 4)\n"
    );
    assert_eq!(assert::parse("1 | 2 | 3"), "((1 | 2) | 3)\n");
    assert_eq!(assert::parse("1 + 2 + 3"), "((1 + 2) + 3)\n");
    assert_eq!(assert::parse("1 * 2 * 3"), "((1 * 2) * 3)\n");
    // Comparisons are not associative
    // TODO - create a better error message for this case
    assert::fail("0 <= 1 < 2", "Parse error");
}

#[test]
fn test_bad_assignment() {
    assert::parse_fail("[!x or y!] = 1");
    assert::parse_fail("![x]! += 1");
}
