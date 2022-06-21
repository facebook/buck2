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

use crate::assert;
use crate::syntax::lexer::Token::*;

#[test]
fn test_int_lit() {
    assert_eq!(assert::lex("0 123"), "0 123 \n");
    assert_eq!(assert::lex("0x7F 0x7d"), "127 125 \n");
    assert_eq!(assert::lex("0B1011 0b1010"), "11 10 \n");
    assert_eq!(assert::lex("0o755 0O753"), "493 491 \n");
    // Starlark requires us to ban leading zeros (confusion with implicit octal)
    assert::parse_fail("x = !01!");
}

#[test]
fn test_indentation() {
    assert_eq!(
        assert::lex(
            "
+
  -
      /
      *
  =
    %
      .
+=
",
        ),
        "\n + \n \t - \n \t / \n * \n #dedent = \n \t % \n \t . \n #dedent #dedent #dedent += \n \n"
    );
}

#[test]
fn test_symbols() {
    assert_eq!(
        assert::lex(", ; : += -= *= /= //= %= == != <= >= ** = < > - + * % / // . { } [ ] ( ) |"),
        ", ; : += -= *= /= //= %= == != <= >= ** = < > - + * % / // . { } [ ] ( ) | \n",
    );
    assert_eq!(assert::lex(",;:{}[]()|"), ", ; : { } [ ] ( ) | \n",);
}

#[test]
fn test_keywords() {
    assert_eq!(
        assert::lex(
            "and else load break for not not  in continue if or def in pass elif return lambda"
        ),
        "and else load break for not not in continue if or def in pass elif return lambda \n"
    );
}

// Regression test for https://github.com/google/starlark-rust/issues/44.
#[test]
fn test_number_collated_with_keywords_or_identifier() {
    assert_eq!(
        assert::lex("0in 1and 2else 3load 4break 5for 6not 7not  in 8continue 10identifier11"),
        "0 in 1 and 2 else 3 load 4 break 5 for 6 not 7 not in 8 continue 10 identifier11 \n"
    );
}

#[test]
fn test_reserved() {
    let reserved =
        "as import is class nonlocal del raise except try finally while from with global yield"
            .split_whitespace();
    for x in reserved {
        assert::parse_fail(&format!("!{}! = 1", x));
    }
}

#[test]
fn test_comment() {
    // Comment should be ignored
    assert_eq!(assert::lex("# a comment\n"), "\n");
    assert_eq!(assert::lex(" # a comment\n"), "\n");
    assert_eq!(assert::lex("a # a comment\n"), "a \n \n");
    // But it should not eat everything
    assert_eq!(assert::lex("[\n# a comment\n]"), "[ ] \n");
}

#[test]
fn test_identifier() {
    assert_eq!(
        assert::lex("a identifier CAPS _CAPS _0123"),
        "a identifier CAPS _CAPS _0123 \n"
    )
}

#[test]
fn test_string_lit() {
    assert_eq!(
        assert::lex("'123' \"123\" '' \"\" '\\'' \"\\\"\" '\"' \"'\" '\\n' '\\w'"),
        "\"123\" \"123\" \"\" \"\" \"\'\" \"\\\"\" \"\\\"\" \"\'\" \"\\n\" \"\\\\w\" \n"
    );

    // unfinished string literal
    assert::parse_fail("!'!\n'");
    assert::parse_fail("!\"!\n\"");
    assert::parse_fail("this = a + test + !r\"!");
    assert::parse_fail("test + !\' of thing that!");
    assert::parse_fail("test + !\' of thing that!\n'");

    // Multiline string
    assert_eq!(
        assert::lex("'''''' '''\\n''' '''\n''' \"\"\"\"\"\" \"\"\"\\n\"\"\" \"\"\"\n\"\"\""),
        "\"\" \"\\n\" \"\\n\" \"\" \"\\n\" \"\\n\" \n"
    );
    // Raw string
    assert_eq!(
        assert::lex("r'' r\"\" r'\\'' r\"\\\"\" r'\"' r\"'\" r'\\n'"),
        "\"\" \"\" \"\'\" \"\\\"\" \"\\\"\" \"\'\" \"\\\\n\" \n"
    );
}

#[test]
fn test_string_escape() {
    assert_eq!(assert::lex("'\\0\\0\\1n'"), "\"\\u0000\\u0000\\u0001n\" \n");
    assert_eq!(
        assert::lex("'\\0\\00\\000\\0000'"),
        "\"\\u0000\\u0000\\u0000\\u00000\" \n"
    );
    assert_eq!(assert::lex("'\\x000'"), "\"\\u00000\" \n");
    assert_eq!(assert::lex("'\\372x'"), "\"úx\" \n");
    assert::parse_fail("test 'more !\\xT!Z");
    assert::parse_fail("test + 'more !\\UFFFFFFFF! overflows'");
    assert::parse_fail("test 'more !\\x0y!abc'");
    assert::parse_fail("test 'more !\\x0!");
}

#[test]
fn test_simple_example() {
    assert_eq!(
        assert::lex(
            "\"\"\"A docstring.\"\"\"

def _impl(ctx):
  # Print Hello, World!
  print('Hello, World!')
"
        ),
        "\"A docstring.\" \n \n def _impl ( ctx ) : \n \t print ( \"Hello, World!\" ) \n \n #dedent"
    );
}

#[test]
fn test_escape_newline() {
    assert_eq!(assert::lex("a \\\nb"), "a b \n");
}

#[test]
fn test_lexer_multiline_triple() {
    assert_eq!(
        assert::lex(
            r#"
cmd = """A \
    B \
    C \
    """"#,
        ),
        "\n cmd = \"A     B     C     \" \n"
    );
}

#[test]
fn test_span() {
    let expected = vec![
        (0, Newline, 1),
        (1, Def, 4),
        (5, Identifier("test".to_owned()), 9),
        (9, OpeningRound, 10),
        (10, Identifier("a".to_owned()), 11),
        (11, ClosingRound, 12),
        (12, Colon, 13),
        (13, Newline, 14),
        (14, Indent, 16),
        (16, Identifier("fail".to_owned()), 20),
        (20, OpeningRound, 21),
        (21, Identifier("a".to_owned()), 22),
        (22, ClosingRound, 23),
        (23, Newline, 24),
        (24, Newline, 25),
        (25, Dedent, 25),
        (25, Identifier("test".to_owned()), 29),
        (29, OpeningRound, 30),
        (30, String("abc".to_owned()), 35),
        (35, ClosingRound, 36),
        (36, Newline, 37),
        (37, Newline, 37),
    ];

    let actual = assert::lex_tokens(
        r#"
def test(a):
  fail(a)

test("abc")
"#,
    );
    assert_eq!(expected, actual);
}

#[test]
fn test_lexer_final_comment() {
    assert_eq!(
        assert::lex(
            r#"
x
# test"#,
        ),
        "\n x \n \n"
    );
}

#[test]
fn test_lexer_dedent() {
    assert_eq!(
        assert::lex(
            r#"
def stuff():
  if 1:
    if 1:
      pass
  pass
"#
        ),
        "\n def stuff ( ) : \n \t if 1 : \n \t if 1 : \n \t pass \n #dedent #dedent pass \n \n #dedent"
    );
}

#[test]
fn test_lexer_operators() {
    assert_eq!(assert::lex("1+-2"), "1 + - 2 \n");
    assert_eq!(assert::lex("1+------2"), "1 + - - - - - - 2 \n");
    assert::eq("1+------2", "3");
    assert_eq!(assert::lex("///==/+-"), "// /= = / + - \n");
}

#[test]
fn test_lexer_error_messages() {
    // What are the common errors people make.
    // Do they have good error messages and span locations.
    fn f(program: &str, msg: &str) {
        assert::parse_fail(program);
        assert::fail(&program.replace('!', ""), msg);
    }

    f("unknown !$!&%+ operator", "invalid input `$`");
    f("an !'incomplete string!\nends", "unfinished string literal");
    f(
        "an + 'invalid escape !\\x3 ! character'",
        "invalid string escape sequence `x3 `",
    );
    f(
        "leading_zero = !003! + 8",
        "integer cannot have leading 0, got `003`",
    );
    f("a + (test!]! + c", "unexpected symbol ']' here");
    f(
        "reserved_word = !raise! + 1",
        "cannot use reserved keyword `raise`",
    );
}

#[test]
fn test_float_lit() {
    assert_eq!(assert::lex("0.0 0. .0"), "0 0 0 \n");
    assert_eq!(
        assert::lex("1e10 1e+10 1e-10"),
        "10000000000 10000000000 0.0000000001 \n"
    );
    assert_eq!(
        assert::lex("1.1e10 1.1e+10 1.1e-10"),
        "11000000000 11000000000 0.00000000011 \n"
    );
    assert_eq!(
        assert::lex("0. .123 3.14 .2e3 1E+4"),
        "0 0.123 3.14 200 10000 \n"
    );
}
