/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Parsing query expressions.
//!
//! The parser doesn't do any validation on the function names or function arguments, that is handled
//! during evaluation. The returned parsed tree has locations attached so that the evaluation can
//! produce good error messages.
//!
//! The grammar is something like:
//!
//! ```text
//!
//! # note that set's args are space-separated, not comma-separated and so cannot be treated as a function
//! EXPR ::=
//!          WORD
//!        | INTEGER
//!        | '(' EXPR ')'
//!        | 'set(' WORD * ')'
//!        | FUNCTION_NAME '(' EXPR ( ',' EXPR ) * ')'
//!        | EXPR 'intersect' EXPR
//!        | EXPR ' ^ ' EXPR
//!        | EXPR ' union ' EXPR
//!        | EXPR ' + ' EXPR
//!        | EXPR ' except ' EXPR
//!        | EXPR ' - ' EXPR
//!
//! # word is much broader than a normal identifier-like thing would allow since we don't want to require
//! # quoting targets "@fbcode//some:target" or common regexes ".*" or filenames "Foo.java".
//! WORD ::=
//!          '\'' not_single_quote * '\''
//!        | '"' not_double_quote * '"'
//!        | "a-zA-Z0-9*/@._:$#%-" *
//!
//! INTEGER ::= "0" | ("1-9" "0-9"*)
//!
//! FUNCTION_NAME ::= "a-zA-Z_" "a-zA-Z0-9_" *
//!
//! ```

#![feature(box_syntax)]

pub mod span;
pub mod spanned;

use std::fmt::Display;

use enum_map::Enum;
use gazebo::{prelude::*, variants::VariantName};
use nom::{
    branch::alt,
    bytes::complete::{is_a, tag, take_till},
    character::complete::{alpha1, alphanumeric1, char, digit1, multispace0, multispace1},
    combinator::{all_consuming, cut, recognize},
    error::{context, convert_error, ErrorKind, VerboseError},
    multi::{many0, many1, separated_list0},
    sequence::{delimited, pair, preceded, terminated},
    IResult,
};
use thiserror::Error;

use crate::{span::Span, spanned::Spanned};

// TODO(cjhopman): Add `LET WORD = expr IN expr`

// TODO(cjhopman): We should switch to our own error type here. VerboseError doesn't even allow us to construct
// our own error messages (so, for example, we can't have a good error message for too large integers) and doesn't
// support propagating anyhow or std errors (and since we can't do a custom message, we can't even capture them as a string).
type NomResult<'a, O, E> = IResult<Span<'a>, O, E>;

// Type alias.
trait NomParseError<'a>:
    nom::error::ParseError<Span<'a>> + nom::error::ContextError<Span<'a>> + 'a
{
}

impl<'a, E: nom::error::ParseError<Span<'a>> + nom::error::ContextError<Span<'a>> + 'a>
    NomParseError<'a> for E
{
}

#[derive(Debug, Error)]
enum ParseError {
    #[error("{0}")]
    NomError(String),
}

/// This is the main output type of the query parser.
pub type SpannedExpr<'a> = Spanned<Expr<'a>>;

#[derive(Debug, VariantName)]
pub enum Expr<'a> {
    // doesn't need to be Span since the Expr itself is always a SpannedExpr.
    String(&'a str),
    Integer(u64),
    Function {
        function_name: Span<'a>,
        args: Vec<SpannedExpr<'a>>,
    },
    BinaryOpSequence(Box<SpannedExpr<'a>>, Vec<(BinaryOp, SpannedExpr<'a>)>),
    Set(Vec<Span<'a>>),
}

impl Display for Expr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::String(word) => {
                let quote = if word.contains('\'') { '"' } else { '\'' };
                // The grammar shouldn't allow a word that contains both quote-types, but
                // this assert should help ensure this gets updated if the grammar changes
                // to allow that.
                assert!(!word.contains(quote));
                write!(f, "{0}{1}{0}", quote, word)?;
            }
            Expr::Integer(val) => write!(f, "{}", val)?,
            Expr::Function {
                function_name,
                args,
            } => {
                f.write_str(function_name.fragment())?;
                f.write_str("(")?;
                for (i, v) in args.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }
                    v.fmt(f)?;
                }
                f.write_str(")")?;
            }
            Expr::BinaryOpSequence(left, exprs) => {
                // prints exprs.len() leading '('
                // see https://stackoverflow.com/questions/35280798/printing-a-character-a-variable-number-of-times-with-println
                write!(f, "{1:(<0$} {2}", exprs.len(), "", left)?;
                for e in exprs {
                    write!(f, " {} {})", e.0, e.1)?;
                }
            }
            Expr::Set(vals) => {
                f.write_str("set(")?;
                for (i, v) in vals.iter().enumerate() {
                    if i != 0 {
                        f.write_str(" ")?;
                    }
                    f.write_str(v.fragment())?;
                }
                f.write_str(")")?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Enum, Copy, Dupe, Clone)]
pub enum BinaryOp {
    Intersect,
    Except,
    Union,
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            BinaryOp::Intersect => "&",
            BinaryOp::Except => "^",
            BinaryOp::Union => "|",
        })
    }
}

/// Wraps a parser producing `O` with one producing `Spanned<O>` by marking the output with a span covering all of the consumed input.
fn spanned<'a, O, E: NomParseError<'a>, F: FnMut(Span<'a>) -> IResult<Span<'a>, O, E>>(
    mut func: F,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, Spanned<O>, E>
where
    O: 'a,
    F: 'a,
{
    move |original_input| {
        let start = original_input.location_offset();
        let (input, value) = func(original_input)?;
        let end = input.location_offset();
        Ok((
            input,
            Spanned {
                position: start..end,
                value,
            },
        ))
    }
}

/// nom's convert_error requires that the error's type be very str-like and Span isn't. So, we convert the Span error into a &str error.
fn convert_to_str_error(err: VerboseError<Span>) -> VerboseError<&str> {
    VerboseError {
        errors: err.errors.into_map(|(span, kind)| (span.fragment(), kind)),
    }
}

/// Parses a query string into a SpannedExpr. Requires that the entire input is consumed.
pub fn parse_expr(input: &str) -> anyhow::Result<SpannedExpr> {
    let span = Span::new(input);
    // Parse with fast error (`()`) first,
    // and on error reparse again with `VerboseError` to get detailed errors.
    match all_consuming(expr)(span) {
        Ok((_, value)) => Ok(value),
        Err(nom::Err::Failure(())) | Err(nom::Err::Error(())) => {
            match all_consuming(expr)(span) {
                Ok(..) => unreachable!(
                    "if fast parse didn't succeed, slow parse should not succeed as well"
                ),
                Err(nom::Err::Failure(err)) | Err(nom::Err::Error(err)) => {
                    // TODO(cjhopman): Do better error message conversion. The default nom one is rather verbose and references line
                    // numbers (which generally are meaningless in this context where we have a single line). It'd also be nice if we
                    // could keep the error information rather than just stringifying it here.
                    Err(
                        ParseError::NomError(convert_error(input, convert_to_str_error(err)))
                            .into(),
                    )
                }
                Err(nom::Err::Incomplete(..)) => unreachable!(),
            }
        }
        Err(nom::Err::Incomplete(..)) => unreachable!(),
    }
}

// Parses a non-infix op expression. This is split out so that we can parse a sequence of infix operators without recursion.
fn single_expr<'a, E: NomParseError<'a>>(input: Span<'a>) -> NomResult<'a, SpannedExpr<'a>, E> {
    // The ordering here is a little important, the first three of these all have a pattern of identifying
    // some prefix (ex, expr_set will identify "set(") and then failing if it doesn't match.
    //
    // The important bit there is probably around set/function/word since we don't want to, for example, parse out a
    //  word from "set(" (we'd only then get an error by the caller requiring that we consumed everything).
    //
    // The infix binary operators require left-recursion, so we can't just handle that like the others. Instead we
    // parse an expression from the beginning of the input and check after if there's a "trailing" infix operator.
    let (input, left_expr) = alt((
        preceded(char('('), cut(terminated(expr, char(')')))),
        expr_set,
        expr_function,
        expr_int,
        expr_word,
    ))(input)?;

    Ok((input, left_expr))
}

/// Tries to parse an Expr.
fn expr<'a, E: NomParseError<'a>>(input: Span<'a>) -> NomResult<'a, SpannedExpr<'a>, E> {
    context(
        "expression",
        delimited(
            multispace0,
            spanned(|input| {
                let (input, left_expr) = single_expr(input)?;

                if let Ok((input, binary_ops)) = trailing_infix(input) {
                    Ok((input, Expr::BinaryOpSequence(box left_expr, binary_ops)))
                } else {
                    Ok((input, left_expr.value))
                }
            }),
            multispace0,
        ),
    )(input)
}

// trailing infix parses a sequence of trailing infix operators. It's important that we don't recurse for each item in such
// a sequence as in practice we get really long such chains.
fn trailing_infix(input: Span) -> NomResult<Vec<(BinaryOp, SpannedExpr)>, ()> {
    fn single_infix(input: Span) -> NomResult<(BinaryOp, SpannedExpr), ()> {
        let (input, operator) = binary_op(input)?;
        let (input, right) = cut(single_expr)(input)?;
        Ok((input, (operator, right)))
    }
    many1(single_infix)(input)
}

/// Tries to parse an Expr::Word
fn expr_word<'a, E: NomParseError<'a>>(input: Span<'a>) -> NomResult<'a, SpannedExpr<'a>, E> {
    spanned(|input| {
        let (remaining, word) = word(input)?;
        Ok((remaining, Expr::String(word.fragment())))
    })(input)
}

/// Tries to parse an Expr::Integer
fn expr_int<'a, E: NomParseError<'a>>(input: Span<'a>) -> NomResult<'a, SpannedExpr<'a>, E> {
    spanned(|input| {
        let (remaining, word) = digit1(input)?;

        let value = match word.fragment() {
            "0" => 0,
            val if val.starts_with('0') => {
                return Err(nom::Err::Failure(nom::error::ParseError::from_error_kind(
                    input,
                    ErrorKind::Digit,
                )));
            }
            val => val.parse::<u64>().map_err(|_| {
                nom::Err::Failure(nom::error::ParseError::from_error_kind(
                    input,
                    ErrorKind::Digit,
                ))
            })?,
        };

        Ok((remaining, Expr::Integer(value)))
    })(input)
}

fn word<'a, E: NomParseError<'a>>(input: Span<'a>) -> NomResult<'a, Span<'a>, E> {
    fn non_quoted_word<'a, E: NomParseError<'a>>(input: Span<'a>) -> NomResult<'a, Span<'a>, E> {
        recognize(many1(alt((alphanumeric1, is_a("*/@.-_:$#%")))))(input)
    }

    alt((
        preceded(
            char('\''),
            cut(terminated(take_till(|c| c == '\''), char('\''))),
        ),
        preceded(
            char('"'),
            cut(terminated(take_till(|c| c == '"'), char('"'))),
        ),
        non_quoted_word,
    ))(input)
}

fn binary_op(input: Span) -> NomResult<BinaryOp, ()> {
    fn keyword(long: &'static str) -> impl Fn(Span) -> NomResult<Span, ()> {
        // keywords require spaces separating from the exprs
        move |span| delimited(multispace1, tag(long), multispace1)(span)
    }

    fn symbol(sym: &'static str) -> impl FnMut(Span) -> NomResult<Span, ()> {
        // symbols can have spaces separating from the exprs, but don't require them
        move |span: Span| delimited(multispace0, tag(sym), multispace0)(span)
    }

    fn op(
        sym: &'static str,
        word: &'static str,
        op: BinaryOp,
    ) -> impl Fn(Span) -> NomResult<BinaryOp, ()> {
        move |span| Ok((alt((symbol(sym), keyword(word)))(span)?.0, op))
    }

    alt((
        op("-", "except", BinaryOp::Except),
        op("^", "intersect", BinaryOp::Intersect),
        op("+", "union", BinaryOp::Union),
    ))(input)
}

/// Tries to parse an Expr::Set. Will fail if it detects an unfinished "set("
fn expr_set<'a, E: NomParseError<'a>>(input: Span<'a>) -> NomResult<'a, SpannedExpr<'a>, E> {
    fn set_args<'a, E: NomParseError<'a>>(input: Span<'a>) -> NomResult<'a, Vec<Span<'a>>, E> {
        separated_list0(multispace1, word)(input)
    }

    spanned(|input| {
        let (input, _) = tag("set(")(input)?;
        cut(move |input| {
            let (input, args) = delimited(multispace0, set_args, multispace0)(input)?;
            let (input, _) = char(')')(input)?;
            Ok((input, Expr::Set(args)))
        })(input)
    })(input)
}

/// Tries to parse an Expr::Function. Will fail if it detects an unfinished "func("
// We don't need to worry about "set(" as the outermost expr() ensures that never gets to here.
fn expr_function<'a, E: NomParseError<'a>>(input: Span<'a>) -> NomResult<'a, SpannedExpr<'a>, E> {
    fn function_args<'a, E: NomParseError<'a>>(
        input: Span<'a>,
    ) -> NomResult<'a, Vec<SpannedExpr<'a>>, E> {
        separated_list0(terminated(tag(","), multispace0), expr)(input)
    }

    spanned(|input| {
        let (input, function_name) = recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        ))(input)?;
        let (input, _) = char('(')(input)?;
        cut(move |input| {
            let (input, args) = terminated(function_args, char(')'))(input)?;
            Ok((
                input,
                Expr::Function {
                    function_name,
                    args,
                },
            ))
        })(input)
    })(input)
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;

    use super::*;

    fn assert_err<'a, E, O: Debug>(res: IResult<Span, O, E>, ctx: &str) -> E
    where
        E: NomParseError<'a> + Debug,
    {
        match res {
            Err(nom::Err::Incomplete(..)) => {
                unreachable!("Only streaming parsing should return 'Err::Incomplete'");
            }
            Err(nom::Err::Error(e)) => e,
            _ => {
                panic!("{} Got `{:?}`", ctx, res);
            }
        }
    }

    fn assert_fail<'a, E, O: Debug>(res: IResult<Span, O, E>, ctx: &str) -> E
    where
        E: NomParseError<'a> + Debug,
    {
        match res {
            Err(nom::Err::Incomplete(..)) => {
                unreachable!("Only streaming parsing should return 'Err::Incomplete'");
            }
            Err(nom::Err::Failure(e)) => e,
            _ => {
                panic!("{} Got `{:?}`", ctx, res);
            }
        }
    }

    #[test]
    fn test_expr() -> anyhow::Result<()> {
        run_tests(
            expr,
            &[
                "f",
                "func(f, a)",
                "deps(%Ss) + set(a b c) ^ func(o(a(b, c, d)))",
                "a + b",
                "(a - (b))",
                "123",
            ],
            &[],
            &["func(", "set(", "(a", "01234"],
        );

        match parse_expr("set(a b c)") {
            Ok(Spanned {
                value: Expr::Set(..),
                ..
            }) => {}
            v => panic!("expected set expr, got `{:?}`", v),
        }

        match parse_expr("set(a b c) ^ func()") {
            Ok(Spanned {
                value: Expr::BinaryOpSequence(..),
                ..
            }) => {}
            v => panic!("expected intersect expr, got `{:?}`", v),
        }

        match parse_expr("func(set(a b c))") {
            Ok(Spanned {
                value: Expr::Function { .. },
                ..
            }) => {}
            v => panic!("expected function expr, got `{:?}`", v),
        }

        match parse_expr("//:tgt") {
            Ok(Spanned {
                value: Expr::String("//:tgt"),
                ..
            }) => {}
            v => panic!("expected '//:tgt', got `{:?}`", v),
        }

        Ok(())
    }

    fn run_tests<'a, O: Debug, F: FnMut(Span<'a>) -> IResult<Span<'a>, O, ()> + Copy>(
        mut parser: F,
        good_cases: &'a [&'a str],
        recoverable_cases: &'a [&'a str],
        nonrecoverable_cases: &'a [&'a str],
    ) {
        for case in good_cases {
            // good_cases the parser should consume the whole thing
            // maybe should consider splitting it into two sets of cases for the caller to
            // explicitly indicate the ones that should be all consuming
            all_consuming(parser)(Span::new(case)).unwrap_or_else(|err| {
                panic!(
                    "Expected successful, complete parse for input `{}`. Got error `{:?}`",
                    case, err
                )
            });
        }

        for case in recoverable_cases {
            assert_err(
                parser(Span::new(case)),
                &format!("Expected recoverable error for input `{}`.", case),
            );
        }

        for case in nonrecoverable_cases {
            assert_fail(
                parser(Span::new(case)),
                &format!("Expected hard failure for input `{}`.", case),
            );
        }
    }

    #[test]
    fn test_function() -> anyhow::Result<()> {
        run_tests(
            expr_function,
            &["a()", "a(a)", "a(a, b, c)", "a(a,   b,   c)"],
            // As long as we don't match "name(", it should be recoverable
            &["xyz", "", "(", ",a(", " a()", "a,a()"],
            // An error after the name + opening paren is non-recoverable
            &["a(", "a(a()", "a(abc, ", "a(abc, ()"],
        );
        Ok(())
    }

    #[test]
    fn test_set() -> anyhow::Result<()> {
        run_tests(
            expr_set,
            &["set(a)", "set(a b c)", "set(a   b   c)", "set(%s)", "set()"],
            // As long as we don't match "set(", it should be recoverable
            &["set", "", "(", "abc(", " set()", ",set()"],
            // An error after "set" + opening paren is non-recoverable
            &["set(", "set(a,)", "set(abc ", "set(abc ()"],
        );
        Ok(())
    }

    #[test]
    fn test_word() -> anyhow::Result<()> {
        run_tests(
            expr_word,
            &[
                "@cell//package:target",
                "'quoted'",
                "regex.*$#",
                r#""double^  -quoted""#,
                "%s",
                "%Ss",
            ],
            // there's not a lot of errors
            &["^word", "", ",word"],
            // unfinished quotes are unrecoverable.
            &["'unfinished quote", "'wrong quote end\""],
        );
        Ok(())
    }

    #[test]
    fn test_integer() -> anyhow::Result<()> {
        run_tests(expr_int, &["0", "1234"], &["w123", ".1", ""], &["0123"]);
        Ok(())
    }

    #[test]
    fn test_trailing_infix() -> anyhow::Result<()> {
        run_tests(
            trailing_infix,
            &[
                " + b",
                " + (b)",
                " ^ (b)",
                " - (b)- (c) -(d)-(e)",
                " intersect b",
                " union b",
                " except b",
            ],
            // there's not a lot of errors
            &["", "| b", " |b"],
            &[],
        );
        Ok(())
    }
}
