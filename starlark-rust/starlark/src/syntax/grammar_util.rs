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

//! Code called by the parser to handle complex cases not handled by the grammar.

use starlark_syntax::dot_format_parser::FormatParser;
use starlark_syntax::dot_format_parser::FormatToken;
use starlark_syntax::eval_exception::EvalException;
use starlark_syntax::lexer::lex_exactly_one_identifier;
use starlark_syntax::lexer::TokenFString;
use starlark_syntax::slice_vec_ext::VecExt;

use crate::codemap::CodeMap;
use crate::codemap::Pos;
use crate::codemap::Span;
use crate::codemap::Spanned;
use crate::syntax::ast::AssignIdentP;
use crate::syntax::ast::AssignOp;
use crate::syntax::ast::AssignP;
use crate::syntax::ast::AssignTarget;
use crate::syntax::ast::AssignTargetP;
use crate::syntax::ast::AstAssignIdent;
use crate::syntax::ast::AstAssignTarget;
use crate::syntax::ast::AstExpr;
use crate::syntax::ast::AstFString;
use crate::syntax::ast::AstParameter;
use crate::syntax::ast::AstStmt;
use crate::syntax::ast::AstString;
use crate::syntax::ast::AstTypeExpr;
use crate::syntax::ast::DefP;
use crate::syntax::ast::Expr;
use crate::syntax::ast::ExprP;
use crate::syntax::ast::FStringP;
use crate::syntax::ast::IdentP;
use crate::syntax::ast::LambdaP;
use crate::syntax::ast::LoadP;
use crate::syntax::ast::Stmt;
use crate::syntax::ast::StmtP;
use crate::syntax::ast::ToAst;
use crate::syntax::ast::TypeExpr;
use crate::syntax::ast::TypeExprP;
use crate::syntax::def::DefParams;
use crate::syntax::state::ParserState;
use crate::syntax::type_expr::TypeExprUnpackP;
use crate::syntax::Dialect;
use crate::syntax::DialectTypes;

#[derive(Debug, thiserror::Error)]
enum GrammarUtilError {
    #[error("left-hand-side of assignment must take the form `a`, `a.b` or `a[b]`")]
    InvalidLhs,
    #[error("left-hand-side of modifying assignment cannot be a list or tuple")]
    InvalidModifyLhs,
    #[error("type annotations not allowed on augmented assignments")]
    TypeAnnotationOnAssignOp,
    #[error("type annotations not allowed on multiple assignments")]
    TypeAnnotationOnTupleAssign,
    #[error("`load` statement requires at least two arguments")]
    LoadRequiresAtLeastTwoArguments,
}

/// Ensure we produce normalised Statements, rather than singleton Statements
pub(crate) fn statements(mut xs: Vec<AstStmt>, begin: usize, end: usize) -> AstStmt {
    if xs.len() == 1 {
        xs.pop().unwrap()
    } else {
        StmtP::Statements(xs).ast(begin, end)
    }
}

pub(crate) fn check_assign(
    codemap: &CodeMap,
    x: AstExpr,
) -> Result<AstAssignTarget, EvalException> {
    Ok(Spanned {
        span: x.span,
        node: match x.node {
            Expr::Tuple(xs) | Expr::List(xs) => {
                AssignTarget::Tuple(xs.into_try_map(|x| check_assign(codemap, x))?)
            }
            Expr::Dot(a, b) => AssignTarget::Dot(a, b),
            Expr::Index(a_b) => AssignTarget::Index(a_b),
            Expr::Identifier(x) => AssignTarget::Identifier(x.map(|s| AssignIdentP(s.0, ()))),
            _ => {
                return Err(EvalException::new(
                    GrammarUtilError::InvalidLhs.into(),
                    x.span,
                    codemap,
                ));
            }
        },
    })
}

pub(crate) fn check_assignment(
    codemap: &CodeMap,
    lhs: AstExpr,
    ty: Option<Box<AstTypeExpr>>,
    op: Option<AssignOp>,
    rhs: AstExpr,
) -> Result<Stmt, EvalException> {
    if op.is_some() {
        // for augmented assignment, Starlark doesn't allow tuple/list
        match &lhs.node {
            Expr::Tuple(_) | Expr::List(_) => {
                return Err(EvalException::new(
                    GrammarUtilError::InvalidModifyLhs.into(),
                    lhs.span,
                    codemap,
                ));
            }
            _ => {}
        }
    }
    let lhs = check_assign(codemap, lhs)?;
    if let Some(ty) = &ty {
        let err = if op.is_some() {
            Some(GrammarUtilError::TypeAnnotationOnAssignOp)
        } else if matches!(lhs.node, AssignTargetP::Tuple(_)) {
            Some(GrammarUtilError::TypeAnnotationOnTupleAssign)
        } else {
            None
        };
        if let Some(err) = err {
            return Err(EvalException::new(err.into(), ty.span, codemap));
        }
    }
    Ok(match op {
        None => Stmt::Assign(AssignP {
            lhs,
            ty: ty.map(|ty| *ty),
            rhs,
        }),
        Some(op) => Stmt::AssignModify(lhs, op, Box::new(rhs)),
    })
}

fn check_parameters<'a>(parameters: &[AstParameter], parser_state: &mut ParserState<'a>) {
    if let Err(e) = DefParams::unpack(parameters, parser_state.codemap) {
        parser_state.errors.push(e);
    }
}

pub(crate) fn check_lambda(
    params: Vec<AstParameter>,
    body: AstExpr,
    parser_state: &mut ParserState,
) -> Expr {
    check_parameters(&params, parser_state);
    Expr::Lambda(LambdaP {
        params,
        body: Box::new(body),
        payload: (),
    })
}

pub(crate) fn check_def(
    name: AstString,
    params: Vec<AstParameter>,
    return_type: Option<Box<AstTypeExpr>>,
    stmts: AstStmt,
    parser_state: &mut ParserState,
) -> Stmt {
    check_parameters(&params, parser_state);
    let name = name.map(|s| AssignIdentP(s, ()));
    Stmt::Def(DefP {
        name,
        params,
        return_type,
        body: Box::new(stmts),
        payload: (),
    })
}

pub(crate) fn check_load(
    module: AstString,
    args: Vec<(AstAssignIdent, AstString)>,
    parser_state: &mut ParserState,
) -> Stmt {
    if args.is_empty() {
        parser_state.errors.push(EvalException::new(
            GrammarUtilError::LoadRequiresAtLeastTwoArguments.into(),
            module.span,
            parser_state.codemap,
        ));
    }

    Stmt::Load(LoadP {
        module,
        args,
        payload: (),
    })
}

#[derive(thiserror::Error, Debug)]
enum FStringError {
    #[error("Not a valid identifier: `{}`", .capture)]
    InvalidIdentifier { capture: String },

    // Always render the causes for this, but don't expose the error when traversing sources.
    #[error("Invalid format: {:#}", .inner)]
    InvalidFormat { inner: anyhow::Error },

    #[error("Your Starlark dialect must enable f-strings to use them")]
    NotEnabled,
}

pub(crate) fn fstring(
    fstring: TokenFString,
    begin: usize,
    end: usize,
    parser_state: &mut ParserState,
) -> AstFString {
    if !parser_state.dialect.enable_f_strings {
        parser_state.error(
            Span::new(Pos::new(begin as _), Pos::new(end as _)),
            FStringError::NotEnabled,
        );
    }

    let TokenFString {
        content,
        content_start_offset,
    } = fstring;

    let mut format = String::with_capacity(content.len());
    let mut expressions = Vec::new();

    let mut parser = FormatParser::new(&content);
    while let Some(res) = parser.next().transpose() {
        match res {
            Ok(FormatToken::Text(text)) => format.push_str(text),
            Ok(FormatToken::Escape(e)) => {
                // We are producing a format string here so we need to escape this back!
                format.push_str(e.back_to_escape())
            }
            Ok(FormatToken::Capture { capture, pos }) => {
                let capture_begin = begin + content_start_offset + pos;
                let capture_end = capture_begin + capture.len();

                let ident = match lex_exactly_one_identifier(capture) {
                    Some(ident) => ident,
                    None => {
                        parser_state.error(
                            Span::new(Pos::new(capture_begin as _), Pos::new(capture_end as _)),
                            FStringError::InvalidIdentifier {
                                capture: capture.to_owned(),
                            },
                        );
                        // Might as well keep going here. This doesn't compromise the parsing of
                        // the rest of the format string.
                        continue;
                    }
                };

                let expr = ExprP::Identifier(IdentP(ident, ()).ast(capture_begin, capture_end))
                    .ast(capture_begin, capture_end);
                expressions.push(expr);
                format.push_str("{}"); // Positional format.
            }
            Err(inner) => {
                // TODO: Reporting the exact position of the error would be better.
                parser_state.error(
                    Span::new(Pos::new(begin as _), Pos::new(end as _)),
                    FStringError::InvalidFormat { inner },
                );
                break;
            }
        }
    }

    format.shrink_to_fit();

    FStringP {
        format: format.ast(begin, end),
        expressions,
    }
    .ast(begin, end)
}

#[derive(thiserror::Error, Debug)]
enum DialectError {
    #[error("`def` is not allowed in this dialect")]
    Def,
    #[error("`lambda` is not allowed in this dialect")]
    Lambda,
    #[error("* keyword-only-arguments is not allowed in this dialect")]
    KeywordOnlyArguments,
    #[error("type annotations are not allowed in this dialect")]
    Types,
}

fn err<T>(codemap: &CodeMap, span: Span, err: DialectError) -> Result<T, EvalException> {
    Err(EvalException::new(err.into(), span, codemap))
}

pub(crate) fn dialect_check_lambda<T>(
    dialect: &Dialect,
    codemap: &CodeMap,
    x: Spanned<T>,
) -> Result<Spanned<T>, EvalException> {
    if dialect.enable_lambda {
        Ok(x)
    } else {
        err(codemap, x.span, DialectError::Lambda)
    }
}

pub(crate) fn dialect_check_def<T>(
    dialect: &Dialect,
    codemap: &CodeMap,
    x: Spanned<T>,
) -> Result<Spanned<T>, EvalException> {
    if dialect.enable_def {
        Ok(x)
    } else {
        err(codemap, x.span, DialectError::Def)
    }
}

pub(crate) fn dialect_check_keyword_only_arguments<T>(
    dialect: &Dialect,
    codemap: &CodeMap,
    begin: usize,
    end: usize,
    x: T,
) -> Result<T, EvalException> {
    let span = Span::new(Pos::new(begin as u32), Pos::new(end as u32));
    if dialect.enable_keyword_only_arguments {
        Ok(x)
    } else {
        err(codemap, span, DialectError::KeywordOnlyArguments)
    }
}

pub(crate) fn dialect_check_type(
    dialect: &Dialect,
    codemap: &CodeMap,
    x: Spanned<Expr>,
) -> Result<Spanned<TypeExpr>, EvalException> {
    let span = x.span;
    if dialect.enable_types == DialectTypes::Disable {
        return err(codemap, x.span, DialectError::Types);
    }

    TypeExprUnpackP::unpack(&x, codemap)?;

    Ok(x.map(|node| TypeExprP {
        expr: Spanned { node, span },
        payload: (),
    }))
}
