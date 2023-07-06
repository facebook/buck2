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

use std::collections::HashSet;

use crate::codemap::CodeMap;
use crate::codemap::Spanned;
use crate::eval::compiler::EvalException;
use crate::slice_vec_ext::VecExt;
use crate::syntax::ast::Assign;
use crate::syntax::ast::AssignIdentP;
use crate::syntax::ast::AssignOp;
use crate::syntax::ast::AssignP;
use crate::syntax::ast::AstAssign;
use crate::syntax::ast::AstAssignIdent;
use crate::syntax::ast::AstExpr;
use crate::syntax::ast::AstParameter;
use crate::syntax::ast::AstStmt;
use crate::syntax::ast::AstString;
use crate::syntax::ast::AstTypeExpr;
use crate::syntax::ast::DefP;
use crate::syntax::ast::Expr;
use crate::syntax::ast::LambdaP;
use crate::syntax::ast::Parameter;
use crate::syntax::ast::Stmt;
use crate::syntax::ast::StmtP;
use crate::syntax::ast::ToAst;

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
}

/// Ensure we produce normalised Statements, rather than singleton Statements
pub(crate) fn statements(mut xs: Vec<AstStmt>, begin: usize, end: usize) -> AstStmt {
    if xs.len() == 1 {
        xs.pop().unwrap()
    } else {
        StmtP::Statements(xs).ast(begin, end)
    }
}

pub(crate) fn check_assign(codemap: &CodeMap, x: AstExpr) -> Result<AstAssign, EvalException> {
    Ok(Spanned {
        span: x.span,
        node: match x.node {
            Expr::Tuple(xs) | Expr::List(xs) => {
                Assign::Tuple(xs.into_try_map(|x| check_assign(codemap, x))?)
            }
            Expr::Dot(a, b) => Assign::Dot(a, b),
            Expr::Index(a_b) => Assign::Index(a_b),
            Expr::Identifier(x) => Assign::Identifier(x.into_map(|s| AssignIdentP(s.0, ()))),
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
        } else if matches!(lhs.node, AssignP::Tuple(_)) {
            Some(GrammarUtilError::TypeAnnotationOnTupleAssign)
        } else {
            None
        };
        if let Some(err) = err {
            return Err(EvalException::new(err.into(), ty.span, codemap));
        }
    }
    Ok(match op {
        None => Stmt::Assign(lhs, Box::new((ty.map(|x| *x), rhs))),
        Some(op) => Stmt::AssignModify(lhs, op, Box::new(rhs)),
    })
}

#[derive(thiserror::Error, Debug)]
enum ArgumentUseOrderError {
    #[error("duplicated parameter name")]
    DuplicateParameterName,
    #[error("positional parameter after non positional")]
    PositionalThenNonPositional,
    #[error("Default parameter after args array or kwargs dictionary")]
    DefaultParameterAfterStars,
    #[error("Args parameter after another args or kwargs parameter")]
    ArgsParameterAfterStars,
    #[error("Multiple kwargs dictionary in parameters")]
    MultipleKwargs,
}

fn test_param_name<'a, T>(
    argset: &mut HashSet<&'a str>,
    n: &'a AstAssignIdent,
    arg: &Spanned<T>,
    codemap: &CodeMap,
) -> Result<(), EvalException> {
    if argset.contains(n.node.0.as_str()) {
        return Err(EvalException::new(
            ArgumentUseOrderError::DuplicateParameterName.into(),
            arg.span,
            codemap,
        ));
    }
    argset.insert(&n.node.0);
    Ok(())
}

fn check_parameters(parameters: &[AstParameter], codemap: &CodeMap) -> Result<(), EvalException> {
    let err = |span, msg: ArgumentUseOrderError| Err(EvalException::new(msg.into(), span, codemap));

    // you can't repeat argument names
    let mut argset = HashSet::new();
    // You can't have more than one *args/*, **kwargs
    // **kwargs must be last
    // You can't have a required `x` after an optional `y=1`
    let mut seen_args = false;
    let mut seen_kwargs = false;
    let mut seen_optional = false;

    for arg in parameters.iter() {
        match &arg.node {
            Parameter::Normal(n, ..) => {
                if seen_kwargs || seen_optional {
                    return err(arg.span, ArgumentUseOrderError::PositionalThenNonPositional);
                }
                test_param_name(&mut argset, n, arg, codemap)?;
            }
            Parameter::WithDefaultValue(n, ..) => {
                if seen_kwargs {
                    return err(arg.span, ArgumentUseOrderError::DefaultParameterAfterStars);
                }
                seen_optional = true;
                test_param_name(&mut argset, n, arg, codemap)?;
            }
            Parameter::NoArgs => {
                if seen_args || seen_kwargs {
                    return err(arg.span, ArgumentUseOrderError::ArgsParameterAfterStars);
                }
                seen_args = true;
            }
            Parameter::Args(n, ..) => {
                if seen_args || seen_kwargs {
                    return err(arg.span, ArgumentUseOrderError::ArgsParameterAfterStars);
                }
                seen_args = true;
                test_param_name(&mut argset, n, arg, codemap)?;
            }
            Parameter::KwArgs(n, ..) => {
                if seen_kwargs {
                    return err(arg.span, ArgumentUseOrderError::MultipleKwargs);
                }
                seen_kwargs = true;
                test_param_name(&mut argset, n, arg, codemap)?;
            }
        }
    }
    Ok(())
}

pub(crate) fn check_lambda(
    params: Vec<AstParameter>,
    body: AstExpr,
    codemap: &CodeMap,
) -> Result<Expr, EvalException> {
    check_parameters(&params, codemap)?;
    Ok(Expr::Lambda(LambdaP {
        params,
        body: Box::new(body),
        payload: (),
    }))
}

pub(crate) fn check_def(
    name: AstString,
    params: Vec<AstParameter>,
    return_type: Option<Box<AstTypeExpr>>,
    stmts: AstStmt,
    codemap: &CodeMap,
) -> Result<Stmt, EvalException> {
    check_parameters(&params, codemap)?;
    let name = name.into_map(|s| AssignIdentP(s, ()));
    Ok(Stmt::Def(DefP {
        name,
        params,
        return_type,
        body: Box::new(stmts),
        payload: (),
    }))
}
