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

use crate::codemap::CodeMap;
use crate::codemap::Spanned;
use crate::eval::compiler::EvalException;
use crate::slice_vec_ext::VecExt;
use crate::syntax::ast::Assign;
use crate::syntax::ast::AssignIdentP;
use crate::syntax::ast::AssignOp;
use crate::syntax::ast::AssignP;
use crate::syntax::ast::AstAssign;
use crate::syntax::ast::AstExpr;
use crate::syntax::ast::AstStmt;
use crate::syntax::ast::AstTypeExpr;
use crate::syntax::ast::Expr;
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
