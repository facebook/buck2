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

//! AST for parsed starlark files.

use thiserror::Error;

use crate::codemap::CodeMap;
use crate::eval_exception::EvalException;
use crate::syntax::ast::AstArgument;
use crate::syntax::ast::AstExpr;
use crate::syntax::ast::AstLiteral;
use crate::syntax::ast::AstStmt;
use crate::syntax::ast::CallArgsP;
use crate::syntax::ast::DefP;
use crate::syntax::ast::Expr;
use crate::syntax::ast::ForP;
use crate::syntax::ast::Stmt;
use crate::syntax::call::CallArgsUnpack;
use crate::syntax::Dialect;
use crate::syntax::DialectTypes;

#[derive(Error, Debug)]
enum ValidateError {
    #[error("`break` cannot be used outside of a `for` loop")]
    BreakOutsideLoop,
    #[error("`continue` cannot be used outside of a `for` loop")]
    ContinueOutsideLoop,
    #[error("`return` cannot be used outside of a `def` function")]
    ReturnOutsideDef,
    #[error("`load` must only occur at the top of a module")]
    LoadNotTop,
    #[error("`if` cannot be used outside `def` in this dialect")]
    NoTopLevelIf,
    #[error("`for` cannot be used outside `def` in this dialect")]
    NoTopLevelFor,
    #[error("`load` is not allowed in this dialect")]
    Load,
    #[error("`...` is not allowed in this dialect")]
    Ellipsis,
}

impl Expr {
    /// We want to check a function call is well-formed.
    /// Our eventual plan is to follow the Python invariants, but for now, we are closer
    /// to the Starlark invariants.
    ///
    /// Python invariants are no positional arguments after named arguments,
    /// no *args after **kwargs, no repeated argument names.
    ///
    /// Starlark invariants are the above, plus at most one *args and the *args must appear
    /// after all positional and named arguments. The spec is silent on whether you are allowed
    /// multiple **kwargs.
    ///
    /// We allow at most one **kwargs.
    pub fn check_call(
        f: AstExpr,
        args: Vec<AstArgument>,
        codemap: &CodeMap,
    ) -> Result<Expr, EvalException> {
        let args = CallArgsP { args };

        CallArgsUnpack::unpack(&args, codemap)?;

        Ok(Expr::Call(Box::new(f), args))
    }
}

impl Stmt {
    /// Validate all statements only occur where they are allowed to.
    pub fn validate(
        codemap: &CodeMap,
        stmt: &AstStmt,
        dialect: &Dialect,
    ) -> Result<(), EvalException> {
        // Inside a for, we allow continue/break, unless we go beneath a def.
        // Inside a def, we allow return.
        // All load's must occur at the top-level.
        // At the top-level we only allow for/if when the dialect permits it.
        fn f(
            codemap: &CodeMap,
            dialect: &Dialect,
            stmt: &AstStmt,
            top_level: bool,
            inside_for: bool,
            inside_def: bool,
        ) -> Result<(), EvalException> {
            let err = |x: anyhow::Error| Err(EvalException::new_anyhow(x, stmt.span, codemap));

            match &stmt.node {
                Stmt::Def(DefP { body, .. }) => f(codemap, dialect, body, false, false, true),
                Stmt::For(ForP { body, .. }) => {
                    if top_level && !dialect.enable_top_level_stmt {
                        err(ValidateError::NoTopLevelFor.into())
                    } else {
                        f(codemap, dialect, body, false, true, inside_def)
                    }
                }
                Stmt::If(..) | Stmt::IfElse(..) => {
                    if top_level && !dialect.enable_top_level_stmt {
                        err(ValidateError::NoTopLevelIf.into())
                    } else {
                        stmt.node.visit_stmt_result(|x| {
                            f(codemap, dialect, x, false, inside_for, inside_def)
                        })
                    }
                }
                Stmt::Break if !inside_for => err(ValidateError::BreakOutsideLoop.into()),
                Stmt::Continue if !inside_for => err(ValidateError::ContinueOutsideLoop.into()),
                Stmt::Return(_) if !inside_def => err(ValidateError::ReturnOutsideDef.into()),
                Stmt::Load(..) => {
                    if !top_level {
                        return err(ValidateError::LoadNotTop.into());
                    }
                    if !dialect.enable_load {
                        return err(ValidateError::Load.into());
                    }
                    Ok(())
                }
                _ => stmt.node.visit_stmt_result(|x| {
                    f(codemap, dialect, x, top_level, inside_for, inside_def)
                }),
            }
        }

        fn expr(expr: &AstExpr, dialect: &Dialect, codemap: &CodeMap) -> Result<(), EvalException> {
            if let Expr::Literal(AstLiteral::Ellipsis) = &expr.node {
                if dialect.enable_types == DialectTypes::Disable {
                    return Err(EvalException::new_anyhow(
                        ValidateError::Ellipsis.into(),
                        expr.span,
                        codemap,
                    ));
                }
            }
            Ok(())
        }

        f(codemap, dialect, stmt, true, false, false)?;

        stmt.visit_expr_result(|x| expr(x, dialect, codemap))?;

        Ok(())
    }
}
