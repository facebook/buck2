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

use std::collections::HashSet;

use gazebo::prelude::*;
use thiserror::Error;

use crate::codemap::CodeMap;
use crate::codemap::Spanned;
use crate::errors::Diagnostic;
use crate::syntax::ast::Argument;
use crate::syntax::ast::Assign;
use crate::syntax::ast::AssignIdentP;
use crate::syntax::ast::AssignOp;
use crate::syntax::ast::AssignP;
use crate::syntax::ast::AstArgument;
use crate::syntax::ast::AstAssign;
use crate::syntax::ast::AstAssignIdent;
use crate::syntax::ast::AstExpr;
use crate::syntax::ast::AstParameter;
use crate::syntax::ast::AstStmt;
use crate::syntax::ast::AstString;
use crate::syntax::ast::Expr;
use crate::syntax::ast::Parameter;
use crate::syntax::ast::Stmt;
use crate::syntax::Dialect;

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
    #[error("left-hand-side of assignment must take the form `a`, `a.b` or `a[b]`")]
    InvalidLhs,
    #[error("left-hand-side of modifying assignment cannot be a list or tuple")]
    InvalidModifyLhs,
    #[error("type annotations not allowed on augmented assignments")]
    TypeAnnotationOnAssignOp,
    #[error("type annotations not allowed on multiple assignments")]
    TypeAnnotationOnTupleAssign,
}

#[derive(Eq, PartialEq, Ord, PartialOrd)]
enum ArgsStage {
    Positional,
    Named,
    Args,
    Kwargs,
}

#[derive(Error, Debug)]
enum ArgumentDefinitionOrderError {
    #[error("positional argument after non positional")]
    PositionalThenNonPositional,
    #[error("named argument after *args or **kwargs")]
    NamedArgumentAfterStars,
    #[error("repeated named argument")]
    RepeatedNamed,
    #[error("Args array after another args or kwargs")]
    ArgsArrayAfterArgsOrKwargs,
    #[error("Multiple kwargs dictionary in arguments")]
    MultipleKwargs,
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
    pub(crate) fn check_call(
        f: AstExpr,
        args: Vec<AstArgument>,
        codemap: &CodeMap,
    ) -> anyhow::Result<Expr> {
        let err = |span, msg| Err(Diagnostic::new(msg, span, codemap));

        let mut stage = ArgsStage::Positional;
        let mut named_args = HashSet::new();
        for arg in &args {
            match &arg.node {
                Argument::Positional(_) => {
                    if stage != ArgsStage::Positional {
                        return err(
                            arg.span,
                            ArgumentDefinitionOrderError::PositionalThenNonPositional,
                        );
                    }
                }
                Argument::Named(n, _) => {
                    if stage > ArgsStage::Named {
                        return err(
                            arg.span,
                            ArgumentDefinitionOrderError::NamedArgumentAfterStars,
                        );
                    } else if !named_args.insert(&n.node) {
                        // Check the names are distinct
                        return err(n.span, ArgumentDefinitionOrderError::RepeatedNamed);
                    } else {
                        stage = ArgsStage::Named;
                    }
                }
                Argument::Args(_) => {
                    if stage > ArgsStage::Named {
                        return err(
                            arg.span,
                            ArgumentDefinitionOrderError::ArgsArrayAfterArgsOrKwargs,
                        );
                    } else {
                        stage = ArgsStage::Args;
                    }
                }
                Argument::KwArgs(_) => {
                    if stage == ArgsStage::Kwargs {
                        return err(arg.span, ArgumentDefinitionOrderError::MultipleKwargs);
                    } else {
                        stage = ArgsStage::Kwargs;
                    }
                }
            }
        }
        Ok(Expr::Call(box f, args))
    }
}

fn test_param_name<'a, T>(
    argset: &mut HashSet<&'a str>,
    n: &'a AstAssignIdent,
    arg: &Spanned<T>,
    codemap: &CodeMap,
) -> anyhow::Result<()> {
    if argset.contains(n.node.0.as_str()) {
        return Err(Diagnostic::new(
            ArgumentUseOrderError::DuplicateParameterName,
            arg.span,
            codemap,
        ));
    }
    argset.insert(&n.node.0);
    Ok(())
}

#[derive(Error, Debug)]
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

fn check_parameters(parameters: &[AstParameter], codemap: &CodeMap) -> anyhow::Result<()> {
    let err = |span, msg| Err(Diagnostic::new(msg, span, codemap));

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

impl Expr {
    pub(crate) fn check_lambda(
        parameters: Vec<AstParameter>,
        body: AstExpr,
        codemap: &CodeMap,
    ) -> anyhow::Result<Expr> {
        check_parameters(&parameters, codemap)?;
        Ok(Expr::Lambda(parameters, box body, ()))
    }
}

impl Stmt {
    pub(crate) fn check_def(
        name: AstString,
        parameters: Vec<AstParameter>,
        return_type: Option<Box<AstExpr>>,
        stmts: AstStmt,
        codemap: &CodeMap,
    ) -> anyhow::Result<Stmt> {
        check_parameters(&parameters, codemap)?;
        let name = name.into_map(|s| AssignIdentP(s, ()));
        Ok(Stmt::Def(name, parameters, return_type, box stmts, ()))
    }

    pub(crate) fn check_assign(codemap: &CodeMap, x: AstExpr) -> anyhow::Result<AstAssign> {
        Ok(Spanned {
            span: x.span,
            node: match x.node {
                Expr::Tuple(xs) | Expr::List(xs) => {
                    Assign::Tuple(xs.into_try_map(|x| Self::check_assign(codemap, x))?)
                }
                Expr::Dot(a, b) => Assign::Dot(a, b),
                Expr::ArrayIndirection(box (a, b)) => Assign::ArrayIndirection(box (a, b)),
                Expr::Identifier(x, ()) => Assign::Identifier(x.into_map(|s| AssignIdentP(s, ()))),
                _ => {
                    return Err(Diagnostic::new(ValidateError::InvalidLhs, x.span, codemap));
                }
            },
        })
    }

    pub(crate) fn check_assignment(
        codemap: &CodeMap,
        lhs: AstExpr,
        ty: Option<Box<AstExpr>>,
        op: Option<AssignOp>,
        rhs: AstExpr,
    ) -> anyhow::Result<Stmt> {
        if op.is_some() {
            // for augmented assignment, Starlark doesn't allow tuple/list
            match &lhs.node {
                Expr::Tuple(_) | Expr::List(_) => {
                    return Err(Diagnostic::new(
                        ValidateError::InvalidModifyLhs,
                        lhs.span,
                        codemap,
                    ));
                }
                _ => {}
            }
        }
        let lhs = Self::check_assign(codemap, lhs)?;
        if let Some(ty) = &ty {
            let err = if op.is_some() {
                Some(ValidateError::TypeAnnotationOnAssignOp)
            } else if matches!(lhs.node, AssignP::Tuple(_)) {
                Some(ValidateError::TypeAnnotationOnTupleAssign)
            } else {
                None
            };
            if let Some(err) = err {
                return Err(Diagnostic::new(err, ty.span, codemap));
            }
        }
        Ok(match op {
            None => Stmt::Assign(lhs, box (ty.map(|x| *x), rhs)),
            Some(op) => Stmt::AssignModify(lhs, op, box rhs),
        })
    }

    /// Validate all statements only occur where they are allowed to.
    pub(crate) fn validate(
        codemap: &CodeMap,
        stmt: &AstStmt,
        dialect: &Dialect,
    ) -> anyhow::Result<()> {
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
        ) -> anyhow::Result<()> {
            let err = |x| Err(Diagnostic::new(x, stmt.span, codemap));

            match &stmt.node {
                Stmt::Def(_, _, _, body, _payload) => f(codemap, dialect, body, false, false, true),
                Stmt::For(_, box (_, body)) => {
                    if top_level && !dialect.enable_top_level_stmt {
                        err(ValidateError::NoTopLevelFor)
                    } else {
                        f(codemap, dialect, body, false, true, inside_def)
                    }
                }
                Stmt::If(..) | Stmt::IfElse(..) => {
                    if top_level && !dialect.enable_top_level_stmt {
                        err(ValidateError::NoTopLevelIf)
                    } else {
                        stmt.node.visit_stmt_result(|x| {
                            f(codemap, dialect, x, false, inside_for, inside_def)
                        })
                    }
                }
                Stmt::Break if !inside_for => err(ValidateError::BreakOutsideLoop),
                Stmt::Continue if !inside_for => err(ValidateError::ContinueOutsideLoop),
                Stmt::Return(_) if !inside_def => err(ValidateError::ReturnOutsideDef),
                Stmt::Load(..) if !top_level => err(ValidateError::LoadNotTop),
                _ => stmt.node.visit_stmt_result(|x| {
                    f(codemap, dialect, x, top_level, inside_for, inside_def)
                }),
            }
        }

        f(codemap, dialect, stmt, true, false, false)
    }
}
