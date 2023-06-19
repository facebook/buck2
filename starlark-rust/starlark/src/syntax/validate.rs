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

use thiserror::Error;

use crate::codemap::CodeMap;
use crate::codemap::Spanned;
use crate::eval::compiler::EvalException;
use crate::slice_vec_ext::VecExt;
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
use crate::syntax::ast::AstTypeExpr;
use crate::syntax::ast::DefP;
use crate::syntax::ast::Expr;
use crate::syntax::ast::LambdaP;
use crate::syntax::ast::Parameter;
use crate::syntax::ast::Stmt;
use crate::syntax::dialect::DialectError;
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

#[derive(Eq, PartialEq, PartialOrd, Ord)]
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
    ) -> Result<Expr, EvalException> {
        let err = |span, msg: ArgumentDefinitionOrderError| {
            Err(EvalException::new(msg.into(), span, codemap))
        };

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
        Ok(Expr::Call(Box::new(f), args))
    }
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

impl Expr {
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
}

impl Stmt {
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

    pub(crate) fn check_assign(codemap: &CodeMap, x: AstExpr) -> Result<AstAssign, EvalException> {
        Ok(Spanned {
            span: x.span,
            node: match x.node {
                Expr::Tuple(xs) | Expr::List(xs) => {
                    Assign::Tuple(xs.into_try_map(|x| Self::check_assign(codemap, x))?)
                }
                Expr::Dot(a, b) => Assign::Dot(a, b),
                Expr::ArrayIndirection(a_b) => Assign::ArrayIndirection(a_b),
                Expr::Identifier(x) => Assign::Identifier(x.into_map(|s| AssignIdentP(s.0, ()))),
                _ => {
                    return Err(EvalException::new(
                        ValidateError::InvalidLhs.into(),
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
                        ValidateError::InvalidModifyLhs.into(),
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
                return Err(EvalException::new(err.into(), ty.span, codemap));
            }
        }
        Ok(match op {
            None => Stmt::Assign(lhs, Box::new((ty.map(|x| *x), rhs))),
            Some(op) => Stmt::AssignModify(lhs, op, Box::new(rhs)),
        })
    }

    /// Validate all statements only occur where they are allowed to.
    pub(crate) fn validate(
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
            let err = |x: anyhow::Error| Err(EvalException::new(x, stmt.span, codemap));

            match &stmt.node {
                Stmt::Def(DefP { body, .. }) => f(codemap, dialect, body, false, false, true),
                Stmt::For(_, over_body) => {
                    let (_, body) = &**over_body;
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
                        return err(DialectError::Load.into());
                    }
                    Ok(())
                }
                _ => stmt.node.visit_stmt_result(|x| {
                    f(codemap, dialect, x, top_level, inside_for, inside_def)
                }),
            }
        }

        f(codemap, dialect, stmt, true, false, false)
    }
}
