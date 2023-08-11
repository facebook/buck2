/*
 * Copyright 2019 The Starlark in Rust Authors.
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

use std::collections::HashMap;
use std::sync::Arc;

use dupe::Dupe;
use starlark_map::small_map::SmallMap;

use crate::codemap::CodeMap;
use crate::codemap::Span;
use crate::codemap::Spanned;
use crate::eval::compiler::scope::payload::CstAssign;
use crate::eval::compiler::scope::payload::CstExpr;
use crate::eval::compiler::scope::payload::CstPayload;
use crate::eval::compiler::scope::payload::CstStmt;
use crate::eval::compiler::scope::BindingId;
use crate::eval::compiler::scope::ResolvedIdent;
use crate::syntax::ast::AssignOp;
use crate::syntax::ast::AssignP;
use crate::syntax::ast::AssignTargetP;
use crate::syntax::ast::ClauseP;
use crate::syntax::ast::DefP;
use crate::syntax::ast::ExprP;
use crate::syntax::ast::ForClauseP;
use crate::syntax::ast::ForP;
use crate::syntax::ast::IdentP;
use crate::syntax::ast::ParameterP;
use crate::syntax::ast::StmtP;
use crate::syntax::uniplate::Visit;
use crate::typing::error::InternalError;
use crate::typing::function::Param;
use crate::typing::mode::TypecheckMode;
use crate::typing::ty::Approximation;
use crate::typing::ty::Ty;

#[derive(Clone)]
pub(crate) enum BindExpr<'a> {
    Expr(&'a CstExpr),
    /// Get this position from the expression
    GetIndex(usize, Box<BindExpr<'a>>),
    Iter(Box<BindExpr<'a>>),
    AssignModify(&'a CstAssign, AssignOp, &'a CstExpr),
    /// Set this index in the variable
    SetIndex(BindingId, &'a CstExpr, Box<BindExpr<'a>>),
    ListAppend(BindingId, &'a CstExpr),
    ListExtend(BindingId, &'a CstExpr),
}

impl<'a> BindExpr<'a> {
    pub(crate) fn span(&self) -> Span {
        match self {
            BindExpr::Expr(x) => x.span,
            BindExpr::GetIndex(_, x) => x.span(),
            BindExpr::Iter(x) => x.span(),
            BindExpr::AssignModify(x, _, _) => x.span,
            BindExpr::SetIndex(_, x, _) => x.span,
            BindExpr::ListAppend(_, x) => x.span,
            BindExpr::ListExtend(_, x) => x.span,
        }
    }
}

#[derive(Default)]
pub(crate) struct Bindings<'a> {
    pub(crate) expressions: SmallMap<BindingId, Vec<BindExpr<'a>>>,
    /// Non-inferred types of bindings: from `load`,
    /// or from variable or function parameter type annotations.
    pub(crate) types: HashMap<BindingId, Ty>,
    /// Expressions which need to be typechecked, but which are not used
    /// in assignments or in other expressions.
    /// For example `expr` in:
    ///
    /// ```python
    /// if expr: ...
    /// ```
    pub(crate) check: Vec<&'a CstExpr>,
    pub(crate) check_type: Vec<(Span, Option<&'a CstExpr>, Ty)>,
}

/// Interface representing the types of all bindings in a module.
#[derive(Default, Dupe, Clone, Debug)]
pub struct Interface(Arc<HashMap<String, Ty>>);

impl Interface {
    /// Create an empty interface, with no bindings.
    pub fn empty() -> Self {
        Self::default()
    }

    /// Create a new interface with the given bindings.
    pub fn new(bindings: HashMap<String, Ty>) -> Self {
        Self(Arc::new(bindings))
    }

    /// Get the type for a given binding.
    pub fn get(&self, name: &str) -> Option<&Ty> {
        self.0.get(name)
    }
}

#[derive(Default)]
pub(crate) struct BindingsCollect<'a> {
    pub(crate) bindings: Bindings<'a>,
    pub(crate) approximations: Vec<Approximation>,
}

impl<'a> BindingsCollect<'a> {
    /// Collect all the assignments to variables.
    ///
    /// This function only fails on internal errors.
    pub(crate) fn collect(
        xs: &'a [CstStmt],
        typecheck_mode: TypecheckMode,
        codemap: &CodeMap,
    ) -> Result<Self, InternalError> {
        let mut res = BindingsCollect::default();
        for x in xs {
            res.visit(Visit::Stmt(x), &Ty::any(), typecheck_mode, codemap)?;
        }
        Ok(res)
    }

    fn assign(
        &mut self,
        lhs: &'a CstAssign,
        rhs: BindExpr<'a>,
        codemap: &CodeMap,
    ) -> Result<(), InternalError> {
        match &**lhs {
            AssignTargetP::Identifier(x) => {
                self.bindings
                    .expressions
                    .entry(x.resolved_binding_id(codemap)?)
                    .or_default()
                    .push(rhs);
            }
            AssignTargetP::Tuple(xs) => {
                for (i, x) in xs.iter().enumerate() {
                    self.assign(x, BindExpr::GetIndex(i, Box::new(rhs.clone())), codemap)?;
                }
            }
            AssignTargetP::Index(array_index) => match &*array_index.0 {
                ExprP::Identifier(Spanned {
                    span: _,
                    node: IdentP(_name, Some(ResolvedIdent::Slot(_, ident))),
                }) => {
                    self.bindings
                        .expressions
                        .entry(*ident)
                        .or_default()
                        .push(BindExpr::SetIndex(*ident, &array_index.1, Box::new(rhs)));
                }
                _ => {
                    self.approximations.push(Approximation::new(
                        "Underapproximation",
                        "a.b[x] = .. not handled",
                    ));
                }
            },
            AssignTargetP::Dot(_, _) => {
                self.approximations.push(Approximation::new(
                    "Underapproximation",
                    "a.b = .. not handled",
                ));
            }
        }
        Ok(())
    }

    fn visit_def(
        &mut self,
        def: &'a DefP<CstPayload>,
        typecheck_mode: TypecheckMode,
        codemap: &CodeMap,
    ) -> Result<(), InternalError> {
        let DefP {
            name,
            params,
            return_type,
            ..
        } = def;
        let mut params2 = Vec::with_capacity(params.len());
        let mut seen_no_args = false;
        for p in params {
            let name_ty = match &**p {
                ParameterP::Normal(name, ty) | ParameterP::WithDefaultValue(name, ty, _) => {
                    let ty = Ty::from_type_expr_opt(
                        ty,
                        typecheck_mode,
                        &mut self.approximations,
                        codemap,
                    )?;
                    let mut param = if seen_no_args {
                        Param::name_only(&name.0, ty.clone())
                    } else {
                        Param::pos_or_name(&name.0, ty.clone())
                    };
                    if matches!(&**p, ParameterP::WithDefaultValue(..)) {
                        param = param.optional();
                    }
                    params2.push(param);
                    Some((name, ty))
                }
                ParameterP::NoArgs => {
                    seen_no_args = true;
                    None
                }
                ParameterP::Args(name, ty) => {
                    // There is the type we require people calling us use (usually any)
                    // and then separately the type we are when we are running (always tuple)
                    let item_ty = Ty::from_type_expr_opt(
                        ty,
                        typecheck_mode,
                        &mut self.approximations,
                        codemap,
                    )?;
                    // TODO(nga): currently there's no way to express the type `tuple[str, ...]`.
                    params2.push(Param::args(item_ty));
                    Some((name, Ty::any_tuple()))
                }
                ParameterP::KwArgs(name, ty) => {
                    let value_ty = Ty::from_type_expr_opt(
                        ty,
                        typecheck_mode,
                        &mut self.approximations,
                        codemap,
                    )?;
                    let ty = Ty::dict(Ty::string(), value_ty.clone());
                    params2.push(Param::kwargs(value_ty));
                    Some((name, ty))
                }
            };
            if let Some((name, ty)) = name_ty {
                self.bindings
                    .types
                    .insert(name.resolved_binding_id(codemap)?, ty);
            }
        }
        let ret_ty = Ty::from_type_expr_opt(
            return_type,
            typecheck_mode,
            &mut self.approximations,
            codemap,
        )?;
        self.bindings.types.insert(
            name.resolved_binding_id(codemap)?,
            Ty::function(params2, ret_ty.clone()),
        );
        def.visit_children_err(|x| self.visit(x, &ret_ty, typecheck_mode, codemap))?;
        Ok(())
    }

    fn visit(
        &mut self,
        x: Visit<'a, CstPayload>,
        return_type: &Ty,
        typecheck_mode: TypecheckMode,
        codemap: &CodeMap,
    ) -> Result<(), InternalError> {
        match x {
            Visit::Stmt(x) => match &**x {
                StmtP::Assign(AssignP { lhs, ty, rhs }) => {
                    if let Some(ty) = ty {
                        let ty2 = Ty::from_type_expr(
                            ty,
                            typecheck_mode,
                            &mut self.approximations,
                            codemap,
                        )?;
                        self.bindings
                            .check_type
                            .push((ty.span, Some(rhs), ty2.clone()));
                        if let AssignTargetP::Identifier(id) = &**lhs {
                            // FIXME: This could be duplicated if you declare the type of a variable twice,
                            // we would only see the second one.
                            self.bindings
                                .types
                                .insert(id.resolved_binding_id(codemap)?, ty2);
                        }
                    }
                    self.assign(lhs, BindExpr::Expr(rhs), codemap)?
                }
                StmtP::AssignModify(lhs, op, rhs) => {
                    self.assign(lhs, BindExpr::AssignModify(lhs, *op, rhs), codemap)?
                }
                StmtP::For(ForP { var, over, body: _ }) => {
                    self.assign(var, BindExpr::Iter(Box::new(BindExpr::Expr(over))), codemap)?
                }
                StmtP::Def(def) => {
                    self.visit_def(def, typecheck_mode, codemap)?;
                    // We do our own visit_children, with a different return type
                    return Ok(());
                }
                StmtP::Load(x) => {
                    let mp = &x.payload;
                    for (ident, _load) in &x.args {
                        let ty = mp.get(ident.0.as_str()).cloned().unwrap_or_else(Ty::any);
                        self.bindings
                            .types
                            .insert(ident.resolved_binding_id(codemap)?, ty);
                    }
                }
                StmtP::Return(ret) => {
                    self.bindings
                        .check_type
                        .push((x.span, ret.as_ref(), return_type.clone()))
                }
                StmtP::Expression(x) => {
                    // We want to find ident.append(), ident.extend(), ident.extend()
                    // to fake up a BindExpr::ListAppend/ListExtend
                    // so that mutating list operations aren't invisible to us
                    if let ExprP::Call(fun, args) = &**x {
                        if let ExprP::Dot(id, attr) = &***fun {
                            if let ExprP::Identifier(id) = &id.node {
                                let res = match attr.as_str() {
                                    "append" if args.len() == 1 => Some((false, 0)),
                                    "insert" if args.len() == 2 => Some((false, 1)),
                                    "extend" if args.len() == 1 => Some((true, 0)),
                                    _ => None,
                                };
                                if let Some((extend, arg)) = res {
                                    if let ResolvedIdent::Slot(_, id) = id.node.1.as_ref().unwrap()
                                    {
                                        let bind = if extend {
                                            BindExpr::ListExtend(*id, args[arg].expr())
                                        } else {
                                            BindExpr::ListAppend(*id, args[arg].expr())
                                        };
                                        self.bindings.expressions.entry(*id).or_default().push(bind)
                                    }
                                }
                            }
                        }
                    }

                    self.bindings.check.push(x)
                }
                StmtP::If(x, _) => self.bindings.check.push(x),
                StmtP::IfElse(x, _) => self.bindings.check.push(x),
                _ => {}
            },
            Visit::Expr(x) => match &**x {
                ExprP::ListComprehension(_, for1, clauses)
                | ExprP::DictComprehension(_, for1, clauses) => {
                    fn get_for_clause(x: &ClauseP<CstPayload>) -> Option<&ForClauseP<CstPayload>> {
                        match x {
                            ClauseP::For(x) => Some(x),
                            _ => None,
                        }
                    }
                    for x in
                        std::iter::once(&**for1).chain(clauses.iter().filter_map(get_for_clause))
                    {
                        self.assign(
                            &x.var,
                            BindExpr::Iter(Box::new(BindExpr::Expr(&x.over))),
                            codemap,
                        )?
                    }
                }
                _ => {}
            },
        }
        x.visit_children_err(|x| self.visit(x, return_type, typecheck_mode, codemap))?;
        Ok(())
    }
}
