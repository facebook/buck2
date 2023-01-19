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

use crate::codemap::Span;
use crate::eval::compiler::scope::BindingId;
use crate::eval::compiler::scope::CstAssign;
use crate::eval::compiler::scope::CstAssignIdent;
use crate::eval::compiler::scope::CstExpr;
use crate::eval::compiler::scope::CstPayload;
use crate::eval::compiler::scope::CstStmt;
use crate::eval::compiler::scope::ResolvedIdent;
use crate::syntax::ast::AssignOp;
use crate::syntax::ast::AssignP;
use crate::syntax::ast::ClauseP;
use crate::syntax::ast::DefP;
use crate::syntax::ast::ExprP;
use crate::syntax::ast::ForClauseP;
use crate::syntax::ast::ParameterP;
use crate::syntax::ast::StmtP;
use crate::syntax::uniplate::Visit;
use crate::typing::ty::*;

#[derive(Clone)]
pub(crate) enum BindExpr<'a> {
    Expr(&'a CstExpr),
    /// Get this position from the expression
    GetIndex(usize, Box<BindExpr<'a>>),
    Iter(Box<BindExpr<'a>>),
    AssignOp(&'a CstAssign, AssignOp, &'a CstExpr),
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
            BindExpr::AssignOp(x, _, _) => x.span,
            BindExpr::SetIndex(_, x, _) => x.span,
            BindExpr::ListAppend(_, x) => x.span,
            BindExpr::ListExtend(_, x) => x.span,
        }
    }
}

#[derive(Default)]
pub(crate) struct Bindings<'a> {
    pub(crate) expressions: HashMap<BindingId, Vec<BindExpr<'a>>>,
    pub(crate) descriptions: HashMap<BindingId, &'a CstAssignIdent>,
    pub(crate) types: HashMap<BindingId, Ty>,
    pub(crate) check: Vec<&'a CstExpr>,
    pub(crate) check_type: Vec<(Span, Option<&'a CstExpr>, Ty)>,
    pub(crate) approximations: Vec<Approximation>,
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

pub type Loads = HashMap<String, Interface>;

impl<'a> Bindings<'a> {
    /// Collect all the assignments to variables
    pub(crate) fn collect(x: &'a CstStmt, loads: &'_ Loads) -> Self {
        fn assign<'a>(lhs: &'a CstAssign, rhs: BindExpr<'a>, bindings: &mut Bindings<'a>) {
            match &**lhs {
                AssignP::Identifier(x) => {
                    bindings.descriptions.insert(x.1.unwrap(), x);
                    bindings
                        .expressions
                        .entry(x.1.unwrap())
                        .or_default()
                        .push(rhs);
                }
                AssignP::Tuple(xs) => {
                    for (i, x) in xs.iter().enumerate() {
                        assign(x, BindExpr::GetIndex(i, Box::new(rhs.clone())), bindings)
                    }
                }
                AssignP::ArrayIndirection(array_index) => match &*array_index.0 {
                    ExprP::Identifier(_name, Some(ResolvedIdent::Slot((_, ident)))) => {
                        bindings
                            .expressions
                            .entry(*ident)
                            .or_default()
                            .push(BindExpr::SetIndex(*ident, &array_index.1, Box::new(rhs)));
                    }
                    _ => {
                        bindings.approximations.push(Approximation::new(
                            "Underapproximation",
                            "a.b[x] = .. not handled",
                        ));
                    }
                },
                AssignP::Dot(_, _) => {
                    bindings.approximations.push(Approximation::new(
                        "Underapproximation",
                        "a.b = .. not handled",
                    ));
                }
            }
        }

        fn visit<'a>(
            x: Visit<'a, CstPayload>,
            return_type: &Ty,
            loads: &Loads,
            bindings: &mut Bindings<'a>,
        ) {
            match x {
                Visit::Stmt(x) => match &**x {
                    StmtP::Assign(lhs, ty_rhs) => {
                        if let Some(ty) = &ty_rhs.0 {
                            let ty2 = Ty::from_expr(ty, &mut bindings.approximations);
                            bindings
                                .check_type
                                .push((ty.span, Some(&ty_rhs.1), ty2.clone()));
                            if let AssignP::Identifier(id) = &**lhs {
                                // FIXME: This could be duplicated if you declare the type of a variable twice,
                                // we would only see the second one.
                                bindings.types.insert(id.1.unwrap(), ty2);
                            }
                        }
                        assign(lhs, BindExpr::Expr(&ty_rhs.1), bindings)
                    }
                    StmtP::AssignModify(lhs, op, rhs) => {
                        assign(lhs, BindExpr::AssignOp(lhs, *op, rhs), bindings)
                    }
                    StmtP::For(lhs, iter_body) => assign(
                        lhs,
                        BindExpr::Iter(Box::new(BindExpr::Expr(&iter_body.0))),
                        bindings,
                    ),
                    StmtP::Def(DefP {
                        name,
                        params,
                        return_type,
                        ..
                    }) => {
                        bindings.descriptions.insert(name.1.unwrap(), name);
                        let mut params2 = Vec::with_capacity(params.len());
                        let mut seen_no_args = false;
                        for p in params {
                            let name_ty = match &**p {
                                ParameterP::Normal(name, ty)
                                | ParameterP::WithDefaultValue(name, ty, _) => {
                                    let ty = Ty::from_expr_opt(ty, &mut bindings.approximations);
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
                                    params2.push(Param::args(Ty::from_expr_opt(
                                        ty,
                                        &mut bindings.approximations,
                                    )));
                                    Some((name, Ty::name("tuple")))
                                }
                                ParameterP::KwArgs(name, ty) => {
                                    let ty = Ty::from_expr_opt(ty, &mut bindings.approximations);
                                    let ty = if ty.is_any() {
                                        Ty::dict(Ty::Any, Ty::Any)
                                    } else {
                                        ty
                                    };
                                    params2.push(Param::kwargs(ty.clone()));
                                    Some((name, ty))
                                }
                            };
                            if let Some((name, ty)) = name_ty {
                                bindings.types.insert(name.1.unwrap(), ty);
                                bindings.descriptions.insert(name.1.unwrap(), name);
                            }
                        }
                        let ret_ty = Ty::from_expr_opt(return_type, &mut bindings.approximations);
                        bindings
                            .types
                            .insert(name.1.unwrap(), Ty::function(params2, ret_ty.clone()));
                        x.visit_children(|x| visit(x, &ret_ty, loads, bindings));
                        // We do our own visit_children, with a different return type
                        return;
                    }
                    StmtP::Load(x) => {
                        let none = Interface::empty();
                        let mp = loads.get(x.module.as_str()).unwrap_or(&none);
                        for (ident, _load) in &x.args {
                            let ty = mp.get(ident.0.as_str()).cloned().unwrap_or(Ty::Any);
                            bindings.descriptions.insert(ident.1.unwrap(), ident);
                            bindings.types.insert(ident.1.unwrap(), ty);
                        }
                    }
                    StmtP::Return(ret) => {
                        bindings
                            .check_type
                            .push((x.span, ret.as_ref(), return_type.clone()))
                    }
                    StmtP::Expression(x) => {
                        // We want to find ident.append(), ident.extend(), ident.extend()
                        // to fake up a BindExpr::ListAppend/ListExtend
                        // so that mutating list operations aren't invisible to us
                        if let ExprP::Call(fun, args) = &**x {
                            if let ExprP::Dot(id, attr) = &***fun {
                                if let ExprP::Identifier(_, id) = &***id {
                                    let res = match attr.as_str() {
                                        "append" if args.len() == 1 => Some((false, 0)),
                                        "insert" if args.len() == 2 => Some((false, 1)),
                                        "extend" if args.len() == 1 => Some((true, 0)),
                                        _ => None,
                                    };
                                    if let Some((extend, arg)) = res {
                                        if let ResolvedIdent::Slot((_, id)) = id.as_ref().unwrap() {
                                            let bind = if extend {
                                                BindExpr::ListExtend(*id, args[arg].expr())
                                            } else {
                                                BindExpr::ListAppend(*id, args[arg].expr())
                                            };
                                            bindings.expressions.entry(*id).or_default().push(bind)
                                        }
                                    }
                                }
                            }
                        }

                        bindings.check.push(x)
                    }
                    StmtP::If(x, _) => bindings.check.push(x),
                    StmtP::IfElse(x, _) => bindings.check.push(x),
                    _ => {}
                },
                Visit::Expr(x) => match &**x {
                    ExprP::ListComprehension(_, for1, clauses)
                    | ExprP::DictComprehension(_, for1, clauses) => {
                        fn get_for_clause(
                            x: &ClauseP<CstPayload>,
                        ) -> Option<&ForClauseP<CstPayload>> {
                            match x {
                                ClauseP::For(x) => Some(x),
                                _ => None,
                            }
                        }
                        for x in std::iter::once(&**for1)
                            .chain(clauses.iter().filter_map(get_for_clause))
                        {
                            assign(
                                &x.var,
                                BindExpr::Iter(Box::new(BindExpr::Expr(&x.over))),
                                bindings,
                            )
                        }
                    }
                    _ => {}
                },
            }
            x.visit_children(|x| visit(x, return_type, loads, bindings))
        }

        let mut res = Bindings::default();
        visit(Visit::Stmt(x), &Ty::Any, loads, &mut res);
        res
    }
}
