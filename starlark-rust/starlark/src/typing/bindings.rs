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

use dupe::Dupe;
use starlark_map::small_map::SmallMap;
use starlark_syntax::syntax::ast::AssignOp;
use starlark_syntax::syntax::ast::AssignP;
use starlark_syntax::syntax::ast::AssignTargetP;
use starlark_syntax::syntax::ast::ClauseP;
use starlark_syntax::syntax::ast::DefP;
use starlark_syntax::syntax::ast::ExprP;
use starlark_syntax::syntax::ast::ForClauseP;
use starlark_syntax::syntax::ast::ForP;
use starlark_syntax::syntax::ast::IdentP;
use starlark_syntax::syntax::ast::StmtP;
use starlark_syntax::syntax::def::DefParamKind;
use starlark_syntax::syntax::def::DefParams;
use starlark_syntax::syntax::def::DefRegularParamMode;
use starlark_syntax::syntax::uniplate::Visit;

use crate::codemap::CodeMap;
use crate::codemap::Span;
use crate::codemap::Spanned;
use crate::eval::compiler::scope::BindingId;
use crate::eval::compiler::scope::ResolvedIdent;
use crate::eval::compiler::scope::payload::CstAssignIdent;
use crate::eval::compiler::scope::payload::CstAssignIdentExt;
use crate::eval::compiler::scope::payload::CstAssignTarget;
use crate::eval::compiler::scope::payload::CstExpr;
use crate::eval::compiler::scope::payload::CstPayload;
use crate::eval::compiler::scope::payload::CstStmt;
use crate::eval::compiler::scope::payload::CstTypeExpr;
use crate::typing::ParamSpec;
use crate::typing::TyBasic;
use crate::typing::arc_ty::ArcTy;
use crate::typing::callable_param::ParamIsRequired;
use crate::typing::error::InternalError;
use crate::typing::error::TypingError;
use crate::typing::mode::TypecheckMode;
use crate::typing::tuple::TyTuple;
use crate::typing::ty::Approximation;
use crate::typing::ty::Ty;
use crate::util::arc_str::ArcStr;

#[derive(Clone)]
pub(crate) enum BindExpr<'a> {
    Expr(&'a CstExpr),
    /// Get this position from the expression
    GetIndex(usize, Box<BindExpr<'a>>),
    Iter(Box<BindExpr<'a>>),
    AssignModify(&'a CstAssignTarget, AssignOp, &'a CstExpr),
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

    /// Includes double-binding errors, like `x: str = ...; x: str = ...`.
    /// These break compiler typechecker, but LSP continues past them.
    pub(crate) errors: Vec<TypingError>,
}

/// Basically a child `def`.
pub(crate) struct ChildDef<'a> {
    pub(crate) body: &'a CstStmt,
    pub(crate) return_type: Ty,
    pub(crate) param_types: HashMap<BindingId, Ty>,
}

pub(crate) struct BindingsCollect<'a, 'b> {
    pub(crate) bindings: Bindings<'a>,
    pub(crate) approximations: &'b mut Vec<Approximation>,
    pub(crate) children_sink: &'b mut Vec<ChildDef<'a>>,
}

impl<'a, 'b> BindingsCollect<'a, 'b> {
    /// Collect all the assignments to variables in a scope.
    ///
    /// This function only fails on internal errors.
    pub(crate) fn collect_scope(
        scope: &'a CstStmt,
        return_type: &Ty,
        visible: &HashMap<BindingId, Ty>,
        typecheck_mode: TypecheckMode,
        codemap: &CodeMap,
        approximations: &'b mut Vec<Approximation>,
        children_sink: &'b mut Vec<ChildDef<'a>>,
    ) -> Result<Bindings<'a>, InternalError> {
        let mut res = BindingsCollect {
            bindings: Bindings::default(),
            approximations,
            children_sink,
        };
        res.bindings.types = visible.clone();

        res.visit(Visit::Stmt(scope), return_type, typecheck_mode, codemap)?;
        Ok(res.bindings)
    }

    fn assign(
        &mut self,
        lhs: &'a CstAssignTarget,
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
                    node:
                        IdentP {
                            ident: _name,
                            payload: Some(ResolvedIdent::Slot(_, ident)),
                        },
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

    /// Type must be populated earlier.
    fn resolved_ty(
        expr: &CstTypeExpr,
        typecheck_mode: TypecheckMode,
        codemap: &CodeMap,
    ) -> Result<Ty, InternalError> {
        let ty = match typecheck_mode {
            TypecheckMode::Lint => expr.payload.typechecker_ty.clone(),
            TypecheckMode::Compiler => expr.payload.compiler_ty.clone(),
        };
        match ty {
            Some(ty) => Ok(ty),
            None => Err(InternalError::msg(
                "Type must be populated earlier",
                expr.span,
                codemap,
            )),
        }
    }

    fn resolve_ty_opt(
        expr: Option<&CstTypeExpr>,
        typecheck_mode: TypecheckMode,
        codemap: &CodeMap,
    ) -> Result<Ty, InternalError> {
        match expr {
            Some(expr) => Self::resolved_ty(expr, typecheck_mode, codemap),
            None => Ok(Ty::any()),
        }
    }

    /// Check declare-once and insert an explicit type for the binding.
    fn type_annotation(
        &mut self,
        binding: &CstAssignIdent,
        ty: Ty,
        error_span: Span,
        codemap: &CodeMap,
    ) -> Result<(), InternalError> {
        let resolved_id = binding.resolved_binding_id(codemap)?;
        // This is always an error for the compiler, but LSP might continue typechecking in a
        // degraded state.
        //
        // Two cases:
        // 1. `x = ...; x: str = ...`. We don't add the annotation. Binding goes through the solver.
        // 2. `x: str = ...; x: int = ...`, even if they're the same type. Keep the original for
        //    degraded typecheck.
        //
        let entry = self.bindings.types.entry(resolved_id);
        // Previous un-annotated assignment
        if self.bindings.expressions.contains_key(&resolved_id)
			// Previous typed assignment
            || matches!(entry, std::collections::hash_map::Entry::Occupied(_))
        {
            self.bindings.errors.push(TypingError::new(
                crate::Error::new_kind(starlark_syntax::ErrorKind::Other(anyhow::anyhow!(
                    "Second declaration of binding `{}`",
                    binding.node.ident,
                ))),
                error_span,
                codemap,
            ));
            return Ok(());
        }
        entry.or_insert(ty);
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
        let DefParams { params, indices: _ } =
            DefParams::unpack(params, codemap).map_err(InternalError::from_eval_exception)?;

        let mut pos_only = Vec::new();
        let mut pos_or_named = Vec::new();
        let mut args = None;
        let mut named_only = Vec::new();
        let mut kwargs = None;
        let mut param_types = HashMap::new();

        for p in params {
            let name = &p.node.ident;
            let ty = p.node.ty;
            let ty = Self::resolve_ty_opt(ty, typecheck_mode, codemap)?;
            let ty = match &p.node.kind {
                DefParamKind::Regular(mode, default_value) => {
                    let required = match default_value.is_some() {
                        true => ParamIsRequired::No,
                        false => ParamIsRequired::Yes,
                    };
                    match mode {
                        DefRegularParamMode::PosOnly => {
                            pos_only.push((required, ty.dupe()));
                        }
                        DefRegularParamMode::PosOrName => {
                            pos_or_named.push((
                                ArcStr::from(name.ident.as_str()),
                                required,
                                ty.dupe(),
                            ));
                        }
                        DefRegularParamMode::NameOnly => {
                            named_only.push((
                                ArcStr::from(name.ident.as_str()),
                                required,
                                ty.dupe(),
                            ));
                        }
                    }
                    ty
                }
                DefParamKind::Args => {
                    // There is the type we require people calling us use (usually any)
                    // and then separately the type we are when we are running (always tuple)
                    args = Some(ty.dupe());
                    Ty::basic(TyBasic::Tuple(TyTuple::Of(ArcTy::new(ty))))
                }
                DefParamKind::Kwargs => {
                    let var_ty = Ty::dict(Ty::string(), ty.clone());
                    kwargs = Some(ty.dupe());
                    var_ty
                }
            };
            param_types.insert(name.resolved_binding_id(codemap)?, ty);
        }
        let params2 = ParamSpec::new_parts(pos_only, pos_or_named, args, named_only, kwargs)
            .map_err(|e| InternalError::from_error(e, def.signature_span(), codemap))?;
        let ret_ty = Self::resolve_ty_opt(return_type.as_deref(), typecheck_mode, codemap)?;

        // Defining a function is like an annotated assignment
        //
        //     foo: Function[...] = lambda x, y: ...
        //
        // (even if there are no type annotations, because even just having parameters
        // is a kind of type annotation).
        //
        self.type_annotation(
            name,
            Ty::function(params2, ret_ty.clone()),
            name.span,
            codemap,
        )?;

        def.visit_header_err(|x| self.visit(x, &ret_ty, typecheck_mode, codemap))?;

        // Function body just gets added to the queue.
        self.children_sink.push(ChildDef {
            body: &def.body,
            param_types,
            return_type: ret_ty,
        });
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
                        let ty2 = Self::resolved_ty(ty, typecheck_mode, codemap)?;
                        self.bindings
                            .check_type
                            .push((ty.span, Some(rhs), ty2.clone()));
                        if let AssignTargetP::Identifier(id) = &**lhs {
                            self.type_annotation(id, ty2, lhs.span.merge(ty.span), codemap)?;
                        } else {
                            // We should have caught this when parsing.
                            return Err(InternalError::from_error(
                                crate::Error::new_kind(starlark_syntax::ErrorKind::Other(
                                    anyhow::anyhow!(
                                        "Cannot annotate type of complex assign target",
                                    ),
                                )),
                                lhs.span.merge(ty.span),
                                codemap,
                            )
                            .into());
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
                StmtP::Load(..) => {}
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
                                    "append" if args.args.len() == 1 => Some((false, 0)),
                                    "insert" if args.args.len() == 2 => Some((false, 1)),
                                    "extend" if args.args.len() == 1 => Some((true, 0)),
                                    _ => None,
                                };
                                if let Some((extend, arg)) = res {
                                    if let ResolvedIdent::Slot(_, id) =
                                        id.node.payload.as_ref().unwrap()
                                    {
                                        let bind = if extend {
                                            BindExpr::ListExtend(*id, args.args[arg].expr())
                                        } else {
                                            BindExpr::ListAppend(*id, args.args[arg].expr())
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
