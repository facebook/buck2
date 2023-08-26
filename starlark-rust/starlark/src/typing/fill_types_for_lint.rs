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

use std::fmt::Display;
use std::iter;

use starlark_map::unordered_map;
use starlark_map::unordered_map::UnorderedMap;

use crate::codemap::Span;
use crate::codemap::Spanned;
use crate::environment::slots::ModuleSlotId;
use crate::eval::compiler::constants::Constants;
use crate::eval::compiler::scope::payload::CstArgument;
use crate::eval::compiler::scope::payload::CstAssignIdent;
use crate::eval::compiler::scope::payload::CstExpr;
use crate::eval::compiler::scope::payload::CstIdent;
use crate::eval::compiler::scope::payload::CstPayload;
use crate::eval::compiler::scope::payload::CstStmt;
use crate::eval::compiler::scope::payload::CstTypeExpr;
use crate::eval::compiler::scope::ModuleScopeData;
use crate::eval::compiler::scope::ResolvedIdent;
use crate::eval::compiler::scope::Slot;
use crate::slice_vec_ext::SliceExt;
use crate::syntax::ast::AssignP;
use crate::syntax::ast::AssignTargetP;
use crate::syntax::ast::AstLiteral;
use crate::syntax::ast::AstString;
use crate::syntax::ast::BinOp;
use crate::syntax::ast::ExprP;
use crate::syntax::ast::ForP;
use crate::syntax::ast::LoadP;
use crate::syntax::ast::StmtP;
use crate::syntax::type_expr::TypeExprUnpackP;
use crate::typing::error::InternalError;
use crate::typing::error::TypingError;
use crate::typing::Approximation;
use crate::typing::Ty;
use crate::typing::TypingOracleCtx;
use crate::values::tuple::AllocTuple;
use crate::values::typing::type_compiled::compiled::TypeCompiled;
use crate::values::Heap;
use crate::values::Value;

/// Value computed during partial evaluation of globals.
#[derive(Clone)]
struct GlobalValue<'v> {
    /// `None` means we don't know (or know it may have different value depending on condition).
    value: Option<Value<'v>>,
}

impl<'v> GlobalValue<'v> {
    fn union2(_a: GlobalValue<'v>, _b: GlobalValue<'v>) -> GlobalValue<'v> {
        // A variable was potentially assigned more than once, e.g.
        // ```
        // if cond:
        //   x = "A"
        // else:
        //   x = "B"
        // ```
        // So here we say that we don't know what is the value.
        // We could check if both values are equal, use that equal value,
        // but values referenced by the types should not be defined conditionally.
        GlobalValue { value: None }
    }
}

impl<'v> GlobalValue<'v> {
    fn value(value: Value<'v>) -> GlobalValue<'v> {
        GlobalValue { value: Some(value) }
    }

    fn any() -> GlobalValue<'v> {
        GlobalValue { value: None }
    }
}

struct GlobalTypesBuilder<'a, 'v> {
    approximations: &'a mut Vec<Approximation>,
    heap: &'v Heap,
    values: UnorderedMap<ModuleSlotId, GlobalValue<'v>>,
    errors: Vec<TypingError>,
    module_scope_data: &'a ModuleScopeData<'a>,
    ctx: TypingOracleCtx<'a>,
}

impl<'a, 'v> GlobalTypesBuilder<'a, 'v> {
    fn internal_error(&self, span: Span, message: impl Display) -> InternalError {
        InternalError::msg(message, span, self.ctx.codemap)
    }

    fn err(&mut self, span: Span, e: anyhow::Error) -> GlobalValue<'v> {
        self.errors
            .push(TypingError::new(e, span, self.ctx.codemap));
        GlobalValue::any()
    }

    fn call(
        &mut self,
        _f: &CstExpr,
        _args: &[CstArgument],
    ) -> Result<GlobalValue<'v>, InternalError> {
        // TODO(nga): could be a call like `record(...)`, and we need to evaluate it.
        Ok(GlobalValue::any())
    }

    fn expr_ident(&self, ident: &CstIdent) -> Result<GlobalValue<'v>, InternalError> {
        let Some(resolved_ident) = &ident.1 else {
            return Err(self.internal_error(ident.span, "unresolved ident"));
        };
        match resolved_ident {
            ResolvedIdent::Slot(Slot::Module(module_slot_id), _) => {
                match self.values.get(module_slot_id) {
                    None => Ok(GlobalValue::any()),
                    Some(v) => Ok(v.clone()),
                }
            }
            ResolvedIdent::Slot(Slot::Local(_), _) => {
                Err(self.internal_error(ident.span, "local slot in global scope"))
            }
            ResolvedIdent::Global(g) => Ok(GlobalValue::value(g.to_value())),
        }
    }

    fn expr_literal(&mut self, literal: &AstLiteral) -> Result<GlobalValue<'v>, InternalError> {
        match literal {
            AstLiteral::String(s) => Ok(GlobalValue::value(self.heap.alloc(s.node.as_str()))),
            // Not used in type expressions.
            _ => Ok(GlobalValue::any()),
        }
    }

    fn tuple(&mut self, xs: &[CstExpr]) -> Result<GlobalValue<'v>, InternalError> {
        let xs = xs.try_map(|x| self.expr_spanned(x))?;
        if let Ok(xs) = xs.try_map(|v| v.value.ok_or(())) {
            Ok(GlobalValue::value(self.heap.alloc(AllocTuple(xs))))
        } else {
            Ok(GlobalValue::any())
        }
    }

    fn dot(
        &mut self,
        span: Span,
        object: &CstExpr,
        field: &AstString,
    ) -> Result<GlobalValue<'v>, InternalError> {
        let object = self.expr(object)?;
        if let Some(object) = object.value {
            match object.get_attr_error(field.as_str(), self.heap) {
                Ok(v) => Ok(GlobalValue::value(v)),
                Err(e) => Ok(self.err(span, e)),
            }
        } else {
            Ok(GlobalValue::any())
        }
    }

    fn index(
        &mut self,
        span: Span,
        array: &CstExpr,
        index: &CstExpr,
    ) -> Result<GlobalValue<'v>, InternalError> {
        let array = self.expr(array)?;
        let index = self.expr_spanned(index)?;
        if let (Some(array), Some(index)) = (array.value, index.node.value) {
            match array.at(index, self.heap) {
                Ok(value) => Ok(GlobalValue::value(value)),
                Err(e) => Ok(self.err(span, e)),
            }
        } else {
            Ok(GlobalValue::any())
        }
    }

    fn index2(
        &mut self,
        span: Span,
        array: &CstExpr,
        index0: &CstExpr,
        index1: &CstExpr,
    ) -> Result<GlobalValue<'v>, InternalError> {
        let array = self.expr(array)?;
        let index0 = self.expr(index0)?;
        let index1 = self.expr(index1)?;
        if let (Some(array), Some(index0), Some(index1)) = (array.value, index0.value, index1.value)
        {
            match array.get_ref().at2(index0, index1, self.heap) {
                Ok(value) => Ok(GlobalValue::value(value)),
                Err(e) => Ok(self.err(span, e)),
            }
        } else {
            Ok(GlobalValue::any())
        }
    }

    fn bin_op(
        &mut self,
        span: Span,
        lhs: &CstExpr,
        op: BinOp,
        rhs: &CstExpr,
    ) -> Result<GlobalValue<'v>, InternalError> {
        let lhs = self.expr(lhs)?;
        let rhs = self.expr(rhs)?;
        if let (Some(lhs), BinOp::BitOr, Some(rhs)) = (lhs.value, op, rhs.value) {
            match lhs.bit_or(rhs, self.heap) {
                Ok(value) => Ok(GlobalValue::value(value)),
                Err(e) => Ok(self.err(span, e)),
            }
        } else {
            Ok(GlobalValue::any())
        }
    }

    fn expr(&mut self, expr: &CstExpr) -> Result<GlobalValue<'v>, InternalError> {
        let span = expr.span;
        match &expr.node {
            ExprP::Tuple(xs) => self.tuple(xs),
            ExprP::Dot(object, field) => self.dot(span, object, field),
            ExprP::Call(f, args) => self.call(f, args),
            ExprP::Index(a_i) => {
                let (a, i) = &**a_i;
                self.index(span, a, i)
            }
            ExprP::Index2(a_i0_i1) => {
                let (a, i0, i1) = &**a_i0_i1;
                self.index2(span, a, i0, i1)
            }
            ExprP::Identifier(ident) => self.expr_ident(ident),
            ExprP::Literal(lit) => self.expr_literal(lit),
            ExprP::Op(lhs, op, rhs) => self.bin_op(span, lhs, *op, rhs),
            // These are not used in type expressions.
            ExprP::Slice(..)
            | ExprP::Lambda(_)
            | ExprP::Not(..)
            | ExprP::Minus(..)
            | ExprP::Plus(..)
            | ExprP::BitNot(..)
            | ExprP::If(..)
            | ExprP::List(_)
            | ExprP::Dict(_)
            | ExprP::ListComprehension(_, _, _)
            | ExprP::DictComprehension(_, _, _)
            | ExprP::FString(_) => Ok(GlobalValue::any()),
        }
    }

    fn expr_spanned(&mut self, expr: &CstExpr) -> Result<Spanned<GlobalValue<'v>>, InternalError> {
        let value = self.expr(expr)?;
        Ok(Spanned {
            span: expr.span,
            node: value,
        })
    }

    fn load(&mut self, load: &LoadP<CstPayload>) -> Result<(), InternalError> {
        for (var, _source) in &load.args {
            self.assign_ident_value(var, GlobalValue::any())?;
        }
        Ok(())
    }

    fn resolve_assign_ident_to_module_slot_id(
        &self,
        ident: &CstAssignIdent,
    ) -> Result<ModuleSlotId, InternalError> {
        let binding_id = ident.resolved_binding_id(self.ctx.codemap)?;
        let binding = self.module_scope_data.get_binding(binding_id);
        let resolved_slot = binding.resolved_slot(self.ctx.codemap)?;
        match resolved_slot {
            Slot::Module(slot_id) => Ok(slot_id),
            Slot::Local(_) => Err(self.internal_error(ident.span, "local slot")),
        }
    }

    fn assign_ident_value(
        &mut self,
        ident: &CstAssignIdent,
        value: GlobalValue<'v>,
    ) -> Result<(), InternalError> {
        let module_slot_id = self.resolve_assign_ident_to_module_slot_id(ident)?;
        match self.values.entry(module_slot_id) {
            unordered_map::Entry::Occupied(mut e) => {
                let value = GlobalValue::union2(value, e.get().clone());
                e.insert(value);
            }
            unordered_map::Entry::Vacant(e) => {
                e.insert(value);
            }
        }
        Ok(())
    }

    fn assign_unset_ident(&mut self, target: &CstAssignIdent) -> Result<(), InternalError> {
        let module_slot_id = self.resolve_assign_ident_to_module_slot_id(target)?;
        self.values.insert(module_slot_id, GlobalValue::any());
        Ok(())
    }

    fn assign_value(
        &mut self,
        lhs: &AssignTargetP<CstPayload>,
        rhs: GlobalValue<'v>,
    ) -> Result<(), InternalError> {
        match lhs {
            AssignTargetP::Tuple(xs) => {
                // TODO(nga): do better if RHS is a tuple.
                for x in xs {
                    self.assign_unset(x)?;
                }
                Ok(())
            }
            AssignTargetP::Index(_) => Ok(()),
            AssignTargetP::Dot(_, _) => Ok(()),
            AssignTargetP::Identifier(ident) => self.assign_ident_value(ident, rhs),
        }
    }

    fn assign(
        &mut self,
        lhs: &AssignTargetP<CstPayload>,
        rhs: &CstExpr,
    ) -> Result<(), InternalError> {
        let rhs = self.expr(rhs)?;
        self.assign_value(lhs, rhs)
    }

    /// Unset the variables.
    ///
    /// When evaluating code like:
    ///
    /// ```python
    /// if x:
    ///   a = list
    /// else:
    ///   b = int
    /// ```
    ///
    /// We don't know what branch is taken. So we just unset both `a` and `b`.
    fn assign_unset(&mut self, lhs: &AssignTargetP<CstPayload>) -> Result<(), InternalError> {
        match lhs {
            AssignTargetP::Tuple(xs) => {
                for x in xs {
                    self.assign_unset(x)?;
                }
                Ok(())
            }
            AssignTargetP::Index(..) => Ok(()),
            AssignTargetP::Dot(..) => Ok(()),
            AssignTargetP::Identifier(ident) => self.assign_unset_ident(ident),
        }
    }

    fn assign_stmt(&mut self, assign: &AssignP<CstPayload>) -> Result<(), InternalError> {
        let AssignP { lhs, ty, rhs } = assign;
        match ty {
            None => self.assign(lhs, rhs),
            Some(_ty) => {
                // TODO(nga): do not ignore type.
                self.assign(lhs, rhs)
            }
        }
    }

    fn for_stmt_unset(&mut self, for_stmt: &ForP<CstPayload>) -> Result<(), InternalError> {
        let ForP { var, over: _, body } = for_stmt;
        self.assign_unset(var)?;
        self.eval_stmt_unset(body)
    }

    /// When we are not sure if code is executed exactly once (like in a for loop body),
    /// we just reset all the variables.
    fn eval_stmt_unset(&mut self, stmt: &CstStmt) -> Result<(), InternalError> {
        match &stmt.node {
            StmtP::Break => Ok(()),
            StmtP::Continue => Ok(()),
            StmtP::Pass => Ok(()),
            StmtP::Return(_) => return Err(self.internal_error(stmt.span, "return")),
            StmtP::Expression(_) => Ok(()),
            StmtP::Assign(AssignP { lhs, .. }) => self.assign_unset(lhs),
            StmtP::AssignModify(lhs, _, _) => self.assign_unset(lhs),
            StmtP::Statements(xs) => {
                for x in xs {
                    self.eval_stmt_unset(x)?;
                }
                Ok(())
            }
            StmtP::If(_cond, then_block) => self.eval_stmt_unset(then_block),
            StmtP::IfElse(_cond, then_else) => {
                let (then_block, else_block) = &**then_else;
                self.eval_stmt_unset(then_block)?;
                self.eval_stmt_unset(else_block)?;
                Ok(())
            }
            StmtP::For(for_stmt) => self.for_stmt_unset(for_stmt),
            StmtP::Def(def) => self.assign_unset_ident(&def.name),
            StmtP::Load(_) => Err(self.internal_error(stmt.span, "load")),
        }
    }

    fn eval_stmt(&mut self, stmt: &CstStmt) -> Result<(), InternalError> {
        let span = stmt.span;
        match &stmt.node {
            StmtP::Break => Err(self.internal_error(span, "top-level break")),
            StmtP::Continue => Err(self.internal_error(span, "top-level continue")),
            StmtP::Pass => Ok(()),
            StmtP::Return(_) => Err(self.internal_error(span, "top-level return")),
            StmtP::Expression(_) => Ok(()),
            StmtP::Assign(assign) => self.assign_stmt(assign),
            StmtP::AssignModify(..) => Ok(()),
            StmtP::Statements(_) => {
                Err(self.internal_error(span, "statements in top-level statement"))
            }
            StmtP::If(_cond, th) => self.eval_stmt_unset(th),
            StmtP::IfElse(_cond, th_el) => {
                let (th, el) = &**th_el;
                self.eval_stmt_unset(th)?;
                self.eval_stmt_unset(el)?;
                Ok(())
            }
            StmtP::For(for_stmt) => self.for_stmt_unset(for_stmt),
            StmtP::Def(def) => {
                // Def cannot be used in types.
                self.assign_unset_ident(&def.name)
            }
            StmtP::Load(load) => self.load(load),
        }
    }

    fn unknown_ty(&mut self, span: Span) -> Ty {
        self.approximations
            .push(Approximation::new("Unknown type", span));
        Ty::any()
    }

    fn try_proper_ty(
        &mut self,
        first: &CstIdent,
        rem: &[Spanned<&str>],
    ) -> Result<Option<Ty>, InternalError> {
        let Some(mut value) = self.expr_ident(first)?.value else {
            return Ok(None);
        };
        for x in rem {
            match value.get_attr_error(x, self.heap) {
                Ok(v) => value = v,
                Err(e) => {
                    let span = first.span.merge(x.span);
                    self.errors
                        .push(TypingError::new(e, span, self.ctx.codemap));
                    return Ok(Some(Ty::any()));
                }
            }
        }
        match TypeCompiled::new(value, self.heap) {
            Ok(ty) => Ok(Some(ty.as_ty().clone())),
            Err(e) => {
                let span =
                    Span::merge_all(iter::once(first.span).chain(rem.iter().map(|x| x.span)));
                self.errors
                    .push(TypingError::new(e, span, self.ctx.codemap));
                Ok(Some(Ty::any()))
            }
        }
    }

    fn path_ty(&mut self, first: &CstIdent, rem: &[Spanned<&str>]) -> Result<Ty, InternalError> {
        if let Some(ty) = self.try_proper_ty(first, rem)? {
            return Ok(ty);
        }

        let span = Span::merge_all(iter::once(first.span).chain(rem.iter().map(|x| x.span)));
        Ok(self.unknown_ty(span))
    }

    fn from_type_expr_impl(
        &mut self,
        x: &Spanned<TypeExprUnpackP<CstPayload>>,
    ) -> Result<Ty, InternalError> {
        match &x.node {
            TypeExprUnpackP::Tuple(xs) => {
                Ok(Ty::tuple(xs.try_map(|x| self.from_type_expr_impl(x))?))
            }
            TypeExprUnpackP::Union(xs) => {
                Ok(Ty::unions(xs.try_map(|x| self.from_type_expr_impl(x))?))
            }
            TypeExprUnpackP::Literal(x) => {
                if x.is_empty() || x.starts_with('_') {
                    Ok(Ty::any())
                } else {
                    Ok(Ty::name(x))
                }
            }
            TypeExprUnpackP::Path(first, rem) => self.path_ty(first, rem),
            TypeExprUnpackP::Index(a, i) => {
                if let Some(a) = self.expr_ident(a)?.value {
                    if !a.ptr_eq(Constants::get().fn_list.0.to_value()) {
                        self.approximations.push(Approximation::new("Not list", x));
                        return Ok(Ty::any());
                    }
                    let i = self.from_type_expr_impl(i)?;
                    let i = TypeCompiled::from_ty(&i, self.heap);
                    match a.get_ref().at(i.to_inner(), self.heap) {
                        Ok(t) => match TypeCompiled::new(t, self.heap) {
                            Ok(ty) => Ok(ty.as_ty().clone()),
                            Err(_) => {
                                // TODO(nga): proper error, not approximation.
                                self.approximations
                                    .push(Approximation::new("TypeCompiled::new failed", x));
                                Ok(Ty::any())
                            }
                        },
                        Err(e) => {
                            self.approximations
                                .push(Approximation::new("Getitem failed", e));
                            Ok(Ty::any())
                        }
                    }
                } else {
                    self.approximations
                        .push(Approximation::new("Not global", x));
                    Ok(Ty::any())
                }
            }
            TypeExprUnpackP::Index2(a, i0, i1) => {
                if let Some(a) = self.expr_ident(a)?.value {
                    if !a.ptr_eq(Constants::get().fn_dict.0.to_value()) {
                        self.approximations.push(Approximation::new("Not dict", x));
                        return Ok(Ty::any());
                    }
                    let i0 = self.from_type_expr_impl(i0)?;
                    let i1 = self.from_type_expr_impl(i1)?;
                    let i0 = TypeCompiled::from_ty(&i0, self.heap);
                    let i1 = TypeCompiled::from_ty(&i1, self.heap);
                    match a.get_ref().at2(i0.to_inner(), i1.to_inner(), self.heap) {
                        Ok(t) => match TypeCompiled::new(t, self.heap) {
                            Ok(ty) => Ok(ty.as_ty().clone()),
                            Err(_) => {
                                self.approximations
                                    .push(Approximation::new("TypeCompiled::new failed", x));
                                Ok(Ty::any())
                            }
                        },
                        Err(e) => {
                            self.approximations
                                .push(Approximation::new("Getitem2 failed", e));
                            Ok(Ty::any())
                        }
                    }
                } else {
                    self.approximations
                        .push(Approximation::new("Not global", x));
                    Ok(Ty::any())
                }
            }
        }
    }

    fn ty_expr(&mut self, expr: &CstTypeExpr) -> Result<Ty, InternalError> {
        let x = TypeExprUnpackP::unpack(&expr.expr, self.ctx.codemap)
            .map_err(InternalError::from_eval_exception)?;
        self.from_type_expr_impl(&x)
    }

    fn fill_types(&mut self, stmt: &mut CstStmt) -> Result<(), InternalError> {
        stmt.visit_type_expr_err_mut(&mut |type_expr| {
            if type_expr.payload.typechecker_ty.is_some() {
                return Err(self.internal_error(type_expr.span, "type already set"));
            }
            type_expr.payload.typechecker_ty = Some(self.ty_expr(type_expr)?);
            Ok(())
        })
    }

    fn top_level_stmt(&mut self, stmt: &mut CstStmt) -> Result<(), InternalError> {
        // Fill all type payloads.
        self.fill_types(stmt)?;
        // Partially evaluate expressions which can be used in the following type expressions.
        self.eval_stmt(stmt)
    }
}

/// Populate `TypeExprP` type payload when running lint typechecker.
/// (Compiler typechecked populates the payload after proper full evaluation.)
pub(crate) fn fill_types_for_lint_typechecker(
    module: &mut [&mut CstStmt],
    ctx: TypingOracleCtx,
    module_scope_data: &ModuleScopeData,
    approximations: &mut Vec<Approximation>,
) -> Result<Vec<TypingError>, InternalError> {
    let heap = Heap::new();
    let mut builder = GlobalTypesBuilder {
        heap: &heap,
        ctx,
        values: UnorderedMap::new(),
        errors: Vec::new(),
        module_scope_data,
        approximations,
    };
    for stmt in module.iter_mut() {
        builder.top_level_stmt(stmt)?;
    }
    let GlobalTypesBuilder { errors, .. } = builder;
    Ok(errors)
}
