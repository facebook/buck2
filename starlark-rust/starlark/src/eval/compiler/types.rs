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

use crate::codemap::Span;
use crate::codemap::Spanned;
use crate::eval::compiler::expr::ExprCompiled;
use crate::eval::compiler::scope::CstExpr;
use crate::eval::compiler::scope::CstIdent;
use crate::eval::compiler::scope::CstParameter;
use crate::eval::compiler::scope::CstPayload;
use crate::eval::compiler::scope::CstStmt;
use crate::eval::compiler::scope::CstTypeExpr;
use crate::eval::compiler::scope::ResolvedIdent;
use crate::eval::compiler::scope::Slot;
use crate::eval::compiler::span::IrSpanned;
use crate::eval::compiler::Compiler;
use crate::eval::compiler::EvalException;
use crate::eval::runtime::frame_span::FrameSpan;
use crate::eval::runtime::frozen_file_span::FrozenFileSpan;
use crate::slice_vec_ext::SliceExt;
use crate::slice_vec_ext::VecExt;
use crate::syntax::ast::ExprP;
use crate::syntax::ast::StmtP;
use crate::syntax::type_expr::TypeExprUnpackP;
use crate::syntax::uniplate::VisitMut;
use crate::values::dict::AllocDict;
use crate::values::dict::DictRef;
use crate::values::list::AllocList;
use crate::values::list::ListRef;
use crate::values::tuple::AllocTuple;
use crate::values::tuple::TupleRef;
use crate::values::typing::TypeCompiled;
use crate::values::FrozenValue;
use crate::values::Value;

#[derive(Debug, thiserror::Error)]
enum TypesError {
    #[error("Type already initialized (internal error)")]
    TypeAlreadySet,
    #[error("Identifier is not resolved (internal error)")]
    UnresolvedIdentifier,
    #[error("Identifier is resolve as local variable (internal error)")]
    LocalIdentifier,
    #[error("Module variable is not set: `{0}`")]
    ModuleVariableNotSet(String),
    #[error("Unsupported type in type expression `{0}`: `{1}`")]
    UnsupportedType(&'static str, String),
    #[error("Type payload not set (internal error)")]
    TypePayloadNotSet,
}

impl<'v> Compiler<'v, '_, '_> {
    /// Compile expression when it is expected to be interpreted as type.
    pub(crate) fn expr_for_type(
        &mut self,
        expr: Option<Box<CstTypeExpr>>,
    ) -> Option<IrSpanned<ExprCompiled>> {
        if !self.check_types {
            return None;
        }
        let expr = expr?;
        let span = FrameSpan::new(FrozenFileSpan::new(self.codemap, expr.span));
        let Some(type_value) = expr.payload else {
            // This is unreachable. But unfortunately we do not return error here.
            // Still make an error in panic to produce nice panic message.
            panic!("{:?}", EvalException::new(
                TypesError::TypePayloadNotSet.into(),
                expr.span,
                &self.codemap));
        };
        if let Some(s) = type_value.unpack_str() {
            if TypeCompiled::is_wildcard(s) {
                return None;
            }
        }
        Some(IrSpanned {
            span,
            node: ExprCompiled::Value(type_value),
        })
    }

    /// We evaluated type expression to `Value`, now convert it to `FrozenValue`.
    // TODO(nga): this step is not really necessary, we should just create `TypeCompiled` directly.
    fn alloc_value_for_type(
        &mut self,
        value: Value<'v>,
        span: Span,
    ) -> Result<FrozenValue, EvalException> {
        if let Some(v) = value.unpack_frozen() {
            return Ok(v);
        }
        if let Some(s) = value.unpack_str() {
            return Ok(self
                .eval
                .frozen_heap()
                .alloc_str_intern(s)
                .to_frozen_value());
        }
        if let Some(t) = TupleRef::from_value(value) {
            let xs = t
                .content()
                .try_map(|x| self.alloc_value_for_type(*x, span))?;
            return Ok(self.eval.frozen_heap().alloc(AllocTuple(xs)));
        }
        if let Some(l) = ListRef::from_value(value) {
            let xs = l
                .content()
                .try_map(|x| self.alloc_value_for_type(*x, span))?;
            return Ok(self.eval.frozen_heap().alloc(AllocList(xs)));
        }
        if let Some(d) = DictRef::from_value(value) {
            let xs = d
                .iter()
                .map(|(k, v)| {
                    Ok((
                        self.alloc_value_for_type(k, span)?,
                        self.alloc_value_for_type(v, span)?,
                    ))
                })
                .collect::<Result<Vec<_>, _>>()?;
            return Ok(self.eval.frozen_heap().alloc(AllocDict(xs)));
        }
        Err(EvalException::new(
            TypesError::UnsupportedType(value.get_type(), value.to_repr()).into(),
            span,
            &self.codemap,
        ))
    }

    /// We may use non-frozen values as types, so we don't reuse `expr_ident` function
    /// which is used in normal compilation.
    fn eval_path_as_type(
        &mut self,
        first: &CstIdent,
        rem: &[Spanned<&str>],
    ) -> Result<FrozenValue, EvalException> {
        let Some(ident_payload) = &first.node.1 else {
            return Err(EvalException::new(
                TypesError::UnresolvedIdentifier.into(),
                first.span,
                &self.codemap,
            ));
        };
        let mut value = match ident_payload {
            ResolvedIdent::Slot((Slot::Local(..), _)) => {
                return Err(EvalException::new(
                    TypesError::LocalIdentifier.into(),
                    first.span,
                    &self.codemap,
                ));
            }
            ResolvedIdent::Slot((Slot::Module(module_slot_id), _)) => {
                match self.eval.module_env.slots().get_slot(*module_slot_id) {
                    Some(v) => v,
                    None => {
                        return Err(EvalException::new(
                            TypesError::ModuleVariableNotSet(first.node.0.clone()).into(),
                            first.span,
                            &self.codemap,
                        ));
                    }
                }
            }
            ResolvedIdent::Global(v) => v.to_value(),
        };
        for step in rem {
            value = value
                .get_attr_error(step.node, self.eval.heap())
                .map_err(|e| EvalException::new(e, step.span, &self.codemap))?;
        }
        let mut span = first.span;
        if let Some(last) = rem.last() {
            span = span.merge(last.span);
        }
        self.alloc_value_for_type(value, span)
    }

    fn eval_expr_as_type(
        &mut self,
        expr: Spanned<TypeExprUnpackP<CstPayload>>,
    ) -> Result<FrozenValue, EvalException> {
        match expr.node {
            TypeExprUnpackP::Path(ident, rem) => self.eval_path_as_type(ident, &rem),
            TypeExprUnpackP::Any(xs) => {
                let xs = xs.into_try_map(|x| self.eval_expr_as_type(x))?;
                Ok(self.eval.frozen_heap().alloc(AllocList(xs)))
            }
            TypeExprUnpackP::ListOf(x) => {
                let x = self.eval_expr_as_type(*x)?;
                Ok(self.eval.frozen_heap().alloc(AllocList([x])))
            }
            TypeExprUnpackP::DictOf(k, v) => {
                let k = self.eval_expr_as_type(*k)?;
                let v = self.eval_expr_as_type(*v)?;
                Ok(self.eval.frozen_heap().alloc(AllocDict([(k, v)])))
            }
            TypeExprUnpackP::Tuple(xs) => {
                let xs = xs.into_try_map(|x| self.eval_expr_as_type(x))?;
                Ok(self.eval.frozen_heap().alloc(AllocTuple(xs)))
            }
            TypeExprUnpackP::Literal(s) => Ok(self
                .eval
                .frozen_heap()
                .alloc_str_intern(s.node)
                .to_frozen_value()),
        }
    }

    fn populate_types_in_type_expr(
        &mut self,
        type_expr: &mut CstTypeExpr,
    ) -> Result<(), EvalException> {
        if type_expr.payload.is_some() {
            return Err(EvalException::new(
                TypesError::TypeAlreadySet.into(),
                type_expr.span,
                &self.codemap,
            ));
        }
        // This should not fail because we validated it at parse time.
        let unpack = TypeExprUnpackP::unpack(&type_expr.expr, &self.codemap)?;
        let type_value = self.eval_expr_as_type(unpack)?;
        type_expr.payload = Some(type_value);
        Ok(())
    }

    fn populate_types_in_params(
        &mut self,
        params: &mut [CstParameter],
    ) -> Result<(), EvalException> {
        for param in params {
            let (_, ty, _) = param.split_mut();
            if let Some(ty) = ty {
                self.populate_types_in_type_expr(ty)?;
            }
        }
        Ok(())
    }

    fn populate_types_in_expr(&mut self, expr: &mut CstExpr) -> Result<(), EvalException> {
        match &mut expr.node {
            ExprP::Lambda(lambda) => {
                self.populate_types_in_params(&mut lambda.params)?;
            }
            _ => {}
        }
        expr.visit_expr_err_mut(|expr| self.populate_types_in_expr(expr))
    }

    pub(crate) fn populate_types_in_stmt(
        &mut self,
        stmt: &mut CstStmt,
    ) -> Result<(), EvalException> {
        match &mut stmt.node {
            StmtP::Assign(_lhs, ty_def) => {
                let (ty, _) = &mut **ty_def;
                if let Some(ty) = ty {
                    self.populate_types_in_type_expr(ty)?;
                }
            }
            StmtP::Def(def) => {
                self.populate_types_in_params(&mut def.params)?;
                if let Some(ret) = &mut def.return_type {
                    self.populate_types_in_type_expr(ret)?;
                }
            }
            _ => {}
        }
        stmt.visit_children_err_mut(|visit| match visit {
            VisitMut::Stmt(stmt) => self.populate_types_in_stmt(stmt),
            VisitMut::Expr(expr) => self.populate_types_in_expr(expr),
        })
    }
}
