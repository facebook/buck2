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
use crate::slice_vec_ext::VecExt;
use crate::syntax::ast::ExprP;
use crate::syntax::ast::StmtP;
use crate::syntax::type_expr::TypeExprUnpackP;
use crate::syntax::uniplate::VisitMut;
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
    #[error("Type payload not set (internal error)")]
    TypePayloadNotSet,
}

impl<'v> Compiler<'v, '_, '_> {
    /// Compile expression when it is expected to be interpreted as type.
    pub(crate) fn expr_for_type(
        &mut self,
        expr: Option<Box<CstTypeExpr>>,
    ) -> Option<IrSpanned<TypeCompiled<FrozenValue>>> {
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
        if type_value.type_is_wildcard() {
            return None;
        }
        Some(IrSpanned {
            span,
            node: type_value,
        })
    }

    /// We evaluated type expression to `Value`, now convert it to `FrozenValue`.
    // TODO(nga): this step is not really necessary, we should just create `TypeCompiled` directly.
    fn alloc_value_for_type(
        &mut self,
        value: Value<'v>,
        span: Span,
    ) -> Result<TypeCompiled<Value<'v>>, EvalException> {
        let ty = TypeCompiled::new(value, self.eval.heap());
        ty.map_err(|e| EvalException::new(e, span, &self.codemap))
    }

    /// We may use non-frozen values as types, so we don't reuse `expr_ident` function
    /// which is used in normal compilation.
    fn eval_path_as_type(
        &mut self,
        first: &CstIdent,
        rem: &[Spanned<&str>],
    ) -> Result<TypeCompiled<Value<'v>>, EvalException> {
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
    ) -> Result<TypeCompiled<Value<'v>>, EvalException> {
        match expr.node {
            TypeExprUnpackP::Path(ident, rem) => self.eval_path_as_type(ident, &rem),
            TypeExprUnpackP::Any(xs) => {
                let xs = xs.into_try_map(|x| self.eval_expr_as_type(x))?;
                Ok(TypeCompiled::type_any_of(xs, self.eval.heap()))
            }
            TypeExprUnpackP::ListOf(x) => {
                let x = self.eval_expr_as_type(*x)?;
                Ok(TypeCompiled::type_list_of(x, self.eval.heap()))
            }
            TypeExprUnpackP::DictOf(k, v) => {
                let k = self.eval_expr_as_type(*k)?;
                let v = self.eval_expr_as_type(*v)?;
                Ok(TypeCompiled::type_dict_of(k, v, self.eval.heap()))
            }
            TypeExprUnpackP::Tuple(xs) => {
                let xs = xs.into_try_map(|x| self.eval_expr_as_type(x))?;
                Ok(TypeCompiled::type_tuple_of(xs, self.eval.heap()))
            }
            TypeExprUnpackP::Literal(s) => Ok(TypeCompiled::from_str(s.node, self.eval.heap())),
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
        let type_value = type_value.to_frozen(self.eval.frozen_heap());
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
