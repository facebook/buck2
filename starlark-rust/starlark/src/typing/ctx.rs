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

use std::cell::RefCell;
use std::fmt::Debug;

use thiserror::Error;

use crate::codemap::Span;
use crate::codemap::Spanned;
use crate::eval::compiler::scope::payload::CstArgument;
use crate::eval::compiler::scope::payload::CstAssign;
use crate::eval::compiler::scope::payload::CstExpr;
use crate::eval::compiler::scope::payload::CstIdent;
use crate::eval::compiler::scope::payload::CstPayload;
use crate::eval::compiler::scope::BindingId;
use crate::eval::compiler::scope::ResolvedIdent;
use crate::slice_vec_ext::SliceExt;
use crate::syntax::ast::ArgumentP;
use crate::syntax::ast::AssignOp;
use crate::syntax::ast::AssignTargetP;
use crate::syntax::ast::AstLiteral;
use crate::syntax::ast::BinOp;
use crate::syntax::ast::ClauseP;
use crate::syntax::ast::ExprP;
use crate::syntax::ast::ForClauseP;
use crate::typing::basic::TyBasic;
use crate::typing::bindings::BindExpr;
use crate::typing::error::InternalError;
use crate::typing::error::TypingError;
use crate::typing::error::TypingOrInternalError;
use crate::typing::function::Arg;
use crate::typing::oracle::ctx::TypingOracleCtx;
use crate::typing::oracle::traits::TypingBinOp;
use crate::typing::oracle::traits::TypingUnOp;
use crate::typing::ty::Approximation;
use crate::typing::ty::Ty;
use crate::typing::unordered_map::UnorderedMap;
use crate::typing::OracleDocs;

#[derive(Error, Debug)]
enum TypingContextError {
    #[error("The builtin `{name}` is not known")]
    UnknownBuiltin { name: String },
}

pub(crate) struct TypingContext<'a> {
    pub(crate) oracle: TypingOracleCtx<'a>,
    pub(crate) global_docs: OracleDocs,
    // We'd prefer this to be a &mut self,
    // but that makes writing the code more fiddly, so just RefCell the errors
    pub(crate) errors: RefCell<Vec<TypingError>>,
    pub(crate) approximoations: RefCell<Vec<Approximation>>,
    pub(crate) types: UnorderedMap<BindingId, Ty>,
}

impl TypingContext<'_> {
    fn add_error(&self, span: Span, err: TypingContextError) -> Ty {
        let err = self.oracle.mk_error(span, err);
        self.errors.borrow_mut().push(err);
        Ty::never()
    }

    pub(crate) fn approximation(&self, category: &'static str, message: impl Debug) -> Ty {
        self.approximoations
            .borrow_mut()
            .push(Approximation::new(category, message));
        Ty::any()
    }

    fn result_to_ty(&self, result: Result<Ty, TypingError>) -> Ty {
        match result {
            Ok(x) => x,
            Err(e) => {
                self.errors.borrow_mut().push(e);
                Ty::never()
            }
        }
    }

    fn result_to_ty_with_internal_error(
        &self,
        result: Result<Ty, TypingOrInternalError>,
    ) -> Result<Ty, InternalError> {
        match result {
            Ok(x) => Ok(x),
            Err(TypingOrInternalError::Internal(e)) => Err(e),
            Err(TypingOrInternalError::Typing(e)) => {
                self.errors.borrow_mut().push(e);
                Ok(Ty::never())
            }
        }
    }

    fn validate_call(
        &self,
        fun: &Ty,
        args: &[Spanned<Arg>],
        span: Span,
    ) -> Result<Ty, InternalError> {
        self.result_to_ty_with_internal_error(self.oracle.validate_call(span, fun, args))
    }

    fn from_iterated(&self, ty: &Ty, span: Span) -> Ty {
        self.result_to_ty(self.oracle.iter_item(Spanned { node: ty, span }))
    }

    pub(crate) fn validate_type(&self, got: &Ty, require: &Ty, span: Span) {
        if let Err(e) = self.oracle.validate_type(got, require, span) {
            self.errors.borrow_mut().push(e);
        }
    }

    fn builtin(&self, name: &str, span: Span) -> Ty {
        match self.global_docs.builtin(name) {
            Ok(x) => x,
            Err(()) => self.add_error(
                span,
                TypingContextError::UnknownBuiltin {
                    name: name.to_owned(),
                },
            ),
        }
    }

    fn expr_dot(&self, ty: &Ty, attr: &str, span: Span) -> Ty {
        self.result_to_ty(self.oracle.expr_dot(span, ty, attr))
    }

    fn expr_index(
        &self,
        span: Span,
        array: &CstExpr,
        index: &CstExpr,
    ) -> Result<Ty, InternalError> {
        let array_ty = self.expression_type(array)?;

        // Hack for `list[str]`: list of `list` is just "function", and we don't want
        // to make it custom type and have overly complex machinery for handling it.
        // So we just special case it here.
        // TODO(nga): push this code down to `OracleTypingCtx`.
        if array_ty.is_function() {
            if let ExprP::Identifier(v0) = &array.node {
                if v0.0 == "list" {
                    // TODO: make this "eval_type" or something.
                    return Ok(Ty::any());
                }
            }
        }

        let index = self.expression_type_spanned(index)?;
        self.result_to_ty_with_internal_error(self.oracle.expr_index(span, array_ty, index))
    }

    fn expression_un_op(
        &self,
        span: Span,
        arg: &CstExpr,
        un_op: TypingUnOp,
    ) -> Result<Ty, InternalError> {
        let ty = self.expression_type(arg)?;
        Ok(self.result_to_ty(self.oracle.expr_un_op(span, ty, un_op)))
    }

    pub(crate) fn expression_bind_type(&self, x: &BindExpr) -> Result<Ty, InternalError> {
        match x {
            BindExpr::Expr(x) => self.expression_type(x),
            BindExpr::GetIndex(i, x) => Ok(self.expression_bind_type(x)?.indexed(*i)),
            BindExpr::Iter(x) => Ok(self.from_iterated(&self.expression_bind_type(x)?, x.span())),
            BindExpr::AssignModify(lhs, op, rhs) => {
                let span = lhs.span;
                let rhs = self.expression_type_spanned(rhs)?;
                let lhs = self.expression_assign_spanned(lhs)?;
                let attr = match op {
                    AssignOp::Add => TypingBinOp::Add,
                    AssignOp::Subtract => TypingBinOp::Sub,
                    AssignOp::Multiply => TypingBinOp::Mul,
                    AssignOp::Divide => TypingBinOp::Div,
                    AssignOp::FloorDivide => TypingBinOp::FloorDiv,
                    AssignOp::Percent => TypingBinOp::Percent,
                    AssignOp::BitAnd => TypingBinOp::BitAnd,
                    AssignOp::BitOr => TypingBinOp::BitOr,
                    AssignOp::BitXor => TypingBinOp::BitXor,
                    AssignOp::LeftShift => TypingBinOp::LeftShift,
                    AssignOp::RightShift => TypingBinOp::RightShift,
                };
                self.result_to_ty_with_internal_error(
                    self.oracle.expr_bin_op_ty(span, lhs, attr, rhs),
                )
            }
            BindExpr::SetIndex(id, index, e) => {
                let span = index.span;
                let index = self.expression_type(index)?;
                let e = self.expression_bind_type(e)?;
                let mut res = Vec::new();
                // We know about list and dict, everything else we just ignore
                if self.types[id].is_list() {
                    // If we know it MUST be a list, then the index must be an int
                    self.validate_type(&index, &Ty::int(), span);
                }
                for ty in self.types[id].iter_union() {
                    match ty {
                        TyBasic::List(_) => {
                            res.push(Ty::list(e.clone()));
                        }
                        TyBasic::Dict(_) => {
                            res.push(Ty::dict(index.clone(), e.clone()));
                        }
                        _ => {
                            // Either it's not something we can apply this to, in which case do nothing.
                            // Or it's an Any, in which case we aren't going to change its type or spot errors.
                        }
                    }
                }
                Ok(Ty::unions(res))
            }
            BindExpr::ListAppend(id, e) => {
                if self.oracle.probably_a_list(&self.types[id]) {
                    Ok(Ty::list(self.expression_type(e)?))
                } else {
                    // It doesn't seem to be a list, so let's assume the append is non-mutating
                    Ok(Ty::never())
                }
            }
            BindExpr::ListExtend(id, e) => {
                if self.oracle.probably_a_list(&self.types[id]) {
                    Ok(Ty::list(
                        self.from_iterated(&self.expression_type(e)?, e.span),
                    ))
                } else {
                    // It doesn't seem to be a list, so let's assume the extend is non-mutating
                    Ok(Ty::never())
                }
            }
        }
    }

    /// Used to get the type of an expression when used as part of a ModifyAssign operation
    fn expression_assign(&self, x: &CstAssign) -> Result<Ty, InternalError> {
        match &**x {
            AssignTargetP::Tuple(_) => Ok(self.approximation("expression_assignment", x)),
            AssignTargetP::Index(a_b) => self.expr_index(x.span, &a_b.0, &a_b.1),
            AssignTargetP::Dot(_, _) => Ok(self.approximation("expression_assignment", x)),
            AssignTargetP::Identifier(x) => {
                if let Some(i) = x.1 {
                    if let Some(ty) = self.types.get(&i) {
                        return Ok(ty.clone());
                    }
                }
                Err(InternalError::msg(
                    "Unknown identifier",
                    x.span,
                    self.oracle.codemap,
                ))
            }
        }
    }

    fn expression_assign_spanned(&self, x: &CstAssign) -> Result<Spanned<Ty>, InternalError> {
        Ok(Spanned {
            span: x.span,
            node: self.expression_assign(x)?,
        })
    }

    /// We don't need the type out of the clauses (it doesn't change the overall type),
    /// but it is important we see through to the nested expressions to raise errors
    fn check_comprehension(
        &self,
        for_: &ForClauseP<CstPayload>,
        clauses: &[ClauseP<CstPayload>],
    ) -> Result<(), InternalError> {
        self.expression_type(&for_.over)?;
        for x in clauses {
            match x {
                ClauseP::For(x) => self.expression_type(&x.over)?,
                ClauseP::If(x) => self.expression_type(x)?,
            };
        }
        Ok(())
    }

    pub(crate) fn expression_type_spanned(
        &self,
        x: &CstExpr,
    ) -> Result<Spanned<Ty>, InternalError> {
        Ok(Spanned {
            span: x.span,
            node: self.expression_type(x)?,
        })
    }

    fn expr_bin_op(
        &self,
        span: Span,
        lhs: &CstExpr,
        op: BinOp,
        rhs: &CstExpr,
    ) -> Result<Ty, InternalError> {
        let lhs = self.expression_type_spanned(lhs)?;
        let rhs = self.expression_type_spanned(rhs)?;
        self.result_to_ty_with_internal_error(self.oracle.expr_bin_op(span, lhs, op, rhs))
    }

    fn expr_call(
        &self,
        span: Span,
        f: &CstExpr,
        args: &[CstArgument],
    ) -> Result<Ty, InternalError> {
        let args_ty: Vec<Spanned<Arg>> = args.try_map(|x| {
            Ok(Spanned {
                span: x.span,
                node: match &**x {
                    ArgumentP::Positional(x) => Arg::Pos(self.expression_type(x)?),
                    ArgumentP::Named(name, x) => {
                        Arg::Name((**name).clone(), self.expression_type(x)?)
                    }
                    ArgumentP::Args(x) => {
                        let ty = self.expression_type(x)?;
                        self.from_iterated(&ty, x.span);
                        Arg::Args(ty)
                    }
                    ArgumentP::KwArgs(x) => {
                        let ty = self.expression_type(x)?;
                        self.validate_type(&ty, &Ty::dict(Ty::string(), Ty::any()), x.span);
                        Arg::Kwargs(ty)
                    }
                },
            })
        })?;
        let f_ty = self.expression_type(f)?;
        // If we can't resolve the types of the arguments, we can't validate the call,
        // but we still know the type of the result since the args don't impact that
        self.validate_call(&f_ty, &args_ty, span)
    }

    fn expr_slice(
        &self,
        span: Span,
        x: &CstExpr,
        start: Option<&CstExpr>,
        stop: Option<&CstExpr>,
        stride: Option<&CstExpr>,
    ) -> Result<Ty, InternalError> {
        for e in [start, stop, stride].iter().copied().flatten() {
            self.validate_type(&self.expression_type(e)?, &Ty::int(), e.span);
        }
        Ok(self.result_to_ty(self.oracle.expr_slice(span, self.expression_type(x)?)))
    }

    fn expr_ident(&self, x: &CstIdent) -> Ty {
        match &x.node.1 {
            Some(ResolvedIdent::Slot(_, i)) => {
                if let Some(ty) = self.types.get(i) {
                    ty.clone()
                } else {
                    // All types must be resolved to this point,
                    // this code is unreachable.
                    Ty::any()
                }
            }
            Some(ResolvedIdent::Global(g)) => {
                if let Some(t) = g.to_value().get_ref().typechecker_ty() {
                    t
                } else {
                    self.builtin(&x.node.0, x.span)
                }
            }
            None => {
                // All identifiers must be resolved at this point,
                // but we don't stop after scope resolution error,
                // so this code is reachable.
                Ty::any()
            }
        }
    }

    pub(crate) fn expression_type(&self, x: &CstExpr) -> Result<Ty, InternalError> {
        let span = x.span;
        match &**x {
            ExprP::Tuple(xs) => Ok(Ty::tuple(xs.try_map(|x| self.expression_type(x))?)),
            ExprP::Dot(a, b) => Ok(self.expr_dot(&self.expression_type(a)?, b, b.span)),
            ExprP::Call(f, args) => self.expr_call(span, f, args),
            ExprP::Index(a_b) => self.expr_index(span, &a_b.0, &a_b.1),
            ExprP::Index2(a_i0_i1) => {
                let (a, i0, i1) = &**a_i0_i1;
                self.expression_type(a)?;
                self.expression_type(i0)?;
                self.expression_type(i1)?;
                Ok(Ty::any())
            }
            ExprP::Slice(x, start, stop, stride) => self.expr_slice(
                span,
                x,
                start.as_deref(),
                stop.as_deref(),
                stride.as_deref(),
            ),
            ExprP::Identifier(x) => Ok(self.expr_ident(x)),
            ExprP::Lambda(_) => {
                self.approximation("We don't type check lambdas", ());
                Ok(Ty::any_function())
            }
            ExprP::Literal(x) => match x {
                AstLiteral::Int(_) => Ok(Ty::int()),
                AstLiteral::Float(_) => Ok(Ty::float()),
                AstLiteral::String(_) => Ok(Ty::string()),
                AstLiteral::Ellipsis => Ok(Ty::any()),
            },
            ExprP::Not(x) => {
                if self.expression_type(x)?.is_never() {
                    Ok(Ty::never())
                } else {
                    Ok(Ty::bool())
                }
            }
            ExprP::Minus(x) => self.expression_un_op(span, x, TypingUnOp::Minus),
            ExprP::Plus(x) => self.expression_un_op(span, x, TypingUnOp::Plus),
            ExprP::BitNot(x) => self.expression_un_op(span, x, TypingUnOp::BitNot),
            ExprP::Op(lhs, op, rhs) => self.expr_bin_op(span, lhs, *op, rhs),
            ExprP::If(c_t_f) => {
                let c = self.expression_type(&c_t_f.0)?;
                let t = self.expression_type(&c_t_f.1)?;
                let f = self.expression_type(&c_t_f.2)?;
                if c.is_never() {
                    Ok(Ty::never())
                } else {
                    Ok(Ty::union2(t, f))
                }
            }
            ExprP::List(xs) => {
                let ts = xs.try_map(|x| self.expression_type(x))?;
                Ok(Ty::list(Ty::unions(ts)))
            }
            ExprP::Dict(xs) => {
                let (ks, vs) = xs
                    .try_map(|(k, v)| Ok((self.expression_type(k)?, self.expression_type(v)?)))?
                    .into_iter()
                    .unzip();
                Ok(Ty::dict(Ty::unions(ks), Ty::unions(vs)))
            }
            ExprP::ListComprehension(a, b, c) => {
                self.check_comprehension(b, c)?;
                Ok(Ty::list(self.expression_type(a)?))
            }
            ExprP::DictComprehension(k_v, b, c) => {
                self.check_comprehension(b, c)?;
                Ok(Ty::dict(
                    self.expression_type(&k_v.0)?,
                    self.expression_type(&k_v.1)?,
                ))
            }
            ExprP::FString(_) => Ok(Ty::string()),
        }
    }
}
