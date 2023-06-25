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
use std::collections::HashMap;
use std::fmt::Debug;

use thiserror::Error;

use crate::codemap::CodeMap;
use crate::codemap::Span;
use crate::eval::compiler::scope::payload::CstAssign;
use crate::eval::compiler::scope::payload::CstExpr;
use crate::eval::compiler::scope::payload::CstPayload;
use crate::eval::compiler::scope::BindingId;
use crate::eval::compiler::scope::ResolvedIdent;
use crate::slice_vec_ext::SliceExt;
use crate::slice_vec_ext::VecExt;
use crate::syntax::ast::ArgumentP;
use crate::syntax::ast::AssignOp;
use crate::syntax::ast::AssignP;
use crate::syntax::ast::AstLiteral;
use crate::syntax::ast::BinOp;
use crate::syntax::ast::ClauseP;
use crate::syntax::ast::ExprP;
use crate::syntax::ast::ForClauseP;
use crate::typing::bindings::BindExpr;
use crate::typing::error::TypingError;
use crate::typing::oracle::traits::TypingAttr;
use crate::typing::oracle::traits::TypingBinOp;
use crate::typing::oracle::traits::TypingOracle;
use crate::typing::oracle::traits::TypingUnOp;
use crate::typing::ty::Approximation;
use crate::typing::ty::Arg;
use crate::typing::ty::Param;
use crate::typing::ty::ParamMode;
use crate::typing::ty::Ty;
use crate::typing::ty::TyFunction;
use crate::typing::OracleDocs;

#[derive(Error, Debug)]
enum TypingContextError {
    #[error("The attribute `{attr}` is not available on the type `{typ}`")]
    AttributeNotAvailable { typ: String, attr: String },
    #[error("The builtin `{name}` is not known")]
    UnknownBuiltin { name: String },
    #[error("Expected type `{require}` but got `{got}`")]
    IncompatibleType { got: String, require: String },
    #[error("Call to a non-callable type `{ty}`")]
    CallToNonCallable { ty: String },
    #[error("Missing required parameter `{name}`")]
    MissingRequiredParameter { name: String },
    #[error("Unexpected parameter named `{name}`")]
    UnexpectedNamedArgument { name: String },
    #[error("Too many positional arguments")]
    TooManyPositionalArguments,
    #[error("Call arguments incompatible")]
    CallArgumentsIncompatible,
}

pub(crate) struct TypingContext<'a> {
    pub(crate) codemap: &'a CodeMap,
    pub(crate) oracle: &'a dyn TypingOracle,
    pub(crate) global_docs: OracleDocs,
    // We'd prefer this to be a &mut self,
    // but that makes writing the code more fiddly, so just RefCell the errors
    pub(crate) errors: RefCell<Vec<TypingError>>,
    pub(crate) approximoations: RefCell<Vec<Approximation>>,
    pub(crate) types: HashMap<BindingId, Ty>,
}

impl TypingContext<'_> {
    fn mk_error(&self, span: Span, err: TypingContextError) -> TypingError {
        TypingError::new(err.into(), span, self.codemap)
    }

    fn add_error(&self, span: Span, err: TypingContextError) -> Ty {
        let err = self.mk_error(span, err);
        self.errors.borrow_mut().push(err);
        Ty::Never
    }

    pub(crate) fn approximation(&self, category: &'static str, message: impl Debug) -> Ty {
        self.approximoations
            .borrow_mut()
            .push(Approximation::new(category, message));
        Ty::Any
    }

    fn validate_args(&self, params: &[Param], args: &[Arg], span: Span) -> Result<(), TypingError> {
        // Want to figure out which arguments go in which positions
        let mut param_args: Vec<Vec<&Ty>> = vec![vec![]; params.len()];
        // The next index a positional parameter might fill
        let mut param_pos = 0;
        let mut seen_vargs = false;

        for arg in args {
            match arg {
                Arg::Pos(ty) => loop {
                    match params.get(param_pos) {
                        None => {
                            return Err(
                                self.mk_error(span, TypingContextError::TooManyPositionalArguments)
                            );
                        }
                        Some(param) => {
                            let found_index = param_pos;
                            if param.mode != ParamMode::Args {
                                param_pos += 1;
                            }
                            if param.allows_pos() {
                                param_args[found_index].push(ty);
                                break;
                            }
                        }
                    }
                },
                Arg::Name(name, ty) => {
                    let mut success = false;
                    for (i, param) in params.iter().enumerate() {
                        if param.name() == name || param.mode == ParamMode::Kwargs {
                            param_args[i].push(ty);
                            success = true;
                            break;
                        }
                    }
                    if !success {
                        return Err(self.mk_error(
                            span,
                            TypingContextError::UnexpectedNamedArgument { name: name.clone() },
                        ));
                    }
                }
                Arg::Args(_) => {
                    param_pos = params.len();
                    seen_vargs = true;
                }
                Arg::Kwargs(_) => {
                    seen_vargs = true;
                }
            }
        }

        for (param, args) in std::iter::zip(params, param_args) {
            if !param.allows_many() && args.len() > 1 {
                panic!("bad")
            }
            if args.is_empty() {
                // We assume that *args/**kwargs might have splatted things everywhere.
                if !param.optional && !seen_vargs {
                    return Err(self.mk_error(
                        span,
                        TypingContextError::MissingRequiredParameter {
                            name: param.name().to_owned(),
                        },
                    ));
                }
                continue;
            }
            match param.mode {
                ParamMode::PosOnly | ParamMode::PosOrName(_) | ParamMode::NameOnly(_) => {
                    self.validate_type_result(args[0], &param.ty, span)?;
                }
                ParamMode::Args => {
                    for ty in args {
                        // For an arg, we require the type annotation to be inner value,
                        // rather than the outer (which is always a tuple)
                        self.validate_type_result(ty, &param.ty, span)?;
                    }
                }
                ParamMode::Kwargs => {
                    let val_types: Vec<_> = param
                        .ty
                        .iter_union()
                        .iter()
                        .filter_map(|x| match x {
                            Ty::Dict(k_v) => Some(k_v.1.clone()),
                            _ => None,
                        })
                        .collect();
                    if !val_types.is_empty() {
                        let require = Ty::unions(val_types);
                        for ty in args {
                            self.validate_type_result(ty, &require, span)?;
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn validate_fn_call(
        &self,
        span: Span,
        fun: &TyFunction,
        args: &[Arg],
    ) -> Result<Ty, TypingError> {
        self.validate_args(&fun.params, args, span)?;
        Ok((*fun.result).clone())
    }

    /// Return `Result` instead of adding to `errors`.
    #[allow(clippy::collapsible_else_if)]
    fn validate_call_result(&self, span: Span, fun: &Ty, args: &[Arg]) -> Result<Ty, TypingError> {
        match fun {
            Ty::Never => Ok(Ty::Never),
            Ty::Any => Ok(Ty::Any),
            Ty::Name(n) => match self.oracle.as_function(n) {
                None => {
                    // Unknown type, may be callable.
                    Ok(Ty::Any)
                }
                Some(Ok(f)) => self.validate_fn_call(span, &f, args),
                Some(Err(())) => Err(self.mk_error(
                    span,
                    TypingContextError::CallToNonCallable {
                        ty: fun.to_string(),
                    },
                )),
            },
            Ty::List(_) | Ty::Dict(_) | Ty::Tuple(_) | Ty::Struct { .. } => Err(self.mk_error(
                span,
                TypingContextError::CallToNonCallable {
                    ty: fun.to_string(),
                },
            )),
            Ty::Iter(_) => {
                // Unknown type, may be callable.
                Ok(Ty::Any)
            }
            Ty::Function(f) => self.validate_fn_call(span, f, args),
            Ty::Custom(t) => {
                t.0.validate_call(args, &self.oracle)
                    .map_err(|e| TypingError::msg(e, span, self.codemap))
            }
            Ty::Union(variants) => {
                let mut successful = Vec::new();
                let mut errors = Vec::new();
                for variant in variants.alternatives() {
                    match self.validate_call_result(span, variant, args) {
                        Ok(ty) => successful.push(ty),
                        Err(e) => errors.push(e),
                    }
                }
                if !successful.is_empty() {
                    Ok(Ty::unions(successful))
                } else {
                    if errors.len() == 1 {
                        Err(errors.pop().unwrap())
                    } else {
                        Err(self.mk_error(span, TypingContextError::CallArgumentsIncompatible))
                    }
                }
            }
        }
    }

    fn validate_call(&self, fun: &Ty, args: &[Arg], span: Span) -> Ty {
        match self.validate_call_result(span, fun, args) {
            Ok(ty) => ty,
            Err(e) => {
                self.errors.borrow_mut().push(e);
                Ty::Never
            }
        }
    }

    fn from_iterated(&self, ty: &Ty, span: Span) -> Ty {
        self.expression_attribute(ty, TypingAttr::Iter, span)
    }

    pub(crate) fn validate_type_result(
        &self,
        got: &Ty,
        require: &Ty,
        span: Span,
    ) -> Result<(), TypingError> {
        if !got.intersects(require, Some(self.oracle)) {
            Err(self.mk_error(
                span,
                TypingContextError::IncompatibleType {
                    got: got.to_string(),
                    require: require.to_string(),
                },
            ))
        } else {
            Ok(())
        }
    }

    pub(crate) fn validate_type(&self, got: &Ty, require: &Ty, span: Span) {
        if let Err(e) = self.validate_type_result(got, require, span) {
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

    fn expression_attribute(&self, ty: &Ty, attr: TypingAttr, span: Span) -> Ty {
        match ty.attribute(attr, self) {
            Ok(x) => x,
            Err(()) => self.add_error(
                span,
                TypingContextError::AttributeNotAvailable {
                    typ: ty.to_string(),
                    attr: attr.to_string(),
                },
            ),
        }
    }

    fn expression_primitive_ty(&self, name: TypingAttr, arg0: Ty, args: Vec<Ty>, span: Span) -> Ty {
        let fun = self.expression_attribute(&arg0, name, span);
        self.validate_call(&fun, &args.into_map(Arg::Pos), span)
    }

    fn expression_primitive(&self, name: TypingAttr, args: &[&CstExpr], span: Span) -> Ty {
        let t0 = self.expression_type(args[0]);
        let ts = args[1..].map(|x| self.expression_type(x));
        self.expression_primitive_ty(name, t0, ts, span)
    }

    pub(crate) fn expression_bind_type(&self, x: &BindExpr) -> Ty {
        match x {
            BindExpr::Expr(x) => self.expression_type(x),
            BindExpr::GetIndex(i, x) => self.expression_bind_type(x).indexed(*i),
            BindExpr::Iter(x) => self.from_iterated(&self.expression_bind_type(x), x.span()),
            BindExpr::AssignOp(lhs, op, rhs) => {
                let span = lhs.span;
                let rhs = self.expression_type(rhs);
                let lhs = self.expression_assign(lhs);
                let attr = match op {
                    AssignOp::Add => TypingAttr::BinOp(TypingBinOp::Add),
                    AssignOp::Subtract => TypingAttr::BinOp(TypingBinOp::Sub),
                    AssignOp::Multiply => TypingAttr::BinOp(TypingBinOp::Mul),
                    AssignOp::Divide => TypingAttr::BinOp(TypingBinOp::Div),
                    AssignOp::FloorDivide => TypingAttr::BinOp(TypingBinOp::FloorDiv),
                    AssignOp::Percent => TypingAttr::BinOp(TypingBinOp::Percent),
                    AssignOp::BitAnd => TypingAttr::BinOp(TypingBinOp::BitAnd),
                    AssignOp::BitOr => TypingAttr::BinOp(TypingBinOp::BitOr),
                    AssignOp::BitXor => TypingAttr::BinOp(TypingBinOp::BitXor),
                    AssignOp::LeftShift => TypingAttr::BinOp(TypingBinOp::LeftShift),
                    AssignOp::RightShift => TypingAttr::BinOp(TypingBinOp::RightShift),
                };
                self.expression_primitive_ty(attr, lhs, vec![rhs], span)
            }
            BindExpr::SetIndex(id, index, e) => {
                let span = index.span;
                let index = self.expression_type(index);
                let e = self.expression_bind_type(e);
                let mut res = Vec::new();
                // We know about list and dict, everything else we just ignore
                if self.types[id].is_list() {
                    // If we know it MUST be a list, then the index must be an int
                    self.validate_type(&index, &Ty::int(), span);
                }
                for ty in self.types[id].iter_union() {
                    match ty {
                        Ty::List(_) => {
                            res.push(Ty::list(e.clone()));
                        }
                        Ty::Dict(_) => {
                            res.push(Ty::dict(index.clone(), e.clone()));
                        }
                        _ => {
                            // Either it's not something we can apply this to, in which case do nothing.
                            // Or it's an Any, in which case we aren't going to change its type or spot errors.
                        }
                    }
                }
                Ty::unions(res)
            }
            BindExpr::ListAppend(id, e) => {
                if Ty::probably_a_list(&self.types[id]) {
                    Ty::list(self.expression_type(e))
                } else {
                    // It doesn't seem to be a list, so let's assume the append is non-mutating
                    Ty::Never
                }
            }
            BindExpr::ListExtend(id, e) => {
                if Ty::probably_a_list(&self.types[id]) {
                    Ty::list(self.from_iterated(&self.expression_type(e), e.span))
                } else {
                    // It doesn't seem to be a list, so let's assume the extend is non-mutating
                    Ty::Never
                }
            }
        }
    }

    /// Used to get the type of an expression when used as part of a ModifyAssign operation
    fn expression_assign(&self, x: &CstAssign) -> Ty {
        match &**x {
            AssignP::Tuple(_) => self.approximation("expression_assignment", x),
            AssignP::ArrayIndirection(a_b) => {
                self.expression_primitive(TypingAttr::Index, &[&a_b.0, &a_b.1], x.span)
            }
            AssignP::Dot(_, _) => self.approximation("expression_assignment", x),
            AssignP::Identifier(x) => {
                if let Some(i) = x.1 {
                    if let Some(ty) = self.types.get(&i) {
                        return ty.clone();
                    }
                }
                panic!("Unknown identifier")
            }
        }
    }

    /// We don't need the type out of the clauses (it doesn't change the overall type),
    /// but it is important we see through to the nested expressions to raise errors
    fn check_comprehension(&self, for_: &ForClauseP<CstPayload>, clauses: &[ClauseP<CstPayload>]) {
        self.expression_type(&for_.over);
        for x in clauses {
            match x {
                ClauseP::For(x) => self.expression_type(&x.over),
                ClauseP::If(x) => self.expression_type(x),
            };
        }
    }

    pub(crate) fn expression_type(&self, x: &CstExpr) -> Ty {
        let span = x.span;
        match &**x {
            ExprP::Tuple(xs) => Ty::Tuple(xs.map(|x| self.expression_type(x))),
            ExprP::Dot(a, b) => {
                self.expression_attribute(&self.expression_type(a), TypingAttr::Regular(b), b.span)
            }
            ExprP::Call(f, args) => {
                let args_ty = args.map(|x| match &**x {
                    ArgumentP::Positional(x) => Arg::Pos(self.expression_type(x)),
                    ArgumentP::Named(name, x) => {
                        Arg::Name((**name).clone(), self.expression_type(x))
                    }
                    ArgumentP::Args(x) => {
                        let ty = self.expression_type(x);
                        self.from_iterated(&ty, x.span);
                        Arg::Args(ty)
                    }
                    ArgumentP::KwArgs(x) => {
                        let ty = self.expression_type(x);
                        self.validate_type(&ty, &Ty::dict(Ty::string(), Ty::Any), x.span);
                        Arg::Kwargs(ty)
                    }
                });
                let f_ty = self.expression_type(f);
                // If we can't resolve the types of the arguments, we can't validate the call,
                // but we still know the type of the result since the args don't impact that
                self.validate_call(&f_ty, &args_ty, span)
            }
            ExprP::ArrayIndirection(a_b) => {
                self.expression_primitive(TypingAttr::Index, &[&a_b.0, &a_b.1], span)
            }
            ExprP::Slice(x, start, stop, stride) => {
                for e in [start, stop, stride].iter().copied().flatten() {
                    self.validate_type(&self.expression_type(e), &Ty::int(), e.span);
                }
                self.expression_attribute(&self.expression_type(x), TypingAttr::Slice, span)
            }
            ExprP::Identifier(x) => {
                match &x.node.1 {
                    Some(ResolvedIdent::Slot(_, i)) => {
                        if let Some(ty) = self.types.get(i) {
                            ty.clone()
                        } else {
                            // All types must be resolved to this point,
                            // this code is unreachable.
                            Ty::Any
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
                        Ty::Any
                    }
                }
            }
            ExprP::Lambda(_) => {
                self.approximation("We don't type check lambdas", ());
                Ty::name("function")
            }
            ExprP::Literal(x) => match x {
                AstLiteral::Int(_) => Ty::int(),
                AstLiteral::Float(_) => Ty::float(),
                AstLiteral::String(_) => Ty::string(),
            },
            ExprP::Not(x) => {
                if self.expression_type(x).is_never() {
                    Ty::Never
                } else {
                    Ty::bool()
                }
            }
            ExprP::Minus(x) => {
                self.expression_primitive(TypingAttr::UnOp(TypingUnOp::Minus), &[&**x], span)
            }
            ExprP::Plus(x) => {
                self.expression_primitive(TypingAttr::UnOp(TypingUnOp::Plus), &[&**x], span)
            }
            ExprP::BitNot(x) => {
                self.expression_primitive(TypingAttr::UnOp(TypingUnOp::BitNot), &[&**x], span)
            }
            ExprP::Op(lhs, op, rhs) => {
                let lhs = self.expression_type(lhs);
                let rhs = self.expression_type(rhs);
                let bool_ret = if lhs.is_never() || rhs.is_never() {
                    Ty::Never
                } else {
                    Ty::bool()
                };
                match op {
                    BinOp::And | BinOp::Or => {
                        if lhs.is_never() {
                            Ty::Never
                        } else {
                            Ty::union2(lhs, rhs)
                        }
                    }
                    BinOp::Equal | BinOp::NotEqual => {
                        // It's not an error to compare two different types, but it is pointless
                        self.validate_type(&lhs, &rhs, span);
                        bool_ret
                    }
                    BinOp::In | BinOp::NotIn => {
                        // We dispatch `x in y` as y.__in__(x) as that's how we validate
                        self.expression_primitive_ty(
                            TypingAttr::BinOp(TypingBinOp::In),
                            rhs,
                            vec![lhs],
                            span,
                        );
                        // Ignore the return type, we know it's always a bool
                        bool_ret
                    }
                    BinOp::Less | BinOp::LessOrEqual | BinOp::Greater | BinOp::GreaterOrEqual => {
                        self.expression_primitive_ty(
                            TypingAttr::BinOp(TypingBinOp::Less),
                            lhs,
                            vec![rhs],
                            span,
                        );
                        bool_ret
                    }
                    BinOp::Subtract => self.expression_primitive_ty(
                        TypingAttr::BinOp(TypingBinOp::Sub),
                        lhs,
                        vec![rhs],
                        span,
                    ),
                    BinOp::Add => self.expression_primitive_ty(
                        TypingAttr::BinOp(TypingBinOp::Add),
                        lhs,
                        vec![rhs],
                        span,
                    ),
                    BinOp::Multiply => self.expression_primitive_ty(
                        TypingAttr::BinOp(TypingBinOp::Mul),
                        lhs,
                        vec![rhs],
                        span,
                    ),
                    BinOp::Percent => self.expression_primitive_ty(
                        TypingAttr::BinOp(TypingBinOp::Percent),
                        lhs,
                        vec![rhs],
                        span,
                    ),
                    BinOp::Divide => self.expression_primitive_ty(
                        TypingAttr::BinOp(TypingBinOp::Div),
                        lhs,
                        vec![rhs],
                        span,
                    ),
                    BinOp::FloorDivide => self.expression_primitive_ty(
                        TypingAttr::BinOp(TypingBinOp::FloorDiv),
                        lhs,
                        vec![rhs],
                        span,
                    ),
                    BinOp::BitAnd => self.expression_primitive_ty(
                        TypingAttr::BinOp(TypingBinOp::BitAnd),
                        lhs,
                        vec![rhs],
                        span,
                    ),
                    BinOp::BitOr => self.expression_primitive_ty(
                        TypingAttr::BinOp(TypingBinOp::BitOr),
                        lhs,
                        vec![rhs],
                        span,
                    ),
                    BinOp::BitXor => self.expression_primitive_ty(
                        TypingAttr::BinOp(TypingBinOp::BitXor),
                        lhs,
                        vec![rhs],
                        span,
                    ),
                    BinOp::LeftShift => self.expression_primitive_ty(
                        TypingAttr::BinOp(TypingBinOp::LeftShift),
                        lhs,
                        vec![rhs],
                        span,
                    ),
                    BinOp::RightShift => self.expression_primitive_ty(
                        TypingAttr::BinOp(TypingBinOp::RightShift),
                        lhs,
                        vec![rhs],
                        span,
                    ),
                }
            }
            ExprP::If(c_t_f) => {
                let c = self.expression_type(&c_t_f.0);
                let t = self.expression_type(&c_t_f.1);
                let f = self.expression_type(&c_t_f.2);
                if c.is_never() {
                    Ty::Never
                } else {
                    Ty::union2(t, f)
                }
            }
            ExprP::List(xs) => {
                let ts = xs.map(|x| self.expression_type(x));
                Ty::list(Ty::unions(ts))
            }
            ExprP::Dict(xs) => {
                let (ks, vs) = xs
                    .iter()
                    .map(|(k, v)| (self.expression_type(k), self.expression_type(v)))
                    .unzip();
                Ty::dict(Ty::unions(ks), Ty::unions(vs))
            }
            ExprP::ListComprehension(a, b, c) => {
                self.check_comprehension(b, c);
                Ty::list(self.expression_type(a))
            }
            ExprP::DictComprehension(k_v, b, c) => {
                self.check_comprehension(b, c);
                Ty::dict(self.expression_type(&k_v.0), self.expression_type(&k_v.1))
            }
        }
    }
}
