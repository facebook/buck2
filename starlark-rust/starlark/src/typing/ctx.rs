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

use gazebo::prelude::*;
use thiserror::Error;

use crate::codemap::CodeMap;
use crate::codemap::ResolvedFileSpan;
use crate::codemap::Span;
use crate::eval::compiler::scope::BindingId;
use crate::eval::compiler::scope::CstAssign;
use crate::eval::compiler::scope::CstExpr;
use crate::eval::compiler::scope::CstPayload;
use crate::eval::compiler::scope::ResolvedIdent;
use crate::syntax::ast::ArgumentP;
use crate::syntax::ast::AssignP;
use crate::syntax::ast::AstLiteral;
use crate::syntax::ast::BinOp;
use crate::syntax::ast::ClauseP;
use crate::syntax::ast::ExprP;
use crate::syntax::ast::ForClauseP;
use crate::typing::bindings::BindExpr;
use crate::typing::oracle::traits::TypingOracle;
use crate::typing::ty::Approximation;
use crate::typing::ty::Arg;
use crate::typing::ty::Param;
use crate::typing::ty::ParamMode;
use crate::typing::ty::Ty;
use crate::typing::ty::TyFunction;

#[derive(Error, Debug)]
pub(crate) enum TypingError {
    #[error("The attribute `{attr}` is not available on the type `{typ}`, at {loc}")]
    AttributeNotAvailable {
        loc: ResolvedFileSpan,
        typ: String,
        attr: String,
    },
    #[error("The builtin `{name}` is not known, at {loc}")]
    UnknownBuiltin { loc: ResolvedFileSpan, name: String },
    #[error("The call to `{name}` is invalid because {reason}, at {loc}")]
    InvalidBuiltinCall {
        loc: ResolvedFileSpan,
        name: String,
        reason: String,
    },
    #[error("Expected type `{require}` but got `{got}`, at {loc}")]
    IncompatibleType {
        loc: ResolvedFileSpan,
        got: String,
        require: String,
    },
    #[error("Call to a non-callable type `{ty}`, at {loc}")]
    CallToNonCallable { loc: ResolvedFileSpan, ty: String },
    #[error("Missing required parameter `{name}`, at {loc}")]
    MissingRequiredParameter { loc: ResolvedFileSpan, name: String },
    #[error("Unexpected parameter named `{name}`, at {loc}")]
    UnexpectedNamedArgument { loc: ResolvedFileSpan, name: String },
    #[error("Too many positional arguments, at {loc}")]
    TooManyPositionalArguments { loc: ResolvedFileSpan },
}

pub(crate) struct TypingContext<'a> {
    pub(crate) codemap: CodeMap,
    pub(crate) oracle: &'a dyn TypingOracle,
    // We'd prefer this to be a &mut self,
    // but that makes writing the code more fiddly, so just RefCell the errors
    pub(crate) errors: RefCell<Vec<TypingError>>,
    pub(crate) approximoations: RefCell<Vec<Approximation>>,
    pub(crate) types: HashMap<BindingId, Ty>,
}

impl TypingContext<'_> {
    fn add_error(&self, err: TypingError) -> Ty {
        self.errors.borrow_mut().push(err);
        Ty::Void
    }

    pub(crate) fn approximation(&self, category: &'static str, message: impl Debug) -> Ty {
        self.approximoations
            .borrow_mut()
            .push(Approximation::new(category, message));
        Ty::Any
    }

    fn resolve(&self, span: Span) -> ResolvedFileSpan {
        self.codemap.file_span(span).resolve()
    }

    fn validate_args(&self, params: &[Param], args: &[Arg], span: Span) {
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
                            self.add_error(TypingError::TooManyPositionalArguments {
                                loc: self.resolve(span),
                            });
                            return;
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
                        self.add_error(TypingError::UnexpectedNamedArgument {
                            loc: self.resolve(span),
                            name: name.clone(),
                        });
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
                    self.add_error(TypingError::MissingRequiredParameter {
                        loc: self.resolve(span),
                        name: param.name().to_owned(),
                    });
                }
                continue;
            }
            match param.mode {
                ParamMode::PosOnly | ParamMode::PosOrName(_) | ParamMode::NameOnly(_) => {
                    self.validate_type(args[0], &param.ty, span)
                }
                ParamMode::Args => {
                    for ty in args {
                        // For an arg, we require the type annotation to be inner value,
                        // rather than the outer (which is always a tuple)
                        self.validate_type(ty, &param.ty, span);
                    }
                }
                ParamMode::Kwargs => {
                    let val_types: Vec<_> = param
                        .ty
                        .iter_union()
                        .filter_map(|x| match x {
                            Ty::Dict(k_v) => Some(k_v.1.clone()),
                            _ => None,
                        })
                        .collect();
                    if !val_types.is_empty() {
                        let require = Ty::unions(val_types);
                        for ty in args {
                            self.validate_type(ty, &require, span);
                        }
                    }
                }
            }
        }
    }

    fn validate_call(&self, fun: &Ty, args: &[Arg], span: Span) -> Ty {
        fn unpack_function(x: &Ty) -> Option<&TyFunction> {
            match x {
                Ty::Function(x) => Some(x),
                _ => None,
            }
        }

        if fun.is_any() || fun.is_void() {
            return fun.clone(); // Everything is valid
        }
        let funs: Vec<_> = fun.iter_union().filter_map(unpack_function).collect();
        if funs.is_empty() {
            return self.add_error(TypingError::CallToNonCallable {
                loc: self.resolve(span),
                ty: fun.to_string(),
            });
        }

        // We call validate_args on each function, which will either
        // add to the errors state, or won't.
        // We capture the length of errors before we start, and after one iteration,
        // and if _any_ operation doesn't add to the errors, we are fine.
        let errors_before_all = self.errors.borrow().len();
        let mut errors_after_one = None;

        let mut successful_return_types = Vec::new();
        for fun in funs {
            let errors_before_this = self.errors.borrow().len();
            let return_type = if let Some(res) = self.oracle.builtin_call(&fun.name, args) {
                match res {
                    Ok(t) => t,
                    Err(reason) => self.add_error(TypingError::InvalidBuiltinCall {
                        loc: self.resolve(span),
                        name: fun.name.to_owned(),
                        reason,
                    }),
                }
            } else {
                self.validate_args(&fun.params, args, span);
                (*fun.result).clone()
            };
            let errors_after_this = self.errors.borrow().len();
            if errors_before_this == errors_after_this {
                successful_return_types.push(return_type);
            }
            if errors_after_one.is_none() {
                errors_after_one = Some(errors_after_this);
            }
        }
        if successful_return_types.is_empty() {
            // We definitely failed, but we might have failed many times, so just keep the errors
            // from the first function call, since we don't want to duplicate lots of errors
            assert!(errors_after_one.unwrap() > errors_before_all);
            self.errors.borrow_mut().truncate(errors_after_one.unwrap());
        } else {
            // Since one succeeded, we don't need any errors
            self.errors.borrow_mut().truncate(errors_before_all);
        }
        Ty::unions(successful_return_types)
    }

    fn from_iterated(&self, ty: &Ty, span: Span) -> Ty {
        self.expression_attribute(ty, "__iter__", span)
    }

    pub(crate) fn validate_type(&self, got: &Ty, require: &Ty, span: Span) {
        if !got.intersects(require, Some(self)) {
            self.add_error(TypingError::IncompatibleType {
                loc: self.resolve(span),
                got: got.to_string(),
                require: require.to_string(),
            });
        }
    }

    fn builtin(&self, name: &str, span: Span) -> Ty {
        match self.oracle.builtin(name) {
            Some(Ok(x)) => x,
            Some(Err(())) => self.add_error(TypingError::UnknownBuiltin {
                loc: self.resolve(span),
                name: name.to_owned(),
            }),
            None => self.approximation("oracle.builtin", name),
        }
    }

    fn expression_attribute(&self, ty: &Ty, attr: &str, span: Span) -> Ty {
        match ty.attribute(attr, self) {
            Ok(x) => x,
            Err(()) => self.add_error(TypingError::AttributeNotAvailable {
                loc: self.resolve(span),
                typ: ty.to_string(),
                attr: attr.to_owned(),
            }),
        }
    }

    fn expression_primitive_ty(&self, name: &str, arg0: Ty, args: Vec<Ty>, span: Span) -> Ty {
        let fun = self.expression_attribute(&arg0, &format!("__{}__", name.to_lowercase()), span);
        self.validate_call(&fun, &args.into_map(Arg::Pos), span)
    }

    fn expression_primitive(&self, name: &str, args: &[&CstExpr], span: Span) -> Ty {
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
                self.expression_primitive_ty(&format!("{:?}", op), lhs, vec![rhs], span)
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
                    Ty::Void
                }
            }
            BindExpr::ListExtend(id, e) => {
                if Ty::probably_a_list(&self.types[id]) {
                    Ty::list(self.from_iterated(&self.expression_type(e), e.span))
                } else {
                    // It doesn't seem to be a list, so let's assume the extend is non-mutating
                    Ty::Void
                }
            }
        }
    }

    /// Used to get the type of an expression when used as part of a ModifyAssign operation
    fn expression_assign(&self, x: &CstAssign) -> Ty {
        match &**x {
            AssignP::Tuple(_) => self.approximation("expression_assignment", x),
            AssignP::ArrayIndirection(a_b) => {
                self.expression_primitive("index", &[&a_b.0, &a_b.1], x.span)
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
            ExprP::Dot(a, b) => self.expression_attribute(&self.expression_type(a), b, b.span),
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
                        self.validate_type(&ty, &Ty::dict(Ty::Any, Ty::Any), x.span);
                        Arg::Kwargs(ty)
                    }
                });
                let f_ty = self.expression_type(f);
                // If we can't resolve the types of the arguments, we can't validate the call,
                // but we still know the type of the result since the args don't impact that
                self.validate_call(&f_ty, &args_ty, span)
            }
            ExprP::ArrayIndirection(a_b) => {
                self.expression_primitive("index", &[&a_b.0, &a_b.1], span)
            }
            ExprP::Slice(x, start, stop, stride) => {
                for e in [start, stop, stride].iter().copied().flatten() {
                    self.validate_type(&self.expression_type(e), &Ty::int(), e.span);
                }
                self.expression_attribute(&self.expression_type(x), "__slice__", span)
            }
            ExprP::Identifier(x, i) => {
                if let Some(ResolvedIdent::Slot((_, i))) = i {
                    if let Some(ty) = self.types.get(i) {
                        return ty.clone();
                    }
                }
                self.builtin(x, x.span)
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
                if self.expression_type(x).is_void() {
                    Ty::Void
                } else {
                    Ty::bool()
                }
            }
            ExprP::Minus(x) => self.expression_primitive("minus", &[&**x], span),
            ExprP::Plus(x) => self.expression_primitive("plus", &[&**x], span),
            ExprP::BitNot(x) => self.expression_primitive("bit_not", &[&**x], span),
            ExprP::Op(lhs, op, rhs) => {
                let lhs = self.expression_type(lhs);
                let rhs = self.expression_type(rhs);
                let bool_ret = if lhs.is_void() || rhs.is_void() {
                    Ty::Void
                } else {
                    Ty::bool()
                };
                match op {
                    BinOp::And | BinOp::Or => {
                        if lhs.is_void() {
                            Ty::Void
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
                        self.expression_primitive_ty("in", rhs, vec![lhs], span);
                        // Ignore the return type, we know it's always a bool
                        bool_ret
                    }
                    BinOp::Less | BinOp::LessOrEqual | BinOp::Greater | BinOp::GreaterOrEqual => {
                        self.expression_primitive_ty("less", lhs, vec![rhs], span);
                        bool_ret
                    }
                    _ => self.expression_primitive_ty(&format!("{:?}", op), lhs, vec![rhs], span),
                }
            }
            ExprP::If(c_t_f) => {
                let c = self.expression_type(&c_t_f.0);
                let t = self.expression_type(&c_t_f.1);
                let f = self.expression_type(&c_t_f.2);
                if c.is_void() {
                    Ty::Void
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
