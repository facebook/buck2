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

//! Map AST payload.

use gazebo::prelude::*;

use crate::codemap::Spanned;
use crate::syntax::ast::ArgumentP;
use crate::syntax::ast::AssignIdentP;
use crate::syntax::ast::AssignP;
use crate::syntax::ast::AstPayload;
use crate::syntax::ast::ClauseP;
use crate::syntax::ast::ExprP;
use crate::syntax::ast::ForClauseP;
use crate::syntax::ast::LoadP;
use crate::syntax::ast::ParameterP;
use crate::syntax::ast::StmtP;

pub(crate) trait AstPayloadFunction<A: AstPayload, B: AstPayload> {
    fn map_ident(&mut self, a: A::IdentPayload) -> B::IdentPayload;
    fn map_ident_assign(&mut self, a: A::IdentAssignPayload) -> B::IdentAssignPayload;
    fn map_def(&mut self, a: A::DefPayload) -> B::DefPayload;
}

impl<A: AstPayload> LoadP<A> {
    pub(crate) fn into_map_payload<B: AstPayload>(
        self,
        f: &mut impl AstPayloadFunction<A, B>,
    ) -> LoadP<B> {
        let LoadP { module, args } = self;
        LoadP {
            module,
            args: args.into_map(|(local, their)| (local.into_map_payload(f), their)),
        }
    }
}

impl<A: AstPayload> StmtP<A> {
    pub(crate) fn into_map_payload<B: AstPayload>(
        self,
        f: &mut impl AstPayloadFunction<A, B>,
    ) -> StmtP<B> {
        match self {
            StmtP::Break => StmtP::Break,
            StmtP::Continue => StmtP::Continue,
            StmtP::Pass => StmtP::Pass,
            StmtP::Return(None) => StmtP::Return(None),
            StmtP::Return(Some(e)) => StmtP::Return(Some(e.into_map_payload(f))),
            StmtP::Expression(e) => StmtP::Expression(e.into_map_payload(f)),
            StmtP::Assign(lhs, ty_rhs) => {
                let (ty, rhs) = *ty_rhs;
                StmtP::Assign(
                    lhs.into_map_payload(f),
                    Box::new((ty.map(|ty| ty.into_map_payload(f)), rhs.into_map_payload(f))),
                )
            }
            StmtP::AssignModify(lhs, op, rhs) => StmtP::AssignModify(
                lhs.into_map_payload(f),
                op,
                Box::new(rhs.into_map_payload(f)),
            ),
            StmtP::Statements(stmts) => {
                StmtP::Statements(stmts.into_map(|s| s.into_map_payload(f)))
            }
            StmtP::If(cond, then_block) => StmtP::If(
                cond.into_map_payload(f),
                Box::new(then_block.into_map_payload(f)),
            ),
            StmtP::IfElse(cond, then_block_else_block) => {
                let (then_block, else_block) = *then_block_else_block;
                StmtP::IfElse(
                    cond.into_map_payload(f),
                    Box::new((
                        then_block.into_map_payload(f),
                        else_block.into_map_payload(f),
                    )),
                )
            }
            StmtP::For(assign, coll_body) => {
                let (coll, body) = *coll_body;
                StmtP::For(
                    assign.into_map_payload(f),
                    Box::new((coll.into_map_payload(f), body.into_map_payload(f))),
                )
            }
            StmtP::Def(name, params, ret, body, p) => StmtP::Def(
                name.into_map_payload(f),
                params.into_map(|p| p.into_map_payload(f)),
                ret.map(|ret| Box::new(ret.into_map_payload(f))),
                Box::new(body.into_map_payload(f)),
                f.map_def(p),
            ),
            StmtP::Load(load) => StmtP::Load(load.into_map_payload(f)),
        }
    }
}

impl<A: AstPayload> ExprP<A> {
    pub(crate) fn into_map_payload<B: AstPayload>(
        self,
        f: &mut impl AstPayloadFunction<A, B>,
    ) -> ExprP<B> {
        match self {
            ExprP::Tuple(exprs) => ExprP::Tuple(exprs.into_map(|e| e.into_map_payload(f))),
            ExprP::Dot(object, field) => ExprP::Dot(Box::new(object.into_map_payload(f)), field),
            ExprP::Call(ca, args) => ExprP::Call(
                Box::new(ca.into_map_payload(f)),
                args.into_map(|a| a.into_map_payload(f)),
            ),
            ExprP::ArrayIndirection(array_index) => {
                let (array, index) = *array_index;
                ExprP::ArrayIndirection(Box::new((
                    array.into_map_payload(f),
                    index.into_map_payload(f),
                )))
            }
            ExprP::Slice(x, a, b, c) => ExprP::Slice(
                Box::new(x.into_map_payload(f)),
                a.map(|e| Box::new(e.into_map_payload(f))),
                b.map(|e| Box::new(e.into_map_payload(f))),
                c.map(|e| Box::new(e.into_map_payload(f))),
            ),
            ExprP::Identifier(id, p) => ExprP::Identifier(id, f.map_ident(p)),
            ExprP::Lambda(ps, body, p) => ExprP::Lambda(
                ps.into_map(|p| p.into_map_payload(f)),
                Box::new(body.into_map_payload(f)),
                f.map_def(p),
            ),
            ExprP::Literal(l) => ExprP::Literal(l),
            ExprP::Not(e) => ExprP::Not(Box::new(e.into_map_payload(f))),
            ExprP::Minus(e) => ExprP::Minus(Box::new(e.into_map_payload(f))),
            ExprP::Plus(e) => ExprP::Plus(Box::new(e.into_map_payload(f))),
            ExprP::BitNot(e) => ExprP::BitNot(Box::new(e.into_map_payload(f))),
            ExprP::Op(l, op, r) => ExprP::Op(
                Box::new(l.into_map_payload(f)),
                op,
                Box::new(r.into_map_payload(f)),
            ),
            ExprP::If(a_b_c) => {
                let (a, b, c) = *a_b_c;
                ExprP::If(Box::new((
                    a.into_map_payload(f),
                    b.into_map_payload(f),
                    c.into_map_payload(f),
                )))
            }
            ExprP::List(es) => ExprP::List(es.into_map(|e| e.into_map_payload(f))),
            ExprP::Dict(kvs) => {
                ExprP::Dict(kvs.into_map(|(k, v)| (k.into_map_payload(f), v.into_map_payload(f))))
            }
            ExprP::ListComprehension(e, c0, cs) => ExprP::ListComprehension(
                Box::new(e.into_map_payload(f)),
                Box::new(c0.into_map_payload(f)),
                cs.into_map(|c| c.into_map_payload(f)),
            ),
            ExprP::DictComprehension(k_v, c0, cs) => {
                let (k, v) = *k_v;
                ExprP::DictComprehension(
                    Box::new((k.into_map_payload(f), v.into_map_payload(f))),
                    Box::new(c0.into_map_payload(f)),
                    cs.into_map(|c| c.into_map_payload(f)),
                )
            }
        }
    }
}

impl<A: AstPayload> AssignP<A> {
    pub(crate) fn into_map_payload<B: AstPayload>(
        self,
        f: &mut impl AstPayloadFunction<A, B>,
    ) -> AssignP<B> {
        match self {
            AssignP::Tuple(args) => AssignP::Tuple(args.into_map(|a| a.into_map_payload(f))),
            AssignP::ArrayIndirection(array_index) => {
                let (array, index) = *array_index;
                AssignP::ArrayIndirection(Box::new((
                    array.into_map_payload(f),
                    index.into_map_payload(f),
                )))
            }
            AssignP::Dot(object, field) => {
                AssignP::Dot(Box::new(object.into_map_payload(f)), field)
            }
            AssignP::Identifier(ident) => AssignP::Identifier(ident.into_map_payload(f)),
        }
    }
}

impl<A: AstPayload> AssignIdentP<A> {
    pub(crate) fn into_map_payload<B: AstPayload>(
        self,
        f: &mut impl AstPayloadFunction<A, B>,
    ) -> AssignIdentP<B> {
        let AssignIdentP(s, p) = self;
        AssignIdentP(s, f.map_ident_assign(p))
    }
}

impl<A: AstPayload> ParameterP<A> {
    pub(crate) fn into_map_payload<B: AstPayload>(
        self,
        f: &mut impl AstPayloadFunction<A, B>,
    ) -> ParameterP<B> {
        match self {
            ParameterP::Normal(name, ty) => ParameterP::Normal(
                name.into_map_payload(f),
                ty.map(|defa| Box::new(defa.into_map_payload(f))),
            ),
            ParameterP::WithDefaultValue(name, ty, defa) => ParameterP::WithDefaultValue(
                name.into_map_payload(f),
                ty.map(|defa| Box::new(defa.into_map_payload(f))),
                Box::new(defa.into_map_payload(f)),
            ),
            ParameterP::NoArgs => ParameterP::NoArgs,
            ParameterP::Args(name, ty) => ParameterP::Args(
                name.into_map_payload(f),
                ty.map(|defa| Box::new(defa.into_map_payload(f))),
            ),
            ParameterP::KwArgs(name, ty) => ParameterP::KwArgs(
                name.into_map_payload(f),
                ty.map(|defa| Box::new(defa.into_map_payload(f))),
            ),
        }
    }
}

impl<A: AstPayload> ArgumentP<A> {
    pub(crate) fn into_map_payload<B: AstPayload>(
        self,
        f: &mut impl AstPayloadFunction<A, B>,
    ) -> ArgumentP<B> {
        match self {
            ArgumentP::Positional(e) => ArgumentP::Positional(e.into_map_payload(f)),
            ArgumentP::Named(n, e) => ArgumentP::Named(n, e.into_map_payload(f)),
            ArgumentP::Args(e) => ArgumentP::Args(e.into_map_payload(f)),
            ArgumentP::KwArgs(e) => ArgumentP::KwArgs(e.into_map_payload(f)),
        }
    }
}

impl<A: AstPayload> ClauseP<A> {
    pub(crate) fn into_map_payload<B: AstPayload>(
        self,
        f: &mut impl AstPayloadFunction<A, B>,
    ) -> ClauseP<B> {
        match self {
            ClauseP::For(c) => ClauseP::For(c.into_map_payload(f)),
            ClauseP::If(e) => ClauseP::If(e.into_map_payload(f)),
        }
    }
}

impl<A: AstPayload> ForClauseP<A> {
    pub(crate) fn into_map_payload<B: AstPayload>(
        self,
        f: &mut impl AstPayloadFunction<A, B>,
    ) -> ForClauseP<B> {
        ForClauseP {
            var: self.var.into_map_payload(f),
            over: self.over.into_map_payload(f),
        }
    }
}

macro_rules! ast_payload_map_stub {
    ($ast_type:ident) => {
        impl<A: AstPayload> Spanned<$ast_type<A>> {
            pub(crate) fn into_map_payload<B: AstPayload>(
                self,
                f: &mut impl AstPayloadFunction<A, B>,
            ) -> Spanned<$ast_type<B>> {
                let Spanned { span, node } = self;
                Spanned {
                    span,
                    node: node.into_map_payload(f),
                }
            }
        }
    };
}

ast_payload_map_stub!(ExprP);
ast_payload_map_stub!(AssignP);
ast_payload_map_stub!(AssignIdentP);
ast_payload_map_stub!(ParameterP);
ast_payload_map_stub!(ArgumentP);
ast_payload_map_stub!(LoadP);
ast_payload_map_stub!(StmtP);
