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

// These are more readable for formulaic code like Uniplate
#![allow(clippy::many_single_char_names)]
// Sometimes we need |x| f(x) to do type/lifetime conversion, sometimes we don't.
// Most consistent to use the closure everywhere.
#![allow(clippy::redundant_closure)]

use crate::syntax::ast::AssignP;
use crate::syntax::ast::AstAssignIdentP;
use crate::syntax::ast::AstExprP;
use crate::syntax::ast::AstPayload;
use crate::syntax::ast::AstStmtP;
use crate::syntax::ast::ClauseP;
use crate::syntax::ast::ExprP;
use crate::syntax::ast::ForClauseP;
use crate::syntax::ast::ParameterP;
use crate::syntax::ast::StmtP;

pub(crate) enum Visit<'a, P: AstPayload> {
    Stmt(&'a AstStmtP<P>),
    Expr(&'a AstExprP<P>),
}

pub(crate) enum VisitMut<'a, P: AstPayload> {
    Stmt(&'a mut AstStmtP<P>),
    Expr(&'a mut AstExprP<P>),
}

impl<'a, P: AstPayload> Visit<'a, P> {
    #[allow(dead_code)]
    pub(crate) fn visit_children(&self, mut f: impl FnMut(Visit<'a, P>)) {
        match self {
            Self::Stmt(x) => x.visit_children(f),
            Self::Expr(x) => x.visit_expr(|x| f(Visit::Expr(x))),
        }
    }
}

impl<P: AstPayload> StmtP<P> {
    pub(crate) fn visit_children<'a>(&'a self, mut f: impl FnMut(Visit<'a, P>)) {
        match self {
            StmtP::Statements(xs) => xs.iter().for_each(|x| f(Visit::Stmt(x))),
            StmtP::If(condition, box then_block) => {
                f(Visit::Expr(condition));
                f(Visit::Stmt(then_block));
            }
            StmtP::IfElse(condition, box (then_block, else_block)) => {
                f(Visit::Expr(condition));
                f(Visit::Stmt(then_block));
                f(Visit::Stmt(else_block));
            }
            StmtP::Def(_, params, ret_type, body, _) => {
                params
                    .iter()
                    .for_each(|x| x.visit_expr(|x| f(Visit::Expr(x))));
                ret_type.iter().for_each(|x| f(Visit::Expr(x)));
                f(Visit::Stmt(body));
            }
            StmtP::For(lhs, box (over, body)) => {
                lhs.visit_expr(|x| f(Visit::Expr(x)));
                f(Visit::Expr(over));
                f(Visit::Stmt(body));
            }
            // Nothing else contains nested statements
            StmtP::Break => {}
            StmtP::Continue => {}
            StmtP::Pass => {}
            StmtP::Return(ret) => {
                ret.iter().for_each(|x| f(Visit::Expr(x)));
            }
            StmtP::Expression(e) => f(Visit::Expr(e)),
            StmtP::Assign(lhs, rhs) => {
                lhs.visit_expr(|x| f(Visit::Expr(x)));
                f(Visit::Expr(rhs));
            }
            StmtP::AssignModify(lhs, _, rhs) => {
                lhs.visit_expr(|x| f(Visit::Expr(x)));
                f(Visit::Expr(rhs));
            }
            StmtP::Load(..) => {}
        }
    }

    pub(crate) fn visit_children_mut<'a>(&'a mut self, mut f: impl FnMut(VisitMut<'a, P>)) {
        match self {
            StmtP::Statements(xs) => xs.iter_mut().for_each(|x| f(VisitMut::Stmt(x))),
            StmtP::If(condition, box then_block) => {
                f(VisitMut::Expr(condition));
                f(VisitMut::Stmt(then_block));
            }
            StmtP::IfElse(condition, box (then_block, else_block)) => {
                f(VisitMut::Expr(condition));
                f(VisitMut::Stmt(then_block));
                f(VisitMut::Stmt(else_block));
            }
            StmtP::Def(_, params, ret_type, body, _) => {
                params
                    .iter_mut()
                    .for_each(|x| x.visit_expr_mut(|x| f(VisitMut::Expr(x))));
                ret_type.iter_mut().for_each(|x| f(VisitMut::Expr(x)));
                f(VisitMut::Stmt(body));
            }
            StmtP::For(lhs, box (over, body)) => {
                lhs.visit_expr_mut(|x| f(VisitMut::Expr(x)));
                f(VisitMut::Expr(over));
                f(VisitMut::Stmt(body));
            }
            // Nothing else contains nested statements
            StmtP::Break => {}
            StmtP::Continue => {}
            StmtP::Pass => {}
            StmtP::Return(ret) => {
                ret.iter_mut().for_each(|x| f(VisitMut::Expr(x)));
            }
            StmtP::Expression(e) => f(VisitMut::Expr(e)),
            StmtP::Assign(lhs, rhs) => {
                lhs.visit_expr_mut(|x| f(VisitMut::Expr(x)));
                f(VisitMut::Expr(rhs));
            }
            StmtP::AssignModify(lhs, _, rhs) => {
                lhs.visit_expr_mut(|x| f(VisitMut::Expr(x)));
                f(VisitMut::Expr(rhs));
            }
            StmtP::Load(..) => {}
        }
    }

    pub(crate) fn visit_stmt<'a>(&'a self, mut f: impl FnMut(&'a AstStmtP<P>)) {
        self.visit_children(|x| match x {
            Visit::Stmt(x) => f(x),
            Visit::Expr(_) => {} // Nothing to do
        })
    }

    pub(crate) fn visit_stmt_mut<'a>(&'a mut self, mut f: impl FnMut(&'a mut AstStmtP<P>)) {
        self.visit_children_mut(|x| match x {
            VisitMut::Stmt(x) => f(x),
            VisitMut::Expr(_) => {} // Nothing to do
        })
    }

    pub(crate) fn visit_expr<'a>(&'a self, mut f: impl FnMut(&'a AstExprP<P>)) {
        // Note the &mut impl on f, it's subtle, see
        // https://stackoverflow.com/questions/54613966/error-reached-the-recursion-limit-while-instantiating-funcclosure
        fn pick<'a, P: AstPayload>(x: Visit<'a, P>, f: &mut impl FnMut(&'a AstExprP<P>)) {
            match x {
                Visit::Stmt(x) => x.visit_children(|x| pick(x, f)),
                Visit::Expr(x) => f(x),
            }
        }
        self.visit_children(|x| pick(x, &mut f))
    }

    pub(crate) fn visit_stmt_result<E>(
        &self,
        mut f: impl FnMut(&AstStmtP<P>) -> Result<(), E>,
    ) -> Result<(), E> {
        let mut result = Ok(());
        let f2 = |x: &AstStmtP<P>| {
            if result.is_ok() {
                result = f(x);
            }
        };
        self.visit_stmt(f2);
        result
    }
}

impl<P: AstPayload> ParameterP<P> {
    // Split a parameter into name, type, default value
    pub(crate) fn split(
        &self,
    ) -> (
        Option<&AstAssignIdentP<P>>,
        Option<&AstExprP<P>>,
        Option<&AstExprP<P>>,
    ) {
        match self {
            ParameterP::Normal(a, b) | ParameterP::Args(a, b) | ParameterP::KwArgs(a, b) => {
                (Some(a), b.as_ref().map(|x| &**x), None)
            }
            ParameterP::WithDefaultValue(a, b, c) => {
                (Some(a), b.as_ref().map(|x| &**x), Some(&**c))
            }
            ParameterP::NoArgs => (None, None, None),
        }
    }

    // Split a parameter into name, type, default value
    pub(crate) fn split_mut(
        &mut self,
    ) -> (
        Option<&mut AstAssignIdentP<P>>,
        Option<&mut AstExprP<P>>,
        Option<&mut AstExprP<P>>,
    ) {
        match self {
            ParameterP::Normal(a, b) | ParameterP::Args(a, b) | ParameterP::KwArgs(a, b) => {
                (Some(a), b.as_mut().map(|x| &mut **x), None)
            }
            ParameterP::WithDefaultValue(a, b, c) => {
                (Some(a), b.as_mut().map(|x| &mut **x), Some(&mut **c))
            }
            ParameterP::NoArgs => (None, None, None),
        }
    }

    pub(crate) fn visit_expr<'a>(&'a self, mut f: impl FnMut(&'a AstExprP<P>)) {
        let (_, typ, def) = self.split();
        typ.iter().for_each(|x| f(x));
        def.iter().for_each(|x| f(x));
    }

    pub(crate) fn visit_expr_mut<'a>(&'a mut self, mut f: impl FnMut(&'a mut AstExprP<P>)) {
        let (_, typ, def) = self.split_mut();
        if let Some(typ) = typ {
            f(typ);
        }
        if let Some(def) = def {
            f(def);
        }
    }
}

impl<P: AstPayload> ExprP<P> {
    pub(crate) fn visit_expr<'a>(&'a self, mut f: impl FnMut(&'a AstExprP<P>)) {
        match self {
            ExprP::Tuple(xs) => xs.iter().for_each(|x| f(x)),
            ExprP::Dot(x, _) => f(x),
            ExprP::Call(a, b) => {
                f(a);
                b.iter().for_each(|x| f(x.expr()));
            }
            ExprP::ArrayIndirection(box (a, b)) => {
                f(a);
                f(b);
            }
            ExprP::Slice(a, b, c, d) => {
                f(a);
                b.iter().for_each(|x| f(x));
                c.iter().for_each(|x| f(x));
                d.iter().for_each(|x| f(x));
            }
            ExprP::Identifier(..) => {}
            ExprP::Lambda(args, body, _) => {
                args.iter().for_each(|x| x.visit_expr(|x| f(x)));
                f(body);
            }
            ExprP::Literal(_) => {}
            ExprP::Not(x) => f(x),
            ExprP::Minus(x) => f(x),
            ExprP::Plus(x) => f(x),
            ExprP::BitNot(x) => f(x),
            ExprP::Op(x, _, y) => {
                f(x);
                f(y);
            }
            ExprP::If(box (a, b, c)) => {
                f(a);
                f(b);
                f(c);
            }
            ExprP::List(x) => x.iter().for_each(|x| f(x)),
            ExprP::Dict(x) => x.iter().for_each(|(x, y)| {
                f(x);
                f(y);
            }),
            ExprP::ListComprehension(x, for_, y) => {
                for_.visit_expr(|x| f(x));
                y.iter().for_each(|x| x.visit_expr(|x| f(x)));
                f(x);
            }
            ExprP::DictComprehension(x, for_, y) => {
                for_.visit_expr(|x| f(x));
                y.iter().for_each(|x| x.visit_expr(|x| f(x)));
                f(&x.0);
                f(&x.1);
            }
        }
    }

    pub(crate) fn visit_expr_mut<'a>(&'a mut self, mut f: impl FnMut(&'a mut AstExprP<P>)) {
        match self {
            ExprP::Tuple(xs) => xs.iter_mut().for_each(|x| f(x)),
            ExprP::Dot(x, _) => f(x),
            ExprP::Call(a, b) => {
                f(a);
                b.iter_mut().for_each(|x| f(x.expr_mut()));
            }
            ExprP::ArrayIndirection(box (a, b)) => {
                f(a);
                f(b);
            }
            ExprP::Slice(a, b, c, d) => {
                f(a);
                b.iter_mut().for_each(|x| f(x));
                c.iter_mut().for_each(|x| f(x));
                d.iter_mut().for_each(|x| f(x));
            }
            ExprP::Identifier(..) => {}
            ExprP::Lambda(args, body, _) => {
                args.iter_mut().for_each(|x| x.visit_expr_mut(|x| f(x)));
                f(body);
            }
            ExprP::Literal(_) => {}
            ExprP::Not(x) => f(x),
            ExprP::Minus(x) => f(x),
            ExprP::Plus(x) => f(x),
            ExprP::BitNot(x) => f(x),
            ExprP::Op(x, _, y) => {
                f(x);
                f(y);
            }
            ExprP::If(box (a, b, c)) => {
                f(a);
                f(b);
                f(c);
            }
            ExprP::List(x) => x.iter_mut().for_each(|x| f(x)),
            ExprP::Dict(x) => x.iter_mut().for_each(|(x, y)| {
                f(x);
                f(y);
            }),
            ExprP::ListComprehension(x, for_, y) => {
                for_.visit_expr_mut(|x| f(x));
                y.iter_mut().for_each(|x| x.visit_expr_mut(|x| f(x)));
                f(x);
            }
            ExprP::DictComprehension(x, for_, y) => {
                for_.visit_expr_mut(|x| f(x));
                y.iter_mut().for_each(|x| x.visit_expr_mut(|x| f(x)));
                f(&mut x.0);
                f(&mut x.1);
            }
        }
    }
}

impl<P: AstPayload> AssignP<P> {
    pub(crate) fn visit_expr<'a>(&'a self, mut f: impl FnMut(&'a AstExprP<P>)) {
        fn recurse<'a, P: AstPayload>(x: &'a AssignP<P>, f: &mut impl FnMut(&'a AstExprP<P>)) {
            match x {
                AssignP::Tuple(xs) => xs.iter().for_each(|x| recurse(x, f)),
                AssignP::Dot(a, _) => f(a),
                AssignP::ArrayIndirection(box (a, b)) => {
                    f(a);
                    f(b);
                }
                AssignP::Identifier(..) => {}
            }
        }
        recurse(self, &mut f)
    }

    pub(crate) fn visit_expr_mut<'a>(&'a mut self, mut f: impl FnMut(&'a mut AstExprP<P>)) {
        fn recurse<'a, P: AstPayload>(
            x: &'a mut AssignP<P>,
            f: &mut impl FnMut(&'a mut AstExprP<P>),
        ) {
            match x {
                AssignP::Tuple(ref mut xs) => xs.iter_mut().for_each(|x| recurse(&mut *x, f)),
                AssignP::Dot(a, _) => f(a),
                AssignP::ArrayIndirection(box (a, b)) => {
                    f(a);
                    f(b);
                }
                AssignP::Identifier(..) => {}
            }
        }
        recurse(self, &mut f)
    }

    /// Assuming this expression was on the left-hand-side of an assignment,
    /// visit all the names that are bound by this assignment.
    /// Note that assignments like `x[i] = n` don't bind any names.
    pub(crate) fn visit_lvalue<'a>(&'a self, mut f: impl FnMut(&'a AstAssignIdentP<P>)) {
        fn recurse<'a, P: AstPayload>(
            x: &'a AssignP<P>,
            f: &mut impl FnMut(&'a AstAssignIdentP<P>),
        ) {
            match x {
                AssignP::Identifier(x) => f(x),
                AssignP::Tuple(xs) => xs.iter().for_each(|x| recurse(x, f)),
                _ => {}
            }
        }
        recurse(self, &mut f)
    }

    pub(crate) fn visit_lvalue_mut<'a>(
        &'a mut self,
        mut f: impl FnMut(&'a mut AstAssignIdentP<P>),
    ) {
        fn recurse<'a, P: AstPayload>(
            x: &'a mut AssignP<P>,
            f: &mut impl FnMut(&'a mut AstAssignIdentP<P>),
        ) {
            match x {
                AssignP::Identifier(x) => f(x),
                AssignP::Tuple(xs) => xs.iter_mut().for_each(|x| recurse(x, f)),
                _ => {}
            }
        }
        recurse(self, &mut f)
    }
}

impl<P: AstPayload> ForClauseP<P> {
    pub(crate) fn visit_expr<'a>(&'a self, mut f: impl FnMut(&'a AstExprP<P>)) {
        self.var.visit_expr(&mut f);
        f(&self.over);
    }

    pub(crate) fn visit_expr_mut<'a>(&'a mut self, mut f: impl FnMut(&'a mut AstExprP<P>)) {
        self.var.visit_expr_mut(&mut f);
        f(&mut self.over);
    }
}

impl<P: AstPayload> ClauseP<P> {
    pub(crate) fn visit_expr<'a>(&'a self, mut f: impl FnMut(&'a AstExprP<P>)) {
        match self {
            ClauseP::For(x) => x.visit_expr(f),
            ClauseP::If(x) => f(x),
        }
    }

    pub(crate) fn visit_expr_mut<'a>(&'a mut self, mut f: impl FnMut(&'a mut AstExprP<P>)) {
        match self {
            ClauseP::For(x) => x.visit_expr_mut(f),
            ClauseP::If(x) => f(x),
        }
    }
}
