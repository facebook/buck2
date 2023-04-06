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

use crate::codemap::Pos;
use crate::codemap::Span;
use crate::syntax::ast::AssignIdentP;
use crate::syntax::ast::AstAssign;
use crate::syntax::ast::AstAssignIdent;
use crate::syntax::ast::AstExpr;
use crate::syntax::ast::AstParameter;
use crate::syntax::ast::AstStmt;
use crate::syntax::ast::AstString;
use crate::syntax::ast::Clause;
use crate::syntax::ast::DefP;
use crate::syntax::ast::Expr;
use crate::syntax::ast::ForClause;
use crate::syntax::ast::LambdaP;
use crate::syntax::ast::Stmt;
use crate::syntax::AstModule;

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) enum Assigner {
    /// Obtained from `load`. `name` is the symbol in that file, not necessarily the local name
    Load {
        path: AstString,
        name: AstString,
    },
    Argument, // From a function argument
    Assign,   // From an assignment
}

#[derive(Debug)]
pub(crate) enum Bind {
    Set(Assigner, AstAssignIdent), // Variable assigned to directly
    Get(AstString),                // Variable that is referenced
    GetDotted(GetDotted),          // Variable is referenced, but is part of a dotted access
    Flow,         // Flow control occurs here (if, for etc) - can arrive or leave at this point
    Scope(Scope), // Entering a new scope (lambda/def/comprehension)
}

/// A 'get' bind that was part of a dotted member access pattern.
///
/// e.g. `x.y.z` would be considered a variable of `x` with attributes `y` and `z`
#[derive(Debug, Clone)]
pub(crate) struct GetDotted {
    pub(crate) variable: AstString,
    pub(crate) attributes: Vec<AstString>,
}

impl GetDotted {
    fn new(variable: AstString, attributes: Vec<AstString>) -> Self {
        Self {
            variable,
            attributes,
        }
    }

    /// Determines if a position is contained in this dot access.
    ///
    /// The returned value is the index of the segment where `pos` was found, and
    /// the span that contained it. If the Pos is inside the variable the index is None,
    /// otherwise the index inside the attributes.
    pub(crate) fn contains(&self, pos: Pos) -> Option<(Option<usize>, Span)> {
        if self.variable.span.contains(pos) {
            return Some((None, self.variable.span));
        }
        for (i, s) in self.attributes.iter().enumerate() {
            if s.span.contains(pos) {
                return Some((Some(i), s.span));
            }
        }
        None
    }
}

#[derive(Debug)]
pub(crate) struct Scope {
    pub inner: Vec<Bind>,
    pub(crate) free: HashMap<String, Span>, // Things referred to in this scope, or inner scopes, that we don't define
    pub(crate) bound: HashMap<String, (Assigner, Span)>, // Things bound in this scope, doesn't include inner scope bindings
}

impl Scope {
    fn new(inner: Vec<Bind>) -> Self {
        let mut bound: HashMap<String, _> = HashMap::new();
        let mut free: HashMap<String, _> = HashMap::new();
        for x in &inner {
            match x {
                Bind::Set(assigner, x) => {
                    bound
                        .entry(x.0.clone())
                        .or_insert((assigner.clone(), x.span));
                }
                Bind::Get(x) => {
                    free.entry(x.node.clone()).or_insert(x.span);
                }
                Bind::GetDotted(x) => {
                    free.entry(x.variable.node.clone())
                        .or_insert(x.variable.span);
                }
                Bind::Scope(scope) => scope.free.iter().for_each(|(k, v)| {
                    free.entry(k.clone()).or_insert(*v);
                }),
                Bind::Flow => {}
            }
        }
        for x in bound.keys() {
            free.remove(x);
        }

        Self { inner, free, bound }
    }
}

fn opt_expr(x: Option<&AstExpr>, res: &mut Vec<Bind>) {
    if let Some(x) = x {
        expr(x, res)
    }
}

fn comprehension(
    for_: &ForClause,
    clauses: &[Clause],
    res: &mut Vec<Bind>,
    end: impl Fn(&mut Vec<Bind>),
) {
    expr(&for_.over, res);
    let mut inner = Vec::new();
    expr_lvalue(&for_.var, &mut inner);
    for clause in clauses {
        match clause {
            Clause::For(ForClause { var, over }) => {
                expr(over, &mut inner);
                expr_lvalue(var, &mut inner);
            }
            Clause::If(x) => expr(x, &mut inner),
        }
    }
    end(&mut inner);
    res.push(Bind::Scope(Scope::new(inner)))
}

/// Can we interpret this as a Dotted expression? If not, just treat it normally.
/// Note that we treat `foo.bar().baz` like the dotted expression `foo.bar.baz`.
fn dot_access<'a>(lhs: &'a AstExpr, attribute: &'a AstString, res: &mut Vec<Bind>) {
    // The attributes are stored in reverse.
    // Keep recursing while we have a chain of dots, if we find something else bail out
    fn f<'a>(lhs: &'a AstExpr, mut attributes: Vec<&'a AstString>, res: &mut Vec<Bind>) {
        match &**lhs {
            Expr::Identifier(id, _) => {
                let attrs = attributes.into_iter().rev().cloned().collect();
                res.push(Bind::GetDotted(GetDotted::new(id.clone(), attrs)));
            }
            Expr::Dot(lhs, attribute) => {
                attributes.push(attribute);
                f(lhs, attributes, res);
            }
            Expr::Call(name, parameters) => {
                f(name, attributes, res);
                // make sure that if someone does a(b).c, 'b' is bound and considered used.
                for parameter in parameters {
                    expr(parameter.expr(), res);
                }
            }
            _ => {
                // We saw through a series of dots, but it isn't a dotted-identifier
                // Bail out and just deal with the LHS normally
                expr(lhs, res);
            }
        }
    }
    f(lhs, vec![attribute], res);
}

fn expr(x: &AstExpr, res: &mut Vec<Bind>) {
    match &**x {
        Expr::Identifier(x, _) => res.push(Bind::Get(x.clone())),
        Expr::Lambda(LambdaP {
            params,
            body,
            payload: _,
        }) => {
            let mut inner = Vec::new();
            parameters(params, res, &mut inner);
            expr(body, &mut inner);
            res.push(Bind::Scope(Scope::new(inner)));
        }
        Expr::Dot(lhs, attribute) => dot_access(lhs, attribute, res),
        Expr::ListComprehension(x, for_, clauses) => {
            comprehension(for_, clauses, res, |res| expr(x, res))
        }
        Expr::DictComprehension(x, for_, clauses) => comprehension(for_, clauses, res, |res| {
            expr(&x.0, res);
            expr(&x.1, res)
        }),

        // Uninteresting - just recurse
        _ => x.visit_expr(|x| expr(x, res)),
    }
}

fn expr_lvalue(x: &AstAssign, res: &mut Vec<Bind>) {
    x.visit_expr(|x| expr(x, res));
    x.visit_lvalue(|x| res.push(Bind::Set(Assigner::Assign, x.clone())))
}

fn parameters(args: &[AstParameter], res: &mut Vec<Bind>, inner: &mut Vec<Bind>) {
    for a in args {
        let (name, typ, default) = a.split();
        opt_expr(typ, res);
        opt_expr(default, res);
        if let Some(name) = name {
            inner.push(Bind::Set(Assigner::Argument, name.clone()))
        }
    }
}

fn flow(res: &mut Vec<Bind>) {
    res.push(Bind::Flow)
}

fn stmt(x: &AstStmt, res: &mut Vec<Bind>) {
    match &**x {
        Stmt::Statements(xs) => {
            for x in xs {
                stmt(x, res)
            }
        }
        Stmt::Break | Stmt::Continue | Stmt::Return(None) => flow(res),
        Stmt::Pass => {}
        Stmt::Return(Some(x)) => {
            expr(x, res);
            flow(res)
        }
        Stmt::Expression(x) => expr(x, res),
        Stmt::If(a, b) => {
            expr(a, res);
            flow(res);
            stmt(b, res);
            flow(res);
        }
        Stmt::IfElse(a, b_c) => {
            let (b, c) = &**b_c;
            expr(a, res);
            flow(res);
            stmt(b, res);
            flow(res);
            stmt(c, res);
            flow(res);
        }
        Stmt::Def(DefP {
            name,
            params,
            return_type,
            body,
            payload: _,
        }) => {
            opt_expr(return_type.as_ref().map(|x| &**x), res);
            let mut inner = Vec::new();
            parameters(params, res, &mut inner);
            res.push(Bind::Set(Assigner::Assign, name.clone()));
            stmt(body, &mut inner);
            res.push(Bind::Scope(Scope::new(inner)));
        }
        Stmt::Assign(lhs, ty_rhs) => {
            let (ty, rhs) = &**ty_rhs;
            opt_expr(ty.as_ref(), res);
            expr(rhs, res);
            expr_lvalue(lhs, res);
        }
        Stmt::AssignModify(lhs, _, rhs) => {
            // For a += b, we:
            // 1. Evaluate all variables and expressions in a.
            // 2. Evaluate b.
            // 3. Assign to all variables in a.
            lhs.visit_expr(|x| expr(x, res));
            lhs.visit_lvalue(|x| res.push(Bind::Get(x.clone().into_map(|AssignIdentP(s, ())| s))));
            expr(rhs, res);
            expr_lvalue(lhs, res);
        }
        Stmt::For(dest, inner_body) => {
            let (inner, body) = &**inner_body;
            expr(inner, res);
            expr_lvalue(dest, res);
            flow(res);
            stmt(body, res);
            flow(res)
        }
        Stmt::Load(load) => {
            for x in &load.args {
                res.push(Bind::Set(
                    Assigner::Load {
                        path: load.module.clone(),
                        name: x.1.clone(),
                    },
                    x.0.clone(),
                ))
            }
        }
    }
}

pub(crate) fn scope(module: &AstModule) -> Scope {
    let mut res = Vec::new();
    stmt(&module.statement, &mut res);
    Scope::new(res)
}

#[cfg(test)]
mod test {
    use std::iter;

    use gazebo::prelude::VecExt;

    use super::*;
    use crate::codemap::Pos;
    use crate::codemap::Span;
    use crate::syntax::AstModule;
    use crate::syntax::Dialect;

    #[test]
    fn dotted_access_is_correct() -> anyhow::Result<()> {
        let contents = "x1.y\nx1.y().z\nx2().y\nx2().y.z";

        let expected = vec![
            vec!["x1", "y"],
            vec!["x1", "y", "z"],
            vec!["x2", "y"],
            vec!["x2", "y", "z"],
        ]
        .into_map(|names| names.into_map(String::from));

        let module = AstModule::parse("foo.star", contents.to_owned(), &Dialect::Extended)?;
        let scope = scope(&module);

        let found_bindings = scope
            .inner
            .iter()
            .map(|b| match b {
                Bind::GetDotted(get) => Ok(iter::once(&get.variable)
                    .chain(&get.attributes)
                    .map(|s| s.node.to_owned())
                    .collect()),
                _ => Err(anyhow::anyhow!("Unexpected bind {:?}", b)),
            })
            .collect::<anyhow::Result<Vec<Vec<_>>>>()?;

        assert_eq!(expected, found_bindings);

        Ok(())
    }

    #[test]
    fn dotted_contains_is_correct() -> anyhow::Result<()> {
        let contents = "x1.y1.z1\nx2.y2.z2";
        let module = AstModule::parse("foo.star", contents.to_owned(), &Dialect::Extended)?;
        let scope = scope(&module);

        let get = scope
            .inner
            .iter()
            .find_map(|b| match b {
                Bind::GetDotted(get) => Some((*get).clone()),
                _ => None,
            })
            .unwrap();

        let x1_pos = Pos::new(1);
        let y1_pos = Pos::new(4);
        let z1_pos = Pos::new(7);
        let x2_pos = Pos::new(10);

        let x1_expected = Some((None, Span::new(Pos::new(0), Pos::new(2))));
        let y1_expected = Some((Some(0), Span::new(Pos::new(3), Pos::new(5))));
        let z1_expected = Some((Some(1), Span::new(Pos::new(6), Pos::new(8))));

        assert_eq!(x1_expected, get.contains(x1_pos));
        assert_eq!(y1_expected, get.contains(y1_pos));
        assert_eq!(z1_expected, get.contains(z1_pos));
        assert!(get.contains(x2_pos).is_none());

        Ok(())
    }
}
