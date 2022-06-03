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

//! List/dict/set comprenension evaluation.

use gazebo::prelude::*;

use crate::{
    eval::compiler::{
        expr::ExprCompiled,
        expr_bool::ExprCompiledBool,
        known::list_to_tuple,
        scope::{CstExpr, CstPayload},
        span::IrSpanned,
        stmt::{AssignCompiledValue, OptimizeOnFreezeContext},
        Compiler,
    },
    syntax::ast::{ClauseP, ForClauseP},
};

impl Compiler<'_, '_, '_> {
    pub fn list_comprehension(
        &mut self,
        x: CstExpr,
        for_: ForClauseP<CstPayload>,
        clauses: Vec<ClauseP<CstPayload>>,
    ) -> ExprCompiled {
        let clauses = self.compile_clauses(for_, clauses);
        let x = self.expr(x);
        ExprCompiled::compr(ComprCompiled::List(box x, clauses))
    }

    pub fn dict_comprehension(
        &mut self,
        k: CstExpr,
        v: CstExpr,
        for_: ForClauseP<CstPayload>,
        clauses: Vec<ClauseP<CstPayload>>,
    ) -> ExprCompiled {
        let clauses = self.compile_clauses(for_, clauses);
        let k = self.expr(k);
        let v = self.expr(v);
        ExprCompiled::compr(ComprCompiled::Dict(box (k, v), clauses))
    }

    /// Peel the final if's from clauses, and return them (in the order they started), plus the next for you get to
    fn compile_ifs(
        &mut self,
        clauses: &mut Vec<ClauseP<CstPayload>>,
    ) -> (Option<ForClauseP<CstPayload>>, Vec<IrSpanned<ExprCompiled>>) {
        let mut ifs = Vec::new();
        while let Some(x) = clauses.pop() {
            match x {
                ClauseP::For(f) => {
                    ifs.reverse();
                    return (Some(f), ifs);
                }
                ClauseP::If(x) => {
                    let x = self.expr_truth(x);
                    if let ExprCompiledBool::Const(true) = &x.node {
                        // If the condition is always true, skip the clause.
                        continue;
                    }
                    // TODO(nga): if condition is false, do something better.
                    ifs.push(x.into_expr());
                }
            }
        }
        ifs.reverse();
        (None, ifs)
    }

    fn compile_clauses(
        &mut self,
        for_: ForClauseP<CstPayload>,
        mut clauses: Vec<ClauseP<CstPayload>>,
    ) -> ClausesCompiled {
        // The first for.over is scoped before we enter the list comp
        let over = self.expr(list_to_tuple(for_.over));

        // Now we want to group them into a `for`, followed by any number of `if`.
        // The evaluator wants to use pop to consume them, so reverse the order.
        let mut res = Vec::new();
        loop {
            let (next_for, ifs) = self.compile_ifs(&mut clauses);
            match next_for {
                None => {
                    let last = ClauseCompiled {
                        var: self.assign(for_.var),
                        over,
                        ifs,
                    };
                    return ClausesCompiled::new(res, last);
                }
                Some(f) => {
                    res.push(ClauseCompiled {
                        over: self.expr(f.over),
                        var: self.assign(f.var),
                        ifs,
                    });
                }
            }
        }
    }
}

#[derive(Clone, Debug, VisitSpanMut)]
pub(crate) enum ComprCompiled {
    List(Box<IrSpanned<ExprCompiled>>, ClausesCompiled),
    Dict(
        Box<(IrSpanned<ExprCompiled>, IrSpanned<ExprCompiled>)>,
        ClausesCompiled,
    ),
}

impl ComprCompiled {
    pub(crate) fn optimize_on_freeze(&self, ctx: &OptimizeOnFreezeContext) -> ExprCompiled {
        match self {
            ComprCompiled::List(box ref x, ref clauses) => {
                let clauses = clauses.optimize_on_freeze(ctx);
                ExprCompiled::compr(ComprCompiled::List(box x.optimize_on_freeze(ctx), clauses))
            }
            ComprCompiled::Dict(box (ref k, ref v), ref clauses) => {
                let clauses = clauses.optimize_on_freeze(ctx);
                ExprCompiled::compr(ComprCompiled::Dict(
                    box (k.optimize_on_freeze(ctx), v.optimize_on_freeze(ctx)),
                    clauses.optimize_on_freeze(ctx),
                ))
            }
        }
    }
}

#[derive(Clone, Debug, VisitSpanMut)]
pub(crate) struct ClauseCompiled {
    pub(crate) var: IrSpanned<AssignCompiledValue>,
    pub(crate) over: IrSpanned<ExprCompiled>,
    pub(crate) ifs: Vec<IrSpanned<ExprCompiled>>,
}

impl ClauseCompiled {
    fn optimize_on_freeze(&self, ctx: &OptimizeOnFreezeContext) -> ClauseCompiled {
        let ClauseCompiled {
            ref var,
            ref over,
            ref ifs,
        } = *self;
        ClauseCompiled {
            var: var.optimize_on_freeze(ctx),
            over: over.optimize_on_freeze(ctx),
            ifs: ifs
                .iter()
                .filter_map(|e| {
                    let e = e.optimize_on_freeze(ctx);
                    let e = ExprCompiledBool::new(e);
                    match &e.node {
                        ExprCompiledBool::Const(true) => None,
                        _ => Some(e.into_expr()),
                    }
                })
                .collect(),
        }
    }
}

/// All clauses in a comprehension. Never empty.
#[derive(Clone, Debug, VisitSpanMut)]
pub(crate) struct ClausesCompiled {
    /// Not empty.
    ///
    /// Clauses are in reverse order, i. e. the first executed clause is the last in the list.
    clauses: Vec<ClauseCompiled>,
}

impl ClausesCompiled {
    fn new(clauses: Vec<ClauseCompiled>, last: ClauseCompiled) -> ClausesCompiled {
        let mut clauses = clauses;
        clauses.push(last);
        ClausesCompiled { clauses }
    }

    /// Clauses are definitely no-op, i. e. zero iterations, and no side effects of iteration.
    pub(crate) fn is_nop(&self) -> bool {
        // NOTE(nga): if the first loop argument is empty collection, clauses are definitely no-op.
        //   But this is not true for the rest of loops: if inner loop collection is empty,
        //   clauses produce no iterations, but the outer loop may still has side effects.
        //   There are missing optimizations here:
        //   * we could separate effects and emit empty list/dict.
        //   * or at least do not generate comprehension terminator.
        self.split_last().0.over.is_iterable_empty()
    }

    pub(crate) fn split_last(&self) -> (&ClauseCompiled, &[ClauseCompiled]) {
        self.clauses.split_last().unwrap()
    }

    fn optimize_on_freeze(&self, ctx: &OptimizeOnFreezeContext) -> ClausesCompiled {
        ClausesCompiled {
            clauses: self.clauses.map(|c| c.optimize_on_freeze(ctx)),
        }
    }
}
