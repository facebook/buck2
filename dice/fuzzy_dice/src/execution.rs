/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    collections::HashMap,
    fmt::{Debug, Formatter},
    io::{stderr, Write},
    sync::Arc,
};

use crossbeam::queue::SegQueue;
use dice::{cycles::DetectCycles, Dice, DiceTransaction};
use gazebo::prelude::*;
use quickcheck::{Arbitrary, Gen};
use serde::{Deserialize, Serialize};

use crate::computation::{ComputationStep, Expr, FuzzEquations, FuzzMath, FuzzState, Unit, Var};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Operation {
    /// Evaluate the variable at the version immediately after
    /// the SetValue op with this ctx_id.
    Query { ctx_id: usize, var: Var },
    /// Set the injected key corresponding to this variable.
    SetValue {
        new_ctx_id: usize,
        var: Var,
        expr: Expr,
    },
    /// Force the next evaluation of this var to do something other than just
    /// returning the correct result (e.g. force DICE to treat the value as
    /// transient).
    /// For now, this can't change the boolean result of the eval, since
    /// that'll break our ability to check DICE's computations are correct.
    EnqueueStep(Var, Vec<ComputationStep>),
}

#[derive(Clone, Serialize, Deserialize)]
pub struct DiceExecutionOrder {
    /// A list of operations that initialize each var, in order, to a literal.
    init_vars: Vec<Operation>,
    /// A list of updates, queries, and enqueued "steps" that tweak computation.
    /// Notably, this doesn't introduce new vars, to simplify the shrink() implementation.
    timeline: Vec<Operation>,
    /// Are we shrinking an already-found failure? See comment in |execute|.
    is_shrinking: bool,
}

impl Debug for DiceExecutionOrder {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", serde_json::to_string(self).unwrap())
    }
}

pub struct DiceExecutionOrderOptions {
    pub print_dumps: bool,
}

impl DiceExecutionOrder {
    const NSAMPLES_SEARCHING: usize = 1;
    const NSAMPLES_SHRINKING: usize = 100;
    const AVG_OPS_PER_VAR: usize = 4;
    const VARS_PER_XOR: usize = 5;

    /// If a repro is nondeterministic, shrunk testcases that contain the
    /// same bug may not repro, so shrinking might stop early.
    /// Since shrinking a big testcase takes multiple consecutive failures,
    /// without multiple samples, shrinking will usually stop early.
    pub async fn execute(&self, options: &DiceExecutionOrderOptions) -> anyhow::Result<()> {
        let ntimes = if self.is_shrinking {
            Self::NSAMPLES_SHRINKING
        } else {
            Self::NSAMPLES_SEARCHING
        };
        for _ in 0..ntimes {
            self.execute_once(options).await?;
        }
        Ok(())
    }

    async fn execute_once(&self, options: &DiceExecutionOrderOptions) -> anyhow::Result<()> {
        let dice = Dice::builder().build(DetectCycles::Disabled);
        let mut dice_ctxs: HashMap<usize, DiceTransaction> = HashMap::new();
        let state = {
            let mut state = FuzzState::new();
            for var in self.init_vars.iter().map(|op| match &op {
                Operation::Query { var, .. } => var,
                Operation::SetValue { var, .. } => var,
                Operation::EnqueueStep(var, _) => var,
            }) {
                state.steps.entry(*var).or_insert_with(SegQueue::new);
            }
            Arc::new(state)
        };
        for op in self.init_vars.iter().chain(self.timeline.iter()) {
            match &op {
                Operation::Query { ctx_id, var } => {
                    if let Some(ctx) = dice_ctxs.get(ctx_id) {
                        ctx.eval(state.dupe(), *var).await;
                    }
                }
                Operation::SetValue {
                    new_ctx_id,
                    var,
                    expr,
                } => {
                    let ctx = dice.ctx();
                    ctx.set_equation(*var, expr.clone());
                    dice_ctxs.insert(*new_ctx_id, ctx.commit());
                }
                Operation::EnqueueStep(var, steps) => {
                    let queue = state.steps.get(var).unwrap();
                    for step in steps {
                        queue.push(*step);
                    }
                }
            }
            Self::maybe_dump_dice(options, &dice).expect("couldn't dump DICE to stderr");
        }
        Ok(())
    }

    fn maybe_dump_dice(
        options: &DiceExecutionOrderOptions,
        dice: &Arc<Dice>,
    ) -> anyhow::Result<()> {
        if options.print_dumps {
            let mut stderr = stderr();
            serde_json::to_writer(&mut stderr, dice.as_ref())?;
            writeln!(stderr)?;
        }
        Ok(())
    }
}

impl Arbitrary for DiceExecutionOrder {
    fn arbitrary(g: &mut Gen) -> Self {
        let mut last_used_var = 0;
        let mut last_ctx_id = 0;
        let mut ctx_ids = vec![];
        let mut active_vars = vec![];
        let mut init_vars = vec![];
        let mut timeline = vec![];
        let mut var_values = Vec::<bool>::arbitrary(g);
        var_values.push(true); // Ensure non-empty.
        for val in var_values.iter() {
            last_ctx_id += 1;
            last_used_var += 1;
            ctx_ids.push(last_ctx_id);
            let var = Var(last_used_var);
            init_vars.push(Operation::SetValue {
                new_ctx_id: last_ctx_id,
                var,
                expr: Expr::Unit(Unit::Literal(*val)),
            });
            active_vars.push(var);
        }

        let select_var =
            |g: &mut Gen, vars: &[Var]| -> Unit { Unit::Variable(*g.choose(vars).unwrap()) };

        let arbitrary_expr = |g: &mut Gen, vars: &[Var]| -> Expr {
            if vars.is_empty() {
                return Expr::Unit(Unit::Literal(bool::arbitrary(g)));
            }
            // Semi-randomly select the expr type.
            match usize::arbitrary(g) % 100 {
                0..33 => Expr::Unit(Unit::Literal(bool::arbitrary(g))),
                34..66 => Expr::Xor({
                    let mut vec = Vec::new();
                    for _ in 0..Self::VARS_PER_XOR {
                        vec.push(select_var(g, vars))
                    }
                    vec
                }),
                _ => Expr::Cond {
                    test: select_var(g, vars),
                    then: select_var(g, vars),
                    otherwise: select_var(g, vars),
                },
            }
        };

        for _ in 0..Self::AVG_OPS_PER_VAR * g.size() {
            let i = usize::arbitrary(g) % active_vars.len();
            // Semi-randomly select a next op.
            timeline.push(match usize::arbitrary(g) % 100 {
                0..40 => Operation::Query {
                    ctx_id: *g.choose(&ctx_ids).unwrap(),
                    var: active_vars[i],
                },
                41..50 => Operation::EnqueueStep(
                    *g.choose(&active_vars).unwrap(),
                    vec![ComputationStep::ReturnTransient],
                ),
                _ => {
                    last_ctx_id += 1;
                    ctx_ids.push(last_ctx_id);
                    Operation::SetValue {
                        new_ctx_id: last_ctx_id,
                        var: active_vars[i],
                        // Avoid circularity by forcing a key to only depend on earlier keys.
                        expr: arbitrary_expr(g, &active_vars[0..i]),
                    }
                }
            });
        }
        DiceExecutionOrder {
            is_shrinking: false,
            init_vars,
            timeline,
        }
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        eprintln!(
            "Trying to shrink (init_vars.len()={}, timeline.len()={}): {:?}",
            self.init_vars.len(),
            self.timeline.len(),
            self
        );
        Box::new(DiceExecutionOrderShrinker::new(self.clone()))
    }
}

struct DiceExecutionOrderShrinker {
    pos: usize,
    seed: DiceExecutionOrder,
}

impl DiceExecutionOrderShrinker {
    fn new(seed: DiceExecutionOrder) -> Self {
        Self {
            pos: seed.timeline.len(),
            seed,
        }
    }
}

impl Iterator for DiceExecutionOrderShrinker {
    type Item = DiceExecutionOrder;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos == 0 {
            return None;
        }

        self.pos -= 1;
        Some(DiceExecutionOrder {
            is_shrinking: true,
            init_vars: self.seed.init_vars.clone(),
            timeline: [
                &self.seed.timeline[0..self.pos],
                &self.seed.timeline[self.pos + 1..],
            ]
            .concat(),
        })
    }
}
