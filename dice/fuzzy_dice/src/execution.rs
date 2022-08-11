/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fs;
use std::fs::File;
use std::panic;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;

use crossbeam::queue::SegQueue;
use dice::cycles::DetectCycles;
use dice::introspection::serialize_dense_graph;
use dice::Dice;
use dice::DiceTransaction;
use futures::FutureExt;
use gazebo::prelude::*;
use itertools::Itertools;
use once_cell::sync::OnceCell;
use quickcheck::Arbitrary;
use quickcheck::Gen;
use quickcheck::TestResult;
use serde::Deserialize;
use serde::Serialize;
use serde::Serializer;
use uuid::Uuid;

use crate::computation::ComputationStep;
use crate::computation::Expr;
use crate::computation::FuzzEquations;
use crate::computation::FuzzMath;
use crate::computation::FuzzState;
use crate::computation::Unit;
use crate::computation::Var;

/// For a given DiceExecutionOrder that includes some Queries, computes the
/// expected result of each of those queries.
/// A result of None indicates the computation is expected to fail or panic.
struct MathAnswerKey {
    values_by_ctx_id: HashMap<usize, HashMap<Var, Option<bool>>>,
}

impl MathAnswerKey {
    pub fn new(order: &DiceExecutionOrder) -> Self {
        // Pre-process the execution order to see which computations will be requested.
        let queried_vars_by_ctx_id = Self::queried_vars_by_ctx_id(order);
        // Maintain the current set of exprs.
        let mut exprs = HashMap::new();
        let mut values_by_ctx_id: HashMap<usize, HashMap<Var, Option<bool>>> = HashMap::new();
        for op in order.init_vars.iter().chain(order.timeline.iter()) {
            match op {
                Operation::SetValue {
                    new_ctx_id,
                    var,
                    expr,
                } => {
                    exprs.insert(*var, expr.clone());
                    if let Some(queried_vars) = queried_vars_by_ctx_id.get(new_ctx_id) {
                        values_by_ctx_id.insert(
                            *new_ctx_id,
                            queried_vars
                                .iter()
                                .map(|v| (*v, Self::eval(*v, &exprs)))
                                .collect(),
                        );
                    }
                }
                Operation::Query { .. } => (),
                Operation::EnqueueStep(..) => (),
            }
        }
        Self { values_by_ctx_id }
    }

    pub fn value_of_at_ctx(&self, ctx_id: usize, var: Var) -> Option<bool> {
        self.values_by_ctx_id
            .get(&ctx_id)
            .and_then(|map| map.get(&var))
            .copied()
            .unwrap_or(None)
    }

    fn queried_vars_by_ctx_id(order: &DiceExecutionOrder) -> HashMap<usize, Vec<Var>> {
        let mut queried_vars_by_ctx_id: HashMap<usize, Vec<Var>> = HashMap::new();
        for op in order.timeline.iter() {
            if let Operation::Query { ctx_id, var } = op {
                queried_vars_by_ctx_id
                    .entry(*ctx_id)
                    .or_insert_with(std::vec::Vec::new)
                    .push(*var);
            }
        }
        queried_vars_by_ctx_id
    }

    fn resolve_unit(unit: &Unit, exprs: &HashMap<Var, Expr>) -> Option<bool> {
        match unit {
            Unit::Variable(var) => Self::eval(*var, exprs),
            Unit::Literal(lit) => Some(*lit),
        }
    }

    fn eval(var: Var, exprs: &HashMap<Var, Expr>) -> Option<bool> {
        exprs.get(&var).and_then(|expr| match expr {
            Expr::Unit(unit) => Self::resolve_unit(unit, exprs),
            Expr::Cond {
                test,
                then,
                otherwise,
            } => {
                if Self::resolve_unit(test, exprs)? {
                    Self::resolve_unit(then, exprs)
                } else {
                    Self::resolve_unit(otherwise, exprs)
                }
            }
            Expr::Xor(vars) => vars
                .iter()
                .map(|x| Self::resolve_unit(x, exprs))
                .collect::<Option<Vec<_>>>()
                .map(|bools| bools.into_iter().reduce(|x, y| x ^ y).unwrap_or(false)),
        })
    }
}

enum ExecutionResult {
    Correct,
    IncorrectResult { expected: bool, actual: bool },
    ExpectedPanic(bool),
    UnexpectedPanic(String),
}

impl Display for ExecutionResult {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ExecutionResult::Correct => {
                write!(f, "Correct")
            }
            ExecutionResult::IncorrectResult { expected, actual } => {
                write!(f, "Expected `{}` but got `{}`", expected, actual)
            }
            ExecutionResult::ExpectedPanic(res) => {
                write!(
                    f,
                    "Expected panic from injected key missing but got `{}`",
                    res
                )
            }
            ExecutionResult::UnexpectedPanic(p) => {
                write!(f, "Expected result but panicked `{}`", p)
            }
        }
    }
}

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

fn uuid_serializer<S>(uuid: &Uuid, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    serializer.serialize_str(&uuid.to_string())
}

#[derive(Clone, Serialize, Deserialize)]
pub struct DiceExecutionOrder {
    #[serde(skip_deserializing, serialize_with = "uuid_serializer")]
    pub uuid: Uuid,
    /// A list of operations that initialize each var, in order, to a literal.
    init_vars: Vec<Operation>,
    /// A list of updates, queries, and enqueued "steps" that tweak computation.
    /// Notably, this doesn't introduce new vars, to simplify the shrink() implementation.
    timeline: Vec<Operation>,
    /// Are we shrinking an already-found failure? See comment in |execute|.
    is_shrinking: bool,
    /// The first failure we encountered
    #[serde(skip)]
    first_failure: Arc<Mutex<Option<Arc<ExecutionResult>>>>,
}

impl Debug for DiceExecutionOrder {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", serde_json::to_string_pretty(self).unwrap())
    }
}

pub struct DiceExecutionOrderOptions {
    pub print_dumps: Option<PathBuf>,
}

static ALL_PANICS: OnceCell<Mutex<Vec<String>>> = OnceCell::new();

impl DiceExecutionOrder {
    const NSAMPLES_SEARCHING: usize = 1;
    const NSAMPLES_SHRINKING: usize = 100;
    const AVG_OPS_PER_VAR: usize = 4;
    const VARS_PER_XOR: usize = 5;

    fn make_failure(&self, msg: String) -> TestResult {
        TestResult::error(format!("Execution: {:?}\nFailure: `{}`", self, msg))
    }

    /// If a repro is nondeterministic, shrunk testcases that contain the
    /// same bug may not repro, so shrinking might stop early.
    /// Since shrinking a big testcase takes multiple consecutive failures,
    /// without multiple samples, shrinking will usually stop early.
    pub async fn execute(&self, options: &DiceExecutionOrderOptions) -> TestResult {
        let answer_key = MathAnswerKey::new(self);
        let ntimes = if self.is_shrinking {
            Self::NSAMPLES_SHRINKING
        } else {
            Self::NSAMPLES_SEARCHING
        };

        let mut ignored_failure = vec![];
        for run_count in 0..ntimes {
            match self.execute_once(&answer_key, run_count, options).await {
                ExecutionResult::Correct => {}
                ExecutionResult::ExpectedPanic(r) => {
                    let mut first_failure = self.first_failure.lock().unwrap();
                    match &*first_failure {
                        None => {
                            *first_failure = Some(Arc::new(ExecutionResult::ExpectedPanic(r)));
                            return self.make_failure(first_failure.as_ref().unwrap().to_string());
                        }
                        Some(prev) => {
                            match &**prev {
                                ExecutionResult::ExpectedPanic(prev_r) if *prev_r == r => {
                                    return self.make_failure(prev.to_string());
                                }
                                _ => {
                                    // keep going because the error doesn't match
                                    ignored_failure.push(ExecutionResult::ExpectedPanic(r))
                                }
                            }
                        }
                    }
                }
                ExecutionResult::IncorrectResult { expected, actual } => {
                    let mut first_failure = self.first_failure.lock().unwrap();
                    match &*first_failure {
                        None => {
                            *first_failure = Some(Arc::new(ExecutionResult::IncorrectResult {
                                expected,
                                actual,
                            }));
                            return self.make_failure(first_failure.as_ref().unwrap().to_string());
                        }
                        Some(prev) => match &**prev {
                            ExecutionResult::IncorrectResult {
                                expected: prev_expected,
                                actual: prev_actual,
                            } if *prev_expected == expected && *prev_actual == actual => {
                                return self.make_failure(prev.to_string());
                            }
                            _ => ignored_failure
                                .push(ExecutionResult::IncorrectResult { expected, actual }),
                        },
                    }
                }
                ExecutionResult::UnexpectedPanic(p) => {
                    let mut first_failure = self.first_failure.lock().unwrap();
                    match &*first_failure {
                        None => {
                            *first_failure = Some(Arc::new(ExecutionResult::UnexpectedPanic(p)));
                            return self.make_failure(first_failure.as_ref().unwrap().to_string());
                        }
                        Some(prev) => {
                            match &**prev {
                                ExecutionResult::UnexpectedPanic(_) => {
                                    return TestResult::error(prev.to_string());
                                }
                                _ => {
                                    // keep going because the error doesn't match
                                    ignored_failure.push(ExecutionResult::UnexpectedPanic(p))
                                }
                            }
                        }
                    }
                }
            }

            if let Some(dump_loc) = self.get_dump_dir(options) {
                // we don't keep dump of any runs that are not a failure
                fs::remove_dir_all(dump_loc.join(&format!("run-{}", run_count)))
                    .expect("failed to remove dump");
            }
        }

        if self.first_failure.lock().unwrap().is_some() && !ignored_failure.is_empty() {
            // we were supposed to find a particular failure but we didn't.
            eprintln!(
                "warning: supposed to find a specific error `{}` but didn't find any",
                self.first_failure.lock().unwrap().as_ref().unwrap()
            );
            return TestResult::discard();
        }

        TestResult::passed()
    }

    async fn execute_once(
        &self,
        answer_key: &MathAnswerKey,
        run_count: usize,
        options: &DiceExecutionOrderOptions,
    ) -> ExecutionResult {
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
        for (step_count, op) in self
            .init_vars
            .iter()
            .chain(self.timeline.iter())
            .enumerate()
        {
            match &op {
                Operation::Query { ctx_id, var } => {
                    if let Some(ctx) = dice_ctxs.get(ctx_id) {
                        let expected = answer_key.value_of_at_ctx(*ctx_id, *var);

                        // quickcheck runs us single threaded, so this is safe to do.
                        let old_handler = panic::take_hook();

                        ALL_PANICS
                            .get_or_init(Default::default)
                            .lock()
                            .unwrap()
                            .clear();
                        // suppress print panics from dice.
                        panic::set_hook(Box::new(|info| {
                            ALL_PANICS.get().unwrap().lock().unwrap().push(
                                if let Some(s) = info.payload().downcast_ref::<&str>() {
                                    (*s).to_owned()
                                } else if let Some(s) = info.payload().downcast_ref::<String>() {
                                    s.clone()
                                } else {
                                    "unknown".to_owned()
                                },
                            )
                            // do nothing. don't print because we have panics that we expect.
                            // they get spammed on the console such that the info we do print
                            // gets washed away. Hiding the panics makes the information we do
                            // print much more useful.
                        }));

                        let exec_result = std::panic::AssertUnwindSafe(async {
                            ctx.eval(state.dupe(), *var).await.expect("eval errored")
                        })
                        .catch_unwind()
                        .await;

                        panic::set_hook(old_handler);

                        if exec_result.is_ok()
                            && !ALL_PANICS.get().unwrap().lock().unwrap().is_empty()
                        {
                            // we panicked on a thread that wasn't joined.
                            // This is still wrong.
                            return ExecutionResult::UnexpectedPanic(
                                ALL_PANICS
                                    .get()
                                    .unwrap()
                                    .lock()
                                    .unwrap()
                                    .iter()
                                    .map(|panic| format!("panic: `{}`", panic))
                                    .join("\n"),
                            );
                        }

                        match (expected, exec_result) {
                            (Some(expected), Ok(result)) => {
                                if expected != result {
                                    return ExecutionResult::IncorrectResult {
                                        expected,
                                        actual: result,
                                    };
                                }
                            }
                            (Some(_), Err(panic)) => {
                                if std::env::var("ALLOW_INCORRECT_WHEN_SHOULD_PANIC").is_err() {
                                    // There's a known bug where DICE sometimes uses injected keys from versions prior to the current query.
                                    // These computations should panic, but don't, and the fuzzer keeps finding them.
                                    // This environment variable disables this class of bugs, so other errors are easier to find.
                                    return ExecutionResult::UnexpectedPanic(
                                        if let Some(s) = panic.downcast_ref::<&str>() {
                                            (*s).to_owned()
                                        } else if let Some(info) = panic.downcast_ref::<String>() {
                                            info.clone()
                                        } else {
                                            "unknown panic".to_owned()
                                        },
                                    );
                                }
                            }
                            (None, Err(_panic)) => {
                                // TODO maybe check the type of panic
                                if std::env::var("ALLOW_EXTRA_PANICS").is_err() {
                                    let panics = ALL_PANICS
                                        .get()
                                        .unwrap()
                                        .lock()
                                        .unwrap()
                                        .iter()
                                        .filter(|panic| !panic.contains("spawned task cancelled"))
                                        .cloned()
                                        .collect::<Vec<_>>();

                                    if panics.len() > 1 {
                                        // we panicked on a thread that wasn't joined.
                                        // This is still wrong.
                                        return ExecutionResult::UnexpectedPanic(
                                            panics
                                                .iter()
                                                .map(|panic| format!("panic: `{}`", panic))
                                                .join("\n"),
                                        );
                                    }
                                }
                            }
                            (None, Ok(result)) => {
                                return ExecutionResult::ExpectedPanic(result);
                            }
                        }
                    }
                }
                Operation::SetValue {
                    new_ctx_id,
                    var,
                    expr,
                } => {
                    let ctx = dice.ctx();
                    ctx.set_equation(*var, expr.clone()).unwrap();
                    dice_ctxs.insert(*new_ctx_id, ctx.commit());
                }
                Operation::EnqueueStep(var, steps) => {
                    let queue = state.steps.get(var).unwrap();
                    for step in steps {
                        queue.push(*step);
                    }
                }
            }

            self.maybe_dump_dice(options, run_count, step_count, &dice)
                .expect("couldn't dump DICE to disk");
        }

        ExecutionResult::Correct
    }

    pub fn get_dump_dir(&self, options: &DiceExecutionOrderOptions) -> Option<PathBuf> {
        options
            .print_dumps
            .as_ref()
            .map(|loc| loc.join(self.uuid.to_string()))
    }

    fn maybe_dump_dice(
        &self,
        options: &DiceExecutionOrderOptions,
        run_count: usize,
        step_count: usize,
        dice: &Arc<Dice>,
    ) -> anyhow::Result<()> {
        if let Some(loc) = self.get_dump_dir(options) {
            let dump_path = loc
                .join(&format!("run-{}", run_count))
                .join(&format!("step-{}", step_count));

            fs::create_dir_all(dump_path.parent().unwrap())?;
            let mut dump_loc = File::create(&dump_path)?;
            serialize_dense_graph(
                &dice.to_introspectable(),
                &mut serde_json::Serializer::pretty(&mut dump_loc),
            )?;
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

        // There's a known bug in DICE involving transient values resulting in incorrect
        // computation. This environment variable disables the fuzzer searching for these
        // bugs, so other errors are easier to find.
        let gen_transients = std::env::var("NOGEN_TRANSIENTS").is_err();

        for _ in 0..Self::AVG_OPS_PER_VAR * g.size() {
            let i = usize::arbitrary(g) % active_vars.len();
            // Semi-randomly select a next op.
            timeline.push(match usize::arbitrary(g) % 100 {
                0..40 => Operation::Query {
                    ctx_id: *g.choose(&ctx_ids).unwrap(),
                    var: active_vars[i],
                },
                41..50 if gen_transients => Operation::EnqueueStep(
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
            uuid: Uuid::new_v4(),
            is_shrinking: false,
            init_vars,
            timeline,
            first_failure: Arc::new(Mutex::new(None)),
        }
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
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
            uuid: Uuid::new_v4(),
            is_shrinking: true,
            init_vars: self.seed.init_vars.clone(),
            timeline: [
                &self.seed.timeline[0..self.pos],
                &self.seed.timeline[self.pos + 1..],
            ]
            .concat(),
            first_failure: self.seed.first_failure.dupe(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn answer_key() {
        let order = DiceExecutionOrder {
            uuid: Default::default(),
            init_vars: vec![],
            timeline: vec![
                Operation::SetValue {
                    new_ctx_id: 1,
                    var: Var(1),
                    expr: Expr::Unit(Unit::Literal(true)),
                },
                Operation::SetValue {
                    new_ctx_id: 2,
                    var: Var(1),
                    expr: Expr::Unit(Unit::Literal(false)),
                },
                Operation::Query {
                    ctx_id: 1,
                    var: Var(1),
                },
                Operation::Query {
                    ctx_id: 2,
                    var: Var(1),
                },
                Operation::Query {
                    ctx_id: 1,
                    var: Var(2),
                },
            ],
            is_shrinking: false,
            first_failure: Arc::new(Mutex::new(None)),
        };
        let answer_key = MathAnswerKey::new(&order);
        // Two present and well-behaved query results.
        assert_eq!(Some(true), answer_key.value_of_at_ctx(1, Var(1)));
        assert_eq!(Some(false), answer_key.value_of_at_ctx(2, Var(1)));
        // A query of an injected key that hasn't been set yet should be None.
        assert_eq!(None, answer_key.value_of_at_ctx(1, Var(2)));
        // A never-queried result should be None.
        assert_eq!(None, answer_key.value_of_at_ctx(2, Var(2)));
    }
}
