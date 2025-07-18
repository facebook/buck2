/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::HashMap;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_futures::cancellation::CancellationContext;
use crossbeam::queue::SegQueue;
use derivative::Derivative;
use derive_more::Display;
use dice::DiceComputations;
use dice::DiceTransactionUpdater;
use dice::InjectedKey;
use dice::Key;
use dupe::Dupe;
use futures::FutureExt;
use futures::future;
use futures::future::BoxFuture;
use serde::Deserialize;
use serde::Serialize;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Display, Debug, Allocative)]
#[derive(Serialize, Deserialize)]
#[display("key{}", _0)]
#[serde(transparent)]
pub struct Var(pub usize);

#[derive(Clone, PartialEq, Eq, Hash, Debug, Serialize, Deserialize, Allocative)]
pub enum Unit {
    Variable(Var),
    Literal(bool),
}

async fn resolve_units<'a>(
    ctx: &'a mut DiceComputations<'_>,
    units: &[Unit],
    state: Arc<FuzzState>,
) -> anyhow::Result<Vec<bool>> {
    let futs = ctx.compute_many(units.iter().map(|unit| {
        let state = state.dupe();
        DiceComputations::declare_closure(
            move |ctx: &mut DiceComputations| -> BoxFuture<Result<bool, anyhow::Error>> {
                match unit {
                    Unit::Variable(var) => ctx.eval(state, *var).boxed(),
                    Unit::Literal(lit) => futures::future::ready(Ok(*lit)).boxed(),
                }
            },
        )
    }));
    future::join_all(futs).await.into_iter().collect()
}

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize, Allocative)]
pub enum Expr {
    Unit(Unit),
    Cond {
        test: Unit,
        then: Unit,
        otherwise: Unit,
    },
    Xor(Vec<Unit>),
}

async fn lookup_unit(ctx: &mut DiceComputations<'_>, var: Var) -> anyhow::Result<Arc<Expr>> {
    Ok(ctx.compute(&LookupVar(var)).await?)
}

#[derive(Clone, Display, Debug, Eq, Hash, PartialEq, Allocative)]
#[display("Lookup({})", _0)]
struct LookupVar(Var);
impl InjectedKey for LookupVar {
    type Value = Arc<Expr>;

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

pub trait FuzzEquations {
    fn set_equation(&mut self, var: Var, expr: Expr) -> anyhow::Result<()>;
    #[allow(dead_code)]
    fn set_equations(&mut self, expr: impl IntoIterator<Item = (Var, Expr)>) -> anyhow::Result<()>;
}

impl FuzzEquations for DiceTransactionUpdater {
    fn set_equation(&mut self, var: Var, expr: Expr) -> anyhow::Result<()> {
        Ok(self.changed_to(vec![(LookupVar(var), Arc::new(expr))])?)
    }
    fn set_equations(
        &mut self,
        exprs: impl IntoIterator<Item = (Var, Expr)>,
    ) -> anyhow::Result<()> {
        Ok(self.changed_to(
            exprs
                .into_iter()
                .map(|(var, expr)| (LookupVar(var), Arc::new(expr)))
                .collect::<Vec<_>>(),
        )?)
    }
}

#[async_trait]
pub trait FuzzMath {
    async fn eval(&mut self, state: Arc<FuzzState>, var: Var) -> anyhow::Result<bool>;
}

#[async_trait]
impl FuzzMath for DiceComputations<'_> {
    async fn eval(&mut self, state: Arc<FuzzState>, var: Var) -> anyhow::Result<bool> {
        Ok(*self
            .compute(&state.eval_var(var))
            .await?
            .map_err(|e| anyhow::anyhow!(format!("{:#}", e)))?
            .as_ref())
    }
}

#[derive(Clone, Copy, PartialEq, Debug, Serialize, Deserialize)]
pub enum ComputationStep {
    ReturnStable,
    ReturnTransient,
}

#[derive(Allocative)]
pub struct FuzzState {
    #[allocative(skip)]
    pub steps: HashMap<Var, SegQueue<ComputationStep>>,
}

impl FuzzState {
    pub(crate) fn new() -> Self {
        Self {
            steps: HashMap::new(),
        }
    }

    pub(crate) fn eval_var(self: &Arc<Self>, key: Var) -> EvalVar {
        EvalVar {
            key,
            state: self.dupe(),
        }
    }

    pub(crate) fn next_step_for_var(self: &Arc<Self>, var: Var) -> ComputationStep {
        match self.steps.get(&var).and_then(|q| q.pop()) {
            Some(step) => step,
            _ => ComputationStep::ReturnStable,
        }
    }
}

#[derive(Derivative, Clone, Display, Allocative)]
#[derivative(Hash, Debug)]
#[display("Eval({})", key)]
pub struct EvalVar {
    key: Var,
    #[derivative(Debug = "ignore", Hash = "ignore")]
    state: Arc<FuzzState>,
}

impl PartialEq for EvalVar {
    fn eq(&self, other: &Self) -> bool {
        self.key == other.key && Arc::ptr_eq(&self.state, &other.state)
    }
}

impl Eq for EvalVar {}

#[derive(PartialEq, Eq, Clone, Dupe, Allocative)]
pub enum MaybeTransient<T> {
    Stable(T),
    Transient(T),
}

impl<T> AsRef<T> for MaybeTransient<T> {
    fn as_ref(&self) -> &T {
        match self {
            MaybeTransient::Stable(v) => v,
            MaybeTransient::Transient(v) => v,
        }
    }
}

impl<T> MaybeTransient<T> {
    fn is_transient(&self) -> bool {
        matches!(self, MaybeTransient::Transient(_))
    }
}

#[async_trait]
impl Key for EvalVar {
    type Value = Result<MaybeTransient<bool>, Arc<anyhow::Error>>;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        let step = self.state.next_step_for_var(self.key);
        let ret = match &*lookup_unit(ctx, self.key).await.map_err(Arc::new)? {
            Expr::Unit(unit) => resolve_units(ctx, &[unit.clone()], self.state.dupe())
                .await
                .map_err(Arc::new)?[0],
            Expr::Cond {
                test,
                then,
                otherwise,
            } => {
                if resolve_units(ctx, &[test.clone()], self.state.dupe())
                    .await
                    .map_err(Arc::new)?[0]
                {
                    resolve_units(ctx, &[then.clone()], self.state.dupe())
                        .await
                        .map_err(Arc::new)?[0]
                } else {
                    resolve_units(ctx, &[otherwise.clone()], self.state.dupe())
                        .await
                        .map_err(Arc::new)?[0]
                }
            }
            Expr::Xor(vars) => resolve_units(ctx, vars, self.state.dupe())
                .await
                .map_err(Arc::new)?
                .into_iter()
                .reduce(|x, y| x ^ y)
                .unwrap_or(false),
        };
        Ok(match step {
            ComputationStep::ReturnStable => MaybeTransient::Stable(ret),
            ComputationStep::ReturnTransient => MaybeTransient::Transient(ret),
        })
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        match (x, y) {
            (Ok(x), Ok(y)) => x == y,
            _ => false,
        }
    }

    fn validity(x: &Self::Value) -> bool {
        x.as_ref().is_ok_and(|x| !x.is_transient())
    }
}

#[cfg(test)]
mod tests {
    use dice::DetectCycles;
    use dice::Dice;

    use super::*;

    #[tokio::test]
    pub async fn test_smoke() -> anyhow::Result<()> {
        let empty_state = Arc::new(FuzzState::new());
        let dice = Dice::builder().build(DetectCycles::Disabled);
        let mut ctx = {
            let mut ctx = dice.updater();
            // let x1 = true
            ctx.set_equation(Var(1), Expr::Unit(Unit::Literal(true)))?;
            // let x2 = x1
            ctx.set_equation(Var(2), Expr::Xor(vec![Unit::Variable(Var(1))]))?;
            // let x3 = x1 ^ x1 = false
            ctx.set_equation(
                Var(3),
                Expr::Xor(vec![Unit::Variable(Var(1)), Unit::Variable(Var(1))]),
            )?;
            // let x4 = if x1 then x2 else x3
            ctx.set_equation(
                Var(4),
                Expr::Cond {
                    test: Unit::Variable(Var(1)),
                    then: Unit::Variable(Var(2)),
                    otherwise: Unit::Variable(Var(3)),
                },
            )?;
            ctx.commit().await
        };
        assert!(ctx.eval(empty_state.dupe(), Var(1)).await?);
        assert!(ctx.eval(empty_state.dupe(), Var(2)).await?);
        assert!(!ctx.eval(empty_state.dupe(), Var(3)).await?);
        assert!(ctx.eval(empty_state.dupe(), Var(4)).await?);
        Ok(())
    }
}
