/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![cfg_attr(feature = "gazebo_lint", feature(plugin))]
#![cfg_attr(feature = "gazebo_lint", allow(deprecated))]
#![cfg_attr(feature = "gazebo_lint", plugin(gazebo_lint))]

mod common;

use async_trait::async_trait;
use common::BenchmarkComputationsPrerequisites;
use dice::DiceTransaction;
use dice_examples::math_computation::{parse_math_equations, Equation, Math, MathEquations, Var};

/// create equations for the first n fibonacci numbers
fn fib(n: usize, offset: Option<u8>, swap: bool) -> Vec<(Var, Equation)> {
    let tmp = (2..=n)
        .map(|i| {
            let (lhs, rhs) = if swap { (i - 2, i - 1) } else { (i - 1, i - 2) };
            let mut eq = format!("a{}=a{}+a{}", i, lhs, rhs);
            if let Some(offset) = offset {
                eq.push_str(&format!("+{}", offset));
            }
            eq
        })
        .collect::<Vec<_>>();
    let reg = parse_math_equations(tmp.iter().map(|x| &**x));
    let base = parse_math_equations(vec!["a0=1", "a1=1"]);
    base.and_then(|mut base_fibs| {
        reg.map(|mut reg_fibs| {
            base_fibs.append(&mut reg_fibs);
            base_fibs
        })
    })
    .expect("Could parse all equations")
}

struct MathBenchmark;

const SIZE: usize = 1000;

#[async_trait]
impl BenchmarkComputationsPrerequisites for MathBenchmark {
    type Updater = (Var, Equation);
    type Key = Var;
    type Value = i64;

    async fn fresh(ctx: DiceTransaction) -> DiceTransaction {
        Self::update(ctx, fib(SIZE, None, false)).await
    }

    async fn update<I>(ctx: DiceTransaction, keys: I) -> DiceTransaction
    where
        I: IntoIterator<Item = Self::Updater> + Send + Sync,
    {
        ctx.set_equations(keys);
        ctx.commit()
    }

    async fn compute(ctx: &DiceTransaction, key: Self::Key) -> Self::Value {
        ctx.eval(key).await
    }

    fn invalidated_recompute() -> (Vec<Self::Updater>, Self::Key) {
        (fib(SIZE / 2, Some(1), false), Var(format!("a{}", SIZE)))
    }

    fn early_cutoff_recompute() -> (Vec<Self::Updater>, Self::Key) {
        (fib(SIZE / 2, None, true), Var(format!("a{}", SIZE)))
    }

    fn get_sample_updates() -> Vec<Self::Updater> {
        fib(SIZE / 2, Some(5), true)
    }

    fn get_sample_key() -> Self::Key {
        Var(format!("a{}", SIZE))
    }
}

benchmark!(math_benchmark);
