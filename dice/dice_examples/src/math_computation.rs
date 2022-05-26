/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//!
//! A math computation built on dice.
//! The math computation will calculate a series of addition equations resolving variables.
//! e.g. `x = 1; y = 2, a = x + y; b = a + a; eval(b)`;

use std::{convert::Infallible, str::FromStr, sync::Arc};

use anyhow::anyhow;
use async_trait::async_trait;
use derive_more::Display;
use dice::{DiceComputations, InjectedKey, Key};
use futures::{future, FutureExt};
use gazebo::prelude::*;

#[derive(Clone, Dupe, PartialEq, Eq, Hash, Display, Debug)]
#[display(fmt = "Var({})", _0)]
pub struct Var(pub Arc<String>);

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Unit {
    Var(Var),
    Literal(i64),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Equation {
    Add(Vec<Unit>),
    Unit(Unit),
}

impl FromStr for Unit {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s.parse::<i64>() {
            Ok(i) => Unit::Literal(i),
            Err(_) => Unit::Var(Var(Arc::new(s.into()))),
        })
    }
}

impl FromStr for Equation {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> anyhow::Result<Self> {
        Ok(if s.contains('+') {
            let units = s
                .split_terminator('+')
                .map(|unit| unit.parse::<Unit>())
                .collect::<Result<Vec<Unit>, _>>()
                .unwrap();

            if units.len() < 2 {
                return Err(anyhow!("+ must have left and right"));
            }

            Equation::Add(units)
        } else {
            Equation::Unit(s.parse().unwrap())
        })
    }
}

pub fn parse_math_equations<'a>(
    math: impl IntoIterator<Item = &'a str>,
) -> anyhow::Result<Vec<(Var, Equation)>> {
    math.into_iter().map(parse_math_equation).collect()
}

pub fn parse_math_equation(math: &str) -> anyhow::Result<(Var, Equation)> {
    let (l, r) = math.split1("=");
    if l.is_empty() || r.is_empty() {
        return Err(anyhow!("= must have left and right"));
    }

    Ok((Var(Arc::new(l.to_owned())), r.parse()?))
}

pub trait MathEquations {
    fn set_equation(&self, var: Var, equation: Equation);
    fn set_equations(&self, equations: impl IntoIterator<Item = (Var, Equation)>);
}

#[async_trait]
pub trait Math {
    async fn eval(&self, var: Var) -> i64;
}

impl MathEquations for DiceComputations {
    fn set_equation(&self, var: Var, equation: Equation) {
        self.changed_to(vec![(LookupVar(var), Arc::new(equation))])
    }
    fn set_equations(&self, equations: impl IntoIterator<Item = (Var, Equation)>) {
        self.changed_to(
            equations
                .into_iter()
                .map(|(var, eq)| (LookupVar(var), Arc::new(eq)))
                .collect::<Vec<_>>(),
        );
    }
}

#[derive(Clone, Display, Debug, Dupe, Eq, Hash, PartialEq)]
#[display(fmt = "Eval({})", _0)]
pub struct EvalVar(pub Var);
#[async_trait]
impl Key for EvalVar {
    type Value = i64;

    async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
        let equation = lookup_unit(ctx, &self.0).await;
        match &*equation {
            Equation::Add(adds) => resolve_units(ctx, &adds[..]).await.iter().sum(),
            Equation::Unit(unit) => resolve_units(ctx, &[unit.clone()]).await[0],
        }
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

#[async_trait]
impl Math for DiceComputations {
    async fn eval(&self, var: Var) -> i64 {
        self.compute(&EvalVar(var)).await
    }
}

async fn resolve_units(ctx: &DiceComputations, units: &[Unit]) -> Vec<i64> {
    let futs = units.map(|unit| match unit {
        Unit::Var(var) => ctx.eval(var.clone()),
        Unit::Literal(lit) => async move { *lit }.boxed(),
    });

    future::join_all(futs).await
}

async fn lookup_unit(ctx: &DiceComputations, var: &Var) -> Arc<Equation> {
    ctx.compute(&LookupVar(var.clone())).await
}

#[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq)]
#[display(fmt = "Lookup({})", _0)]
struct LookupVar(Var);
impl InjectedKey for LookupVar {
    type Value = Arc<Equation>;

    fn compare(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

#[cfg(test)]
mod parser_tests {
    use super::*;

    fn var(name: &str) -> Var {
        Var(Arc::new(name.to_owned()))
    }

    #[test]
    fn test_parser_unit_var() {
        let eq = "a=bbab";
        assert_eq!(
            vec![(var("a"), Equation::Unit(Unit::Var(var("bbab"))))],
            parse_math_equations(vec![eq]).unwrap()
        );
    }

    #[test]
    fn test_parser_unit_int() {
        let eq = "hello world=25";
        assert_eq!(
            vec![(var("hello world"), Equation::Unit(Unit::Literal(25)))],
            parse_math_equations(vec![eq]).unwrap()
        );
    }

    #[test]
    fn test_parser_expr() {
        assert_eq!(
            vec![(
                var("a"),
                Equation::Add(vec![
                    Unit::Var(var("bd")),
                    Unit::Literal(25),
                    Unit::Var(var("b"))
                ])
            )],
            parse_math_equations(vec!["a=bd+25+b"]).unwrap()
        )
    }

    #[test]
    #[allow(clippy::many_single_char_names)]
    fn test_parser_compound() {
        let eq = vec!["x=1", "y=2", "a=x+y", "b=a+c"];

        let x = var("x");
        let y = var("y");
        let a = var("a");
        let b = var("b");
        let c = var("c");

        assert_eq!(
            vec![
                (x.clone(), Equation::Unit(Unit::Literal(1))),
                (y.clone(), Equation::Unit(Unit::Literal(2))),
                (a.clone(), Equation::Add(vec![Unit::Var(x), Unit::Var(y)])),
                (b, Equation::Add(vec![Unit::Var(a), Unit::Var(c)]))
            ],
            parse_math_equations(eq).unwrap()
        );
    }

    #[test]
    #[should_panic(expected = "+ must have left and right")]
    fn test_parser_invalid() {
        parse_math_equations(vec!["a=a+"]).unwrap();
    }
}
