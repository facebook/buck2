/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use dice::cycles::DetectCycles;
use dice::Dice;
use dice_examples::math_computation::parse_math_equation;
use dice_examples::math_computation::parse_math_equations;
use dice_examples::math_computation::Equation;
use dice_examples::math_computation::Math;
use dice_examples::math_computation::MathEquations;
use dice_examples::math_computation::Unit;
use dice_examples::math_computation::Var;
use gazebo::dupe::Dupe;

fn var(name: &str) -> Var {
    Var(Arc::new(name.to_owned()))
}

#[tokio::test]
async fn test_literal() {
    let dice = Dice::builder().build(DetectCycles::Enabled);
    let ctx = dice.ctx();
    let (var, eq) = parse_math_equation("a=5").unwrap();
    ctx.set_equation(var.dupe(), eq);
    let ctx = ctx.commit();

    assert_eq!(5, ctx.eval(var).await);
}

#[tokio::test]
async fn test_var() {
    let dice = Dice::builder().build(DetectCycles::Enabled);
    let ctx = dice.ctx();

    let eqs = parse_math_equations(vec!["b=a", "a=3"]).unwrap();

    ctx.set_equations(eqs);
    let ctx = ctx.commit();

    assert_eq!(3, ctx.eval(var("b")).await);
}

#[tokio::test]
async fn test_compound() {
    let dice = Dice::builder().build(DetectCycles::Enabled);
    let ctx = dice.ctx();

    let eq = vec!["x=1", "y=2", "a=x+y", "b=a+a"];
    let eq = parse_math_equations(eq).unwrap();

    ctx.set_equations(eq);
    let ctx = ctx.commit();

    assert_eq!(3, ctx.eval(var("a")).await);
    assert_eq!(6, ctx.eval(var("b")).await);
}

#[tokio::test]
async fn test_changed_eq() {
    let dice = Dice::builder().build(DetectCycles::Enabled);
    let ctx = dice.ctx();

    let eq = vec!["x=1", "y=2", "a=x+y", "b=a+a"];
    let eq = parse_math_equations(eq).unwrap();

    ctx.set_equations(eq);
    let ctx = ctx.commit();

    assert_eq!(6, ctx.eval(var("b")).await);

    ctx.set_equation(var("a"), Equation::Unit(Unit::Literal(4)));
    let ctx = ctx.commit();

    assert_eq!(8, ctx.eval(var("b")).await)
}
