/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use dice::DiceComputations;
use starlark::eval::Evaluator;

#[async_trait(?Send)]
pub trait AnonPromisesDyn<'v>: 'v {
    async fn run_promises(
        self: Box<Self>,
        dice: &DiceComputations,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<()>;
}
