/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use async_trait::async_trait;
use buck2_interpreter::factory::ReentrantStarlarkEvaluator;
use dice::DiceComputations;

#[async_trait(?Send)]
pub trait AnonPromisesDyn<'v>: 'v {
    async fn run_promises<'x, 'a: 'x, 'e: 'a, 'd>(
        self: Box<Self>,
        accessor: &mut dyn RunAnonPromisesAccessor<'x, 'v, 'a, 'e, 'd>,
    ) -> buck2_error::Result<()>
    where
        'v: 'x;
}

pub trait RunAnonPromisesAccessor<'x, 'v, 'a, 'e, 'd> {
    fn eval(&mut self) -> &mut ReentrantStarlarkEvaluator<'x, 'v, 'a, 'e>;
    fn dice(&mut self) -> &mut DiceComputations<'d>;
}

pub struct RunAnonPromisesAccessorPair<'me, 'x, 'v, 'a, 'e, 'd>(
    pub &'me mut ReentrantStarlarkEvaluator<'x, 'v, 'a, 'e>,
    pub &'me mut DiceComputations<'d>,
);

impl<'me, 'x, 'v, 'a, 'e, 'd> RunAnonPromisesAccessor<'x, 'v, 'a, 'e, 'd>
    for RunAnonPromisesAccessorPair<'me, 'x, 'v, 'a, 'e, 'd>
{
    fn eval(&mut self) -> &mut ReentrantStarlarkEvaluator<'x, 'v, 'a, 'e> {
        self.0
    }

    fn dice(&mut self) -> &mut DiceComputations<'d> {
        self.1
    }
}
