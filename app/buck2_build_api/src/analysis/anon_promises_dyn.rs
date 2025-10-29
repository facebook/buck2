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
use starlark::eval::Evaluator;

#[async_trait(?Send)]
pub trait AnonPromisesDyn<'v>: 'v {
    async fn run_promises<'a, 'e: 'a, 'd>(
        self: Box<Self>,
        accessor: &mut dyn RunAnonPromisesAccessor<'v, 'a, 'e, 'd>,
    ) -> buck2_error::Result<()>;
}

pub trait RunAnonPromisesAccessor<'v, 'a, 'e, 'd> {
    fn with_evaluator(
        &mut self,
        closure: &mut dyn FnMut(&mut Evaluator<'v, 'a, 'e>) -> buck2_error::Result<()>,
    ) -> buck2_error::Result<()>;

    fn dice(&mut self) -> &mut DiceComputations<'d>;
}

pub struct RunAnonPromisesAccessorPair<'me, 'v, 'a, 'e, 'd>(
    pub &'me mut ReentrantStarlarkEvaluator<'v, 'a, 'e>,
    pub &'me mut DiceComputations<'d>,
);

impl<'me, 'v, 'a, 'e, 'd> RunAnonPromisesAccessor<'v, 'a, 'e, 'd>
    for RunAnonPromisesAccessorPair<'me, 'v, 'a, 'e, 'd>
{
    fn with_evaluator(
        &mut self,
        closure: &mut dyn FnMut(&mut Evaluator<'v, 'a, 'e>) -> buck2_error::Result<()>,
    ) -> buck2_error::Result<()> {
        self.0.with_evaluator(closure)
    }

    fn dice(&mut self) -> &mut DiceComputations<'d> {
        self.1
    }
}
