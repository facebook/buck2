/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use async_trait::async_trait;
use buck2_build_api::analysis::anon_promises_dyn::AnonPromisesDyn;
use buck2_interpreter::dice::starlark_provider::with_starlark_eval_provider;
use buck2_interpreter::starlark_profiler::StarlarkProfilerOrInstrumentation;
use buck2_interpreter::starlark_promise::StarlarkPromise;
use dice::DiceComputations;
use either::Either;
use futures::future;
use starlark::eval::Evaluator;
use starlark::values::list::AllocList;
use starlark::values::Trace;
use starlark::values::ValueTyped;

use crate::anon_targets::AnonTargetKey;

#[derive(Default, Debug, Trace, Allocative)]
pub(crate) struct AnonPromises<'v> {
    // The actual data
    entries: Vec<(
        ValueTyped<'v, StarlarkPromise<'v>>,
        // Either a single entry, or a list that becomes a list of providers
        Either<AnonTargetKey, Vec<AnonTargetKey>>,
    )>,
}

impl<'v> AnonPromises<'v> {
    pub(crate) fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub(crate) fn push_one(
        &mut self,
        promise: ValueTyped<'v, StarlarkPromise<'v>>,
        one: AnonTargetKey,
    ) {
        self.entries.push((promise, Either::Left(one)));
    }

    pub(crate) fn push_list(
        &mut self,
        promise: ValueTyped<'v, StarlarkPromise<'v>>,
        many: Vec<AnonTargetKey>,
    ) {
        self.entries.push((promise, Either::Right(many)));
    }
}

#[async_trait(?Send)]
impl<'v> AnonPromisesDyn<'v> for AnonPromises<'v> {
    async fn run_promises(
        self: Box<Self>,
        dice: &DiceComputations,
        eval: &mut Evaluator<'v, '_>,
        description: String,
    ) -> anyhow::Result<()> {
        // Resolve all the targets in parallel
        // We have vectors of vectors, so we create a "shape" which has the same shape but with indices
        let mut shape = Vec::new();
        let mut targets = Vec::new();
        for (promise, xs) in self.entries {
            match xs {
                Either::Left(x) => {
                    shape.push((promise, Either::Left(shape.len())));
                    targets.push(x);
                }
                Either::Right(xs) => {
                    shape.push((promise, Either::Right(shape.len()..shape.len() + xs.len())));
                    targets.extend(xs);
                }
            }
        }

        let values =
            future::try_join_all(targets.iter().map(|target| target.resolve(dice))).await?;
        with_starlark_eval_provider(
            dice,
            &mut StarlarkProfilerOrInstrumentation::disabled(),
            description,
            |_provider, _| {
                // But must bind the promises sequentially
                for (promise, xs) in shape {
                    match xs {
                        Either::Left(i) => {
                            let val = values[i]
                                .provider_collection
                                .value()
                                .owned_value(eval.frozen_heap());
                            promise.resolve(val, eval)?
                        }
                        Either::Right(is) => {
                            let xs: Vec<_> = is
                                .map(|i| {
                                    values[i]
                                        .provider_collection
                                        .value()
                                        .owned_value(eval.frozen_heap())
                                })
                                .collect();
                            let list = eval.heap().alloc(AllocList(xs));
                            promise.resolve(list, eval)?
                        }
                    }
                }
                Ok(())
            },
        )
        .await
    }
}
