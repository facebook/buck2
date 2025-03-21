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
use buck2_interpreter::dice::starlark_provider::StarlarkEvalKind;
use buck2_interpreter::starlark_profiler::profiler::StarlarkProfilerOpt;
use buck2_interpreter::starlark_promise::StarlarkPromise;
use dice::DiceComputations;
use either::Either;
use futures::FutureExt;
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
}

#[async_trait(?Send)]
impl<'v> AnonPromisesDyn<'v> for AnonPromises<'v> {
    async fn run_promises(
        self: Box<Self>,
        dice: &mut DiceComputations,
        eval: &mut Evaluator<'v, '_, '_>,
        eval_kind: &StarlarkEvalKind,
    ) -> buck2_error::Result<()> {
        // Resolve all the targets in parallel
        // We have vectors of vectors, so we create a "shape" which has the same shape but with indices
        let mut shape = Vec::new();
        let mut anon_target_keys = Vec::new();
        for (promise, xs) in self.entries {
            match xs {
                Either::Left(x) => {
                    shape.push((promise, Either::Left(shape.len())));
                    anon_target_keys.push(x);
                }
                Either::Right(xs) => {
                    shape.push((promise, Either::Right(shape.len()..shape.len() + xs.len())));
                    anon_target_keys.extend(xs);
                }
            }
        }

        let values = dice
            .try_compute_join(anon_target_keys.iter(), |dice, anon_target_key| {
                async move { anon_target_key.resolve(dice).await }.boxed()
            })
            .await?;

        Ok(with_starlark_eval_provider(
            dice,
            &mut StarlarkProfilerOpt::disabled(),
            &eval_kind,
            |_provider, _| {
                // But must bind the promises sequentially
                for (promise, xs) in shape {
                    match xs {
                        Either::Left(i) => {
                            let val = values[i].providers()?.add_heap_ref(eval.frozen_heap());
                            promise.resolve(val.to_value(), eval)?
                        }
                        Either::Right(is) => {
                            let xs: Vec<_> = is
                                .map(|i| {
                                    Ok(values[i].providers()?.add_heap_ref(eval.frozen_heap()))
                                })
                                .collect::<buck2_error::Result<_>>()?;
                            let list = eval.heap().alloc(AllocList(xs));
                            promise.resolve(list, eval)?
                        }
                    }
                }
                Ok(())
            },
        )
        .await?)
    }
}
