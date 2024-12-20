/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::Allocative;
use dice_error::result::CancellableResult;
use dupe::Dupe;

use crate::api::computations::DiceComputations;
use crate::api::projection::DiceProjectionComputations;
use crate::api::storage_type::StorageType;
use crate::api::user_data::UserComputationData;
use crate::ctx::DiceComputationsImpl;
use crate::impls::ctx::ModernComputeCtx;
use crate::impls::ctx::SharedLiveTransactionCtx;
use crate::impls::deps::graph::SeriesParallelDeps;
use crate::impls::dice::DiceModern;
use crate::impls::key::DiceKey;
use crate::impls::key::DiceKeyErased;
use crate::impls::key::ParentKey;
use crate::impls::task::handle::DiceTaskHandle;
use crate::impls::user_cycle::KeyComputingUserCycleDetectorData;
use crate::impls::value::MaybeValidDiceValue;
use crate::impls::value::TrackedInvalidationPaths;
use crate::impls::worker::state::DiceWorkerStateEvaluating;
use crate::impls::worker::state::DiceWorkerStateFinishedEvaluating;
use crate::ActivationData;

/// Evaluates Keys
#[derive(Clone, Dupe, Allocative)]
pub(crate) struct AsyncEvaluator {
    pub(super) per_live_version_ctx: SharedLiveTransactionCtx,
    pub(super) user_data: Arc<UserComputationData>,
    pub(super) dice: Arc<DiceModern>,
}

impl AsyncEvaluator {
    pub(crate) fn storage_type(&self, key: DiceKey) -> StorageType {
        let key_erased = self.dice.key_index.get(key);
        match key_erased {
            DiceKeyErased::Key(k) => k.storage_type(),
            DiceKeyErased::Projection(p) => p.proj().storage_type(),
        }
    }

    pub(crate) async fn evaluate(
        &self,
        handle: &mut DiceTaskHandle<'_>,
        key: DiceKey,
        state: DiceWorkerStateEvaluating,
        cycles: KeyComputingUserCycleDetectorData,
    ) -> CancellableResult<DiceWorkerStateFinishedEvaluating> {
        let key_erased = self.dice.key_index.get(key);

        match key_erased {
            DiceKeyErased::Key(key_dyn) => {
                let mut new_ctx =
                    DiceComputations(DiceComputationsImpl::Modern(ModernComputeCtx::new(
                        ParentKey::Some(key), // within this key's compute, this key is the parent
                        cycles,
                        self.dupe(),
                    )));

                let value = key_dyn
                    .compute(&mut new_ctx, &handle.cancellation_ctx())
                    .await;
                let (recorded_deps, evaluation_data, cycles) = match new_ctx.0 {
                    DiceComputationsImpl::Modern(new_ctx) => new_ctx.finalize(),
                };

                state.finished(
                    handle,
                    cycles,
                    KeyEvaluationResult {
                        value: MaybeValidDiceValue::new(value, recorded_deps.deps_validity),
                        deps: recorded_deps.deps,
                        storage: key_dyn.storage_type(),
                        invalidation_paths: recorded_deps.invalidation_paths,
                    },
                    evaluation_data.into_activation_data(),
                )
            }
            DiceKeyErased::Projection(proj) => {
                let base = self
                    .per_live_version_ctx
                    .compute_opaque(
                        proj.base(),
                        ParentKey::Some(key), // the parent requesting the projection base is the projection itself
                        self,
                        cycles.subrequest(proj.base(), &self.dice.key_index),
                    )
                    .await?;

                let ctx = DiceProjectionComputations {
                    data: &self.dice.global_data,
                    user_data: &self.user_data,
                };

                let value = proj.proj().compute(base.value(), &ctx);

                state.finished(
                    handle,
                    cycles,
                    KeyEvaluationResult {
                        value: MaybeValidDiceValue::new(value, base.value().validity()),
                        deps: SeriesParallelDeps::serial_from_vec(vec![proj.base()]),
                        storage: proj.proj().storage_type(),
                        invalidation_paths: base.invalidation_paths().for_dependent(key),
                    },
                    ActivationData::Evaluated(None), // Projection keys can't set this.
                )
            }
        }
    }
}

/// Evaluates Keys
#[derive(Clone, Dupe)]
pub(crate) struct SyncEvaluator {
    user_data: Arc<UserComputationData>,
    dice: Arc<DiceModern>,
    base: MaybeValidDiceValue,
    base_invalidation_paths: TrackedInvalidationPaths,
}

impl SyncEvaluator {
    pub(crate) fn new(
        user_data: Arc<UserComputationData>,
        dice: Arc<DiceModern>,
        base: MaybeValidDiceValue,
        base_invalidation_paths: TrackedInvalidationPaths,
    ) -> Self {
        Self {
            user_data,
            dice,
            base,
            base_invalidation_paths,
        }
    }

    pub(crate) fn evaluate(&self, key: DiceKey) -> KeyEvaluationResult {
        let key_erased = self.dice.key_index.get(key);
        match key_erased {
            DiceKeyErased::Key(_) => {
                unreachable!("cannot evaluate async keys synchronously")
            }
            DiceKeyErased::Projection(proj) => {
                let ctx = DiceProjectionComputations {
                    data: &self.dice.global_data,
                    user_data: &self.user_data,
                };

                let value = proj.proj().compute(&self.base, &ctx);

                KeyEvaluationResult {
                    value: MaybeValidDiceValue::new(value, self.base.validity()),
                    deps: SeriesParallelDeps::serial_from_vec(vec![proj.base()]),
                    storage: proj.proj().storage_type(),
                    invalidation_paths: self.base_invalidation_paths.for_dependent(key),
                }
            }
        }
    }
}

pub(crate) struct KeyEvaluationResult {
    pub(crate) value: MaybeValidDiceValue,
    pub(crate) deps: SeriesParallelDeps,
    pub(crate) storage: StorageType,
    pub(crate) invalidation_paths: TrackedInvalidationPaths,
}
