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
use dupe::Dupe;

use crate::api::computations::DiceComputations;
use crate::api::projection::DiceProjectionComputations;
use crate::api::storage_type::StorageType;
use crate::api::user_data::UserComputationData;
use crate::ctx::DiceComputationsImpl;
use crate::impls::ctx::ModernComputeCtx;
use crate::impls::ctx::PerComputeCtx;
use crate::impls::ctx::SharedLiveTransactionCtx;
use crate::impls::dice::DiceModern;
use crate::impls::key::DiceKey;
use crate::impls::key::DiceKeyErased;
use crate::impls::key::ParentKey;
use crate::impls::value::MaybeValidDiceValue;
use crate::impls::worker::state::ActivationInfo;
use crate::impls::worker::state::DiceWorkerStateComputing;
use crate::impls::worker::state::DiceWorkerStateFinishedEvaluating;
use crate::result::CancellableResult;
use crate::ActivationData;
use crate::HashSet;

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

    pub(crate) async fn evaluate<'a, 'b>(
        &self,
        key: DiceKey,
        state: DiceWorkerStateComputing<'a, 'b>,
    ) -> CancellableResult<DiceWorkerStateFinishedEvaluating<'a, 'b>> {
        let key_erased = self.dice.key_index.get(key);

        let (cycles, state) = state.evaluating();

        match key_erased {
            DiceKeyErased::Key(key_dyn) => {
                let mut new_ctx = DiceComputations(DiceComputationsImpl::Modern(
                    ModernComputeCtx::Regular(PerComputeCtx::new(
                        ParentKey::Some(key), // within this key's compute, this key is the parent
                        self.per_live_version_ctx.dupe(),
                        self.user_data.dupe(),
                        self.dice.dupe(),
                        cycles,
                    )),
                ));

                let value = key_dyn
                    .compute(&mut new_ctx, &state.cancellation_ctx().into_compatible())
                    .await;
                let ((deps, dep_validity), evaluation_data, cycles) = match new_ctx.0 {
                    DiceComputationsImpl::Legacy(_) => {
                        unreachable!("modern dice created above")
                    }
                    DiceComputationsImpl::Modern(new_ctx) => new_ctx
                        .into_regular()
                        .expect("created regular above")
                        .finalize(),
                };

                let activation = ActivationInfo::new(
                    &self.dice.key_index,
                    &self.user_data.activation_tracker,
                    key,
                    deps.iter(),
                    evaluation_data.into_activation_data(), // Projection keys can't set this.
                );

                state.finished(
                    cycles,
                    KeyEvaluationResult {
                        value: MaybeValidDiceValue::new(value, dep_validity),
                        deps,
                        storage: key_dyn.storage_type(),
                    },
                    activation,
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

                let activation = ActivationInfo::new(
                    &self.dice.key_index,
                    &self.user_data.activation_tracker,
                    key,
                    [proj.base()].iter(),
                    ActivationData::Evaluated(None), // Projection keys can't set this.
                );

                state.finished(
                    cycles,
                    KeyEvaluationResult {
                        value: MaybeValidDiceValue::new(value, base.value().validity()),
                        deps: [proj.base()].into_iter().collect(),
                        storage: proj.proj().storage_type(),
                    },
                    activation,
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
}

impl SyncEvaluator {
    pub(crate) fn new(
        user_data: Arc<UserComputationData>,
        dice: Arc<DiceModern>,
        base: MaybeValidDiceValue,
    ) -> Self {
        Self {
            user_data,
            dice,
            base,
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
                    deps: [proj.base()].into_iter().collect(),
                    storage: proj.proj().storage_type(),
                }
            }
        }
    }
}

pub(crate) struct KeyEvaluationResult {
    pub(crate) value: MaybeValidDiceValue,
    pub(crate) deps: HashSet<DiceKey>,
    pub(crate) storage: StorageType,
}
