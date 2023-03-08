/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use dupe::Dupe;

use crate::api::computations::DiceComputations;
use crate::api::error::DiceResult;
use crate::api::projection::DiceProjectionComputations;
use crate::api::storage_type::StorageType;
use crate::api::user_data::UserComputationData;
use crate::ctx::DiceComputationsImpl;
use crate::impls::ctx::PerComputeCtx;
use crate::impls::ctx::SharedLiveTransactionCtx;
use crate::impls::dice::DiceModern;
use crate::impls::events::DiceEventDispatcher;
use crate::impls::key::DiceKey;
use crate::impls::key::DiceKeyErased;
use crate::impls::key::ParentKey;
use crate::impls::value::DiceValue;
use crate::HashSet;

/// Evaluates Keys
#[allow(unused)]
#[derive(Clone, Dupe)]
pub(crate) struct AsyncEvaluator {
    per_live_version_ctx: SharedLiveTransactionCtx,
    user_data: Arc<UserComputationData>,
    dice: Arc<DiceModern>,
}

#[allow(unused)]
impl AsyncEvaluator {
    pub(crate) fn new(
        per_live_version_ctx: SharedLiveTransactionCtx,
        user_data: Arc<UserComputationData>,
        dice: Arc<DiceModern>,
    ) -> Self {
        Self {
            per_live_version_ctx,
            user_data,
            dice,
        }
    }

    pub(crate) fn storage_type(&self, key: DiceKey) -> StorageType {
        let key_erased = self.dice.key_index.get(key);
        match key_erased {
            DiceKeyErased::Key(k) => k.storage_type(),
            DiceKeyErased::Projection(p) => p.proj().storage_type(),
        }
    }

    pub(crate) async fn evaluate<'b>(&self, key: DiceKey) -> DiceResult<DiceValueStorageAndDeps> {
        let key_erased = self.dice.key_index.get(key);
        match key_erased {
            DiceKeyErased::Key(key_dyn) => {
                let new_ctx = DiceComputations(DiceComputationsImpl::Modern(PerComputeCtx::new(
                    ParentKey::Some(key), // within this key's compute, this key is the parent
                    self.per_live_version_ctx.dupe(),
                    self.user_data.dupe(),
                    self.dice.dupe(),
                )));

                let value = key_dyn.compute(&new_ctx).await;
                let deps = match new_ctx.0 {
                    DiceComputationsImpl::Legacy(_) => {
                        unreachable!("modern dice created above")
                    }
                    DiceComputationsImpl::Modern(new_ctx) => new_ctx.finalize_deps(),
                };

                Ok(DiceValueStorageAndDeps {
                    value,
                    deps,
                    storage: key_dyn.storage_type(),
                })
            }
            DiceKeyErased::Projection(proj) => {
                let base = self
                    .per_live_version_ctx
                    .compute_opaque(
                        proj.base(),
                        ParentKey::Some(key), // the parent requesting the projection base is the projection itself
                        self.dice.state_handle.dupe(),
                        self.dupe(),
                        &self.user_data,
                        DiceEventDispatcher::new(self.user_data.tracker.dupe(), self.dice.dupe()),
                    )
                    .await?;

                let ctx = DiceProjectionComputations {
                    data: &self.dice.global_data,
                    user_data: &self.user_data,
                };

                let value = proj.proj().compute(base.value(), &ctx);

                Ok(DiceValueStorageAndDeps {
                    value,
                    deps: [proj.base()].into_iter().collect(),
                    storage: proj.proj().storage_type(),
                })
            }
        }
    }
}

/// Evaluates Keys
#[derive(Clone, Dupe)]
pub(crate) struct SyncEvaluator {
    per_live_version_ctx: SharedLiveTransactionCtx,
    user_data: Arc<UserComputationData>,
    dice: Arc<DiceModern>,
    base: DiceValue,
}

impl SyncEvaluator {
    pub(crate) fn new(
        per_live_version_ctx: SharedLiveTransactionCtx,
        user_data: Arc<UserComputationData>,
        dice: Arc<DiceModern>,
        base: DiceValue,
    ) -> Self {
        Self {
            per_live_version_ctx,
            user_data,
            dice,
            base,
        }
    }

    pub(crate) fn evaluate(&self, key: DiceKey) -> DiceResult<DiceValueStorageAndDeps> {
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

                Ok(DiceValueStorageAndDeps {
                    value,
                    deps: [proj.base()].into_iter().collect(),
                    storage: proj.proj().storage_type(),
                })
            }
        }
    }
}

#[allow(unused)] // TODO(bobyf)
pub(crate) struct DiceValueStorageAndDeps {
    pub(crate) value: DiceValue,
    pub(crate) deps: HashSet<DiceKey>,
    pub(crate) storage: StorageType,
}
