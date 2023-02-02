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
use more_futures::spawner::Spawner;
use more_futures::spawner::TokioSpawner;

use crate::api::data::DiceData;
use crate::ctx::NoOpTracker;
use crate::DiceEventListener;

/// Includes all user related computation-specific data.
#[derive(Allocative)]
pub struct UserComputationData {
    /// The DiceData provides a spot for users to attach whatever extra things they want.
    ///
    /// This can contain arbitrary data from users that will not be part of the dice graph.
    /// As an example, users may want to inject some form of event dispatcher to send events from their computations.
    pub data: DiceData,
    pub tracker: Arc<dyn DiceEventListener>,
    #[allocative(skip)]
    pub spawner: Arc<dyn Spawner<Self>>,

    /// We require that UserComputationData always be constructed with `..Default::default()`
    pub _requires_default: RequireDefault,
}

#[derive(Allocative)]
pub struct RequireDefault(());

impl UserComputationData {
    pub fn new() -> Self {
        Self::default()
    }
}

impl Default for UserComputationData {
    fn default() -> Self {
        Self {
            data: DiceData::new(),
            tracker: Arc::new(NoOpTracker),
            spawner: Arc::new(TokioSpawner::default()),
            _requires_default: RequireDefault(()),
        }
    }
}
