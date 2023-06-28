/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any::Any;
use std::sync::Arc;

use allocative::Allocative;
use more_futures::spawner::Spawner;
use more_futures::spawner::TokioSpawner;

use crate::api::activation_tracker::ActivationTracker;
use crate::api::data::DiceData;
use crate::api::events::DiceEvent;
use crate::api::events::DiceEventListener;

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

    #[allocative(skip)]
    pub cycle_detector: Option<Arc<dyn UserCycleDetector>>,

    #[allocative(skip)]
    pub activation_tracker: Option<Arc<dyn ActivationTracker>>,

    /// We require that UserComputationData always be constructed with `..Default::default()`
    pub _requires_default: RequireDefault,
}

/// A UserCycleDetector can be used for custom cycle detection in the DICE computation.
pub trait UserCycleDetector: Send + Sync + 'static {
    /// Called by DICE when it starts computing a key. `key` will be a user Key type (and so user can reliably downcast it to known types).
    fn start_computing_key(&self, key: &dyn Any) -> Option<Box<dyn UserCycleDetectorGuard>>;

    /// Called by DICE when the key finished computing.
    fn finished_computing_key(&self, key: &dyn Any);
}

/// A UserCycleDetectorGuard is used to track the currently computing key. User code can access this through
/// ComputationData::cycle_guard() (and then downcast it with as_any to potentially access custom cycle behavior).
pub trait UserCycleDetectorGuard: Send + Sync + 'static {
    /// Called by dice when a dependency edge is encountered.
    fn add_edge(&self, key: &dyn Any);

    /// This is used to allow user code to get at the concrete guard instance.
    fn as_any(&self) -> &dyn Any;

    /// Used in error messages.
    fn type_name(&self) -> &'static str;
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
            spawner: Arc::new(TokioSpawner),
            cycle_detector: None,
            activation_tracker: None,
            _requires_default: RequireDefault(()),
        }
    }
}

#[derive(Allocative)]
pub(crate) struct NoOpTracker;

impl DiceEventListener for NoOpTracker {
    fn event(&self, _ev: DiceEvent) {}
}
