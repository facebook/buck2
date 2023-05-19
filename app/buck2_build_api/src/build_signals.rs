/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;
use std::time::Duration;

use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_core::target::label::ConfiguredTargetLabel;
use buck2_events::span::SpanId;
use dice::ActivationTracker;
use dice::UserComputationData;
use dupe::Dupe;

use crate::artifact_groups::ArtifactGroup;

/// Everything we need to setup build signals when starting a command.
#[derive(Clone, Dupe)]
pub struct BuildSignalsInstaller {
    pub build_signals: Arc<dyn BuildSignals>,
    pub activation_tracker: Arc<dyn ActivationTracker>,
}

/// Send notifications to the build signals backend.
pub trait BuildSignals: Send + Sync + 'static {
    fn top_level_target(&self, label: ConfiguredTargetLabel, artifacts: Vec<ArtifactGroup>);

    fn final_materialization(
        &self,
        artifact: BuildArtifact,
        duration: NodeDuration,
        span_id: Option<SpanId>,
    );

    fn build_finished(&self);
}

#[derive(Copy, Clone, Dupe)]
pub struct NodeDuration {
    /// The amount of time for this node that corresponds to something the user might be able to
    /// improve. We should better break this down.
    pub user: Duration,
    /// The total duration for this node.
    pub total: Duration,
}

impl NodeDuration {
    /// Returns the duration we are using in our critical path calculation. This doesn't really
    /// *need* to be a function but right now we use user and want to switch to total so it's
    /// easier to do that if this is in a single function.
    pub fn critical_path_duration(&self) -> Duration {
        self.total
    }

    pub fn zero() -> Self {
        Self {
            user: Duration::from_secs(0),
            total: Duration::from_secs(0),
        }
    }
}

pub trait SetBuildSignals {
    fn set_build_signals(&mut self, sender: Arc<dyn BuildSignals>);
}

impl SetBuildSignals for UserComputationData {
    fn set_build_signals(&mut self, sender: Arc<dyn BuildSignals>) {
        self.data.set(sender);
    }
}

pub trait HasBuildSignals {
    fn get_build_signals(&self) -> Option<&dyn BuildSignals>;
}

impl HasBuildSignals for UserComputationData {
    fn get_build_signals(&self) -> Option<&dyn BuildSignals> {
        self.data
            .get::<Arc<dyn BuildSignals>>()
            .ok()
            .map(|build_signals| build_signals.as_ref())
    }
}
