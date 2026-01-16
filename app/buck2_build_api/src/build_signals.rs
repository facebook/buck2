/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use buck2_artifact::actions::key::ActionKey;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_build_signals::env::DeferredBuildSignals;
use buck2_build_signals::env::NodeDuration;
use buck2_build_signals::env::WaitingData;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_events::span::SpanId;
use buck2_util::late_binding::LateBinding;
use dice::ActivationTracker;
use dice::UserComputationData;
use dupe::Dupe;

use crate::artifact_groups::ResolvedArtifactGroupBuildSignalsKey;

pub static CREATE_BUILD_SIGNALS: LateBinding<
    fn() -> (BuildSignalsInstaller, Box<dyn DeferredBuildSignals>),
> = LateBinding::new("CREATE_BUILD_SIGNALS");

pub fn create_build_signals() -> (BuildSignalsInstaller, Box<dyn DeferredBuildSignals>) {
    (CREATE_BUILD_SIGNALS.get().unwrap())()
}

/// Everything we need to setup build signals when starting a command.
#[derive(Clone, Dupe)]
pub struct BuildSignalsInstaller {
    pub build_signals: Arc<dyn BuildSignals>,
    pub activation_tracker: Arc<dyn ActivationTracker>,
}

/// Send notifications to the build signals backend.
pub trait BuildSignals: Send + Sync + 'static {
    fn top_level_target(
        &self,
        label: ConfiguredTargetLabel,
        artifacts: Vec<ResolvedArtifactGroupBuildSignalsKey>,
    );

    fn final_materialization(
        &self,
        artifact: BuildArtifact,
        duration: NodeDuration,
        span_id: Option<SpanId>,
        waiting_data: WaitingData,
    );

    fn test_listing(
        &self,
        target: ConfiguredTargetLabel,
        suite: String,
        duration: NodeDuration,
        deps: &[ActionKey],
    );

    fn test_execution(
        &self,
        target: ConfiguredTargetLabel,
        suite: String,
        testcases: &[String],
        variant: Option<String>,
        duration: NodeDuration,
        deps: &[ActionKey],
    );
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
    fn get_build_signals(&self) -> Option<&Arc<dyn BuildSignals>>;
}

impl HasBuildSignals for UserComputationData {
    fn get_build_signals(&self) -> Option<&Arc<dyn BuildSignals>> {
        self.data.get::<Arc<dyn BuildSignals>>().ok()
    }
}
