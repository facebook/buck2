/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::future::Future;
use std::str::FromStr;
use std::sync::Arc;
use std::time::Duration;

use allocative::Allocative;
use anyhow::Context;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_core::soft_error;
use buck2_core::target::label::ConfiguredTargetLabel;
use buck2_events::dispatch::EventDispatcher;
use buck2_events::span::SpanId;
use buck2_util::late_binding::LateBinding;
use dice::ActivationTracker;
use dice::UserComputationData;
use dupe::Dupe;
use tokio::task::JoinHandle;

use crate::artifact_groups::ArtifactGroup;

#[derive(Copy, Clone, Dupe, derive_more::Display, Allocative)]
pub enum CriticalPathBackendName {
    #[display(fmt = "longest-path-graph")]
    LongestPathGraph,
    #[display(fmt = "default")]
    Default,
}

impl FromStr for CriticalPathBackendName {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "longest-path-graph" {
            return Ok(Self::LongestPathGraph);
        }

        if s == "default" {
            return Ok(Self::Default);
        }

        Err(anyhow::anyhow!("Invalid backend name: `{}`", s))
    }
}

pub static START_LISTENER_BY_BACKEND_NAME: LateBinding<
    fn(
        EventDispatcher,
        CriticalPathBackendName,
    ) -> (BuildSignalsInstaller, JoinHandle<anyhow::Result<()>>),
> = LateBinding::new("START_LISTENER_BY_BACKEND_NAME");

/// Creates a Build Listener signal pair and invokes the given asynchronous function with the send-end of the signal
/// sender.
///
/// Build listeners in this module operate by creating a matched pair of signal senders and signal receivers. Senders
/// are Dupe and allow for arbitrarily many writeres. Receivers are not Dupe and are expected to be driven by a single
/// thread. This implies that, in order for the receiver to function correctly and dispatch to build listeners, it must
/// be run in a background task that is periodically polled.
///
/// This function arranges for a background task to be spawned that drives the receiver, while invoking the called
/// function with a live BuildSignalSender that can be used to send events to the listening receiver. Upon return of
/// `scope`, the sender terminates the receiver by sending a `BuildFinished` signal and joins the receiver task.
pub async fn scope<F, R, Fut>(
    events: EventDispatcher,
    backend: CriticalPathBackendName,
    func: F,
) -> anyhow::Result<R>
where
    F: FnOnce(BuildSignalsInstaller) -> Fut,
    Fut: Future<Output = anyhow::Result<R>>,
{
    let (installer, handle) = (START_LISTENER_BY_BACKEND_NAME.get()?)(events, backend);
    let result = func(installer.dupe()).await;
    installer.build_signals.build_finished();
    let res = handle
        .await
        .context("Error joining critical path task")?
        .context("Error computing critical path");
    if let Err(e) = res {
        soft_error!("critical_path_computation_failed", e)?;
    }
    result
}

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
