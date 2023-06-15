/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::str::FromStr;
use std::time::Duration;

use allocative::Allocative;
use anyhow::Context as _;
use async_trait::async_trait;
use buck2_core::soft_error;
use buck2_events::dispatch::EventDispatcher;
use dice::UserComputationData;
use dupe::Dupe;
use futures::future::Future;

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

/// Created along with the BuildSignalsInstaller (ideally, BuildSignalsInstaller's definition would
/// live here, but that can't be done for now because it has some dependencies on buck2_build_api).
///
/// This can be started to actually start processing build signals.
pub trait DeferredBuildSignals: Send {
    fn start(
        self: Box<Self>,
        events: EventDispatcher,
        backend: CriticalPathBackendName,
    ) -> Box<dyn FinishBuildSignals>;
}

/// Returned by DeferredBuildSignals once started. Lets us report that we finished.
#[async_trait]
pub trait FinishBuildSignals: Send {
    async fn finish(self: Box<Self>) -> anyhow::Result<()>;
}

/// Start the backend for a DeferredBuildSignals instance.
///
/// Build listeners operate by creating a matched pair of signal senders and signal receivers.
/// Senders are Dupe and allow for arbitrarily many writeres. Receivers are not Dupe and are
/// expected to be driven by a single thread. This implies that, in order for the receiver to
/// function correctly and dispatch to build listeners, it must be run in a background task that is
/// periodically polled.
///
/// This function arranges for a background task to be spawned that drives the receiver, while
/// invoking the called function with a live BuildSignalSender that can be used to send events to
/// the listening receiver. Upon return of `scope`, the sender terminates the receiver by sending a
/// `BuildFinished` signal and joins the receiver task.
pub async fn scope<F, R, Fut>(
    deferred: Box<dyn DeferredBuildSignals>,
    events: EventDispatcher,
    backend: CriticalPathBackendName,
    func: F,
) -> anyhow::Result<R>
where
    F: FnOnce() -> Fut + Send,
    Fut: Future<Output = anyhow::Result<R>> + Send,
    R: Send,
{
    let handle = deferred.start(events, backend);
    let result = func().await;
    let res = handle
        .finish()
        .await
        .context("Error computing critical path");
    if let Err(e) = res {
        soft_error!("critical_path_computation_failed", e)?;
    }
    result
}

pub trait HasCriticalPathBackend {
    fn set_critical_path_backend(&mut self, backend: CriticalPathBackendName);

    fn get_critical_path_backend(&self) -> CriticalPathBackendName;
}

impl HasCriticalPathBackend for UserComputationData {
    fn set_critical_path_backend(&mut self, backend: CriticalPathBackendName) {
        self.data.set(backend);
    }

    fn get_critical_path_backend(&self) -> CriticalPathBackendName {
        *self
            .data
            .get::<CriticalPathBackendName>()
            .expect("CriticalPathBackendName should be set")
    }
}
