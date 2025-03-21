/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::future::Future;
use std::str::FromStr;
use std::time::Duration;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_core::soft_error;
use buck2_error::BuckErrorContext;
use buck2_events::dispatch::EventDispatcher;
use dice::UserComputationData;
use dupe::Dupe;

#[derive(Copy, Clone, Dupe)]
pub struct NodeDuration {
    /// The amount of time for this node that corresponds to something the user might be able to
    /// improve. We should better break this down.
    pub user: Duration,
    /// The total duration for this node.
    pub total: Duration,
    /// The waiting duration for this node.
    pub queue: Option<Duration>,
}

impl NodeDuration {
    /// Returns the duration we are using in our critical path calculation. This doesn't really
    /// *need* to be a function but is helpful so that not every callsite has to know which one we chose.
    pub fn critical_path_duration(&self) -> Duration {
        self.total
    }

    pub fn zero() -> Self {
        Self {
            user: Duration::from_secs(0),
            total: Duration::from_secs(0),
            queue: None,
        }
    }
}

#[derive(Copy, Clone, Dupe, derive_more::Display, Allocative)]
pub enum CriticalPathBackendName {
    /// This is the default backend.
    #[display("longest-path-graph")]
    LongestPathGraph,
    #[display("logging")]
    Logging,
}

impl FromStr for CriticalPathBackendName {
    type Err = buck2_error::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "longest-path-graph" {
            return Ok(Self::LongestPathGraph);
        } else if s == "logging" {
            return Ok(Self::Logging);
        }

        Err(buck2_error::buck2_error!(
            buck2_error::ErrorTag::Input,
            "Invalid backend name: `{}`",
            s
        ))
    }
}

pub struct EarlyCommandEntry {
    pub kind: String,
    pub duration: Duration,
}

pub struct BuildSignalsContext {
    pub command_name: String,
    pub metadata: HashMap<String, String>,
    pub isolation_prefix: FileNameBuf,
    pub early_command_entries: Vec<EarlyCommandEntry>,
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
        ctx: BuildSignalsContext,
    ) -> Box<dyn FinishBuildSignals>;
}

/// Returned by DeferredBuildSignals once started. Lets us report that we finished.
#[async_trait]
pub trait FinishBuildSignals: Send {
    async fn finish(self: Box<Self>) -> buck2_error::Result<()>;
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
    ctx: BuildSignalsContext,
    func: F,
) -> buck2_error::Result<R>
where
    F: FnOnce() -> Fut + Send,
    Fut: Future<Output = buck2_error::Result<R>> + Send,
    R: Send,
{
    let handle = deferred.start(events, backend, ctx);
    let result = func().await;
    let res = handle
        .finish()
        .await
        .buck_error_context("Error computing critical path");
    if let Err(e) = res {
        soft_error!("critical_path_computation_failed", e.into())?;
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
