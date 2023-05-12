/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;

use anyhow::Context;
use buck2_common::kill_util::try_terminate_process_gracefully;
use buck2_common::local_resource_state::LocalResourceState;
use buck2_common::result::SharedResult;
use buck2_core::target::label::ConfiguredTargetLabel;
use buck2_data::ReleaseLocalResourcesEnd;
use buck2_data::ReleaseLocalResourcesStart;
use buck2_events::dispatch::span_async;
use dashmap::DashMap;
use futures::future::BoxFuture;
use futures::future::Shared;

pub struct LocalResourceRegistry<'a>(
    pub DashMap<ConfiguredTargetLabel, Shared<BoxFuture<'a, SharedResult<LocalResourceState>>>>,
);

impl<'a> LocalResourceRegistry<'a> {
    pub(crate) fn new() -> Self {
        LocalResourceRegistry(DashMap::new())
    }

    pub(crate) async fn release_all_resources(&self) -> anyhow::Result<()> {
        // We setup resources prior to running tests so at this point everything should be set up, so just resolve all futures.
        let resource_futs = self
            .0
            .iter()
            .map(|entry| entry.value().clone())
            .collect::<Vec<_>>();

        if resource_futs.is_empty() {
            return Ok(());
        }

        let cleanup = async move || -> anyhow::Result<()> {
            let futs = futures::future::join_all(resource_futs)
                .await
                .into_iter()
                // Failed setup most likely means the test failed and problem will be reported in the test status.
                .flat_map(|r| r.into_iter())
                .filter(|s| s.owning_pid().is_some())
                .map(|s| async move {
                    let pid = s.owning_pid().unwrap();
                    try_terminate_process_gracefully(pid, Duration::from_secs(20))
                        .await
                        .context(format!(
                            "Failed to kill a process with `{}` PID to release local resource `{}`",
                            pid,
                            s.source_target()
                        ))
                });

            futures::future::join_all(futs)
                .await
                .into_iter()
                .collect::<Result<_, _>>()?;

            Ok(())
        };

        let start = ReleaseLocalResourcesStart {};
        let end = ReleaseLocalResourcesEnd {};

        span_async(start, async move { (cleanup().await, end) }).await?;

        Ok(())
    }
}
