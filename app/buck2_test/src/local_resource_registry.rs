/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;

use anyhow::Context;
use buck2_common::kill_util::try_terminate_process_gracefully;
use buck2_common::local_resource_state::LocalResourceState;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_data::ReleaseLocalResourcesEnd;
use buck2_data::ReleaseLocalResourcesStart;
use buck2_error::BuckErrorContext;
use buck2_events::dispatch::span_async_simple;
use dice::DiceComputations;
use dice::UserComputationData;
use dupe::Dupe;
use tokio::sync::Mutex;

#[derive(Default)]
pub struct LocalResourceRegistry(
    pub Arc<Mutex<HashMap<ConfiguredTargetLabel, buck2_error::Result<LocalResourceState>>>>,
);

impl LocalResourceRegistry {
    pub(crate) async fn release_all_resources(&self) -> anyhow::Result<()> {
        let resources = {
            let mut lock = self.0.lock().await;
            lock.drain().flat_map(|(_, v)| v).collect::<Vec<_>>()
        };

        if resources.is_empty() {
            return Ok(());
        }

        let cleanup = || async move {
            let resource_futs = resources
                .into_iter()
                .filter(|s| s.owning_pid().is_some())
                .map(|s| async move {
                    let pid = s.owning_pid().unwrap();
                    try_terminate_process_gracefully(pid, Duration::from_secs(20))
                        .await
                        .buck_error_context_anyhow(format!(
                            "Failed to kill a process with `{}` PID to release local resource `{}`",
                            pid,
                            s.source_target()
                        ))
                });

            futures::future::join_all(resource_futs)
                .await
                .into_iter()
                .collect::<Result<(), _>>()?;

            Ok::<(), anyhow::Error>(())
        };

        let start = ReleaseLocalResourcesStart {};
        let end = ReleaseLocalResourcesEnd {};

        span_async_simple(start, cleanup(), end).await?;

        Ok::<(), anyhow::Error>(())
    }
}

pub trait InitLocalResourceRegistry {
    fn init_local_resource_registry(&mut self);
}

pub trait HasLocalResourceRegistry {
    fn get_local_resource_registry(&self) -> anyhow::Result<Arc<LocalResourceRegistry>>;
}

impl InitLocalResourceRegistry for UserComputationData {
    fn init_local_resource_registry(&mut self) {
        self.data.set(Arc::new(LocalResourceRegistry::default()));
    }
}

impl HasLocalResourceRegistry for DiceComputations<'_> {
    fn get_local_resource_registry(&self) -> anyhow::Result<Arc<LocalResourceRegistry>> {
        let data = self
            .per_transaction_data()
            .data
            .get::<Arc<LocalResourceRegistry>>()
            .context("LocalResourceRegistry should be set")?;

        Ok(data.dupe())
    }
}
