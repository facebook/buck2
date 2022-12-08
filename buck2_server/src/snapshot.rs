/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;
use std::time::Instant;

use anyhow::Context as _;
use buck2_common::process_stats::process_stats;
use buck2_core::io_counters::IoCounterKey;
use buck2_execute::execute::blocking::BlockingExecutor;
use buck2_execute::re::manager::ReConnectionManager;
use dice::Dice;
use gazebo::prelude::*;

use crate::jemalloc_stats::get_allocator_stats;

/// Stores state handles necessary to produce snapshots.
#[derive(Clone, Dupe)]
pub struct SnapshotCollector {
    re_client_manager: Arc<ReConnectionManager>,
    blocking_executor: Arc<dyn BlockingExecutor>,
    daemon_start_time: Instant,
    dice: Arc<Dice>,
}

impl SnapshotCollector {
    pub fn new(
        re_client_manager: Arc<ReConnectionManager>,
        blocking_executor: Arc<dyn BlockingExecutor>,
        daemon_start_time: Instant,
        dice: Arc<Dice>,
    ) -> SnapshotCollector {
        SnapshotCollector {
            re_client_manager,
            blocking_executor,
            daemon_start_time,
            dice,
        }
    }

    /// We emit a Snapshot before a BaseCommandContext is made available.
    /// Initializes snapshot with all information we don't get off a BaseCommandContext.
    /// This lets us send our first Snapshot before fully initializing/syncing.
    pub fn pre_initialization_snapshot(daemon_start_time: Instant) -> buck2_data::Snapshot {
        let mut snapshot = buck2_data::Snapshot::default();
        add_system_metrics(&mut snapshot, daemon_start_time);
        snapshot
    }

    /// Create a new Snapshot.
    pub fn create_snapshot(&self) -> buck2_data::Snapshot {
        let mut snapshot = Self::pre_initialization_snapshot(self.daemon_start_time);
        self.add_daemon_metrics(&mut snapshot);
        self.add_re_metrics(&mut snapshot);
        self.add_io_metrics(&mut snapshot);
        self.add_dice_metrics(&mut snapshot);
        snapshot
    }

    fn add_daemon_metrics(&self, snapshot: &mut buck2_data::Snapshot) {
        snapshot.blocking_executor_io_queue_size = self.blocking_executor.queue_size() as u64;
    }

    fn add_io_metrics(&self, snapshot: &mut buck2_data::Snapshot) {
        // Using loop here to make sure no key is forgotten.
        for key in IoCounterKey::ALL {
            let pointer = match key {
                IoCounterKey::Stat => &mut snapshot.io_in_flight_stat,
                IoCounterKey::Copy => &mut snapshot.io_in_flight_copy,
                IoCounterKey::Symlink => &mut snapshot.io_in_flight_symlink,
                IoCounterKey::Hardlink => &mut snapshot.io_in_flight_hardlink,
                IoCounterKey::MkDir => &mut snapshot.io_in_flight_mk_dir,
                IoCounterKey::ReadDir => &mut snapshot.io_in_flight_read_dir,
                IoCounterKey::ReadDirEden => &mut snapshot.io_in_flight_read_dir_eden,
                IoCounterKey::RmDir => &mut snapshot.io_in_flight_rm_dir,
                IoCounterKey::RmDirAll => &mut snapshot.io_in_flight_rm_dir_all,
                IoCounterKey::StatEden => &mut snapshot.io_in_flight_stat_eden,
                IoCounterKey::Chmod => &mut snapshot.io_in_flight_chmod,
                IoCounterKey::ReadLink => &mut snapshot.io_in_flight_read_link,
                IoCounterKey::Remove => &mut snapshot.io_in_flight_remove,
                IoCounterKey::Rename => &mut snapshot.io_in_flight_rename,
                IoCounterKey::Read => &mut snapshot.io_in_flight_read,
                IoCounterKey::Write => &mut snapshot.io_in_flight_write,
                IoCounterKey::Canonicalize => &mut snapshot.io_in_flight_canonicalize,
                IoCounterKey::EdenSettle => &mut snapshot.io_in_flight_eden_settle,
            };
            *pointer = key.get();
        }
    }

    fn add_re_metrics(&self, snapshot: &mut buck2_data::Snapshot) {
        fn inner(
            snapshot: &mut buck2_data::Snapshot,
            re: &ReConnectionManager,
        ) -> anyhow::Result<()> {
            let stats = match re
                .get_network_stats()
                .context("Error collecting network stats")?
            {
                Some(stats) => stats,
                None => return Ok(()),
            };

            snapshot.re_download_bytes = stats.downloaded;
            snapshot.re_upload_bytes = stats.uploaded;
            snapshot.re_uploads_started = stats.uploads.started;
            snapshot.re_uploads_finished_successfully = stats.uploads.finished_successfully;
            snapshot.re_uploads_finished_with_error = stats.uploads.finished_with_error;
            snapshot.re_downloads_started = stats.downloads.started;
            snapshot.re_downloads_finished_successfully = stats.downloads.finished_successfully;
            snapshot.re_downloads_finished_with_error = stats.downloads.finished_with_error;
            snapshot.re_action_cache_started = stats.action_cache.started;
            snapshot.re_action_cache_finished_successfully =
                stats.action_cache.finished_successfully;
            snapshot.re_action_cache_finished_with_error = stats.action_cache.finished_with_error;
            snapshot.re_executes_started = stats.executes.started;
            snapshot.re_executes_finished_successfully = stats.executes.finished_successfully;
            snapshot.re_executes_finished_with_error = stats.executes.finished_with_error;
            snapshot.re_materializes_started = stats.materializes.started;
            snapshot.re_materializes_finished_successfully =
                stats.materializes.finished_successfully;
            snapshot.re_materializes_finished_with_error = stats.materializes.finished_with_error;
            snapshot.re_write_action_results_started = stats.write_action_results.started;
            snapshot.re_write_action_results_finished_successfully =
                stats.write_action_results.finished_successfully;
            snapshot.re_write_action_results_finished_with_error =
                stats.write_action_results.finished_with_error;
            snapshot.re_get_digest_expirations_started = stats.get_digest_expirations.started;
            snapshot.re_get_digest_expirations_finished_successfully =
                stats.get_digest_expirations.finished_successfully;
            snapshot.re_get_digest_expirations_finished_with_error =
                stats.get_digest_expirations.finished_with_error;

            Ok(())
        }

        // Nothing we can do if we get an error, unfortunately.
        if let Err(e) = inner(snapshot, &self.re_client_manager) {
            tracing::debug!("Error collecting network stats: {:#}", e);
        }
    }

    fn add_dice_metrics(&self, snapshot: &mut buck2_data::Snapshot) {
        let metrics = self.dice.metrics();
        snapshot.dice_key_count = metrics.key_count as u64;
        snapshot.dice_currently_running_key_count = metrics.currently_running_key_count as u64;
        snapshot.dice_active_transaction_count = metrics.active_transaction_count;
    }
}

fn add_system_metrics(snapshot: &mut buck2_data::Snapshot, daemon_start_time: Instant) {
    if let Some(stats) = process_stats() {
        snapshot.buck2_max_rss = stats.max_rss_bytes;
        snapshot.buck2_user_cpu_us = stats.user_cpu_us;
        snapshot.buck2_system_cpu_us = stats.system_cpu_us;
        snapshot.daemon_uptime_s = daemon_start_time.elapsed().as_secs();
        snapshot.buck2_rss = stats.rss_bytes;
    }
    let allocator_stats = get_allocator_stats().ok();
    if let Some(alloc_stats) = allocator_stats {
        snapshot.malloc_bytes_active = alloc_stats.bytes_active;
        snapshot.malloc_bytes_allocated = alloc_stats.bytes_allocated;
    }
}
