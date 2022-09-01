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
use buck2_execute::execute::blocking::BlockingExecutor;
use buck2_execute::re::manager::ReConnectionManager;
use gazebo::prelude::*;

/// Stores state handles necessary to produce snapshots.
#[derive(Clone, Dupe)]
pub struct SnapshotCollector {
    re_client_manager: Arc<ReConnectionManager>,
    blocking_executor: Arc<dyn BlockingExecutor>,
    daemon_start_time: Instant,
}

impl SnapshotCollector {
    pub fn new(
        re_client_manager: Arc<ReConnectionManager>,
        blocking_executor: Arc<dyn BlockingExecutor>,
        daemon_start_time: Instant,
    ) -> SnapshotCollector {
        SnapshotCollector {
            re_client_manager,
            blocking_executor,
            daemon_start_time,
        }
    }

    /// We emit a Snapshot before a BaseCommandContext is made available.
    /// Initializes snapshot with all information we don't get off a BaseCommandContext.
    /// This lets us send our first Snapshot before fully initializing/syncing.
    pub fn pre_initialization_snapshot() -> buck2_data::Snapshot {
        let mut snapshot = buck2_data::Snapshot::default();
        add_system_metrics(&mut snapshot);
        snapshot
    }

    /// Create a new Snapshot.
    pub fn create_snapshot(&self) -> buck2_data::Snapshot {
        let mut snapshot = Self::pre_initialization_snapshot();
        self.add_daemon_metrics(&mut snapshot);
        self.add_re_metrics(&mut snapshot);
        snapshot
    }

    fn add_daemon_metrics(&self, snapshot: &mut buck2_data::Snapshot) {
        snapshot.blocking_executor_io_queue_size = self.blocking_executor.queue_size() as u64;
        snapshot.daemon_uptime_s = self.daemon_start_time.elapsed().as_secs();
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

            snapshot.re_download_bytes = stats
                .downloaded
                .try_into()
                .with_context(|| format!("Invalid downloaded bytes: `{}`", stats.downloaded))?;

            snapshot.re_upload_bytes = stats
                .uploaded
                .try_into()
                .with_context(|| format!("Invalid uploaded bytes: `{}`", stats.uploaded))?;

            Ok(())
        }

        // Nothing we can do if we get an error, unfortunately.
        if let Err(e) = inner(snapshot, &self.re_client_manager) {
            tracing::debug!("Error collecting network stats: {:#}", e);
        }
    }
}

#[cfg(unix)]
fn add_system_metrics(snapshot: &mut buck2_data::Snapshot) {
    let usage = unsafe {
        let mut usage: libc::rusage = std::mem::zeroed();
        match libc::getrusage(libc::RUSAGE_SELF, &mut usage as *mut _) {
            0 => usage,
            _ => return,
        }
    };
    // POSIX didn't specify unit of ru_maxrss. Linux uses KB while BSD and
    // OSX use bytes (despite their manpages might say differently).
    let rss_scale = if cfg!(target_os = "linux") {
        1024
    } else {
        // Assume BSD-ish
        1
    };
    fn tv_to_micros(tv: &libc::timeval) -> u64 {
        (1_000_000 * tv.tv_sec as u64) + (tv.tv_usec as u64)
    }

    snapshot.buck2_max_rss = (usage.ru_maxrss as u64) * rss_scale;
    snapshot.buck2_user_cpu_us = tv_to_micros(&usage.ru_utime);
    snapshot.buck2_system_cpu_us = tv_to_micros(&usage.ru_stime);
}

#[cfg(not(unix))]
fn add_system_metrics(_: &mut buck2_data::Snapshot) {}
