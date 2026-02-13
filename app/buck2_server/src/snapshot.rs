/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::HashMap;
use std::sync::Arc;
use std::time::Instant;

use buck2_core::io_counters::IoCounterKey;
use buck2_error::BuckErrorContext;
use buck2_events::EventSinkStats;
use buck2_execute::re::manager::ReConnectionManager;
use buck2_fs::fs_util::DiskSpaceStats;
use buck2_fs::fs_util::disk_space_stats;
use buck2_fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_util::process_stats::process_stats;
use buck2_util::system_stats::UnixSystemStats;
use dupe::Dupe;

use crate::cpu_usage_collector::CpuUsageCollector;
use crate::daemon::state::DaemonStateData;
use crate::jemalloc_stats::get_allocator_stats;
use crate::net_io::NetworkKind;
use crate::net_io::SystemNetworkIoCollector;

/// Stores state handles necessary to produce snapshots.
#[derive(Clone, Dupe)]
pub struct SnapshotCollector {
    daemon: Arc<DaemonStateData>,
    net_io_collector: SystemNetworkIoCollector,
    buck_out_path: Arc<AbsNormPathBuf>,
    cpu_usage_collector: Option<CpuUsageCollector>,
}

impl SnapshotCollector {
    pub fn new(daemon: Arc<DaemonStateData>, buck_out_path: AbsNormPathBuf) -> SnapshotCollector {
        SnapshotCollector {
            daemon,
            net_io_collector: SystemNetworkIoCollector::new(),
            buck_out_path: buck_out_path.into(),
            cpu_usage_collector: CpuUsageCollector::new().ok(),
        }
    }

    /// Create a new Snapshot.
    pub async fn create_snapshot(&self) -> buck2_data::Snapshot {
        let mut snapshot = buck2_data::Snapshot::default();
        self.add_system_metrics(&mut snapshot);
        self.add_daemon_metrics(&mut snapshot);
        self.add_re_metrics(&mut snapshot);
        self.add_http_metrics(&mut snapshot);
        self.add_io_metrics(&mut snapshot);
        self.add_dice_metrics(&mut snapshot);
        self.add_materializer_metrics(&mut snapshot);
        self.add_sink_metrics(&mut snapshot);
        self.add_net_io_metrics(&mut snapshot);
        self.add_cpu_usage(&mut snapshot);
        self.add_memory_metrics(&mut snapshot).await;
        snapshot
    }

    fn add_daemon_metrics(&self, snapshot: &mut buck2_data::Snapshot) {
        snapshot.blocking_executor_io_queue_size =
            self.daemon.blocking_executor.queue_size() as u64;
    }

    fn add_io_metrics(&self, snapshot: &mut buck2_data::Snapshot) {
        let metrics = tokio::runtime::Handle::current().metrics();
        snapshot.tokio_blocking_queue_depth = metrics.blocking_queue_depth() as u64;
        snapshot.tokio_num_idle_blocking_threads = metrics.num_idle_blocking_threads() as u64;
        snapshot.tokio_num_blocking_threads = metrics.num_blocking_threads() as u64;

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

        for key in IoCounterKey::ALL {
            let pointer = match key {
                IoCounterKey::Copy => &mut snapshot.io_copy_count,
                IoCounterKey::Symlink => &mut snapshot.io_symlink_count,
                IoCounterKey::Hardlink => &mut snapshot.io_hardlink_count,
                IoCounterKey::MkDir => &mut snapshot.io_mkdir_count,
                IoCounterKey::ReadDir => &mut snapshot.io_readdir_count,
                IoCounterKey::ReadDirEden => &mut snapshot.io_readdir_eden_count,
                IoCounterKey::RmDir => &mut snapshot.io_rmdir_count,
                IoCounterKey::RmDirAll => &mut snapshot.io_rmdir_all_count,
                IoCounterKey::Stat => &mut snapshot.io_stat_count,
                IoCounterKey::StatEden => &mut snapshot.io_stat_eden_count,
                IoCounterKey::Chmod => &mut snapshot.io_chmod_count,
                IoCounterKey::ReadLink => &mut snapshot.io_readlink_count,
                IoCounterKey::Remove => &mut snapshot.io_remove_count,
                IoCounterKey::Rename => &mut snapshot.io_rename_count,
                IoCounterKey::Read => &mut snapshot.io_read_count,
                IoCounterKey::Write => &mut snapshot.io_write_count,
                IoCounterKey::Canonicalize => &mut snapshot.io_canonicalize_count,
                IoCounterKey::EdenSettle => &mut snapshot.io_eden_settle_count,
            };
            *pointer = Some(key.get_finished());
        }
    }

    fn add_re_metrics(&self, snapshot: &mut buck2_data::Snapshot) {
        fn inner(
            snapshot: &mut buck2_data::Snapshot,
            re: &ReConnectionManager,
        ) -> buck2_error::Result<()> {
            let stats = re
                .get_network_stats()
                .buck_error_context("Error collecting network stats")?;

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

            snapshot.zdb_download_queries = stats.download_stats.zdb.queries;
            snapshot.zdb_download_bytes = stats.download_stats.zdb.bytes;
            snapshot.zdb_upload_queries = stats.upload_stats.zdb.queries;
            snapshot.zdb_upload_bytes = stats.upload_stats.zdb.bytes;

            snapshot.zgateway_download_queries = stats.download_stats.zgateway.queries;
            snapshot.zgateway_download_bytes = stats.download_stats.zgateway.bytes;
            snapshot.zgateway_upload_queries = stats.upload_stats.zgateway.queries;
            snapshot.zgateway_upload_bytes = stats.upload_stats.zgateway.bytes;

            snapshot.manifold_download_queries = stats.download_stats.manifold.queries;
            snapshot.manifold_download_bytes = stats.download_stats.manifold.bytes;
            snapshot.manifold_upload_queries = stats.upload_stats.manifold.queries;
            snapshot.manifold_upload_bytes = stats.upload_stats.manifold.bytes;

            snapshot.hedwig_download_queries = stats.download_stats.hedwig.queries;
            snapshot.hedwig_download_bytes = stats.download_stats.hedwig.bytes;
            snapshot.hedwig_upload_queries = stats.upload_stats.hedwig.queries;
            snapshot.hedwig_upload_bytes = stats.upload_stats.hedwig.bytes;

            snapshot.local_cache_hits_files = stats.local_cache.hits_files;
            snapshot.local_cache_hits_bytes = stats.local_cache.hits_bytes;
            snapshot.local_cache_misses_files = stats.local_cache.misses_files;
            snapshot.local_cache_misses_bytes = stats.local_cache.misses_bytes;

            snapshot.local_cache_hits_files_from_memory_cache = stats.local_cache.hits_from_memory;
            snapshot.local_cache_hits_files_from_filesystem_cache = stats.local_cache.hits_from_fs;

            snapshot.local_cache_lookups = stats.local_cache.cache_lookups;
            snapshot.local_cache_lookup_latency_microseconds =
                stats.local_cache.cache_lookup_latency_microseconds;

            Ok(())
        }

        // Nothing we can do if we get an error, unfortunately.
        if let Err(e) = inner(snapshot, &self.daemon.re_client_manager) {
            tracing::debug!("Error collecting network stats: {:#}", e);
        }
    }

    fn add_http_metrics(&self, snapshot: &mut buck2_data::Snapshot) {
        snapshot.http_download_bytes = self.daemon.http_client.stats().get_downloaded_bytes();
    }

    fn add_dice_metrics(&self, snapshot: &mut buck2_data::Snapshot) {
        let metrics = self.daemon.dice_manager.unsafe_dice().metrics();
        snapshot.dice_key_count = metrics.key_count as u64;
        snapshot.dice_currently_active_key_count = metrics.currently_active_key_count as u64;
        snapshot.dice_active_transaction_count = metrics.active_transaction_count;
    }

    fn add_materializer_metrics(&self, snapshot: &mut buck2_data::Snapshot) {
        self.daemon.materializer.add_snapshot_stats(snapshot);
    }

    fn add_sink_metrics(&self, snapshot: &mut buck2_data::Snapshot) {
        if let Some(metrics) = self.daemon.scribe_sink.as_ref().map(|sink| sink.stats()) {
            let EventSinkStats {
                successes,
                failures_invalid_request,
                failures_unauthorized,
                failures_rate_limited,
                failures_pushed_back,
                failures_enqueue_failed,
                failures_internal_error,
                failures_timed_out,
                failures_unknown,
                buffered,
                dropped,
                bytes_written,
            } = metrics;
            snapshot.sink_successes = Some(successes);
            snapshot.sink_failures = Some(metrics.failures());
            snapshot.sink_failures_invalid_request = Some(failures_invalid_request);
            snapshot.sink_failures_unauthorized = Some(failures_unauthorized);
            snapshot.sink_failures_rate_limited = Some(failures_rate_limited);
            snapshot.sink_failures_pushed_back = Some(failures_pushed_back);
            snapshot.sink_failures_enqueue_failed = Some(failures_enqueue_failed);
            snapshot.sink_failures_internal_error = Some(failures_internal_error);
            snapshot.sink_failures_timed_out = Some(failures_timed_out);
            snapshot.sink_failures_unknown = Some(failures_unknown);
            snapshot.sink_buffer_depth = Some(buffered);
            snapshot.sink_dropped = Some(dropped);
            snapshot.sink_bytes_written = Some(bytes_written);
        }
    }

    fn add_net_io_metrics(&self, snapshot: &mut buck2_data::Snapshot) {
        if let Ok(Some(net_io_counters_per_nic)) = self.net_io_collector.collect() {
            snapshot.network_interface_stats = net_io_counters_per_nic
                .into_iter()
                .map(|(nic, counters)| {
                    (
                        nic,
                        buck2_data::NetworkInterfaceStats {
                            tx_bytes: counters.bytes_sent,
                            rx_bytes: counters.bytes_recv,
                            network_kind: match counters.network_kind {
                                NetworkKind::WiFi => buck2_data::NetworkKind::WiFi.into(),
                                NetworkKind::Ethernet => buck2_data::NetworkKind::Ethernet.into(),
                                NetworkKind::Unknown => {
                                    buck2_data::NetworkKind::UnknownNetKind.into()
                                }
                            },
                        },
                    )
                })
                .collect();
        } else {
            snapshot.network_interface_stats = HashMap::new();
        }
    }

    fn add_system_metrics(&self, snapshot: &mut buck2_data::Snapshot) {
        let process_stats = process_stats();
        if let Some(max_rss_bytes) = process_stats.max_rss_bytes {
            snapshot.buck2_max_rss = max_rss_bytes;
        }
        if let Some(user_cpu_us) = process_stats.user_cpu_us {
            snapshot.buck2_user_cpu_us = user_cpu_us;
        }
        if let Some(system_cpu_us) = process_stats.system_cpu_us {
            snapshot.buck2_system_cpu_us = system_cpu_us;
        }
        snapshot.daemon_uptime_s = (Instant::now() - self.daemon.start_time).as_secs();
        snapshot.buck2_rss = process_stats.rss_bytes;
        let allocator_stats = get_allocator_stats().ok();
        if let Some(alloc_stats) = allocator_stats {
            snapshot.malloc_bytes_active = alloc_stats.bytes_active;
            snapshot.malloc_bytes_allocated = alloc_stats.bytes_allocated;
        }

        if let Ok(DiskSpaceStats {
            total_space,
            free_space,
        }) = disk_space_stats(&*self.buck_out_path)
        {
            snapshot.used_disk_space_bytes = Some(total_space - free_space);
        }

        if let Some(UnixSystemStats {
            load1,
            load5,
            load15,
        }) = UnixSystemStats::get()
        {
            snapshot.unix_system_stats = Some(buck2_data::UnixSystemStats {
                load1,
                load5,
                load15,
            });
        }
    }

    fn add_cpu_usage(&self, snapshot: &mut buck2_data::Snapshot) {
        if let Some(collector) = &self.cpu_usage_collector {
            if let Some(cpu_usage) = collector.get_usage_since_command_start() {
                snapshot.host_cpu_usage_system_ms = Some(cpu_usage.system_millis);
                snapshot.host_cpu_usage_user_ms = Some(cpu_usage.user_millis);
            }
        }
    }

    async fn add_memory_metrics(&self, snapshot: &mut buck2_data::Snapshot) {
        #[cfg(not(unix))]
        {
            let _snapshot = snapshot;
        }
        #[cfg(unix)]
        {
            use buck2_resource_control::cgroup_files::MemoryStat;

            fn convert_stats(stats: &MemoryStat) -> buck2_data::UnixCgroupMemoryStats {
                buck2_data::UnixCgroupMemoryStats {
                    anon: stats.anon,
                    file: stats.file,
                    kernel: stats.kernel,
                }
            }

            // Try to read Buck2 daemon memory information from cgroup

            if let Some(memory_tracker) = self.daemon.memory_tracker.as_ref() {
                let cgroup_tree = &memory_tracker.cgroup_tree;
                if let Ok(stat) = cgroup_tree.allprocs().read_memory_stat().await {
                    snapshot.allprocs_cgroup = Some(convert_stats(&stat))
                }

                if let Ok(stat) = cgroup_tree
                    .forkserver_and_actions()
                    .read_memory_stat()
                    .await
                {
                    snapshot.forkserver_actions_cgroup = Some(convert_stats(&stat))
                }
            }
        }
    }
}
