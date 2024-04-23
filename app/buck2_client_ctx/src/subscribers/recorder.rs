/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cmp;
use std::collections::HashMap;
use std::collections::HashSet;
use std::future::Future;
use std::io::Write;
use std::iter;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;
use std::time::SystemTime;

use anyhow::Context;
use async_trait::async_trait;
use buck2_cli_proto::command_result;
use buck2_common::convert::ProstDurationExt;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_path::AbsPathBuf;
use buck2_data::error::ErrorTag;
use buck2_error::classify::best_tag;
use buck2_error::classify::ERROR_TAG_UNCLASSIFIED;
use buck2_event_observer::action_stats;
use buck2_event_observer::cache_hit_rate::total_cache_hit_rate;
use buck2_event_observer::last_command_execution_kind;
use buck2_event_observer::last_command_execution_kind::LastCommandExecutionKind;
use buck2_events::errors::create_error_report;
use buck2_events::sink::scribe::new_thrift_scribe_sink_if_enabled;
use buck2_events::BuckEvent;
use buck2_util::cleanup_ctx::AsyncCleanupContext;
use buck2_util::system_stats::system_memory_stats;
use buck2_wrapper_common::invocation_id::TraceId;
use dupe::Dupe;
use fbinit::FacebookInit;
use futures::FutureExt;
use gazebo::prelude::VecExt;
use gazebo::variants::VariantName;
use itertools::Itertools;
use termwiz::istty::IsTty;

use crate::build_count::BuildCountManager;
use crate::client_ctx::ClientCommandContext;
use crate::client_metadata::ClientMetadata;
use crate::common::CommonEventLogOptions;
use crate::subscribers::classify_server_stderr::classify_server_stderr;
use crate::subscribers::observer::ErrorObserver;
use crate::subscribers::subscriber::EventSubscriber;

struct ErrorIntermediate {
    processed: buck2_data::ProcessedErrorReport,
    /// Append stderr to the message before sending the report.
    want_stderr: bool,
    best_tag: Option<ErrorTag>,
}

pub(crate) struct InvocationRecorder<'a> {
    fb: FacebookInit,
    write_to_path: Option<AbsPathBuf>,
    command_name: &'static str,
    cli_args: Vec<String>,
    isolation_dir: String,
    start_time: Instant,
    async_cleanup_context: AsyncCleanupContext<'a>,
    build_count_manager: BuildCountManager,
    trace_id: TraceId,
    command_end: Option<buck2_data::CommandEnd>,
    command_duration: Option<prost_types::Duration>,
    re_session_id: Option<String>,
    re_experiment_name: Option<String>,
    critical_path_duration: Option<Duration>,
    tags: Vec<String>,
    run_local_count: u64,
    run_remote_count: u64,
    run_action_cache_count: u64,
    run_remote_dep_file_cache_count: u64,
    run_skipped_count: u64,
    run_fallback_count: u64,
    local_actions_executed_via_worker: u64,
    first_snapshot: Option<buck2_data::Snapshot>,
    last_snapshot: Option<buck2_data::Snapshot>,
    min_build_count_since_rebase: u64,
    cache_upload_count: u64,
    cache_upload_attempt_count: u64,
    parsed_target_patterns: Option<buck2_data::ParsedTargetPatterns>,
    filesystem: String,
    watchman_version: Option<String>,
    eden_version: Option<String>,
    test_info: Option<String>,
    eligible_for_full_hybrid: bool,
    max_event_client_delay: Option<Duration>,
    max_malloc_bytes_active: Option<u64>,
    max_malloc_bytes_allocated: Option<u64>,
    run_command_failure_count: u64,
    event_count: u64,
    time_to_first_action_execution: Option<Duration>,
    materialization_output_size: u64,
    initial_materializer_entries_from_sqlite: Option<u64>,
    time_to_command_start: Option<Duration>,
    time_to_command_critical_section: Option<Duration>,
    time_to_first_analysis: Option<Duration>,
    time_to_load_first_build_file: Option<Duration>,
    time_to_first_command_execution_start: Option<Duration>,
    time_to_first_test_discovery: Option<Duration>,
    system_total_memory_bytes: Option<u64>,
    file_watcher_stats: Option<buck2_data::FileWatcherStats>,
    file_watcher_duration: Option<Duration>,
    time_to_last_action_execution_end: Option<Duration>,
    initial_sink_success_count: Option<u64>,
    initial_sink_failure_count: Option<u64>,
    initial_sink_dropped_count: Option<u64>,
    sink_max_buffer_depth: u64,
    soft_error_categories: HashSet<String>,
    concurrent_command_blocking_duration: Option<Duration>,
    metadata: HashMap<String, String>,
    analysis_count: u64,
    daemon_in_memory_state_is_corrupted: bool,
    daemon_materializer_state_is_corrupted: bool,
    enable_restarter: bool,
    restarted_trace_id: Option<TraceId>,
    has_command_result: bool,
    has_end_of_stream: bool,
    compressed_event_log_size_bytes: Option<Arc<AtomicU64>>,
    critical_path_backend: Option<String>,
    instant_command_is_success: Option<bool>,
    bxl_ensure_artifacts_duration: Option<prost_types::Duration>,
    initial_re_upload_bytes: Option<u64>,
    initial_re_download_bytes: Option<u64>,
    concurrent_command_ids: HashSet<String>,
    daemon_connection_failure: bool,
    /// Daemon started by this command.
    daemon_was_started: Option<buck2_data::DaemonWasStartedReason>,
    client_metadata: Vec<buck2_data::ClientMetadata>,
    errors: Vec<ErrorIntermediate>,
    /// To append to gRPC errors.
    server_stderr: String,
    target_rule_type_names: Vec<String>,
    new_configs_used: bool,
}

impl<'a> InvocationRecorder<'a> {
    pub fn new(
        fb: FacebookInit,
        async_cleanup_context: AsyncCleanupContext<'a>,
        write_to_path: Option<AbsPathBuf>,
        command_name: &'static str,
        sanitized_argv: Vec<String>,
        trace_id: TraceId,
        isolation_dir: String,
        build_count_manager: BuildCountManager,
        filesystem: String,
        restarted_trace_id: Option<TraceId>,
        log_size_counter_bytes: Option<Arc<AtomicU64>>,
        client_metadata: Vec<buck2_data::ClientMetadata>,
    ) -> Self {
        Self {
            fb,
            write_to_path,
            command_name,
            cli_args: sanitized_argv,
            isolation_dir,
            start_time: Instant::now(),
            async_cleanup_context,
            build_count_manager,
            trace_id,
            command_end: None,
            command_duration: None,
            re_session_id: None,
            re_experiment_name: None,
            critical_path_duration: None,
            tags: vec![],
            run_local_count: 0,
            run_remote_count: 0,
            run_action_cache_count: 0,
            run_remote_dep_file_cache_count: 0,
            run_skipped_count: 0,
            run_fallback_count: 0,
            local_actions_executed_via_worker: 0,
            first_snapshot: None,
            last_snapshot: None,
            min_build_count_since_rebase: 0,
            cache_upload_count: 0,
            cache_upload_attempt_count: 0,
            parsed_target_patterns: None,
            filesystem,
            watchman_version: None,
            eden_version: None,
            test_info: None,
            eligible_for_full_hybrid: false,
            max_event_client_delay: None,
            max_malloc_bytes_active: None,
            max_malloc_bytes_allocated: None,
            run_command_failure_count: 0,
            event_count: 0,
            time_to_first_action_execution: None,
            materialization_output_size: 0,
            initial_materializer_entries_from_sqlite: None,
            time_to_command_start: None,
            time_to_command_critical_section: None,
            time_to_first_analysis: None,
            time_to_load_first_build_file: None,
            time_to_first_command_execution_start: None,
            time_to_first_test_discovery: None,
            system_total_memory_bytes: Some(system_memory_stats()),
            file_watcher_stats: None,
            file_watcher_duration: None,
            time_to_last_action_execution_end: None,
            initial_sink_success_count: None,
            initial_sink_failure_count: None,
            initial_sink_dropped_count: None,
            sink_max_buffer_depth: 0,
            soft_error_categories: HashSet::new(),
            concurrent_command_blocking_duration: None,
            metadata: buck2_events::metadata::collect(),
            analysis_count: 0,
            daemon_in_memory_state_is_corrupted: false,
            daemon_materializer_state_is_corrupted: false,
            enable_restarter: false,
            restarted_trace_id,
            has_command_result: false,
            has_end_of_stream: false,
            compressed_event_log_size_bytes: log_size_counter_bytes,
            critical_path_backend: None,
            instant_command_is_success: None,
            bxl_ensure_artifacts_duration: None,
            initial_re_upload_bytes: None,
            initial_re_download_bytes: None,
            concurrent_command_ids: HashSet::new(),
            daemon_connection_failure: false,
            daemon_was_started: None,
            client_metadata,
            errors: Vec::new(),
            server_stderr: String::new(),
            target_rule_type_names: Vec::new(),
            new_configs_used: false,
        }
    }

    pub fn instant_command_outcome(&mut self, is_success: bool) {
        self.instant_command_is_success = Some(is_success);
    }

    async fn build_count(&mut self, is_success: bool, command_name: &str) -> anyhow::Result<u64> {
        if let Some(stats) = &self.file_watcher_stats {
            if let Some(merge_base) = &stats.branched_from_revision {
                match &self.parsed_target_patterns {
                    None => {
                        if is_success {
                            return Err(anyhow::anyhow!(
                                "successful {} commands should have resolved target patterns",
                                command_name
                            ));
                        }
                        // fallthrough to 0 below
                    }
                    Some(v) => {
                        return self
                            .build_count_manager
                            .min_build_count(merge_base, v, is_success)
                            .await
                            .context("Error recording build count");
                    }
                };
            }
        }

        Ok(0)
    }

    fn maybe_add_server_stderr_to_errors(&mut self) {
        for error in &mut self.errors {
            if !error.want_stderr {
                continue;
            }

            if error.processed.message.is_empty() {
                error.processed.message =
                    "Error is empty? But it is too late to do anything about it\n".to_owned();
            } else if !error.processed.message.ends_with('\n') {
                error.processed.message.push('\n');
            }

            error.processed.message.push('\n');

            if self.server_stderr.is_empty() {
                error.processed.message.push_str("buckd stderr is empty\n");
            } else {
                error.processed.message.push_str("buckd stderr:\n");
                // Scribe sink truncates messages, but here we can do it better:
                // - truncate even if total message is not large enough
                // - truncate stderr, but keep the error message
                let server_stderr = truncate_stderr(&self.server_stderr);
                error.processed.message.push_str(server_stderr);
            }

            let stderr_tag = classify_server_stderr(&self.server_stderr);
            // Note: side effect, `best_error_tag` must be called after this function.
            error.best_tag = best_tag(error.best_tag.into_iter().chain(iter::once(stderr_tag)));
            error
                .processed
                .tags
                .push(stderr_tag.as_str_name().to_owned());
        }
    }

    fn best_error_tag(&self) -> Option<&'static str> {
        if self.errors.is_empty() {
            None
        } else {
            Some(
                best_tag(self.errors.iter().filter_map(|e| e.best_tag)).map_or(
                    // If we don't have tags on the errors,
                    // we still want to add a tag to Scuba column.
                    ERROR_TAG_UNCLASSIFIED,
                    |t| t.as_str_name(),
                ),
            )
        }
    }

    fn send_it(&mut self) -> Option<impl Future<Output = ()> + 'static + Send> {
        self.maybe_add_server_stderr_to_errors();

        // `None` if no errors, `Some("UNCLASSIFIED")` if no tags.
        let best_error_tag = self.best_error_tag();

        let mut sink_success_count = None;
        let mut sink_failure_count = None;
        let mut sink_dropped_count = None;
        let mut re_upload_bytes = None;
        let mut re_download_bytes = None;
        if let Some(snapshot) = &self.last_snapshot {
            sink_success_count =
                calculate_diff_if_some(&snapshot.sink_successes, &self.initial_sink_success_count);
            sink_failure_count =
                calculate_diff_if_some(&snapshot.sink_failures, &self.initial_sink_failure_count);
            sink_dropped_count =
                calculate_diff_if_some(&snapshot.sink_dropped, &self.initial_sink_dropped_count);
            re_upload_bytes = calculate_diff_if_some(
                &Some(snapshot.re_upload_bytes),
                &self.initial_re_upload_bytes,
            );
            re_download_bytes = calculate_diff_if_some(
                &Some(snapshot.re_download_bytes),
                &self.initial_re_download_bytes,
            );
        }

        let mut metadata = Self::default_metadata();
        metadata.strings.extend(std::mem::take(&mut self.metadata));

        let record = buck2_data::InvocationRecord {
            command_name: Some(self.command_name.to_owned()),
            command_end: self.command_end.take(),
            command_duration: self.command_duration.take(),
            client_walltime: self.start_time.elapsed().try_into().ok(),
            re_session_id: self.re_session_id.take().unwrap_or_default(),
            re_experiment_name: self.re_experiment_name.take().unwrap_or_default(),
            cli_args: self.cli_args.clone(),
            critical_path_duration: self.critical_path_duration.and_then(|x| x.try_into().ok()),
            metadata: Some(metadata),
            tags: self.tags.drain(..).collect(),
            run_local_count: self.run_local_count,
            run_remote_count: self.run_remote_count,
            run_action_cache_count: self.run_action_cache_count,
            run_remote_dep_file_cache_count: self.run_remote_dep_file_cache_count,
            cache_hit_rate: total_cache_hit_rate(
                self.run_local_count,
                self.run_remote_count,
                self.run_action_cache_count,
                self.run_remote_dep_file_cache_count,
            ) as f32,
            run_skipped_count: self.run_skipped_count,
            run_fallback_count: Some(self.run_fallback_count),
            local_actions_executed_via_worker: Some(self.local_actions_executed_via_worker),
            first_snapshot: self.first_snapshot.take(),
            last_snapshot: self.last_snapshot.take(),
            min_build_count_since_rebase: self.min_build_count_since_rebase,
            cache_upload_count: self.cache_upload_count,
            cache_upload_attempt_count: self.cache_upload_attempt_count,
            parsed_target_patterns: self.parsed_target_patterns.take(),
            filesystem: std::mem::take(&mut self.filesystem),
            watchman_version: self.watchman_version.take(),
            eden_version: self.eden_version.take(),
            test_info: self.test_info.take(),
            eligible_for_full_hybrid: Some(self.eligible_for_full_hybrid),
            max_event_client_delay_ms: self
                .max_event_client_delay
                .and_then(|d| u64::try_from(d.as_millis()).ok()),
            max_malloc_bytes_active: self.max_malloc_bytes_active.take(),
            max_malloc_bytes_allocated: self.max_malloc_bytes_allocated.take(),
            run_command_failure_count: Some(self.run_command_failure_count),
            event_count: Some(self.event_count),
            time_to_first_action_execution_ms: self
                .time_to_first_action_execution
                .and_then(|d| u64::try_from(d.as_millis()).ok()),
            materialization_output_size: Some(self.materialization_output_size),
            initial_materializer_entries_from_sqlite: self.initial_materializer_entries_from_sqlite,
            time_to_command_start_ms: self
                .time_to_command_start
                .and_then(|d| u64::try_from(d.as_millis()).ok()),
            time_to_command_critical_section_ms: self
                .time_to_command_critical_section
                .and_then(|d| u64::try_from(d.as_millis()).ok()),
            time_to_first_analysis_ms: self
                .time_to_first_analysis
                .and_then(|d| u64::try_from(d.as_millis()).ok()),
            time_to_load_first_build_file_ms: self
                .time_to_load_first_build_file
                .and_then(|d| u64::try_from(d.as_millis()).ok()),
            time_to_first_command_execution_start_ms: self
                .time_to_first_command_execution_start
                .and_then(|d| u64::try_from(d.as_millis()).ok()),
            time_to_first_test_discovery_ms: self
                .time_to_first_test_discovery
                .and_then(|d| u64::try_from(d.as_millis()).ok()),
            system_total_memory_bytes: self.system_total_memory_bytes,
            file_watcher_stats: self.file_watcher_stats.take(),
            file_watcher_duration_ms: self
                .file_watcher_duration
                .and_then(|d| u64::try_from(d.as_millis()).ok()),
            time_to_last_action_execution_end_ms: self
                .time_to_last_action_execution_end
                .and_then(|d| u64::try_from(d.as_millis()).ok()),
            isolation_dir: Some(self.isolation_dir.clone()),
            sink_success_count,
            sink_failure_count,
            sink_dropped_count,
            sink_max_buffer_depth: Some(self.sink_max_buffer_depth),
            soft_error_categories: std::mem::take(&mut self.soft_error_categories)
                .into_iter()
                .collect(),
            concurrent_command_blocking_duration: self
                .concurrent_command_blocking_duration
                .and_then(|x| x.try_into().ok()),
            analysis_count: Some(self.analysis_count),
            restarted_trace_id: self.restarted_trace_id.as_ref().map(|t| t.to_string()),
            has_command_result: Some(self.has_command_result),
            has_end_of_stream: Some(self.has_end_of_stream),
            // At this point we expect the event log writer to have finished
            compressed_event_log_size_bytes: Some(
                self.compressed_event_log_size_bytes
                    .as_ref()
                    .map(|x| x.load(Ordering::Relaxed))
                    .unwrap_or_default(),
            ),
            critical_path_backend: self.critical_path_backend.take(),
            instant_command_is_success: self.instant_command_is_success.take(),
            bxl_ensure_artifacts_duration: self.bxl_ensure_artifacts_duration.take(),
            re_upload_bytes,
            re_download_bytes,
            concurrent_command_ids: std::mem::take(&mut self.concurrent_command_ids)
                .into_iter()
                .collect(),
            daemon_connection_failure: Some(self.daemon_connection_failure),
            daemon_was_started: self.daemon_was_started.map(|t| t as i32),
            client_metadata: std::mem::take(&mut self.client_metadata),
            errors: std::mem::take(&mut self.errors).into_map(|e| e.processed),
            best_error_tag: best_error_tag.map(|t| t.to_owned()),
            target_rule_type_names: std::mem::take(&mut self.target_rule_type_names),
            new_configs_used: Some(self.new_configs_used),
        };

        let event = BuckEvent::new(
            SystemTime::now(),
            self.trace_id.dupe(),
            None,
            None,
            buck2_data::RecordEvent {
                data: Some((Box::new(record)).into()),
            }
            .into(),
        );

        if let Some(path) = &self.write_to_path {
            let res = (|| {
                let out = fs_util::create_file(path).context("Error opening")?;
                let mut out = std::io::BufWriter::new(out);
                serde_json::to_writer(&mut out, event.event()).context("Error writing")?;
                out.flush().context("Error flushing")?;
                anyhow::Ok(())
            })();

            if let Err(e) = &res {
                tracing::warn!(
                    "Failed to write InvocationRecord to `{}`: {:#}",
                    path.as_path().display(),
                    e
                );
            }
        }

        if let Ok(Some(scribe_sink)) =
            new_thrift_scribe_sink_if_enabled(self.fb, 1, Duration::from_millis(500), 5, None)
        {
            tracing::info!("Recording invocation to Scribe: {:?}", &event);
            Some(async move {
                scribe_sink.send_now(event).await;
            })
        } else {
            tracing::info!("Invocation record is not sent to Scribe: {:?}", &event);
            None
        }
    }

    // Collects client-side state and data, suitable for telemetry.
    // NOTE: If data is visible from the daemon, put it in cli::metadata::collect()
    fn default_metadata() -> buck2_data::TypedMetadata {
        let mut ints = HashMap::new();
        ints.insert("is_tty".to_owned(), std::io::stderr().is_tty() as i64);
        buck2_data::TypedMetadata {
            ints,
            strings: HashMap::new(),
        }
    }

    fn handle_command_start(
        &mut self,
        command: &buck2_data::CommandStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        self.metadata.extend(command.metadata.clone());
        self.time_to_command_start = Some(self.start_time.elapsed());
        Ok(())
    }

    async fn handle_command_end(
        &mut self,
        command: &buck2_data::CommandEnd,
        event: &BuckEvent,
    ) -> anyhow::Result<()> {
        let mut command = command.clone();
        self.errors
            .extend(std::mem::take(&mut command.errors).into_iter().map(|e| {
                let best_tag = best_tag(e.tags.iter().filter_map(|t| {
                    // This should never be `None`, but with weak prost types,
                    // it is safer to just ignore incorrect integers.
                    ErrorTag::from_i32(*t)
                }));
                ErrorIntermediate {
                    processed: process_error_report(e),
                    want_stderr: false,
                    best_tag,
                }
            }));

        // Awkwardly unpacks the SpanEnd event so we can read its duration.
        let command_end = match event.data() {
            buck2_data::buck_event::Data::SpanEnd(ref end) => end.clone(),
            _ => {
                return Err(anyhow::anyhow!(
                    "handle_command_end was passed a CommandEnd not contained in a SpanEndEvent"
                ));
            }
        };
        self.command_duration = command_end.duration;
        let command_data = command.data.as_ref().context("Missing command data")?;
        self.min_build_count_since_rebase = match command_data {
            buck2_data::command_end::Data::Build(..)
            | buck2_data::command_end::Data::Test(..)
            | buck2_data::command_end::Data::Install(..) => {
                self.build_count(command.is_success, command_data.variant_name())
                    .await?
            }
            // other events don't count builds
            _ => 0,
        };
        self.command_end = Some(command);
        Ok(())
    }
    fn handle_command_critical_start(
        &mut self,
        command: &buck2_data::CommandCriticalStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        self.metadata.extend(command.metadata.clone());
        self.time_to_command_critical_section = Some(self.start_time.elapsed());
        Ok(())
    }
    fn handle_command_critical_end(
        &mut self,
        command: &buck2_data::CommandCriticalEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        self.metadata.extend(command.metadata.clone());
        Ok(())
    }

    fn handle_action_execution_start(
        &mut self,
        _action: &buck2_data::ActionExecutionStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        if self.time_to_first_action_execution.is_none() {
            self.time_to_first_action_execution = Some(self.start_time.elapsed());
        }
        Ok(())
    }
    fn handle_action_execution_end(
        &mut self,
        action: &buck2_data::ActionExecutionEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        if action.kind == buck2_data::ActionKind::Run as i32 {
            if action_stats::was_fallback_action(action) {
                self.run_fallback_count += 1;
            }

            match last_command_execution_kind::get_last_command_execution_kind(action) {
                LastCommandExecutionKind::Local => {
                    self.run_local_count += 1;
                }
                LastCommandExecutionKind::LocalWorker => {
                    self.run_local_count += 1;
                    self.local_actions_executed_via_worker += 1;
                }
                LastCommandExecutionKind::Cached => {
                    self.run_action_cache_count += 1;
                }
                LastCommandExecutionKind::RemoteDepFileCached => {
                    self.run_remote_dep_file_cache_count += 1;
                }
                LastCommandExecutionKind::Remote => {
                    self.run_remote_count += 1;
                }
                LastCommandExecutionKind::NoCommand => {
                    self.run_skipped_count += 1;
                }
            }
        }

        if action.eligible_for_full_hybrid.unwrap_or_default() {
            self.eligible_for_full_hybrid = true;
        }

        if action.commands.iter().any(|c| {
            matches!(
                c.status,
                Some(buck2_data::command_execution::Status::Failure(..))
            )
        }) {
            self.run_command_failure_count += 1;
        }

        self.time_to_last_action_execution_end = Some(self.start_time.elapsed());

        Ok(())
    }

    fn handle_analysis_start(
        &mut self,
        _analysis: &buck2_data::AnalysisStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        self.time_to_first_analysis
            .get_or_insert_with(|| self.start_time.elapsed());
        Ok(())
    }

    fn handle_load_start(
        &mut self,
        _eval: &buck2_data::LoadBuildFileStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        self.time_to_load_first_build_file
            .get_or_insert_with(|| self.start_time.elapsed());
        Ok(())
    }

    fn handle_executor_stage_start(
        &mut self,
        executor_stage: &buck2_data::ExecutorStageStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        match &executor_stage.stage {
            Some(buck2_data::executor_stage_start::Stage::Re(re_stage)) => match &re_stage.stage {
                Some(buck2_data::re_stage::Stage::Execute(_)) => {
                    self.time_to_first_command_execution_start
                        .get_or_insert_with(|| self.start_time.elapsed());
                }
                _ => {}
            },
            Some(buck2_data::executor_stage_start::Stage::Local(local_stage)) => {
                match &local_stage.stage {
                    Some(buck2_data::local_stage::Stage::Execute(_)) => {
                        self.time_to_first_command_execution_start
                            .get_or_insert_with(|| self.start_time.elapsed());
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn handle_cache_upload_end(
        &mut self,
        cache_upload: &buck2_data::CacheUploadEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        if cache_upload.success {
            self.cache_upload_count += 1;
        }
        self.cache_upload_attempt_count += 1;
        Ok(())
    }

    fn handle_re_session_created(
        &mut self,
        session: &buck2_data::RemoteExecutionSessionCreated,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        self.re_session_id = Some(session.session_id.clone());
        self.re_experiment_name = Some(session.experiment_name.clone());
        Ok(())
    }

    fn handle_materialization_end(
        &mut self,
        materialization: &buck2_data::MaterializationEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        self.materialization_output_size += materialization.total_bytes;
        Ok(())
    }

    fn handle_materializer_state_info(
        &mut self,
        materializer_state_info: &buck2_data::MaterializerStateInfo,
    ) -> anyhow::Result<()> {
        self.initial_materializer_entries_from_sqlite =
            Some(materializer_state_info.num_entries_from_sqlite);
        Ok(())
    }

    fn handle_bxl_ensure_artifacts_end(
        &mut self,
        _bxl_ensure_artifacts_end: &buck2_data::BxlEnsureArtifactsEnd,
        event: &BuckEvent,
    ) -> anyhow::Result<()> {
        let bxl_ensure_artifacts_end = match event.data() {
            buck2_data::buck_event::Data::SpanEnd(ref end) => end.clone(),
            _ => {
                return Err(anyhow::anyhow!(
                    "handle_bxl_ensure_artifacts_end was passed a BxlEnsureArtifacts not contained in a SpanEndEvent"
                ));
            }
        };

        self.bxl_ensure_artifacts_duration = bxl_ensure_artifacts_end.duration;
        Ok(())
    }

    fn handle_test_discovery(
        &mut self,
        test_info: &buck2_data::TestDiscovery,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        match &test_info.data {
            Some(buck2_data::test_discovery::Data::Session(session_info)) => {
                self.test_info = Some(session_info.info.clone());
            }
            Some(buck2_data::test_discovery::Data::Tests(..)) | None => {}
        }

        Ok(())
    }

    fn handle_test_discovery_start(
        &mut self,
        _test_discovery: &buck2_data::TestDiscoveryStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        self.time_to_first_test_discovery
            .get_or_insert_with(|| self.start_time.elapsed());
        Ok(())
    }

    fn handle_build_graph_info(
        &mut self,
        info: &buck2_data::BuildGraphExecutionInfo,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        let mut duration = Duration::default();

        for node in &info.critical_path {
            if let Some(d) = &node.duration {
                duration += d.try_into_duration()?;
            }
        }

        for node in &info.critical_path2 {
            if let Some(d) = &node.duration {
                duration += d.try_into_duration()?;
            }
        }

        self.critical_path_duration = Some(duration);
        self.critical_path_backend = info.backend_name.clone();
        Ok(())
    }

    fn handle_io_provider_info(
        &mut self,
        io_provider_info: &buck2_data::IoProviderInfo,
    ) -> anyhow::Result<()> {
        self.eden_version = io_provider_info.eden_version.to_owned();
        Ok(())
    }

    fn handle_tag(&mut self, tag: &buck2_data::TagEvent) -> anyhow::Result<()> {
        self.tags.extend(tag.tags.iter().cloned());
        Ok(())
    }

    fn handle_concurrent_commands(
        &mut self,
        concurrent_commands: &buck2_data::ConcurrentCommands,
    ) -> anyhow::Result<()> {
        concurrent_commands.trace_ids.iter().for_each(|c| {
            self.concurrent_command_ids.insert(c.clone());
        });
        Ok(())
    }

    fn handle_snapshot(
        &mut self,
        update: &buck2_data::Snapshot,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        self.max_malloc_bytes_active =
            cmp::max(self.max_malloc_bytes_active, update.malloc_bytes_active);
        self.max_malloc_bytes_allocated = cmp::max(
            self.max_malloc_bytes_allocated,
            update.malloc_bytes_allocated,
        );
        if self.first_snapshot.is_none() {
            self.first_snapshot = Some(update.clone());
        } else {
            self.last_snapshot = Some(update.clone());
        }
        if self.initial_sink_success_count.is_none() {
            self.initial_sink_success_count = update.sink_successes;
        }
        if self.initial_sink_failure_count.is_none() {
            self.initial_sink_failure_count = update.sink_failures;
        }
        if self.initial_sink_dropped_count.is_none() {
            self.initial_sink_dropped_count = update.sink_dropped;
        }
        self.sink_max_buffer_depth =
            cmp::max(self.sink_max_buffer_depth, update.sink_buffer_depth());

        if self.initial_re_upload_bytes.is_none() {
            self.initial_re_upload_bytes = Some(update.re_upload_bytes);
        }
        if self.initial_re_download_bytes.is_none() {
            self.initial_re_download_bytes = Some(update.re_download_bytes);
        }

        Ok(())
    }

    fn handle_file_watcher_end(
        &mut self,
        file_watcher: &buck2_data::FileWatcherEnd,
        duration: Option<&prost_types::Duration>,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        // We might receive this event twice, so ... deal with it by merging the two.
        // See: https://fb.workplace.com/groups/buck2dev/permalink/3396726613948720/
        self.file_watcher_stats =
            merge_file_watcher_stats(self.file_watcher_stats.take(), file_watcher.stats.clone());
        if let Some(duration) = duration.cloned().and_then(|x| Duration::try_from(x).ok()) {
            *self.file_watcher_duration.get_or_insert_default() += duration;
        }
        if let Some(stats) = &file_watcher.stats {
            self.watchman_version = stats.watchman_version.to_owned();
        }
        Ok(())
    }

    fn handle_parsed_target_patterns(
        &mut self,
        patterns: &buck2_data::ParsedTargetPatterns,
    ) -> anyhow::Result<()> {
        self.parsed_target_patterns = Some(patterns.clone());
        Ok(())
    }

    fn handle_structured_error(&mut self, err: &buck2_data::StructuredError) -> anyhow::Result<()> {
        if let Some(soft_error_category) = err.soft_error_category.as_ref() {
            self.soft_error_categories
                .insert(soft_error_category.to_owned());

            if err.daemon_in_memory_state_is_corrupted {
                self.daemon_in_memory_state_is_corrupted = true;
            }

            if err.daemon_materializer_state_is_corrupted {
                self.daemon_materializer_state_is_corrupted = true;
            }
        }

        Ok(())
    }

    fn handle_dice_block_concurrent_command_end(
        &mut self,
        _command: &buck2_data::DiceBlockConcurrentCommandEnd,
        event: &BuckEvent,
    ) -> anyhow::Result<()> {
        let block_concurrent_command = match event.data() {
            buck2_data::buck_event::Data::SpanEnd(ref end) => end.clone(),
            _ => {
                return Err(anyhow::anyhow!(
                    "handle_dice_block_concurrent_command_end was passed a DiceBlockConcurrentCommandEnd not contained in a SpanEndEvent"
                ));
            }
        };

        let mut duration = self
            .concurrent_command_blocking_duration
            .unwrap_or_default();
        if let Some(d) = &block_concurrent_command.duration {
            duration += d.try_into_duration()?;
        }

        self.concurrent_command_blocking_duration = Some(duration);

        Ok(())
    }

    fn handle_dice_cleanup_end(
        &mut self,
        _command: &buck2_data::DiceCleanupEnd,
        event: &BuckEvent,
    ) -> anyhow::Result<()> {
        let dice_cleanup_end = match event.data() {
            buck2_data::buck_event::Data::SpanEnd(ref end) => end.clone(),
            _ => {
                return Err(anyhow::anyhow!(
                    "handle_dice_cleanup_end was passed a DiceCleanupEnd not contained in a SpanEndEvent"
                ));
            }
        };

        let mut duration = self
            .concurrent_command_blocking_duration
            .unwrap_or_default();
        if let Some(d) = &dice_cleanup_end.duration {
            duration += d.try_into_duration()?;
        }

        self.concurrent_command_blocking_duration = Some(duration);

        Ok(())
    }

    async fn handle_event(&mut self, event: &Arc<BuckEvent>) -> anyhow::Result<()> {
        // TODO(nga): query now once in `EventsCtx`.
        let now = SystemTime::now();
        if let Ok(delay) = now.duration_since(event.timestamp()) {
            self.max_event_client_delay = Some(cmp::max(
                self.max_event_client_delay.unwrap_or_default(),
                delay,
            ));
        }
        self.event_count += 1;

        match event.data() {
            buck2_data::buck_event::Data::SpanStart(ref start) => {
                match start.data.as_ref().context("Missing `start`")? {
                    buck2_data::span_start_event::Data::Command(command) => {
                        self.handle_command_start(command, event)
                    }
                    buck2_data::span_start_event::Data::CommandCritical(command) => {
                        self.handle_command_critical_start(command, event)
                    }
                    buck2_data::span_start_event::Data::ActionExecution(action) => {
                        self.handle_action_execution_start(action, event)
                    }
                    buck2_data::span_start_event::Data::Analysis(analysis) => {
                        self.handle_analysis_start(analysis, event)
                    }
                    buck2_data::span_start_event::Data::Load(eval) => {
                        self.handle_load_start(eval, event)
                    }
                    buck2_data::span_start_event::Data::ExecutorStage(stage) => {
                        self.handle_executor_stage_start(stage, event)
                    }
                    buck2_data::span_start_event::Data::TestDiscovery(test_discovery) => {
                        self.handle_test_discovery_start(test_discovery, event)
                    }
                    _ => Ok(()),
                }
            }
            buck2_data::buck_event::Data::SpanEnd(ref end) => {
                match end.data.as_ref().context("Missing `end`")? {
                    buck2_data::span_end_event::Data::Command(command) => {
                        self.handle_command_end(command, event).await
                    }
                    buck2_data::span_end_event::Data::CommandCritical(command) => {
                        self.handle_command_critical_end(command, event)
                    }
                    buck2_data::span_end_event::Data::ActionExecution(action) => {
                        self.handle_action_execution_end(action, event)
                    }
                    buck2_data::span_end_event::Data::FileWatcher(file_watcher) => {
                        self.handle_file_watcher_end(file_watcher, end.duration.as_ref(), event)
                    }
                    buck2_data::span_end_event::Data::CacheUpload(cache_upload) => {
                        self.handle_cache_upload_end(cache_upload, event)
                    }
                    buck2_data::span_end_event::Data::Materialization(materialization) => {
                        self.handle_materialization_end(materialization, event)
                    }
                    buck2_data::span_end_event::Data::Analysis(..) => {
                        self.analysis_count += 1;
                        Ok(())
                    }
                    buck2_data::span_end_event::Data::DiceBlockConcurrentCommand(
                        block_concurrent_command,
                    ) => self
                        .handle_dice_block_concurrent_command_end(block_concurrent_command, event),
                    buck2_data::span_end_event::Data::DiceCleanup(dice_cleanup_end) => {
                        self.handle_dice_cleanup_end(dice_cleanup_end, event)
                    }
                    buck2_data::span_end_event::Data::BxlEnsureArtifacts(_bxl_ensure_artifacts) => {
                        self.handle_bxl_ensure_artifacts_end(_bxl_ensure_artifacts, event)
                    }
                    _ => Ok(()),
                }
            }
            buck2_data::buck_event::Data::Instant(ref instant) => {
                match instant.data.as_ref().context("Missing `data`")? {
                    buck2_data::instant_event::Data::ReSession(session) => {
                        self.handle_re_session_created(session, event)
                    }
                    buck2_data::instant_event::Data::BuildGraphInfo(info) => {
                        self.handle_build_graph_info(info, event)
                    }
                    buck2_data::instant_event::Data::TestDiscovery(discovery) => {
                        self.handle_test_discovery(discovery, event)
                    }
                    buck2_data::instant_event::Data::Snapshot(result) => {
                        self.handle_snapshot(result, event)
                    }
                    buck2_data::instant_event::Data::TagEvent(tag) => self.handle_tag(tag),
                    buck2_data::instant_event::Data::IoProviderInfo(io_provider_info) => {
                        self.handle_io_provider_info(io_provider_info)
                    }
                    buck2_data::instant_event::Data::TargetPatterns(tag) => {
                        self.handle_parsed_target_patterns(tag)
                    }
                    buck2_data::instant_event::Data::MaterializerStateInfo(materializer_state) => {
                        self.handle_materializer_state_info(materializer_state)
                    }
                    buck2_data::instant_event::Data::StructuredError(err) => {
                        self.handle_structured_error(err)
                    }
                    buck2_data::instant_event::Data::RestartConfiguration(conf) => {
                        self.enable_restarter = conf.enable_restarter;
                        Ok(())
                    }
                    buck2_data::instant_event::Data::ConcurrentCommands(concurrent_commands) => {
                        self.handle_concurrent_commands(concurrent_commands)
                    }
                    buck2_data::instant_event::Data::BuckConfigs(conf) => {
                        self.new_configs_used = conf.new_configs_used;
                        Ok(())
                    }
                    _ => Ok(()),
                }
            }
            buck2_data::buck_event::Data::Record(_) => Ok(()),
        }
    }
}

fn process_error_report(error: buck2_data::ErrorReport) -> buck2_data::ProcessedErrorReport {
    let best_tag = best_tag(error.tags.iter().filter_map(|tag|
        // This should never fail, but it is safer to just ignore incorrect integers.
        ErrorTag::from_i32(*tag)))
    .map(|t| t.as_str_name())
    .unwrap_or(ERROR_TAG_UNCLASSIFIED);
    buck2_data::ProcessedErrorReport {
        tier: error.tier,
        message: error.message,
        telemetry_message: error.telemetry_message,
        typ: error
            .typ
            .and_then(buck2_data::error::ErrorType::from_i32)
            .map(|t| t.as_str_name().to_owned()),
        source_location: error.source_location,
        tags: error
            .tags
            .iter()
            .copied()
            .filter_map(buck2_data::error::ErrorTag::from_i32)
            .map(|t| t.as_str_name().to_owned())
            .collect(),
        best_tag: Some(best_tag.to_owned()),
    }
}

impl<'a> Drop for InvocationRecorder<'a> {
    fn drop(&mut self) {
        if let Some(fut) = self.send_it() {
            self.async_cleanup_context
                .register("sending invocation to Scribe", fut.boxed());
        }
    }
}

#[async_trait]
impl<'a> EventSubscriber for InvocationRecorder<'a> {
    async fn handle_events(&mut self, events: &[Arc<BuckEvent>]) -> anyhow::Result<()> {
        for event in events {
            self.handle_event(event).await?;
        }
        Ok(())
    }

    async fn handle_console_interaction(&mut self, _c: char) -> anyhow::Result<()> {
        self.tags.push("console-interaction".to_owned());
        Ok(())
    }

    async fn handle_command_result(
        &mut self,
        result: &buck2_cli_proto::CommandResult,
    ) -> anyhow::Result<()> {
        self.has_command_result = true;
        match &result.result {
            Some(command_result::Result::BuildResponse(res)) => {
                let mut built_rule_type_names: Vec<String> = res
                    .build_targets
                    .iter()
                    .map(|t| {
                        t.target_rule_type_name
                            .clone()
                            .unwrap_or_else(|| "NULL".to_owned())
                    })
                    .unique_by(|x| x.clone())
                    .collect();
                built_rule_type_names.sort();
                self.target_rule_type_names = built_rule_type_names;
            }
            _ => {}
        }
        Ok(())
    }

    async fn handle_error(&mut self, error: &buck2_error::Error) -> anyhow::Result<()> {
        let want_stderr = error.tags().iter().any(|t| *t == ErrorTag::ClientGrpc);
        let best_tag = error.best_tag();
        let error = create_error_report(error);
        self.errors.push(ErrorIntermediate {
            processed: process_error_report(error),
            want_stderr,
            best_tag,
        });
        Ok(())
    }

    async fn handle_tailer_stderr(&mut self, stderr: &str) -> anyhow::Result<()> {
        if self.server_stderr.len() > 100_000 {
            // Proper truncation of the head is tricky, and for practical purposes
            // discarding the whole thing is fine.
            self.server_stderr.clear();
        }

        if !stderr.is_empty() {
            // We don't know yet whether we will need stderr or not,
            // so we capture it unconditionally.
            self.server_stderr.push_str(stderr);
            self.server_stderr.push('\n');
        }

        Ok(())
    }

    async fn exit(&mut self) -> anyhow::Result<()> {
        self.has_end_of_stream = true;
        Ok(())
    }

    fn as_error_observer(&self) -> Option<&dyn ErrorObserver> {
        Some(self)
    }

    fn handle_daemon_connection_failure(&mut self, error: &buck2_error::Error) {
        self.daemon_connection_failure = true;
        let best_tag = error.best_tag();
        let error = create_error_report(error);
        self.errors.push(ErrorIntermediate {
            processed: process_error_report(error),
            want_stderr: false,
            best_tag,
        });
    }

    fn handle_daemon_started(&mut self, daemon_was_started: buck2_data::DaemonWasStartedReason) {
        self.daemon_was_started = Some(daemon_was_started);
    }
}

impl<'a> ErrorObserver for InvocationRecorder<'a> {
    fn daemon_in_memory_state_is_corrupted(&self) -> bool {
        self.daemon_in_memory_state_is_corrupted
    }

    fn daemon_materializer_state_is_corrupted(&self) -> bool {
        self.daemon_materializer_state_is_corrupted
    }

    fn restarter_is_enabled(&self) -> bool {
        self.enable_restarter
    }
}

fn calculate_diff_if_some(a: &Option<u64>, b: &Option<u64>) -> Option<u64> {
    match (a, b) {
        (Some(av), Some(bv)) => Some(std::cmp::max(av, bv) - std::cmp::min(av, bv)),
        _ => None,
    }
}

fn merge_file_watcher_stats(
    a: Option<buck2_data::FileWatcherStats>,
    b: Option<buck2_data::FileWatcherStats>,
) -> Option<buck2_data::FileWatcherStats> {
    let (mut a, b) = match (a, b) {
        (Some(a), Some(b)) => (a, b),
        (a, None) => return a,
        (None, b) => return b,
    };

    a.fresh_instance = a.fresh_instance || b.fresh_instance;
    a.events_total += b.events_total;
    a.events_processed += b.events_processed;
    a.branched_from_revision = a.branched_from_revision.or(b.branched_from_revision);
    a.branched_from_global_rev = a.branched_from_global_rev.or(b.branched_from_global_rev);
    a.events.extend(b.events);
    a.incomplete_events_reason = a.incomplete_events_reason.or(b.incomplete_events_reason);
    a.watchman_version = a.watchman_version.or(b.watchman_version);
    Some(a)
}

pub(crate) fn try_get_invocation_recorder<'a>(
    ctx: &ClientCommandContext<'a>,
    opts: &CommonEventLogOptions,
    command_name: &'static str,
    sanitized_argv: Vec<String>,
    log_size_counter_bytes: Option<Arc<AtomicU64>>,
) -> anyhow::Result<Box<InvocationRecorder<'a>>> {
    let write_to_path = opts
        .unstable_write_invocation_record
        .as_ref()
        .map(|path| path.resolve(&ctx.working_dir));

    let filesystem;
    #[cfg(fbcode_build)]
    {
        let root = std::path::Path::to_owned(ctx.paths()?.project_root().root().to_buf().as_ref());
        if detect_eden::is_eden(root).unwrap_or(false) {
            filesystem = "eden".to_owned();
        } else {
            filesystem = "default".to_owned();
        }
    }
    #[cfg(not(fbcode_build))]
    {
        filesystem = "default".to_owned();
    }

    let recorder = InvocationRecorder::new(
        ctx.fbinit(),
        ctx.async_cleanup_context().dupe(),
        write_to_path,
        command_name,
        sanitized_argv,
        ctx.trace_id.dupe(),
        ctx.paths()?.isolation.as_str().to_owned(),
        BuildCountManager::new(ctx.paths()?.build_count_dir()),
        filesystem,
        ctx.restarted_trace_id.dupe(),
        log_size_counter_bytes,
        ctx.client_metadata
            .iter()
            .map(ClientMetadata::to_proto)
            .collect(),
    );
    Ok(Box::new(recorder))
}

fn truncate_stderr(stderr: &str) -> &str {
    // If server crashed, it means something is very broken,
    // and we don't really need nicely formatted stderr.
    // We only need to see it once, fix it, and never see it again.
    let max_len = 20_000;
    let truncate_at = stderr.len().saturating_sub(max_len);
    let truncate_at = stderr.ceil_char_boundary(truncate_at);
    &stderr[truncate_at..]
}

#[cfg(test)]
mod tests {
    use crate::subscribers::recorder::truncate_stderr;

    #[test]
    fn test_truncate_stderr() {
        let mut stderr = String::new();
        stderr.push_str("prefix");
        stderr.push('Ъ'); // 2 bytes, so asking to truncate in the middle of the char.
        for _ in 0..19_999 {
            stderr.push('a');
        }
        let truncated = truncate_stderr(&stderr);
        assert_eq!(truncated.len(), 19_999);
    }
}
