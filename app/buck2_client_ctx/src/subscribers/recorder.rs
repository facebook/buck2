/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::PathBuf;
use std::time::Duration;

use buck2_events::sink::scribe::new_thrift_scribe_sink_if_enabled;
use dupe::Dupe;

use crate::build_count::BuildCountManager;
use crate::client_ctx::ClientCommandContext;
use crate::subscribers::subscriber::EventSubscriber;

mod imp {
    use std::cmp;
    use std::collections::HashMap;
    use std::future::Future;
    use std::path::Path;
    use std::sync::Arc;
    use std::time::Duration;
    use std::time::Instant;
    use std::time::SystemTime;

    use anyhow::Context;
    use async_trait::async_trait;
    use buck2_common::convert::ProstDurationExt;
    use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
    use buck2_event_observer::action_stats;
    use buck2_event_observer::last_command_execution_kind;
    use buck2_event_observer::last_command_execution_kind::LastCommandExecutionKind;
    use buck2_events::sink::scribe::ThriftScribeSink;
    use buck2_events::trace::TraceId;
    use buck2_events::BuckEvent;
    use dupe::Dupe;
    use futures::FutureExt;
    use termwiz::istty::IsTty;

    use crate::build_count::BuildCountManager;
    use crate::cleanup_ctx::AsyncCleanupContext;
    use crate::subscribers::recorder::is_eden_dir;
    use crate::subscribers::recorder::system_memory_stats;
    use crate::subscribers::subscriber::EventSubscriber;

    pub struct InvocationRecorder {
        command_name: &'static str,
        cli_args: Vec<String>,
        isolation_dir: String,
        start_time: Instant,
        async_cleanup_context: AsyncCleanupContext,
        scribe: Arc<ThriftScribeSink>,
        build_count_manager: BuildCountManager,
        trace_id: TraceId,
        command_start: Option<buck2_data::CommandStart>,
        command_end: Option<buck2_data::CommandEnd>,
        command_critical_start: Option<buck2_data::CommandCriticalStart>,
        command_critical_end: Option<buck2_data::CommandCriticalEnd>,
        command_duration: Option<prost_types::Duration>,
        re_session_id: Option<String>,
        re_experiment_name: Option<String>,
        critical_path_duration: Option<Duration>,
        tags: Vec<String>,
        run_local_count: u64,
        run_remote_count: u64,
        run_action_cache_count: u64,
        run_skipped_count: u64,
        run_fallback_count: u64,
        first_snapshot: Option<buck2_data::Snapshot>,
        last_snapshot: Option<buck2_data::Snapshot>,
        min_build_count_since_rebase: u64,
        cache_upload_count: u64,
        cache_upload_attempt_count: u64,
        resolved_target_patterns: Option<buck2_data::ResolvedTargetPatterns>,
        invocation_root_path: AbsNormPathBuf,
        filesystem: Option<String>,
        watchman_version: Option<String>,
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
        system_total_memory_bytes: Option<u64>,
        file_watcher_stats: Option<buck2_data::FileWatcherStats>,
        time_to_last_action_execution_end: Option<Duration>,
        initial_sink_success_count: Option<u64>,
        initial_sink_failure_count: Option<u64>,
        initial_sink_dropped_count: Option<u64>,
        sink_max_buffer_depth: u64,
    }

    impl InvocationRecorder {
        pub fn new(
            async_cleanup_context: AsyncCleanupContext,
            scribe: ThriftScribeSink,
            mut command_name: &'static str,
            sanitized_argv: Vec<String>,
            trace_id: TraceId,
            isolation_dir: String,
            build_count_manager: BuildCountManager,
            invocation_root_path: AbsNormPathBuf,
        ) -> Self {
            // FIXME: Figure out if we can replace this. We used to log this this way in Ingress :/
            if command_name == "uquery" {
                command_name = "query";
            }

            Self {
                command_name,
                cli_args: sanitized_argv,
                isolation_dir,
                start_time: Instant::now(),
                async_cleanup_context,
                scribe: Arc::new(scribe),
                build_count_manager,
                trace_id,
                command_start: None,
                command_end: None,
                command_critical_start: None,
                command_critical_end: None,
                command_duration: None,
                re_session_id: None,
                re_experiment_name: None,
                critical_path_duration: None,
                tags: vec![],
                run_local_count: 0,
                run_remote_count: 0,
                run_action_cache_count: 0,
                run_skipped_count: 0,
                run_fallback_count: 0,
                first_snapshot: None,
                last_snapshot: None,
                min_build_count_since_rebase: 0,
                cache_upload_count: 0,
                cache_upload_attempt_count: 0,
                resolved_target_patterns: None,
                invocation_root_path,
                filesystem: None,
                watchman_version: None,
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
                system_total_memory_bytes: Some(system_memory_stats()),
                file_watcher_stats: None,
                time_to_last_action_execution_end: None,
                initial_sink_success_count: None,
                initial_sink_failure_count: None,
                initial_sink_dropped_count: None,
                sink_max_buffer_depth: 0,
            }
        }

        async fn build_count(
            &mut self,
            target_patterns: &[buck2_data::TargetPattern],
        ) -> anyhow::Result<u64> {
            if let Some(stats) = &self.file_watcher_stats {
                if let Some(merge_base) = &stats.branched_from_revision {
                    return self
                        .build_count_manager
                        .min_build_count(
                            merge_base,
                            self.resolved_target_patterns
                                .as_ref()
                                .map_or(target_patterns, |d| &d.target_patterns[..]),
                        )
                        .await
                        .context("Error recording build count");
                }
            }

            Ok(0)
        }

        fn exit(&mut self) -> Option<impl Future<Output = ()> + 'static + Send> {
            let mut sink_success_count = None;
            let mut sink_failure_count = None;
            let mut sink_dropped_count = None;
            if let Some(snapshot) = &self.last_snapshot {
                sink_success_count = calculate_diff_if_some(
                    &snapshot.sink_successes,
                    &self.initial_sink_success_count,
                );
                sink_failure_count = calculate_diff_if_some(
                    &snapshot.sink_failures,
                    &self.initial_sink_failure_count,
                );
                sink_dropped_count = calculate_diff_if_some(
                    &snapshot.sink_dropped,
                    &self.initial_sink_dropped_count,
                );
            }

            let record = buck2_data::InvocationRecord {
                command_name: Some(self.command_name.to_owned()),
                command_start: self.command_start.take(),
                command_end: self.command_end.take(),
                command_critical_start: self.command_critical_start.take(),
                command_critical_end: self.command_critical_end.take(),
                command_duration: self.command_duration.take(),
                client_walltime: self.start_time.elapsed().try_into().ok(),
                re_session_id: self.re_session_id.take().unwrap_or_default(),
                re_experiment_name: self.re_experiment_name.take().unwrap_or_default(),
                cli_args: self.cli_args.clone(),
                critical_path_duration: self.critical_path_duration.and_then(|x| x.try_into().ok()),
                metadata: Some(Self::default_metadata()),
                tags: self.tags.drain(..).collect(),
                run_local_count: self.run_local_count,
                run_remote_count: self.run_remote_count,
                run_action_cache_count: self.run_action_cache_count,
                run_skipped_count: self.run_skipped_count,
                run_fallback_count: Some(self.run_fallback_count),
                first_snapshot: self.first_snapshot.take(),
                last_snapshot: self.last_snapshot.take(),
                min_build_count_since_rebase: self.min_build_count_since_rebase,
                cache_upload_count: self.cache_upload_count,
                cache_upload_attempt_count: self.cache_upload_attempt_count,
                resolved_target_patterns: self.resolved_target_patterns.take(),
                filesystem: self.filesystem.take().unwrap_or_default(),
                watchman_version: self.watchman_version.take(),
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
                initial_materializer_entries_from_sqlite: self
                    .initial_materializer_entries_from_sqlite,
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
                system_total_memory_bytes: self.system_total_memory_bytes,
                file_watcher_stats: self.file_watcher_stats.take(),
                time_to_last_action_execution_end_ms: self
                    .time_to_last_action_execution_end
                    .and_then(|d| u64::try_from(d.as_millis()).ok()),
                isolation_dir: Some(self.isolation_dir.clone()),
                sink_success_count,
                sink_failure_count,
                sink_dropped_count,
                sink_max_buffer_depth: Some(self.sink_max_buffer_depth),
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
            tracing::info!("Recording invocation to Scribe: {:?}", &event);
            let scribe = self.scribe.dupe();
            Some(async move {
                scribe.send_now(event).await;
            })
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
            self.command_start = Some(command.clone());
            self.time_to_command_start = Some(self.start_time.elapsed());
            Ok(())
        }

        async fn handle_command_end(
            &mut self,
            command: &buck2_data::CommandEnd,
            event: &BuckEvent,
        ) -> anyhow::Result<()> {
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
            self.command_end = Some(command.clone());
            self.min_build_count_since_rebase =
                match command.data.as_ref().context("Missing command data")? {
                    buck2_data::command_end::Data::Build(cmd) => {
                        self.build_count(&cmd.unresolved_target_patterns).await?
                    }
                    buck2_data::command_end::Data::Test(cmd) => {
                        self.build_count(&cmd.unresolved_target_patterns).await?
                    }
                    buck2_data::command_end::Data::Install(cmd) => {
                        self.build_count(&cmd.unresolved_target_patterns).await?
                    }
                    // other events don't have target patterns
                    _ => 0,
                };
            let root = Path::to_owned(self.invocation_root_path.as_ref());
            if is_eden_dir(root).unwrap_or(false) {
                self.filesystem = Some("eden".to_owned());
            } else {
                self.filesystem = Some("default".to_owned());
            }
            Ok(())
        }
        fn handle_command_critical_start(
            &mut self,
            command: &buck2_data::CommandCriticalStart,
            _event: &BuckEvent,
        ) -> anyhow::Result<()> {
            self.command_critical_start = Some(command.clone());
            self.time_to_command_critical_section = Some(self.start_time.elapsed());
            Ok(())
        }
        fn handle_command_critical_end(
            &mut self,
            command: &buck2_data::CommandCriticalEnd,
            _event: &BuckEvent,
        ) -> anyhow::Result<()> {
            self.command_critical_end = Some(command.clone());
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
                    LastCommandExecutionKind::Cached => {
                        self.run_action_cache_count += 1;
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
                Some(buck2_data::executor_stage_start::Stage::Re(re_stage)) => {
                    match &re_stage.stage {
                        Some(buck2_data::re_stage::Stage::Execute(_)) => {
                            self.time_to_first_command_execution_start
                                .get_or_insert_with(|| self.start_time.elapsed());
                        }
                        _ => {}
                    }
                }
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

        fn handle_build_graph_info(
            &mut self,
            info: &buck2_data::BuildGraphExecutionInfo,
            _event: &BuckEvent,
        ) -> anyhow::Result<()> {
            let durations = info
                .critical_path
                .iter()
                .filter_map(|x| x.duration.as_ref())
                .map(|d| d.try_into_duration())
                .collect::<Result<Vec<_>, _>>()?;
            self.critical_path_duration = Some(durations.iter().sum());
            Ok(())
        }

        fn handle_tag(&mut self, tag: &buck2_data::TagEvent) -> anyhow::Result<()> {
            self.tags.extend(tag.tags.iter().cloned());
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

            Ok(())
        }

        fn handle_file_watcher_end(
            &mut self,
            file_watcher: &buck2_data::FileWatcherEnd,
            _event: &BuckEvent,
        ) -> anyhow::Result<()> {
            // We might receive this event twice, so ... deal with it by merging the two.
            // See: https://fb.workplace.com/groups/buck2dev/permalink/3396726613948720/
            self.file_watcher_stats = merge_file_watcher_stats(
                self.file_watcher_stats.take(),
                file_watcher.stats.clone(),
            );

            if let Some(stats) = &file_watcher.stats {
                self.watchman_version = stats.watchman_version.to_owned();
            }
            Ok(())
        }

        fn handle_resolved_target_patterns(
            &mut self,
            patterns: &buck2_data::ResolvedTargetPatterns,
        ) -> anyhow::Result<()> {
            self.resolved_target_patterns = Some(patterns.clone());
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
                            self.handle_file_watcher_end(file_watcher, event)
                        }
                        buck2_data::span_end_event::Data::CacheUpload(cache_upload) => {
                            self.handle_cache_upload_end(cache_upload, event)
                        }
                        buck2_data::span_end_event::Data::Materialization(materialization) => {
                            self.handle_materialization_end(materialization, event)
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
                        buck2_data::instant_event::Data::TargetPatterns(tag) => {
                            self.handle_resolved_target_patterns(tag)
                        }
                        buck2_data::instant_event::Data::MaterializerStateInfo(
                            materializer_state,
                        ) => self.handle_materializer_state_info(materializer_state),
                        _ => Ok(()),
                    }
                }
                buck2_data::buck_event::Data::Record(_) => Ok(()),
            }
        }
    }

    impl Drop for InvocationRecorder {
        fn drop(&mut self) {
            if let Some(fut) = self.exit() {
                self.async_cleanup_context
                    .register("sending invocation to Scribe", fut.boxed());
            }
        }
    }

    #[async_trait]
    impl EventSubscriber for InvocationRecorder {
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
        a.events.extend(b.events);
        a.incomplete_events_reason = a.incomplete_events_reason.or(b.incomplete_events_reason);
        a.watchman_version = a.watchman_version.or(b.watchman_version);
        Some(a)
    }
}

pub fn try_get_invocation_recorder(
    ctx: &ClientCommandContext,
    command_name: &'static str,
    sanitized_argv: Vec<String>,
) -> anyhow::Result<Option<Box<dyn EventSubscriber>>> {
    if ctx.replayer.is_none() {
        if let Some(sink) =
            new_thrift_scribe_sink_if_enabled(ctx.fbinit(), 1, Duration::from_millis(500), 5, None)?
        {
            let recorder = imp::InvocationRecorder::new(
                ctx.async_cleanup_context().dupe(),
                sink,
                command_name,
                sanitized_argv,
                ctx.trace_id.dupe(),
                ctx.paths()?.isolation.as_str().to_owned(),
                BuildCountManager::new(ctx.paths()?.build_count_dir()),
                ctx.paths()?.project_root().root().to_buf(),
            );
            return Ok(Some(Box::new(recorder) as _));
        }
    }
    Ok(None)
}

// TODO: is_eden_dir() should probably be placed in buck2_common/src/eden/mod.rs as a public function,
// but current Windows build limitations make importing that module difficult
// https://www.internalfb.com/intern/wiki/EdenFS/detecting-an-eden-mount/
#[cfg(not(windows))]
pub fn is_eden_dir(mut dir: PathBuf) -> anyhow::Result<bool> {
    dir.push(".eden");
    dir.push("root");
    Ok(std::fs::read_link(&dir).is_ok())
}

#[cfg(windows)]
pub fn is_eden_dir(mut dir: PathBuf) -> anyhow::Result<bool> {
    // https://docs.microsoft.com/en-us/windows/win32/fileio/determining-whether-a-directory-is-a-volume-mount-point
    fn is_mount_point(mut temp_dir: PathBuf) -> Result<bool, std::io::Error> {
        use std::mem::MaybeUninit;
        use std::os::windows::ffi::OsStrExt;
        use std::os::windows::fs::MetadataExt;

        // Append a `\` to the end of the directory path, per Windows documentation requirement
        if !temp_dir.ends_with("") {
            temp_dir.push("");
        }
        let mut encoded = temp_dir.as_os_str().encode_wide().collect::<Vec<u16>>();
        encoded.push(0);

        unsafe {
            let metadata = std::fs::metadata(&temp_dir)?;
            if metadata.file_attributes() & winapi::um::winnt::FILE_ATTRIBUTE_REPARSE_POINT == 0 {
                return Ok(false);
            }

            let mut data: MaybeUninit<winapi::um::minwinbase::WIN32_FIND_DATAW> =
                MaybeUninit::uninit();
            let data_ptr = data.as_mut_ptr();
            let handle = winapi::um::fileapi::FindFirstFileW(encoded.as_ptr(), data_ptr);
            if handle == winapi::um::handleapi::INVALID_HANDLE_VALUE {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    "invalid handle value".to_owned(),
                ));
            }
            winapi::um::fileapi::FindClose(handle);

            let data = data.assume_init();
            Ok(data.dwReserved0 == winapi::um::winnt::IO_REPARSE_TAG_MOUNT_POINT)
        }
    }

    dir = std::fs::canonicalize(&dir)?;
    if is_mount_point(dir.clone())? {
        return Ok(false);
    }

    dir.push(".eden");
    dir.push("config");
    if std::fs::metadata(&dir).map_or(false, |metadata| metadata.is_file()) {
        Ok(true)
    } else {
        Ok(false)
    }
}

fn system_memory_stats() -> u64 {
    use sysinfo::RefreshKind;
    use sysinfo::System;
    use sysinfo::SystemExt;

    let system = System::new_with_specifics(RefreshKind::new().with_memory());
    system.total_memory()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_system_memory_stats() {
        let total_mem = system_memory_stats();
        // sysinfo returns zero when fails to retrieve data
        assert!(total_mem > 0);
    }
}
