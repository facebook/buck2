/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use async_trait::async_trait;
use buck2_data::buck_event;
use buck2_data::InstantEvent;
use buck2_data::SpanCancelled;
use buck2_data::SpanEndEvent;
use buck2_data::SpanStartEvent;
use buck2_data::TestDiscoveryEnd;
use buck2_data::TestDiscoveryStart;
use buck2_data::TestRunEnd;
use buck2_data::TestRunStart;
use buck2_events::BuckEvent;
use cli_proto::CommandResult;

use crate::subscribers::subscriber::EventSubscriber;
use crate::subscribers::subscriber::Tick;
use crate::subscribers::subscriber::VisitorError;

/// Wrap an `UnpackingEventSubscriber` instance to provide an `EventSubscriber`.
pub(crate) struct UnpackingEventSubscriberAsEventSubscriber<U: UnpackingEventSubscriber>(
    pub(crate) U,
);

#[async_trait]
pub trait UnpackingEventSubscriber: Send {
    async fn handle_output(&mut self, _raw_output: &str) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_stderr(&mut self, _stderr: &str) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_console_interaction(&mut self, _c: char) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_event(&mut self, event: &Arc<BuckEvent>) -> anyhow::Result<()> {
        self.handle_inner_event(event).await
    }
    async fn handle_inner_event(&mut self, event: &Arc<BuckEvent>) -> anyhow::Result<()> {
        match event.data() {
            buck_event::Data::SpanStart(ref start) => self.handle_event_start(start, event),
            buck_event::Data::SpanEnd(ref end) => self.handle_event_end(end, event),
            buck_event::Data::Instant(ref instant) => self.handle_instant(instant, event),
            // Not present in the event stream from the daemon to CLI.
            buck_event::Data::Record(_) => Box::pin(async { Ok(()) }),
        }
        .await
    }
    async fn handle_command_result(&mut self, _result: &CommandResult) -> anyhow::Result<()> {
        Ok(())
    }

    async fn exit(&mut self) -> anyhow::Result<()> {
        Ok(())
    }

    async fn handle_event_start(
        &mut self,
        start: &SpanStartEvent,
        event: &Arc<BuckEvent>,
    ) -> anyhow::Result<()> {
        match start
            .data
            .as_ref()
            .ok_or_else(|| VisitorError::MissingField((**event).clone()))?
        {
            buck2_data::span_start_event::Data::Command(command) => {
                self.handle_command_start(command, event)
            }
            buck2_data::span_start_event::Data::CommandCritical(command) => {
                self.handle_command_critical_start(command, event)
            }
            buck2_data::span_start_event::Data::ActionExecution(action) => {
                self.handle_action_execution_start(action, event)
            }
            buck2_data::span_start_event::Data::FinalMaterialization(materialization) => {
                self.handle_final_materialization_start(materialization, event)
            }
            buck2_data::span_start_event::Data::Analysis(analysis) => {
                self.handle_analysis_start(analysis, event)
            }
            buck2_data::span_start_event::Data::AnalysisStage(stage) => {
                self.handle_analysis_stage_start(stage, event)
            }
            buck2_data::span_start_event::Data::Load(eval) => self.handle_load_start(eval, event),
            buck2_data::span_start_event::Data::LoadPackage(eval) => {
                self.handle_load_package_start(eval, event)
            }
            buck2_data::span_start_event::Data::ExecutorStage(stage) => {
                self.handle_executor_stage_start(stage, event)
            }
            buck2_data::span_start_event::Data::TestDiscovery(discovery) => {
                self.handle_test_discovery_start(discovery, event)
            }
            buck2_data::span_start_event::Data::TestStart(test_suite) => {
                self.handle_test_start(test_suite, event)
            }
            buck2_data::span_start_event::Data::FileWatcher(file_watcher) => {
                self.handle_file_watcher_start(file_watcher, event)
            }
            buck2_data::span_start_event::Data::MatchDepFiles(dep_files) => {
                self.handle_match_dep_files_start(dep_files, event)
            }
            buck2_data::span_start_event::Data::SharedTask(shared_task) => {
                self.handle_shared_task_start(shared_task, event)
            }
            buck2_data::span_start_event::Data::CacheUpload(cache_upload) => {
                self.handle_cache_upload_start(cache_upload, event)
            }
            buck2_data::span_start_event::Data::CreateOutputSymlinks(create_output_symlinks) => {
                self.handle_create_output_symlinks_start(create_output_symlinks, event)
            }
            buck2_data::span_start_event::Data::InstallEventInfo(info) => {
                self.handle_install_event_info_start(info, event)
            }
            buck2_data::span_start_event::Data::DiceStateUpdate(dice_update) => {
                self.handle_dice_state_update_start(dice_update, event)
            }
            buck2_data::span_start_event::Data::Materialization(materialization) => {
                self.handle_materialization_start(materialization, event)
            }
            buck2_data::span_start_event::Data::DiceCriticalSection(dice_critical_section) => {
                self.handle_dice_critical_section_start(dice_critical_section, event)
            }
            buck2_data::span_start_event::Data::DiceBlockConcurrentCommand(
                dice_block_concurrent_command,
            ) => self
                .handle_dice_block_concurrent_command_start(dice_block_concurrent_command, event),
            buck2_data::span_start_event::Data::Fake(fake) => self.handle_fake_start(fake, event),
        }
        .await
    }

    async fn handle_event_end(
        &mut self,
        end: &SpanEndEvent,
        event: &Arc<BuckEvent>,
    ) -> anyhow::Result<()> {
        match end
            .data
            .as_ref()
            .ok_or_else(|| VisitorError::MissingField((**event).clone()))?
        {
            buck2_data::span_end_event::Data::Command(command) => {
                self.handle_command_end(command, event)
            }
            buck2_data::span_end_event::Data::CommandCritical(command) => {
                self.handle_command_critical_end(command, event)
            }
            buck2_data::span_end_event::Data::ActionExecution(action) => {
                self.handle_action_execution_end(action, event)
            }
            buck2_data::span_end_event::Data::FinalMaterialization(materialization) => {
                self.handle_final_materialization_end(materialization, event)
            }
            buck2_data::span_end_event::Data::Analysis(analysis) => {
                self.handle_analysis_end(analysis, event)
            }
            buck2_data::span_end_event::Data::AnalysisStage(stage) => {
                self.handle_analysis_stage_end(stage, event)
            }
            buck2_data::span_end_event::Data::Load(eval) => self.handle_load_end(eval, event),
            buck2_data::span_end_event::Data::LoadPackage(eval) => {
                self.handle_load_package_end(eval, event)
            }
            buck2_data::span_end_event::Data::ExecutorStage(stage) => {
                self.handle_executor_stage_end(stage, event)
            }
            buck2_data::span_end_event::Data::TestDiscovery(discovery) => {
                self.handle_test_discovery_end(discovery, event)
            }
            buck2_data::span_end_event::Data::TestEnd(test_suite) => {
                self.handle_test_end(test_suite, event)
            }
            buck2_data::span_end_event::Data::SpanCancelled(span_cancelled) => {
                self.handle_span_cancelled(span_cancelled, event)
            }
            buck2_data::span_end_event::Data::FileWatcher(file_watcher) => {
                self.handle_file_watcher_end(file_watcher, event)
            }
            buck2_data::span_end_event::Data::SharedTask(shared_task) => {
                self.handle_shared_task_end(shared_task, event)
            }
            buck2_data::span_end_event::Data::MatchDepFiles(dep_files) => {
                self.handle_match_dep_files_end(dep_files, event)
            }
            buck2_data::span_end_event::Data::CacheUpload(cache_upload) => {
                self.handle_cache_upload_end(cache_upload, event)
            }
            buck2_data::span_end_event::Data::CreateOutputSymlinks(create_output_symlinks) => {
                self.handle_create_output_symlinks_end(create_output_symlinks, event)
            }
            buck2_data::span_end_event::Data::InstallEventInfo(info) => {
                self.handle_install_event_info_end(info, event)
            }
            buck2_data::span_end_event::Data::DiceStateUpdate(dice_update) => {
                self.handle_dice_state_update_end(dice_update, event)
            }
            buck2_data::span_end_event::Data::Materialization(materialization) => {
                self.handle_materialization_end(materialization, event)
            }
            buck2_data::span_end_event::Data::DiceCriticalSection(dice_critical_section) => {
                self.handle_dice_critical_section_end(dice_critical_section, event)
            }
            buck2_data::span_end_event::Data::DiceBlockConcurrentCommand(
                dice_block_concurrent_command,
            ) => {
                self.handle_dice_block_concurrent_command_end(dice_block_concurrent_command, event)
            }
            buck2_data::span_end_event::Data::Fake(fake) => self.handle_fake_end(fake, event),
        }
        .await
    }

    async fn handle_instant(
        &mut self,
        instant: &InstantEvent,
        event: &Arc<BuckEvent>,
    ) -> anyhow::Result<()> {
        match instant
            .data
            .as_ref()
            .ok_or_else(|| VisitorError::MissingField((**event).clone()))?
        {
            buck2_data::instant_event::Data::ConsoleMessage(message) => {
                self.handle_console_message(message, event)
            }
            buck2_data::instant_event::Data::ReSession(session) => {
                self.handle_re_session_created(session, event)
            }
            buck2_data::instant_event::Data::Panic(panic) => self.handle_panic(panic, event),
            buck2_data::instant_event::Data::HgInfo(hg) => self.handle_hg_info(hg, event),
            buck2_data::instant_event::Data::BuildGraphInfo(info) => {
                self.handle_build_graph_info(info, event)
            }
            buck2_data::instant_event::Data::TestDiscovery(discovery) => {
                self.handle_test_discovery(discovery, event)
            }
            buck2_data::instant_event::Data::TestResult(result) => {
                self.handle_test_result(result, event)
            }
            buck2_data::instant_event::Data::RageInvoked(result) => {
                self.handle_rage_invoked(result, event)
            }
            buck2_data::instant_event::Data::RawOutput(output) => {
                self.handle_output(&output.raw_output)
            }
            buck2_data::instant_event::Data::Snapshot(result) => {
                self.handle_snapshot(result, event)
            }
            buck2_data::instant_event::Data::DiceStateSnapshot(update) => {
                self.handle_dice_snapshot(update)
            }
            buck2_data::instant_event::Data::LspResult(result) => self.handle_lsp_result(result),
            buck2_data::instant_event::Data::TagEvent(tag) => self.handle_tag(tag),
            buck2_data::instant_event::Data::TargetPatterns(tag) => {
                self.handle_resolved_target_patterns(tag)
            }
        }
        .await
    }

    async fn handle_command_start(
        &mut self,
        _command: &buck2_data::CommandStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_command_end(
        &mut self,
        _command: &buck2_data::CommandEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_command_critical_start(
        &mut self,
        _command: &buck2_data::CommandCriticalStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_command_critical_end(
        &mut self,
        _command: &buck2_data::CommandCriticalEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_action_execution_start(
        &mut self,
        _action: &buck2_data::ActionExecutionStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_action_execution_end(
        &mut self,
        _action: &buck2_data::ActionExecutionEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_final_materialization_start(
        &mut self,
        _materialization: &buck2_data::MaterializeRequestedArtifactStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_final_materialization_end(
        &mut self,
        _materialization: &buck2_data::MaterializeRequestedArtifactEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_analysis_start(
        &mut self,
        _analysis: &buck2_data::AnalysisStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_analysis_end(
        &mut self,
        _analysis: &buck2_data::AnalysisEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_analysis_stage_start(
        &mut self,
        _eval: &buck2_data::AnalysisStageStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_analysis_stage_end(
        &mut self,
        _eval: &buck2_data::AnalysisStageEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_load_start(
        &mut self,
        _eval: &buck2_data::LoadBuildFileStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_load_end(
        &mut self,
        _eval: &buck2_data::LoadBuildFileEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_load_package_start(
        &mut self,
        _eval: &buck2_data::LoadPackageStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_load_package_end(
        &mut self,
        _eval: &buck2_data::LoadPackageEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_executor_stage_start(
        &mut self,
        _eval: &buck2_data::ExecutorStageStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_executor_stage_end(
        &mut self,
        _eval: &buck2_data::ExecutorStageEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_console_message(
        &mut self,
        _message: &buck2_data::ConsoleMessage,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_re_session_created(
        &mut self,
        _session: &buck2_data::RemoteExecutionSessionCreated,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_panic(
        &mut self,
        _panic: &buck2_data::Panic,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_hg_info(
        &mut self,
        _hg: &buck2_data::MercurialInfo,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_build_graph_info(
        &mut self,
        _info: &buck2_data::BuildGraphExecutionInfo,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_test_discovery(
        &mut self,
        _test_info: &buck2_data::TestDiscovery,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_test_result(
        &mut self,
        _result: &buck2_data::TestResult,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_test_discovery_start(
        &mut self,
        _test_info: &TestDiscoveryStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_test_discovery_end(
        &mut self,
        _test_info: &TestDiscoveryEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_test_start(
        &mut self,
        _test_info: &TestRunStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_test_end(
        &mut self,
        _test_info: &TestRunEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_span_cancelled(
        &mut self,
        _test_info: &SpanCancelled,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_file_watcher_start(
        &mut self,
        _watchman: &buck2_data::FileWatcherStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_file_watcher_end(
        &mut self,
        _watchman: &buck2_data::FileWatcherEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_shared_task_start(
        &mut self,
        _shared_task: &buck2_data::SharedTaskStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_shared_task_end(
        &mut self,
        _shared_task: &buck2_data::SharedTaskEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_fake_start(
        &mut self,
        _fake: &buck2_data::FakeStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_fake_end(
        &mut self,
        _fake: &buck2_data::FakeEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_match_dep_files_start(
        &mut self,
        _dep_files: &buck2_data::MatchDepFilesStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_match_dep_files_end(
        &mut self,
        _dep_files: &buck2_data::MatchDepFilesEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_cache_upload_start(
        &mut self,
        _cache_upload: &buck2_data::CacheUploadStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_cache_upload_end(
        &mut self,
        _cache_upload: &buck2_data::CacheUploadEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_create_output_symlinks_start(
        &mut self,
        _create_output_symlinks: &buck2_data::CreateOutputSymlinksStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_create_output_symlinks_end(
        &mut self,
        _create_output_symlinks: &buck2_data::CreateOutputSymlinksEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_install_event_info_start(
        &mut self,
        _info: &buck2_data::InstallEventInfoStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_install_event_info_end(
        &mut self,
        _info: &buck2_data::InstallEventInfoEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_dice_state_update_start(
        &mut self,
        _dice_update: &buck2_data::DiceStateUpdateStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_dice_state_update_end(
        &mut self,
        _dice_update: &buck2_data::DiceStateUpdateEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_materialization_start(
        &mut self,
        _materialization: &buck2_data::MaterializationStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_dice_critical_section_start(
        &mut self,
        _dice_critical_section: &buck2_data::DiceCriticalSectionStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_dice_critical_section_end(
        &mut self,
        _dice_critical_section: &buck2_data::DiceCriticalSectionEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_dice_block_concurrent_command_start(
        &mut self,
        _dice_block_concurrent_command: &buck2_data::DiceBlockConcurrentCommandStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_dice_block_concurrent_command_end(
        &mut self,
        _dice_block_concurrent_command: &buck2_data::DiceBlockConcurrentCommandEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_materialization_end(
        &mut self,
        _materialization: &buck2_data::MaterializationEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_rage_invoked(
        &mut self,
        _command: &buck2_data::RageInvoked,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_snapshot(
        &mut self,
        _snapshot: &buck2_data::Snapshot,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_dice_snapshot(
        &mut self,
        _update: &buck2_data::DiceStateSnapshot,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_tag(&mut self, _tag: &buck2_data::TagEvent) -> anyhow::Result<()> {
        Ok(())
    }

    async fn handle_resolved_target_patterns(
        &mut self,
        _pattern: &buck2_data::ResolvedTargetPatterns,
    ) -> anyhow::Result<()> {
        Ok(())
    }

    async fn handle_lsp_result(&mut self, _msg: &buck2_data::LspResult) -> anyhow::Result<()> {
        Ok(())
    }

    /// Give the subscriber a chance to react to errors as we start trying to clean up.
    /// They may return another error, which will be incorporated into the end result.
    async fn handle_error(&mut self, _error: &anyhow::Error) -> anyhow::Result<()> {
        Ok(())
    }

    /// Allow the subscriber to do some sort of action once every render cycle.
    async fn tick(&mut self, _tick: &Tick) -> anyhow::Result<()> {
        Ok(())
    }
}

#[async_trait]
impl<U: UnpackingEventSubscriber> EventSubscriber for UnpackingEventSubscriberAsEventSubscriber<U> {
    async fn handle_tailer_stdout(&mut self, raw_output: &str) -> anyhow::Result<()> {
        self.0.handle_output(raw_output).await
    }

    async fn handle_tailer_stderr(&mut self, stderr: &str) -> anyhow::Result<()> {
        self.0.handle_stderr(stderr).await
    }

    async fn handle_console_interaction(&mut self, c: char) -> anyhow::Result<()> {
        self.0.handle_console_interaction(c).await
    }

    async fn handle_events(&mut self, events: &[Arc<BuckEvent>]) -> anyhow::Result<()> {
        for event in events {
            self.0.handle_event(event).await?;
        }
        Ok(())
    }

    async fn handle_error(&mut self, error: &anyhow::Error) -> anyhow::Result<()> {
        self.0.handle_error(error).await
    }

    async fn tick(&mut self, tick: &Tick) -> anyhow::Result<()> {
        self.0.tick(tick).await
    }

    async fn handle_command_result(
        &mut self,
        result: &cli_proto::CommandResult,
    ) -> anyhow::Result<()> {
        self.0.handle_command_result(result).await
    }

    async fn exit(&mut self) -> anyhow::Result<()> {
        self.0.exit().await
    }
}
