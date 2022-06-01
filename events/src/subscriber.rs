use std::time::{Duration, Instant};

use async_trait::async_trait;
use buck2_data::{
    buck_event, InstantEvent, SpanCancelled, SpanEndEvent, SpanStartEvent, TestDiscoveryEnd,
    TestDiscoveryStart, TestRunEnd, TestRunStart,
};
use cli_proto::CommandResult;
use gazebo::prelude::*;
use thiserror::Error;

use crate::BuckEvent;

/// Information about tick timing.
#[derive(Debug, Clone, Dupe)]
pub struct Tick {
    /// Previous tick counter. 0 indicates no previous tick.
    pub previous_tick: u32,
    /// Current tick counter. Note this may be greater than `previous_tick + 1` if ticks were skipped.
    /// It may be equal to previous_tick if additional ticks are requested (using `tick_now()`).
    pub current_tick: u32,
    /// The time that the ticker was started.
    pub start_time: Instant,
    /// Elapsed time since the ticker was started for this tick.
    pub elapsed_time: Duration,
    /// Target number of ticks per second.
    pub ticks_per_second: u32,
}

impl Tick {
    pub fn now() -> Tick {
        Self {
            previous_tick: 0,
            current_tick: 0,
            start_time: Instant::now(),
            elapsed_time: Duration::ZERO,
            ticks_per_second: 1,
        }
    }
}

/// Just a simple structure that makes it easier to deal with BuckEvent rather than
/// needing to deal with the unpacking of optional fields yourself.
pub enum UnpackedBuckEvent<'a> {
    SpanStart(
        &'a BuckEvent,
        &'a SpanStartEvent,
        &'a buck2_data::span_start_event::Data,
    ),
    SpanEnd(
        &'a BuckEvent,
        &'a SpanEndEvent,
        &'a buck2_data::span_end_event::Data,
    ),
    Instant(
        &'a BuckEvent,
        &'a InstantEvent,
        &'a buck2_data::instant_event::Data,
    ),
}

pub fn unpack_event(event: &BuckEvent) -> anyhow::Result<UnpackedBuckEvent> {
    match &event.data {
        buck_event::Data::SpanStart(v) => Ok(UnpackedBuckEvent::SpanStart(
            event,
            v,
            v.data
                .as_ref()
                .ok_or_else(|| VisitorError::MissingField((*event).clone()))?,
        )),
        buck_event::Data::SpanEnd(v) => Ok(UnpackedBuckEvent::SpanEnd(
            event,
            v,
            v.data
                .as_ref()
                .ok_or_else(|| VisitorError::MissingField((*event).clone()))?,
        )),
        buck_event::Data::Instant(v) => Ok(UnpackedBuckEvent::Instant(
            event,
            v,
            v.data
                .as_ref()
                .ok_or_else(|| VisitorError::MissingField((*event).clone()))?,
        )),
    }
}

/// Visitor trait.  Implement this to subscribe to the event streams.
/// Each method will be called whenever an event occurs.
// TODO(brasselsprouts): convert events so this event subscriber trait can be reduced.
#[async_trait]
pub trait EventSubscriber: Send {
    async fn handle_output(&mut self, _raw_output: &str) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_stderr(&mut self, _stderr: &str) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_event(&mut self, event: &BuckEvent) -> anyhow::Result<()> {
        self.handle_inner_event(event).await
    }
    /// Unpacks a [`BuckEvent`] and calls the appropriate specific event handle
    ///
    /// This should only be called by implementors of [`EventSubscriber`]
    async fn handle_inner_event(&mut self, event: &BuckEvent) -> anyhow::Result<()> {
        match event.data {
            buck_event::Data::SpanStart(ref start) => self.handle_event_start(start, event.dupe()),
            buck_event::Data::SpanEnd(ref end) => self.handle_event_end(end, event.dupe()),
            buck_event::Data::Instant(ref instant) => self.handle_instant(instant, event.dupe()),
        }
        .await
    }
    async fn handle_command_result(&mut self, _result: &CommandResult) -> anyhow::Result<()> {
        Ok(())
    }

    async fn handle_event_start(
        &mut self,
        start: &SpanStartEvent,
        event: &BuckEvent,
    ) -> anyhow::Result<()> {
        match start
            .data
            .as_ref()
            .ok_or_else(|| VisitorError::MissingField((*event).clone()))?
        {
            buck2_data::span_start_event::Data::Command(command) => {
                self.handle_command_start(command, event)
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
            buck2_data::span_start_event::Data::Watchman(watchman) => {
                self.handle_watchman_start(watchman, event)
            }
            buck2_data::span_start_event::Data::MatchDepFiles(dep_files) => {
                self.handle_match_dep_files_start(dep_files, event)
            }
        }
        .await
    }

    async fn handle_event_end(
        &mut self,
        end: &SpanEndEvent,
        event: &BuckEvent,
    ) -> anyhow::Result<()> {
        match end
            .data
            .as_ref()
            .ok_or_else(|| VisitorError::MissingField((*event).clone()))?
        {
            buck2_data::span_end_event::Data::Command(command) => {
                self.handle_command_end(command, event)
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
            buck2_data::span_end_event::Data::Watchman(watchman) => {
                self.handle_watchman_end(watchman, event)
            }
            buck2_data::span_end_event::Data::MatchDepFiles(dep_files) => {
                self.handle_match_dep_files_end(dep_files, event)
            }
        }
        .await
    }

    async fn handle_instant(
        &mut self,
        instant: &InstantEvent,
        event: &BuckEvent,
    ) -> anyhow::Result<()> {
        match instant
            .data
            .as_ref()
            .ok_or_else(|| VisitorError::MissingField((*event).clone()))?
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
            buck2_data::instant_event::Data::Log(log) => self.handle_log(log),
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
    async fn handle_watchman_start(
        &mut self,
        _watchman: &buck2_data::WatchmanStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_watchman_end(
        &mut self,
        _watchman: &buck2_data::WatchmanEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_match_dep_files_start(
        &mut self,
        _watchman: &buck2_data::MatchDepFilesStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_match_dep_files_end(
        &mut self,
        _watchman: &buck2_data::MatchDepFilesEnd,
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
        _update: &buck2_data::DiceComputationStateSnapshot,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_log(&mut self, _log: &buck2_data::Log) -> anyhow::Result<()> {
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

#[derive(Error, Debug)]
pub enum VisitorError {
    #[error("Sent an event missing one or more fields: `{0:?}`")]
    MissingField(BuckEvent),
}
