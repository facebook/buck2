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

use buck2_error::BuckErrorContext;
use buck2_events::BuckEvent;
use buck2_health_check::health_check_client::HealthCheckClient;
use buck2_wrapper_common::invocation_id::TraceId;

use crate::action_stats::ActionStats;
use crate::debug_events::DebugEventsState;
use crate::dice_state::DiceState;
use crate::progress::BuildProgressStateTracker;
use crate::re_state::ReState;
use crate::session_info::SessionInfo;
use crate::span_tracker::BuckEventSpanTracker;
use crate::starlark_debug::StarlarkDebuggerState;
use crate::test_state::TestState;
use crate::two_snapshots::TwoSnapshots;

pub struct EventObserver<E> {
    pub span_tracker: BuckEventSpanTracker,
    pub action_stats: ActionStats,
    re_state: ReState,
    two_snapshots: TwoSnapshots, // NOTE: We got many more copies of this than we should.
    system_info: buck2_data::SystemInfo,
    session_info: SessionInfo,
    test_state: TestState,
    starlark_debugger_state: StarlarkDebuggerState,
    pub health_check_client: Option<HealthCheckClient>,
    dice_state: DiceState,
    /// When running without the Superconsole, we skip some state that we don't need. This might be
    /// premature optimization.
    extra: E,
}

impl<E> EventObserver<E>
where
    E: EventObserverExtra,
{
    pub fn new(trace_id: TraceId) -> Self {
        Self {
            span_tracker: BuckEventSpanTracker::new(),
            action_stats: ActionStats::default(),
            re_state: ReState::new(),
            two_snapshots: TwoSnapshots::default(),
            system_info: buck2_data::SystemInfo::default(),
            session_info: SessionInfo {
                trace_id: trace_id.clone(),
                test_session: None,
                legacy_dice: false,
            },
            test_state: TestState::default(),
            starlark_debugger_state: StarlarkDebuggerState::new(),
            health_check_client: Some(HealthCheckClient::new(trace_id.to_string(), None, None)),
            dice_state: DiceState::new(),
            extra: E::new(),
        }
    }

    pub async fn observe(
        &mut self,
        receive_time: Instant,
        event: &Arc<BuckEvent>,
    ) -> buck2_error::Result<()> {
        self.span_tracker.handle_event(receive_time, event)?;

        {
            use buck2_data::buck_event::Data::*;

            match event.data() {
                SpanStart(start) => match &start.data {
                    Some(buck2_data::span_start_event::Data::Command(command)) => {
                        if let Some(client) = &mut self.health_check_client {
                            client.update_command_data(command.data.clone()).await;
                        }
                    }
                    _ => {}
                },
                SpanEnd(end) => {
                    use buck2_data::span_end_event::Data::*;

                    match end
                        .data
                        .as_ref()
                        .buck_error_context("Missing `data` in SpanEnd")?
                    {
                        ActionExecution(action_execution_end) => {
                            self.action_stats.update(action_execution_end);
                            let has_excess_cache_misses = self.action_stats.excess_cache_misses > 0;
                            if has_excess_cache_misses {
                                if let Some(health_check_client) = &mut self.health_check_client {
                                    health_check_client
                                        .update_excess_cache_misses(has_excess_cache_misses)
                                        .await;
                                }
                            }
                        }
                        buck2_data::span_end_event::Data::FileWatcher(file_watcher) => {
                            if let Some(merge_base) = file_watcher
                                .stats
                                .as_ref()
                                .and_then(|stats| stats.branched_from_revision.as_ref())
                            {
                                if let Some(health_check_client) = &mut self.health_check_client {
                                    health_check_client
                                        .update_branched_from_revision(&merge_base)
                                        .await;
                                }
                            }
                        }
                        _ => {}
                    }
                }
                Instant(instant) => {
                    use buck2_data::instant_event::Data::*;

                    match instant
                        .data
                        .as_ref()
                        .buck_error_context("Missing `data` in `Instant`")?
                    {
                        ReSession(re_session) => {
                            self.re_state.add_re_session(re_session);
                        }
                        Snapshot(snapshot) => {
                            self.re_state.update(snapshot);
                            self.two_snapshots.update(event.timestamp(), snapshot);
                        }
                        TestDiscovery(discovery) => {
                            use buck2_data::test_discovery::Data::*;

                            match discovery
                                .data
                                .as_ref()
                                .buck_error_context("Missing `data` in `TestDiscovery`")?
                            {
                                Session(session) => {
                                    self.session_info.test_session = Some(session.clone());
                                }
                                Tests(tests) => {
                                    self.test_state.discovered += tests.test_names.len() as u64
                                }
                            }
                        }
                        TestResult(result) => {
                            self.test_state.update(result)?;
                        }
                        DebugAdapterSnapshot(snapshot) => {
                            self.starlark_debugger_state
                                .update(event.timestamp(), snapshot)?;
                        }
                        TagEvent(tags) => {
                            if tags.tags.contains(&"which-dice:Legacy".to_owned()) {
                                self.session_info.legacy_dice = true;
                            }
                        }
                        SystemInfo(system_info) => {
                            self.system_info = system_info.clone();
                            if let Some(client) = &mut self.health_check_client {
                                client.update_experiment_configurations(system_info).await;
                            }
                        }
                        TargetPatterns(tag) => {
                            if let Some(client) = &mut self.health_check_client {
                                client.update_parsed_target_patterns(tag).await;
                            }
                        }
                        DiceStateSnapshot(dice) => {
                            self.dice_state.update(dice);
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        }

        self.extra.observe(receive_time, event)?;

        Ok(())
    }

    pub fn spans(&self) -> &BuckEventSpanTracker {
        &self.span_tracker
    }

    pub fn action_stats(&self) -> &ActionStats {
        &self.action_stats
    }

    pub fn re_state(&self) -> &ReState {
        &self.re_state
    }

    pub fn two_snapshots(&self) -> &TwoSnapshots {
        &self.two_snapshots
    }

    pub fn system_info(&self) -> &buck2_data::SystemInfo {
        &self.system_info
    }

    pub fn session_info(&self) -> &SessionInfo {
        &self.session_info
    }

    pub fn starlark_debugger_state(&self) -> &StarlarkDebuggerState {
        &self.starlark_debugger_state
    }

    pub fn test_state(&self) -> &TestState {
        &self.test_state
    }

    pub fn extra(&self) -> &E {
        &self.extra
    }

    pub fn dice_state(&self) -> &DiceState {
        &self.dice_state
    }
}

pub trait EventObserverExtra: Send {
    fn new() -> Self;

    fn observe(&mut self, receive_time: Instant, event: &Arc<BuckEvent>)
    -> buck2_error::Result<()>;
}

/// This has more fields for debug info. We don't always capture those.
pub struct DebugEventObserverExtra {
    debug_events: DebugEventsState,
    progress_state: BuildProgressStateTracker,
}

impl EventObserverExtra for DebugEventObserverExtra {
    fn new() -> Self {
        Self {
            debug_events: DebugEventsState::new(),
            progress_state: BuildProgressStateTracker::new(),
        }
    }

    fn observe(
        &mut self,
        receive_time: Instant,
        event: &Arc<BuckEvent>,
    ) -> buck2_error::Result<()> {
        self.debug_events.handle_event(receive_time, event)?;
        self.progress_state.handle_event(receive_time, event)?;

        Ok(())
    }
}

impl DebugEventObserverExtra {
    pub fn debug_events(&self) -> &DebugEventsState {
        &self.debug_events
    }

    pub fn progress_state(&self) -> &BuildProgressStateTracker {
        &self.progress_state
    }
}

pub struct NoopEventObserverExtra;

impl EventObserverExtra for NoopEventObserverExtra {
    fn new() -> Self {
        Self
    }

    fn observe(
        &mut self,
        _receive_time: Instant,
        _event: &Arc<BuckEvent>,
    ) -> buck2_error::Result<()> {
        // Noop
        Ok(())
    }
}
