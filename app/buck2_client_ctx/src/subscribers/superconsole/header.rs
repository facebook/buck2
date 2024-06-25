/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_event_observer::fmt_duration;
use buck2_event_observer::humanized::HumanizedCount;
use buck2_event_observer::pending_estimate::pending_estimate;
use superconsole::Component;
use superconsole::Dimensions;
use superconsole::DrawMode;
use superconsole::Line;
use superconsole::Lines;

use crate::subscribers::superconsole::common::HeaderLineComponent;
use crate::subscribers::superconsole::common::StaticStringComponent;
use crate::subscribers::superconsole::SuperConsoleState;

pub(crate) struct TasksHeader<'s> {
    header: &'s str,
    state: &'s SuperConsoleState,
}

impl<'s> TasksHeader<'s> {
    pub fn new(header: &'s str, state: &'s SuperConsoleState) -> Self {
        Self { header, state }
    }
}

impl<'s> Component for TasksHeader<'s> {
    fn draw_unchecked(&self, dimensions: Dimensions, mode: DrawMode) -> anyhow::Result<Lines> {
        let info = StaticStringComponent {
            header: self.header,
        };
        HeaderLineComponent::new(info, CountComponent { state: self.state }).draw(dimensions, mode)
    }
}

fn time_elapsed(state: &SuperConsoleState) -> String {
    fmt_duration::fmt_duration(state.current_tick.elapsed_time, state.time_speed.speed())
}

/// This component is used to display summary counts about the number of jobs.
struct CountComponent<'s> {
    state: &'s SuperConsoleState,
}

impl<'s> Component for CountComponent<'s> {
    fn draw_unchecked(&self, _dimensions: Dimensions, mode: DrawMode) -> anyhow::Result<Lines> {
        let observer = self.state.simple_console.observer();
        let spans = observer.spans();
        let action_stats = self.state.simple_console.observer().action_stats();

        let finished = spans.roots_completed() as u64;

        let elapsed = time_elapsed(self.state);

        match mode {
            DrawMode::Normal => {
                let pending = pending_estimate(spans.roots(), observer.extra().dice_state());

                let remaining = spans.iter_roots().len() as u64 + pending;
                let total = remaining + finished;

                let remaining = HumanizedCount::new(remaining);
                let total = HumanizedCount::new(total);

                let contents = if action_stats.log_stats() {
                    let mut actions_summary = format!(
                        "Remaining: {}/{}. Cache hits: {}%. ",
                        remaining,
                        total,
                        action_stats.total_cache_hit_percentage()
                    );
                    if action_stats.fallback_actions > 0 {
                        actions_summary += format!(
                            "Fallback: {}/{}. ",
                            HumanizedCount::new(action_stats.fallback_actions),
                            HumanizedCount::new(action_stats.total_executed_actions())
                        )
                        .as_str();
                    }
                    actions_summary += format!("Time elapsed: {}", elapsed).as_str();
                    actions_summary
                } else {
                    format!(
                        "Remaining: {}/{}. Time elapsed: {}",
                        remaining, total, elapsed
                    )
                };
                Ok(Lines(vec![Line::unstyled(&contents)?]))
            }
            DrawMode::Final => {
                let mut lines = vec![Line::unstyled(&format!(
                    "Jobs completed: {}. Time elapsed: {}.",
                    finished, elapsed,
                ))?];
                if action_stats.log_stats() {
                    lines.push(Line::unstyled(&action_stats.to_string())?);
                }
                Ok(Lines(lines))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::sync::Arc;
    use std::time::Duration;
    use std::time::Instant;
    use std::time::UNIX_EPOCH;

    use buck2_data::FakeStart;
    use buck2_data::SpanStartEvent;
    use buck2_event_observer::action_stats::ActionStats;
    use buck2_event_observer::span_tracker::BuckEventSpanTracker;
    use buck2_event_observer::verbosity::Verbosity;
    use buck2_events::span::SpanId;
    use buck2_events::BuckEvent;
    use buck2_wrapper_common::invocation_id::TraceId;
    use dupe::Dupe;

    use super::*;
    use crate::subscribers::subscriber::Tick;
    use crate::subscribers::superconsole::SuperConsoleConfig;
    use crate::subscribers::superconsole::TimeSpeed;

    const TIME_DILATION: u64 = 10;

    fn fake_time_speed() -> TimeSpeed {
        // We run time 10x slower so that any time occurring due to the
        // test running on an overloaded server is ignored.
        //
        // Note that going to 100x slower causes Windows CI to fail, because
        // the `Instant` can't go below the time when the VM was booted, or you get an
        // underflow of `Instant`.
        TimeSpeed::new(Some(1.0 / (TIME_DILATION as f64))).unwrap()
    }

    fn fake_time(tick: &Tick, secs: u64) -> Instant {
        tick.start_time
            .checked_sub(Duration::from_secs(secs * TIME_DILATION))
            .unwrap_or_else(|| {
                panic!(
                    "Instant went too low: {:?} - ({secs} * {TIME_DILATION}",
                    tick.start_time
                )
            })
            // We add 50ms to give us a 100ms window where we round down correctly
            .checked_add(Duration::from_millis(50 * TIME_DILATION))
            .unwrap()
    }

    fn super_console_state_for_test(
        span_tracker: BuckEventSpanTracker,
        action_stats: ActionStats,
        tick: Tick,
        time_speed: TimeSpeed,
        timed_list_state: SuperConsoleConfig,
    ) -> SuperConsoleState {
        let mut state = SuperConsoleState::new(
            None,
            TraceId::null(),
            Verbosity::default(),
            false,
            timed_list_state,
        )
        .unwrap();
        state.simple_console.observer.span_tracker = span_tracker;
        state.simple_console.observer.action_stats = action_stats;
        state.current_tick = tick;
        state.time_speed = time_speed;
        state
    }

    #[test]
    fn test_remaining() -> anyhow::Result<()> {
        let tick = Tick::now();

        let e1 = BuckEvent::new(
            UNIX_EPOCH,
            TraceId::new(),
            Some(SpanId::next()),
            None,
            buck2_data::buck_event::Data::SpanStart(SpanStartEvent {
                data: Some(buck2_data::span_start_event::Data::Fake(FakeStart {
                    caramba: "e1".to_owned(),
                })),
            }),
        );

        let e2 = BuckEvent::new(
            UNIX_EPOCH,
            TraceId::new(),
            Some(SpanId::next()),
            None,
            buck2_data::buck_event::Data::SpanStart(SpanStartEvent {
                data: Some(buck2_data::span_start_event::Data::Fake(FakeStart {
                    caramba: "e2".to_owned(),
                })),
            }),
        );

        let e3 = BuckEvent::new(
            UNIX_EPOCH,
            TraceId::new(),
            Some(SpanId::next()),
            None,
            buck2_data::buck_event::Data::SpanStart(SpanStartEvent {
                data: Some(buck2_data::span_start_event::Data::Fake(FakeStart {
                    caramba: "e3".to_owned(),
                })),
            }),
        );

        let mut state = BuckEventSpanTracker::new();

        for e in [e1, e2, e3] {
            state
                .start_at(&Arc::new(e.clone()), fake_time(&tick, 1))
                .unwrap();
        }

        let time_speed = fake_time_speed();
        let action_stats = ActionStats {
            local_actions: 0,
            remote_actions: 0,
            cached_actions: 1,
            fallback_actions: 0,
            remote_dep_file_cached_actions: 0,
        };

        let timed_list_state = SuperConsoleConfig {
            max_lines: 2,
            ..Default::default()
        };

        let output = TasksHeader::new(
            "test",
            &super_console_state_for_test(state, action_stats, tick, time_speed, timed_list_state),
        )
        .draw(
            Dimensions {
                width: 40,
                height: 10,
            },
            DrawMode::Normal,
        )?;
        let expected = "testRemaining: 3/3. Cache hits: 100%. Ti\n".to_owned();

        pretty_assertions::assert_eq!(output.fmt_for_test().to_string(), expected);

        Ok(())
    }

    #[test]
    fn test_remaining_with_pending() -> anyhow::Result<()> {
        let tick = Tick::now();

        let mut state = SuperConsoleState::new(
            None,
            TraceId::null(),
            Verbosity::default(),
            false,
            SuperConsoleConfig {
                max_lines: 2,
                ..Default::default()
            },
        )?;

        state.time_speed = fake_time_speed();
        state.current_tick = tick.clone();

        state.simple_console.observer.observe(
            fake_time(&tick, 10),
            &Arc::new(BuckEvent::new(
                UNIX_EPOCH,
                TraceId::new(),
                Some(SpanId::next()),
                None,
                SpanStartEvent {
                    data: Some(
                        buck2_data::ActionExecutionStart {
                            key: Some(buck2_data::ActionKey {
                                id: Default::default(),
                                owner: Some(buck2_data::action_key::Owner::TargetLabel(
                                    buck2_data::ConfiguredTargetLabel {
                                        label: Some(buck2_data::TargetLabel {
                                            package: "pkg".into(),
                                            name: "target".into(),
                                        }),
                                        configuration: Some(buck2_data::Configuration {
                                            full_name: "conf".into(),
                                        }),
                                        execution_configuration: None,
                                    },
                                )),
                                key: "".to_owned(),
                            }),
                            name: Some(buck2_data::ActionName {
                                category: "category".into(),
                                identifier: "identifier".into(),
                            }),
                            kind: buck2_data::ActionKind::NotSet as i32,
                        }
                        .into(),
                    ),
                }
                .into(),
            )),
        )?;

        state.simple_console.observer.observe(
            fake_time(&tick, 1),
            &Arc::new(BuckEvent::new(
                UNIX_EPOCH,
                TraceId::new(),
                None,
                None,
                buck2_data::InstantEvent {
                    data: Some(
                        buck2_data::DiceStateSnapshot {
                            key_states: {
                                let mut map = HashMap::new();
                                map.insert(
                                    "BuildKey".to_owned(),
                                    buck2_data::DiceKeyState {
                                        started: 4,
                                        finished: 2,
                                        check_deps_started: 2,
                                        check_deps_finished: 1,
                                        compute_started: 5,
                                        compute_finished: 2,
                                    },
                                );
                                map
                            },
                        }
                        .into(),
                    ),
                }
                .into(),
            )),
        )?;

        {
            let output = TasksHeader::new("test", &state).draw(
                Dimensions {
                    width: 60,
                    height: 10,
                },
                DrawMode::Normal,
            )?;

            let expected =
                "test                      Remaining: 2/2. Time elapsed: 0.0s\n".to_owned();

            pretty_assertions::assert_eq!(output.fmt_for_test().to_string(), expected);
        }

        Ok(())
    }

    #[test]
    fn test_children() -> anyhow::Result<()> {
        let tick = Tick::now();

        let parent = SpanId::next();

        let action = Arc::new(BuckEvent::new(
            UNIX_EPOCH,
            TraceId::new(),
            Some(parent),
            None,
            SpanStartEvent {
                data: Some(
                    buck2_data::ActionExecutionStart {
                        key: Some(buck2_data::ActionKey {
                            id: Default::default(),
                            owner: Some(buck2_data::action_key::Owner::TargetLabel(
                                buck2_data::ConfiguredTargetLabel {
                                    label: Some(buck2_data::TargetLabel {
                                        package: "pkg".into(),
                                        name: "target".into(),
                                    }),
                                    configuration: Some(buck2_data::Configuration {
                                        full_name: "conf".into(),
                                    }),
                                    execution_configuration: None,
                                },
                            )),
                            key: "".to_owned(),
                        }),
                        name: Some(buck2_data::ActionName {
                            category: "category".into(),
                            identifier: "identifier".into(),
                        }),
                        kind: buck2_data::ActionKind::NotSet as i32,
                    }
                    .into(),
                ),
            }
            .into(),
        ));

        let prepare = Arc::new(BuckEvent::new(
            UNIX_EPOCH,
            TraceId::new(),
            Some(SpanId::next()),
            Some(parent),
            SpanStartEvent {
                data: Some(
                    buck2_data::ExecutorStageStart {
                        stage: Some(buck2_data::PrepareAction {}.into()),
                    }
                    .into(),
                ),
            }
            .into(),
        ));

        let mut state = BuckEventSpanTracker::new();
        state.start_at(&action, fake_time(&tick, 10)).unwrap();
        state.start_at(&prepare, fake_time(&tick, 5)).unwrap();

        let time_speed = fake_time_speed();

        let action_stats = ActionStats {
            local_actions: 0,
            remote_actions: 0,
            cached_actions: 1,
            fallback_actions: 0,
            remote_dep_file_cached_actions: 0,
        };

        let timed_list_state = SuperConsoleConfig {
            max_lines: 5,
            ..Default::default()
        };

        let output = TasksHeader::new(
            "test",
            &super_console_state_for_test(
                state.clone(),
                action_stats.dupe(),
                tick.dupe(),
                time_speed,
                timed_list_state.clone(),
            ),
        )
        .draw(
            Dimensions {
                width: 80,
                height: 10,
            },
            DrawMode::Normal,
        )?;
        let expected =
            "test                        Remaining: 1/1. Cache hits: 100%. Time elapsed: 0.0s\n"
                .to_owned();

        pretty_assertions::assert_eq!(output.fmt_for_test().to_string(), expected);

        Ok(())
    }
}
