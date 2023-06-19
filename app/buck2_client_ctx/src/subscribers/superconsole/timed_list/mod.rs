/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Write;
use std::time::Duration;
use std::time::Instant;

use buck2_event_observer::display;
use buck2_event_observer::display::TargetDisplayOptions;
use buck2_event_observer::fmt_duration;
use buck2_event_observer::humanized::HumanizedCount;
use buck2_event_observer::pending_estimate::pending_estimate;
use buck2_event_observer::span_tracker::BuckEventSpanHandle;
use buck2_event_observer::span_tracker::BuckEventSpanTracker;
use superconsole::components::bordering::BorderedSpec;
use superconsole::components::Bordered;
use superconsole::components::DrawVertical;
use superconsole::style::Stylize;
use superconsole::Component;
use superconsole::Dimensions;
use superconsole::DrawMode;
use superconsole::Line;
use superconsole::Lines;
use superconsole::Span;

use self::table_builder::Table;
use crate::subscribers::superconsole::common::HeaderLineComponent;
use crate::subscribers::superconsole::common::StaticStringComponent;
use crate::subscribers::superconsole::timed_list::table_builder::Row;
use crate::subscribers::superconsole::timed_list::table_builder::TimedRow;
use crate::subscribers::superconsole::SuperConsoleState;

mod table_builder;

/// The minimum time disparity between a single subaction and a target elapsed time
/// before the former will be displayed separately from the latter's time.
/// Heuristic uses a percent difference to normalize for long running actions.
const DISPLAY_SUBACTION_CUTOFF: f64 = 0.9;

/// Information about notable event durations.
#[derive(Debug)]
pub struct Cutoffs {
    /// Cutoff for normal execution time.
    pub inform: Duration,
    /// Cutoff for abnormal but still OK execution time.
    pub warn: Duration,
    /// Minimum time an event must be alive before it is worth displaying.
    pub _notable: Duration,
}

/// This component renders each event and a timer indicating for how long the event has been ongoing.

struct TimedListBody<'c> {
    cutoffs: &'c Cutoffs,
    state: &'c SuperConsoleState,
}

impl<'c> TimedListBody<'c> {
    /// Render a root  as `root [first child + remaining children]`
    fn draw_root_first_child(
        &self,
        root: &BuckEventSpanHandle,
        single_child: BuckEventSpanHandle,
        remaining_children: usize,
        display_platform: bool,
    ) -> anyhow::Result<TimedRow> {
        let time_speed = self.state.time_speed;
        let info = root.info();
        let child_info = single_child.info();

        // always display the event and subaction
        let mut event_string = format!(
            "{} [{}",
            display::display_event(
                &info.event,
                TargetDisplayOptions::for_console(display_platform)
            )?,
            display::display_event(
                &child_info.event,
                TargetDisplayOptions::for_console(display_platform)
            )?
        );

        let now = Instant::now();
        let child_info_elapsed = now - child_info.start;
        let info_elapsed = now - info.start;
        let subaction_ratio = child_info_elapsed.as_secs_f64() / info_elapsed.as_secs_f64();

        // but only display the time of the subaction if it differs significantly.
        if subaction_ratio < DISPLAY_SUBACTION_CUTOFF {
            let subaction_time = fmt_duration::fmt_duration(child_info_elapsed, time_speed.speed());
            event_string.push(' ');
            event_string.push_str(&subaction_time);
        }

        if remaining_children > 0 {
            write!(event_string, " + {}", remaining_children)
                .expect("Write to String is not fallible");
        }

        event_string.push(']');

        TimedRow::text(
            0,
            event_string,
            fmt_duration::fmt_duration(info_elapsed, time_speed.speed()),
            info_elapsed.mul_f64(time_speed.speed()),
            self.cutoffs,
        )
    }

    fn draw_root(&self, root: &BuckEventSpanHandle) -> anyhow::Result<Vec<TimedRow>> {
        let time_speed = self.state.time_speed;
        let config = &self.state.config;
        let two_lines = config.two_lines;
        let display_platform = config.display_platform;
        let info = root.info();

        let mut it = root.children();

        match it.next() {
            Some(first) if !two_lines => Ok(vec![self.draw_root_first_child(
                root,
                first,
                it.len(),
                display_platform,
            )?]),
            first => {
                let mut rows = Vec::new();
                rows.push(TimedRow::span(
                    0,
                    info,
                    time_speed.speed(),
                    self.cutoffs,
                    display_platform,
                )?);

                for child in first.into_iter().chain(it) {
                    rows.push(TimedRow::span(
                        2,
                        child.info(),
                        time_speed.speed(),
                        self.cutoffs,
                        display_platform,
                    )?);
                }
                Ok(rows)
            }
        }
    }
}

impl<'c> Component for TimedListBody<'c> {
    fn draw_unchecked(&self, dimensions: Dimensions, mode: DrawMode) -> anyhow::Result<Lines> {
        let config = &self.state.config;
        let max_lines = config.max_lines;

        let observer = self.state.simple_console.observer();

        let spans = observer.spans();

        let mut roots = spans.iter_roots();

        let mut builder = Table::new();

        let mut first_not_rendered = None;

        for root in &mut roots {
            let rows = self.draw_root(&root)?;

            if builder.len() + rows.len() >= max_lines {
                first_not_rendered = Some(root);
                break;
            }

            builder.rows.extend(rows.into_iter().map(Row::from));
        }

        // Add remaining unshown tasks, if any.
        let more = roots.len() as u64 + first_not_rendered.map_or(0, |_| 1);

        if more > 0 {
            let remaining = format!("... and {} more currently executing", more);
            builder.rows.push(
                std::iter::once(Span::new_styled(remaining.italic())?)
                    .collect::<Line>()
                    .into(),
            );
        }

        builder.draw(dimensions, mode)
    }
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
        let time_speed = self.state.time_speed;

        let finished = spans.roots_completed() as u64;

        let elapsed =
            fmt_duration::fmt_duration(self.state.current_tick.elapsed_time, time_speed.speed());

        let pending = pending_estimate(spans.roots(), observer.extra().dice_state());

        let remaining = spans.iter_roots().len() as u64 + pending;
        let total = remaining + finished;

        let remaining = HumanizedCount::new(remaining);
        let total = HumanizedCount::new(total);

        let contents = match mode {
            DrawMode::Normal => {
                if action_stats.log_stats() {
                    let mut actions_summary = format!(
                        "Remaining: {}/{}. Cache hits: {}%. ",
                        remaining,
                        total,
                        action_stats.action_cache_hit_percentage()
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
                }
            }
            DrawMode::Final => {
                if action_stats.log_stats() {
                    format!(
                        "Jobs completed: {}. Time elapsed: {}. {}",
                        finished, elapsed, action_stats,
                    )
                } else {
                    format!("Jobs completed: {}. Time elapsed: {}.", finished, elapsed,)
                }
            }
        };
        Ok(Lines(vec![Line::unstyled(&contents)?]))
    }
}

/// Wrapper component for Header + Count
struct TimedListHeader<'s> {
    state: &'s SuperConsoleState,
    header: &'s str,
}

impl<'s> Component for TimedListHeader<'s> {
    fn draw_unchecked(&self, dimensions: Dimensions, mode: DrawMode) -> anyhow::Result<Lines> {
        let info = StaticStringComponent {
            header: self.header,
        };
        let header_split = HeaderLineComponent::new(info, CountComponent { state: self.state });
        let header_box = Bordered::new(
            header_split,
            BorderedSpec {
                bottom: Some(Span::dash()),
                top: None,
                left: None,
                right: None,
            },
        );

        header_box.draw(dimensions, mode)
    }
}

/// Component that displays ongoing events and their durations + summary stats.
pub struct TimedList<'a> {
    header: &'a str,
    cutoffs: &'a Cutoffs,
    state: &'a SuperConsoleState,
}

impl<'a> TimedList<'a> {
    /// * `cutoffs` determines durations for warnings, time-outs, and baseline notability.
    /// * `header` is the string displayed at the top of the list.
    pub fn new(cutoffs: &'a Cutoffs, header: &'a str, state: &'a SuperConsoleState) -> Self {
        Self {
            header,
            cutoffs,
            state,
        }
    }
}

impl<'a> Component for TimedList<'a> {
    fn draw_unchecked(&self, dimensions: Dimensions, mode: DrawMode) -> anyhow::Result<Lines> {
        let span_tracker: &BuckEventSpanTracker = self.state.simple_console.observer().spans();

        match mode {
            DrawMode::Normal if !span_tracker.is_unused() => {
                let header = TimedListHeader {
                    header: self.header,
                    state: self.state,
                };
                let body = TimedListBody {
                    cutoffs: self.cutoffs,
                    state: self.state,
                };

                let mut draw = DrawVertical::new(dimensions);
                draw.draw(&header, mode)?;
                draw.draw(&body, mode)?;
                Ok(draw.finish())
            }
            // show a summary at the end
            DrawMode::Final => {
                CountComponent { state: self.state }.draw(dimensions, DrawMode::Final)
            }
            _ => Ok(Lines::new()),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::sync::Arc;
    use std::time::UNIX_EPOCH;

    use buck2_core::fs::paths::file_name::FileNameBuf;
    use buck2_data::FakeStart;
    use buck2_data::SpanStartEvent;
    use buck2_event_observer::action_stats::ActionStats;
    use buck2_event_observer::verbosity::Verbosity;
    use buck2_events::span::SpanId;
    use buck2_events::BuckEvent;
    use buck2_wrapper_common::invocation_id::TraceId;
    use dupe::Dupe;
    use itertools::Itertools;

    use super::*;
    use crate::subscribers::subscriber::Tick;
    use crate::subscribers::superconsole::SuperConsoleConfig;
    use crate::subscribers::superconsole::TimeSpeed;

    const CUTOFFS: Cutoffs = Cutoffs {
        inform: Duration::from_secs(2),
        warn: Duration::from_secs(4),
        _notable: Duration::from_millis(200),
    };

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
            FileNameBuf::unchecked_new("ignore"),
            Verbosity::Default,
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
    fn test_normal() -> anyhow::Result<()> {
        let tick = Tick::now();

        let label = Arc::new(BuckEvent::new(
            UNIX_EPOCH,
            TraceId::new(),
            Some(SpanId::new()),
            None,
            buck2_data::buck_event::Data::SpanStart(SpanStartEvent {
                data: Some(buck2_data::span_start_event::Data::Fake(FakeStart {
                    caramba: "test".to_owned(),
                })),
            }),
        ));

        let module = Arc::new(BuckEvent::new(
            UNIX_EPOCH,
            TraceId::new(),
            Some(SpanId::new()),
            None,
            buck2_data::buck_event::Data::SpanStart(SpanStartEvent {
                data: Some(buck2_data::span_start_event::Data::Fake(FakeStart {
                    caramba: "foo".to_owned(),
                })),
            }),
        ));

        let mut state = BuckEventSpanTracker::new();
        state.start_at(&label, fake_time(&tick, 3)).unwrap();
        state.start_at(&module, fake_time(&tick, 1)).unwrap();

        let time_speed = fake_time_speed();
        let action_stats = ActionStats {
            local_actions: 0,
            remote_actions: 0,
            cached_actions: 1,
            fallback_actions: 0,
        };

        let timed_list_state = SuperConsoleConfig {
            max_lines: 5,
            ..Default::default()
        };

        let output = TimedList::new(
            &CUTOFFS,
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
        let expected = [
            "testRemaining: 2/2. Cache hits: 100%. Ti",
            "----------------------------------------",
            "<span fg=dark_yellow>test -- speak of the devil</span>          <span fg=dark_yellow>3.0s</span>",
            "foo -- speak of the devil           1.0s",
        ].iter().map(|l| format!("{}\n", l)).join("");

        pretty_assertions::assert_eq!(output.fmt_for_test().to_string(), expected);

        Ok(())
    }

    #[test]
    fn test_remaining() -> anyhow::Result<()> {
        let tick = Tick::now();

        let e1 = BuckEvent::new(
            UNIX_EPOCH,
            TraceId::new(),
            Some(SpanId::new()),
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
            Some(SpanId::new()),
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
            Some(SpanId::new()),
            None,
            buck2_data::buck_event::Data::SpanStart(SpanStartEvent {
                data: Some(buck2_data::span_start_event::Data::Fake(FakeStart {
                    caramba: "e3".to_owned(),
                })),
            }),
        );

        let mut state = BuckEventSpanTracker::new();

        for e in vec![e1, e2, e3] {
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
        };

        let timed_list_state = SuperConsoleConfig {
            max_lines: 2,
            ..Default::default()
        };

        let output = TimedList::new(
            &CUTOFFS,
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
        let expected = [
            "testRemaining: 3/3. Cache hits: 100%. Ti",
            "----------------------------------------",
            "e1 -- speak of the devil            1.0s",
            "<span italic>... and 2 more currently executing</span>",
        ]
        .iter()
        .map(|l| format!("{}\n", l))
        .join("");

        pretty_assertions::assert_eq!(output.fmt_for_test().to_string(), expected);

        Ok(())
    }

    #[test]
    fn test_remaining_with_pending() -> anyhow::Result<()> {
        let tick = Tick::now();

        let mut state = SuperConsoleState::new(
            None,
            TraceId::null(),
            FileNameBuf::unchecked_new("ignore"),
            Verbosity::Default,
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
                Some(SpanId::new()),
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
            let output = TimedList::new(&CUTOFFS, "test", &state).draw(
                Dimensions {
                    width: 60,
                    height: 10,
                },
                DrawMode::Normal,
            )?;

            let expected = [
                "test                      Remaining: 3/3. Time elapsed: 0.0s",
                "------------------------------------------------------------",
                "<span fg=dark_red>pkg:target -- action (category identifier)</span>             <span fg=dark_red>10.0s</span>",
            ].iter().map(|l| format!("{}\n", l)).join("");

            pretty_assertions::assert_eq!(output.fmt_for_test().to_string(), expected);
        }

        {
            state.config.max_lines = 1; // With fewer lines now

            let output = TimedList::new(&CUTOFFS, "test", &state).draw(
                Dimensions {
                    width: 60,
                    height: 10,
                },
                DrawMode::Normal,
            )?;

            let expected = [
                "test                      Remaining: 3/3. Time elapsed: 0.0s",
                "------------------------------------------------------------",
                "<span italic>... and 1 more currently executing</span>",
            ]
            .iter()
            .map(|l| format!("{}\n", l))
            .join("");

            pretty_assertions::assert_eq!(output.fmt_for_test().to_string(), expected);
        }

        Ok(())
    }

    #[test]
    fn test_children() -> anyhow::Result<()> {
        let tick = Tick::now();

        let parent = SpanId::new();

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
            Some(SpanId::new()),
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
        };

        let timed_list_state = SuperConsoleConfig {
            max_lines: 5,
            ..Default::default()
        };

        let output = TimedList::new(
            &CUTOFFS,
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
        let expected = [
            "test                        Remaining: 1/1. Cache hits: 100%. Time elapsed: 0.0s",
            "--------------------------------------------------------------------------------",
            "<span fg=dark_red>pkg:target -- action (category identifier) [prepare 5.0s]</span>                  <span fg=dark_red>10.0s</span>",
        ].iter().map(|l| format!("{}\n", l)).join("");

        pretty_assertions::assert_eq!(output.fmt_for_test().to_string(), expected);

        // Now, add another action. Normally we don't have multiple stages actually running
        // concurrently but this is a test!

        let re_download = Arc::new(BuckEvent::new(
            UNIX_EPOCH,
            TraceId::new(),
            Some(SpanId::new()),
            Some(parent),
            SpanStartEvent {
                data: Some(
                    buck2_data::ExecutorStageStart {
                        stage: Some(
                            buck2_data::ReStage {
                                stage: Some(buck2_data::ReDownload {}.into()),
                            }
                            .into(),
                        ),
                    }
                    .into(),
                ),
            }
            .into(),
        ));

        state.start_at(&re_download, fake_time(&tick, 2)).unwrap();

        let output = TimedList::new(
            &CUTOFFS,
            "test",
            &super_console_state_for_test(state, action_stats, tick, time_speed, timed_list_state),
        )
        .draw(
            Dimensions {
                width: 80,
                height: 10,
            },
            DrawMode::Normal,
        )?;
        let expected = [
            "test                        Remaining: 1/1. Cache hits: 100%. Time elapsed: 0.0s",
            "--------------------------------------------------------------------------------",
            "<span fg=dark_red>pkg:target -- action (category identifier) [prepare 5.0s + 1]</span>              <span fg=dark_red>10.0s</span>",
        ].iter().map(|l| format!("{}\n", l)).join("");

        pretty_assertions::assert_eq!(output.fmt_for_test().to_string(), expected);

        Ok(())
    }
}
