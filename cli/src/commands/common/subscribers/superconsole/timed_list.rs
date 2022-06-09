/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;

use events::subscriber::Tick;
use superconsole::{
    components::{
        bordering::BorderedSpec, splitting::SplitKind, Bordered, Bounded, Expanding, Split,
    },
    style::Stylize,
    Component, Dimensions, Direction, DrawMode, Lines, Span, State,
};

use self::table_builder::Table;
use crate::commands::common::subscribers::{
    display,
    simpleconsole::ActionStats,
    span_tracker::SpanTracker,
    superconsole::{
        common::{HeaderLineComponent, StaticStringComponent},
        TimeSpeed,
    },
};

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
    pub notable: Duration,
}

/// This component renders each event and a timer indicating for how long the event has been ongoing.
#[derive(Debug)]
pub struct TimedListBody(Bounded);

impl TimedListBody {
    pub fn new(max_size: usize, cutoffs: Cutoffs) -> Self {
        Self(Bounded::new(
            box Expanding::new(box TimedListBodyInner { max_size, cutoffs }),
            None,
            Some(max_size),
        ))
    }
}

#[derive(Debug)]
struct TimedListBodyInner {
    max_size: usize,
    cutoffs: Cutoffs,
}

impl Component for TimedListBody {
    fn draw_unchecked(
        &self,
        state: &State,
        dimensions: Dimensions,
        mode: DrawMode,
    ) -> anyhow::Result<Vec<superconsole::Line>> {
        self.0.draw(state, dimensions, mode)
    }
}

impl Component for TimedListBodyInner {
    fn draw_unchecked(
        &self,
        state: &State,
        dimensions: Dimensions,
        mode: DrawMode,
    ) -> anyhow::Result<Lines> {
        let spans = state.get::<SpanTracker>()?;

        let time_speed = state.get::<TimeSpeed>()?;

        let mut roots = spans.iter_roots();

        let mut builder = Table::new(&self.cutoffs);

        let mut first_not_rendered = None;

        for root in &mut roots {
            let info = root.info();

            // We need to estimate how many rows this will require to draw before deciding whether
            // or not to render it. See below for the logic that controls the actual rendering of
            // children.
            let rows = match root.children_count() {
                1 => 1,
                n => n + 1,
            };

            if builder.len() + rows >= self.max_size {
                first_not_rendered = Some(root);
                break;
            }

            let mut it = root.children();
            let (first, second) = (it.next(), it.next());

            match (first, second) {
                (Some(first), None) => {
                    // Special-case when we have exactly one child by collapsing.
                    let child_info = first.info();

                    let event_string = {
                        // always display the event and subaction
                        let mut builder = format!(
                            "{} [{}",
                            display::display_event(&info.event)?,
                            display::display_event(&child_info.event)?
                        );

                        let subaction_ratio = child_info.start.elapsed().as_secs_f64()
                            / info.start.elapsed().as_secs_f64();

                        // but only display the time of the subaction if it differs significantly.
                        if subaction_ratio < DISPLAY_SUBACTION_CUTOFF {
                            let subaction_time = display::duration_as_secs_elapsed(
                                child_info.start.elapsed(),
                                time_speed.speed(),
                            );
                            builder.push(' ');
                            builder.push_str(&subaction_time);
                        }

                        builder.push(']');
                        builder
                    };

                    builder.push().text(
                        event_string,
                        display::duration_as_secs_elapsed(info.start.elapsed(), time_speed.speed()),
                        info.start.elapsed(),
                    )?;
                }
                (first, second) => {
                    builder.push().span(info, time_speed.speed())?;

                    for child in first.into_iter().chain(second.into_iter()).chain(it) {
                        builder
                            .push()
                            .pad(2)
                            .span(child.info(), time_speed.speed())?;
                    }
                }
            }
        }

        // Add remaining unshown tasks, if any.
        if let Some(first) = first_not_rendered {
            let remaining_msg = format!("...and {} more not shown above.", roots.len() + 1);
            let longest_count = first.info().start.elapsed();
            let formatted_count =
                display::duration_as_secs_elapsed(longest_count, time_speed.speed());

            builder.push().styled(
                remaining_msg.italic(),
                formatted_count.italic(),
                longest_count,
            )?;
        }

        builder.draw(&superconsole::state![], dimensions, mode)
    }
}

/// This component is used to display summary counts about the number of jobs.
#[derive(Debug)]
struct CountComponent;

impl Component for CountComponent {
    fn draw_unchecked(
        &self,
        state: &State,
        _dimensions: Dimensions,
        mode: DrawMode,
    ) -> anyhow::Result<Lines> {
        let spans = state.get::<SpanTracker>()?;
        let action_stats = state.get::<ActionStats>()?;
        let time_speed = state.get::<TimeSpeed>()?;

        let finished = spans.roots_completed();
        let progress = spans.iter_roots().len();

        let elapsed = display::duration_as_secs_elapsed(
            state.get::<Tick>()?.elapsed_time,
            time_speed.speed(),
        );

        let cache_hit_percentage = action_stats.action_cache_hit_percentage();

        let contents = match mode {
            DrawMode::Normal => {
                if action_stats.log_stats() {
                    format!(
                        "Jobs: In progress: {}. Finished: {}. Cache hits: {}%. Time elapsed: {}",
                        progress, finished, cache_hit_percentage, elapsed
                    )
                } else {
                    format!(
                        "Jobs: In progress: {}. Finished: {}. Time elapsed: {}",
                        progress, finished, elapsed
                    )
                }
            }
            DrawMode::Final => {
                if action_stats.log_stats() {
                    format!(
                        "Jobs completed: {}. Time elapsed: {}. Cache hits: {}%. Commands: {} (cached: {}, remote: {}, local: {})",
                        finished,
                        elapsed,
                        cache_hit_percentage,
                        action_stats.total_executed_and_cached_actions(),
                        action_stats.cached_actions,
                        action_stats.remote_actions,
                        action_stats.local_actions
                    )
                } else {
                    format!("Jobs completed: {}. Time elapsed: {}.", finished, elapsed,)
                }
            }
        };
        let span = Span::new_unstyled(contents)?;
        Ok(vec![superconsole::line!(span)])
    }
}

/// Wrapper component for Header + Count
#[derive(Debug)]
pub struct TimedListHeader(Bordered);

impl TimedListHeader {
    pub fn new(header: String) -> Self {
        let info = box StaticStringComponent { header };
        let count = box CountComponent;
        let header_split = box HeaderLineComponent::new(info, count);
        let header_box = Bordered::new(
            header_split,
            BorderedSpec {
                bottom: Some(Span::sanitized("-")),
                top: None,
                left: None,
                right: None,
            },
        );

        Self(header_box)
    }
}

impl Component for TimedListHeader {
    fn draw_unchecked(
        &self,
        state: &State,
        dimensions: Dimensions,
        mode: DrawMode,
    ) -> anyhow::Result<Lines> {
        self.0.draw(state, dimensions, mode)
    }
}

/// Component that displays ongoing events and their durations + summary stats.
#[derive(Debug)]
pub struct TimedList {
    child: Split,
}

impl TimedList {
    /// * `max_events` is the maximum number of events displayed
    /// * `cutoffs` determines durations for warnings, time-outs, and baseline notability.
    /// * `header` is the string displayed at the top of the list.
    pub fn new(max_events: usize, cutoffs: Cutoffs, header: String) -> Self {
        let head = box TimedListHeader::new(header);
        // Subtract for the header and the padding row above and beneath
        let body = box TimedListBody::new(max_events, cutoffs);

        Self {
            child: Split::new(vec![head, body], Direction::Vertical, SplitKind::Adaptive),
        }
    }
}

impl Component for TimedList {
    fn draw_unchecked(
        &self,
        state: &State,
        dimensions: Dimensions,
        mode: DrawMode,
    ) -> anyhow::Result<Lines> {
        let span_tracker: &SpanTracker = state.get()?;

        match mode {
            DrawMode::Normal if !span_tracker.is_unused() => {
                self.child.draw(state, dimensions, mode)
            }
            // show a summary at the end
            DrawMode::Final => CountComponent.draw(state, dimensions, DrawMode::Final),
            _ => Ok(vec![]),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{convert::TryInto, time::SystemTime};

    use buck2_data::{FakeStart, SpanStartEvent};
    use events::{subscriber::Tick, BuckEvent, SpanId, TraceId};
    use superconsole::style::style;

    use super::*;

    const CUTOFFS: Cutoffs = Cutoffs {
        inform: Duration::from_secs(2),
        warn: Duration::from_secs(4),
        notable: Duration::from_millis(200),
    };

    fn get_span_start(event: &BuckEvent) -> &SpanStartEvent {
        match event.data {
            buck2_data::buck_event::Data::SpanStart(ref start) => start,
            _ => panic!("The buck event must be a start event"),
        }
    }

    #[test]
    fn test_normal() -> anyhow::Result<()> {
        let tick = Tick::now();
        let now = SystemTime::now();

        let timed_list = TimedList::new(5, CUTOFFS, "test".to_owned());
        let label = BuckEvent {
            timestamp: now,
            trace_id: TraceId::new(),
            span_id: Some(SpanId::new()),
            parent_id: None,
            data: buck2_data::buck_event::Data::SpanStart(SpanStartEvent {
                data: Some(buck2_data::span_start_event::Data::Fake(FakeStart {
                    caramba: "test".to_owned(),
                })),
            }),
        };

        let module = BuckEvent {
            timestamp: now,
            trace_id: TraceId::new(),
            span_id: Some(SpanId::new()),
            parent_id: None,
            data: buck2_data::buck_event::Data::SpanStart(SpanStartEvent {
                data: Some(buck2_data::span_start_event::Data::Fake(FakeStart {
                    caramba: "foo".to_owned(),
                })),
            }),
        };

        let mut state = SpanTracker::new();
        state
            .start_at(
                get_span_start(&label),
                &label,
                tick.start_time - Duration::from_secs(3),
            )
            .unwrap();
        state
            .start_at(
                get_span_start(&module),
                &module,
                tick.start_time - Duration::from_secs(1),
            )
            .unwrap();

        let time_speed = TimeSpeed::new(Some(1.0)).unwrap();
        let action_stats = ActionStats::new_with_local_remote_cached(0, 0, 1);

        let output = timed_list.draw(
            &superconsole::state!(&state, &tick, &time_speed, &action_stats),
            Dimensions { x: 40, y: 10 },
            DrawMode::Normal,
        )?;
        let expected = vec![
            vec!["test", "Jobs: In progress: 2. Finished: 0. C"].try_into()?,
            superconsole::line![Span::sanitized("-".repeat(40))],
            superconsole::line![
                Span::new_styled("test -- speak of the devil".to_owned().dark_yellow())?,
                Span::padding(10),
                Span::new_styled("3.0s".to_owned().dark_yellow())?,
            ],
            superconsole::line![
                Span::new_unstyled("foo -- speak of the devil".to_owned())?,
                Span::padding(11),
                Span::new_unstyled("1.0s".to_owned())?,
            ],
        ];

        pretty_assertions::assert_eq!(output, expected);

        Ok(())
    }

    #[test]
    fn test_remaining() -> anyhow::Result<()> {
        let tick = Tick::now();
        let now = SystemTime::now();

        let e1 = BuckEvent {
            timestamp: now,
            trace_id: TraceId::new(),
            span_id: Some(SpanId::new()),
            parent_id: None,
            data: buck2_data::buck_event::Data::SpanStart(SpanStartEvent {
                data: Some(buck2_data::span_start_event::Data::Fake(FakeStart {
                    caramba: "e1".to_owned(),
                })),
            }),
        };

        let e2 = BuckEvent {
            timestamp: now,
            trace_id: TraceId::new(),
            span_id: Some(SpanId::new()),
            parent_id: None,
            data: buck2_data::buck_event::Data::SpanStart(SpanStartEvent {
                data: Some(buck2_data::span_start_event::Data::Fake(FakeStart {
                    caramba: "e2".to_owned(),
                })),
            }),
        };

        let e3 = BuckEvent {
            timestamp: now,
            trace_id: TraceId::new(),
            span_id: Some(SpanId::new()),
            parent_id: None,
            data: buck2_data::buck_event::Data::SpanStart(SpanStartEvent {
                data: Some(buck2_data::span_start_event::Data::Fake(FakeStart {
                    caramba: "e3".to_owned(),
                })),
            }),
        };

        let mut state = SpanTracker::new();

        for e in vec![e1, e2, e3] {
            state
                .start_at(
                    get_span_start(&e),
                    &e,
                    tick.start_time - Duration::from_secs(1),
                )
                .unwrap();
        }

        let time_speed = TimeSpeed::new(Some(1.0)).unwrap();
        let timed_list = TimedList::new(2, CUTOFFS, "test".to_owned());
        let action_stats = ActionStats::new_with_local_remote_cached(0, 0, 1);

        let output = timed_list.draw(
            &superconsole::state!(&state, &tick, &time_speed, &action_stats),
            Dimensions { x: 40, y: 10 },
            DrawMode::Normal,
        )?;
        let expected = vec![
            vec!["test", "Jobs: In progress: 3. Finished: 0. C"].try_into()?,
            superconsole::line![Span::sanitized("-".repeat(40))],
            superconsole::line![
                Span::new_styled(style("e1 -- speak of the devil".to_owned()))?,
                Span::padding(12),
                Span::new_styled(style("1.0s".to_owned()))?,
            ],
            superconsole::line![
                Span::new_styled("...and 2 more not shown above.".to_owned().italic())?,
                Span::padding(6),
                Span::new_styled("1.0s".to_owned().italic())?,
            ],
        ];

        pretty_assertions::assert_eq!(output, expected);

        Ok(())
    }

    #[test]
    fn test_children() -> anyhow::Result<()> {
        let tick = Tick::now();
        let now = SystemTime::now();

        let parent = SpanId::new();

        let timed_list = TimedList::new(5, CUTOFFS, "test".to_owned());

        let action = BuckEvent {
            timestamp: now,
            trace_id: TraceId::new(),
            span_id: Some(parent),
            parent_id: None,
            data: SpanStartEvent {
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
        };

        let prepare = BuckEvent {
            timestamp: now,
            trace_id: TraceId::new(),
            span_id: Some(SpanId::new()),
            parent_id: Some(parent),
            data: SpanStartEvent {
                data: Some(
                    buck2_data::ExecutorStageStart {
                        stage: Some(buck2_data::PrepareAction {}.into()),
                    }
                    .into(),
                ),
            }
            .into(),
        };

        let mut state = SpanTracker::new();
        state
            .start_at(
                get_span_start(&action),
                &action,
                tick.start_time - Duration::from_secs(10),
            )
            .unwrap();
        state
            .start_at(
                get_span_start(&prepare),
                &prepare,
                tick.start_time - Duration::from_secs(5),
            )
            .unwrap();

        let time_speed = TimeSpeed::new(Some(1.0)).unwrap();

        let action_stats = ActionStats::new_with_local_remote_cached(0, 0, 1);

        let output = timed_list.draw(
            &superconsole::state!(&state, &tick, &time_speed, &action_stats),
            Dimensions { x: 80, y: 10 },
            DrawMode::Normal,
        )?;
        let expected = vec![
            vec![
                "test",
                "     ",
                "Jobs: In progress: 1. Finished: 0. Cache hits: 100%. Time elapsed: 0.0s",
            ]
            .try_into()?,
            superconsole::line![Span::sanitized("-".repeat(80))],
            superconsole::line![
                Span::new_styled(
                    "pkg:target -- running action (category identifier) [prepare 5.0s]"
                        .to_owned()
                        .dark_red()
                )?,
                Span::padding(10),
                Span::new_styled("10.0s".to_owned().dark_red())?,
            ],
        ];

        pretty_assertions::assert_eq!(output, expected);

        // Now, add another action. Normally we don't have multiple stages actually running
        // concurrently but this is a test!

        let re_download = BuckEvent {
            timestamp: now,
            trace_id: TraceId::new(),
            span_id: Some(SpanId::new()),
            parent_id: Some(parent),
            data: SpanStartEvent {
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
        };

        state
            .start_at(
                get_span_start(&re_download),
                &re_download,
                tick.start_time - Duration::from_secs(2),
            )
            .unwrap();

        let output = timed_list.draw(
            &superconsole::state!(&state, &tick, &time_speed, &action_stats),
            Dimensions { x: 80, y: 10 },
            DrawMode::Normal,
        )?;
        let expected = vec![
            vec![
                "test",
                "     ",
                "Jobs: In progress: 1. Finished: 0. Cache hits: 100%. Time elapsed: 0.0s",
            ]
            .try_into()?,
            superconsole::line![Span::sanitized("-".repeat(80))],
            superconsole::line![
                Span::new_styled(
                    "pkg:target -- running action (category identifier)"
                        .to_owned()
                        .dark_red()
                )?,
                Span::padding(25),
                Span::new_styled("10.0s".to_owned().dark_red())?,
            ],
            superconsole::line![
                Span::padding(2),
                Span::new_styled("prepare".to_owned().dark_red())?,
                Span::padding(67),
                Span::new_styled("5.0s".to_owned().dark_red())?,
            ],
            superconsole::line![
                Span::padding(2),
                Span::new_styled("re_download".to_owned().dark_yellow())?,
                Span::padding(63),
                Span::new_styled("2.0s".to_owned().dark_yellow())?,
            ],
        ];

        pretty_assertions::assert_eq!(output, expected);

        Ok(())
    }
}
