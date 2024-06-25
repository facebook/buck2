/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_event_observer::action_stats::ActionStats;
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
    data: HeaderData<'s>,
}

struct HeaderData<'s> {
    header: &'s str,
    action_stats: &'s ActionStats,
    elapsed_str: String,
    finished: u64,
    remaining: u64,
}

impl<'s> HeaderData<'s> {
    fn from_state(header: &'s str, state: &'s SuperConsoleState) -> Self {
        let observer = state.simple_console.observer();
        let spans = observer.spans();
        let pending = pending_estimate(spans.roots(), observer.extra().dice_state());
        let finished = spans.roots_completed() as u64;
        let remaining = spans.iter_roots().len() as u64 + pending;

        HeaderData {
            header,
            action_stats: state.simple_console.observer().action_stats(),
            elapsed_str: time_elapsed(state),
            finished,
            remaining,
        }
    }

    fn total(&self) -> u64 {
        self.finished + self.remaining
    }
}

impl<'s> TasksHeader<'s> {
    pub fn new(header: &'s str, state: &'s SuperConsoleState) -> Self {
        Self::new_for_data(HeaderData::from_state(header, state))
    }

    fn new_for_data(data: HeaderData<'s>) -> Self {
        Self { data }
    }
}

impl<'s> Component for TasksHeader<'s> {
    fn draw_unchecked(&self, dimensions: Dimensions, mode: DrawMode) -> anyhow::Result<Lines> {
        let info = StaticStringComponent {
            header: self.data.header,
        };
        HeaderLineComponent::new(info, CountComponent { data: &self.data }).draw(dimensions, mode)
    }
}

fn time_elapsed(state: &SuperConsoleState) -> String {
    fmt_duration::fmt_duration(state.current_tick.elapsed_time, state.time_speed.speed())
}

/// This component is used to display summary counts about the number of jobs.
struct CountComponent<'s> {
    data: &'s HeaderData<'s>,
}

impl<'s> Component for CountComponent<'s> {
    fn draw_unchecked(&self, _dimensions: Dimensions, mode: DrawMode) -> anyhow::Result<Lines> {
        match mode {
            DrawMode::Normal => {
                let remaining = HumanizedCount::new(self.data.remaining);
                let total = HumanizedCount::new(self.data.total());

                let contents = if self.data.action_stats.log_stats() {
                    let mut actions_summary = format!(
                        "Remaining: {}/{}. Cache hits: {}%. ",
                        remaining,
                        total,
                        self.data.action_stats.total_cache_hit_percentage()
                    );
                    if self.data.action_stats.fallback_actions > 0 {
                        actions_summary += format!(
                            "Fallback: {}/{}. ",
                            HumanizedCount::new(self.data.action_stats.fallback_actions),
                            HumanizedCount::new(self.data.action_stats.total_executed_actions())
                        )
                        .as_str();
                    }
                    actions_summary += format!("Time elapsed: {}", self.data.elapsed_str).as_str();
                    actions_summary
                } else {
                    format!(
                        "Remaining: {}/{}. Time elapsed: {}",
                        self.data.remaining,
                        self.data.total(),
                        self.data.elapsed_str
                    )
                };
                Ok(Lines(vec![Line::unstyled(&contents)?]))
            }
            DrawMode::Final => {
                let mut lines = vec![Line::unstyled(&format!(
                    "Jobs completed: {}. Time elapsed: {}.",
                    self.data.finished, self.data.elapsed_str,
                ))?];
                if self.data.action_stats.log_stats() {
                    lines.push(Line::unstyled(&self.data.action_stats.to_string())?);
                }
                Ok(Lines(lines))
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_remaining() -> anyhow::Result<()> {
        let action_stats = ActionStats {
            local_actions: 0,
            remote_actions: 0,
            cached_actions: 1,
            fallback_actions: 0,
            remote_dep_file_cached_actions: 0,
        };
        let output = TasksHeader::new_for_data(HeaderData {
            header: "test",
            action_stats: &action_stats,
            elapsed_str: "123s".to_owned(),
            finished: 0,
            remaining: 3,
        })
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
        let action_stats = ActionStats {
            local_actions: 0,
            remote_actions: 0,
            cached_actions: 0,
            fallback_actions: 0,
            remote_dep_file_cached_actions: 0,
        };
        let output = TasksHeader::new_for_data(HeaderData {
            header: "test",
            action_stats: &action_stats,
            elapsed_str: "0.0s".to_owned(),
            finished: 0,
            remaining: 2,
        })
        .draw(
            Dimensions {
                width: 60,
                height: 10,
            },
            DrawMode::Normal,
        )?;

        let expected = "test                      Remaining: 2/2. Time elapsed: 0.0s\n".to_owned();

        pretty_assertions::assert_eq!(output.fmt_for_test().to_string(), expected);

        Ok(())
    }

    #[test]
    fn test_children() -> anyhow::Result<()> {
        let action_stats = ActionStats {
            local_actions: 0,
            remote_actions: 0,
            cached_actions: 1,
            fallback_actions: 0,
            remote_dep_file_cached_actions: 0,
        };
        let output = TasksHeader::new_for_data(HeaderData {
            header: "test",
            action_stats: &action_stats,
            elapsed_str: "0.0s".to_owned(),
            finished: 0,
            remaining: 1,
        })
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
