/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;

use buck2_event_observer::action_stats::ActionStats;
use buck2_event_observer::fmt_duration;
use buck2_event_observer::humanized::HumanizedCount;
use buck2_event_observer::pending_estimate::pending_estimate;
use buck2_event_observer::progress::BuildProgressPhaseStats;
use buck2_event_observer::progress::BuildProgressStats;
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
        if self.state.config.expanded_progress {
            let mut phase_stats = self.state.extra().progress_state().phase_stats();
            if let DrawMode::Final = mode {
                phase_stats.loads.mark_all_finished();
                phase_stats.analyses.mark_all_finished();
                phase_stats.actions.mark_all_finished();
            }

            ProgressHeader {
                header: self.header,
                phase_stats: &phase_stats,
                progress_stats: self.state.extra().progress_state().progress_stats(),
                action_stats: self.state.simple_console.observer.action_stats(),
                time_elapsed: time_elapsed(self.state),
            }
            .draw(dimensions, mode)
        } else {
            SimpleHeader::new(self.header, self.state).draw(dimensions, mode)
        }
    }
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
        let pending = pending_estimate(spans.roots(), observer.dice_state());
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

struct SimpleHeader<'s> {
    data: HeaderData<'s>,
}

impl<'s> SimpleHeader<'s> {
    fn new(header: &'s str, state: &'s SuperConsoleState) -> Self {
        Self::new_for_data(HeaderData::from_state(header, state))
    }

    fn new_for_data(data: HeaderData<'s>) -> Self {
        Self { data }
    }
}

impl<'s> Component for SimpleHeader<'s> {
    fn draw_unchecked(&self, dimensions: Dimensions, mode: DrawMode) -> anyhow::Result<Lines> {
        match mode {
            DrawMode::Normal => HeaderLineComponent::new(
                StaticStringComponent {
                    header: self.data.header,
                },
                CountComponent { data: &self.data },
            )
            .draw(dimensions, mode),
            DrawMode::Final => CountComponent { data: &self.data }.draw(dimensions, mode),
        }
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

pub(crate) struct ProgressHeader<'s> {
    header: &'s str,
    phase_stats: &'s BuildProgressPhaseStats,
    progress_stats: &'s BuildProgressStats,
    action_stats: &'s ActionStats,
    time_elapsed: String,
}

#[derive(Clone, Copy)]
enum Style {
    Normal(usize),
    Compact(usize),
    ExtraCompact,
}

impl Style {
    fn render(
        &self,
        mode: DrawMode,
        header: &str,
        mut pending: u64,
        total: u64,
        running_str: &str,
        running_num: u64,
    ) -> String {
        if let DrawMode::Final = mode {
            pending = 0;
        }
        let mut line = match self {
            Style::Normal(num_width) | Style::Compact(num_width) => format!(
                "{header} Remaining {pending:>num_width$}/{total:<num_width$}",
                header = header,
                pending = pending,
                total = total,
                num_width = *num_width
            ),
            Style::ExtraCompact => {
                format!(
                    "{header} Remaining {pending}/{total}",
                    header = header,
                    pending = pending,
                    total = total,
                )
            }
        };

        if let DrawMode::Normal = mode {
            line += &match self {
                Style::Normal(_) | Style::Compact(_) => {
                    format!(" (running: {running_str})", running_str = running_str,)
                }
                Style::ExtraCompact => {
                    format!(" ({running_num})", running_num = running_num,)
                }
            };
        }
        line
    }

    fn display_num(&self, num: u64) -> String {
        match self {
            Style::Normal(num_width) | Style::Compact(num_width) => {
                format!("{:num_width$}", num, num_width = num_width)
            }
            Style::ExtraCompact => format!("{}", num),
        }
    }
}

impl ProgressHeader<'_> {
    fn render_loads(&self, style: Style, mode: DrawMode) -> String {
        style.render(
            mode,
            "Loading targets.  ",
            self.phase_stats.loads.pending(),
            self.phase_stats.loads.started,
            &style.display_num(self.phase_stats.loads.running),
            self.phase_stats.loads.running,
        )
    }

    fn render_loads_extra(&self) -> String {
        let mut msgs = Vec::new();
        if self.progress_stats.dirs_read > 0 {
            msgs.push(format!("{} dirs read", self.progress_stats.dirs_read));
        }
        if self.progress_stats.targets > 0 {
            msgs.push(format!("{} targets declared", self.progress_stats.targets));
        }
        msgs.join(", ")
    }

    fn render_analyses(&self, style: Style, mode: DrawMode) -> String {
        style.render(
            mode,
            "Analyzing targets.",
            self.phase_stats.analyses.pending(),
            self.phase_stats.analyses.started,
            &style.display_num(self.phase_stats.analyses.running),
            self.phase_stats.analyses.running,
        )
    }

    fn render_analyses_extra(&self) -> String {
        let mut msgs = Vec::new();
        if self.progress_stats.actions_declared > 0 {
            msgs.push(format!("{} actions", self.progress_stats.actions_declared));
        }
        if self.progress_stats.artifacts_declared > 0 {
            msgs.push(format!(
                "{} artifacts declared",
                self.progress_stats.artifacts_declared
            ));
        }
        msgs.join(", ")
    }

    fn render_actions(&self, style: Style, mode: DrawMode) -> String {
        let phase_stats = &self.phase_stats.actions;

        let mut running = Vec::new();
        if self.progress_stats.running_local > 0 || self.action_stats.local_actions > 0 {
            running.push(format!(
                "{} local",
                style.display_num(self.progress_stats.running_local),
            ));
        }
        if self.progress_stats.running_remote > 0 || self.action_stats.remote_actions > 0 {
            running.push(format!(
                "{} remote",
                style.display_num(self.progress_stats.running_remote),
            ));
        }

        let running_str = if running.is_empty() {
            style.display_num(0)
        } else {
            running.join(", ")
        };

        style.render(
            mode,
            "Executing actions.",
            phase_stats.pending(),
            phase_stats.started,
            &running_str,
            phase_stats.running,
        )
    }

    fn render_actions_extra(&self) -> String {
        let exec_time_ms = self.progress_stats.exec_time_ms;
        if exec_time_ms > 0 {
            format!(
                "{} exec time total",
                fmt_duration::fmt_duration(Duration::from_millis(exec_time_ms), 1.0),
            )
        } else {
            String::new()
        }
    }

    fn render_actions_stats(&self, style: Style) -> String {
        match style {
            Style::Normal(_) | Style::Compact(_) => {
                let compact = matches!(style, Style::Compact(_));

                let mut res_types = Vec::new();
                if self.action_stats.local_actions > 0 {
                    res_types.push(format!("{} local", self.action_stats.local_actions));
                }
                if self.action_stats.remote_actions > 0 {
                    res_types.push(format!("{} remote", self.action_stats.remote_actions));
                }
                if self.action_stats.total_cached_actions() > 0 {
                    res_types.push(format!(
                        "{} cache ({}%{})",
                        self.action_stats.total_cached_actions(),
                        self.action_stats.total_cache_hit_percentage(),
                        if compact { "" } else { " hit" }
                    ));
                }

                if res_types.is_empty() {
                    String::new()
                } else {
                    format!(
                        "{}{}",
                        if compact { "" } else { "Finished " },
                        res_types.join(", ")
                    )
                }
            }

            Style::ExtraCompact => {
                if self.action_stats.total_cached_actions() > 0 {
                    format!(
                        "Cache hits {}%",
                        self.action_stats.total_cache_hit_percentage()
                    )
                } else {
                    String::new()
                }
            }
        }
    }

    fn render_actions_stats_extra(&self) -> String {
        let exec_time_ms = self.progress_stats.exec_time_ms;
        let cached_exec_time_ms = self.progress_stats.cached_exec_time_ms;

        if cached_exec_time_ms > 0 {
            format!(
                "{} exec time cached ({}%)",
                fmt_duration::fmt_duration(Duration::from_millis(cached_exec_time_ms), 1.0),
                cached_exec_time_ms * 100 / std::cmp::max(exec_time_ms, 1)
            )
        } else {
            String::new()
        }
    }
}

impl<'s> Component for ProgressHeader<'s> {
    fn draw_unchecked(&self, dimensions: Dimensions, mode: DrawMode) -> anyhow::Result<Lines> {
        fn digits_len(v: u64) -> usize {
            (v.checked_ilog10().unwrap_or(0) + 1) as usize
        }

        let loads = &self.phase_stats.loads;
        let analysis = &self.phase_stats.analyses;
        let actions = &self.phase_stats.actions;

        let max_total = std::cmp::max(
            std::cmp::max(loads.started, analysis.started),
            actions.started,
        );

        let num_width = std::cmp::max(5, digits_len(max_total));

        let header_width = "Executing actions. Remaining _/_ (running: _ local, _ remote)  ".len()
            + 4 * (num_width - 1);

        let elapsed = format!("Time elapsed: {}", &self.time_elapsed);

        // During normal drawing, the elapsed time is in the last row at the end. In the final rendering it gets its own line and is on the left.
        let inline_elapsed = match mode {
            DrawMode::Normal => &elapsed,
            DrawMode::Final => "",
        };

        let long_middle_len = "111222333 actions, 111222333 artifacts declared  ".len();

        let style = if header_width + long_middle_len < dimensions.width {
            Style::Normal(num_width)
        } else if header_width < dimensions.width {
            Style::Compact(num_width)
        } else {
            Style::ExtraCompact
        };

        let mut main = Vec::new();
        let mut extra = Vec::new();

        if loads.started > 0 {
            main.push(self.render_loads(style, mode));
            if let Style::Normal(..) = style {
                extra.push(self.render_loads_extra());
            } else {
                extra.push(String::new());
            }
        }

        if analysis.started > 0 {
            main.push(self.render_analyses(style, mode));
            if let Style::Normal(..) = style {
                extra.push(self.render_analyses_extra());
            } else {
                extra.push(String::new());
            }
        }

        if actions.started == 0 {
            main.push(self.header.to_owned());
            extra.push(String::new());
        } else {
            main.push(self.render_actions(style, mode));
            main.push(format!(
                // typically aligns this with "Remaining:" in the line above, but a long header would push it over, which is okay
                "{:<18} {}",
                self.header,
                self.render_actions_stats(if dimensions.width > 90 {
                    Style::Normal(num_width)
                } else {
                    style
                })
            ));
            if let Style::Normal(..) = style {
                extra.push(self.render_actions_extra());
                extra.push(self.render_actions_stats_extra());
            } else {
                extra.push(String::new());
                extra.push(String::new());
            }
        }

        assert!(!extra.is_empty());
        assert_eq!(main.len(), extra.len());

        // We now have the "main" column and the "extra" column and we want to lay them out. In addition, we're going to insert
        // the "Time elapsed: 12s" string at the end of the final line.
        //
        // The main column is printed on the left and then padded to align the extra column.
        // As long as there is less than `extra_preferred_width` space, the extra column will go immediately after the main column,
        // once it's wider than that we'll right align it.

        let main_width = main.iter().map(String::len).max().unwrap();

        let extra_preferred_width = long_middle_len + 20;
        let extra_width = extra.iter().map(String::len).max().unwrap();
        // need to append elapsed time to the final line
        let extra_min_width = 2 + std::cmp::max(
            extra_width,
            extra.last().unwrap().len() + inline_elapsed.len() + 2,
        );
        let extra_max_width = dimensions.width.saturating_sub(main_width + 2);

        // If there's not actually enough space to draw them both, we'll prefer for the extra column to be truncated.
        let extra_final_width = std::cmp::min(
            std::cmp::max(extra_preferred_width, extra_min_width),
            extra_max_width,
        );

        let pad_to = std::cmp::max(
            main_width,
            dimensions.width.saturating_sub(extra_final_width),
        );

        let mut lines = Vec::new();
        for i in 0..main.len() {
            let mut line = format!("{:<pad_to$}{}", main[i], extra[i], pad_to = pad_to);

            if i == main.len() - 1 {
                let wanted_len = dimensions.width.saturating_sub(inline_elapsed.len() + 2);
                if line.len() > wanted_len {
                    // If we're going to have to truncate the extra column for the elapsed time, just drop it in this row.
                    line = main[i].to_owned();
                }

                if line.len() < wanted_len {
                    line += &" ".repeat(wanted_len - line.len());
                } else {
                    line.truncate(wanted_len);
                }
                line += "  ";
                line += inline_elapsed;
            }

            lines.push(Line::unstyled(&line)?);
        }

        if let DrawMode::Final = mode {
            lines.push(Line::unstyled(&elapsed)?);
        }

        Ok(Lines(lines))
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Write;

    use buck2_error::conversion::from_any_with_tag;
    use buck2_event_observer::progress::BuildProgressPhaseStatsItem;
    use itertools::Itertools;

    use super::*;

    fn phase_stats() -> BuildProgressPhaseStats {
        BuildProgressPhaseStats {
            loads: BuildProgressPhaseStatsItem {
                started: 11111,
                finished: 111,
                running: 11,
            },
            analyses: BuildProgressPhaseStatsItem {
                started: 22222,
                finished: 222,
                running: 22,
            },
            actions: BuildProgressPhaseStatsItem {
                started: 33333,
                finished: 333,
                running: 100,
            },
        }
    }

    fn progress_stats() -> BuildProgressStats {
        BuildProgressStats {
            dirs_read: 111,
            targets: 22222,
            actions_declared: 3333333,
            artifacts_declared: 4444444,
            running_local: 55,
            running_remote: 66,
            exec_time_ms: 7777000,
            cached_exec_time_ms: 666000,
        }
    }

    fn action_stats() -> ActionStats {
        ActionStats {
            local_actions: 100,
            remote_actions: 122,
            cached_actions: 133,
            fallback_actions: 0,
            remote_dep_file_cached_actions: 0,
            excess_cache_misses: 0,
        }
    }

    #[test]
    fn test_different_sizes_dont_fail() -> buck2_error::Result<()> {
        let phase_stats = &phase_stats();
        let progress_stats = &progress_stats();
        let action_stats = &action_stats();
        for i in 0..120 {
            let header = ProgressHeader {
                header: "header",
                phase_stats,
                progress_stats,
                action_stats,
                time_elapsed: "1234s".to_owned(),
            };

            header
                .draw(
                    Dimensions {
                        width: i,
                        height: 10,
                    },
                    DrawMode::Normal,
                )
                .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))?;
            header
                .draw(
                    Dimensions {
                        width: i,
                        height: 10,
                    },
                    DrawMode::Final,
                )
                .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))?;
        }
        Ok(())
    }

    #[test]
    fn test_rendering_golden() -> buck2_error::Result<()> {
        let mut all_output = String::new();

        fn draw(
            width: usize,
            normal: bool,
            phase_stats: &BuildProgressPhaseStats,
        ) -> anyhow::Result<Lines> {
            ProgressHeader {
                header: "header",
                phase_stats,
                progress_stats: &progress_stats(),
                action_stats: &action_stats(),
                time_elapsed: "1234s".to_owned(),
            }
            .draw(
                Dimensions { width, height: 10 },
                if normal {
                    DrawMode::Normal
                } else {
                    DrawMode::Final
                },
            )
        }

        // 129 looks out of place here, but it tests the case where we have an extra column but Time elapsed won't quite fit.
        for width in [30, 40, 60, 80, 100, 129, 130, 140, 160] {
            writeln!(
                &mut all_output,
                "{}",
                draw(width, true, &phase_stats())
                    .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))?
                    .fmt_for_test()
            )
            .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))?;
        }

        for width in [60, 140] {
            writeln!(
                &mut all_output,
                "{}",
                draw(width, false, &phase_stats())
                    .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))?
                    .fmt_for_test()
            )
            .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))?;
        }

        let expected = indoc::indoc!(
            r#"
                Loading targets.   Remaining 1
                Analyzing targets. Remaining 2
                Executing actions. Remaining 3
                header     Time elapsed: 1234s

                Loading targets.   Remaining 11000/11111
                Analyzing targets. Remaining 22000/22222
                Executing actions. Remaining 33000/33333
                header               Time elapsed: 1234s

                Loading targets.   Remaining 11000/11111 (11)
                Analyzing targets. Remaining 22000/22222 (22)
                Executing actions. Remaining 33000/33333 (100)
                header             Cache hits 37%        Time elapsed: 1234s

                Loading targets.   Remaining 11000/11111 (running:    11)
                Analyzing targets. Remaining 22000/22222 (running:    22)
                Executing actions. Remaining 33000/33333 (running:    55 local,    66 remote)
                header             100 local, 122 remote, 133 cache (37%)    Time elapsed: 1234s

                Loading targets.   Remaining 11000/11111 (running:    11)
                Analyzing targets. Remaining 22000/22222 (running:    22)
                Executing actions. Remaining 33000/33333 (running:    55 local,    66 remote)
                header             Finished 100 local, 122 remote, 133 cache (37% hit)           Time elapsed: 1234s

                Loading targets.   Remaining 11000/11111 (running:    11)                      111 dirs read, 22222 targets declared
                Analyzing targets. Remaining 22000/22222 (running:    22)                      3333333 actions, 4444444 artifacts declared
                Executing actions. Remaining 33000/33333 (running:    55 local,    66 remote)  2:09:37.0s exec time total
                header             Finished 100 local, 122 remote, 133 cache (37% hit)                                        Time elapsed: 1234s

                Loading targets.   Remaining 11000/11111 (running:    11)                      111 dirs read, 22222 targets declared
                Analyzing targets. Remaining 22000/22222 (running:    22)                      3333333 actions, 4444444 artifacts declared
                Executing actions. Remaining 33000/33333 (running:    55 local,    66 remote)  2:09:37.0s exec time total
                header             Finished 100 local, 122 remote, 133 cache (37% hit)         11:06.0s exec time cached (8%)  Time elapsed: 1234s

                Loading targets.   Remaining 11000/11111 (running:    11)                      111 dirs read, 22222 targets declared
                Analyzing targets. Remaining 22000/22222 (running:    22)                      3333333 actions, 4444444 artifacts declared
                Executing actions. Remaining 33000/33333 (running:    55 local,    66 remote)  2:09:37.0s exec time total
                header             Finished 100 local, 122 remote, 133 cache (37% hit)         11:06.0s exec time cached (8%)            Time elapsed: 1234s

                Loading targets.   Remaining 11000/11111 (running:    11)                                  111 dirs read, 22222 targets declared
                Analyzing targets. Remaining 22000/22222 (running:    22)                                  3333333 actions, 4444444 artifacts declared
                Executing actions. Remaining 33000/33333 (running:    55 local,    66 remote)              2:09:37.0s exec time total
                header             Finished 100 local, 122 remote, 133 cache (37% hit)                     11:06.0s exec time cached (8%)                    Time elapsed: 1234s

                Loading targets.   Remaining 0/11111
                Analyzing targets. Remaining 0/22222
                Executing actions. Remaining 0/33333
                header             Cache hits 37%
                Time elapsed: 1234s

                Loading targets.   Remaining     0/11111                                111 dirs read, 22222 targets declared
                Analyzing targets. Remaining     0/22222                                3333333 actions, 4444444 artifacts declared
                Executing actions. Remaining     0/33333                                2:09:37.0s exec time total
                header             Finished 100 local, 122 remote, 133 cache (37% hit)  11:06.0s exec time cached (8%)
                Time elapsed: 1234s

        "#
        );

        // copy-paste is easier if we don't need to worry about getting trailing spaces right
        let expected = expected.lines().map(str::trim_end).join("\n");
        let all_output = all_output.lines().map(str::trim_end).join("\n");

        // don't use pretty_assertions here because we mostly just want to copy-paste the golden
        assert!(
            all_output == expected,
            "GOLDEN:\n{}\nEND_GOLDEN\nEXPECTED:\n{}\nEND_EXPECTED",
            all_output,
            expected
        );

        Ok(())
    }

    #[test]
    fn test_remaining() -> buck2_error::Result<()> {
        let action_stats = ActionStats {
            local_actions: 0,
            remote_actions: 0,
            cached_actions: 1,
            fallback_actions: 0,
            remote_dep_file_cached_actions: 0,
            excess_cache_misses: 0,
        };
        let output = SimpleHeader::new_for_data(HeaderData {
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
        )
        .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))?;
        let expected = "testRemaining: 3/3. Cache hits: 100%. Ti\n".to_owned();

        pretty_assertions::assert_eq!(output.fmt_for_test().to_string(), expected);

        Ok(())
    }

    #[test]
    fn test_remaining_with_pending() -> buck2_error::Result<()> {
        let action_stats = ActionStats {
            local_actions: 0,
            remote_actions: 0,
            cached_actions: 0,
            fallback_actions: 0,
            remote_dep_file_cached_actions: 0,
            excess_cache_misses: 0,
        };
        let output = SimpleHeader::new_for_data(HeaderData {
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
        )
        .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))?;

        let expected = "test                      Remaining: 2/2. Time elapsed: 0.0s\n".to_owned();

        pretty_assertions::assert_eq!(output.fmt_for_test().to_string(), expected);

        Ok(())
    }

    #[test]
    fn test_children() -> buck2_error::Result<()> {
        let action_stats = ActionStats {
            local_actions: 0,
            remote_actions: 0,
            cached_actions: 1,
            fallback_actions: 0,
            remote_dep_file_cached_actions: 0,
            excess_cache_misses: 0,
        };
        let output = SimpleHeader::new_for_data(HeaderData {
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
        )
        .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))?;
        let expected =
            "test                        Remaining: 1/1. Cache hits: 100%. Time elapsed: 0.0s\n"
                .to_owned();

        pretty_assertions::assert_eq!(output.fmt_for_test().to_string(), expected);

        Ok(())
    }

    #[test]
    fn test_simple_header_final() -> buck2_error::Result<()> {
        let action_stats = ActionStats {
            local_actions: 0,
            remote_actions: 0,
            cached_actions: 1,
            fallback_actions: 0,
            remote_dep_file_cached_actions: 0,
            excess_cache_misses: 0,
        };
        let output = SimpleHeader::new_for_data(HeaderData {
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
            DrawMode::Final,
        )
        .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))?;
        let expected = indoc::indoc!(
            r#"
            Jobs completed: 0. Time elapsed: 0.0s.
            Cache hits: 100%. Commands: 1 (cached: 1, remote: 0, local: 0)
            "#
        );

        pretty_assertions::assert_eq!(output.fmt_for_test().to_string(), expected);

        Ok(())
    }
}
