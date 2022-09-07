/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;

use superconsole::style::style;
use superconsole::style::StyledContent;
use superconsole::style::Stylize;
use superconsole::Component;
use superconsole::Dimensions;
use superconsole::DrawMode;
use superconsole::Line;
use superconsole::Lines;
use superconsole::Span;
use superconsole::State;

use crate::subscribers::display;
use crate::subscribers::display::TargetDisplayOptions;
use crate::subscribers::span_tracker::SpanInfo;
use crate::subscribers::superconsole::timed_list::Cutoffs;

#[derive(Debug)]
pub(crate) struct Table {
    pub(crate) rows: Vec<Row>,
}

impl Table {
    pub(crate) fn new() -> Self {
        Self { rows: Vec::new() }
    }

    pub(crate) fn len(&self) -> usize {
        self.rows.len()
    }
}

impl Component for Table {
    /// Zips together each time and label lines, but gives the times preferential treatment.
    fn draw_unchecked(
        &self,
        _state: &State,
        Dimensions { width, .. }: Dimensions,
        _mode: DrawMode,
    ) -> anyhow::Result<Lines> {
        let combined = self
            .rows
            .iter()
            .cloned()
            .map(|row| {
                let mut label = row.event;
                let mut time = row.time;
                let time_len = time.len();
                let padding = 1;
                let maximum_label_width = width.saturating_sub(time_len + padding);
                let original_label_len = label.len();
                let will_be_truncated = original_label_len > maximum_label_width;
                if will_be_truncated {
                    // make space for ellipses
                    let styling = label.0.last().unwrap().stylization;
                    label.truncate_line(maximum_label_width.saturating_sub(3));
                    label.0.push(match styling {
                        Some(style) => {
                            Span::new_styled_lossy(StyledContent::new(style, "...".to_owned()))
                        }
                        None => Span::new_unstyled_lossy("...".to_owned()),
                    });
                }

                // add extra padding to compensate for missing spaces between label and time
                label.pad_right(width.saturating_sub(time_len + label.len()));
                let mut combined = label;
                combined.0.append(&mut time.0);
                combined
            })
            .collect();

        Ok(combined)
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Row {
    event: Line,
    time: Line,
}

impl Row {
    pub(crate) fn span(
        padding: usize,
        span: &SpanInfo,
        time_speed: f64,
        cutoffs: &Cutoffs,
    ) -> anyhow::Result<Row> {
        let event = display::display_event(&span.event, TargetDisplayOptions::for_console())?;
        let time = display::duration_as_secs_elapsed(span.start.elapsed(), time_speed);
        let age = span.start.elapsed();
        Row::text(padding, event, time, age, cutoffs)
    }

    pub(crate) fn text(
        padding: usize,
        event: String,
        time: String,
        age: Duration,
        cutoffs: &Cutoffs,
    ) -> anyhow::Result<Row> {
        Row::styled(padding, style(event), style(time), age, cutoffs)
    }

    pub(crate) fn styled(
        padding: usize,
        event: StyledContent<String>,
        time: StyledContent<String>,
        age: Duration,
        cutoffs: &Cutoffs,
    ) -> anyhow::Result<Row> {
        let event = Span::new_styled(styled_for_delay(event, age, cutoffs))?;

        let line = if padding > 0 {
            superconsole::line![Span::padding(padding), event]
        } else {
            superconsole::line![event]
        };

        let time = superconsole::line![Span::new_styled(styled_for_delay(time, age, cutoffs))?];
        Ok(Row { event: line, time })
    }
}

/// This component echoes the `Lines` that have been stored in it.
#[derive(Debug)]
struct LinesComponent(Lines);

impl Component for LinesComponent {
    fn draw_unchecked(
        &self,
        _state: &State,
        _dimensions: Dimensions,
        _mode: DrawMode,
    ) -> anyhow::Result<Lines> {
        Ok(self.0.clone())
    }
}

/// Colorize based on time.
fn styled_for_delay(
    content: StyledContent<String>,
    elapsed: Duration,
    cutoffs: &Cutoffs,
) -> StyledContent<String> {
    if elapsed < cutoffs.inform {
        content
    } else if elapsed < cutoffs.warn {
        content.dark_yellow()
    } else {
        content.dark_red()
    }
}
