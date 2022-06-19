/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;

use superconsole::{
    style::{style, StyledContent, Stylize},
    Component, Dimensions, DrawMode, Line, Lines, Span, State,
};

use crate::commands::common::subscribers::{
    span_tracker::SpanInfo,
    superconsole::{display, timed_list::Cutoffs},
};

#[derive(Debug)]
pub(crate) struct Table<'a> {
    events: Vec<Line>,
    times: Vec<Line>,
    cutoffs: &'a Cutoffs,
}

impl<'a> Table<'a> {
    pub(crate) fn new(cutoffs: &'a Cutoffs) -> Self {
        Self {
            events: Default::default(),
            times: Default::default(),
            cutoffs,
        }
    }

    pub(crate) fn len(&self) -> usize {
        self.events.len()
    }

    pub(crate) fn push<'b>(&'b mut self) -> Push<'a, 'b>
    where
        'a: 'b,
    {
        Push {
            builder: self,
            padding: 0,
        }
    }
}

impl Component for Table<'_> {
    /// Zips together each time and label lines, but gives the times preferential treatment.
    fn draw_unchecked(
        &self,
        _state: &State,
        Dimensions { x, .. }: Dimensions,
        _mode: DrawMode,
    ) -> anyhow::Result<Lines> {
        let combined = self
            .events
            .clone()
            .into_iter()
            .zip(self.times.clone().into_iter())
            .map(|(mut label, mut time)| {
                let time_len = time.len();
                let padding = 1;
                let maximum_label_width = x - time_len - padding;
                let original_label_len = label.len();
                let will_be_truncated = original_label_len > maximum_label_width;
                if will_be_truncated {
                    // make space for ellipses
                    let styling = label.0.last().unwrap().stylization;
                    label.truncate_line(maximum_label_width - 3);
                    label.0.push(match styling {
                        Some(style) => {
                            Span::new_styled_lossy(StyledContent::new(style, "...".to_owned()))
                        }
                        None => Span::new_unstyled_lossy("...".to_owned()),
                    });
                }

                // add extra padding to compensate for missing spaces between label and time
                label.pad_right(x.saturating_sub(time_len + label.len()));
                let mut combined = label;
                combined.0.append(&mut time.0);
                combined
            })
            .collect();

        Ok(combined)
    }
}

pub(crate) struct Push<'a, 'b> {
    builder: &'b mut Table<'a>,
    padding: usize,
}

impl<'a, 'b> Push<'a, 'b> {
    pub(crate) fn pad(mut self, padding: usize) -> Self {
        self.padding = padding;
        self
    }

    pub(crate) fn span(self, span: &SpanInfo, time_speed: f64) -> anyhow::Result<()> {
        self.text(
            display::display_event(&span.event)?,
            display::duration_as_secs_elapsed(span.start.elapsed(), time_speed),
            span.start.elapsed(),
        )
    }

    pub(crate) fn text(self, event: String, time: String, age: Duration) -> anyhow::Result<()> {
        self.styled(style(event), style(time), age)
    }

    pub(crate) fn styled(
        self,
        event: StyledContent<String>,
        time: StyledContent<String>,
        age: Duration,
    ) -> anyhow::Result<()> {
        let event = Span::new_styled(styled_for_delay(event, age, self.builder.cutoffs))?;

        let line = if self.padding > 0 {
            superconsole::line![Span::padding(self.padding), event]
        } else {
            superconsole::line![event]
        };

        self.builder.events.push(line);

        self.builder
            .times
            .push(superconsole::line![Span::new_styled(styled_for_delay(
                time,
                age,
                self.builder.cutoffs
            ))?]);

        Ok(())
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
