/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

use crossterm::style::Color;
use crossterm::style::ContentStyle;
use crossterm::style::ResetColor;
use crossterm::style::SetForegroundColor;
use superconsole::Component;
use superconsole::Dimensions;
use superconsole::DrawMode;
use superconsole::Line;
use superconsole::Lines;
use superconsole::State;
use test_api::data::TestStatus;

use crate::commands::common::subscribers::superconsole::SessionInfo;

#[derive(Default)]
pub(crate) struct TestState {
    pub discovered: u64,
    pub pass: u64,
    pub fail: u64,
    pub fatal: u64,
    pub timeout: u64,
    pub skipped: u64,
    pub omitted: u64,
    pub retry: u64,
    pub unknown: u64,
    pub listing_success: u64,
    pub listing_failed: u64,
}

impl TestState {
    pub(crate) fn update(&mut self, result: &buck2_data::TestResult) -> anyhow::Result<()> {
        let status = TestStatus::try_from(result.status)?;
        let counter = match status {
            TestStatus::PASS => &mut self.pass,
            TestStatus::FAIL => &mut self.fail,
            TestStatus::FATAL => &mut self.fatal,
            TestStatus::SKIP => &mut self.skipped,
            TestStatus::OMITTED => &mut self.omitted,
            TestStatus::TIMEOUT => &mut self.timeout,
            TestStatus::UNKNOWN => &mut self.unknown,
            TestStatus::RERUN => &mut self.retry,
            TestStatus::LISTING_SUCCESS => &mut self.listing_success,
            TestStatus::LISTING_FAILED => &mut self.listing_failed,
        };
        *counter += 1;

        Ok(())
    }

    pub(crate) fn not_executed(&self) -> u64 {
        self.skipped + self.omitted
    }
}

#[derive(Debug)]
struct TestCounterComponent;

impl Component for TestCounterComponent {
    fn draw_unchecked(
        &self,
        state: &State,
        _dimensions: Dimensions,
        mode: DrawMode,
    ) -> anyhow::Result<Lines> {
        if matches!(mode, DrawMode::Final) {
            return Ok(vec![]);
        }

        let test_state = state.get::<TestState>()?;

        // TODO(brasselsprouts): use the outer try_into conversion on Lines.

        let mut spans = Vec::new();
        if test_state.listing_failed > 0 {
            spans.push(
                StylizedCount {
                    label: "Listing Fail",
                    count: test_state.listing_failed,
                    color: Color::Red,
                }
                .to_span()?,
            );
            spans.push(". ".try_into()?);
        }
        spans.push(
            StylizedCount {
                label: "Discovered",
                count: test_state.discovered,
                color: Color::White,
            }
            .to_span()?,
        );
        spans.push(". ".try_into()?);
        spans.push(
            StylizedCount {
                label: "Pass",
                count: test_state.pass,
                color: Color::Green,
            }
            .to_span()?,
        );
        spans.push(". ".try_into()?);
        spans.push(
            StylizedCount {
                label: "Fail",
                count: test_state.fail,
                color: Color::Red,
            }
            .to_span()?,
        );
        spans.push(". ".try_into()?);
        spans.push(
            StylizedCount {
                label: "Fatal",
                count: test_state.fatal,
                color: Color::DarkRed,
            }
            .to_span()?,
        );
        spans.push(". ".try_into()?);
        spans.push(
            StylizedCount {
                label: "Skip",
                count: test_state.not_executed(),
                color: Color::Cyan,
            }
            .to_span()?,
        );
        spans.push(". ".try_into()?);
        spans.push(
            StylizedCount {
                label: "Timeout",
                count: test_state.timeout,
                color: Color::Yellow,
            }
            .to_span()?,
        );
        Ok(vec![Line(spans)])
    }
}

/// Draw the test summary line above the `timed_list`
#[derive(Debug)]
pub(crate) struct TestHeader(Box<dyn Component>);

impl TestHeader {
    pub(crate) fn new() -> Self {
        Self(box TestCounterComponent)
    }
}

impl Component for TestHeader {
    fn draw_unchecked(
        &self,
        state: &superconsole::State,
        dimensions: superconsole::Dimensions,
        mode: superconsole::DrawMode,
    ) -> anyhow::Result<superconsole::Lines> {
        let session_info = state.get::<SessionInfo>()?;
        if session_info.test_session.is_some() {
            self.0.draw_unchecked(state, dimensions, mode)
        } else {
            Ok(vec![])
        }
    }
}

/// A count that receives color if and only if it's > 0
pub(crate) struct StylizedCount {
    pub label: &'static str,
    pub count: u64,
    pub color: Color,
}

impl StylizedCount {
    /// Turn this StylizedCount into a Superconsole Span.
    pub(crate) fn to_span(&self) -> anyhow::Result<superconsole::Span> {
        let mut style = ContentStyle::default();
        if self.count > 0 {
            style.foreground_color = Some(self.color);
        }
        style
            .apply(format!("{} {}", self.label, self.count))
            .try_into()
    }

    /// Turn this StylizedCount into output suitable for stdio.
    pub(crate) fn to_stdio(&self) -> StylizedCountForStdio<'_> {
        StylizedCountForStdio { inner: self }
    }
}

pub(crate) struct StylizedCountForStdio<'a> {
    inner: &'a StylizedCount,
}

impl<'a> fmt::Display for StylizedCountForStdio<'a> {
    fn fmt(&self, w: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.inner.count > 0 {
            write!(w, "{}", SetForegroundColor(self.inner.color))?;
        }

        write!(w, "{} {}", self.inner.label, self.inner.count)?;
        write!(w, "{}", ResetColor)?;

        Ok(())
    }
}
