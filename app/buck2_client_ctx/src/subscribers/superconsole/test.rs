/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

use buck2_event_observer::test_state::TestState;
use crossterm::style::Color;
use crossterm::style::ContentStyle;
use crossterm::style::ResetColor;
use crossterm::style::SetForegroundColor;
use superconsole::Component;
use superconsole::Dimensions;
use superconsole::DrawMode;
use superconsole::Line;
use superconsole::Lines;
use superconsole::Span;

use crate::subscribers::superconsole::SessionInfo;

struct TestCounterComponent;

struct TestCounterColumn {
    label: &'static str,
    color: Color,
    get_from_test_state: fn(&TestState) -> u64,
}

impl TestCounterColumn {
    const LISTING_FAIL: TestCounterColumn = TestCounterColumn {
        label: "Listing Fail",
        color: Color::Red,
        get_from_test_state: |test_state| test_state.listing_failed,
    };
    const DISCOVERED: TestCounterColumn = TestCounterColumn {
        label: "Discovered",
        color: Color::White,
        get_from_test_state: |test_state| test_state.discovered,
    };
    const PASS: TestCounterColumn = TestCounterColumn {
        label: "Pass",
        color: Color::Green,
        get_from_test_state: |test_state| test_state.pass,
    };
    const FAIL: TestCounterColumn = TestCounterColumn {
        label: "Fail",
        color: Color::Red,
        get_from_test_state: |test_state| test_state.fail,
    };
    const FATAL: TestCounterColumn = TestCounterColumn {
        label: "Fatal",
        color: Color::Red,
        get_from_test_state: |test_state| test_state.fatal,
    };
    const SKIP: TestCounterColumn = TestCounterColumn {
        label: "Skip",
        color: Color::Yellow,
        get_from_test_state: |test_state| test_state.skipped,
    };
    const TIMEOUT: TestCounterColumn = TestCounterColumn {
        label: "Timeout",
        color: Color::Yellow,
        get_from_test_state: |test_state| test_state.timeout,
    };

    fn to_span_from_test_state(&self, test_state: &TestState) -> anyhow::Result<Span> {
        StylizedCount {
            label: self.label,
            count: (self.get_from_test_state)(test_state),
            color: self.color,
        }
        .to_span()
    }
}

impl TestCounterComponent {
    fn draw_unchecked(
        &self,
        test_state: &TestState,
        _dimensions: Dimensions,
        mode: DrawMode,
    ) -> anyhow::Result<Lines> {
        if matches!(mode, DrawMode::Final) {
            return Ok(Lines::new());
        }

        // TODO(brasselsprouts): use the outer try_into conversion on Lines.

        let mut spans = Vec::new();
        if test_state.listing_failed > 0 {
            spans.push(TestCounterColumn::LISTING_FAIL.to_span_from_test_state(test_state)?);
            spans.push(". ".try_into()?);
        }
        spans.push(TestCounterColumn::DISCOVERED.to_span_from_test_state(test_state)?);
        spans.push(". ".try_into()?);
        spans.push(TestCounterColumn::PASS.to_span_from_test_state(test_state)?);
        spans.push(". ".try_into()?);
        spans.push(TestCounterColumn::FAIL.to_span_from_test_state(test_state)?);
        spans.push(". ".try_into()?);
        spans.push(TestCounterColumn::FATAL.to_span_from_test_state(test_state)?);
        spans.push(". ".try_into()?);
        spans.push(TestCounterColumn::SKIP.to_span_from_test_state(test_state)?);
        spans.push(". ".try_into()?);
        spans.push(TestCounterColumn::TIMEOUT.to_span_from_test_state(test_state)?);
        Ok(Lines::from_iter([Line::from_iter(spans)]))
    }
}

/// Draw the test summary line above the `timed_list`
pub(crate) struct TestHeader<'a> {
    pub(crate) session_info: &'a SessionInfo,
    pub(crate) test_state: &'a TestState,
}

impl<'a> Component for TestHeader<'a> {
    fn draw_unchecked(
        &self,

        dimensions: superconsole::Dimensions,
        mode: superconsole::DrawMode,
    ) -> anyhow::Result<superconsole::Lines> {
        if self.session_info.test_session.is_some() {
            TestCounterComponent.draw_unchecked(self.test_state, dimensions, mode)
        } else {
            Ok(Lines::new())
        }
    }
}

/// A count that receives color if and only if it's > 0
pub struct StylizedCount {
    pub label: &'static str,
    pub count: u64,
    pub color: Color,
}

impl StylizedCount {
    /// Turn this StylizedCount into a Superconsole Span.
    pub fn to_span(&self) -> anyhow::Result<superconsole::Span> {
        let mut style = ContentStyle::default();
        if self.count > 0 {
            style.foreground_color = Some(self.color);
        }
        style
            .apply(format!("{} {}", self.label, self.count))
            .try_into()
    }

    /// Turn this StylizedCount into output suitable for stdio.
    pub fn to_stdio(&self) -> StylizedCountForStdio<'_> {
        StylizedCountForStdio { inner: self }
    }
}

pub struct StylizedCountForStdio<'a> {
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
