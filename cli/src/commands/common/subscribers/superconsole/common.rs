/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use superconsole::{
    components::{alignment::HorizontalAlignmentKind, splitting::SplitKind, Aligned, Split},
    Component, Dimensions, Direction, DrawMode, Lines, Span, State,
};

#[derive(Debug)]
pub(crate) struct HeaderLineComponent(Split);

impl HeaderLineComponent {
    pub(crate) fn new(lhs: Box<dyn Component>, rhs: Box<dyn Component>) -> Self {
        let rhs = box Aligned {
            child: rhs,
            horizontal: HorizontalAlignmentKind::Right,
            ..Default::default()
        };
        Self(Split::new(
            vec![lhs, rhs],
            Direction::Horizontal,
            SplitKind::Adaptive,
        ))
    }
}

impl Component for HeaderLineComponent {
    fn draw_unchecked(
        &self,
        state: &State,
        dimensions: Dimensions,
        mode: DrawMode,
    ) -> anyhow::Result<Vec<superconsole::Line>> {
        self.0.draw(state, dimensions, mode)
    }
}

/// This component is part of the header line and displays a hardcoded message.
#[derive(Debug)]
pub(crate) struct StaticStringComponent {
    pub(crate) header: String,
}

impl Component for StaticStringComponent {
    fn draw_unchecked(
        &self,
        _state: &State,
        _dimensions: Dimensions,
        _mode: DrawMode,
    ) -> anyhow::Result<Lines> {
        let title = Span::new_unstyled(&self.header)?;
        Ok(vec![superconsole::line!(title)])
    }
}
