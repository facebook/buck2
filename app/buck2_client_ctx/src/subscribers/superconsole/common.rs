/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use superconsole::components::alignment::HorizontalAlignmentKind;
use superconsole::components::Aligned;
use superconsole::components::DrawHorizontal;
use superconsole::Component;
use superconsole::Dimensions;
use superconsole::DrawMode;
use superconsole::Line;
use superconsole::Lines;
use superconsole::State;

#[derive(Debug)]
pub(crate) struct HeaderLineComponent {
    lhs: Box<dyn Component>,
    rhs: Box<dyn Component>,
}

impl HeaderLineComponent {
    pub(crate) fn new(lhs: Box<dyn Component>, rhs: Box<dyn Component>) -> Self {
        let rhs = Box::new(Aligned {
            child: rhs,
            horizontal: HorizontalAlignmentKind::Right,
            ..Default::default()
        });
        HeaderLineComponent { lhs, rhs }
    }
}

impl Component for HeaderLineComponent {
    fn draw_unchecked(
        &self,
        state: &State,
        dimensions: Dimensions,
        mode: DrawMode,
    ) -> anyhow::Result<Lines> {
        let mut draw = DrawHorizontal::new(dimensions);
        draw.draw(&*self.lhs, state, mode)?;
        draw.draw(&*self.rhs, state, mode)?;
        Ok(draw.finish())
    }
}

/// This component is part of the header line and displays a hardcoded message.
#[derive(Debug)]
pub(crate) struct StaticStringComponent {
    pub header: String,
}

impl Component for StaticStringComponent {
    fn draw_unchecked(
        &self,
        _state: &State,
        _dimensions: Dimensions,
        _mode: DrawMode,
    ) -> anyhow::Result<Lines> {
        Ok(Lines(vec![Line::unstyled(&self.header)?]))
    }
}
