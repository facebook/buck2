/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;

use superconsole::components::alignment::HorizontalAlignmentKind;
use superconsole::components::alignment::VerticalAlignmentKind;
use superconsole::components::Aligned;
use superconsole::components::DrawHorizontal;
use superconsole::Component;
use superconsole::Dimensions;
use superconsole::DrawMode;
use superconsole::Line;
use superconsole::Lines;
use superconsole::State;

#[derive(Debug)]
pub(crate) struct HeaderLineComponent<A: Component, B: Component> {
    lhs: A,
    rhs: B,
}

impl<A: Component, B: Component> HeaderLineComponent<A, B> {
    pub(crate) fn new(lhs: A, rhs: B) -> Self {
        HeaderLineComponent { lhs, rhs }
    }
}

impl<A: Component, B: Component> Component for HeaderLineComponent<A, B> {
    fn draw_unchecked(
        &self,
        state: &State,
        dimensions: Dimensions,
        mode: DrawMode,
    ) -> anyhow::Result<Lines> {
        let mut draw = DrawHorizontal::new(dimensions);
        draw.draw(&self.lhs, state, mode)?;
        draw.draw(
            &Aligned {
                child: &self.rhs,
                horizontal: HorizontalAlignmentKind::Right,
                vertical: VerticalAlignmentKind::Top,
            },
            state,
            mode,
        )?;
        Ok(draw.finish())
    }
}

/// This component is part of the header line and displays a hardcoded message.
#[derive(Debug)]
pub(crate) struct StaticStringComponent<S: AsRef<str>> {
    pub header: S,
}

impl<S: AsRef<str>> Component for StaticStringComponent<S> {
    fn draw_unchecked(
        &self,
        _state: &State,
        _dimensions: Dimensions,
        _mode: DrawMode,
    ) -> anyhow::Result<Lines> {
        Ok(Lines(vec![Line::unstyled(self.header.as_ref())?]))
    }
}
