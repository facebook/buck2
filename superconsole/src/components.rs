/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Getting started truly from scratch is excessive for every user of superconsole.
//! A small set of starter components are provided, exposed below.

use std::fmt::Debug;

pub use alignment::Aligned;
pub use blank::Blank;
pub use bordering::Bordered;
pub use bounding::Bounded;
pub(crate) use canvas::Canvas;
pub use expanding::Expanding;
pub use padding::Padded;
pub use splitting::Split;

pub use crate::components::draw_horizontal::DrawHorizontal;
pub use crate::components::draw_vertical::DrawVertical;
use crate::Dimensions;
use crate::Lines;
use crate::State;

pub mod alignment;
mod blank;
pub mod bordering;
mod bounding;
mod canvas;
mod draw_horizontal;
mod draw_vertical;
pub(crate) mod echo;
mod expanding;
pub mod padding;
pub mod splitting;

/// Used to mark whether a draw is final.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum DrawMode {
    Normal,
    Final,
}

/// Components are pluggable drawers that output lines of formatted text.
/// They are composable (eventually) and re-render in place at each render.
pub trait Component: Debug + Send {
    /// This method is to be implemented for components to provide the `draw` method.
    fn draw_unchecked(
        &self,
        state: &State,
        dimensions: Dimensions,
        mode: DrawMode,
    ) -> anyhow::Result<Lines>;

    /// Interprets the current caller state to create its drawing.
    /// Dimensions refers to the maximum (width, height) this component may use.
    /// The mode refers to if this is the final time the component will be drawn.
    /// If a child component is too large to fit in the dimensions, it is truncated.
    fn draw(&self, state: &State, dimensions: Dimensions, mode: DrawMode) -> anyhow::Result<Lines> {
        let mut res = self.draw_unchecked(state, dimensions, mode)?;
        res.shrink_lines_to_dimensions(dimensions);
        Ok(res)
    }
}

impl Component for Box<dyn Component> {
    fn draw_unchecked(
        &self,
        state: &State,
        dimensions: Dimensions,
        mode: DrawMode,
    ) -> anyhow::Result<Lines> {
        (**self).draw_unchecked(state, dimensions, mode)
    }
}

// TODO(nga): this is not really needed.
impl<C: Component> Component for Box<C> {
    fn draw_unchecked(
        &self,
        state: &State,
        dimensions: Dimensions,
        mode: DrawMode,
    ) -> anyhow::Result<Lines> {
        (**self).draw_unchecked(state, dimensions, mode)
    }
}
