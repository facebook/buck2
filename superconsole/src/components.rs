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
pub use echo::Echo;
pub use expanding::Expanding;
use gazebo::prelude::*;
pub use padding::Padded;
pub use splitting::Split;

use crate::content::LinesExt;
use crate::Dimensions;
use crate::Lines;
use crate::State;

pub mod alignment;
mod blank;
pub mod bordering;
mod bounding;
mod canvas;
mod echo;
mod expanding;
pub mod padding;
pub mod splitting;

/// Used to mark whether a draw is final.
#[derive(Debug, Copy, Dupe, Clone, Eq, PartialEq)]
pub enum DrawMode {
    Normal,
    Final,
}

/// Components are pluggable drawers that output lines of formatted text.
/// They are composable (eventually) and re-render in place at each render.
pub trait Component: Debug + Send + ComponentName {
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

pub trait ComponentName {
    fn name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }
}

impl<C: Component + ?Sized> ComponentName for C {}
