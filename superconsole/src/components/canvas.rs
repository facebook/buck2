/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! The root component which manages all other components.
//! This is a fake component since it 1) manages state and 2) cannot be made by users and 3) is heavily coupled with [`superconsole`](crate::SuperConsole).
//! Do not make this somewhere down the component hierarchy unless you have a good reason for it.

use std::{cell::Cell, convert::TryInto};

use crossterm::{
    cursor::{MoveToColumn, MoveUp},
    queue,
    terminal::{Clear, ClearType},
};

use crate::{
    components::{Blank, Dimensions, DrawMode},
    Component, Line, State,
};

/// The root components which manages all other components.
#[derive(Debug)]
pub(crate) struct Canvas {
    child: Box<dyn Component>,
    // used to overwrite previous canvas buffer
    len: Cell<u16>,
}

impl Default for Canvas {
    fn default() -> Self {
        Self {
            child: Box::new(Blank),
            len: Cell::default(),
        }
    }
}

impl Component for Canvas {
    /// A passthrough method that resizes the Canvas to reflect the size of the root.
    /// Allows dynamic resizing.
    /// Cuts off any lines that are too for long a single row
    fn draw_unchecked(
        &self,
        state: &State,
        dimensions: Dimensions,
        mode: DrawMode,
    ) -> anyhow::Result<Vec<Line>> {
        let output = self.child.draw(state, dimensions, mode)?;
        self.set_last_canvas_size(output.len())?;
        Ok(output)
    }
}

impl Canvas {
    /// Canvas only has a single child.
    /// It essentially functions as a passthrough - an invisible window which handles sizing and re-drawing correctly.
    pub(crate) fn new(child: Box<dyn Component>) -> Self {
        Self {
            child,
            ..Default::default()
        }
    }

    /// The first half of drawing.  It moves the buffer up to be overwritten and sets the length to 0.
    /// This is used to clear the scratch area so that any possibly emitted messages can write over it.
    pub(crate) fn move_up(&self, writer: &mut Vec<u8>) -> anyhow::Result<()> {
        let len = self.len.take();
        queue!(writer, MoveUp(len), MoveToColumn(0),)?;

        Ok(())
    }

    /// Clears the canvas.
    pub fn clear(&self, writer: &mut Vec<u8>) -> anyhow::Result<()> {
        self.move_up(writer)?;
        queue!(writer, Clear(ClearType::FromCursorDown),)?;

        Ok(())
    }

    /// helper method that downcasts a usize canvas size to u16
    fn set_last_canvas_size(&self, new_size: usize) -> anyhow::Result<()> {
        self.len.set(new_size.try_into()?);

        Ok(())
    }
}
