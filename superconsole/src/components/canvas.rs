/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::Cell;

use crossterm::cursor::MoveToColumn;
use crossterm::cursor::MoveUp;
use crossterm::terminal::Clear;
use crossterm::terminal::ClearType;
use crossterm::QueueableCommand;

use crate::components::Dimensions;
use crate::components::DrawMode;
use crate::Component;
use crate::Lines;

/// The root components which manages all other components.
#[derive(Debug, Default)]
pub(crate) struct Canvas {
    // used to overwrite previous canvas buffer
    last_lines: Cell<u16>,
}

impl Canvas {
    /// Canvas only has a single child.
    /// It essentially functions as a passthrough - an invisible window which handles sizing and re-drawing correctly.
    pub(crate) fn new() -> Self {
        Self {
            ..Default::default()
        }
    }

    /// A passthrough method that resizes the Canvas to reflect the size of the root.
    /// Allows dynamic resizing.
    /// Cuts off any lines that are too for long a single row
    pub(crate) fn draw(
        &self,
        root: &dyn Component,
        dimensions: Dimensions,
        mode: DrawMode,
    ) -> anyhow::Result<Lines> {
        let mut output = root.draw(dimensions, mode)?;
        // We don't trust the child to not truncate the result.
        output.shrink_lines_to_dimensions(dimensions);
        self.last_lines.set(output.len().try_into()?);
        Ok(output)
    }

    /// The first half of drawing.  It moves the buffer up to be overwritten and sets the length to 0.
    /// This is used to clear the scratch area so that any possibly emitted messages can write over it.
    pub(crate) fn move_up(&self, writer: &mut Vec<u8>) -> anyhow::Result<()> {
        let len = self.last_lines.take();
        if len != 0 {
            writer.queue(MoveUp(len))?;
        }
        writer.queue(MoveToColumn(0))?;

        Ok(())
    }

    /// Clears the canvas.
    pub(crate) fn clear(&self, writer: &mut Vec<u8>) -> anyhow::Result<()> {
        self.move_up(writer)?;
        writer.queue(Clear(ClearType::FromCursorDown))?;

        Ok(())
    }
}
