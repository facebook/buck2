/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cmp;
use std::io;

use crossterm::queue;
use crossterm::terminal;
use crossterm::terminal::Clear;
use crossterm::terminal::ClearType;
use crossterm::tty::IsTty;

use crate::components::Canvas;
use crate::components::Component;
use crate::components::DrawMode;
use crate::content::Line;
use crate::content::LinesExt;
use crate::output::BlockingSuperConsoleOutput;
use crate::output::SuperConsoleOutput;
use crate::Dimensions;
use crate::Direction;
use crate::Lines;
use crate::State;

const MINIMUM_EMIT: usize = 5;
const MAX_GRAPHEME_BUFFER: usize = 1000000;

/// Handles rendering the console using the user-defined [Component](Component)s and emitted messages.
/// A Canvas area at the bottom of the terminal is re-rendered in place at each tick for the components,
/// while a log area of emitted messages is produced above.
/// Producing output from sources other than SuperConsole while break the TUI.
pub struct SuperConsole {
    root: Canvas,
    to_emit: Vec<Line>,
    // A default screen size to use if the size cannot be fetched
    // from the terminal. This generally is only used for testing
    // situations.
    default_size: Option<Dimensions>,
    pub output: Box<dyn SuperConsoleOutput>,
}

impl SuperConsole {
    /// Build a new SuperConsole with a root component.
    pub fn new(root: Box<dyn Component>) -> Option<Self> {
        Self::compatible().then(|| {
            Self::new_internal(
                root,
                None,
                Box::new(BlockingSuperConsoleOutput::new(Box::new(io::stderr()))),
            )
        })
    }

    /// Force a new SuperConsole to be built with a root component, regardless of
    /// whether the tty is compatible
    pub fn forced_new(root: Box<dyn Component>, default_size: Dimensions) -> Self {
        Self::new_internal(
            root,
            Some(default_size),
            Box::new(BlockingSuperConsoleOutput::new(Box::new(io::stderr()))),
        )
    }

    pub(crate) fn new_internal(
        root: Box<dyn Component>,
        default_size: Option<Dimensions>,
        output: Box<dyn SuperConsoleOutput>,
    ) -> Self {
        Self {
            root: Canvas::new(root),
            to_emit: Vec::new(),
            default_size,
            output,
        }
    }

    pub fn compatible() -> bool {
        // Superconsole only renders on the stderr, so we can display the superconsole
        // even if someone does `command > out.txt`.
        io::stderr().is_tty()
    }

    /// Render at a given tick.  Draws all components and drains the emitted events buffer.
    /// This will produce any pending emitting events above the Canvas and will re-render the drawing area.
    pub fn render(&mut self, state: &State) -> anyhow::Result<()> {
        // `render_general` refuses to drain more than a single frame, so repeat until done.
        // or until the rendered frame is too large to print anything.
        let mut anything_emitted = true;
        let mut has_rendered = false;
        while !has_rendered || (anything_emitted && !self.to_emit.is_empty()) {
            if !self.output.should_render() {
                break;
            }

            let last_len = self.to_emit.len();
            self.render_with_mode(state, DrawMode::Normal)?;
            anything_emitted = last_len == self.to_emit.len();
            has_rendered = true;
        }

        Ok(())
    }

    /// Perform a final render.
    /// This time, each component will have a chance to finalize themselves before the terminal is disposed of.
    pub fn finalize(mut self, state: &State) -> anyhow::Result<()> {
        self.render_with_mode(state, DrawMode::Final)?;
        self.output.finalize()?;
        Ok(())
    }

    /// Convenience method:
    /// - Calls queue_emit to add the lines.
    /// - Next, re-renders the `superconsole`.
    ///
    /// Because this re-renders the console, it requires passed state.
    /// Overuse of this method can cause `superconsole` to use significant CPU.
    pub fn emit_now(&mut self, lines: Lines, state: &State) -> anyhow::Result<()> {
        self.emit(lines);
        self.render(state)
    }

    /// Queues the passed lines to be drawn on the next render.
    /// The lines *will not* appear until the next render is called.
    pub fn emit(&mut self, mut lines: Lines) {
        self.to_emit.append(&mut lines);
    }

    fn size(&self) -> anyhow::Result<Dimensions> {
        match terminal::size() {
            Ok(size) => Ok(size.into()),
            Err(e) => match self.default_size {
                Some(default) => Ok(default),
                None => Err(e.into()),
            },
        }
    }

    /// Clears the canvas portion of the superconsole.
    pub fn clear(&mut self) -> anyhow::Result<()> {
        let mut buffer = vec![];
        self.root.clear(&mut buffer)?;
        self.output.output(buffer)
    }

    /// Helper method to share render + finalize behavior by specifying mode.
    fn render_with_mode(&mut self, state: &State, mode: DrawMode) -> anyhow::Result<()> {
        // TODO(cjhopman): We may need to try to keep each write call to be under the pipe buffer
        // size so it can be completed in a single syscall otherwise we might see a partially
        // rendered frame.

        // We remove the last line as we always have a blank final line in our output.
        let size = self.size()?.saturating_sub(1, Direction::Vertical);
        let mut buffer = Vec::new();

        self.render_general(&mut buffer, state, mode, size)?;
        self.output.output(buffer)
    }

    /// Helper method that makes rendering highly configurable.
    fn render_general(
        &mut self,
        buffer: &mut Vec<u8>,
        state: &State,
        mode: DrawMode,
        size: Dimensions,
    ) -> anyhow::Result<()> {
        /// Heuristic to determine if a buffer is too large to buffer.
        /// Can be tuned, but is currently set to 1000000 graphemes.
        #[allow(clippy::ptr_arg)]
        fn is_big(buf: &Lines) -> bool {
            let len: usize = buf.iter().map(Line::len).sum();
            len > MAX_GRAPHEME_BUFFER
        }

        // Go the beginning of the canvas.
        self.root.move_up(buffer)?;

        // Pre-draw the frame *and then* start rendering emitted messages.
        let mut frame = self.root.draw(state, size, mode)?;
        // Render at most a single frame if this not the last render.
        // Does not buffer if there is a ridiculous amount of data.
        let limit = match mode {
            DrawMode::Normal if !is_big(&self.to_emit) => {
                let limit = (size.y as usize).saturating_sub(frame.len());
                // arbitrary value picked so we don't starve `emit` on small terminal sizes.
                Some(cmp::max(limit, MINIMUM_EMIT))
            }
            _ => None,
        };
        self.to_emit.render(buffer, limit)?;
        frame.render(buffer, None)?;

        // clear any residue from the previous render.
        queue!(buffer, Clear(ClearType::FromCursorDown))?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use anyhow::Context as _;
    use derive_more::AsRef;

    use super::*;
    use crate::components::Echo;
    use crate::testing::frame_contains;
    use crate::testing::test_console;
    use crate::testing::SuperConsoleTestingExt;
    use crate::Lines;

    #[derive(AsRef, Debug)]
    struct Msg(Lines);

    #[test]
    fn test_small_buffer() -> anyhow::Result<()> {
        let root = Box::new(Echo::<Msg>::new(false));
        let mut console = test_console(root);
        let msg_count = MINIMUM_EMIT + 5;
        console.emit(vec![vec!["line 1"].try_into()?; msg_count]);
        let msg = Msg(vec![vec!["line"].try_into()?; msg_count]);
        let state = crate::state![&msg];
        let mut buffer = Vec::new();

        // even though the canvas is larger than the tty
        console.render_general(
            &mut buffer,
            &state,
            DrawMode::Normal,
            Dimensions::new(100, 2),
        )?;

        // we should still drain a minimum of 5 messages.
        assert_eq!(console.to_emit.len(), msg_count - MINIMUM_EMIT);

        Ok(())
    }

    #[test]
    fn test_huge_buffer() -> anyhow::Result<()> {
        let root = Box::new(Echo::<Msg>::new(false));
        let mut console = test_console(root);
        console.emit(vec![vec!["line 1"].try_into()?; MAX_GRAPHEME_BUFFER * 2]);
        let msg = Msg(vec![vec!["line"].try_into()?; 1]);
        let state = crate::state![&msg];
        let mut buffer = Vec::new();

        // Even though we have more messages than fit on the screen in the `to_emit` buffer
        console.render_general(
            &mut buffer,
            &state,
            DrawMode::Normal,
            Dimensions::new(100, 20),
        )?;

        // We have so many that we should just drain them all.
        assert!(console.to_emit.is_empty());

        Ok(())
    }

    /// Check that no frames are produced when should_render returns false.
    #[test]
    fn test_block_render() -> anyhow::Result<()> {
        let root = Box::new(Echo::<Msg>::new(false));
        let mut console = test_console(root);

        let msg = Msg(vec![vec!["state"].try_into()?; 1]);
        let state = crate::state![&msg];

        console.render(&state)?;
        assert_eq!(console.test_output()?.frames.len(), 1);

        console.test_output_mut()?.should_render = false;
        console.render(&state)?;
        assert_eq!(console.test_output()?.frames.len(), 1);

        console.emit(vec![vec!["line 1"].try_into()?]);
        console.render(&state)?;
        assert_eq!(console.test_output()?.frames.len(), 1);

        Ok(())
    }

    /// Check that lines are deferred when should_render returns false, and emitted once the output
    /// is unblocked.
    #[test]
    fn test_block_lines() -> anyhow::Result<()> {
        let root = Box::new(Echo::<Msg>::new(false));
        let mut console = test_console(root);

        let msg = Msg(vec![vec!["state"].try_into()?; 1]);
        let state = crate::state![&msg];

        console.test_output_mut()?.should_render = false;
        console.emit(vec![vec!["line 1"].try_into()?]);
        console.render(&state)?;
        assert_eq!(console.test_output()?.frames.len(), 0);

        console.test_output_mut()?.should_render = true;
        console.emit(vec![vec!["line 2"].try_into()?]);
        console.render(&state)?;

        let frame = console
            .test_output_mut()?
            .frames
            .pop()
            .context("No frame was emitted")?;

        assert!(frame_contains(&frame, "state".as_bytes()));
        assert!(frame_contains(&frame, "line 1".as_bytes()));
        assert!(frame_contains(&frame, "line 2".as_bytes()));

        Ok(())
    }

    /// Check that render_with_mode does not respect should_render.
    #[test]
    fn test_block_finalize() -> anyhow::Result<()> {
        let root = Box::new(Echo::<Msg>::new(false));
        let mut console = test_console(root);

        let msg = Msg(vec![vec!["state"].try_into()?; 1]);
        let state = crate::state![&msg];

        console.test_output_mut()?.should_render = false;
        console.emit(vec![vec!["line 1"].try_into()?]);
        console.emit(vec![vec!["line 2"].try_into()?]);
        console.render_with_mode(&state, DrawMode::Final)?;

        let frame = console
            .test_output_mut()?
            .frames
            .pop()
            .context("No frame was emitted")?;

        assert!(frame_contains(&frame, "state".as_bytes()));
        assert!(frame_contains(&frame, "line 1".as_bytes()));
        assert!(frame_contains(&frame, "line 2".as_bytes()));

        Ok(())
    }
}
