/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cmp;
use std::env;
use std::io;

use crossterm::cursor::MoveToColumn;
use crossterm::cursor::MoveUp;
use crossterm::terminal::Clear;
use crossterm::terminal::ClearType;
use crossterm::tty::IsTty;
use crossterm::QueueableCommand;

use crate::ansi_support::enable_ansi_support;
use crate::components::Component;
use crate::components::DrawMode;
use crate::content::Line;
use crate::output::BlockingSuperConsoleOutput;
use crate::output::SuperConsoleOutput;
use crate::Dimensions;
use crate::Direction;
use crate::Lines;

const MINIMUM_EMIT: usize = 5;
const MAX_GRAPHEME_BUFFER: usize = 1000000;

/// Handles rendering the console using the user-defined [Component](Component)s and emitted messages.
/// A Canvas area at the bottom of the terminal is re-rendered in place at each tick for the components,
/// while a log area of emitted messages is produced above.
/// Producing output from sources other than SuperConsole while break the TUI.
pub struct SuperConsole {
    /// Number of lines that were used to render the canvas last time.
    canvas_contents: Lines,
    /// Buffer storing the lines we should emit next time we render.
    to_emit: Lines,
    /// A default screen size to use if the size cannot be fetched
    /// from the terminal. This generally is only used for testing
    /// situations.
    fallback_size: Option<Dimensions>,
    /// The terminal handle to write a buffer to the screen.
    /// All IO goes through this handle.
    pub(crate) output: Box<dyn SuperConsoleOutput>,
}

impl SuperConsole {
    /// Build a new SuperConsole with a root component.
    pub fn new() -> Option<Self> {
        Self::compatible().then(|| {
            Self::new_with_output(
                None,
                Box::new(BlockingSuperConsoleOutput::new(Box::new(io::stderr()))),
            )
        })
    }

    /// Force a new SuperConsole to be built with a root component, regardless of
    /// whether the tty is compatible
    pub fn forced_new(fallback_size: Dimensions) -> Self {
        Self::new_with_output(
            Some(fallback_size),
            Box::new(BlockingSuperConsoleOutput::new(Box::new(io::stderr()))),
        )
    }

    pub(crate) fn new_with_output(
        fallback_size: Option<Dimensions>,
        output: Box<dyn SuperConsoleOutput>,
    ) -> Self {
        Self {
            canvas_contents: Lines::new(),
            to_emit: Lines::new(),
            fallback_size,
            output,
        }
    }

    pub fn compatible() -> bool {
        // Superconsole only renders on the stderr, so we can display the superconsole
        // even if someone does `command > out.txt`.
        io::stderr().is_tty() && !Self::is_term_dumb() && enable_ansi_support().is_ok()
    }

    fn is_term_dumb() -> bool {
        // Some terminals (e.g. Emacs' eshell) are very limited in functionality and don't support
        // the control codes required for superconsole to work.
        matches!(env::var("TERM"), Ok(kind) if kind == "dumb")
    }

    /// Render at a given tick.  Draws all components and drains the emitted events buffer.
    /// This will produce any pending emitting events above the Canvas and will re-render the drawing area.
    pub fn render(&mut self, root: &dyn Component) -> anyhow::Result<()> {
        // `render_general` refuses to drain more than a single frame, so repeat until done.
        // or until the rendered frame is too large to print anything.
        let mut anything_emitted = true;
        let mut has_rendered = false;
        while !has_rendered || (anything_emitted && !self.to_emit.is_empty()) {
            if !self.output.should_render() {
                break;
            }

            let last_len = self.to_emit.len();
            self.render_with_mode(root, DrawMode::Normal)?;
            anything_emitted = last_len == self.to_emit.len();
            has_rendered = true;
        }

        Ok(())
    }

    /// Perform a final render with [`DrawMode::Final`].
    /// Each component will have a chance to finalize themselves before the terminal is disposed of.
    pub fn finalize(self, root: &dyn Component) -> anyhow::Result<()> {
        self.finalize_with_mode(root, DrawMode::Final)
    }

    /// Perform a final render, using a specified [`DrawMode`].
    /// Each component will have a chance to finalize themselves before the terminal is disposed of.
    pub fn finalize_with_mode(
        mut self,
        root: &dyn Component,
        mode: DrawMode,
    ) -> anyhow::Result<()> {
        self.render_with_mode(root, mode)?;
        self.output.finalize()
    }

    /// Convenience method:
    /// - Calls queue_emit to add the lines.
    /// - Next, re-renders the `superconsole`.
    ///
    /// Because this re-renders the console, it requires passed state.
    /// Overuse of this method can cause `superconsole` to use significant CPU.
    pub fn emit_now(&mut self, lines: Lines, root: &dyn Component) -> anyhow::Result<()> {
        self.emit(lines);
        self.render(root)
    }

    /// Queues the passed lines to be drawn on the next render.
    /// The lines *will not* appear until the next render is called.
    pub fn emit(&mut self, lines: Lines) {
        self.to_emit.extend(lines);
    }

    fn size(&self) -> anyhow::Result<Dimensions> {
        // We want to get the size, but if that fails or is empty use the fallback_size if available.
        match (self.output.terminal_size(), self.fallback_size) {
            (Ok(size), Some(fallback)) if size.width == 0 || size.height == 0 => Ok(fallback),
            (Ok(size), _) => Ok(size),
            (Err(_), Some(fallback)) => Ok(fallback),
            (Err(e), None) => Err(e),
        }
    }

    /// The first step of drawing.  It moves the buffer up to be overwritten and sets the length to 0.
    /// This is used to clear the scratch area so that any possibly emitted messages can write over it.
    pub(crate) fn clear_canvas_pre(writer: &mut Vec<u8>, mut height: usize) -> anyhow::Result<()> {
        while height > 0 {
            // We can only move up at most u16 at a time, so repeat until we move up enough
            let step = height.try_into().unwrap_or(u16::MAX);
            writer.queue(MoveUp(step))?;
            height -= step as usize;
        }
        writer.queue(MoveToColumn(0))?;
        Ok(())
    }

    /// The last step of drawing. Ensures there is nothing else below on the console.
    /// Important in case the new canvas was smaller than the last.
    pub(crate) fn clear_canvas_post(writer: &mut Vec<u8>) -> anyhow::Result<()> {
        writer.queue(Clear(ClearType::FromCursorDown))?;
        Ok(())
    }

    /// Clears the canvas portion of the superconsole.
    pub fn clear(&mut self) -> anyhow::Result<()> {
        let mut buffer = Vec::new();
        Self::clear_canvas_pre(&mut buffer, self.canvas_contents.len())?;
        self.canvas_contents = Lines::new();
        Self::clear_canvas_post(&mut buffer)?;
        self.output.output(buffer)
    }

    /// Helper method to share render + finalize behavior by specifying mode.
    fn render_with_mode(&mut self, root: &dyn Component, mode: DrawMode) -> anyhow::Result<()> {
        // TODO(cjhopman): We may need to try to keep each write call to be under the pipe buffer
        // size so it can be completed in a single syscall otherwise we might see a partially
        // rendered frame.

        // We remove the last line as we always have a blank final line in our output.
        let size = self.size()?.saturating_sub(1, Direction::Vertical);
        let mut buffer = Vec::new();

        self.render_general(&mut buffer, root, mode, size)?;
        self.output.output(buffer)
    }

    /// Helper method that makes rendering highly configurable.
    fn render_general(
        &mut self,
        buffer: &mut Vec<u8>,
        root: &dyn Component,
        mode: DrawMode,
        size: Dimensions,
    ) -> anyhow::Result<()> {
        /// Heuristic to determine if a buffer is too large to buffer.
        /// Can be tuned, but is currently set to 1000000 graphemes.
        fn is_big(buf: &Lines) -> bool {
            let len: usize = buf.iter().map(Line::len).sum();
            len > MAX_GRAPHEME_BUFFER
        }

        // Pre-draw the frame *and then* start rendering emitted messages.
        let mut canvas = root.draw(size, mode)?;
        // We don't trust the child to not truncate the result.
        canvas.shrink_lines_to_dimensions(size);

        // Render at most a single frame if this not the last render.
        // Does not buffer if there is a ridiculous amount of data.
        let limit = match mode {
            DrawMode::Normal if !is_big(&self.to_emit) => {
                let limit = size.height.saturating_sub(canvas.len());
                // arbitrary value picked so we don't starve `emit` on small terminal sizes.
                Some(cmp::max(limit, MINIMUM_EMIT))
            }
            _ => None,
        };

        // How much of the canvas hasn't changed, so I can avoid overwriting
        // and thus avoid flickering things like URL's in VS Code terminal.
        let reuse_prefix = if self.to_emit.is_empty() {
            self.canvas_contents.lines_equal(&canvas)
        } else {
            0
        };

        Self::clear_canvas_pre(buffer, self.canvas_contents.len() - reuse_prefix)?;
        self.to_emit.render_with_limit(buffer, limit)?;
        canvas.render_from_line(buffer, reuse_prefix)?;
        Self::clear_canvas_post(buffer)?;
        self.canvas_contents = canvas;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use anyhow::Context as _;
    use derive_more::AsRef;

    use super::*;
    use crate::components::echo::Echo;
    use crate::testing::frame_contains;
    use crate::testing::test_console;
    use crate::testing::SuperConsoleTestingExt;

    #[derive(AsRef, Debug)]
    #[allow(dead_code)]
    struct Msg(Lines);

    #[test]
    fn test_small_buffer() -> anyhow::Result<()> {
        let mut console = test_console();
        let msg_count = MINIMUM_EMIT + 5;
        console.emit(Lines(vec![vec!["line 1"].try_into()?; msg_count]));
        let msg = Lines(vec![vec!["line"].try_into()?; msg_count]);
        let root = Echo(msg);
        let mut buffer = Vec::new();

        // even though the canvas is larger than the tty
        console.render_general(
            &mut buffer,
            &root,
            DrawMode::Normal,
            Dimensions::new(100, 2),
        )?;

        // we should still drain a minimum of 5 messages.
        assert_eq!(console.to_emit.len(), msg_count - MINIMUM_EMIT);

        Ok(())
    }

    #[test]
    fn test_huge_buffer() -> anyhow::Result<()> {
        let mut console = test_console();
        console.emit(Lines(vec![
            vec!["line 1"].try_into()?;
            MAX_GRAPHEME_BUFFER * 2
        ]));
        let root = Echo(Lines(vec![vec!["line"].try_into()?]));
        let mut buffer = Vec::new();

        // Even though we have more messages than fit on the screen in the `to_emit` buffer
        console.render_general(
            &mut buffer,
            &root,
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
        let mut console = test_console();

        let root = Echo(Lines(vec![vec!["state"].try_into()?]));

        console.render(&root)?;
        assert_eq!(console.test_output()?.frames.len(), 1);

        console.test_output_mut()?.should_render = false;
        console.render(&root)?;
        assert_eq!(console.test_output()?.frames.len(), 1);

        console.emit(Lines(vec![vec!["line 1"].try_into()?]));
        console.render(&root)?;
        assert_eq!(console.test_output()?.frames.len(), 1);

        Ok(())
    }

    /// Check that lines are deferred when should_render returns false, and emitted once the output
    /// is unblocked.
    #[test]
    fn test_block_lines() -> anyhow::Result<()> {
        let mut console = test_console();

        let root = Echo(Lines(vec![vec!["state"].try_into()?]));

        console.test_output_mut()?.should_render = false;
        console.emit(Lines(vec![vec!["line 1"].try_into()?]));
        console.render(&root)?;
        assert_eq!(console.test_output()?.frames.len(), 0);

        console.test_output_mut()?.should_render = true;
        console.emit(Lines(vec![vec!["line 2"].try_into()?]));
        console.render(&root)?;

        let frame = console
            .test_output_mut()?
            .frames
            .pop()
            .context("No frame was emitted")?;

        assert!(frame_contains(&frame, "state"));
        assert!(frame_contains(&frame, "line 1"));
        assert!(frame_contains(&frame, "line 2"));

        Ok(())
    }

    /// Check that render_with_mode does not respect should_render.
    #[test]
    fn test_block_finalize() -> anyhow::Result<()> {
        let mut console = test_console();

        let root = Echo(Lines(vec![vec!["state"].try_into()?]));

        console.test_output_mut()?.should_render = false;
        console.emit(Lines(vec![vec!["line 1"].try_into()?]));
        console.emit(Lines(vec![vec!["line 2"].try_into()?]));
        console.render_with_mode(&root, DrawMode::Final)?;

        let frame = console
            .test_output_mut()?
            .frames
            .pop()
            .context("No frame was emitted")?;

        assert!(frame_contains(&frame, "state"));
        assert!(frame_contains(&frame, "line 1"));
        assert!(frame_contains(&frame, "line 2"));

        Ok(())
    }

    #[test]
    fn test_reuse_buffer() -> anyhow::Result<()> {
        let mut console = test_console();

        console.render(&Echo(Lines(vec![
            vec!["http://example.com/ link"].try_into()?,
            vec!["number 1, special 1"].try_into()?,
        ])))?;
        console.render(&Echo(Lines(vec![
            vec!["http://example.com/ link"].try_into()?,
            vec!["number 2, special 2"].try_into()?,
        ])))?;
        console.emit(Lines(vec![vec!["special 3"].try_into()?]));
        console.render(&Echo(Lines(vec![
            vec!["http://example.com/ link"].try_into()?,
            vec!["number 3"].try_into()?,
        ])))?;
        console.render(&Echo(Lines(vec![
            vec!["http://example.com/ link"].try_into()?,
            vec!["special 4"].try_into()?,
            vec!["number 4"].try_into()?,
        ])))?;

        let frames = &console.test_output()?.frames;
        assert_eq!(frames.len(), 4);
        // We expect the URL to be omitted on some frames, because it didn't change.
        let expect_url = [0, 2];
        for (i, frame) in frames.iter().enumerate() {
            assert_eq!(
                frame_contains(frame, "http://example.com/"),
                expect_url.contains(&i),
            );
            assert!(frame_contains(frame, format!("number {}", i + 1)));
            assert!(frame_contains(frame, format!("special {}", i + 1)));
        }
        Ok(())
    }
}
