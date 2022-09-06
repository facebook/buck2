/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Testing utilities for Superconsole.
use std::any::Any;

use anyhow::Context as _;

use crate::components::Canvas;
use crate::output::SuperConsoleOutput;
use crate::superconsole::SuperConsole;
use crate::Component;
use crate::Dimensions;

/// An output for testing that doesn't do real I/O.
pub struct TestOutput {
    /// Callers can modify this to indicate whether the output is blocked.
    pub should_render: bool,
    /// The size to report the terminal
    pub terminal_size: Dimensions,
    /// The frames that were written to this output.
    pub frames: Vec<Vec<u8>>,
}

impl SuperConsoleOutput for TestOutput {
    fn should_render(&mut self) -> bool {
        self.should_render
    }

    fn output(&mut self, buffer: Vec<u8>) -> anyhow::Result<()> {
        self.frames.push(buffer);
        Ok(())
    }

    fn terminal_size(&self) -> anyhow::Result<Dimensions> {
        Ok(self.terminal_size)
    }

    fn finalize(self: Box<Self>) -> anyhow::Result<()> {
        Ok(())
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

pub trait SuperConsoleTestingExt {
    fn test_output(&self) -> anyhow::Result<&TestOutput>;
    fn test_output_mut(&mut self) -> anyhow::Result<&mut TestOutput>;
}

impl SuperConsoleTestingExt for SuperConsole {
    fn test_output(&self) -> anyhow::Result<&TestOutput> {
        self.output
            .as_any()
            .downcast_ref()
            .context("Downcast failed")
    }

    fn test_output_mut(&mut self) -> anyhow::Result<&mut TestOutput> {
        self.output
            .as_any_mut()
            .downcast_mut()
            .context("Downcast failed")
    }
}

pub fn test_console(root: Box<dyn Component>) -> SuperConsole {
    let size = Dimensions {
        width: 80,
        height: 80,
    };
    SuperConsole::new_internal(
        Box::new(Canvas::new(root)),
        Some(size),
        Box::new(TestOutput {
            should_render: true,
            terminal_size: size,
            frames: Vec::new(),
        }),
    )
}

pub fn frame_contains(frame: &[u8], needle: &[u8]) -> bool {
    for w in frame.windows(needle.len()) {
        if w == needle {
            return true;
        }
    }
    false
}
