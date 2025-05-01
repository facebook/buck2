/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io;
use std::io::Write;

use crate::Dimensions;
use crate::SuperConsole;
use crate::output::BlockingSuperConsoleOutput;
use crate::output::NonBlockingSuperConsoleOutput;
use crate::output::SuperConsoleOutput;

/// A builder to create SuperConsole, with more options.
pub struct Builder {
    non_blocking: bool,
    stream: Box<dyn Write + Send + 'static + Sync>,
}

impl Default for Builder {
    fn default() -> Self {
        Builder::new()
    }
}

impl Builder {
    pub fn new() -> Self {
        Self {
            non_blocking: false,
            stream: Box::new(io::stderr()),
        }
    }

    /// Enable non-blocking I/O.
    pub fn non_blocking(&mut self) -> &mut Self {
        self.non_blocking = true;
        self
    }

    /// Write to a different I/O
    pub fn write_to(&mut self, stream: Box<dyn Write + Send + 'static + Sync>) -> &mut Self {
        self.stream = stream;
        self
    }

    /// Build a new SuperConsole if stderr is a TTY.
    pub fn build(self) -> anyhow::Result<Option<SuperConsole>> {
        if !SuperConsole::compatible() {
            return Ok(None);
        }
        Some(self.build_inner(None)).transpose()
    }

    /// Build a new SuperConsole regardless of whether stderr is a TTY.
    pub fn build_forced(self, fallback_size: Dimensions) -> anyhow::Result<SuperConsole> {
        self.build_inner(Some(fallback_size))
    }

    fn build_inner(self, fallback_size: Option<Dimensions>) -> anyhow::Result<SuperConsole> {
        Ok(SuperConsole::new_with_output(fallback_size, self.output()?))
    }

    fn output(self) -> anyhow::Result<Box<dyn SuperConsoleOutput>> {
        if self.non_blocking {
            Ok(Box::new(NonBlockingSuperConsoleOutput::new(self.stream)?))
        } else {
            Ok(Box::new(BlockingSuperConsoleOutput::new(self.stream)))
        }
    }
}
