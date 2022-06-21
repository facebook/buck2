/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::components::Component;
use crate::output::BlockingSuperConsoleOutput;
use crate::output::NonBlockingSuperConsoleOutput;
use crate::output::SuperConsoleOutput;
use crate::Dimensions;
use crate::SuperConsole;

/// A builder to create SuperConsole, with more options.
pub struct Builder {
    non_blocking: bool,
}

impl Builder {
    pub fn new() -> Self {
        Self {
            non_blocking: false,
        }
    }

    /// Enable non-blocking I/O.
    pub fn non_blocking(&mut self) -> &mut Self {
        self.non_blocking = true;
        self
    }

    /// Build a new SuperConsole if stderr is a TTY.
    pub fn build(&self, root: Box<dyn Component>) -> anyhow::Result<Option<SuperConsole>> {
        if !SuperConsole::compatible() {
            return Ok(None);
        }
        Some(self.build_inner(root, None)).transpose()
    }

    /// Build a new SuperConsole regardless of whether stderr is a TTY.
    pub fn build_forced(
        &self,
        root: Box<dyn Component>,
        default_size: Dimensions,
    ) -> anyhow::Result<SuperConsole> {
        self.build_inner(root, Some(default_size))
    }

    fn build_inner(
        &self,
        root: Box<dyn Component>,
        default_size: Option<Dimensions>,
    ) -> anyhow::Result<SuperConsole> {
        Ok(SuperConsole::new_internal(
            root,
            default_size,
            self.output()?,
        ))
    }

    fn output(&self) -> anyhow::Result<Box<dyn SuperConsoleOutput>> {
        if self.non_blocking {
            Ok(Box::new(NonBlockingSuperConsoleOutput::new()?))
        } else {
            Ok(Box::new(BlockingSuperConsoleOutput))
        }
    }
}
