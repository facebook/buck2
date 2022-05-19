/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use superconsole::style::{Color, ContentStyle, StyledContent};

/// A way to uniformly print to the console after a command has finished. This should
/// only be used at the end of a command, after the event context from the buckd client
/// is not available.
pub struct FinalConsole {
    is_tty: bool,
}

impl FinalConsole {
    pub fn new_with_tty() -> Self {
        Self { is_tty: true }
    }

    pub fn new_without_tty() -> Self {
        Self { is_tty: false }
    }

    fn stderr_colored(&self, message: &str, color: Color) -> anyhow::Result<()> {
        if self.is_tty {
            let sc = StyledContent::new(
                ContentStyle {
                    foreground_color: Some(color),
                    background_color: None,
                    attributes: Default::default(),
                },
                message,
            );
            crate::eprintln!("{}", sc)?;
        } else {
            crate::eprintln!("{}", message)?;
        }
        Ok(())
    }

    /// Print the given message to stderr, in red if possible
    pub fn print_error(&self, message: &str) -> anyhow::Result<()> {
        self.stderr_colored(message, Color::DarkRed)
    }

    /// Print the given message to stderr, in green if possible
    pub fn print_success(&self, message: &str) -> anyhow::Result<()> {
        self.stderr_colored(message, Color::Green)
    }

    /// Print a string directly to stderr with no extra formatting
    #[allow(unused)]
    pub fn print_stderr(&self, message: &str) -> anyhow::Result<()> {
        crate::eprintln!("{}", message)
    }

    /// Print a string directly to stdout with no extra formatting
    #[allow(unused)]
    pub fn print_stdout(&self, message: &str) -> anyhow::Result<()> {
        crate::println!("{}", message)
    }
}
