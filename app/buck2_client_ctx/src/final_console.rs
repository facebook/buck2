/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use superconsole::style::Color;
use superconsole::style::ContentStyle;
use superconsole::style::StyledContent;

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

    fn stderr_colored_ln(&self, message: &str, color: Color) -> buck2_error::Result<()> {
        self.stderr_colored(message, color)?;
        crate::eprintln!()?;
        Ok(())
    }

    fn stderr_colored(&self, message: &str, color: Color) -> buck2_error::Result<()> {
        if self.is_tty {
            let sc = StyledContent::new(
                ContentStyle {
                    foreground_color: Some(color),
                    background_color: None,
                    underline_color: None,
                    attributes: Default::default(),
                },
                message,
            );
            crate::eprint!("{}", sc)?;
        } else {
            crate::eprint!("{}", message)?;
        }
        Ok(())
    }

    /// Print the given message to stderr, in red if possible
    pub fn print_error(&self, message: &str) -> buck2_error::Result<()> {
        self.stderr_colored_ln(message, Color::DarkRed)
    }

    /// Print the given message to stderr, in yellow if possible
    pub fn print_warning(&self, message: &str) -> buck2_error::Result<()> {
        self.stderr_colored_ln(message, Color::Yellow)
    }

    /// Print the given message to stderr, in green if possible
    pub fn print_success(&self, message: &str) -> buck2_error::Result<()> {
        self.stderr_colored_ln(message, Color::Green)
    }

    /// Print the given message to stderr, in green if possible
    pub fn print_success_no_newline(&self, message: &str) -> buck2_error::Result<()> {
        self.stderr_colored(message, Color::Green)
    }

    /// Print a string directly to stderr with no extra formatting
    pub fn print_stderr(&self, message: &str) -> buck2_error::Result<()> {
        crate::eprintln!("{}", message)
    }
}
