/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use gazebo::prelude::*;

/// The logging verbosity to use in our various consoles.
///
/// Each level is a superset of the previous ones.
/// Accessor methods should be used to determine
/// specific requested functionality.
#[derive(Debug, Copy, Clone, Dupe, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Verbosity {
    /// Print as few messages as possible.
    Quiet,
    /// The default verbosity. Error messages are printed, but extra details may not be.
    Default,
    /// If an error has additional details to print, they are printed.
    ///
    /// This may be something like a stack trace, or fuller information about a command's
    /// execution.
    VerboseError,
    /// Includes printing the command line and stderr of actions.
    VerboseEverything,
}

impl Verbosity {
    pub(crate) fn try_from_cli(value: &str) -> anyhow::Result<Self> {
        Ok(match value.parse::<i64>()? {
            i if i <= 0 => Self::Quiet,
            1 => Self::Default,
            2 => Self::VerboseError,
            _ => Self::VerboseEverything,
        })
    }

    /// If a verbosity setting is triggered at a particular level, does this verbosity trigger it
    fn at(self, required: Verbosity) -> bool {
        self >= required
    }

    /// Whether stderr should be printed to users for successful commands by default.
    pub(crate) fn print_success_stderr(self) -> bool {
        self.at(Self::VerboseEverything)
    }

    /// Whether the full command for failed actions should be printed. Otherwise, a truncated command is printed.
    pub(crate) fn print_failure_full_command(self) -> bool {
        self.at(Self::VerboseError)
    }

    /// Whether to print all commands that are being executed
    pub(crate) fn print_all_commands(self) -> bool {
        self.at(Self::VerboseError)
    }

    /// Whether we should print periodic status messages
    pub(crate) fn print_status(self) -> bool {
        self.at(Self::Default)
    }
}
