/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use dupe::Dupe;
use termwiz::istty::IsTty;

use crate::final_console::FinalConsole;
use crate::subscribers::superconsole::SuperConsoleConfig;

#[derive(
    Debug,
    serde::Serialize,
    serde::Deserialize,
    Clone,
    Dupe,
    Copy,
    clap::ArgEnum
)]
#[clap(rename_all = "lower")]
pub enum ConsoleType {
    Simple,
    SimpleNoTty,
    SimpleTty,
    Super,
    Auto,
    None,
}

#[derive(
    Debug,
    serde::Serialize,
    serde::Deserialize,
    Clone,
    Dupe,
    Copy,
    clap::ArgEnum
)]
#[clap(rename_all = "lower")]
pub enum UiOptions {
    Dice,
    DebugEvents,
    /// I/O panel.
    Io,
    /// RE panel.
    Re,
}

/// Defines common console options for commands.
#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
pub struct CommonConsoleOptions {
    #[clap(
        long = "console",
        help = "Which console to use for this command",
        default_value = "auto",
        ignore_case = true,
        env = "BUCK_CONSOLE",
        value_name = "super|simple|...",
        arg_enum
    )]
    pub console_type: ConsoleType,

    /// Configure additional superconsole ui components.
    ///
    /// Accepts a comma-separated list of superconsole components to add. Possible values are:
    ///
    ///   dice - shows information about evaluated dice nodes
    ///   debugevents - shows information about the flow of events from buckd
    ///
    /// These components can be turned on/off interactively.
    /// Press 'h' for help when superconsole is active.
    #[clap(
        long = "ui",
        ignore_case = true,
        multiple = true,
        number_of_values = 1,
        arg_enum
    )]
    pub ui: Vec<UiOptions>,

    #[clap(
        long,
        help = "Disable console interactions",
        env = "BUCK_NO_INTERACTIVE_CONSOLE"
    )]
    pub no_interactive_console: bool,
}

impl Default for CommonConsoleOptions {
    fn default() -> Self {
        Self {
            console_type: ConsoleType::Auto,
            ui: Vec::new(),
            no_interactive_console: false,
        }
    }
}

impl CommonConsoleOptions {
    pub fn default_ref() -> &'static Self {
        static OPTS: CommonConsoleOptions = CommonConsoleOptions {
            console_type: ConsoleType::Auto,
            ui: vec![],
            no_interactive_console: false,
        };
        &OPTS
    }

    pub fn simple_ref() -> &'static Self {
        static OPTS: CommonConsoleOptions = CommonConsoleOptions {
            console_type: ConsoleType::Simple,
            ui: vec![],
            no_interactive_console: false,
        };
        &OPTS
    }

    pub fn none_ref() -> &'static Self {
        static OPTS: CommonConsoleOptions = CommonConsoleOptions {
            console_type: ConsoleType::None,
            ui: vec![],
            no_interactive_console: false,
        };
        &OPTS
    }

    pub fn final_console(&self) -> FinalConsole {
        let is_tty = match self.console_type {
            ConsoleType::Auto | ConsoleType::Simple => std::io::stderr().is_tty(),
            ConsoleType::Super => true,
            ConsoleType::SimpleNoTty => false,
            ConsoleType::SimpleTty => true,
            ConsoleType::None => false,
        };
        if is_tty {
            FinalConsole::new_with_tty()
        } else {
            FinalConsole::new_without_tty()
        }
    }

    pub fn superconsole_config(&self) -> SuperConsoleConfig {
        let mut config = SuperConsoleConfig::default();
        for option in &self.ui {
            match option {
                UiOptions::Dice => config.enable_dice = true,
                UiOptions::DebugEvents => config.enable_debug_events = true,
                UiOptions::Io => config.enable_io = true,
                UiOptions::Re => config.enable_detailed_re = true,
            }
        }
        config
    }
}
