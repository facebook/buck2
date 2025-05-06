/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::buck2_env;
use buck2_core::buck2_env_name;
use buck2_core::soft_error;
use buck2_error::conversion::from_any_with_tag;
use buck2_event_observer::event_observer::NoopEventObserverExtra;
use buck2_event_observer::verbosity::Verbosity;
use buck2_health_check::report::DisplayReport;
use buck2_wrapper_common::invocation_id::TraceId;
use clap::builder::FalseyValueParser;
use dupe::Dupe;
use termwiz::istty::IsTty;
use tokio::sync::mpsc::Receiver;

use crate::final_console::FinalConsole;
use crate::subscribers::errorconsole::ErrorConsole;
use crate::subscribers::simpleconsole::SimpleConsole;
use crate::subscribers::subscriber::EventSubscriber;
use crate::subscribers::superconsole::BUCK_NO_INTERACTIVE_CONSOLE;
use crate::subscribers::superconsole::StatefulSuperConsole;
use crate::subscribers::superconsole::SuperConsoleConfig;

#[derive(
    Debug,
    serde::Serialize,
    serde::Deserialize,
    Clone,
    Dupe,
    Copy,
    clap::ValueEnum
)]
#[clap(rename_all = "lower")]
pub enum ConsoleType {
    Auto,
    None,
    Simple,
    SimpleNoTty,
    SimpleTty,
    Super,
}

/// Given a command name and the command arguments, create a default console / superconsole.
pub fn get_console_with_root(
    trace_id: TraceId,
    console_type: ConsoleType,
    verbosity: Verbosity,
    expect_spans: bool,
    replay_speed: Option<f64>,
    command_name: &str,
    config: SuperConsoleConfig,
    health_check_display_reports_receiver: Option<Receiver<Vec<DisplayReport>>>,
) -> Box<dyn EventSubscriber> {
    let result: buck2_error::Result<Box<dyn EventSubscriber>> = match console_type {
        ConsoleType::Simple => Ok(Box::new(
            SimpleConsole::<NoopEventObserverExtra>::autodetect(
                trace_id.dupe(),
                verbosity,
                expect_spans,
                health_check_display_reports_receiver,
            ),
        )),
        ConsoleType::SimpleNoTty => Ok(Box::new(
            SimpleConsole::<NoopEventObserverExtra>::without_tty(
                trace_id.dupe(),
                verbosity,
                expect_spans,
                health_check_display_reports_receiver,
            ),
        )),
        ConsoleType::SimpleTty => Ok(Box::new(SimpleConsole::<NoopEventObserverExtra>::with_tty(
            trace_id.dupe(),
            verbosity,
            expect_spans,
            health_check_display_reports_receiver,
        ))),
        ConsoleType::Super => StatefulSuperConsole::new_with_root_forced(
            trace_id.dupe(),
            command_name,
            verbosity,
            expect_spans,
            replay_speed,
            None,
            config,
            health_check_display_reports_receiver,
        )
        .map(|c| Box::new(c) as Box<dyn EventSubscriber>),
        ConsoleType::Auto => {
            match StatefulSuperConsole::console_builder()
                .build()
                .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))
            {
                Ok(Some(sc)) => StatefulSuperConsole::new(
                    command_name,
                    trace_id.dupe(),
                    sc,
                    verbosity,
                    expect_spans,
                    replay_speed,
                    config,
                    health_check_display_reports_receiver,
                )
                .map(|c| Box::new(c) as Box<dyn EventSubscriber>),
                _ => Ok(Box::new(
                    SimpleConsole::<NoopEventObserverExtra>::autodetect(
                        trace_id.dupe(),
                        verbosity,
                        expect_spans,
                        health_check_display_reports_receiver,
                    ),
                )),
            }
        }
        ConsoleType::None => Ok(Box::new(ErrorConsole)),
    };

    match result {
        Ok(result) => result,
        Err(e) => {
            eprintln!(
                "Falling back to simple console, super console initialization failed: {}",
                e
            );
            let _unused = soft_error!("console_init_failed", e);
            Box::new(SimpleConsole::<NoopEventObserverExtra>::autodetect(
                trace_id,
                verbosity,
                expect_spans,
                // Maybe refactor and set this.
                None,
            ))
        }
    }
}

#[derive(
    Debug,
    serde::Serialize,
    serde::Deserialize,
    Clone,
    Dupe,
    Copy,
    clap::ValueEnum
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
#[clap(next_help_heading = "Console Options")]
pub struct CommonConsoleOptions {
    #[clap(
        long = "console",
        help = "Which console to use for this command",
        default_value = "auto",
        ignore_case = true,
        env = buck2_env_name!("BUCK_CONSOLE"),
        value_name = "super|simple|...",
        value_enum
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
        num_args = 1..,
        value_enum,
    )]
    pub ui: Vec<UiOptions>,

    #[clap(
        long,
        help = "Disable console interactions",
        env = buck2_env_name!(BUCK_NO_INTERACTIVE_CONSOLE),
        value_parser = FalseyValueParser::new(),
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
        let mut config = SuperConsoleConfig {
            expanded_progress: !buck2_env!("BUCK_DISABLE_EXPANDED_PROGRESS", bool).unwrap_or(false),
            ..SuperConsoleConfig::default()
        };

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
