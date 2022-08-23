/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! This modules contains common options that are shared between different commands.
//! They are shared by composition together with flattening of the options.
//!
//! For example, to adopt config options, add the following field to the
//! command definition:
//!
//! ```ignore
//! #[derive(Debug, clap::Parser)]
//! struct MyCommand {
//!    #[clap(flatten)]
//!    config_opts: CommonConfigOptions,
//!    ...
//! }
//! ```
use std::path::Path;
use std::path::PathBuf;

use buck2_core::fs::anyhow as fs;
use buck2_server::configs::ConfigType;
use cli_proto::common_build_options::ExecutionStrategy;
use cli_proto::ConfigOverride;
use gazebo::prelude::*;
use termwiz::istty::IsTty;

use crate::commands::common::final_console::FinalConsole;
use crate::commands::common::subscribers::superconsole::SuperConsoleConfig;

pub(crate) mod final_console;
pub mod subscribers;
pub mod what_ran;

pub const EVENT_LOG: &str = "--event-log";
pub const NO_EVENT_LOG: &str = "--no-event-log";

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
pub(crate) enum ConsoleType {
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
pub(crate) enum UiOptions {
    Dice,
    DebugEvents,
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
pub(crate) enum HostPlatformOverride {
    Default,
    Linux,
    MacOs,
    Windows,
}

/// Defines options related to commands that involves a streaming daemon command.
#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
pub(crate) struct CommonDaemonCommandOptions {
    /// Write events to this log file
    #[clap(value_name = "PATH", long = EVENT_LOG)]
    pub event_log: Option<PathBuf>,

    /// Do not write any event logs. Overrides --event-log. Used from `replay` to avoid recursive logging
    #[clap(long = NO_EVENT_LOG, hidden = true)]
    pub no_event_log: bool,

    #[clap(long = "--write-build-id")]
    pub build_id_file: Option<PathBuf>,
}

impl CommonDaemonCommandOptions {
    pub(crate) fn default_ref() -> &'static Self {
        static DEFAULT: CommonDaemonCommandOptions = CommonDaemonCommandOptions {
            event_log: None,
            no_event_log: false,
            build_id_file: None,
        };
        &DEFAULT
    }
}

/// Defines options for config and configuration related things. Any command that involves the build graph should include these options.
#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
pub(crate) struct CommonBuildConfigurationOptions {
    #[clap(
        value_name = "SECTION.OPTION=VALUE",
        long = "config",
        short = 'c',
        help = "List of config options",
        // Needs to be explicitly set, otherwise will treat `-c a b c` -> [a, b, c]
        // rather than [a] and other positional arguments `b c`.
        number_of_values = 1
    )]
    pub config_values: Vec<String>,

    #[clap(
        value_name = "PATH",
        long = "config-file",
        help = "List of config file paths",
        number_of_values = 1
    )]
    pub config_files: Vec<String>,

    #[clap(
        long = "target-platforms",
        help = "Configuration target (one) to use to configure targets",
        number_of_values = 1,
        value_name = "PLATFORM"
    )]
    pub target_platforms: Option<String>,

    #[clap(long, ignore_case = true, value_name = "HOST", arg_enum)]
    fake_host: Option<HostPlatformOverride>,

    // TODO(cjhopman): Why is this only in CommonConfigOptions options, it has nothing to do with config? Shouldn't all commands support --oncall?
    #[clap(long)]
    pub oncall: Option<String>,

    /// Disable runtime type checking in Starlark interpreter.
    ///
    /// This option is not stable, and can be used only locally
    /// to diagnose evaluation performance problems.
    #[clap(long)]
    pub disable_starlark_types: bool,
}

impl CommonBuildConfigurationOptions {
    /// Produces a single, ordered list of config overrides. A `ConfigOverride`
    /// represents either a file, passed via `--config-file`, or a config value,
    /// passed via `-c`/`--config`. The relative order of those are important,
    /// hence they're merged into a single list.
    pub(crate) fn config_overrides(
        &self,
        matches: &clap::ArgMatches,
    ) -> anyhow::Result<Vec<ConfigOverride>> {
        fn with_indices<'a, T>(
            collection: &'a [T],
            name: &str,
            matches: &'a clap::ArgMatches,
        ) -> impl Iterator<Item = (usize, &'a T)> + 'a {
            let indices = matches.indices_of(name);
            let indices = indices.unwrap_or_default();
            assert_eq!(
                indices.len(),
                collection.len(),
                "indices len is not equal to collection len for flag `{}`",
                name
            );
            indices.into_iter().zip(collection)
        }

        // Relative paths passed on the command line are relative to the cwd
        // of the client, not the daemon, so perform path canonicalisation here.
        fn resolve_config_file_argument(arg: &str) -> anyhow::Result<String> {
            if arg.contains("//") {
                // Cell-relative path resolution would be performed by the daemon
                return Ok(arg.to_owned());
            }

            let path = Path::new(arg);
            if path.is_absolute() {
                return Ok(arg.to_owned());
            }

            let abs_path = fs::canonicalize(path)?;
            Ok(abs_path.to_string_lossy().into_owned())
        }

        let config_values_args = with_indices(&self.config_values, "config-values", matches).map(
            |(index, config_value)| {
                (
                    index,
                    ConfigOverride {
                        config_override: config_value.clone(),
                        config_type: ConfigType::Value as i32,
                    },
                )
            },
        );

        let config_file_args = with_indices(&self.config_files, "config-files", matches)
            .map(|(index, unresolved_file)| {
                let resolved_file = resolve_config_file_argument(unresolved_file)?;
                Ok((
                    index,
                    ConfigOverride {
                        config_override: resolved_file,
                        config_type: ConfigType::File as i32,
                    },
                ))
            })
            .collect::<anyhow::Result<Vec<_>>>()?;

        let mut ordered_merged_configs: Vec<(usize, ConfigOverride)> = config_file_args;
        ordered_merged_configs.extend(config_values_args);
        ordered_merged_configs.sort_by(|(lhs_index, _), (rhs_index, _)| lhs_index.cmp(rhs_index));

        Ok(ordered_merged_configs.into_map(|(_, config_arg)| config_arg))
    }

    pub(crate) fn host_platform_override(&self) -> HostPlatformOverride {
        match &self.fake_host {
            Some(v) => *v,
            None => HostPlatformOverride::Default,
        }
    }

    pub(crate) fn default_ref() -> &'static Self {
        static DEFAULT: CommonBuildConfigurationOptions = CommonBuildConfigurationOptions {
            config_values: vec![],
            config_files: vec![],
            target_platforms: None,
            fake_host: None,
            oncall: None,
            disable_starlark_types: false,
        };
        &DEFAULT
    }
}

/// Defines common options for build-like commands (build, test, install).
#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
pub(crate) struct CommonBuildOptions {
    /// Print a build report
    ///
    /// --build-report=- will print the build report to stdout
    /// --build-report=<filepath> will write the build report to the file
    #[clap(long = "build-report", value_name = "PATH")]
    build_report: Option<String>,

    /// Deprecated. Use --build-report=-
    // TODO(cjhopman): this is probably only used by the e2e framework. remove it from there
    #[clap(long = "print-build-report", hidden = true)]
    print_build_report: bool,

    /// Number of threads to use during execution (default is # cores)
    // TODO(cjhopman): This only limits the threads used for action execution and it doesn't work correctly with concurrent commands.
    #[clap(short = 'j', long = "num-threads", value_name = "THREADS")]
    pub num_threads: Option<u32>,

    /// **You probably should not use this, use --prefer-local instead**. Enable only local
    /// execution. Will reject actions that cannot execute locally. If execution platforms are
    /// enabled and you have a hybrid platform, this will most likely result in just rejecting all
    /// actions.
    #[clap(long, group = "build_strategy")]
    local_only: bool,

    /// Enable only remote execution. Will reject actions that cannot execute remotely.
    #[clap(long, group = "build_strategy")]
    remote_only: bool,

    /// Enable hybrid execution. Will prefer executing actions that can execute locally on the
    /// local host.
    #[clap(long, group = "build_strategy")]
    prefer_local: bool,

    /// Enable hybrid execution. Will prefer executing actions that can execute remotely on RE.
    #[clap(long, group = "build_strategy")]
    hybrid: bool,

    /// Experimental: Disable all execution.
    #[clap(long, group = "build_strategy")]
    unstable_no_execution: bool,

    /// Process dep files when they are generated (i.e. after running a command that produces dep
    /// files), rather than when they are used (i.e. before re-running a command that previously
    /// produced dep files). Use this when debugging commands that produce dep files. Note that
    /// commands that previously produced dep files will not re-run: only dep files produced during
    /// this command will be eagerly loaded.
    #[clap(long)]
    eager_dep_files: bool,

    #[clap(long)]
    upload_all_actions: bool,
}

impl CommonBuildOptions {
    fn build_report(&self) -> (bool, String) {
        match (self.print_build_report, &self.build_report) {
            (false, None) => (false, "".to_owned()),
            (_, Some(path)) if path != "-" => (true, path.to_owned()),
            _ => (true, "".to_owned()),
        }
    }

    pub(crate) fn to_proto(&self) -> cli_proto::CommonBuildOptions {
        let (unstable_print_build_report, unstable_build_report_filename) = self.build_report();
        let concurrency = self
            .num_threads
            .map(|num| cli_proto::Concurrency { concurrency: num });

        cli_proto::CommonBuildOptions {
            concurrency,
            execution_strategy: if self.local_only {
                ExecutionStrategy::LocalOnly as i32
            } else if self.remote_only {
                ExecutionStrategy::RemoteOnly as i32
            } else if self.hybrid {
                ExecutionStrategy::Hybrid as i32
            } else if self.prefer_local {
                ExecutionStrategy::HybridPreferLocal as i32
            } else if self.unstable_no_execution {
                ExecutionStrategy::NoExecution as i32
            } else {
                ExecutionStrategy::Default as i32
            },
            unstable_print_build_report,
            unstable_build_report_filename,
            eager_dep_files: self.eager_dep_files,
            upload_all_actions: self.upload_all_actions,
        }
    }
}

/// Defines common console options for commands.
#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
pub(crate) struct CommonConsoleOptions {
    #[clap(
        long = "--console",
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
    #[clap(
        long = "--ui",
        ignore_case = true,
        multiple = true,
        number_of_values = 1,
        arg_enum
    )]
    pub ui: Vec<UiOptions>,
}

impl Default for CommonConsoleOptions {
    fn default() -> Self {
        Self {
            console_type: ConsoleType::Auto,
            ui: Vec::new(),
        }
    }
}

impl CommonConsoleOptions {
    pub(crate) fn default_ref() -> &'static Self {
        static OPTS: CommonConsoleOptions = CommonConsoleOptions {
            console_type: ConsoleType::Auto,
            ui: vec![],
        };
        &OPTS
    }
    pub(crate) fn final_console(&self) -> FinalConsole {
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

    pub(crate) fn superconsole_config(&self) -> SuperConsoleConfig {
        let mut config = SuperConsoleConfig::default();
        for option in &self.ui {
            match option {
                UiOptions::Dice => config.enable_dice = true,
                UiOptions::DebugEvents => config.enable_debug_events = true,
            }
        }
        config
    }
}
