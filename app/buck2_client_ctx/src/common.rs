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

use buck2_cli_proto::common_build_options::ExecutionStrategy;
use buck2_cli_proto::config_override::ConfigType;
use buck2_cli_proto::ConfigOverride;
use buck2_core::fs::fs_util;
use dupe::Dupe;
use gazebo::prelude::*;
use termwiz::istty::IsTty;

use crate::final_console::FinalConsole;
use crate::path_arg::PathArg;
use crate::subscribers::superconsole::SuperConsoleConfig;

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
pub enum HostPlatformOverride {
    Default,
    Linux,
    MacOs,
    Windows,
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
pub enum HostArchOverride {
    Default,
    AArch64,
    X86_64,
}

/// Defines options related to commands that involves a streaming daemon command.
#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize, Default)]
pub struct CommonDaemonCommandOptions {
    /// Write events to this log file
    #[clap(value_name = "PATH", long = EVENT_LOG)]
    pub event_log: Option<PathArg>,

    /// Do not write any event logs. Overrides --event-log. Used from `replay` to avoid recursive logging
    #[clap(long = NO_EVENT_LOG, hidden = true)]
    pub no_event_log: bool,

    /// Write command invocation id into this file.
    #[clap(long, value_name = "PATH")]
    pub(crate) write_build_id: Option<PathArg>,

    /// Write the invocation record (as JSON) to this path. No guarantees whatsoever are made
    /// regarding the stability of the format.
    #[clap(long, value_name = "PATH")]
    pub(crate) unstable_write_invocation_record: Option<PathArg>,
}

impl CommonDaemonCommandOptions {
    pub fn default_ref() -> &'static Self {
        static DEFAULT: CommonDaemonCommandOptions = CommonDaemonCommandOptions {
            event_log: None,
            no_event_log: false,
            write_build_id: None,
            unstable_write_invocation_record: None,
        };
        &DEFAULT
    }
}

/// Defines options for config and configuration related things. Any command that involves the build graph should include these options.
#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize, Default)]
pub struct CommonBuildConfigurationOptions {
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

    #[clap(long, ignore_case = true, value_name = "ARCH", arg_enum)]
    fake_arch: Option<HostArchOverride>,

    /// Value must be formatted as: version-build (e.g., 14.3.0-14C18 or 14.1-14B47b)
    #[clap(long, value_name = "VERSION-BUILD")]
    fake_xcode_version: Option<String>,

    // TODO(cjhopman): Why is this only in CommonConfigOptions options, it has nothing to do with config? Shouldn't all commands support --oncall?
    #[clap(long)]
    pub oncall: Option<String>,

    /// Disable runtime type checking in Starlark interpreter.
    ///
    /// This option is not stable, and can be used only locally
    /// to diagnose evaluation performance problems.
    #[clap(long)]
    pub disable_starlark_types: bool,

    /// Record or show target call stacks.
    ///
    /// Starlark call stacks will be included in duplicate targets error.
    ///
    /// If a command outputs targets (like `targets` command),
    /// starlark call stacks will be printed after the targets.
    #[clap(long = "stack")]
    pub target_call_stacks: bool,

    /// If there are targets with duplicate names in `BUCK` file,
    /// skip all the duplicates but the first one.
    /// This is a hack for TD. Do not use this option.
    #[clap(long)]
    pub(crate) skip_targets_with_duplicate_names: bool,

    #[clap(long)]
    pub reuse_current_config: bool,

    /// Used for exiting a concurrent command when a different state is detected.
    #[clap(long)]
    pub exit_when_different_state: bool,
}

impl CommonBuildConfigurationOptions {
    /// Produces a single, ordered list of config overrides. A `ConfigOverride`
    /// represents either a file, passed via `--config-file`, or a config value,
    /// passed via `-c`/`--config`. The relative order of those are important,
    /// hence they're merged into a single list.
    pub fn config_overrides(
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

            let abs_path = fs_util::canonicalize(path)?;
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

    pub fn host_platform_override(&self) -> HostPlatformOverride {
        match &self.fake_host {
            Some(v) => *v,
            None => HostPlatformOverride::Default,
        }
    }
    pub fn host_arch_override(&self) -> HostArchOverride {
        match &self.fake_arch {
            Some(v) => *v,
            None => HostArchOverride::Default,
        }
    }
    pub fn host_xcode_version_override(&self) -> Option<String> {
        self.fake_xcode_version.to_owned()
    }

    pub fn default_ref() -> &'static Self {
        static DEFAULT: CommonBuildConfigurationOptions = CommonBuildConfigurationOptions {
            config_values: vec![],
            config_files: vec![],
            target_platforms: None,
            fake_host: None,
            fake_arch: None,
            fake_xcode_version: None,
            oncall: None,
            disable_starlark_types: false,
            target_call_stacks: false,
            skip_targets_with_duplicate_names: false,
            reuse_current_config: false,
            exit_when_different_state: false,
        };
        &DEFAULT
    }
}

/// Defines common options for build-like commands (build, test, install).
#[allow(rustdoc::invalid_html_tags)]
#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
pub struct CommonBuildOptions {
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

    /// Enable only local execution. Will reject actions that cannot execute locally.
    #[clap(long, group = "build_strategy")]
    local_only: bool,

    /// Enable only remote execution. Will reject actions that cannot execute remotely.
    #[clap(long, group = "build_strategy")]
    remote_only: bool,

    /// Enable hybrid execution. Will prefer executing actions that can execute locally on the
    /// local host.
    #[clap(long, group = "build_strategy")]
    prefer_local: bool,

    /// Enable hybrid execution. Will prefer executing actions that can execute remotely on RE and will avoid racing local and remote execution.
    #[clap(long, group = "build_strategy")]
    prefer_remote: bool,

    /// Experimental: Disable all execution.
    #[clap(long, group = "build_strategy")]
    unstable_no_execution: bool,

    /// Do not perform remote cache queries or cache writes. If remote execution is enabled, the RE
    /// service might still deduplicate actions, so for e.g. benchmarking, using a random isolation
    /// dir is preferred.
    #[clap(long)]
    no_remote_cache: bool,

    /// Could be used to enable the action cache writes on the RE worker when no_remote_cache is specified
    #[clap(long, requires("no-remote-cache"))]
    write_to_cache_anyway: bool,

    /// Process dep files when they are generated (i.e. after running a command that produces dep
    /// files), rather than when they are used (i.e. before re-running a command that previously
    /// produced dep files). Use this when debugging commands that produce dep files. Note that
    /// commands that previously produced dep files will not re-run: only dep files produced during
    /// this command will be eagerly loaded.
    #[clap(long)]
    eager_dep_files: bool,

    #[clap(long)]
    upload_all_actions: bool,

    /// If Buck hits an error, do as little work as possible before exiting.
    #[clap(long, group = "fail-when")]
    fail_fast: bool,

    /// If Buck hits an error, continue doing as much work as possible before exiting.
    #[clap(long, group = "fail-when")]
    keep_going: bool,

    /// If target is missing, then skip building instead of throwing error.
    #[clap(long)]
    skip_missing_targets: bool,

    /// If target is incompatible with the specified configuration, skip building instead of throwing error.
    /// This does not apply to targets specified with glob patterns `/...` or `:`
    /// which are skipped unconditionally.
    #[clap(long)]
    skip_incompatible_targets: bool,
}

impl CommonBuildOptions {
    fn build_report(&self) -> (bool, String) {
        match (self.print_build_report, &self.build_report) {
            (false, None) => (false, "".to_owned()),
            (_, Some(path)) if path != "-" => (true, path.to_owned()),
            _ => (true, "".to_owned()),
        }
    }

    pub fn to_proto(&self) -> buck2_cli_proto::CommonBuildOptions {
        let (unstable_print_build_report, unstable_build_report_filename) = self.build_report();
        let concurrency = self
            .num_threads
            .map(|num| buck2_cli_proto::Concurrency { concurrency: num });

        buck2_cli_proto::CommonBuildOptions {
            concurrency,
            execution_strategy: if self.local_only {
                ExecutionStrategy::LocalOnly as i32
            } else if self.remote_only {
                ExecutionStrategy::RemoteOnly as i32
            } else if self.prefer_local {
                ExecutionStrategy::HybridPreferLocal as i32
            } else if self.prefer_remote {
                ExecutionStrategy::HybridPreferRemote as i32
            } else if self.unstable_no_execution {
                ExecutionStrategy::NoExecution as i32
            } else {
                ExecutionStrategy::Default as i32
            },
            unstable_print_build_report,
            unstable_build_report_filename,
            eager_dep_files: self.eager_dep_files,
            upload_all_actions: self.upload_all_actions,
            skip_cache_read: self.no_remote_cache,
            skip_cache_write: self.no_remote_cache && !self.write_to_cache_anyway,
            fail_fast: self.fail_fast,
            keep_going: self.keep_going,
            skip_missing_targets: self.skip_missing_targets,
            skip_incompatible_targets: self.skip_incompatible_targets,
        }
    }
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

/// Common options for commands like `build` or `query`.
/// Not all the commands have all the options.
#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize, Default)]
pub struct CommonCommandOptions {
    /// Buckconfig and similar options.
    #[clap(flatten)]
    pub config_opts: CommonBuildConfigurationOptions,

    /// UI options.
    #[clap(flatten)]
    pub console_opts: CommonConsoleOptions,

    /// Event-log options.
    #[clap(flatten)]
    pub event_log_opts: CommonDaemonCommandOptions,
}
