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

pub mod build;
pub mod target_cfg;
pub mod ui;

use std::path::Path;

use buck2_cli_proto::config_override::ConfigType;
use buck2_cli_proto::ConfigOverride;
use buck2_core::fs::fs_util;
use dupe::Dupe;
use gazebo::prelude::*;

use crate::common::ui::CommonConsoleOptions;
use crate::path_arg::PathArg;

pub const EVENT_LOG: &str = "event-log";
pub const NO_EVENT_LOG: &str = "no-event-log";

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
    clap::ValueEnum
)]
#[clap(rename_all = "lower")]
pub enum HostArchOverride {
    Default,
    AArch64,
    X86_64,
}

/// Defines options related to commands that involves a streaming daemon command.
#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize, Default)]
pub struct CommonEventLogOptions {
    /// Write events to this log file
    #[clap(value_name = "PATH", long = EVENT_LOG)]
    pub event_log: Option<PathArg>,

    /// Do not write any event logs. Overrides --event-log. Used from `replay` to avoid recursive logging
    #[clap(long = NO_EVENT_LOG, hide = true)]
    pub no_event_log: bool,

    /// Write command invocation id into this file.
    #[clap(long, value_name = "PATH")]
    pub(crate) write_build_id: Option<PathArg>,

    /// Write the invocation record (as JSON) to this path. No guarantees whatsoever are made
    /// regarding the stability of the format.
    #[clap(long, value_name = "PATH")]
    pub(crate) unstable_write_invocation_record: Option<PathArg>,
}

impl CommonEventLogOptions {
    pub fn default_ref() -> &'static Self {
        static DEFAULT: CommonEventLogOptions = CommonEventLogOptions {
            event_log: None,
            no_event_log: false,
            write_build_id: None,
            unstable_write_invocation_record: None,
        };
        &DEFAULT
    }
}

/// Defines options for config and configuration related things. Any command that involves the build
/// graph should include these options.
#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize, Default)]
pub struct CommonBuildConfigurationOptions {
    #[clap(
        value_name = "SECTION.OPTION=VALUE",
        long = "config",
        short = 'c',
        help = "List of config options",
        // Needs to be explicitly set, otherwise will treat `-c a b c` -> [a, b, c]
        // rather than [a] and other positional arguments `b c`.
        num_args = 1
    )]
    pub config_values: Vec<String>,

    #[clap(
        value_name = "PATH",
        long = "config-file",
        help = "List of config file paths",
        num_args = 1
    )]
    pub config_files: Vec<String>,

    #[clap(long, ignore_case = true, value_name = "HOST", value_enum)]
    fake_host: Option<HostPlatformOverride>,

    #[clap(long, ignore_case = true, value_name = "ARCH", value_enum)]
    fake_arch: Option<HostArchOverride>,

    /// Value must be formatted as: version-build (e.g., 14.3.0-14C18 or 14.1-14B47b)
    #[clap(long, value_name = "VERSION-BUILD")]
    fake_xcode_version: Option<String>,

    /// Re-uses any `--config` values (inline or via modefiles) if there's
    /// a previous command, otherwise the flag is ignored.
    ///
    /// If there is a previous command and `--reuse-current-config` is set,
    /// then the old config is used, ignoring any overrides.
    ///
    /// If there is no previous command but the flag was set, then the flag is ignored,
    /// the command behaves as if the flag was not set at all.
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

        let config_values_args = with_indices(&self.config_values, "config_values", matches).map(
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

        let config_file_args = with_indices(&self.config_files, "config_files", matches)
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
            fake_host: None,
            fake_arch: None,
            fake_xcode_version: None,

            reuse_current_config: false,
            exit_when_different_state: false,
        };
        &DEFAULT
    }
}

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize, Default)]
pub struct CommonStarlarkOptions {
    /// Disable runtime type checking in Starlark interpreter.
    ///
    /// This option is not stable, and can be used only locally
    /// to diagnose evaluation performance problems.
    #[clap(long)]
    pub disable_starlark_types: bool,

    /// Typecheck bzl and bxl files during evaluation.
    #[clap(long, hide = true)]
    pub unstable_typecheck: bool,

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
}

impl CommonStarlarkOptions {
    pub fn default_ref() -> &'static Self {
        static DEFAULT: CommonStarlarkOptions = CommonStarlarkOptions {
            disable_starlark_types: false,
            unstable_typecheck: false,
            target_call_stacks: false,
            skip_targets_with_duplicate_names: false,
        };
        &DEFAULT
    }
}

/// Common options for commands like `build` or `query`.
/// Not all the commands have all the options.
#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize, Default)]
pub struct CommonCommandOptions {
    /// Buckconfig and similar options.
    #[clap(flatten)]
    pub config_opts: CommonBuildConfigurationOptions,

    /// Starlark options.
    #[clap(flatten)]
    pub starlark_opts: CommonStarlarkOptions,

    /// UI options.
    #[clap(flatten)]
    pub console_opts: CommonConsoleOptions,

    /// Event-log options.
    #[clap(flatten)]
    pub event_log_opts: CommonEventLogOptions,
}

#[derive(Debug, PartialEq)]
pub enum PrintOutputsFormat {
    Plain,
    Simple,
    Json,
}
