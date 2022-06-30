/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_core::exit_result::ExitResult;
use buck2_core::fs::paths::AbsPathBuf;
use cli_proto::targets_request;
use cli_proto::TargetsRequest;
use futures::FutureExt;
use gazebo::prelude::*;

use crate::commands::common::CommonBuildConfigurationOptions;
use crate::commands::common::CommonConsoleOptions;
use crate::commands::common::CommonEventLogOptions;
use crate::daemon::client::BuckdClientConnector;
use crate::CommandContext;
use crate::StreamingCommand;

// Use non-camel case so the possible values match buck1's
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Dupe, clap::ArgEnum)]
#[clap(rename_all = "snake_case")]
enum TargetHashFileMode {
    PathsOnly,
    PathsAndContents,
}

#[derive(Debug, clap::ArgEnum, Clone, Dupe)]
enum TargetHashGraphType {
    None,
    Unconfigured,
    Configured,
}

// Use non-camel case so the possible values match buck1's
/// Possible values for the --target-hash-function arg. We don't actually
/// honor the specific algorithms, we use them as a hint to pick "fast" or "strong".
#[allow(non_camel_case_types)]
#[derive(Debug, clap::ArgEnum, Clone, Dupe)]
enum TargetHashFunction {
    Sha1,
    Sha256,
    Murmur_Hash3,
    Fast,
    Strong,
}

#[derive(Debug, clap::Parser)]
#[clap(name = "targets", about = "Show details about the specified targets")]
pub(crate) struct TargetsCommand {
    #[clap(flatten)]
    config_opts: CommonBuildConfigurationOptions,

    #[clap(flatten)]
    #[allow(unused)]
    console_opts: CommonConsoleOptions,

    #[clap(flatten)]
    event_log_opts: CommonEventLogOptions,

    #[clap(long = "json", help = "print targets json")]
    json: bool,

    #[clap(long = "stats", help = "print stats")]
    stats: bool,

    #[clap(
        long,
        alias = "resolvealias",
        help = "Print the fully-qualified build target for the specified alias[es]"
    )]
    resolve_alias: bool,

    /// Print a stable hash of each target after the target name. Incompatible with '--show-rulekey'.
    #[clap(long, conflicts_with = "show-unconfigured-target-hash")]
    show_target_hash: bool,

    /// TODO: Print a stable unconfigured hash of each target after the target name.
    #[clap(long, conflicts_with = "show-target-hash")]
    show_unconfigured_target_hash: bool,

    /// Modifies computation of target hashes. If set to `PATHS_AND_CONTENTS` (the default), the contents
    /// of all files referenced from the targets will be used to compute the target hash. If set to
    /// `PATHS_ONLY`, only files' paths contribute to the hash. See also --target-hash-modified-paths.
    #[clap(
        long,
        arg_enum,
        ignore_case = true,
        default_value = "paths_and_contents"
    )]
    target_hash_file_mode: TargetHashFileMode,

    /// Modifies computation of target hashes. Only effective when --target-hash-file-mode is set to
    /// `PATHS_ONLY`. If a target or its dependencies reference a file from this set, the target's hash
    /// will be different than if this option was omitted. Otherwise, the target's hash will be the same
    /// as if this option was omitted.
    #[clap(long, multiple_values = true)]
    target_hash_modified_paths: Vec<String>,

    /// Selects either the "fast" or the "strong" target hash function to be used for computing target hashes.
    /// While we don't specify the exact algorithm, the "strong" algorithm should be a reasonable cryptographic
    /// hash (ex. blake3) while the "fast" function will likely be a non-crypto hash. Both functions are
    /// guaranteed to be deterministic and to have the same value across different platforms/architectures.
    #[clap(long, ignore_case = true, default_value = "fast", arg_enum)]
    target_hash_function: TargetHashFunction,

    #[clap(
        long,
        help = "List of attributes to output, --output-attribute attr1. Attributes can be \
        regular expressions. Multiple attributes may be selected by specifying this option \
        multiple times.",
        // without limiting number_of_values, clap will read all space-separated values
        // after the flag, we want to require that each value be preceded individually by the flag.
        number_of_values = 1
    )]
    output_attribute: Vec<String>,

    /// Enables printing of default attributes. This would be attributes in a target that aren't
    /// explicitly set in the target but instead use the default set in the rule declaration.
    #[structopt(long)]
    include_defaults: bool,

    /// Deprecated: Use `--output-attribute` instead.
    ///
    /// List of space-separated attributes to output, --output-attributes attr1 attr2.
    #[clap(long, multiple_values = true)]
    output_attributes: Vec<String>,

    #[clap(name = "TARGET_PATTERNS", help = "Patterns to interpret")]
    patterns: Vec<String>,

    #[clap(
        long = "show-output",
        help = "Print the path to the output for each of the rules relative to the cell"
    )]
    show_output: bool,

    #[clap(
        long = "show-full-output",
        help = "Print the absolute path to the output for each of the rules relative to the cell"
    )]
    show_full_output: bool,

    #[clap(long, help = "Show target call stacks")]
    target_call_stacks: bool,
}

#[async_trait]
impl StreamingCommand for TargetsCommand {
    const COMMAND_NAME: &'static str = "targets";

    async fn exec_impl(
        mut self,
        buckd: BuckdClientConnector,
        matches: &clap::ArgMatches,
        ctx: CommandContext,
    ) -> ExitResult {
        let target_hash_use_fast_hash = match self.target_hash_function {
            TargetHashFunction::Sha1 | TargetHashFunction::Sha256 => {
                crate::eprintln!(
                    "buck2 only supports \"fast\" and \"strong\" target hash functions. Using the \"strong\" hash."
                )?;
                false
            }
            TargetHashFunction::Murmur_Hash3 => {
                crate::eprintln!(
                    "buck2 only supports \"fast\" and \"strong\" target hash functions. Using the \"fast\" hash."
                )?;
                true
            }
            TargetHashFunction::Fast => true,
            TargetHashFunction::Strong => false,
        };

        #[derive(thiserror::Error, Debug)]
        enum TargetsError {
            #[error("Flags are mutually exclusive (internal error)")]
            IncompatibleArguments,
        }

        let output_attributes = self.output_attributes();
        let target_hash_graph_type =
            match (self.show_target_hash, self.show_unconfigured_target_hash) {
                (true, true) => {
                    return ExitResult::Err(anyhow::Error::new(
                        TargetsError::IncompatibleArguments,
                    ));
                }
                (true, false) => targets_request::TargetHashGraphType::Configured as i32,
                (false, true) => targets_request::TargetHashGraphType::Unconfigured as i32,
                (false, false) => targets_request::TargetHashGraphType::None as i32,
            };

        let target_request = TargetsRequest {
            context: Some(ctx.client_context(&self.config_opts, matches)?),
            target_patterns: self.patterns.map(|pat| buck2_data::TargetPattern {
                value: pat.to_owned(),
            }),
            json: self.json,
            stats: self.stats,
            output_attributes,
            target_hash_file_mode: match self.target_hash_file_mode {
                TargetHashFileMode::PathsOnly => {
                    targets_request::TargetHashFileMode::PathsOnly as i32
                }
                TargetHashFileMode::PathsAndContents => {
                    targets_request::TargetHashFileMode::PathsAndContents as i32
                }
            },
            target_hash_modified_paths: self.target_hash_modified_paths,
            target_hash_use_fast_hash,
            unstable_resolve_aliases: self.resolve_alias,
            target_call_stacks: self.target_call_stacks,
            target_hash_graph_type,
            include_default_attributes: self.include_defaults,
        };

        if self.show_output {
            targets_show_outputs(buckd, target_request, None).await
        } else if self.show_full_output {
            let project_root = ctx.paths?.roots.project_root;
            targets_show_outputs(buckd, target_request, Some(&project_root)).await
        } else {
            targets(buckd, target_request).await
        }
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        &self.console_opts
    }

    fn event_log_opts(&self) -> &CommonEventLogOptions {
        &self.event_log_opts
    }

    fn common_opts(&self) -> &CommonBuildConfigurationOptions {
        &self.config_opts
    }
}

async fn targets_show_outputs(
    mut buckd: BuckdClientConnector,
    target_request: TargetsRequest,
    root_path: Option<&AbsPathBuf>,
) -> ExitResult {
    let response = buckd
        .with_flushing(|client| client.targets_show_outputs(target_request).boxed())
        .await???;
    for target_paths in response.targets_paths {
        for path in target_paths.paths {
            match root_path {
                Some(root) => {
                    crate::println!("{} {}", target_paths.target, root.join(path).display())
                }
                None => crate::println!("{} {}", target_paths.target, path),
            }?;
        }
    }
    ExitResult::success()
}

async fn targets(mut buckd: BuckdClientConnector, target_request: TargetsRequest) -> ExitResult {
    let response = buckd
        .with_flushing(|client| client.targets(target_request).boxed())
        .await???;
    if !response.serialized_targets_output.is_empty() {
        crate::print!("{}", response.serialized_targets_output)?;
    }
    ExitResult::success()
}

impl TargetsCommand {
    fn output_attributes(&self) -> Vec<String> {
        if !self.output_attribute.is_empty() {
            self.output_attribute.clone()
        } else {
            self.output_attributes.clone()
        }
    }
}
