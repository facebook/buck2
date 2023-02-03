/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_cli_proto::targets_request;
use buck2_cli_proto::TargetsRequest;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::CommonBuildConfigurationOptions;
use buck2_client_ctx::common::CommonConsoleOptions;
use buck2_client_ctx::common::CommonDaemonCommandOptions;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::path_arg::PathArg;
use buck2_client_ctx::stdin::Stdin;
use buck2_client_ctx::streaming::StreamingCommand;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_query_common::query_args::CommonAttributeArgs;
use dupe::Dupe;
use gazebo::prelude::*;

// Use non-camel case so the possible values match buck1's
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Dupe, clap::ArgEnum)]
#[clap(rename_all = "snake_case")]
enum TargetHashFileMode {
    PathsOnly,
    PathsAndContents,
    None,
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
pub struct TargetsCommand {
    #[clap(flatten)]
    config_opts: CommonBuildConfigurationOptions,

    #[clap(flatten)]
    #[allow(unused)]
    console_opts: CommonConsoleOptions,

    #[clap(flatten)]
    event_log_opts: CommonDaemonCommandOptions,

    /// Print targets as JSON
    #[clap(long)]
    json: bool,

    /// Print statistics of how many entries were processed
    #[clap(long)]
    stats: bool,

    /// Print the fully-qualified build target for the specified aliases
    #[clap(long, alias = "resolvealias")]
    resolve_alias: bool,

    /// Print a stable hash of each target after the target name. Incompatible with '--show-rulekey'.
    #[clap(long, conflicts_with = "streaming")]
    show_target_hash: bool,

    /// Print a stable unconfigured hash of each target after the target name.
    #[clap(long, conflicts_with = "show-target-hash")]
    show_unconfigured_target_hash: bool,

    /// Modifies computation of target hashes. If set to `PATHS_AND_CONTENTS` (the default), the contents
    /// of all files referenced from the targets will be used to compute the target hash. If set to
    /// `PATHS_ONLY`, only files' paths contribute to the hash. If set to `NONE` no files will be used.
    /// See also --target-hash-modified-paths.
    #[clap(
        long,
        arg_enum,
        ignore_case = true,
        default_value = "paths_and_contents",
        conflicts_with = "streaming"
    )]
    target_hash_file_mode: TargetHashFileMode,

    /// Modifies computation of target hashes. Only effective when --target-hash-file-mode is set to
    /// `PATHS_ONLY`. If a target or its dependencies reference a file from this set, the target's hash
    /// will be different than if this option was omitted. Otherwise, the target's hash will be the same
    /// as if this option was omitted.
    #[clap(long, multiple_values = true, conflicts_with = "streaming")]
    target_hash_modified_paths: Vec<PathArg>,

    /// Selects either the "fast" or the "strong" target hash function to be used for computing target hashes.
    /// While we don't specify the exact algorithm, the "strong" algorithm should be a reasonable cryptographic
    /// hash (ex. blake3) while the "fast" function will likely be a non-crypto hash. Both functions are
    /// guaranteed to be deterministic and to have the same value across different platforms/architectures.
    #[clap(long, ignore_case = true, default_value = "fast", arg_enum)]
    target_hash_function: TargetHashFunction,

    /// When true, emit the hash or target node and all dependencies recursively.
    /// When false, hash only the target node.
    #[clap(long, action = clap::ArgAction::Set, default_value = "true", conflicts_with = "streaming")]
    target_hash_recursive: bool,

    #[clap(flatten)]
    attributes: CommonAttributeArgs,

    /// Enables printing of default attributes. This would be attributes in a target that aren't
    /// explicitly set in the target but instead use the default set in the rule declaration.
    #[clap(long)]
    include_defaults: bool,

    /// Patterns to interpret
    #[clap(name = "TARGET_PATTERNS")]
    patterns: Vec<String>,

    /// Print the path to the output for each of the rules relative to the cell
    #[clap(long)]
    show_output: bool,

    /// Print the absolute path to the output for each of the rules relative to the cell
    #[clap(long)]
    show_full_output: bool,

    /// Show target call stacks
    #[clap(long)]
    target_call_stacks: bool,

    /// On loading errors, put buck.error in the output stream and continue
    #[clap(long)]
    keep_going: bool,

    /// Write output as soon as it is available. The order of the output items
    /// is non-deterministic and if multiple patterns cover the same target, may
    /// have duplicates.
    #[clap(long)]
    streaming: bool,

    /// Don't cache the target information on the build graph
    #[clap(long, requires = "streaming")]
    no_cache: bool,

    /// Show the imports of each package/import. Shows an additional output per package/import
    /// (not per target), including implicit dependencies (e.g. the prelude) but only direct
    /// dependencies (not the transitive closure).
    #[clap(long, requires = "streaming")]
    imports: bool,
}

#[async_trait]
impl StreamingCommand for TargetsCommand {
    const COMMAND_NAME: &'static str = "targets";

    async fn exec_impl(
        mut self,
        buckd: BuckdClientConnector,
        matches: &clap::ArgMatches,
        mut ctx: ClientCommandContext,
    ) -> ExitResult {
        let target_hash_use_fast_hash = match self.target_hash_function {
            TargetHashFunction::Sha1 | TargetHashFunction::Sha256 => {
                buck2_client_ctx::eprintln!(
                    "buck2 only supports \"fast\" and \"strong\" target hash functions. Using the \"strong\" hash."
                )?;
                false
            }
            TargetHashFunction::Murmur_Hash3 => {
                buck2_client_ctx::eprintln!(
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

        let output_attributes = self.attributes.get()?;
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

        let context =
            Some(ctx.client_context(&self.config_opts, matches, self.sanitized_argv())?);

        let target_hash_modified_paths = self
            .target_hash_modified_paths
            .into_try_map(|path| path.resolve(&ctx.working_dir).into_string())?;

        let target_request = TargetsRequest {
            context,
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
                TargetHashFileMode::None => targets_request::TargetHashFileMode::NoFiles as i32,
            },
            target_hash_modified_paths,
            target_hash_use_fast_hash,
            unstable_resolve_aliases: self.resolve_alias,
            target_call_stacks: self.target_call_stacks,
            target_hash_graph_type,
            include_default_attributes: self.include_defaults,
            target_hash_recursive: self.target_hash_recursive,
            keep_going: self.keep_going,
            streaming: self.streaming,
            cached: !self.no_cache,
            imports: self.imports,
        };

        if self.show_output {
            targets_show_outputs(ctx.stdin(), buckd, target_request, None, &self.console_opts).await
        } else if self.show_full_output {
            let project_root = ctx.paths()?.roots.project_root.clone();
            targets_show_outputs(
                ctx.stdin(),
                buckd,
                target_request,
                Some(project_root.root()),
                &self.console_opts,
            )
            .await
        } else {
            targets(ctx.stdin(), buckd, target_request, &self.console_opts).await
        }
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        &self.console_opts
    }

    fn event_log_opts(&self) -> &CommonDaemonCommandOptions {
        &self.event_log_opts
    }

    fn common_opts(&self) -> &CommonBuildConfigurationOptions {
        &self.config_opts
    }
}

async fn targets_show_outputs(
    stdin: &mut Stdin,
    mut buckd: BuckdClientConnector,
    target_request: TargetsRequest,
    root_path: Option<&AbsNormPath>,
    console_opts: &CommonConsoleOptions,
) -> ExitResult {
    let response = buckd
        .with_flushing()
        .targets_show_outputs(
            target_request,
            stdin.console_interaction_stream(console_opts),
        )
        .await??;
    for target_paths in response.targets_paths {
        for path in target_paths.paths {
            let path = if cfg!(windows) {
                path.replace('/', "\\")
            } else {
                path
            };
            match root_path {
                Some(root) => {
                    buck2_client_ctx::println!(
                        "{} {}",
                        target_paths.target,
                        root.as_path().join(path).display()
                    )
                }
                None => buck2_client_ctx::println!("{} {}", target_paths.target, path),
            }?;
        }
    }
    ExitResult::success()
}

async fn targets(
    stdin: &mut Stdin,
    mut buckd: BuckdClientConnector,
    target_request: TargetsRequest,
    console_opts: &CommonConsoleOptions,
) -> ExitResult {
    let response = buckd
        .with_flushing()
        .targets(
            target_request,
            stdin.console_interaction_stream(console_opts),
        )
        .await??;
    if !response.serialized_targets_output.is_empty() {
        buck2_client_ctx::print!("{}", response.serialized_targets_output)?;
    }
    if response.error_count == 0 {
        ExitResult::success()
    } else {
        ExitResult::failure()
    }
}
