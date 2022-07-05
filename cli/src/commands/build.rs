/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::collections::HashMap;
use std::io;
use std::path::Path;
use std::path::PathBuf;

use anyhow::anyhow;
use anyhow::Context;
use async_trait::async_trait;
use buck2_core::exit_result::ExitResult;
use buck2_core::exit_result::FailureExitCode;
use cli_proto::build_request::build_providers;
use cli_proto::build_request::BuildProviders;
use cli_proto::build_request::ResponseOptions;
use cli_proto::BuildRequest;
use cli_proto::BuildTarget;
use futures::FutureExt;
use futures::TryStreamExt;
use gazebo::prelude::*;
use multimap::MultiMap;
use serde::Serialize;

use crate::commands::common::final_console::FinalConsole;
use crate::commands::common::CommonBuildConfigurationOptions;
use crate::commands::common::CommonBuildOptions;
use crate::commands::common::CommonConsoleOptions;
use crate::commands::common::CommonDaemonCommandOptions;
use crate::daemon::client::BuckdClientConnector;
use crate::daemon::client::CommandOutcome;
use crate::CommandContext;
use crate::StreamingCommand;

#[derive(Debug, clap::Parser)]
#[clap(name = "build", about = "Build the specified targets")]
pub(crate) struct BuildCommand {
    #[clap(flatten)]
    config_opts: CommonBuildConfigurationOptions,

    #[clap(flatten)]
    console_opts: CommonConsoleOptions,

    #[clap(flatten)]
    event_log_opts: CommonDaemonCommandOptions,

    #[clap(flatten)]
    build_opts: CommonBuildOptions,

    #[clap(name = "TARGET_PATTERNS", help = "Patterns to build")]
    patterns: Vec<String>,

    #[clap(long = "providers", help = "Print the providers of each target")]
    print_providers: bool,

    /// This option is currently on by default, but will become a proper option in future (T110004971)
    #[clap(long = "keep-going")]
    #[allow(unused)]
    keep_going: bool,

    /// This option does nothing. It is here to keep compatibility with Buck1 and ci
    #[clap(long = "deep")]
    #[allow(unused)] // for v1 compat
    deep: bool,

    #[clap(
        long = "show-output",
        help = "Print the path to the output for each of the built rules relative to the cell"
    )]
    show_output: bool,

    #[clap(
        long = "show-full-output",
        help = "Print the absolute path to the output for each of the built rules"
    )]
    show_full_output: bool,

    #[clap(
        long = "show-json-output",
        help = "Print the output paths relative to the cell, in JSON format"
    )]
    show_json_output: bool,

    #[clap(
        long = "show-full-json-output",
        help = "Print the output absolute paths, in JSON format"
    )]
    show_full_json_output: bool,

    #[clap(
        long = "materializations",
        help = "Materialize (or skip) the final artifacts, bypassing buckconfig.",
        ignore_case = true,
        arg_enum
    )]
    materializations: Option<FinalArtifactMaterializations>,

    #[allow(unused)]
    #[clap(
        long,
        group = "default-info",
        help = "Build default info (this is the default)"
    )]
    build_default_info: bool,

    #[clap(
        long,
        group = "default-info",
        help = "Do not build default info (this is not the default)"
    )]
    skip_default_info: bool,

    #[allow(unused)]
    #[clap(
        long,
        group = "run-info",
        help = "Build runtime dependencies (this is the default)"
    )]
    build_run_info: bool,

    #[clap(
        long,
        group = "run-info",
        help = "Do not build runtime dependencies (this is not the default)"
    )]
    skip_run_info: bool,

    #[clap(
        long,
        group = "test-info",
        alias = "build-test-dependencies",
        help = "Build tests (this is not the default)"
    )]
    build_test_info: bool,

    #[allow(unused)]
    #[clap(
        long,
        group = "test-info",
        help = "Do not build tests (this is the default)"
    )]
    skip_test_info: bool,

    #[clap(
        long = "out",
        help = "Copy the output of the built target to this path"
    )]
    output_path: Option<PathBuf>,
}

impl BuildCommand {
    fn default_info(&self) -> build_providers::Action {
        if self.skip_default_info {
            return build_providers::Action::Skip;
        }
        build_providers::Action::Build
    }

    fn run_info(&self) -> build_providers::Action {
        if self.skip_run_info {
            return build_providers::Action::Skip;
        }
        build_providers::Action::BuildIfAvailable
    }

    fn test_info(&self) -> build_providers::Action {
        if self.build_test_info {
            return build_providers::Action::BuildIfAvailable;
        }
        build_providers::Action::Skip
    }
}

#[derive(Debug, Clone, Dupe, clap::ArgEnum)]
#[clap(rename_all = "snake_case")]
pub(crate) enum FinalArtifactMaterializations {
    All,
    None,
}

pub(crate) trait MaterializationsToProto {
    fn to_proto(&self) -> cli_proto::build_request::Materializations;
}
impl MaterializationsToProto for Option<FinalArtifactMaterializations> {
    fn to_proto(&self) -> cli_proto::build_request::Materializations {
        match self {
            Some(FinalArtifactMaterializations::All) => {
                cli_proto::build_request::Materializations::Materialize
            }
            Some(FinalArtifactMaterializations::None) => {
                cli_proto::build_request::Materializations::Skip
            }
            None => cli_proto::build_request::Materializations::Default,
        }
    }
}

pub(crate) fn print_build_result(
    console: &FinalConsole,
    error_messages: &[String],
) -> anyhow::Result<()> {
    for error_message in error_messages {
        console.print_error(error_message)?;
    }
    Ok(())
}

#[async_trait]
impl StreamingCommand for BuildCommand {
    const COMMAND_NAME: &'static str = "build";

    async fn exec_impl(
        self,
        mut buckd: BuckdClientConnector,
        matches: &clap::ArgMatches,
        ctx: CommandContext,
    ) -> ExitResult {
        let ctx = ctx.client_context(&self.config_opts, matches)?;
        let result = buckd
            .with_flushing(|client| {
                client
                    .build(BuildRequest {
                        context: Some(ctx),
                        target_patterns: self
                            .patterns
                            .map(|p| buck2_data::TargetPattern { value: p.clone() }),
                        unstable_print_providers: self.print_providers,
                        build_providers: Some(BuildProviders {
                            default_info: self.default_info() as i32,
                            run_info: self.run_info() as i32,
                            test_info: self.test_info() as i32,
                        }),
                        response_options: Some(ResponseOptions {
                            return_outputs: self.show_output
                                || self.show_full_output
                                || self.show_json_output
                                || self.show_full_json_output
                                || self.output_path.is_some(),
                        }),
                        build_opts: Some(self.build_opts.to_proto()),
                        final_artifact_materializations: self.materializations.to_proto() as i32,
                    })
                    .boxed()
            })
            .await?;
        let success = match &result {
            Ok(CommandOutcome::Success(response)) => response.error_messages.is_empty(),
            Ok(CommandOutcome::Failure(_)) => false,
            Err(_) => false,
        };

        let console = self.console_opts.final_console();

        if success {
            console.print_success("BUILD SUCCEEDED")?;
        } else {
            console.print_error("BUILD FAILED")?;
        }

        // Action errors will have already been printed, but any other type
        // of error will be printed below the FAILED line here.
        let response = result??;

        print_build_result(&console, &response.error_messages)?;

        if !response.serialized_build_report.is_empty() {
            console.print_stdout(&response.serialized_build_report)?;
        }

        if !success {
            return ExitResult::failure();
        }

        if let Some(ref path) = self.output_path {
            copy_to_out(&response.build_targets, &response.project_root, path)
                .await
                .context("when requesting specific output path for --out")?;
        }

        if self.show_output
            || self.show_full_output
            || self.show_json_output
            || self.show_full_json_output
        {
            print_outputs(
                &console,
                response.build_targets,
                if self.show_full_output || self.show_full_json_output {
                    Some(response.project_root)
                } else {
                    None
                },
                self.show_json_output || self.show_full_json_output,
                false,
            )?;
        }

        ExitResult::success()
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

pub(crate) fn print_outputs(
    console: &FinalConsole,
    targets: Vec<BuildTarget>,
    root_path: Option<String>,
    as_json: bool,
    show_all_outputs: bool,
) -> anyhow::Result<()> {
    #[derive(Serialize)]
    #[serde(untagged)]
    enum TargetOutputs {
        AllOutputs(MultiMap<String, String>),
        DefaultOutput(HashMap<String, String>),
    }

    impl TargetOutputs {
        fn all_outputs() -> Self {
            Self::AllOutputs(MultiMap::new())
        }

        fn default_output() -> Self {
            Self::DefaultOutput(HashMap::new())
        }

        fn insert(&mut self, target: String, output: String) {
            match self {
                TargetOutputs::AllOutputs(map) => {
                    map.insert(target, output);
                }
                TargetOutputs::DefaultOutput(map) => {
                    map.insert(target, output);
                }
            }
        }
    }

    let mut output_map = if show_all_outputs {
        TargetOutputs::all_outputs()
    } else {
        TargetOutputs::default_output()
    };
    let mut process_output = |target: &String, output: Option<String>| -> anyhow::Result<()> {
        let output = match output {
            Some(output) => match &root_path {
                Some(root) => Path::new(&root).join(output).to_string_lossy().into_owned(),
                None => output,
            },
            None => "".to_owned(),
        };
        if as_json {
            output_map.insert(target.clone(), output);
        } else {
            console.print_stdout(&format!("{} {}", target, output))?;
        }

        Ok(())
    };

    for build_target in targets {
        // just print the default info for build command
        let outputs = build_target.outputs.into_iter().filter(|output| {
            output
                .providers
                .as_ref()
                .map_or(true, |p| show_all_outputs || (p.default_info && !p.other))
        });

        // only print the unconfigured target for now until we migrate everything to support
        // also printing configurations
        if outputs.clone().count() > 1 && !show_all_outputs {
            // We only print the default outputs when we don't `show_all_outputs`,
            // which shouldn't have more than one output.
            // (although we currently don't yet restrict this, but we should).
            process_output(&build_target.target, None)?;
            continue;
        }
        for output in outputs {
            process_output(&build_target.target, Some(output.path))?;
        }
    }

    if as_json {
        console.print_stdout(
            &serde_json::to_string_pretty(&output_map)
                .unwrap_or_else(|_| panic!("Error when converting output map to JSON.")),
        )?;
    }

    Ok(())
}

/// Given a list of targets built by this command, extracts a reasonable default output from the list and writes it
/// to the path given by `out`.
///
/// In order to extract a "reasonable default output", this function will bail if any of the following are true:
///  1. Multiple top-level targets were built, in which case the correct output to write is ambiguous,
///  2. A single top-level target was built, but it produced zero default outputs,
///  3. A single top-level target was built, but it produced more than two default outputs
///
/// Otherwise, we'll extract the single default output from the single top-level target and copy it to the output
/// path. If the given path is a directory then all output files will be copied inside of it.
async fn copy_to_out(targets: &[BuildTarget], root_path: &str, out: &Path) -> anyhow::Result<()> {
    let target = if targets.len() != 1 {
        return Err(anyhow!(
            "build command built multiple top-level targets, choice of output is ambiguous"
        ));
    } else {
        &targets[0]
    };

    let outputs: Vec<_> = target
        .outputs
        .iter()
        .filter(|output| {
            output
                .providers
                .as_ref()
                .map_or(true, |p| p.default_info && !p.other)
        })
        .collect();
    let output = match outputs.len() {
        0 => {
            return Err(anyhow!(
                "target {} produced zero default outputs",
                target.target
            ));
        }
        1 => &target.outputs[0],
        n => {
            return Err(anyhow!(
                "target {} produced {} outputs, choice of output is ambiguous",
                target.target,
                n
            ));
        }
    };

    let output_path = Path::new(root_path).join(Path::new(&output.path));
    let output_meta = tokio::fs::metadata(&output_path)
        .await
        .context("when inspecting file metadata")?;
    if output_meta.is_dir() {
        copy_directory(&output_path, out).await?;
    } else {
        let dest_path = match out.is_dir() {
            true => Cow::Owned(
                out.join(Path::new(
                    output_path
                        .file_name()
                        .context("Failed getting output name")?,
                )),
            ),
            false => Cow::Borrowed(out),
        };

        tokio::fs::copy(
            Path::new(root_path).join(Path::new(&output.path)),
            dest_path,
        )
        .await
        .map_err(|e| {
            if e.kind() == io::ErrorKind::BrokenPipe {
                anyhow::Error::new(FailureExitCode::OutputFileBrokenPipe)
            } else {
                anyhow::Error::new(e).context("when writing build artifact to --out")
            }
        })?;
    }

    Ok(())
}

/// Recursively copies a directory to the output path, rooted at `dst`.
#[async_recursion::async_recursion]
async fn copy_directory(src: &Path, dst: &Path) -> anyhow::Result<()> {
    tokio::fs::create_dir_all(dst).await?;
    let stream = tokio_stream::wrappers::ReadDirStream::new(
        tokio::fs::read_dir(src)
            .await
            .context(format!("reading directory {:?}", src))?,
    )
    .err_into::<anyhow::Error>();
    stream
        .try_for_each(|entry| async move {
            if entry.file_type().await?.is_dir() {
                copy_directory(&entry.path(), &dst.join(entry.file_name()))
                    .await
                    .context(format!("copying subdirectory {:?}", entry.path()))
            } else {
                tokio::fs::copy(&entry.path(), &dst.join(entry.file_name()))
                    .await
                    .context(format!("copying file {:?}", entry.path()))
                    .map(|_| ())
            }
        })
        .await?;

    Ok(())
}

#[cfg(test)]
mod test {
    use assert_matches::assert_matches;
    use build_providers::Action;
    use clap::Parser;

    use super::*;

    fn parse(args: &[&str]) -> anyhow::Result<BuildCommand> {
        Ok(BuildCommand::from_iter_safe(
            std::iter::once("program").chain(args.iter().copied()),
        )?)
    }

    #[test]
    fn infos_default() -> anyhow::Result<()> {
        let opts = parse(&[])?;

        assert_eq!(opts.default_info(), Action::Build);
        assert_eq!(opts.run_info(), Action::BuildIfAvailable);
        assert_eq!(opts.test_info(), Action::Skip);

        Ok(())
    }

    #[test]
    fn infos_noop() -> anyhow::Result<()> {
        let opts = parse(&[
            "--skip-test-info",
            "--build-default-info",
            "--build-run-info",
        ])?;

        assert_eq!(opts.default_info(), Action::Build);
        assert_eq!(opts.run_info(), Action::BuildIfAvailable);
        assert_eq!(opts.test_info(), Action::Skip);

        Ok(())
    }

    #[test]
    fn infos_configure() -> anyhow::Result<()> {
        let opts = parse(&["--skip-default-info"])?;
        assert_eq!(opts.default_info(), Action::Skip);

        let opts = parse(&["--skip-run-info"])?;
        assert_eq!(opts.run_info(), Action::Skip);

        let opts = parse(&["--build-test-info"])?;
        assert_eq!(opts.test_info(), Action::BuildIfAvailable);

        // Legacy flag from before we could configure the other options.
        let opts = parse(&["--build-test-dependencies"])?;
        assert_eq!(opts.test_info(), Action::BuildIfAvailable);

        Ok(())
    }

    #[test]
    fn infos_validation() -> anyhow::Result<()> {
        // Test duplicate args
        assert_matches!(
            parse(&["--build-default-info", "--skip-default-info"]),
            Err(..)
        );
        assert_matches!(parse(&["--build-run-info", "--skip-run-info"]), Err(..));
        assert_matches!(parse(&["--build-test-info", "--skip-test-info"]), Err(..));

        // Test args across all groups.
        assert_matches!(
            parse(&[
                "--skip-default-info",
                "--skip-run-info",
                "--build-test-info"
            ]),
            Ok(..)
        );

        Ok(())
    }
}
