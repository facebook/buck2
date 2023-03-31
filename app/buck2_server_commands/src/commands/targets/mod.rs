/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

mod default;
pub(crate) mod fmt;
mod resolve_alias;
mod streaming;

use std::fs::File;
use std::io::BufWriter;
use std::io::Write;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_cli_proto::targets_request;
use buck2_cli_proto::targets_request::TargetHashGraphType;
use buck2_cli_proto::HasClientContext;
use buck2_cli_proto::TargetsRequest;
use buck2_cli_proto::TargetsResponse;
use buck2_common::dice::cells::HasCellResolver;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::pattern::parse_patterns_from_cli_args;
use buck2_server_ctx::pattern::target_platform_from_client_context;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use dice::DiceTransaction;

use crate::commands::targets::default::targets_batch;
use crate::commands::targets::default::TargetHashOptions;
use crate::commands::targets::fmt::create_formatter;
use crate::commands::targets::resolve_alias::targets_resolve_aliases;
use crate::commands::targets::streaming::targets_streaming;

#[derive(Debug, thiserror::Error)]
enum TargetsCommandError {
    #[error("Missing field in proto request (internal error)")]
    MissingField,
}

pub(crate) enum Outputter {
    Stdout,
    File(BufWriter<File>),
}

impl Outputter {
    fn new(request: &TargetsRequest) -> anyhow::Result<Self> {
        match &request.output {
            None => Ok(Self::Stdout),
            Some(file) => Ok(Self::File(BufWriter::new(
                File::create(file).with_context(|| {
                    format!("Failed to open file `{}` for `targets` output ", file)
                })?,
            ))),
        }
    }

    fn write1(&mut self, stdout: &mut impl Write, x: &str) -> anyhow::Result<()> {
        match self {
            Self::Stdout => stdout.write_all(x.as_bytes())?,
            Self::File(f) => f.write_all(x.as_bytes())?,
        }
        Ok(())
    }

    fn write2(&mut self, stdout: &mut impl Write, x: &str, y: &str) -> anyhow::Result<()> {
        match self {
            Self::Stdout => {
                stdout.write_all(x.as_bytes())?;
                stdout.write_all(y.as_bytes())?;
            }
            Self::File(f) => {
                f.write_all(x.as_bytes())?;
                f.write_all(y.as_bytes())?;
            }
        }
        Ok(())
    }

    /// If this outputter should write anything to a file, do so, and return whatever buffer is left over.
    fn write_to_file(&mut self, buffer: String) -> anyhow::Result<String> {
        match self {
            Self::Stdout => Ok(buffer),
            Self::File(f) => {
                f.write_all(buffer.as_bytes())?;
                Ok(String::new())
            }
        }
    }

    fn flush(&mut self) -> anyhow::Result<()> {
        match self {
            Self::Stdout => Ok(()),
            Self::File(f) => Ok(f.flush()?),
        }
    }
}

pub async fn targets_command(
    server_ctx: Box<dyn ServerCommandContextTrait>,
    partial_result_dispatcher: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
    req: TargetsRequest,
) -> anyhow::Result<TargetsResponse> {
    run_server_command(
        TargetsServerCommand { req },
        server_ctx,
        partial_result_dispatcher,
    )
    .await
}

struct TargetsServerCommand {
    req: TargetsRequest,
}

#[async_trait]
impl ServerCommandTemplate for TargetsServerCommand {
    type StartEvent = buck2_data::TargetsCommandStart;
    type EndEvent = buck2_data::TargetsCommandEnd;
    type Response = TargetsResponse;
    type PartialResult = buck2_cli_proto::StdoutBytes;

    async fn command<'v>(
        &self,
        server_ctx: &'v dyn ServerCommandContextTrait,
        mut partial_result_dispatcher: PartialResultDispatcher<Self::PartialResult>,
        dice: DiceTransaction,
    ) -> anyhow::Result<Self::Response> {
        targets(
            server_ctx,
            &mut partial_result_dispatcher.as_writer(),
            dice,
            &self.req,
        )
        .await
    }

    fn is_success(&self, _response: &Self::Response) -> bool {
        // No response if we failed.
        true
    }
}

async fn targets(
    server_ctx: &dyn ServerCommandContextTrait,
    stdout: &mut impl Write,
    dice: DiceTransaction,
    request: &TargetsRequest,
) -> anyhow::Result<TargetsResponse> {
    // TODO(nmj): Rather than returning fully formatted data in the TargetsResponse, we should
    //            instead return structured data, and return *that* to the CLI. The CLI should
    //            then handle printing. The current approach is just a temporary hack to fix some
    //            issues with printing to stdout.

    let cwd = server_ctx.working_dir();
    let cell_resolver = dice.get_cell_resolver().await?;
    let parsed_target_patterns =
        parse_patterns_from_cli_args::<TargetPatternExtra>(&dice, &request.target_patterns, cwd)
            .await?;

    let mut outputter = Outputter::new(request)?;

    let response = match &request.targets {
        Some(targets_request::Targets::ResolveAlias(_)) => {
            targets_resolve_aliases(dice, request, parsed_target_patterns).await?
        }
        Some(targets_request::Targets::Other(other)) => {
            if other.streaming {
                let formatter = create_formatter(request, other)?;
                let hashing = match TargetHashGraphType::from_i32(other.target_hash_graph_type)
                    .expect("buck cli should send valid target hash graph type")
                {
                    TargetHashGraphType::None => None,
                    _ => Some(other.target_hash_use_fast_hash),
                };

                let res = targets_streaming(
                    server_ctx,
                    stdout,
                    dice,
                    formatter,
                    &mut outputter,
                    parsed_target_patterns,
                    other.keep_going,
                    other.cached,
                    other.imports,
                    hashing,
                )
                .await;
                // Make sure we always flush the outputter, even on failure, as we may have partially written to it
                outputter.flush()?;
                res?
            } else {
                let formatter = create_formatter(request, other)?;
                let client_ctx = request.client_context()?;
                let target_platform =
                    target_platform_from_client_context(client_ctx, server_ctx, &dice).await?;
                let fs = server_ctx.project_root();
                targets_batch(
                    server_ctx,
                    dice,
                    &*formatter,
                    parsed_target_patterns,
                    target_platform,
                    TargetHashOptions::new(other, &cell_resolver, fs)?,
                    other.keep_going,
                )
                .await?
            }
        }
        None => return Err(TargetsCommandError::MissingField.into()),
    };

    let response = TargetsResponse {
        error_count: response.error_count,
        serialized_targets_output: outputter.write_to_file(response.serialized_targets_output)?,
    };
    outputter.flush()?;
    Ok(response)
}

fn mk_error(errors: u64) -> anyhow::Error {
    // Simpler error so that we don't print long errors twice (when exiting buck2)
    let package_str = if errors == 1 { "package" } else { "packages" };
    anyhow::anyhow!("Failed to parse {} {}", errors, package_str)
}
