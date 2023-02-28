/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::Write;

use async_trait::async_trait;
use buck2_build_api::calculation::Calculation;
use buck2_build_api::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;
use buck2_cli_proto::ClientContext;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_core::pattern::ProvidersPattern;
use buck2_core::provider::label::ProvidersName;
use buck2_interpreter_for_build::interpreter::calculation::InterpreterCalculation;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::pattern::parse_patterns_from_cli_args;
use buck2_server_ctx::pattern::resolve_patterns;
use buck2_server_ctx::pattern::target_platform_from_client_context;
use dice::DiceTransaction;
use dupe::Dupe;
use futures::stream::FuturesOrdered;
use futures::StreamExt;
use gazebo::prelude::*;

use crate::AuditCommandCommonOptions;
use crate::AuditSubcommand;

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(
    name = "audit-providers",
    about = "prints out the providers for a target pattern"
)]
pub struct AuditProvidersCommand {
    #[clap(flatten)]
    common_opts: AuditCommandCommonOptions,

    #[clap(name = "TARGET_PATTERNS", help = "Patterns to analyze")]
    patterns: Vec<String>,

    #[clap(long, conflicts_with_all=&["list", "print-debug"])]
    quiet: bool,

    #[clap(
        long,
        short = 'l',
        help = "List the available providers", conflicts_with_all=&["print-debug", "quiet"]
    )]
    list: bool,

    #[clap(
        long = "print-debug",
        help = "Print the providers using debug format (very verbose)",
        conflicts_with_all=&["list", "quiet"]
    )]
    print_debug: bool,
}

#[async_trait]
impl AuditSubcommand for AuditProvidersCommand {
    async fn server_execute(
        &self,
        server_ctx: Box<dyn ServerCommandContextTrait>,
        stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        server_ctx
            .with_dice_ctx(move |server_ctx, ctx| {
                self.server_execute_with_dice(client_ctx, server_ctx, stdout, ctx)
            })
            .await
    }

    fn common_opts(&self) -> &AuditCommandCommonOptions {
        &self.common_opts
    }
}

#[derive(Debug, thiserror::Error)]
enum AuditProvidersError {
    #[error("Evaluation of at least one target providers failed")]
    AtLeastOneFailed,
}

impl AuditProvidersCommand {
    async fn server_execute_with_dice(
        &self,
        client_ctx: ClientContext,
        server_ctx: &dyn ServerCommandContextTrait,
        mut stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        ctx: DiceTransaction,
    ) -> anyhow::Result<()> {
        let cells = ctx.get_cell_resolver().await?;
        let target_platform = target_platform_from_client_context(
            Some(&client_ctx),
            &cells,
            server_ctx.working_dir(),
        )
        .await?;

        let parsed_patterns = parse_patterns_from_cli_args::<ProvidersPattern>(
            &self
                .patterns
                .map(|pat| buck2_data::TargetPattern { value: pat.clone() }),
            &cells,
            &ctx.get_legacy_configs().await?,
            server_ctx.working_dir(),
        )?;
        let resolved_pattern = resolve_patterns(&parsed_patterns, &cells, &ctx.file_ops()).await?;

        let mut futs = FuturesOrdered::new();
        for (package, spec) in resolved_pattern.specs {
            let ctx = &ctx;
            let targets = match spec {
                buck2_core::pattern::PackageSpec::Targets(targets) => targets,
                buck2_core::pattern::PackageSpec::All => {
                    let interpreter_results = ctx.get_interpreter_results(package.dupe()).await?;
                    interpreter_results
                        .targets()
                        .keys()
                        .map(|target| ProvidersPattern {
                            target: target.to_owned(),
                            providers: ProvidersName::Default,
                        })
                        .collect()
                }
            };

            for pattern in targets {
                let label = pattern.into_providers_label(package.dupe());
                let providers_label = ctx
                    .get_configured_target(&label, target_platform.as_ref())
                    .await?;

                // `.push` is deprecated in newer `futures`,
                // but we did not updated vendored `futures` yet.
                #[allow(deprecated)]
                futs.push(async move {
                    let result = ctx.get_providers(&providers_label).await;
                    (providers_label, result)
                });
            }
        }

        let mut stdout = stdout.as_writer();
        let mut stderr = server_ctx.stderr()?;

        fn indent(s: &str) -> String {
            s.lines().map(|line| format!("  {}\n", line)).collect()
        }

        let mut at_least_one_error = false;
        while let Some((target, result)) = futs.next().await {
            match result {
                Ok(v) => {
                    let v: FrozenProviderCollectionValue = v.require_compatible()?;

                    if self.quiet {
                        writeln!(&mut stdout, "{}", target)?
                    } else if self.list {
                        let mut provider_names = v.provider_collection().provider_names();
                        // Create a deterministic output.
                        provider_names.sort();
                        write!(
                            &mut stdout,
                            "{}:\n{}",
                            target,
                            indent(
                                &provider_names
                                    .iter()
                                    .fold(String::new(), |acc, arg| acc + &format!("- {}\n", arg))
                            )
                        )?;
                    } else if self.print_debug {
                        write!(
                            &mut stdout,
                            "{}:\n{}",
                            target,
                            indent(&format!("{:?}", v.provider_collection()))
                        )?;
                    } else {
                        write!(
                            &mut stdout,
                            "{}:\n{}",
                            target,
                            indent(&format!("{:#}", v.provider_collection()))
                        )?;
                    }
                }
                Err(e) => {
                    write!(
                        &mut stderr,
                        "{}: failed:\n{}",
                        target,
                        indent(&format!("{:?}", e))
                    )?;
                    at_least_one_error = true;
                }
            }
        }

        stdout.flush()?;
        stderr.flush()?;

        if at_least_one_error {
            Err(AuditProvidersError::AtLeastOneFailed.into())
        } else {
            Ok(())
        }
    }
}
