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
use buck2_build_api::{
    calculation::Calculation, interpreter::rule_defs::provider::FrozenProviderCollectionValue,
};
use buck2_common::dice::{cells::HasCellResolver, file_ops::HasFileOps};
use buck2_core::{
    provider::{ProvidersLabel, ProvidersName},
    target::TargetLabel,
};
use buck2_interpreter::pattern::ProvidersPattern;
use cli_proto::ClientContext;
use futures::{stream::FuturesOrdered, StreamExt};
use gazebo::prelude::*;
use structopt::StructOpt;

use crate::{
    commands::{
        audit::AuditSubcommand,
        common::{CommonConfigOptions, CommonConsoleOptions, CommonEventLogOptions},
    },
    daemon::{
        common::{
            parse_patterns_from_cli_args, resolve_patterns, target_platform_from_client_context,
        },
        server::ServerCommandContext,
    },
};

#[derive(Debug, StructOpt, serde::Serialize, serde::Deserialize)]
#[structopt(
    name = "audit-providers",
    about = "prints out the providers for a target pattern"
)]
pub struct AuditProvidersCommand {
    #[structopt(flatten)]
    pub config_opts: CommonConfigOptions,

    #[structopt(flatten)]
    console_opts: CommonConsoleOptions,

    #[structopt(flatten)]
    event_log_opts: CommonEventLogOptions,

    #[structopt(name = "TARGET_PATTERNS", help = "Patterns to analyze")]
    patterns: Vec<String>,

    #[structopt(long)]
    quiet: bool,

    #[structopt(
        long = "print-debug",
        help = "Print the providers using debug format (very verbose)"
    )]
    print_debug: bool,
}

#[async_trait]
impl AuditSubcommand for AuditProvidersCommand {
    async fn server_execute(
        &self,
        mut server_ctx: ServerCommandContext,
        client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        let ctx = server_ctx.dice_ctx().await?;
        let cells = ctx.get_cell_resolver().await;
        let target_platform =
            target_platform_from_client_context(Some(&client_ctx), &server_ctx).await?;

        let parsed_patterns = parse_patterns_from_cli_args::<ProvidersPattern>(
            &self
                .patterns
                .map(|pat| buck2_data::TargetPattern { value: pat.clone() }),
            &ctx,
            &server_ctx.working_dir,
        )
        .await?;
        let resolved_pattern = resolve_patterns(&parsed_patterns, &cells, &ctx.file_ops()).await?;

        let mut futs = FuturesOrdered::new();
        for (package, spec) in resolved_pattern.specs {
            let ctx = &ctx;
            let targets = match spec {
                buck2_interpreter::pattern::PackageSpec::Targets(targets) => targets,
                buck2_interpreter::pattern::PackageSpec::All => {
                    let interpreter_results = ctx.get_interpreter_results(&package).await?;
                    interpreter_results
                        .targets()
                        .keys()
                        .duped()
                        .map(|t| (t, ProvidersName::Default))
                        .collect()
                }
            };

            for (target, providers_name) in targets {
                let label =
                    ProvidersLabel::new(TargetLabel::new(package.dupe(), target), providers_name);
                let providers_label = ctx
                    .get_configured_target(&label, target_platform.as_ref())
                    .await?;

                futs.push(async move {
                    let result = ctx.get_providers(&providers_label).await;
                    (providers_label, result)
                });
            }
        }

        let mut stdout = server_ctx.stdout()?;

        while let Some((target, result)) = futs.next().await {
            match result {
                Ok(v) => {
                    let v: FrozenProviderCollectionValue = v.require_compatible()?;

                    if self.quiet {
                        writeln!(&mut stdout, "{}", target)?
                    } else if self.print_debug {
                        writeln!(&mut stdout, "{}:\n{:?}\n", target, v.provider_collection())?;
                    } else {
                        writeln!(&mut stdout, "{}:\n{:#}\n", target, v.provider_collection())?;
                    }
                }
                Err(e) => {
                    if self.quiet {
                        crate::eprintln!("{}: failed: {:#}", target, e)?;
                    } else {
                        crate::eprintln!("{}: \n{:#}", target, e)?;
                    }
                }
            }
        }

        Ok(())
    }

    fn config_opts(&self) -> Option<&CommonConfigOptions> {
        Some(&self.config_opts)
    }

    fn console_opts(&self) -> Option<&CommonConsoleOptions> {
        Some(&self.console_opts)
    }

    fn event_log_opts(&self) -> Option<&CommonEventLogOptions> {
        Some(&self.event_log_opts)
    }
}
