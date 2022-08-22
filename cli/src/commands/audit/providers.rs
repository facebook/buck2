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
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_core::pattern::ProvidersPattern;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_core::target::TargetLabel;
use buck2_server::ctx::ServerCommandContext;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::pattern::parse_patterns_from_cli_args;
use buck2_server_ctx::pattern::resolve_patterns;
use buck2_server_ctx::pattern::target_platform_from_client_context;
use cli_proto::ClientContext;
use futures::stream::FuturesOrdered;
use futures::StreamExt;
use gazebo::prelude::*;

use crate::commands::audit::AuditSubcommand;
use crate::commands::common::CommonBuildConfigurationOptions;
use crate::commands::common::CommonConsoleOptions;
use crate::commands::common::CommonDaemonCommandOptions;

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(
    name = "audit-providers",
    about = "prints out the providers for a target pattern"
)]
pub(crate) struct AuditProvidersCommand {
    #[clap(flatten)]
    pub config_opts: CommonBuildConfigurationOptions,

    #[clap(flatten)]
    console_opts: CommonConsoleOptions,

    #[clap(flatten)]
    event_log_opts: CommonDaemonCommandOptions,

    #[clap(name = "TARGET_PATTERNS", help = "Patterns to analyze")]
    patterns: Vec<String>,

    #[clap(long)]
    quiet: bool,

    #[clap(
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
        let cells = ctx.get_cell_resolver().await?;
        let target_platform =
            target_platform_from_client_context(Some(&client_ctx), &cells, &server_ctx.working_dir)
                .await?;

        let parsed_patterns = parse_patterns_from_cli_args::<ProvidersPattern>(
            &self
                .patterns
                .map(|pat| buck2_data::TargetPattern { value: pat.clone() }),
            &cells,
            &ctx.get_legacy_configs().await?,
            &server_ctx.working_dir,
        )?;
        let resolved_pattern = resolve_patterns(&parsed_patterns, &cells, &ctx.file_ops()).await?;

        let mut futs = FuturesOrdered::new();
        for (package, spec) in resolved_pattern.specs {
            let ctx = &ctx;
            let targets = match spec {
                buck2_core::pattern::PackageSpec::Targets(targets) => targets,
                buck2_core::pattern::PackageSpec::All => {
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

    fn config_opts(&self) -> Option<&CommonBuildConfigurationOptions> {
        Some(&self.config_opts)
    }

    fn console_opts(&self) -> Option<&CommonConsoleOptions> {
        Some(&self.console_opts)
    }

    fn event_log_opts(&self) -> Option<&CommonDaemonCommandOptions> {
        Some(&self.event_log_opts)
    }
}
