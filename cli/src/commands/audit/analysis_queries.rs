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
use buck2_build_api::analysis::calculation::resolve_queries;
use buck2_build_api::calculation::Calculation;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_core::pattern::TargetPattern;
use buck2_core::target::TargetLabel;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::pattern::parse_patterns_from_cli_args;
use buck2_server_ctx::pattern::resolve_patterns;
use buck2_server_ctx::pattern::target_platform_from_client_context;
use cli_proto::ClientContext;
use gazebo::prelude::*;

use crate::commands::audit::AuditSubcommand;
use crate::commands::common::CommonBuildConfigurationOptions;
use crate::commands::common::CommonConsoleOptions;
use crate::commands::common::CommonDaemonCommandOptions;

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(
    name = "audit-analysis-queries",
    about = "buck audit analysis resolving query attrs"
)]
pub(crate) struct AuditAnalysisQueriesCommand {
    #[clap(flatten)]
    config_opts: CommonBuildConfigurationOptions,

    #[clap(flatten)]
    console_opts: CommonConsoleOptions,

    #[clap(flatten)]
    event_log_opts: CommonDaemonCommandOptions,

    #[clap(
        name = "TARGET_PATTERNS",
        help = "Patterns to evaluate. The query attributes for targets matching these patterns will be evaluated"
    )]
    patterns: Vec<String>,

    #[clap(
        long,
        help = "Enable to print the outputs for the targets in the resolved queries"
    )]
    include_outputs: bool,
}

#[async_trait]
impl AuditSubcommand for AuditAnalysisQueriesCommand {
    async fn server_execute(
        &self,
        mut server_ctx: Box<dyn ServerCommandContextTrait>,
        client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        let ctx = server_ctx.dice_ctx().await?;
        let cells = ctx.get_cell_resolver().await?;

        let global_target_platform = target_platform_from_client_context(
            Some(&client_ctx),
            &cells,
            server_ctx.working_dir(),
        )
        .await?;

        let parsed_patterns = parse_patterns_from_cli_args::<TargetPattern>(
            &self
                .patterns
                .map(|pat| buck2_data::TargetPattern { value: pat.clone() }),
            &cells,
            &ctx.get_legacy_configs().await?,
            server_ctx.working_dir(),
        )?;
        let resolved_pattern = resolve_patterns(&parsed_patterns, &cells, &ctx.file_ops()).await?;

        let mut stdout = server_ctx.stdout()?;

        for (package, spec) in resolved_pattern.specs {
            match spec {
                buck2_core::pattern::PackageSpec::Targets(targets) => {
                    for target in targets {
                        let label = TargetLabel::new(package.dupe(), target);
                        let configured_target = ctx
                            .get_configured_target(&label, global_target_platform.as_ref())
                            .await?;
                        let node = ctx.get_configured_target_node(&configured_target).await?;
                        let node = node.require_compatible()?;
                        let query_results = resolve_queries(&ctx, &node).await?;
                        writeln!(stdout, "{}:", label)?;
                        for (query, result) in &query_results {
                            writeln!(stdout, "  {}", query)?;
                            for (target, providers) in &**result {
                                writeln!(stdout, "    {}", target.unconfigured())?;
                                if self.include_outputs {
                                    let outputs = providers
                                        .provider_collection()
                                        .default_info()
                                        .default_outputs_raw();
                                    writeln!(stdout, "        {}", outputs)?;
                                }
                            }
                        }
                    }
                }
                buck2_core::pattern::PackageSpec::All => {
                    unimplemented!()
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
