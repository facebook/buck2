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
use buck2_build_api::analysis::resolve_queries;
use buck2_build_api::calculation::Calculation;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_core::target::TargetLabel;
use buck2_interpreter::dice::HasEvents;
use buck2_interpreter::pattern::TargetPattern;
use cli_proto::ClientContext;
use gazebo::prelude::*;

use crate::commands::audit::AuditSubcommand;
use crate::commands::common::CommonConfigOptions;
use crate::commands::common::CommonConsoleOptions;
use crate::commands::common::CommonEventLogOptions;
use crate::daemon::common::parse_patterns_from_cli_args;
use crate::daemon::common::resolve_patterns;
use crate::daemon::common::target_platform_from_client_context;
use crate::daemon::server::ServerCommandContext;

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(
    name = "audit-analysis-queries",
    about = "buck audit analysis resolving query attrs"
)]
pub(crate) struct AuditAnalysisQueriesCommand {
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
        mut server_ctx: ServerCommandContext,
        client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        let ctx = server_ctx.dice_ctx().await?;
        let cells = ctx.get_cell_resolver().await;

        let global_target_platform =
            target_platform_from_client_context(Some(&client_ctx), &server_ctx).await?;

        let parsed_patterns = parse_patterns_from_cli_args::<TargetPattern>(
            &self
                .patterns
                .map(|pat| buck2_data::TargetPattern { value: pat.clone() }),
            &ctx,
            &server_ctx.working_dir,
        )
        .await?;
        let resolved_pattern = resolve_patterns(&parsed_patterns, &cells, &ctx.file_ops()).await?;
        let events = ctx.per_transaction_data().get_dispatcher();

        let mut stdout = server_ctx.stdout()?;

        for (package, spec) in resolved_pattern.specs {
            match spec {
                buck2_interpreter::pattern::PackageSpec::Targets(targets) => {
                    for target in targets {
                        let label = TargetLabel::new(package.dupe(), target);
                        let configured_target = ctx
                            .get_configured_target(&label, global_target_platform.as_ref())
                            .await?;
                        let node = ctx.get_configured_target_node(&configured_target).await?;
                        let node = node.require_compatible()?;
                        let query_results = resolve_queries(&ctx, events, &node).await?;
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
                buck2_interpreter::pattern::PackageSpec::All => {
                    unimplemented!()
                }
            }
        }

        Ok(())
    }

    fn config_opts(&self) -> Option<&CommonConfigOptions> {
        None
    }

    fn console_opts(&self) -> Option<&CommonConsoleOptions> {
        None
    }

    fn event_log_opts(&self) -> Option<&CommonEventLogOptions> {
        None
    }
}
