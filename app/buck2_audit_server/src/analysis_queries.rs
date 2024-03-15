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
use buck2_analysis::analysis::calculation::resolve_queries;
use buck2_audit::analysis_queries::AuditAnalysisQueriesCommand;
use buck2_cli_proto::ClientContext;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_core::target::label::TargetLabel;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::pattern::parse_and_resolve_patterns_from_cli_args;
use dupe::Dupe;
use gazebo::prelude::*;

use crate::target_resolution_config::audit_command_target_resolution_config;
use crate::ServerAuditSubcommand;

#[async_trait]
impl ServerAuditSubcommand for AuditAnalysisQueriesCommand {
    async fn server_execute(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        mut stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        _client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        server_ctx
            .with_dice_ctx(async move |server_ctx, mut ctx| {
                let target_resolution_config =
                    audit_command_target_resolution_config(&mut ctx, &self.target_cfg, server_ctx)
                        .await?;

                let resolved_pattern =
                    parse_and_resolve_patterns_from_cli_args::<TargetPatternExtra>(
                        &mut ctx,
                        &self
                            .patterns
                            .map(|pat| buck2_data::TargetPattern { value: pat.clone() }),
                        server_ctx.working_dir(),
                    )
                    .await?;

                let mut stdout = stdout.as_writer();

                for (package, spec) in resolved_pattern.specs {
                    match spec {
                        buck2_core::pattern::PackageSpec::Targets(targets) => {
                            for (target, TargetPatternExtra) in targets {
                                let label = TargetLabel::new(package.dupe(), target.as_ref());
                                for configured_target in target_resolution_config
                                    .get_configured_target(&mut ctx, &label)
                                    .await?
                                {
                                    let node =
                                        ctx.get_configured_target_node(&configured_target).await?;
                                    let node = node.require_compatible()?;
                                    let query_results =
                                        resolve_queries(&mut ctx, node.as_ref()).await?;
                                    writeln!(stdout, "{}:", label)?;
                                    for (query, result) in &query_results {
                                        writeln!(stdout, "  {}", query)?;
                                        for (target, providers) in &result.result {
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
                        }
                        buck2_core::pattern::PackageSpec::All => {
                            unimplemented!()
                        }
                    }
                }

                Ok(())
            })
            .await
    }
}
