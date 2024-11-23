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
use buck2_audit::execution_platform_resolution::AuditExecutionPlatformResolutionCommand;
use buck2_cli_proto::ClientContext;
use buck2_node::execution::GetExecutionPlatforms;
use buck2_node::execution::EXECUTION_PLATFORMS_BUCKCONFIG;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use indent_write::io::IndentWriter;

use crate::common::configured_target_labels::audit_command_configured_target_labels;
use crate::ServerAuditSubcommand;

#[async_trait]
impl ServerAuditSubcommand for AuditExecutionPlatformResolutionCommand {
    async fn server_execute(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        mut stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        _client_ctx: ClientContext,
    ) -> buck2_error::Result<()> {
        Ok(server_ctx
            .with_dice_ctx(|server_ctx, mut ctx| async move {
                let configured_patterns = audit_command_configured_target_labels(
                    &mut ctx,
                    &self.patterns,
                    &self.target_cfg,
                    server_ctx,
                )
                .await?;

                let mut stdout = stdout.as_writer();

                match ctx.get_execution_platforms().await? {
                    None => {
                        writeln!(
                            stdout,
                            "Execution platforms are not configured: {} unset",
                            EXECUTION_PLATFORMS_BUCKCONFIG
                        )?;
                        writeln!(stdout, "Using legacy execution platform")?;
                    }
                    Some(platforms) => {
                        writeln!(
                            stdout,
                            "Checking each target against execution platforms defined by {}",
                            platforms.execution_platforms_target()
                        )?;
                    }
                }

                for configured_target in configured_patterns {
                    // This calls `get_internal_configured_target_node` rather than
                    // `get_configured_target_node` because exec platform resolution operates
                    // on `get_internal_configured_target_node`.
                    let configured_node = ctx
                        .get_internal_configured_target_node(&configured_target)
                        .await?;
                    let configured_node = configured_node.require_compatible()?;
                    writeln!(stdout, "{}:", configured_target)?;
                    let resolution = configured_node.execution_platform_resolution();
                    match resolution.platform() {
                        Ok(platform) => {
                            writeln!(stdout, "  Execution platform: {}", platform.id())?;
                            writeln!(
                                stdout,
                                "    Execution platform configuration: {}",
                                platform.cfg()
                            )?;
                            writeln!(stdout, "    Execution deps:")?;
                            for execution_dep in configured_node.exec_deps() {
                                writeln!(stdout, "      {}", execution_dep.label())?;
                            }
                            writeln!(stdout, "    Toolchain deps:")?;
                            for toolchain_dep in configured_node.toolchain_deps() {
                                writeln!(stdout, "      {}", toolchain_dep.label())?;
                            }
                            writeln!(stdout, "    Configuration deps:")?;
                            for config_dep in configured_node.configuration_deps() {
                                writeln!(stdout, "      {}", config_dep.label())?;
                            }
                            for (label, reason) in resolution.skipped() {
                                writeln!(stdout, "    Skipped {}", label)?;
                                writeln!(IndentWriter::new("      ", &mut stdout), "{:#}", reason)?;
                            }
                        }
                        Err(e) => writeln!(stdout, "{}", e)?,
                    }
                }

                Ok(())
            })
            .await?)
    }
}
