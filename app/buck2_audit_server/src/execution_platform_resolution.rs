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
use buck2_core::configuration::bound_id::BoundConfigurationId;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::pattern::pattern_type::ConfigurationPredicate;
use buck2_core::pattern::pattern_type::ConfiguredTargetPatternExtra;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use buck2_node::target_calculation::ConfiguredTargetCalculation;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::pattern::global_cfg_options_from_client_context;
use buck2_server_ctx::pattern::parse_and_resolve_patterns_to_targets_from_cli_args;
use gazebo::prelude::SliceExt;
use indent_write::io::IndentWriter;

use crate::AuditSubcommand;

#[derive(Debug, buck2_error::Error)]
enum AuditExecutionPlatformResolutionCommandError {
    #[error("Builtin configurations are not supported: `{0}`")]
    BuiltinConfigurationsNotSupported(String),
    #[error(
        "Patterns with configuration label without configuration hash are not supported: `{0}`"
    )]
    ConfigurationLabelWithoutHashNotSupported(String),
}

#[async_trait]
impl AuditSubcommand for AuditExecutionPlatformResolutionCommand {
    async fn server_execute(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        mut stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        server_ctx.with_dice_ctx(
            async move |server_ctx, mut ctx| {
                let targets = parse_and_resolve_patterns_to_targets_from_cli_args::<ConfiguredTargetPatternExtra>(
                    &mut ctx,
                    &self
                        .patterns
                        .map(|pat| buck2_data::TargetPattern { value: pat.clone() }),
                    server_ctx.working_dir(),
                )
                    .await?;

                let global_cfg_options = global_cfg_options_from_client_context(
                    &client_ctx,
                    server_ctx,
                    &mut ctx,
                )
                    .await?;

                let mut configured_patterns: Vec<ConfiguredTargetLabel> = Vec::new();
                for (target_label, extra) in targets {
                    match extra.cfg {
                        ConfigurationPredicate::Any => {
                            configured_patterns.push(
                                ctx.get_configured_target(&target_label, &global_cfg_options)
                                    .await?,
                            );
                        },
                        ConfigurationPredicate::Builtin(p) => {
                            return Err(AuditExecutionPlatformResolutionCommandError::BuiltinConfigurationsNotSupported(p.to_string()).into())
                        }
                        ConfigurationPredicate::Bound(label, None) => {
                            return Err(AuditExecutionPlatformResolutionCommandError::ConfigurationLabelWithoutHashNotSupported(label.to_string()).into())
                        }
                        ConfigurationPredicate::Bound(label, Some(hash)) => {
                            let cfg = ConfigurationData::lookup_bound(BoundConfigurationId { label, hash })?;
                            configured_patterns.push(target_label.configure(cfg));
                        }
                    }
                }

                let mut stdout = stdout.as_writer();

                for configured_target in configured_patterns {
                    let configured_node = ctx.get_configured_target_node(&configured_target).await?;
                    let configured_node = configured_node.require_compatible()?;
                    writeln!(stdout, "{}:", configured_target)?;
                    let resolution = configured_node.execution_platform_resolution();
                    match resolution.platform() {
                        Ok(platform) => {
                            writeln!(stdout, "  Execution platform: {}", platform.id())?;
                            writeln!(stdout, "    Execution platform configuration: {}", platform.cfg())?;
                            writeln!(stdout, "    Execution deps:")?;
                            for execution_dep in configured_node.exec_deps() {
                                writeln!(stdout, "      {}", execution_dep.label())?;
                            }
                            writeln!(stdout, "    Toolchain deps:")?;
                            for toolchain_dep in configured_node.toolchain_deps() {
                                writeln!(stdout, "      {}", toolchain_dep.label())?;
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
        .await
    }
}
