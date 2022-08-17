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
use buck2_build_api::calculation::load_patterns;
use buck2_build_api::calculation::Calculation;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_core::configuration::Configuration;
use buck2_core::pattern::TargetPattern;
use cli_proto::ClientContext;
use indent_write::io::IndentWriter;

use crate::commands::audit::AuditSubcommand;
use crate::commands::common::CommonBuildConfigurationOptions;
use crate::commands::common::CommonConsoleOptions;
use crate::commands::common::CommonDaemonCommandOptions;
use crate::daemon::common::target_platform_from_client_context;
use crate::daemon::common::PatternParser;
use crate::daemon::server::ctx::ServerCommandContext;

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(
    name = "audit-execution-platform-resolution",
    about = "prints out information about execution platform resolution"
)]
pub(crate) struct AuditExecutionPlatformResolutionCommand {
    #[clap(flatten)]
    pub config_opts: CommonBuildConfigurationOptions,

    #[clap(flatten)]
    console_opts: CommonConsoleOptions,

    #[clap(flatten)]
    event_log_opts: CommonDaemonCommandOptions,

    #[clap(name = "TARGET_PATTERNS", help = "Patterns to analyze")]
    patterns: Vec<String>,
}

#[async_trait]
impl AuditSubcommand for AuditExecutionPlatformResolutionCommand {
    async fn server_execute(
        &self,
        mut server_ctx: ServerCommandContext,
        client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        let ctx = server_ctx.dice_ctx().await?;
        let cell_resolver = ctx.get_cell_resolver().await?;

        let pattern_parser = PatternParser::new(
            &cell_resolver,
            &ctx.get_legacy_configs().await?,
            &server_ctx.working_dir,
        )
        .await?;

        let mut configured_patterns = Vec::new();
        let mut target_patterns = Vec::new();
        for pat in self.patterns.iter() {
            if let Some((target, cfg_str)) = pat.split_once(' ') {
                let cfg_str = match cfg_str.strip_prefix('(').and_then(|s| s.strip_suffix(')')) {
                    Some(s) => s,
                    None => {
                        return Err(anyhow::anyhow!(
                            "Expected a configuration of the form `(//a:config-<hash>)`, but didn't see surrounding parens"
                        ));
                    }
                };
                let cfg = Configuration::lookup_from_string(cfg_str)?;
                let target = pattern_parser
                    .parse_pattern::<TargetPattern>(target)?
                    .as_target_label(target)?;
                configured_patterns.push(target.configure(cfg));
            } else {
                target_patterns.push(pattern_parser.parse_pattern::<TargetPattern>(pat)?);
            }
        }

        let loaded_patterns = load_patterns(&ctx, target_patterns).await?;
        let target_platform = target_platform_from_client_context(
            Some(&client_ctx),
            &cell_resolver,
            &server_ctx.working_dir,
        )
        .await?;

        for (_, targets) in loaded_patterns.into_iter() {
            for (_, node) in targets? {
                configured_patterns.push(
                    ctx.get_configured_target(node.label(), target_platform.as_ref())
                        .await?,
                );
            }
        }

        let mut stdout = server_ctx.stdout()?;

        for configured_target in configured_patterns {
            let configured_node = ctx.get_configured_target_node(&configured_target).await?;
            let configured_node = configured_node.require_compatible()?;
            writeln!(stdout, "{}:", configured_target)?;
            let resolution = configured_node.execution_platform_resolution();
            match resolution.platform() {
                Ok(platform) => {
                    writeln!(stdout, "  Execution platform: {}", platform.cfg())?;
                    writeln!(stdout, "    Execution deps:")?;
                    for execution_dep in configured_node.exec_deps() {
                        writeln!(stdout, "      {}", execution_dep.name())?;
                    }
                    writeln!(stdout, "    Toolchain deps:")?;
                    for toolchain_dep in configured_node.toolchain_deps() {
                        writeln!(stdout, "      {}", toolchain_dep.name())?;
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
