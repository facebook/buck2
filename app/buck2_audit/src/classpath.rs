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
use buck2_build_api::query::analysis::environment::classpath;
use buck2_build_api::query::dice::get_compatible_targets;
use buck2_cli_proto::ClientContext;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_core::target::name::TargetName;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::pattern::parse_patterns_from_cli_args;
use buck2_server_ctx::pattern::target_platform_from_client_context;
use gazebo::prelude::SliceExt;

use crate::AuditCommandCommonOptions;
use crate::AuditSubcommand;

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(
    name = "audit-classpath",
    about = "Prints out a target's classpaths if it has one.
    This command is deprecated and currently available for compatibility with buck1.
    We will replace this command with something that can audit the entire `TemplatePlaceholderInfo` in the future."
)]
pub struct AuditClasspathCommand {
    #[clap(flatten)]
    common_opts: AuditCommandCommonOptions,

    #[clap(name = "TARGET_PATTERNS", help = "Target patterns to audit")]
    patterns: Vec<String>,
    // TODO(scottcao): Add --show-targets, --json, --dot, and other relevant flags
}

#[async_trait]
impl AuditSubcommand for AuditClasspathCommand {
    async fn server_execute(
        &self,
        server_ctx: Box<dyn ServerCommandContextTrait>,
        mut stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        server_ctx
            .with_dice_ctx(async move |server_ctx, ctx| {
                let cwd = server_ctx.working_dir();
                let cell_resolver = ctx.get_cell_resolver().await?;
                let parsed_patterns = parse_patterns_from_cli_args::<TargetName>(
                    &self
                        .patterns
                        .map(|pat| buck2_data::TargetPattern { value: pat.clone() }),
                    &cell_resolver,
                    &ctx.get_legacy_configs().await?,
                    cwd,
                )?;
                let loaded_patterns = load_patterns(&ctx, parsed_patterns).await?;
                let target_platform =
                    target_platform_from_client_context(Some(&client_ctx), &cell_resolver, cwd)
                        .await?;
                // Incompatible targets are skipped because this is an audit command
                let targets = get_compatible_targets(
                    &ctx,
                    loaded_patterns.iter_loaded_targets_by_package(),
                    target_platform,
                )
                .await?;

                let label_to_artifact = classpath(&ctx, targets).await?;

                let mut stdout = stdout.as_writer();
                let artifact_fs = ctx.get_artifact_fs().await?;
                for (_label, artifact) in label_to_artifact {
                    let path = artifact_fs.resolve(artifact.get_path())?;
                    let abs_path = artifact_fs.fs().resolve(&path);
                    writeln!(stdout, "{}", &abs_path)?;
                }

                Ok(())
            })
            .await
    }

    fn common_opts(&self) -> &AuditCommandCommonOptions {
        &self.common_opts
    }
}
