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
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::pattern::parse_patterns_from_cli_args;
use buck2_server_ctx::pattern::target_platform_from_client_context;
use dupe::Dupe;
use gazebo::prelude::SliceExt;
use indexmap::IndexMap;

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

    /// Output in JSON format
    #[clap(long)]
    json: bool,
    // TODO(scottcao): Add --show-targets, --dot, and other relevant flags
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
                let parsed_patterns = parse_patterns_from_cli_args::<TargetPatternExtra>(
                    &ctx,
                    &self
                        .patterns
                        .map(|pat| buck2_data::TargetPattern { value: pat.clone() }),
                    cwd,
                )
                .await?;
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

                let mut stdout = stdout.as_writer();
                let artifact_fs = ctx.get_artifact_fs().await?;

                // Json prints a map of targets to list of classpaths while default prints
                // classpaths for all targets.
                if self.json {
                    let target_to_artifacts =
                        futures::future::try_join_all(targets.into_iter().map(|target| {
                            let ctx = &ctx;
                            async move {
                                let label = target.label().dupe();
                                let label_to_artifact =
                                    classpath(ctx, std::iter::once(target)).await?;
                                anyhow::Ok((label, label_to_artifact))
                            }
                        }))
                        .await?;
                    let target_to_classpaths: anyhow::Result<IndexMap<_, _>> = target_to_artifacts
                        .into_iter()
                        .map(|(target, label_to_artifact)| {
                            let classpaths: anyhow::Result<Vec<_>> = label_to_artifact
                                .into_values()
                                .map(|artifact| {
                                    let path = artifact.get_path().resolve(&artifact_fs)?;
                                    anyhow::Ok(artifact_fs.fs().resolve(&path))
                                })
                                .collect();
                            // Note: We are choosing unconfigured targets here to match buck1 behavior.
                            // This means that if same unconfigured target with different configurations are audited,
                            // one will override the other in the output.
                            // The replacement for this command in the future should return configured targets.
                            anyhow::Ok((target.unconfigured().dupe(), classpaths?))
                        })
                        .collect();
                    writeln!(
                        stdout,
                        "{}",
                        serde_json::to_string_pretty(&target_to_classpaths?)?
                    )?;
                } else {
                    let label_to_artifact = classpath(&ctx, targets.into_iter()).await?;
                    for (_label, artifact) in label_to_artifact {
                        let path = artifact.get_path().resolve(&artifact_fs)?;
                        let abs_path = artifact_fs.fs().resolve(&path);
                        writeln!(stdout, "{}", &abs_path)?;
                    }
                }

                Ok(())
            })
            .await
    }

    fn common_opts(&self) -> &AuditCommandCommonOptions {
        &self.common_opts
    }
}
