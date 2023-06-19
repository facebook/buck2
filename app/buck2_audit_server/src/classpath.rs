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
use buck2_audit::classpath::AuditClasspathCommand;
use buck2_build_api::actions::artifact::get_artifact_fs::GetArtifactFs;
use buck2_build_api::configure_targets::load_compatible_patterns;
use buck2_build_api::query::analysis::CLASSPATH_FOR_TARGETS;
use buck2_cli_proto::ClientContext;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_node::load_patterns::MissingTargetBehavior;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::pattern::parse_patterns_from_cli_args;
use buck2_server_ctx::pattern::target_platform_from_client_context;
use dupe::Dupe;
use gazebo::prelude::SliceExt;
use indexmap::IndexMap;

use crate::AuditSubcommand;

#[async_trait]
impl AuditSubcommand for AuditClasspathCommand {
    async fn server_execute(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        mut stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        server_ctx
            .with_dice_ctx(async move |server_ctx, ctx| {
                let cwd = server_ctx.working_dir();
                let parsed_patterns = parse_patterns_from_cli_args::<TargetPatternExtra>(
                    &ctx,
                    &self
                        .patterns
                        .map(|pat| buck2_data::TargetPattern { value: pat.clone() }),
                    cwd,
                )
                .await?;
                let target_platform =
                    target_platform_from_client_context(&client_ctx, server_ctx, &ctx).await?;
                // Incompatible targets are skipped because this is an audit command
                let targets = load_compatible_patterns(
                    &ctx,
                    parsed_patterns,
                    target_platform,
                    MissingTargetBehavior::Fail,
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
                                    (CLASSPATH_FOR_TARGETS.get()?)(ctx, vec![label.dupe()]).await?;
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
                    let label_to_artifact = (CLASSPATH_FOR_TARGETS.get()?)(
                        &ctx,
                        targets.into_iter().map(|t| t.label().dupe()).collect(),
                    )
                    .await?;
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
}
