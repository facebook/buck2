/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_build_api::calculation::Calculation;
use buck2_cli_proto::ClientContext;
use buck2_common::dice::cells::HasCellResolver;
use buck2_core::configuration::Configuration;
use buck2_execute::path::buck_out_path::BuckOutPathParser;
use buck2_execute::path::buck_out_path::BuckOutPathType;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;
use buck2_server_ctx::pattern::target_platform_from_client_context;
use thiserror::Error;

use crate::AuditCommandCommonOptions;
use crate::AuditSubcommand;

#[derive(Debug, Error)]
pub(crate) enum AuditOutputError {
    #[error(
        "BXL, anonymous target, test, and tmp artifacts are not supported for audit output. Only rule output artifacts are supported. Path: `{0}`"
    )]
    UnsupportedPathType(String),
    #[error(
        "Current platform does not match the configuration of the artifact path. Current platform: `{0}` with hash: `{1}`. Artifact platform hash: `{2}`"
    )]
    PlatformMismatch(Configuration, String, String),
}

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(
    name = "audit-output",
    about = "Query the action that produced the output artifact. Does not support BXL, test, scratch, or anon artifacts."
)]
pub struct AuditOutputCommand {
    #[clap(flatten)]
    common_opts: AuditCommandCommonOptions,

    #[clap(long = "json", help = "Output in JSON format")]
    json: bool,

    #[clap(
        name = "OUTPUT_PATH",
        help = "The buck-out path to the build artifact, starting with `buck-out` and including the configuration platform."
    )]
    output_path: String,
}

#[async_trait]
impl AuditSubcommand for AuditOutputCommand {
    async fn server_execute(
        &self,
        server_ctx: Box<dyn ServerCommandContextTrait>,
        client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        server_ctx
            .with_dice_ctx(async move |server_ctx, dice_ctx| {
                let working_dir = server_ctx.working_dir();
                let cell_resolver = dice_ctx.get_cell_resolver().await?;

                let buck_out_parser = BuckOutPathParser::new(&cell_resolver);

                let parsed = buck_out_parser.parse(&self.output_path)?;

                match parsed {
                    BuckOutPathType::RuleOutput {
                        target_label,
                        config_hash,
                        ..
                    } => {
                        let global_target_platform = target_platform_from_client_context(
                            Some(&client_ctx),
                            &cell_resolver,
                            working_dir,
                        )
                        .await?;

                        let configured_target_label = dice_ctx
                            .get_configured_target(&target_label, global_target_platform.as_ref())
                            .await?;

                        let command_config = configured_target_label.cfg();
                        let command_config_hash = command_config.output_hash().to_owned();
                        if !command_config_hash.eq(&config_hash) {
                            return Err(anyhow::anyhow!(AuditOutputError::PlatformMismatch(
                                command_config.clone(),
                                command_config_hash,
                                config_hash,
                            )));
                        }

                        // TODO(@wendyy) implement running analysis and looking through actions
                        Ok(())
                    }
                    _ => Err(anyhow::anyhow!(AuditOutputError::UnsupportedPathType(
                        self.output_path.to_owned()
                    ))),
                }
            })
            .await
    }

    fn common_opts(&self) -> &AuditCommandCommonOptions {
        &self.common_opts
    }
}
