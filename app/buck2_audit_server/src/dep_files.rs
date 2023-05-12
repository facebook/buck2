/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_audit::dep_files::AuditDepFilesCommand;
use buck2_build_api::audit_dep_files::AUDIT_DEP_FILES;
use buck2_build_api::calculation::Calculation;
use buck2_cli_proto::ClientContext;
use buck2_core::category::Category;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::pattern::parse_patterns_from_cli_args;
use buck2_server_ctx::pattern::target_platform_from_client_context;

use crate::AuditSubcommand;

#[async_trait]
impl AuditSubcommand for AuditDepFilesCommand {
    async fn server_execute(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        mut stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        server_ctx
            .with_dice_ctx(async move |server_ctx, ctx| {
                let target_platform =
                    target_platform_from_client_context(&client_ctx, server_ctx, &ctx).await?;

                let label = parse_patterns_from_cli_args::<TargetPatternExtra>(
                    &ctx,
                    &[buck2_data::TargetPattern {
                        value: self.pattern.clone(),
                    }],
                    server_ctx.working_dir(),
                )
                .await?
                .into_iter()
                .next()
                .context("Parsing patterns returned nothing")?
                .as_target_label(&self.pattern)?;

                let label = ctx
                    .get_configured_target(&label, target_platform.as_ref())
                    .await?;

                let category = Category::try_from(self.category.as_str())?;

                (AUDIT_DEP_FILES.get()?)(
                    &ctx,
                    label,
                    category,
                    self.identifier.clone(),
                    &mut stdout.as_writer(),
                )
                .await?;

                Ok(())
            })
            .await
    }
}
