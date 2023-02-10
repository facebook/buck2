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
use buck2_build_api::interpreter::context::prelude_path;
use buck2_cli_proto::ClientContext;
use buck2_common::dice::cells::HasCellResolver;
use buck2_interpreter::global_interpreter_state::HasGlobalInterpreterState;
use buck2_interpreter::path::StarlarkModulePath;
use buck2_interpreter_for_build::interpreter::dice_calculation_delegate::HasCalculationDelegate;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;

use crate::AuditCommandCommonOptions;
use crate::AuditSubcommand;

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(
    name = "audit-prelude",
    about = "print the interpreter prelude to stdout"
)]
pub struct AuditPreludeCommand {
    #[clap(flatten)]
    common_opts: AuditCommandCommonOptions,
}

#[async_trait]
impl AuditSubcommand for AuditPreludeCommand {
    async fn server_execute(
        &self,
        server_ctx: Box<dyn ServerCommandContextTrait>,
        _client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        server_ctx
            .with_dice_ctx(async move |server_ctx, ctx| {
                let mut stdout = server_ctx.stdout()?;
                // Print out all the Prelude-like stuff that is loaded into each module
                let global_interpreter_state = ctx.get_global_interpreter_state().await?;
                let cell_resolver = ctx.get_cell_resolver().await?;
                let cell_alias_resolver = cell_resolver.root_cell_instance().cell_alias_resolver();
                let prelude_path = prelude_path(cell_alias_resolver)?;
                let interpreter_calculation = ctx
                    .get_interpreter_calculator(prelude_path.cell(), prelude_path.build_file_cell())
                    .await?;
                writeln!(
                    stdout,
                    "{}",
                    global_interpreter_state.build_file_global_env.describe()
                )?;
                writeln!(
                    stdout,
                    "{}",
                    interpreter_calculation
                        .eval_module(StarlarkModulePath::LoadFile(&prelude_path))
                        .await?
                        .env()
                        .describe()
                )?;

                Ok(())
            })
            .await
    }

    fn common_opts(&self) -> &AuditCommandCommonOptions {
        &self.common_opts
    }
}
