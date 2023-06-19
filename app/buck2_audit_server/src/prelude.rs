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
use buck2_audit::prelude::AuditPreludeCommand;
use buck2_cli_proto::ClientContext;
use buck2_common::dice::cells::HasCellResolver;
use buck2_interpreter::file_type::StarlarkFileType;
use buck2_interpreter::load_module::InterpreterCalculation;
use buck2_interpreter::load_module::INTERPRETER_CALCULATION_IMPL;
use buck2_interpreter::prelude_path::prelude_path;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;

use crate::AuditSubcommand;

#[async_trait]
impl AuditSubcommand for AuditPreludeCommand {
    async fn server_execute(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        mut stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        _client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        server_ctx
            .with_dice_ctx(async move |_server_ctx, ctx| {
                let mut stdout = stdout.as_writer();
                // Print out all the Prelude-like stuff that is loaded into each module
                let cell_resolver = ctx.get_cell_resolver().await?;
                let cell_alias_resolver = cell_resolver.root_cell_instance().cell_alias_resolver();
                let prelude_path = prelude_path(cell_alias_resolver)?;
                writeln!(
                    stdout,
                    "{}",
                    INTERPRETER_CALCULATION_IMPL
                        .get()?
                        .global_env_for_file_type(&ctx, StarlarkFileType::Buck)
                        .await?
                        .describe()
                )?;
                writeln!(
                    stdout,
                    "{}",
                    ctx.get_loaded_module_from_import_path(&prelude_path)
                        .await?
                        .env()
                        .describe()
                )?;

                Ok(())
            })
            .await
    }
}
