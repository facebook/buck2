/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_audit::output::parse::AuditParseCommand;
use buck2_cli_proto::ClientContext;
use buck2_common::dice::cells::HasCellResolver;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;

use super::buck_out_path_parser::BuckOutPathParser;
use super::buck_out_path_type_printer::BuckOutPathTypePrinter;
use crate::AuditSubcommand;

#[async_trait]
impl AuditSubcommand for AuditParseCommand {
    async fn server_execute(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        mut stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        _client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        server_ctx
            .with_dice_ctx(async move |_server_ctx, dice_ctx| {
                let cell_resolver = dice_ctx.get_cell_resolver().await?;
                let buck_out_parser = BuckOutPathParser::new(&cell_resolver);
                let parsed_path = buck_out_parser.parse(&self.output_path)?;

                let printer = BuckOutPathTypePrinter::new(self.json, &self.output_attribute)?;

                let stdout = stdout.as_writer();

                printer.print(&parsed_path, stdout)
            })
            .await
    }
}
