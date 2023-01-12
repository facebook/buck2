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
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use dice::DiceTransaction;

use crate::ctx::ServerCommandContext;

pub(crate) async fn file_status_command(
    ctx: ServerCommandContext,
    req: cli_proto::FileStatusRequest,
) -> anyhow::Result<cli_proto::GenericResponse> {
    run_server_command(FileStatusServerCommand { req }, box ctx).await
}
struct FileStatusServerCommand {
    req: cli_proto::FileStatusRequest,
}

#[async_trait]
impl ServerCommandTemplate for FileStatusServerCommand {
    type StartEvent = buck2_data::FileStatusCommandStart;
    type EndEvent = buck2_data::FileStatusCommandEnd;
    type Response = cli_proto::GenericResponse;

    async fn command<'v>(
        &self,
        server_ctx: &'v dyn ServerCommandContextTrait,
        _ctx: DiceTransaction,
    ) -> anyhow::Result<Self::Response> {
        let mut stderr = server_ctx.stderr()?;
        for path in &self.req.paths {
            writeln!(stderr, "Check path: {}", path)?;
        }
        Ok(cli_proto::GenericResponse {})
    }

    fn is_success(&self, _response: &Self::Response) -> bool {
        // No response if we failed.
        true
    }
}
