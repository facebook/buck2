/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use cli_proto::CleanRequest;
use cli_proto::CleanResponse;
use dice::DiceTransaction;

pub async fn clean_command(
    ctx: Box<dyn ServerCommandContextTrait>,
    req: cli_proto::CleanRequest,
) -> anyhow::Result<cli_proto::CleanResponse> {
    run_server_command(CleanServerCommand { req }, ctx).await
}

struct CleanServerCommand {
    req: cli_proto::CleanRequest,
}

#[async_trait]
impl ServerCommandTemplate for CleanServerCommand {
    type StartEvent = buck2_data::CleanCommandStart;
    type EndEvent = buck2_data::CleanCommandEnd;
    type Response = cli_proto::CleanResponse;

    async fn command(
        &self,
        server_ctx: Box<dyn ServerCommandContextTrait>,
        ctx: DiceTransaction,
    ) -> anyhow::Result<Self::Response> {
        clean(server_ctx, ctx, &self.req).await
    }
}

async fn clean(
    _server_ctx: Box<dyn ServerCommandContextTrait>,
    _dice_ctx: DiceTransaction,
    _request: &CleanRequest,
) -> anyhow::Result<CleanResponse> {
    Ok(CleanResponse {
        clean_paths: vec![],
    })
}
