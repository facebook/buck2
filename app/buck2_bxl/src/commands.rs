/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_server_ctx::bxl::BxlServerCommands;
use buck2_server_ctx::bxl::BXL_SERVER_COMMANDS;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::NoPartialResult;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;

use crate::command::bxl_command;
use crate::profile_command::bxl_profile_command;

struct BxlServerCommandsInstance;

#[async_trait]
impl BxlServerCommands for BxlServerCommandsInstance {
    async fn bxl(
        &self,
        ctx: &dyn ServerCommandContextTrait,
        partial_result_dispatcher: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        req: buck2_cli_proto::BxlRequest,
    ) -> buck2_error::Result<buck2_cli_proto::BxlResponse> {
        Ok(bxl_command(ctx, partial_result_dispatcher, req).await?)
    }

    async fn bxl_profile(
        &self,
        ctx: &dyn ServerCommandContextTrait,
        partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
        req: buck2_cli_proto::ProfileRequest,
    ) -> buck2_error::Result<buck2_cli_proto::ProfileResponse> {
        Ok(bxl_profile_command(ctx, partial_result_dispatcher, req).await?)
    }
}

pub(crate) fn init_bxl_server_commands() {
    BXL_SERVER_COMMANDS.init(&BxlServerCommandsInstance);
}
