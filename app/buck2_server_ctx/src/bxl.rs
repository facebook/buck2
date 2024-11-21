/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_util::late_binding::LateBinding;

use crate::ctx::ServerCommandContextTrait;
use crate::partial_result_dispatcher::NoPartialResult;
use crate::partial_result_dispatcher::PartialResultDispatcher;

#[async_trait]
pub trait BxlServerCommands: Send + Sync + 'static {
    async fn bxl(
        &self,
        ctx: &dyn ServerCommandContextTrait,
        partial_result_dispatcher: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        req: buck2_cli_proto::BxlRequest,
    ) -> buck2_error::Result<buck2_cli_proto::BxlResponse>;
    async fn bxl_profile(
        &self,
        ctx: &dyn ServerCommandContextTrait,
        partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
        req: buck2_cli_proto::ProfileRequest,
    ) -> buck2_error::Result<buck2_cli_proto::ProfileResponse>;
}

pub static BXL_SERVER_COMMANDS: LateBinding<&'static dyn BxlServerCommands> =
    LateBinding::new("BXL_SERVER_COMMANDS");
