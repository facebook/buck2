/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Starlark debugging.

mod module;
mod package_deps;

use async_trait::async_trait;
use buck2_audit::starlark::StarlarkCommand;
use buck2_cli_proto::ClientContext;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;

use crate::AuditSubcommand;

#[async_trait]
impl AuditSubcommand for StarlarkCommand {
    async fn server_execute(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        match self {
            StarlarkCommand::Module(cmd) => {
                module::server_execute(cmd, server_ctx, stdout, client_ctx).await
            }
            StarlarkCommand::PackageDeps(cmd) => {
                package_deps::server_execute(cmd, server_ctx, stdout, client_ctx).await
            }
        }
    }
}
