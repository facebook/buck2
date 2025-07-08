/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use async_trait::async_trait;
use buck2_audit::classpath::AuditClasspathCommand;
use buck2_cli_proto::ClientContext;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;

use crate::ServerAuditSubcommand;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum AuditClasspathError {
    #[error(
        "Using `audit classpath` is no longer supported. Use the `[classpath]` or `[classpath_targets]` sub-targets instead."
    )]
    Deprecated,
}

#[async_trait]
impl ServerAuditSubcommand for AuditClasspathCommand {
    async fn server_execute(
        &self,
        _server_ctx: &dyn ServerCommandContextTrait,
        mut _stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        _client_ctx: ClientContext,
    ) -> buck2_error::Result<()> {
        Err(AuditClasspathError::Deprecated.into())
    }
}
