/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(async_closure)]
#![feature(try_blocks)]
#![feature(provide_any)]

use async_trait::async_trait;
use buck2_audit::AuditCommand;
use buck2_cli_proto::ClientContext;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;

mod analysis_queries;
mod cell;
mod classpath;
mod config;
mod configurations;
pub mod deferred_materializer;
mod dep_files;
mod execution_platform_resolution;
mod includes;
pub mod output;
mod prelude;
mod providers;
pub mod server;
mod starlark;
mod visibility;

/// `buck2 audit` subcommands have a somewhat unique approach to make it really easy to
/// add them without the boilerplate necessary for normal commands. The main difference
/// is that there is not a custom endpoint added in the daemon for each subcommand, instead
/// there is a single endpoint where we send the entire serialized AuditCommand and
/// reproduce it in the daemon.
///
/// Audit subcommands implement this trait so that we can handle the entire client side
/// logic here and to support that serialization to the daemon.
#[async_trait]
pub trait AuditSubcommand: Send + Sync + 'static {
    async fn server_execute(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        client_server_ctx: ClientContext,
    ) -> anyhow::Result<()>;
}

#[async_trait]
pub trait AuditCommandExt {
    async fn server_execute(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        client_server_ctx: ClientContext,
    ) -> anyhow::Result<()>;
    fn as_subcommand(&self) -> &dyn AuditSubcommand;
}

#[async_trait]
impl AuditCommandExt for AuditCommand {
    async fn server_execute(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        client_server_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        self.as_subcommand()
            .server_execute(server_ctx, stdout, client_server_ctx)
            .await
    }
    fn as_subcommand(&self) -> &dyn AuditSubcommand {
        match self {
            AuditCommand::Cell(cmd) => cmd,
            AuditCommand::Classpath(cmd) => cmd,
            AuditCommand::Config(cmd) => cmd,
            AuditCommand::Configurations(cmd) => cmd,
            AuditCommand::Includes(cmd) => cmd,
            AuditCommand::Prelude(cmd) => cmd,
            AuditCommand::Providers(cmd) => cmd,
            AuditCommand::AnalysisQueries(cmd) => cmd,
            AuditCommand::ExecutionPlatformResolution(cmd) => cmd,
            AuditCommand::Starlark(cmd) => cmd,
            AuditCommand::DepFiles(cmd) => cmd,
            AuditCommand::DeferredMaterializer(cmd) => cmd,
            AuditCommand::Visibility(cmd) => cmd,
            AuditCommand::Output(cmd) => cmd,
        }
    }
}
