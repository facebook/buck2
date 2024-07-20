/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::PathBuf;
use std::time::Instant;

use buck2_cli_proto::new_generic::CompleteRequest;
use buck2_cli_proto::new_generic::NewGenericRequest;
use buck2_cli_proto::new_generic::NewGenericResponse;
use buck2_cli_proto::ClientContext;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::command_outcome::CommandOutcome;
use buck2_client_ctx::common::target_cfg::TargetCfgOptions;
use buck2_client_ctx::common::ui::CommonConsoleOptions;
use buck2_client_ctx::common::CommonBuildConfigurationOptions;
use buck2_client_ctx::common::CommonEventLogOptions;
use buck2_client_ctx::common::CommonStarlarkOptions;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::daemon::client::FlushingBuckdClient;
use buck2_client_ctx::exit_result::ExitCode;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::streaming::StreamingCommand;
use clap::ArgMatches;
use futures::future::BoxFuture;
use futures::FutureExt;
use tokio::time;

use super::pattern;

type CompleteCallback = fn(CommandOutcome<Vec<String>>) -> ExitResult;

pub(crate) struct CompleteTargetCommand {
    target_cfg: TargetCfgOptions,

    cwd: PathBuf,
    package: String,
    partial_target: String,

    deadline: Instant,
    callback: CompleteCallback,
}

impl CompleteTargetCommand {
    pub(crate) fn new(
        cwd: PathBuf,
        package: String,
        partial_target: String,
        deadline: Instant,
        callback: CompleteCallback,
    ) -> Self {
        let target_cfg = TargetCfgOptions::default();
        Self {
            target_cfg,
            cwd,
            package,
            partial_target,
            deadline,
            callback,
        }
    }
}

#[async_trait::async_trait]
impl StreamingCommand for CompleteTargetCommand {
    const COMMAND_NAME: &'static str = "complete";

    async fn exec_impl(
        self,
        buckd: &mut BuckdClientConnector,
        matches: &ArgMatches,
        ctx: &mut ClientCommandContext<'_>,
    ) -> ExitResult {
        let buckd_client = buckd.with_flushing();
        let context = ctx.client_context(matches, &self)?;
        let mut resolver = DaemonTargetResolver {
            buckd_client,
            context,
            target_cfg: self.target_cfg,
        };

        let completer =
            pattern::TargetCompleter::new(self.cwd.as_path(), &ctx.paths()?.roots, &mut resolver)
                .await
                .expect("Failed to create target completer");
        let task = completer.complete(&self.package, &self.partial_target);

        let remaining_time = self.deadline.saturating_duration_since(Instant::now());
        match time::timeout(remaining_time, task).await {
            Ok(CommandOutcome::Success(completions)) => {
                (self.callback)(CommandOutcome::Success(completions))
            }
            Ok(CommandOutcome::Failure(err)) => err,
            Err(_) => ExitResult::status(ExitCode::Timeout),
        }
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        CommonConsoleOptions::none_ref()
    }

    fn event_log_opts(&self) -> &CommonEventLogOptions {
        CommonEventLogOptions::default_ref()
    }

    fn build_config_opts(&self) -> &CommonBuildConfigurationOptions {
        CommonBuildConfigurationOptions::reuse_current_config_ref()
    }

    fn starlark_opts(&self) -> &CommonStarlarkOptions {
        CommonStarlarkOptions::default_ref()
    }
}

struct DaemonTargetResolver<'a, 'b> {
    buckd_client: FlushingBuckdClient<'a, 'b>,
    context: ClientContext,
    target_cfg: TargetCfgOptions,
}

impl<'a, 'b> pattern::TargetResolver for DaemonTargetResolver<'a, 'b> {
    fn resolve(&mut self, partial_target: String) -> BoxFuture<CommandOutcome<Vec<String>>> {
        let request = NewGenericRequest::Complete(CompleteRequest {
            target_cfg: self.target_cfg.target_cfg(),
            partial_target,
        });
        self.buckd_client
            .new_generic(self.context.clone(), request, None)
            .then(|res| async move {
                match res {
                    Ok(CommandOutcome::Success(NewGenericResponse::Complete(res))) => {
                        CommandOutcome::Success(res.completions)
                    }
                    Ok(CommandOutcome::Success(_)) => CommandOutcome::Failure(ExitResult::bail(
                        "Unexpected response type from generic command",
                    )),
                    Ok(CommandOutcome::Failure(result)) => CommandOutcome::Failure(result),
                    Err(e) => CommandOutcome::Failure(ExitResult::err(e)),
                }
            })
            .boxed()
    }
}
