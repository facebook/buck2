/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_cli_proto::new_generic::ExplainRequest;
use buck2_cli_proto::new_generic::NewGenericRequest;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::ui::CommonConsoleOptions;
use buck2_client_ctx::common::CommonBuildConfigurationOptions;
use buck2_client_ctx::common::CommonEventLogOptions;
use buck2_client_ctx::common::CommonStarlarkOptions;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::path_arg::PathArg;
use buck2_client_ctx::streaming::StreamingCommand;
use clap::ArgMatches;
use tonic::async_trait;

/// Buck2 Explain
///
/// This command is to allow users to dive in and understand
/// builds, without requiring a solid grasp of Buck2 concepts
#[derive(Debug, clap::Parser)]
#[clap(name = "explain", group = clap::ArgGroup::new("out").multiple(true))]
pub struct ExplainCommand {
    /// Output file path for profile data.
    ///
    /// File will be created if it does not exist, and overwritten if it does.
    #[clap(long, short = 'o', group = "out")]
    output: Option<PathArg>,
    // TODO iguridi: pass target for now, eventually read it from the logs for an actual build
    /// Target to get information from
    #[clap(long, short = 't')]
    target: String,
    /// Whether to upload the output to Manifold
    #[clap(long, group = "out")]
    upload: bool,
    /// Dev only: dump the flatbuffer info to file path
    #[clap(long, hide = true)]
    fbs_dump: Option<PathArg>,
}

// TODO: not sure I need StreamingCommand
#[async_trait]
impl StreamingCommand for ExplainCommand {
    const COMMAND_NAME: &'static str = "explain";

    async fn exec_impl(
        self,
        buckd: &mut BuckdClientConnector,
        matches: &ArgMatches,
        ctx: &mut ClientCommandContext<'_>,
    ) -> ExitResult {
        if cfg!(windows) {
            return ExitResult::bail("Not implemented for windows");
        }

        let output = self.output.clone().map(|o| o.resolve(&ctx.working_dir));

        let manifold_path = if self.upload {
            // TODO iguridi: eventually use associated build uuid
            Some(format!("flat/{}-explain.html", ctx.trace_id))
        } else {
            None
        };

        let context = ctx.client_context(matches, &self)?;
        buckd
            .with_flushing()
            .new_generic(
                context,
                NewGenericRequest::Explain(ExplainRequest {
                    output,
                    target: self.target,
                    fbs_dump: self.fbs_dump.map(|x| x.resolve(&ctx.working_dir)),
                    allow_vpnless: ctx.allow_vpnless().unwrap_or(true),
                    manifold_path: manifold_path.clone(),
                }),
                None,
            )
            .await??;

        if let Some(p) = manifold_path {
            buck2_client_ctx::eprintln!(
                "\nView html in your browser: https://interncache-all.fbcdn.net/manifold/buck2_logs/{}\n",
                p
            )?;
        }

        ExitResult::success()
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        CommonConsoleOptions::default_ref()
    }

    fn event_log_opts(&self) -> &CommonEventLogOptions {
        CommonEventLogOptions::default_ref()
    }

    fn build_config_opts(&self) -> &CommonBuildConfigurationOptions {
        CommonBuildConfigurationOptions::default_ref()
    }

    fn starlark_opts(&self) -> &CommonStarlarkOptions {
        CommonStarlarkOptions::default_ref()
    }
}
