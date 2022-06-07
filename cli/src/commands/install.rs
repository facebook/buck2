/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_core::exit_result::ExitResult;
use cli_proto::InstallRequest;
use futures::FutureExt;
use gazebo::prelude::*;
use structopt::{clap, StructOpt};

use crate::{
    commands::common::{
        CommonBuildOptions, CommonConfigOptions, CommonConsoleOptions, CommonEventLogOptions,
    },
    daemon::client::{BuckdClientConnector, CommandOutcome},
    CommandContext, StreamingCommand,
};

#[derive(Debug, StructOpt)]
#[structopt(name = "install", about = "builds and installs an application")]
pub struct InstallCommand {
    #[structopt(flatten)]
    config_opts: CommonConfigOptions,

    #[structopt(flatten)]
    console_opts: CommonConsoleOptions,

    #[structopt(flatten)]
    event_log_opts: CommonEventLogOptions,

    #[structopt(flatten)]
    build_opts: CommonBuildOptions,

    #[structopt(name = "TARGET", help = "Target to build and install")]
    patterns: Vec<String>,

    #[structopt(
        short = "-",
        name = "INSTALL_ARGS",
        help = "Additional arguments passed to the install when running it",
        raw = true
    )]
    extra_run_args: Vec<String>,
}

#[async_trait]
impl StreamingCommand for InstallCommand {
    const COMMAND_NAME: &'static str = "install";
    async fn exec_impl(
        self,
        mut buckd: BuckdClientConnector,
        matches: &clap::ArgMatches,
        ctx: CommandContext,
    ) -> ExitResult {
        let ctx = ctx.client_context(&self.config_opts, matches)?;
        let response = buckd
            .with_flushing(|client| {
                client
                    .install(InstallRequest {
                        context: Some(ctx),
                        target_patterns: self.patterns.map(|pat| buck2_data::TargetPattern {
                            value: pat.to_owned(),
                        }),
                        build_opts: Some(self.build_opts.to_proto()),
                        installer_run_args: self.extra_run_args,
                    })
                    .boxed()
            })
            .await?;
        let console = self.console_opts.final_console();

        match response {
            Ok(CommandOutcome::Success(_)) => {
                console.print_success("INSTALL SUCCEEDED")?;
                ExitResult::success()
            }
            Ok(CommandOutcome::Failure(_)) | Err(_) => {
                console.print_error("INSTALL FAILED")?;
                ExitResult::failure()
            }
        }
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        &self.console_opts
    }

    fn event_log_opts(&self) -> &CommonEventLogOptions {
        &self.event_log_opts
    }
}
