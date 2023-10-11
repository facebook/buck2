/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context as _;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::CommonConsoleOptions;
use buck2_client_ctx::daemon::client::NoPartialResultHandler;
use buck2_client_ctx::events_ctx::EventsCtx;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::exit_result::FailureExitCode;
use buck2_client_ctx::replayer::Replayer;
use buck2_client_ctx::signal_handler::with_simple_sigint_handler;
use buck2_client_ctx::subscribers::get::get_console_with_root;

use crate::commands::log::options::EventLogOptions;

/// Replay an event log.
///
/// This command allows visualizing an existing event log in a Superconsole.
#[derive(Debug, clap::Parser)]
#[clap(
    setting = clap::AppSettings::TrailingVarArg
)]
pub struct ReplayCommand {
    #[clap(flatten)]
    event_log: EventLogOptions,

    #[clap(
        long,
        help = "Control the playback speed using a float (i.e. 0.5, 2, etc)",
        value_name = "NUMBER"
    )]
    pub speed: Option<f64>,

    /// Preload the event log. This is typically only useful for benchmarking.
    #[clap(long)]
    preload: bool,

    #[clap(flatten)]
    console_opts: CommonConsoleOptions,

    #[clap(help = "Override the arguments")]
    pub override_args: Vec<String>,
}

impl ReplayCommand {
    pub fn exec(self, _matches: &clap::ArgMatches, ctx: ClientCommandContext<'_>) -> ExitResult {
        let Self {
            event_log,
            speed,
            preload,
            console_opts,
            override_args: _,
        } = self;

        ctx.with_runtime(async move |mut ctx| {
            let work = async {
                let (replayer, invocation) =
                    Replayer::new(event_log.get(&ctx).await?, speed, preload).await?;

                let console = get_console_with_root(
                    invocation.trace_id,
                    console_opts.console_type,
                    ctx.verbosity,
                    true,
                    speed,
                    "(replay)", // Could be better
                    console_opts.superconsole_config(),
                )?
                .context("You must request a console for replay")?;

                let res = EventsCtx::new(vec![console])
                    .unpack_stream::<_, ReplayResult, _>(
                        &mut NoPartialResultHandler,
                        Box::pin(replayer),
                        None,
                        ctx.stdin().console_interaction_stream(&console_opts),
                    )
                    .await;

                if let Err(e) = &res {
                    let msg = "request finished without returning a CommandResult";
                    if e.to_string().contains(msg) {
                        buck2_client_ctx::eprintln!(
                            "Warning: Incomplete log. Replay may be inaccurate."
                        )?;
                    };
                };

                let res = res??;
                for e in &res.errors {
                    buck2_client_ctx::eprintln!("{}", e)?;
                }

                ExitResult::success()
            };

            with_simple_sigint_handler(work)
                .await
                .unwrap_or_else(|| ExitResult::from(FailureExitCode::SignalInterrupt))
        })
    }
}

struct ReplayResult {
    errors: Vec<String>,
}

impl TryFrom<buck2_cli_proto::command_result::Result> for ReplayResult {
    type Error = buck2_cli_proto::command_result::Result;

    fn try_from(v: buck2_cli_proto::command_result::Result) -> Result<Self, Self::Error> {
        use buck2_cli_proto::command_result::Result;

        // It would be good to declare this as a extension trait on our types, but for now to
        // support Replay this is fine;
        let errors = match v {
            Result::Error(v) => v.messages,
            Result::BuildResponse(v) => v.error_messages,
            Result::TestResponse(v) => v.error_messages,
            Result::BxlResponse(v) => v.error_messages,
            _ => Vec::new(),
        };

        Ok(Self { errors })
    }
}
