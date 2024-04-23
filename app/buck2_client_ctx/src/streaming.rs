/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::atomic::AtomicU64;
use std::sync::Arc;

use async_trait::async_trait;
use buck2_common::argv::Argv;
use buck2_common::argv::SanitizedArgv;
use dupe::Dupe;

use crate::client_ctx::ClientCommandContext;
use crate::common::ui::CommonConsoleOptions;
use crate::common::CommonBuildConfigurationOptions;
use crate::common::CommonEventLogOptions;
use crate::common::CommonStarlarkOptions;
use crate::daemon::client::connect::BuckdConnectConstraints;
use crate::daemon::client::connect::BuckdConnectOptions;
use crate::daemon::client::connect::DaemonConstraintsRequest;
use crate::daemon::client::connect::DesiredTraceIoState;
use crate::daemon::client::BuckdClientConnector;
use crate::exit_result::ExitCode;
use crate::exit_result::ExitResult;
use crate::path_arg::PathArg;
use crate::signal_handler::with_simple_sigint_handler;
use crate::subscribers::get::get_console_with_root;
use crate::subscribers::get::try_get_build_graph_stats;
use crate::subscribers::get::try_get_build_id_writer;
use crate::subscribers::get::try_get_event_log_subscriber;
use crate::subscribers::get::try_get_re_log_subscriber;
use crate::subscribers::recorder::try_get_invocation_recorder;
use crate::subscribers::subscriber::EventSubscriber;
use crate::subscribers::subscribers::EventSubscribers;

fn default_subscribers<'a, T: StreamingCommand>(
    cmd: &T,
    ctx: &ClientCommandContext<'a>,
) -> anyhow::Result<EventSubscribers<'a>> {
    let console_opts = cmd.console_opts();
    let mut subscribers = vec![];
    let expect_spans = cmd.should_expect_spans();

    // Need this to get information from one subscriber (event_log)
    // and log it in another (invocation_recorder)
    let log_size_counter_bytes = Some(Arc::new(AtomicU64::new(0)));

    subscribers.push(get_console_with_root(
        ctx.trace_id.dupe(),
        console_opts.console_type,
        ctx.verbosity,
        expect_spans,
        None,
        T::COMMAND_NAME,
        console_opts.superconsole_config(),
    )?);

    if let Some(event_log) = try_get_event_log_subscriber(cmd, ctx, log_size_counter_bytes.clone())?
    {
        subscribers.push(event_log)
    }
    if let Some(re_log) = try_get_re_log_subscriber(ctx)? {
        subscribers.push(re_log)
    }
    if let Some(build_id_writer) = try_get_build_id_writer(cmd.event_log_opts(), ctx)? {
        subscribers.push(build_id_writer)
    }
    if let Some(build_graph_stats) = try_get_build_graph_stats(cmd, ctx)? {
        subscribers.push(build_graph_stats)
    }
    let recorder = try_get_invocation_recorder(
        ctx,
        cmd.event_log_opts(),
        cmd.logging_name(),
        cmd.sanitize_argv(ctx.argv.clone()).argv,
        log_size_counter_bytes,
    )?;
    subscribers.push(recorder);

    subscribers.extend(cmd.extra_subscribers());
    Ok(EventSubscribers::new(subscribers))
}

/// Trait to generalize the behavior of executable buck2 commands that rely on a server.
/// This trait is most helpful when the command wants a superconsole, to stream events, etc.
/// However, this is the most robustly tested of our code paths, and there is little cost to defaulting to it.
/// As a result, prefer to default to streaming mode unless there is a compelling reason not to
/// (e.g `status`)
#[async_trait]
pub trait StreamingCommand: Sized + Send + Sync {
    /// Give the command a name for printing, debugging, etc.
    const COMMAND_NAME: &'static str;

    /// Run the command.
    async fn exec_impl(
        self,
        buckd: &mut BuckdClientConnector,
        matches: &clap::ArgMatches,
        ctx: &mut ClientCommandContext<'_>,
    ) -> ExitResult;

    /// Should we only connect to existing servers (`true`), or spawn a new server if required (`false`).
    /// Defaults to `false`.
    fn existing_only() -> bool {
        false
    }

    fn trace_io(&self) -> DesiredTraceIoState {
        DesiredTraceIoState::Existing
    }

    fn console_opts(&self) -> &CommonConsoleOptions;

    fn event_log_opts(&self) -> &CommonEventLogOptions;

    fn build_config_opts(&self) -> &CommonBuildConfigurationOptions;

    fn starlark_opts(&self) -> &CommonStarlarkOptions;

    fn extra_subscribers(&self) -> Vec<Box<dyn EventSubscriber>> {
        vec![]
    }

    fn sanitize_argv(&self, argv: Argv) -> SanitizedArgv {
        argv.no_need_to_sanitize()
    }

    fn logging_name(&self) -> &'static str {
        Self::COMMAND_NAME
    }

    /// Some commands should always be displaying
    /// at least 1 ongoing process in the terminal, aka "spans".
    /// In the simple console, we want to display a "Waiting for daemon..." message
    /// exclusively for these commands whenever there are long periods without spans.
    fn should_expect_spans(&self) -> bool {
        true
    }

    /// Currently only for BxlCommand.
    fn user_event_log(&self) -> &Option<PathArg> {
        &None
    }
}

/// Just provides a common interface for buck subcommands for us to interact with here.
pub trait BuckSubcommand {
    fn exec(self, matches: &clap::ArgMatches, ctx: ClientCommandContext<'_>) -> ExitResult;
}

impl<T: StreamingCommand> BuckSubcommand for T {
    /// Actual call that runs a `StreamingCommand`.
    /// Handles all of the business of setting up a runtime, server, and subscribers.
    fn exec(self, matches: &clap::ArgMatches, ctx: ClientCommandContext<'_>) -> ExitResult {
        ctx.with_runtime(|mut ctx| async move {
            let work = async {
                let constraints = if T::existing_only() {
                    BuckdConnectConstraints::ExistingOnly
                } else {
                    let mut req =
                        DaemonConstraintsRequest::new(ctx.immediate_config, T::trace_io(&self))?;
                    ctx.restarter.apply_to_constraints(&mut req);
                    BuckdConnectConstraints::Constraints(req)
                };

                let mut connect_options = BuckdConnectOptions {
                    subscribers: default_subscribers(&self, &ctx)?,
                    constraints,
                };

                let buckd = match ctx.start_in_process_daemon.take() {
                    None => ctx.connect_buckd(connect_options).await,
                    Some(start_in_process_daemon) => {
                        // Start in-process daemon, wait until it is ready to accept connections.
                        start_in_process_daemon()?;

                        // Do not attempt to spawn a daemon if connect failed.
                        // Connect should not fail.
                        connect_options.constraints = BuckdConnectConstraints::ExistingOnly;

                        ctx.connect_buckd(connect_options).await
                    }
                };

                let mut buckd = match buckd {
                    Ok(buckd) => buckd,
                    Err(e) => {
                        return ExitResult::err_with_exit_code(e, ExitCode::ConnectError);
                    }
                };

                let command_result = self.exec_impl(&mut buckd, matches, &mut ctx).await;

                ctx.restarter.observe(&buckd);

                command_result
            };

            with_simple_sigint_handler(work)
                .await
                .unwrap_or_else(|| ExitResult::status(ExitCode::SignalInterrupt))
        })
    }
}
