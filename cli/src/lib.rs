/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(stable_features)]
#![feature(box_syntax)]
#![feature(async_closure)]
#![feature(try_blocks)]
#![feature(negative_impls)]
#![feature(exact_size_is_empty)]
#![feature(box_into_pin)]
#![feature(try_trait_v2)]
// Plugins
#![cfg_attr(feature = "gazebo_lint", feature(plugin))]
#![cfg_attr(feature = "gazebo_lint", allow(deprecated))] // :(
#![cfg_attr(feature = "gazebo_lint", plugin(gazebo_lint))]

#[macro_use]
extern crate maplit;

use std::path::Path;
use std::path::PathBuf;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_client::cleanup_ctx::AsyncCleanupContext;
use buck2_client::cleanup_ctx::AsyncCleanupContextGuard;
use buck2_client::exit_result::ExitResult;
use buck2_client::exit_result::FailureExitCode;
use buck2_client::subscribers::superconsole::StatefulSuperConsole;
use buck2_client::subscribers::superconsole::SuperConsoleConfig;
use buck2_client::verbosity::Verbosity;
use buck2_common::invocation_paths::InvocationPaths;
use buck2_common::invocation_roots;
use buck2_common::result::ToSharedResultExt;
use buck2_core::fs::paths::FileNameBuf;
use buck2_events::subscriber::EventSubscriber;
use clap::AppSettings;
use clap::Parser;
use client_command_context::ClientCommandContext;
use dice::cycles::DetectCycles;
use futures::future;
use gazebo::dupe::Dupe;
use superconsole::Component;

use crate::args::expand_argfiles;
use crate::commands::aquery::AqueryCommand;
use crate::commands::audit::AuditCommand;
use crate::commands::build::BuildCommand;
use crate::commands::bxl::BxlCommand;
use crate::commands::clean::CleanCommand;
use crate::commands::common::subscribers::get_console_with_root;
use crate::commands::common::subscribers::recorder::try_get_invocation_recorder;
use crate::commands::common::subscribers::try_get_build_id_writer;
use crate::commands::common::subscribers::try_get_event_log_subscriber;
use crate::commands::common::CommonBuildConfigurationOptions;
use crate::commands::common::CommonConsoleOptions;
use crate::commands::common::CommonDaemonCommandOptions;
use crate::commands::cquery::CqueryCommand;
use crate::commands::daemon::DaemonCommand;
use crate::commands::debug::DebugCommand;
use crate::commands::docs::DocsCommand;
use crate::commands::forkserver::ForkserverCommand;
use crate::commands::install::InstallCommand;
use crate::commands::kill::KillCommand;
use crate::commands::log::LogCommand;
use crate::commands::lsp::LspCommand;
use crate::commands::profile::ProfileCommand;
use crate::commands::rage::RageCommand;
use crate::commands::root::RootCommand;
use crate::commands::run::RunCommand;
use crate::commands::server::ServerCommand;
use crate::commands::status::StatusCommand;
use crate::commands::targets::TargetsCommand;
use crate::commands::test::TestCommand;
use crate::commands::uquery::UqueryCommand;
use crate::daemon::client::replayer::Replayer;
use crate::daemon::client::BuckdClientConnector;
use crate::daemon::client::BuckdConnectOptions;
use crate::future::Either;
use crate::version::BuckVersion;

#[macro_use]
pub mod panic;

pub mod args;
mod client_command_context;
pub mod commands;
pub mod daemon;
mod stdin_stream;
pub mod version;

fn parse_isolation_dir(s: &str) -> anyhow::Result<FileNameBuf> {
    FileNameBuf::try_from(s.to_owned()).context("isolation dir must be a directory name")
}

#[derive(Debug, clap::Parser)]
pub(crate) struct CommonOptions {
    #[clap(
        parse(try_from_str = parse_isolation_dir),
        env("BUCK_ISOLATION_DIR"),
        long,
        hidden(true),
        default_value="v2"
    )]
    isolation_dir: FileNameBuf,

    #[clap(env("DICE_DETECT_CYCLES_UNSTABLE"), long, hidden(true))]
    detect_cycles: Option<DetectCycles>,

    /// How verbose buck should be while logging.
    /// Values:
    /// 0 = Quiet, errors only;
    /// 1 = default;
    /// 2 = more info about errors;
    /// 3 = more info about everything
    #[clap(
        short = 'v',
        long = "verbose",
        default_value = "1",
        global = true,
        value_name = "NUMBER",
        parse(try_from_str = Verbosity::try_from_cli)
    )]
    verbosity: Verbosity,
}

#[derive(Debug, clap::Parser)]
#[clap(
    name = "buck2",
    about = "a build system",
    version(BuckVersion::get_version())
)]
pub(crate) struct Opt {
    #[clap(flatten)]
    common_opts: CommonOptions,
    #[clap(subcommand)]
    cmd: CommandKind,
}

impl Opt {
    pub(crate) fn exec(
        self,
        matches: &clap::ArgMatches,
        init: fbinit::FacebookInit,
        replayer: Option<Replayer>,
        async_cleanup_context: AsyncCleanupContext,
    ) -> ExitResult {
        let subcommand_matches = match matches.subcommand().map(|s| s.1) {
            Some(submatches) => submatches,
            None => panic!("Parsed a subcommand but couldn't extract subcommand argument matches"),
        };

        self.cmd.exec(
            subcommand_matches,
            self.common_opts,
            init,
            replayer,
            async_cleanup_context,
        )
    }
}

pub fn exec(
    args: Vec<String>,
    cwd: PathBuf,
    init: fbinit::FacebookInit,
    replayer: Option<Replayer>,
) -> ExitResult {
    let guard = AsyncCleanupContextGuard::new();
    let async_cleanup_context = guard.ctx().dupe();

    let expanded_args = expand_argfiles(args, &cwd).context("Error expanding argsfiles")?;

    let clap = Opt::clap();
    let matches = clap.get_matches_from(expanded_args);
    let opt = Opt::from_clap(&matches);
    opt.exec(&matches, init, replayer, async_cleanup_context)
}

fn default_subscribers<T: StreamingCommand>(
    cmd: &T,
    ctx: &ClientCommandContext,
) -> anyhow::Result<Vec<Box<dyn EventSubscriber>>> {
    let console_opts = cmd.console_opts();
    let mut subscribers = vec![];
    let root = StatefulSuperConsole::default_layout(
        T::COMMAND_NAME,
        SuperConsoleConfig {
            sandwiched: cmd.extra_superconsole_component(),
            ..console_opts.superconsole_config()
        },
    );

    // If we're running the LSP, do not show "Waiting for daemon..." if we do not get any spans.
    let show_waiting_message = T::COMMAND_NAME != LspCommand::COMMAND_NAME;

    if let Some(v) = get_console_with_root(
        console_opts.console_type,
        ctx.verbosity,
        show_waiting_message,
        ctx.replay_speed,
        root,
    )? {
        subscribers.push(v)
    }
    if let Some(event_log) = try_get_event_log_subscriber(cmd.event_log_opts(), ctx)? {
        subscribers.push(event_log)
    }
    if let Some(build_id_writer) = try_get_build_id_writer(cmd.event_log_opts())? {
        subscribers.push(build_id_writer)
    }
    if let Some(recorder) = try_get_invocation_recorder(ctx)? {
        subscribers.push(recorder);
    }
    Ok(subscribers)
}

/// Trait to generalize the behavior of executable buck2 commands that rely on a server.
/// This trait is most helpful when the command wants a superconsole, to stream events, etc.
/// However, this is the most robustly tested of our code paths, and there is little cost to defaulting to it.
/// As a result, prefer to default to streaming mode unless there is a compelling reason not to
/// (e.g `status`)
#[async_trait]
pub(crate) trait StreamingCommand: Sized + Send + Sync {
    /// Give the command a name for printing, debugging, etc.
    const COMMAND_NAME: &'static str;

    /// Run the command.
    async fn exec_impl(
        self,
        buckd: BuckdClientConnector,
        matches: &clap::ArgMatches,
        ctx: ClientCommandContext,
    ) -> ExitResult;

    /// Provide a list of all options to connect to the server.
    /// By default, just checks to make sure the server is started.
    async fn server_connect_options<'a, 'b>(
        &self,
        ctx: &'b ClientCommandContext,
    ) -> anyhow::Result<BuckdConnectOptions> {
        Ok(BuckdConnectOptions {
            subscribers: default_subscribers(self, ctx)?,
            ..Default::default()
        })
    }

    fn console_opts(&self) -> &CommonConsoleOptions;

    fn event_log_opts(&self) -> &CommonDaemonCommandOptions;

    fn common_opts(&self) -> &CommonBuildConfigurationOptions;

    /// Allows a command to add additional superconsole components when superconsole is used.
    fn extra_superconsole_component(&self) -> Option<Box<dyn Component>> {
        None
    }
}

#[derive(Debug, clap::Subcommand)]
pub(crate) enum CommandKind {
    #[clap(setting(AppSettings::Hidden))]
    Daemon(DaemonCommand),
    #[clap(setting(AppSettings::Hidden))]
    Forkserver(ForkserverCommand),
    #[clap(subcommand)]
    Audit(AuditCommand),
    Aquery(AqueryCommand),
    Build(BuildCommand),
    Bxl(BxlCommand),
    Test(TestCommand),
    Cquery(CqueryCommand),
    Install(InstallCommand),
    Kill(KillCommand),
    Root(RootCommand),
    Query(UqueryCommand),
    Run(RunCommand),
    Server(ServerCommand),
    Status(StatusCommand),
    Targets(TargetsCommand),
    Uquery(UqueryCommand),
    #[clap(subcommand, setting(AppSettings::Hidden))]
    Debug(DebugCommand),
    Docs(DocsCommand),
    #[clap(subcommand)]
    Profile(ProfileCommand),
    Rage(RageCommand),
    Clean(CleanCommand),
    #[clap(subcommand)]
    Log(LogCommand),
    Lsp(LspCommand),
}

/// Just provides a common interface for buck subcommands for us to interact with here.
pub(crate) trait BuckSubcommand {
    fn exec(self, matches: &clap::ArgMatches, ctx: ClientCommandContext) -> ExitResult;
}

impl<T: StreamingCommand> BuckSubcommand for T {
    /// Actual call that runs a `StreamingCommand`.
    /// Handles all of the business of setting up a runtime, server, and subscribers.
    fn exec(self, matches: &clap::ArgMatches, ctx: ClientCommandContext) -> ExitResult {
        ctx.with_runtime(async move |mut ctx| {
            let work = async {
                let connect_options = self.server_connect_options(&ctx).await?;

                let buckd = match ctx.replayer.take() {
                    Some(replayer) => {
                        connect_options.replay(replayer.into_inner(), ctx.paths()?)?
                    }
                    None => ctx.connect_buckd(connect_options).await?,
                };

                self.exec_impl(buckd, matches, ctx).await
            };

            // Race our work with a ctrl+c future. If we hit ctrl+c, then we'll drop the work
            // future. with_runtime sets up an AsyncCleanupContext that will allow drop
            // implementations within this future to clean up before we return from with_runtime.
            let exit = tokio::signal::ctrl_c();

            futures::pin_mut!(work);
            futures::pin_mut!(exit);

            match future::select(work, exit).await {
                Either::Left((res, _)) => res,
                Either::Right((_signal, _)) => ExitResult::from(FailureExitCode::SignalInterrupt),
            }
        })
    }
}

impl CommandKind {
    pub(crate) fn exec(
        self,
        matches: &clap::ArgMatches,
        common_opts: CommonOptions,
        init: fbinit::FacebookInit,
        replayer: Option<Replayer>,
        async_cleanup_context: AsyncCleanupContext,
    ) -> ExitResult {
        let roots = invocation_roots::find_current_invocation_roots();
        let replay_speed = replayer.as_ref().map(|r| r.speed());
        let command_ctx = ClientCommandContext {
            init,
            paths: roots
                .map(|r| InvocationPaths {
                    roots: r,
                    isolation: common_opts.isolation_dir,
                })
                .shared_error(),
            detect_cycles: common_opts.detect_cycles,
            replayer: replayer.map(sync_wrapper::SyncWrapper::new),
            replay_speed,
            verbosity: common_opts.verbosity,
            async_cleanup_context,
        };
        match self {
            CommandKind::Daemon(cmd) => cmd.exec(matches, command_ctx).into(),
            CommandKind::Forkserver(cmd) => cmd.exec(matches, command_ctx).into(),
            CommandKind::Aquery(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Build(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Bxl(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Test(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Cquery(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Kill(cmd) => cmd.exec(matches, command_ctx).into(),
            CommandKind::Root(cmd) => cmd.exec(matches, command_ctx).into(),
            CommandKind::Query(cmd) => {
                buck2_client::eprintln!(
                    "WARNING: \"buck2 query\" is an alias for \"buck2 uquery\". Consider using \"buck2 cquery\" or \"buck2 uquery\" explicitly."
                )?;
                cmd.exec(matches, command_ctx)
            }
            CommandKind::Server(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Status(cmd) => cmd.exec(matches, command_ctx).into(),
            CommandKind::Targets(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Audit(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Run(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Uquery(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Debug(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Docs(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Profile(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Rage(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Clean(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Install(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Log(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Lsp(cmd) => cmd.exec(matches, command_ctx),
        }
    }
}
