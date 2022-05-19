/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

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

use std::{
    convert::TryFrom,
    path::{Path, PathBuf},
};

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_core::{
    exit_result::ExitResult,
    fs::paths::ForwardRelativePathBuf,
    result::{SharedResult, ToSharedResultExt},
};
use cli_proto::{client_context::HostPlatformOverride as GrpcHostPlatformOverride, ClientContext};
use dice::cycles::DetectCycles;
use events::subscriber::EventSubscriber;
use futures::Future;
use gazebo::prelude::*;
use structopt::{
    clap::{self, AppSettings},
    StructOpt,
};
use superconsole::Component;
use tokio::runtime::Builder;

use crate::{
    args::expand_argfiles,
    commands::{
        aquery::AqueryCommand,
        audit::AuditCommand,
        build::BuildCommand,
        clean::CleanCommand,
        common::{
            subscribers::{
                get_console_with_root,
                superconsole::{StatefulSuperConsole, SuperConsoleConfig},
                try_get_event_log_subscriber,
            },
            verbosity::Verbosity,
            CommonConfigOptions, CommonConsoleOptions, CommonEventLogOptions, HostPlatformOverride,
        },
        cquery::CqueryCommand,
        daemon::DaemonCommand,
        debug::DebugCommand,
        docs::DocsCommand,
        install::InstallCommand,
        kill::KillCommand,
        log::LogCommand,
        profile::ProfileCommand,
        rage::RageCommand,
        root::RootCommand,
        run::RunCommand,
        server::ServerCommand,
        status::StatusCommand,
        targets::TargetsCommand,
        test::TestCommand,
        uquery::UqueryCommand,
    },
    daemon::client::{BuckdClient, BuckdConnectOptions, Replayer},
    paths::Paths,
    version::BuckVersion,
};

#[macro_use]
pub mod panic;

pub mod args;
mod commands;
pub mod configs;
pub mod daemon;
pub mod dot;
pub mod metadata;
pub mod paths;
pub mod query;
pub mod roots;
mod stdio;
pub mod target_hash;
pub mod version;
pub mod watchman;
fn parse_forward_path(s: &str) -> anyhow::Result<ForwardRelativePathBuf> {
    ForwardRelativePathBuf::try_from(s.to_owned())
}

#[derive(Debug, StructOpt)]
pub struct CommonOptions {
    #[structopt(
        parse(try_from_str = parse_forward_path),
        env("BUCK_ISOLATION_DIR"),
        long,
        hidden(true),
        default_value="v2"
    )]
    isolation_dir: ForwardRelativePathBuf,

    #[structopt(
        env("DICE_DETECT_CYCLES_UNSTABLE"),
        long,
        hidden(true),
        default_value = "DISABLED"
    )]
    detect_cycles: DetectCycles,

    /// How verbose buck should be while logging.
    /// Values:
    /// 0 = Quiet, errors only;
    /// 1 = default;
    /// 2 = more info about errors;
    /// 3 = more info about everything
    #[structopt(
        short = "v",
        long = "verbose",
        default_value = "1",
        global = true,
        value_name = "NUMBER",
        parse(try_from_str = Verbosity::try_from_cli)
    )]
    verbosity: Verbosity,
}

#[derive(Debug, StructOpt)]
#[structopt(
    name = "buck2",
    about = "a build system",
    version(BuckVersion::get_version()),
    global_settings(&[AppSettings::ColoredHelp]),
)]
pub struct Opt {
    #[structopt(flatten)]
    common_opts: CommonOptions,
    #[structopt(subcommand)]
    cmd: CommandKind,
}

impl Opt {
    pub fn exec(
        self,
        matches: &structopt::clap::ArgMatches,
        init: fbinit::FacebookInit,
        replayer: Option<Replayer>,
    ) -> ExitResult {
        let subcommand_matches = match matches.subcommand().1 {
            Some(submatches) => submatches,
            None => panic!("Parsed a subcommand but couldn't extract subcommand argument matches"),
        };

        self.cmd
            .exec(subcommand_matches, self.common_opts, init, replayer)
    }
}

pub fn exec(
    args: Vec<String>,
    cwd: PathBuf,
    init: fbinit::FacebookInit,
    replayer: Option<Replayer>,
) -> ExitResult {
    let expanded_args = expand_argfiles(args, &cwd).context("Error expanding argsfiles")?;

    let clap = Opt::clap();
    let matches = clap.get_matches_from(expanded_args);
    let opt = Opt::from_clap(&matches);
    opt.exec(&matches, init, replayer)
}

fn default_subscribers<T: StreamingCommand>(
    cmd: &T,
    ctx: &CommandContext,
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

    if let Some(v) = get_console_with_root(
        console_opts.console_type,
        ctx.verbosity,
        ctx.replay_speed,
        root,
    )? {
        subscribers.push(v)
    }
    if let Some(event_log) = try_get_event_log_subscriber(cmd.event_log_opts(), ctx)? {
        subscribers.push(event_log)
    }
    Ok(subscribers)
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
        buckd: BuckdClient,
        matches: &clap::ArgMatches,
        ctx: CommandContext,
    ) -> ExitResult;

    /// Provide a list of all options to connect to the server.
    /// By default, just checks to make sure the server is started.
    async fn server_connect_options<'a, 'b>(
        &self,
        ctx: &'b CommandContext,
    ) -> anyhow::Result<BuckdConnectOptions> {
        Ok(BuckdConnectOptions {
            subscribers: default_subscribers(self, ctx)?,
            ..Default::default()
        })
    }

    fn console_opts(&self) -> &CommonConsoleOptions;

    fn event_log_opts(&self) -> &CommonEventLogOptions;

    /// Allows a command to add additional superconsole components when superconsole is used.
    fn extra_superconsole_component(&self) -> Option<Box<dyn Component>> {
        None
    }
}

#[derive(Debug, StructOpt)]
#[structopt(setting = AppSettings::VersionlessSubcommands)]
pub enum CommandKind {
    #[structopt(setting(AppSettings::Hidden))]
    Daemon(DaemonCommand),
    Audit(AuditCommand),
    Aquery(AqueryCommand),
    Build(BuildCommand),
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
    #[structopt(setting(AppSettings::Hidden))]
    Debug(DebugCommand),
    Docs(DocsCommand),
    Profile(ProfileCommand),
    Rage(RageCommand),
    Clean(CleanCommand),
    Log(LogCommand),
}

pub struct CommandContext {
    init: fbinit::FacebookInit,
    paths: SharedResult<Paths>,
    detect_cycles: DetectCycles,
    replayer: Option<sync_wrapper::SyncWrapper<Replayer>>,
    verbosity: Verbosity,
    replay_speed: Option<f64>,
}

impl CommandContext {
    pub fn fbinit(&self) -> fbinit::FacebookInit {
        self.init
    }

    pub fn paths(&self) -> SharedResult<&Paths> {
        match &self.paths {
            Ok(p) => Ok(p),
            Err(e) => Err(e.dupe()),
        }
    }

    pub fn with_runtime<Fut: Future, F: FnOnce(CommandContext) -> Fut>(
        self,
        func: F,
    ) -> <Fut as Future>::Output {
        let runtime = Builder::new_current_thread()
            .enable_all()
            .build()
            .expect("Should be able to start a runtime");
        runtime.block_on(func(self))
    }

    pub async fn connect_buckd(&self, options: BuckdConnectOptions) -> anyhow::Result<BuckdClient> {
        BuckdConnectOptions { ..options }
            .connect(self.paths()?)
            .await
        .context("Failed to connect to buck daemon. Try running `buck2 clean` and your command afterwards. Alternatively, try running `rm -rf ~/.buck/buckd` and your command afterwards")
    }

    pub fn client_context(
        &self,
        config_opts: &CommonConfigOptions,
        arg_matches: &clap::ArgMatches,
    ) -> anyhow::Result<ClientContext> {
        let config_overrides = config_opts.config_overrides(arg_matches)?;
        // TODO(cjhopman): Support non unicode paths?
        Ok(ClientContext {
            config_overrides,
            target_platform: config_opts.target_platforms.clone().unwrap_or_default(),
            host_platform: match config_opts.host_platform_override() {
                HostPlatformOverride::Default => GrpcHostPlatformOverride::Default,
                HostPlatformOverride::Linux => GrpcHostPlatformOverride::Linux,
                HostPlatformOverride::MacOs => GrpcHostPlatformOverride::MacOs,
                HostPlatformOverride::Windows => GrpcHostPlatformOverride::Windows,
            }
            .into(),
            oncall: config_opts.oncall.as_ref().cloned().unwrap_or_default(),
            disable_starlark_types: config_opts.disable_starlark_types,
            ..self.empty_client_context()?
        })
    }

    /// A client context for commands where CommonConfigOptions are not provided.
    pub fn empty_client_context(&self) -> anyhow::Result<ClientContext> {
        #[derive(Debug, thiserror::Error)]
        #[error("Current directory is not UTF-8")]
        struct CurrentDirIsNotUtf8;

        Ok(ClientContext {
            working_dir: std::env::current_dir()?
                .to_str()
                .ok_or(CurrentDirIsNotUtf8)?
                .to_owned(),
            config_overrides: Default::default(),
            target_platform: Default::default(),
            host_platform: Default::default(),
            oncall: Default::default(),
            disable_starlark_types: false,
        })
    }
}

/// Just provides a common interface for buck subcommands for us to interact with here.
pub trait BuckSubcommand {
    fn exec(self, matches: &clap::ArgMatches, ctx: CommandContext) -> ExitResult;
}

impl<T: StreamingCommand> BuckSubcommand for T {
    /// Actual call that runs a `StreamingCommand`.
    /// Handles all of the business of setting up a runtime, server, and subscribers.
    fn exec(self, matches: &clap::ArgMatches, ctx: CommandContext) -> ExitResult {
        ctx.with_runtime(async move |mut ctx| {
            let connect_options = self.server_connect_options(&ctx).await?;

            let buckd = match ctx.replayer.take() {
                Some(replayer) => connect_options.replay(replayer.into_inner(), ctx.paths()?)?,
                None => ctx.connect_buckd(connect_options).await?,
            };

            self.exec_impl(buckd, matches, ctx).await
        })
    }
}

impl CommandKind {
    pub fn exec(
        self,
        matches: &structopt::clap::ArgMatches,
        common_opts: CommonOptions,
        init: fbinit::FacebookInit,
        replayer: Option<Replayer>,
    ) -> ExitResult {
        let roots = roots::find_current_roots();
        let replay_speed = replayer.as_ref().map(|r| r.speed());
        let command_ctx = CommandContext {
            init,
            paths: roots
                .map(|r| Paths {
                    roots: r,
                    isolation: common_opts.isolation_dir,
                })
                .shared_error(),
            detect_cycles: common_opts.detect_cycles,
            replayer: replayer.map(sync_wrapper::SyncWrapper::new),
            replay_speed,
            verbosity: common_opts.verbosity,
        };
        match self {
            CommandKind::Daemon(cmd) => cmd.exec(matches, command_ctx).into(),
            CommandKind::Aquery(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Build(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Test(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Cquery(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Kill(cmd) => cmd.exec(matches, command_ctx).into(),
            CommandKind::Root(cmd) => cmd.exec(matches, command_ctx).into(),
            CommandKind::Query(cmd) => cmd.exec(matches, command_ctx),
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
        }
    }
}
