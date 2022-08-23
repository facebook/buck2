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

use std::path::PathBuf;

use anyhow::Context as _;
use buck2_client::args::expand_argfiles;
use buck2_client::cleanup_ctx::AsyncCleanupContext;
use buck2_client::cleanup_ctx::AsyncCleanupContextGuard;
use buck2_client::client_ctx::ClientCommandContext;
use buck2_client::commands::aquery::AqueryCommand;
use buck2_client::commands::build::BuildCommand;
use buck2_client::commands::bxl::BxlCommand;
use buck2_client::commands::clean::CleanCommand;
use buck2_client::commands::cquery::CqueryCommand;
use buck2_client::commands::debug::DebugCommand;
use buck2_client::commands::install::InstallCommand;
use buck2_client::commands::kill::KillCommand;
use buck2_client::commands::log::LogCommand;
use buck2_client::commands::lsp::LspCommand;
use buck2_client::commands::profile::ProfileCommand;
use buck2_client::commands::rage::RageCommand;
use buck2_client::commands::root::RootCommand;
use buck2_client::commands::run::RunCommand;
use buck2_client::commands::server::ServerCommand;
use buck2_client::commands::status::StatusCommand;
use buck2_client::commands::streaming::BuckSubcommand;
use buck2_client::commands::targets::TargetsCommand;
use buck2_client::commands::test::TestCommand;
use buck2_client::commands::uquery::UqueryCommand;
use buck2_client::exit_result::ExitResult;
use buck2_client::replayer::Replayer;
use buck2_client::verbosity::Verbosity;
use buck2_client::version::BuckVersion;
use buck2_common::invocation_paths::InvocationPaths;
use buck2_common::invocation_roots;
use buck2_common::result::ToSharedResultExt;
use buck2_core::fs::paths::FileNameBuf;
use clap::AppSettings;
use clap::Parser;
use dice::cycles::DetectCycles;
use gazebo::dupe::Dupe;

use crate::commands::audit::AuditCommand;
use crate::commands::daemon::DaemonCommand;
use crate::commands::docs::DocsCommand;
use crate::commands::forkserver::ForkserverCommand;

#[macro_use]
pub mod panic;

pub mod commands;

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
            CommandKind::Debug(cmd) => cmd.exec(matches, command_ctx, exec),
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
