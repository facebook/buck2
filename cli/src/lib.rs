/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! `buck2 audit` command implementation, both client and server.

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

use std::thread;

use anyhow::Context as _;
use buck2_audit::AuditCommand;
use buck2_client::args::expand_argfiles;
use buck2_client::client_ctx::ClientCommandContext;
use buck2_client::client_ctx::ProcessContext;
use buck2_client::commands::aquery::AqueryCommand;
use buck2_client::commands::build::BuildCommand;
use buck2_client::commands::bxl::BxlCommand;
use buck2_client::commands::clean::CleanCommand;
use buck2_client::commands::cquery::CqueryCommand;
use buck2_client::commands::debug::DebugCommand;
use buck2_client::commands::init::InitCommand;
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
use buck2_core::env_helper::EnvHelper;
use buck2_core::fs::paths::AbsPathBuf;
use buck2_core::fs::paths::FileNameBuf;
use clap::AppSettings;
use clap::Parser;
use dice::cycles::DetectCycles;
use gazebo::variants::VariantName;

use crate::check_user_allowed::check_user_allowed;
use crate::commands::daemon::DaemonCommand;
use crate::commands::docs::DocsCommand;
use crate::commands::forkserver::ForkserverCommand;

#[macro_use]
pub mod panic;
mod check_user_allowed;

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

    /// Do not launch a daemon process, run buck server in client process.
    ///
    /// This is an unsupported option used only for development work.
    #[clap(long, global(true))]
    no_buckd: bool,
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
        working_dir: AbsPathBuf,
        matches: &clap::ArgMatches,
        init: fbinit::FacebookInit,
        replay: Option<(ProcessContext, Replayer)>,
    ) -> ExitResult {
        let subcommand_matches = match matches.subcommand().map(|s| s.1) {
            Some(submatches) => submatches,
            None => panic!("Parsed a subcommand but couldn't extract subcommand argument matches"),
        };

        self.cmd.exec(
            working_dir,
            subcommand_matches,
            self.common_opts,
            init,
            replay,
        )
    }
}

pub fn exec(
    args: Vec<String>,
    working_dir: AbsPathBuf,
    init: fbinit::FacebookInit,
    replay: Option<(ProcessContext, Replayer)>,
) -> ExitResult {
    let mut expanded_args =
        expand_argfiles(args, &working_dir).context("Error expanding argsfiles")?;

    // Override arg0 in `buck2 help`.
    static BUCK2_ARG0: EnvHelper<String> = EnvHelper::new("BUCK2_ARG0");
    if let Some(arg0) = BUCK2_ARG0.get()? {
        expanded_args[0] = arg0.clone();
    }

    let clap = Opt::clap();
    let matches = clap.get_matches_from(expanded_args);
    let opt: Opt = Opt::from_clap(&matches);

    match &opt.cmd {
        CommandKind::Clean(..) => {}
        _ => {
            check_user_allowed()?;
        }
    }

    opt.exec(working_dir, &matches, init, replay)
}

#[derive(Debug, clap::Subcommand, VariantName)]
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
    Init(InitCommand),
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
    fn command_name(&self) -> String {
        // clap derive does not expose command name, so do this with gazebo.
        self.variant_name().to_lowercase()
    }

    pub(crate) fn exec(
        self,
        working_dir: AbsPathBuf,
        matches: &clap::ArgMatches,
        common_opts: CommonOptions,
        init: fbinit::FacebookInit,
        replay: Option<(ProcessContext, Replayer)>,
    ) -> ExitResult {
        let roots = invocation_roots::find_current_invocation_roots();
        let paths = roots
            .map(|r| InvocationPaths {
                roots: r,
                isolation: common_opts.isolation_dir,
            })
            .shared_error();

        // Handle the daemon command earlier: it wants to fork, but the things we do below might
        // want to create threads.
        if let CommandKind::Daemon(cmd) = &self {
            return cmd
                .exec(init, paths?, common_opts.detect_cycles, || {})
                .into();
        }

        let replay_speed = replay.as_ref().map(|(_, r)| r.speed());

        let (process_context, _cleanup_drop_guard, replayer) = match replay {
            Some((pctx, replayer)) => (pctx, None, Some(sync_wrapper::SyncWrapper::new(replayer))),
            None => {
                let (pctx, drop_guard) = ProcessContext::initialize()?;
                (pctx, Some(drop_guard), None)
            }
        };

        let start_in_process_daemon: Option<Box<dyn FnOnce() -> anyhow::Result<()> + Send + Sync>> =
            if common_opts.no_buckd {
                let paths = paths.clone()?;
                // Create a function which spawns an in-process daemon.
                Some(box move || {
                    let (tx, rx) = std::sync::mpsc::channel();
                    // Spawn a thread which runs the daemon.
                    thread::spawn(move || {
                        let tx_clone = tx.clone();
                        let result = DaemonCommand::new_in_process().exec(
                            init,
                            paths,
                            common_opts.detect_cycles,
                            move || drop(tx_clone.send(Ok(()))),
                        );
                        // Since `tx` is unbounded, there's race here: it is possible
                        // that error message will be lost in the channel and not reported anywhere.
                        // Not an issue practically, because daemon does not usually error
                        // after it started listening.
                        if let Err(e) = tx.send(result) {
                            match e.0 {
                                Ok(()) => drop(buck2_client::eprintln!(
                                    "In-process daemon gracefully stopped"
                                )),
                                Err(e) => drop(buck2_client::eprintln!(
                                    "In-process daemon run failed: {:#}",
                                    e
                                )),
                            }
                        }
                    });
                    // Wait for listener to start (or to fail).
                    match rx.recv() {
                        Ok(r) => r,
                        Err(_) => Err(anyhow::anyhow!(
                            "In-process daemon failed to start and we don't know why"
                        )),
                    }
                })
            } else {
                None
            };

        let command_ctx = ClientCommandContext {
            init,
            paths,
            replayer,
            replay_speed,
            verbosity: common_opts.verbosity,
            process_context,
            start_in_process_daemon,
            command_name: self.command_name(),
            working_dir,
            sanitized_argv: Vec::new(),
        };

        match self {
            CommandKind::Daemon(..) => unreachable!("Checked earlier"),
            CommandKind::Forkserver(cmd) => cmd.exec(matches, command_ctx).into(),
            CommandKind::Aquery(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Build(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Bxl(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Test(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Cquery(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Kill(cmd) => cmd.exec(matches, command_ctx).into(),
            CommandKind::Clean(cmd) => cmd.exec(matches, command_ctx).into(),
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
            CommandKind::Init(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Install(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Log(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Lsp(cmd) => cmd.exec(matches, command_ctx),
        }
    }
}

#[cfg(test)]
mod tests {
    use buck2_client::commands::kill::KillCommand;

    use crate::CommandKind;

    #[test]
    fn test_command_name() {
        assert_eq!("kill", CommandKind::Kill(KillCommand {}).command_name());
    }
}
