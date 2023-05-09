/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! `buck2 audit` command implementation, both client and server.

// Plugins
#![cfg_attr(feature = "gazebo_lint", feature(plugin))]
#![cfg_attr(feature = "gazebo_lint", allow(deprecated))] // :(
#![cfg_attr(feature = "gazebo_lint", plugin(gazebo_lint))]

use std::thread;

use anyhow::Context as _;
use buck2_audit::AuditCommand;
use buck2_client::args::expand_argfiles_with_context;
use buck2_client::args::ArgExpansionContext;
use buck2_client::commands::build::BuildCommand;
use buck2_client::commands::bxl::BxlCommand;
use buck2_client::commands::clean::CleanCommand;
use buck2_client::commands::ctargets::ConfiguredTargetsCommand;
use buck2_client::commands::debug::DebugCommand;
use buck2_client::commands::init::InitCommand;
use buck2_client::commands::install::InstallCommand;
use buck2_client::commands::kill::KillCommand;
use buck2_client::commands::killall::KillallCommand;
use buck2_client::commands::log::LogCommand;
use buck2_client::commands::lsp::LspCommand;
use buck2_client::commands::profile::ProfileCommand;
use buck2_client::commands::query::aquery::AqueryCommand;
use buck2_client::commands::query::cquery::CqueryCommand;
use buck2_client::commands::query::uquery::UqueryCommand;
use buck2_client::commands::rage::RageCommand;
use buck2_client::commands::root::RootCommand;
use buck2_client::commands::run::RunCommand;
use buck2_client::commands::server::ServerCommand;
use buck2_client::commands::status::StatusCommand;
use buck2_client::commands::subscribe::SubscribeCommand;
use buck2_client::commands::targets::TargetsCommand;
use buck2_client::commands::test::TestCommand;
use buck2_client_ctx::cleanup_ctx::AsyncCleanupContextGuard;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::streaming::BuckSubcommand;
use buck2_client_ctx::version::BuckVersion;
use buck2_common::invocation_paths::InvocationPaths;
use buck2_common::invocation_roots::find_invocation_roots;
use buck2_common::result::ToSharedResultExt;
use buck2_core::env_helper::EnvHelper;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_core::logging::LogConfigurationReloadHandle;
use buck2_event_observer::verbosity::Verbosity;
use buck2_server::daemon::server::BuckdServerInitPreferences;
use buck2_starlark::StarlarkCommand;
use clap::AppSettings;
use clap::Parser;
use dice::DetectCycles;
use dice::WhichDice;
use dupe::Dupe;
use gazebo::variants::VariantName;

use crate::check_user_allowed::check_user_allowed;
use crate::commands::daemon::DaemonCommand;
use crate::commands::docs::DocsCommand;
use crate::commands::forkserver::ForkserverCommand;
use crate::commands::internal_test_runner::InternalTestRunnerCommand;
use crate::process_context::ProcessContext;

#[macro_use]
pub mod panic;
mod check_user_allowed;

pub mod commands;
pub mod process_context;

fn parse_isolation_dir(s: &str) -> anyhow::Result<FileNameBuf> {
    FileNameBuf::try_from(s.to_owned()).context("isolation dir must be a directory name")
}

pub use buck2_server_ctx::logging::TracingLogFile;

/// Options of `buck2` command, before subcommand.
#[derive(Clone, Debug, clap::Parser)]
struct BeforeSubcommandOptions {
    /// Instances of Buck2 share a daemon if and only if their isolation directory is identical.
    /// The isolation directory also influences the output paths provided by Buck2,
    /// and as a result using a non-default isolation dir will cause cache misses (and slower builds).
    #[clap(
        parse(try_from_str = parse_isolation_dir),
        env("BUCK_ISOLATION_DIR"),
        long,
        default_value="v2"
    )]
    isolation_dir: FileNameBuf,

    #[clap(env("DICE_DETECT_CYCLES_UNSTABLE"), long, hidden(true))]
    detect_cycles: Option<DetectCycles>,

    #[clap(env("WHICH_DICE_UNSTABLE"), long, hidden(true))]
    which_dice: Option<WhichDice>,

    #[clap(env("ENABLE_TRACE_IO"), long, hidden(true))]
    enable_trace_io: bool,

    /// If passed a given materializer identity, if the materializer state DB matches that
    /// identity, the daemon will not use it and will instead create a new empty materializer
    /// state.
    #[clap(long, hidden(true))]
    reject_materializer_state: Option<String>,

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
    /// Note even when running in no-buckd mode, it still writes state files.
    /// In particular, this command effectively kills buckd process
    /// running with the same isolation directory.
    ///
    /// This is an unsupported option used only for development work.
    #[clap(long, global(true), hidden(true))]
    no_buckd: bool,

    /// Print buck wrapper help.
    #[clap(skip)] // @oss-enable
    // @oss-disable: #[clap(long)]
    help_wrapper: bool,
}

impl BeforeSubcommandOptions {
    pub fn to_server_init_context(&self) -> BuckdServerInitPreferences {
        BuckdServerInitPreferences {
            detect_cycles: self.detect_cycles,
            which_dice: self.which_dice,
            enable_trace_io: self.enable_trace_io,
            reject_materializer_state: self.reject_materializer_state.clone().map(|s| s.into()),
        }
    }
}

#[rustfmt::skip] // Formatting in internal and in OSS versions disagree after oss markers applied.
fn help() -> &'static str {
    concat!(
        "A build system\n",
        "\n",
        "Documentation: https://buck2.build/docs/\n", // @oss-enable
        // @oss-disable: "Documentation: https://internalfb.com/intern/staticdocs/buck2/docs/\n",
    )
}

#[derive(Debug, clap::Parser)]
#[clap(
    name = "buck2",
    about(Some(help())),
    version(BuckVersion::get_version())
)]
pub(crate) struct Opt {
    #[clap(flatten)]
    common_opts: BeforeSubcommandOptions,
    #[clap(subcommand)]
    cmd: CommandKind,
}

impl Opt {
    pub(crate) fn exec(
        self,
        process: ProcessContext<'_>,
        matches: &clap::ArgMatches,
        argfiles_trace: Vec<AbsNormPathBuf>,
    ) -> ExitResult {
        let subcommand_matches = match matches.subcommand().map(|s| s.1) {
            Some(submatches) => submatches,
            None => panic!("Parsed a subcommand but couldn't extract subcommand argument matches"),
        };

        self.cmd.exec(
            process,
            subcommand_matches,
            self.common_opts,
            argfiles_trace,
        )
    }
}

pub fn exec(process: ProcessContext<'_>) -> ExitResult {
    let mut argfile_context = ArgExpansionContext::new(process.working_dir);
    let mut expanded_args =
        expand_argfiles_with_context(process.args.to_vec(), &mut argfile_context)
            .context("Error expanding argsfiles")?;

    // Override arg0 in `buck2 help`.
    static BUCK2_ARG0: EnvHelper<String> = EnvHelper::new("BUCK2_ARG0");
    if let Some(arg0) = BUCK2_ARG0.get()? {
        expanded_args[0] = arg0.clone();
    }

    let clap = Opt::clap();
    let matches = clap.get_matches_from(expanded_args);
    let opt: Opt = Opt::from_clap(&matches);

    if opt.common_opts.help_wrapper {
        return ExitResult::err(anyhow::anyhow!(
            "`--help-wrapper` should have been handled by the wrapper"
        ));
    }

    match &opt.cmd {
        CommandKind::Clean(..) | CommandKind::Daemon(..) | CommandKind::Forkserver(..) => {}
        _ => {
            check_user_allowed()?;
        }
    }

    let argfiles_trace = argfile_context.trace();
    opt.exec(process, &matches, argfiles_trace)
}

#[derive(Debug, clap::Subcommand, VariantName)]
pub(crate) enum CommandKind {
    #[clap(setting(AppSettings::Hidden))]
    Daemon(DaemonCommand),
    #[clap(setting(AppSettings::Hidden))]
    Forkserver(ForkserverCommand),
    #[clap(setting(AppSettings::Hidden))]
    InternalTestRunner(InternalTestRunnerCommand),
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
    Killall(KillallCommand),
    Root(RootCommand),
    /// Alias for `uquery`.
    Query(UqueryCommand),
    Run(RunCommand),
    Server(ServerCommand),
    Status(StatusCommand),
    #[clap(subcommand)]
    Starlark(StarlarkCommand),
    Targets(TargetsCommand),
    Ctargets(ConfiguredTargetsCommand),
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
    Subscribe(SubscribeCommand),
}

impl CommandKind {
    fn command_name(&self) -> String {
        // clap derive does not expose command name, so do this with gazebo.
        self.variant_name().to_lowercase()
    }

    pub(crate) fn exec(
        self,
        process: ProcessContext<'_>,
        matches: &clap::ArgMatches,
        common_opts: BeforeSubcommandOptions,
        argfiles_trace: Vec<AbsNormPathBuf>,
    ) -> ExitResult {
        let init_ctx = common_opts.to_server_init_context();
        let roots = find_invocation_roots(process.working_dir.path());
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
                .exec(
                    process.init,
                    process.log_reload_handle.dupe(),
                    paths?,
                    init_ctx,
                    false,
                    || {},
                )
                .into();
        }

        let async_cleanup = AsyncCleanupContextGuard::new();

        let start_in_process_daemon: Option<Box<dyn FnOnce() -> anyhow::Result<()> + Send + Sync>> =
            if common_opts.no_buckd {
                let paths = paths.clone()?;
                // Create a function which spawns an in-process daemon.
                Some(Box::new(move || {
                    let (tx, rx) = std::sync::mpsc::channel();
                    // Spawn a thread which runs the daemon.
                    thread::spawn(move || {
                        let tx_clone = tx.clone();
                        let result = DaemonCommand::new_in_process().exec(
                            process.init,
                            <dyn LogConfigurationReloadHandle>::noop(),
                            paths,
                            init_ctx,
                            true,
                            move || drop(tx_clone.send(Ok(()))),
                        );
                        // Since `tx` is unbounded, there's race here: it is possible
                        // that error message will be lost in the channel and not reported anywhere.
                        // Not an issue practically, because daemon does not usually error
                        // after it started listening.
                        if let Err(e) = tx.send(result) {
                            match e.0 {
                                Ok(()) => drop(buck2_client_ctx::eprintln!(
                                    "In-process daemon gracefully stopped"
                                )),
                                Err(e) => drop(buck2_client_ctx::eprintln!(
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
                }))
            } else {
                None
            };

        let command_ctx = ClientCommandContext {
            init: process.init,
            paths,
            verbosity: common_opts.verbosity,
            start_in_process_daemon,
            command_name: self.command_name(),
            working_dir: process.working_dir.clone(),
            sanitized_argv: Vec::new(),
            trace_id: process.trace_id.dupe(),
            argfiles_trace,
            async_cleanup: async_cleanup.ctx().dupe(),
            stdin: process.stdin,
            restarter: process.restarter,
            restarted_trace_id: process.restarted_trace_id.dupe(),
        };

        match self {
            CommandKind::Daemon(..) => unreachable!("Checked earlier"),
            CommandKind::Forkserver(cmd) => cmd
                .exec(matches, command_ctx, process.log_reload_handle.dupe())
                .into(),
            CommandKind::InternalTestRunner(cmd) => cmd.exec(matches, command_ctx).into(),
            CommandKind::Aquery(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Build(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Bxl(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Test(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Cquery(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Kill(cmd) => cmd.exec(matches, command_ctx).into(),
            CommandKind::Killall(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Clean(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Root(cmd) => cmd.exec(matches, command_ctx).into(),
            CommandKind::Query(cmd) => {
                buck2_client_ctx::eprintln!(
                    "WARNING: \"buck2 query\" is an alias for \"buck2 uquery\". Consider using \"buck2 cquery\" or \"buck2 uquery\" explicitly."
                )?;
                cmd.exec(matches, command_ctx)
            }
            CommandKind::Server(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Status(cmd) => cmd.exec(matches, command_ctx).into(),
            CommandKind::Targets(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Ctargets(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Audit(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Starlark(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Run(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Uquery(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Debug(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Docs(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Profile(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Rage(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Init(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Install(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Log(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Lsp(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Subscribe(cmd) => cmd.exec(matches, command_ctx),
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
