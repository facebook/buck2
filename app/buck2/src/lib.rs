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
use buck2_client_ctx::argv::Argv;
use buck2_client_ctx::cleanup_ctx::AsyncCleanupContextGuard;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::client_metadata::ClientMetadata;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::immediate_config::ImmediateConfigContext;
use buck2_client_ctx::streaming::BuckSubcommand;
use buck2_client_ctx::tokio_runtime_setup::client_tokio_runtime;
use buck2_client_ctx::version::BuckVersion;
use buck2_common::invocation_paths::InvocationPaths;
use buck2_common::invocation_roots::find_invocation_roots;
use buck2_common::result::ToSharedResultExt;
use buck2_core::env_helper::EnvHelper;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_core::logging::LogConfigurationReloadHandle;
use buck2_event_observer::verbosity::Verbosity;
use buck2_starlark::StarlarkCommand;
use clap::AppSettings;
use clap::Parser;
use dice::DetectCycles;
use dice::WhichDice;
use dupe::Dupe;

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

    // TODO: Those should be on the daemon subcommand.
    #[clap(flatten)]
    daemon: DaemonBeforeSubcommandOptions,

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

    /// The oncall executing this command
    #[clap(long, global = true)]
    oncall: Option<String>,

    /// Metadata key-value pairs to inject into Buck2's logging. Client metadata must be of the
    /// form `key=value`, where `key` is a snake_case identifier, and will be sent to backend
    /// datasets.
    #[clap(long, global = true)]
    client_metadata: Vec<ClientMetadata>,

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

#[derive(Clone, Debug, clap::Parser)]
struct DaemonBeforeSubcommandOptions {
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
        immediate_config: &ImmediateConfigContext,
        matches: &clap::ArgMatches,
        argv: Argv,
    ) -> ExitResult {
        let subcommand_matches = match matches.subcommand().map(|s| s.1) {
            Some(submatches) => submatches,
            None => panic!("Parsed a subcommand but couldn't extract subcommand argument matches"),
        };

        self.cmd.exec(
            process,
            immediate_config,
            subcommand_matches,
            argv,
            self.common_opts,
        )
    }
}

pub fn exec(process: ProcessContext<'_>) -> ExitResult {
    let mut immediate_config = ImmediateConfigContext::new(process.working_dir);
    let mut expanded_args =
        expand_argfiles_with_context(process.args.to_vec(), &mut immediate_config)
            .context("Error expanding argsfiles")?;

    // Override arg0 in `buck2 help`.
    static BUCK2_ARG0: EnvHelper<String> = EnvHelper::new("BUCK2_ARG0");
    if let Some(arg0) = BUCK2_ARG0.get()? {
        expanded_args[0] = arg0.clone();
    }

    let clap = Opt::clap();
    let matches = clap.get_matches_from(&expanded_args);
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

    let argv = Argv {
        argv: process.args.to_vec(),
        expanded_argv: expanded_args,
    };

    opt.exec(process, &immediate_config, &matches, argv)
}

#[derive(Debug, clap::Subcommand)]
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
    /// Alias for `utargets`.
    Targets(TargetsCommand),
    Utargets(TargetsCommand),
    Ctargets(ConfiguredTargetsCommand),
    Uquery(UqueryCommand),
    #[clap(subcommand, setting(AppSettings::Hidden))]
    Debug(DebugCommand),
    Docs(DocsCommand),
    #[clap(subcommand)]
    Profile(ProfileCommand),
    #[clap(hide(true))] // @oss-enable
    Rage(RageCommand),
    Clean(CleanCommand),
    #[clap(subcommand)]
    Log(LogCommand),
    Lsp(LspCommand),
    Subscribe(SubscribeCommand),
}

impl CommandKind {
    pub(crate) fn exec(
        self,
        process: ProcessContext<'_>,
        immediate_config: &ImmediateConfigContext,
        matches: &clap::ArgMatches,
        argv: Argv,
        common_opts: BeforeSubcommandOptions,
    ) -> ExitResult {
        let roots = find_invocation_roots(process.working_dir.path());
        let paths = roots
            .map(|r| InvocationPaths {
                roots: r,
                isolation: common_opts.isolation_dir.clone(),
            })
            .shared_error();

        // Handle the daemon command earlier: it wants to fork, but the things we do below might
        // want to create threads.
        if let CommandKind::Daemon(cmd) = self {
            return cmd
                .exec(
                    process.init,
                    process.log_reload_handle.dupe(),
                    paths?,
                    common_opts.daemon,
                    false,
                    || {},
                )
                .into();
        }

        let runtime = client_tokio_runtime()?;
        let async_cleanup = AsyncCleanupContextGuard::new(&runtime);

        let start_in_process_daemon: Option<Box<dyn FnOnce() -> anyhow::Result<()> + Send + Sync>> =
            if common_opts.no_buckd {
                let daemon_startup_config = immediate_config.daemon_startup_config()?.clone();
                let paths = paths.clone()?;
                // Create a function which spawns an in-process daemon.
                Some(Box::new(move || {
                    let (tx, rx) = std::sync::mpsc::channel();
                    // Spawn a thread which runs the daemon.
                    thread::spawn(move || {
                        let tx_clone = tx.clone();
                        let result = DaemonCommand::new_in_process(daemon_startup_config).exec(
                            process.init,
                            <dyn LogConfigurationReloadHandle>::noop(),
                            paths,
                            common_opts.daemon,
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
            immediate_config,
            paths,
            verbosity: common_opts.verbosity,
            start_in_process_daemon,
            working_dir: process.working_dir.clone(),
            trace_id: process.trace_id.dupe(),
            async_cleanup: async_cleanup.ctx().dupe(),
            stdin: process.stdin,
            restarter: process.restarter,
            restarted_trace_id: process.restarted_trace_id.dupe(),
            argv,
            runtime: &runtime,
            oncall: common_opts.oncall,
            client_metadata: common_opts.client_metadata,
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
            CommandKind::Kill(cmd) => cmd.exec(matches, command_ctx),
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
            CommandKind::Utargets(cmd) => cmd.exec(matches, command_ctx),
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
