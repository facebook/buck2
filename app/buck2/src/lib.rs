/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(error_generic_member_access)]
#![feature(used_with_arg)]

use std::thread;

use buck2_audit::AuditCommand;
use buck2_client::commands::build::BuildCommand;
use buck2_client::commands::bxl::BxlCommand;
use buck2_client::commands::clean::CleanCommand;
use buck2_client::commands::ctargets::ConfiguredTargetsCommand;
use buck2_client::commands::debug::DebugCommand;
use buck2_client::commands::expand_external_cell::ExpandExternalCellsCommand;
use buck2_client::commands::explain::ExplainCommand;
use buck2_client::commands::help_env::HelpEnvCommand;
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
use buck2_client_ctx::argfiles::expand_argv;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::client_metadata::ClientMetadata;
use buck2_client_ctx::common::BuckArgMatches;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::immediate_config::ImmediateConfigContext;
use buck2_client_ctx::streaming::BuckSubcommand;
use buck2_client_ctx::tokio_runtime_setup::client_tokio_runtime;
use buck2_client_ctx::version::BuckVersion;
use buck2_cmd_starlark_client::StarlarkCommand;
use buck2_common::argv::Argv;
use buck2_common::invocation_paths_result::InvocationPathsResult;
use buck2_common::invocation_roots::get_invocation_paths_result;
use buck2_core::buck2_env;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_error::buck2_error;
use buck2_error::conversion::from_any_with_tag;
use buck2_error::BuckErrorContext;
use buck2_event_observer::verbosity::Verbosity;
use buck2_util::threads::thread_spawn_scoped;
use clap::CommandFactory;
use clap::FromArgMatches;
use dupe::Dupe;

use crate::check_user_allowed::check_user_allowed;
use crate::process_context::ProcessContext;

mod check_user_allowed;
mod cli_style;
pub(crate) mod commands;
pub mod panic;
pub mod process_context;

fn parse_isolation_dir(s: &str) -> anyhow::Result<FileNameBuf> {
    FileNameBuf::try_from(s.to_owned())
        .buck_error_context_anyhow("isolation dir must be a directory name")
}

/// Options of `buck2` command, before subcommand.
#[derive(Clone, Debug, clap::Parser)]
#[clap(next_help_heading = "Universal Options")]
struct BeforeSubcommandOptions {
    /// The name of the directory that Buck2 creates within buck-out for writing outputs and daemon
    /// information. If one is not provided, Buck2 creates a directory with the default name.
    ///
    /// Instances of Buck2 share a daemon if and only if their isolation directory is identical.
    /// The isolation directory also influences the output paths provided by Buck2,
    /// and as a result using a non-default isolation dir will cause cache misses (and slower builds).
    #[clap(
        value_parser = parse_isolation_dir,
        env("BUCK_ISOLATION_DIR"),
        long,
        default_value="v2"
    )]
    isolation_dir: FileNameBuf,

    /// How verbose buck should be while logging.
    ///
    /// Values:
    /// 0 = Quiet, errors only;
    /// 1 = Show status. Default;
    /// 2 = more info about errors;
    /// 3 = more info about everything;
    /// 4 = more info about everything + stderr;
    ///
    /// It can be combined with specific log items (stderr, full_failed_command, commands, actions,
    /// status, stats, success) to fine-tune the verbosity of the log. Example usage "-v=1,stderr"
    #[clap(
        short = 'v',
        long = "verbose",
        default_value = "1",
        global = true,
        value_parser= Verbosity::try_from_cli
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
    #[clap(env("BUCK2_NO_BUCKD"), long, global(true), hide(true))]
    // Env var is BUCK2_NO_BUCKD instead of NO_BUCKD env var from buck1 because no buckd
    // is not supported for production work for buck2 and lots of places already set
    // NO_BUCKD=1 for buck1.
    no_buckd: bool,

    /// Print buck wrapper help.
    #[clap(skip)] // @oss-enable
    // @oss-disable: #[clap(long)]
    help_wrapper: bool,
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
    version(BuckVersion::get_version()),
    styles = cli_style::get_styles(),
)]
pub(crate) struct Opt {
    #[clap(subcommand)]
    cmd: CommandKind,
    #[clap(flatten)]
    common_opts: BeforeSubcommandOptions,
}

impl Opt {
    pub(crate) fn exec(
        self,
        process: ProcessContext<'_>,
        immediate_config: &ImmediateConfigContext,
        matches: BuckArgMatches<'_>,
        argv: Argv,
    ) -> ExitResult {
        let subcommand_matches = matches.unwrap_subcommand();

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
    let arg0_override = buck2_env!("BUCK2_ARG0")?;
    let expanded_args = expand_argv(
        arg0_override,
        process.args.to_vec(),
        &mut immediate_config,
        process.working_dir,
    )
    .buck_error_context("Error expanding argsfiles")?;

    let argv = Argv {
        argv: process.args.to_vec(),
        expanded_argv: expanded_args,
    };

    let opt = ParsedArgv::parse(argv)?;

    opt.exec(process, &immediate_config)
}

struct ParsedArgv {
    opt: Opt,
    argv: Argv,
    matches: clap::ArgMatches,
}

impl ParsedArgv {
    fn parse(argv: Argv) -> buck2_error::Result<Self> {
        let clap = Opt::command();
        let matches = clap.get_matches_from(argv.expanded_argv.args());

        let opt: Opt = Opt::from_arg_matches(&matches)?;

        if opt.common_opts.help_wrapper {
            return Err(buck2_error!(
                buck2_error::ErrorTag::Tier0,
                "`--help-wrapper` should have been handled by the wrapper"
            ));
        }

        match &opt.cmd {
            #[cfg(not(client_only))]
            CommandKind::Daemon(..) | CommandKind::Forkserver(..) => {}
            CommandKind::Clean(..) => {}
            _ => {
                check_user_allowed()?;
            }
        }

        Ok(ParsedArgv { opt, argv, matches })
    }

    fn exec(
        self,
        process: ProcessContext<'_>,
        immediate_config: &ImmediateConfigContext,
    ) -> ExitResult {
        let expanded_args = self.argv.expanded_argv.clone();
        self.opt.exec(
            process,
            &immediate_config,
            BuckArgMatches::from_clap(&self.matches, &expanded_args),
            self.argv,
        )
    }
}

#[derive(Debug, clap::Subcommand)]
pub(crate) enum CommandKind {
    #[cfg(not(client_only))]
    #[clap(hide = true)]
    Daemon(buck2_daemon::daemon::DaemonCommand),
    #[cfg(not(client_only))]
    #[clap(hide = true)]
    Forkserver(crate::commands::forkserver::ForkserverCommand),
    #[cfg(not(client_only))]
    #[clap(hide = true)]
    InternalTestRunner(crate::commands::internal_test_runner::InternalTestRunnerCommand),
    #[clap(subcommand)]
    Audit(AuditCommand),
    Aquery(AqueryCommand),
    Build(BuildCommand),
    Bxl(BxlCommand),
    // TODO(nga): implement `buck2 help-buckconfig` too
    //   https://www.internalfb.com/tasks/?t=183528129
    HelpEnv(HelpEnvCommand),
    Test(TestCommand),
    Cquery(CqueryCommand),
    Init(InitCommand),
    Explain(ExplainCommand),
    ExpandExternalCell(ExpandExternalCellsCommand),
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
    #[clap(subcommand, hide = true)]
    Debug(DebugCommand),
    #[clap(hide = true)]
    Complete(buck2_cmd_completion_client::complete::CompleteCommand),
    Completion(buck2_cmd_completion_client::completion::CompletionCommand),
    Docs(buck2_cmd_docs::DocsCommand),
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
        matches: BuckArgMatches<'_>,
        argv: Argv,
        common_opts: BeforeSubcommandOptions,
    ) -> ExitResult {
        let paths_result =
            get_invocation_paths_result(process.working_dir, common_opts.isolation_dir.clone());

        // Handle the daemon command earlier: it wants to fork, but the things we do below might
        // want to create threads.
        #[cfg(not(client_only))]
        if let CommandKind::Daemon(cmd) = self {
            return cmd
                .exec(
                    process.log_reload_handle.dupe(),
                    paths_result.get_result()?,
                    false,
                    || {},
                )
                .into();
        }
        thread::scope(|scope| {
            // Spawn a thread to have stack size independent on linker/environment.
            match thread_spawn_scoped("buck2-main", scope, move || {
                self.exec_no_daemon(
                    common_opts,
                    process,
                    immediate_config,
                    matches,
                    argv,
                    paths_result,
                )
            }) {
                Ok(t) => match t.join() {
                    Ok(res) => res,
                    Err(_) => ExitResult::bail("Main thread panicked"),
                },
                Err(e) => ExitResult::bail(format_args!("Failed to start main thread: {}", e)),
            }
        })
    }

    fn exec_no_daemon(
        self,
        common_opts: BeforeSubcommandOptions,
        process: ProcessContext<'_>,
        immediate_config: &ImmediateConfigContext,
        matches: BuckArgMatches<'_>,
        argv: Argv,
        paths: InvocationPathsResult,
    ) -> ExitResult {
        if common_opts.no_buckd {
            // `no_buckd` can't work in a client-only binary
            if let Some(res) = ExitResult::retry_command_with_full_binary()? {
                return res;
            }
        }

        let fb = buck2_common::fbinit::get_or_init_fbcode_globals();

        let runtime = client_tokio_runtime()?;

        let start_in_process_daemon = if common_opts.no_buckd {
            #[cfg(not(client_only))]
            let v = buck2_daemon::no_buckd::start_in_process_daemon(
                immediate_config.daemon_startup_config()?,
                paths.clone().get_result()?,
                &runtime,
            )?;
            #[cfg(client_only)]
            let v = unreachable!(); // case covered above
            #[allow(dead_code)]
            v
        } else {
            None
        };

        let command_ctx = ClientCommandContext::new(
            fb,
            immediate_config,
            paths,
            process.working_dir.clone(),
            common_opts.verbosity,
            start_in_process_daemon,
            argv,
            process.trace_id.dupe(),
            process.stdin,
            process.restarter,
            process.restarted_trace_id.dupe(),
            &runtime,
            common_opts.oncall,
            common_opts.client_metadata,
            common_opts.isolation_dir,
        );

        match self {
            #[cfg(not(client_only))]
            CommandKind::Daemon(..) => unreachable!("Checked earlier"),
            #[cfg(not(client_only))]
            CommandKind::Forkserver(cmd) => cmd
                .exec(matches, command_ctx, process.log_reload_handle.dupe())
                .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))
                .into(),
            #[cfg(not(client_only))]
            CommandKind::InternalTestRunner(cmd) => cmd
                .exec(matches, command_ctx)
                .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))
                .into(),
            CommandKind::Aquery(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Build(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Bxl(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Test(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Cquery(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::HelpEnv(cmd) => cmd.exec(matches, command_ctx),
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
            CommandKind::Complete(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Completion(cmd) => cmd.exec(Opt::command(), matches, command_ctx),
            CommandKind::Docs(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Profile(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Rage(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Init(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Explain(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Install(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Log(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Lsp(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::Subscribe(cmd) => cmd.exec(matches, command_ctx),
            CommandKind::ExpandExternalCell(cmd) => cmd.exec(matches, command_ctx),
        }
    }
}
