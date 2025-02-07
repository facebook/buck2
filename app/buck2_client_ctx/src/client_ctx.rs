/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::future::Future;

use buck2_cli_proto::client_context::HostArchOverride as GrpcHostArchOverride;
use buck2_cli_proto::client_context::HostPlatformOverride as GrpcHostPlatformOverride;
use buck2_cli_proto::client_context::PreemptibleWhen as GrpcPreemptibleWhen;
use buck2_cli_proto::ClientContext;
use buck2_common::argv::Argv;
use buck2_common::init::LogDownloadMethod;
use buck2_common::invocation_paths::InvocationPaths;
use buck2_common::invocation_paths_result::InvocationPathsResult;
use buck2_core::error::buck2_hard_error_env;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_core::fs::working_dir::AbsWorkingDir;
use buck2_error::BuckErrorContext;
use buck2_event_observer::verbosity::Verbosity;
use buck2_util::cleanup_ctx::AsyncCleanupContext;
use buck2_wrapper_common::invocation_id::TraceId;
use dupe::Dupe;
use tokio::runtime::Runtime;

use crate::client_metadata::ClientMetadata;
use crate::common::ui::CommonConsoleOptions;
use crate::common::BuckArgMatches;
use crate::common::CommonEventLogOptions;
use crate::common::HostArchOverride;
use crate::common::HostPlatformOverride;
use crate::common::PreemptibleWhen;
use crate::console_interaction_stream::ConsoleInteractionStream;
use crate::daemon::client::connect::BuckdConnectOptions;
use crate::daemon::client::BuckdClientConnector;
use crate::daemon_constraints::get_possibly_nested_invocation_daemon_uuid;
use crate::immediate_config::ImmediateConfigContext;
use crate::restarter::Restarter;
use crate::stdin::Stdin;
use crate::streaming::StreamingCommand;
use crate::subscribers::recorder::try_get_invocation_recorder;

pub struct ClientCommandContext<'a> {
    init: fbinit::FacebookInit,
    pub immediate_config: &'a ImmediateConfigContext<'a>,
    paths: InvocationPathsResult,
    pub working_dir: AbsWorkingDir,
    pub verbosity: Verbosity,
    /// When set, this function is called to launch in process daemon.
    /// The function returns `Ok` when daemon successfully started
    /// and ready to accept connections.
    pub(crate) start_in_process_daemon:
        Option<Box<dyn FnOnce() -> buck2_error::Result<()> + Send + Sync>>,
    pub(crate) argv: Argv,
    pub trace_id: TraceId,
    async_cleanup: AsyncCleanupContext<'a>,
    stdin: &'a mut Stdin,
    pub(crate) restarter: &'a mut Restarter,
    pub(crate) restarted_trace_id: Option<TraceId>,
    runtime: &'a Runtime,
    oncall: Option<String>,
    pub(crate) client_metadata: Vec<ClientMetadata>,
    pub(crate) isolation: FileNameBuf,
}

impl<'a> ClientCommandContext<'a> {
    pub fn new(
        init: fbinit::FacebookInit,
        immediate_config: &'a ImmediateConfigContext<'a>,
        paths: InvocationPathsResult,
        working_dir: AbsWorkingDir,
        verbosity: Verbosity,
        start_in_process_daemon: Option<Box<dyn FnOnce() -> buck2_error::Result<()> + Send + Sync>>,
        argv: Argv,
        trace_id: TraceId,
        async_cleanup: AsyncCleanupContext<'a>,
        stdin: &'a mut Stdin,
        restarter: &'a mut Restarter,
        restarted_trace_id: Option<TraceId>,
        runtime: &'a Runtime,
        oncall: Option<String>,
        client_metadata: Vec<ClientMetadata>,
        isolation: FileNameBuf,
    ) -> Self {
        ClientCommandContext {
            init,
            immediate_config,
            paths,
            working_dir,
            verbosity,
            start_in_process_daemon,
            argv,
            trace_id,
            async_cleanup,
            stdin,
            restarter,
            restarted_trace_id,
            runtime,
            oncall,
            client_metadata,
            isolation,
        }
    }

    pub fn fbinit(&self) -> fbinit::FacebookInit {
        self.init
    }

    pub fn paths(&self) -> buck2_error::Result<&InvocationPaths> {
        match &self.paths {
            InvocationPathsResult::Paths(p) => Ok(p),
            InvocationPathsResult::OutsideOfRepo(e) | InvocationPathsResult::OtherError(e) => {
                Err(e.dupe().into())
            }
        }
    }

    pub fn maybe_paths(&self) -> buck2_error::Result<Option<&InvocationPaths>> {
        match &self.paths {
            InvocationPathsResult::Paths(p) => Ok(Some(p)),
            InvocationPathsResult::OutsideOfRepo(_) => Ok(None), // commands like log don't need a root but still need to create an invocation record
            InvocationPathsResult::OtherError(e) => Err(e.dupe().into()),
        }
    }

    pub fn with_runtime<Fut, F>(self, func: F) -> <Fut as Future>::Output
    where
        Fut: Future + 'a,
        F: FnOnce(ClientCommandContext<'a>) -> Fut,
    {
        self.runtime.block_on(func(self))
    }

    pub fn instant_command<Fut, F>(
        self,
        command_name: &'static str,
        event_log_opts: &CommonEventLogOptions,
        func: F,
    ) -> buck2_error::Result<()>
    where
        Fut: Future<Output = buck2_error::Result<()>> + 'a,
        F: FnOnce(ClientCommandContext<'a>) -> Fut,
    {
        let mut recorder = try_get_invocation_recorder(
            &self,
            &event_log_opts,
            command_name,
            std::env::args().collect(),
            Vec::new(),
            None,
        )?;

        recorder.update_metadata_from_client_metadata(&self.client_metadata);

        let result = self.with_runtime(func);

        recorder.instant_command_outcome(result.is_ok());
        result.into()
    }

    /// Invoke a command without writing event log.
    /// (For example, we don't write logs in `buck2 log` command.)
    pub fn instant_command_no_log<Fut, F>(
        self,
        command_name: &'static str,
        func: F,
    ) -> buck2_error::Result<()>
    where
        Fut: Future<Output = buck2_error::Result<()>> + 'a,
        F: FnOnce(ClientCommandContext<'a>) -> Fut,
    {
        self.instant_command(
            command_name,
            &CommonEventLogOptions {
                no_event_log: true,
                ..CommonEventLogOptions::default()
            },
            func,
        )
    }

    pub fn stdin(&mut self) -> &mut Stdin {
        self.stdin
    }

    pub fn console_interaction_stream(
        &mut self,
        opts: &CommonConsoleOptions,
    ) -> Option<ConsoleInteractionStream<'_>> {
        if opts.no_interactive_console {
            tracing::debug!("Disabling console interaction: no_interactive_console is set");
            return None;
        }

        ConsoleInteractionStream::new(self.stdin)
    }

    pub async fn connect_buckd(
        &self,
        options: BuckdConnectOptions<'a>,
    ) -> buck2_error::Result<BuckdClientConnector<'a>> {
        BuckdConnectOptions { ..options }
            .connect(self.paths()?)
            .await
    }

    pub fn client_context<T: StreamingCommand>(
        &self,
        arg_matches: BuckArgMatches<'_>,
        cmd: &T,
    ) -> buck2_error::Result<ClientContext> {
        // TODO(cjhopman): Support non unicode paths?
        let config_opts = cmd.build_config_opts();
        let starlark_opts = cmd.starlark_opts();

        Ok(ClientContext {
            config_overrides: config_opts.config_overrides(
                arg_matches,
                &self.immediate_config,
                &self.working_dir,
            )?,
            host_platform: match config_opts.host_platform_override() {
                HostPlatformOverride::Default => GrpcHostPlatformOverride::DefaultPlatform,
                HostPlatformOverride::Linux => GrpcHostPlatformOverride::Linux,
                HostPlatformOverride::MacOs => GrpcHostPlatformOverride::MacOs,
                HostPlatformOverride::Windows => GrpcHostPlatformOverride::Windows,
            }
            .into(),
            host_arch: match config_opts.host_arch_override() {
                HostArchOverride::Default => GrpcHostArchOverride::DefaultArch,
                HostArchOverride::X86_64 => GrpcHostArchOverride::X8664,
                HostArchOverride::AArch64 => GrpcHostArchOverride::AArch64,
            }
            .into(),
            host_xcode_version: config_opts.host_xcode_version_override(),
            disable_starlark_types: starlark_opts.disable_starlark_types,
            unstable_typecheck: starlark_opts.unstable_typecheck,
            skip_targets_with_duplicate_names: starlark_opts.skip_targets_with_duplicate_names,
            reuse_current_config: config_opts.reuse_current_config,
            sanitized_argv: cmd.sanitize_argv(self.argv.clone()).argv,
            exit_when_different_state: config_opts.exit_when_different_state,
            preemptible: match config_opts.preemptible {
                None => GrpcPreemptibleWhen::Never,
                Some(PreemptibleWhen::Never) => GrpcPreemptibleWhen::Never,
                Some(PreemptibleWhen::Always) => GrpcPreemptibleWhen::Always,
                Some(PreemptibleWhen::OnDifferentState) => GrpcPreemptibleWhen::OnDifferentState,
            }
            .into(),
            argfiles: self
                .immediate_config
                .trace()
                .iter()
                .map(|path| path.to_string())
                .collect(),
            target_call_stacks: starlark_opts.target_call_stacks,
            representative_config_flags: arg_matches.get_representative_config_flags_by_source()?,
            ..self.empty_client_context(cmd.logging_name())?
        })
    }

    /// A client context for commands where CommonConfigOptions are not provided.
    pub fn empty_client_context(&self, command_name: &str) -> buck2_error::Result<ClientContext> {
        #[derive(Debug, buck2_error::Error)]
        #[error("Current directory is not UTF-8")]
        #[buck2(tag = Input)]
        struct CurrentDirIsNotUtf8;

        Ok(ClientContext {
            working_dir: self
                .working_dir
                .path()
                .to_str()
                .buck_error_context(CurrentDirIsNotUtf8.to_string())?
                .to_owned(),
            config_overrides: Default::default(),
            host_platform: Default::default(),
            host_arch: Default::default(),
            host_xcode_version: Default::default(),
            oncall: self.oncall.clone().unwrap_or_default(), // TODO: Why do we not make this optional?
            disable_starlark_types: false,
            unstable_typecheck: false,
            target_call_stacks: false,
            skip_targets_with_duplicate_names: false,
            trace_id: format!("{}", self.trace_id),
            reuse_current_config: false,
            daemon_uuid: get_possibly_nested_invocation_daemon_uuid(),
            sanitized_argv: Vec::new(),
            argfiles: Vec::new(),
            buck2_hard_error: buck2_hard_error_env()?.unwrap_or_default().to_owned(),
            command_name: command_name.to_owned(),
            exit_when_different_state: false,
            client_metadata: self
                .client_metadata
                .iter()
                .map(ClientMetadata::to_proto)
                .collect(),
            preemptible: Default::default(),
            representative_config_flags: Vec::new(),
        })
    }

    pub fn async_cleanup_context(&self) -> &AsyncCleanupContext<'a> {
        &self.async_cleanup
    }

    pub fn log_download_method(&self) -> LogDownloadMethod {
        self.immediate_config
            .daemon_startup_config()
            .unwrap()
            .log_download_method
            .clone()
    }
}
