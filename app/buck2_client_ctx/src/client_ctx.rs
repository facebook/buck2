/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::future::Future;

use anyhow::Context as _;
use buck2_cli_proto::client_context::HostArchOverride as GrpcHostArchOverride;
use buck2_cli_proto::client_context::HostPlatformOverride as GrpcHostPlatformOverride;
use buck2_cli_proto::ClientContext;
use buck2_common::argv::Argv;
use buck2_common::invocation_paths::InvocationPaths;
use buck2_core::error::buck2_hard_error_env;
use buck2_core::fs::working_dir::WorkingDir;
use buck2_event_observer::verbosity::Verbosity;
use buck2_util::cleanup_ctx::AsyncCleanupContext;
use buck2_wrapper_common::invocation_id::TraceId;
use dupe::Dupe;
use tokio::runtime::Runtime;

use crate::client_metadata::ClientMetadata;
use crate::common::CommonEventLogOptions;
use crate::common::HostArchOverride;
use crate::common::HostPlatformOverride;
use crate::daemon::client::connect::BuckdConnectOptions;
use crate::daemon::client::BuckdClientConnector;
use crate::exit_result::ExitResult;
use crate::immediate_config::ImmediateConfigContext;
use crate::restarter::Restarter;
use crate::stdin::Stdin;
use crate::streaming::StreamingCommand;
use crate::subscribers::recorder::try_get_invocation_recorder;

pub struct ClientCommandContext<'a> {
    pub init: fbinit::FacebookInit,
    pub immediate_config: &'a ImmediateConfigContext<'a>,
    pub paths: buck2_error::Result<InvocationPaths>,
    pub working_dir: WorkingDir,
    pub verbosity: Verbosity,
    /// When set, this function is called to launch in process daemon.
    /// The function returns `Ok` when daemon successfully started
    /// and ready to accept connections.
    pub start_in_process_daemon: Option<Box<dyn FnOnce() -> anyhow::Result<()> + Send + Sync>>,
    pub argv: Argv,
    pub trace_id: TraceId,
    pub async_cleanup: AsyncCleanupContext<'a>,
    pub stdin: &'a mut Stdin,
    pub restarter: &'a mut Restarter,
    pub restarted_trace_id: Option<TraceId>,
    pub runtime: &'a Runtime,
    pub oncall: Option<String>,
    pub client_metadata: Vec<ClientMetadata>,
}

impl<'a> ClientCommandContext<'a> {
    pub fn fbinit(&self) -> fbinit::FacebookInit {
        self.init
    }

    pub fn paths(&self) -> anyhow::Result<&InvocationPaths> {
        match &self.paths {
            Ok(p) => Ok(p),
            Err(e) => Err(e.dupe().into()),
        }
    }

    pub fn with_runtime<Fut, F>(self, func: F) -> <Fut as Future>::Output
    where
        Fut: Future + 'a,
        F: FnOnce(ClientCommandContext<'a>) -> Fut,
    {
        self.runtime.block_on(func(self))
    }

    pub fn instant_command<Fut, F>(self, command_name: &'static str, func: F) -> ExitResult
    where
        Fut: Future<Output = anyhow::Result<()>> + 'a,
        F: FnOnce(ClientCommandContext<'a>) -> Fut,
    {
        let mut recorder = try_get_invocation_recorder(
            &self,
            CommonEventLogOptions::default_ref(),
            command_name,
            std::env::args().collect(),
            None,
        )?;

        let result = self.runtime.block_on(func(self));

        recorder.instant_command_outcome(result.is_ok());
        result.into()
    }

    pub fn stdin(&mut self) -> &mut Stdin {
        self.stdin
    }

    pub async fn connect_buckd(
        &self,
        options: BuckdConnectOptions<'a>,
    ) -> anyhow::Result<BuckdClientConnector<'a>> {
        BuckdConnectOptions { ..options }
            .connect(self.paths()?)
            .await
    }

    pub fn client_context<T: StreamingCommand>(
        &self,
        arg_matches: &clap::ArgMatches,
        cmd: &T,
    ) -> anyhow::Result<ClientContext> {
        // TODO(cjhopman): Support non unicode paths?
        let config_opts = cmd.build_config_opts();
        let starlark_opts = cmd.starlark_opts();
        Ok(ClientContext {
            config_overrides: config_opts.config_overrides(arg_matches)?,
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
            argfiles: self
                .immediate_config
                .trace()
                .iter()
                .map(|path| path.to_string())
                .collect(),
            target_call_stacks: starlark_opts.target_call_stacks,
            ..self.empty_client_context(cmd.logging_name())?
        })
    }

    /// A client context for commands where CommonConfigOptions are not provided.
    pub fn empty_client_context(&self, command_name: &str) -> anyhow::Result<ClientContext> {
        #[derive(Debug, buck2_error::Error)]
        #[error("Current directory is not UTF-8")]
        struct CurrentDirIsNotUtf8;

        let daemon_uuid = match std::env::var("BUCK2_DAEMON_UUID") {
            Ok(daemon_uuid) => Some(daemon_uuid),
            _ => None,
        };

        Ok(ClientContext {
            working_dir: self
                .working_dir
                .path()
                .to_str()
                .context(CurrentDirIsNotUtf8)?
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
            daemon_uuid,
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
        })
    }

    pub fn async_cleanup_context(&self) -> &AsyncCleanupContext<'a> {
        &self.async_cleanup
    }

    pub fn allow_vpnless(&self) -> anyhow::Result<bool> {
        Ok(self.immediate_config.daemon_startup_config()?.allow_vpnless)
    }
}
