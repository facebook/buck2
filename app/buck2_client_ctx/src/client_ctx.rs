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
use buck2_cli_proto::ClientContext;
use buck2_common::invocation_paths::InvocationPaths;
use buck2_common::result::SharedResult;
use buck2_core::error::BUCK2_HARD_ERROR_ENV_VAR;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::working_dir::WorkingDir;
use buck2_event_observer::verbosity::Verbosity;
use buck2_wrapper_common::invocation_id::TraceId;
use dupe::Dupe;

use crate::cleanup_ctx::AsyncCleanupContext;
use crate::common::CommonBuildConfigurationOptions;
use crate::common::HostArchOverride;
use crate::common::HostPlatformOverride;
use crate::daemon::client::connect::BuckdConnectOptions;
use crate::daemon::client::BuckdClientConnector;
use crate::restarter::Restarter;
use crate::stdin::Stdin;
use crate::tokio_runtime_setup::client_tokio_runtime;

pub struct ClientCommandContext<'a> {
    pub init: fbinit::FacebookInit,
    pub paths: SharedResult<InvocationPaths>,
    pub working_dir: WorkingDir,
    pub verbosity: Verbosity,
    /// When set, this function is called to launch in process daemon.
    /// The function returns `Ok` when daemon successfully started
    /// and ready to accept connections.
    pub start_in_process_daemon: Option<Box<dyn FnOnce() -> anyhow::Result<()> + Send + Sync>>,
    pub command_name: String,
    pub sanitized_argv: Vec<String>,
    pub trace_id: TraceId,
    pub argfiles_trace: Vec<AbsNormPathBuf>,
    pub async_cleanup: AsyncCleanupContext,
    pub stdin: &'a mut Stdin,
    pub restarter: &'a mut Restarter,
    pub restarted_trace_id: Option<TraceId>,
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
        let runtime = client_tokio_runtime().unwrap();
        runtime.block_on(func(self))
    }

    pub fn stdin(&mut self) -> &mut Stdin {
        self.stdin
    }

    pub async fn connect_buckd(
        &self,
        options: BuckdConnectOptions,
    ) -> anyhow::Result<BuckdClientConnector> {
        BuckdConnectOptions { ..options }
            .connect(self.paths()?)
            .await
    }

    pub fn client_context(
        &self,
        config_opts: &CommonBuildConfigurationOptions,
        arg_matches: &clap::ArgMatches,
        sanitized_argv: Vec<String>,
    ) -> anyhow::Result<ClientContext> {
        // TODO(cjhopman): Support non unicode paths?
        Ok(ClientContext {
            config_overrides: config_opts.config_overrides(arg_matches)?,
            target_platform: config_opts.target_platforms.clone().unwrap_or_default(),
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
            oncall: config_opts.oncall.as_ref().cloned().unwrap_or_default(),
            disable_starlark_types: config_opts.disable_starlark_types,
            reuse_current_config: config_opts.reuse_current_config,
            sanitized_argv,
            argfiles: self
                .argfiles_trace
                .iter()
                .map(|path| path.to_string())
                .collect(),
            ..self.empty_client_context()?
        })
    }

    /// A client context for commands where CommonConfigOptions are not provided.
    pub fn empty_client_context(&self) -> anyhow::Result<ClientContext> {
        #[derive(Debug, thiserror::Error)]
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
                .ok_or(CurrentDirIsNotUtf8)?
                .to_owned(),
            config_overrides: Default::default(),
            target_platform: Default::default(),
            host_platform: Default::default(),
            host_arch: Default::default(),
            host_xcode_version: Default::default(),
            oncall: Default::default(),
            disable_starlark_types: false,
            trace_id: format!("{}", self.trace_id),
            reuse_current_config: false,
            daemon_uuid,
            sanitized_argv: Vec::new(),
            argfiles: Vec::new(),
            buck2_hard_error: BUCK2_HARD_ERROR_ENV_VAR.get()?.cloned().unwrap_or_default(),
        })
    }

    pub fn async_cleanup_context(&self) -> &AsyncCleanupContext {
        &self.async_cleanup
    }
}
