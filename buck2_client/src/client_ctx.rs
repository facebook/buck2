/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::future::Future;
use std::str::FromStr;

use anyhow::Context;
use buck2_common::invocation_paths::InvocationPaths;
use buck2_common::result::SharedResult;
use buck2_core::fs::paths::AbsPathBuf;
use buck2_core::truncate::truncate_container;
use buck2_events::trace::TraceId;
use cli_proto::client_context::HostPlatformOverride as GrpcHostPlatformOverride;
use cli_proto::ClientContext;
use gazebo::dupe::Dupe;
use tokio::runtime::Builder;

use crate::cleanup_ctx::AsyncCleanupContext;
use crate::cleanup_ctx::AsyncCleanupContextGuard;
use crate::common::CommonBuildConfigurationOptions;
use crate::common::HostPlatformOverride;
use crate::daemon::client::connect::BuckdConnectOptions;
use crate::daemon::client::BuckdClientConnector;
use crate::replayer::Replayer;
use crate::stdin::Stdin;
use crate::verbosity::Verbosity;

/// Contains fields that should only created once per proess and not once per command. We don't put
/// them directly in ClientCommandContext to support Replay.
pub struct ProcessContext {
    async_cleanup: AsyncCleanupContext,
    stdin: Stdin,
}

impl ProcessContext {
    /// NOTE: This returns the AsyncCleanupContextGuard separately since we pass the ProcessContext
    /// down but want to keep the AsyncCleanupContextGuard at the top of the stack.
    pub fn initialize() -> anyhow::Result<(Self, AsyncCleanupContextGuard)> {
        let async_cleanup = AsyncCleanupContextGuard::new();

        Ok((
            Self {
                async_cleanup: async_cleanup.ctx().dupe(),
                stdin: Stdin::new()?,
            },
            async_cleanup,
        ))
    }
}

pub struct ClientCommandContext {
    pub init: fbinit::FacebookInit,
    pub paths: SharedResult<InvocationPaths>,
    pub working_dir: AbsPathBuf,
    pub replayer: Option<sync_wrapper::SyncWrapper<Replayer>>,
    pub verbosity: Verbosity,
    pub replay_speed: Option<f64>,
    pub process_context: ProcessContext,
    /// When set, this function is called to launch in process daemon.
    /// The function returns `Ok` when daemon successfully started
    /// and ready to accept connections.
    pub start_in_process_daemon: Option<Box<dyn FnOnce() -> anyhow::Result<()> + Send + Sync>>,
    pub command_name: String,
}

impl ClientCommandContext {
    pub fn fbinit(&self) -> fbinit::FacebookInit {
        self.init
    }

    pub fn paths(&self) -> SharedResult<&InvocationPaths> {
        match &self.paths {
            Ok(p) => Ok(p),
            Err(e) => Err(e.dupe()),
        }
    }

    pub fn with_runtime<Fut: Future, F: FnOnce(ClientCommandContext) -> Fut>(
        self,
        func: F,
    ) -> <Fut as Future>::Output {
        let runtime = Builder::new_current_thread()
            .enable_all()
            .build()
            .expect("Should be able to start a runtime");
        runtime.block_on(func(self))
    }

    pub fn stdin(&mut self) -> &mut Stdin {
        &mut self.process_context.stdin
    }

    pub async fn connect_buckd(
        &self,
        options: BuckdConnectOptions,
    ) -> anyhow::Result<BuckdClientConnector> {
        BuckdConnectOptions { ..options }
            .connect(self.paths()?)
            .await
        .context("Failed to connect to buck daemon. Try running `buck2 clean` and your command afterwards. Alternatively, try running `rm -rf ~/.buck/buckd` and your command afterwards")
    }

    pub fn client_context(
        &self,
        config_opts: &CommonBuildConfigurationOptions,
        arg_matches: &clap::ArgMatches,
    ) -> anyhow::Result<ClientContext> {
        let config_overrides = config_opts.config_overrides(arg_matches)?;
        if !config_overrides.is_empty() && config_opts.reuse_current_config {
            tracing::warn!(
                "Found config overrides while using --reuse_current_config flag. Ignoring overrides [{}] and using current config instead",
                truncate_container(config_overrides.iter().map(|e| &*e.config_override), 200),
            );
        }

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
            reuse_current_config: config_opts.reuse_current_config,
            ..self.empty_client_context()?
        })
    }

    /// A client context for commands where CommonConfigOptions are not provided.
    pub fn empty_client_context(&self) -> anyhow::Result<ClientContext> {
        #[derive(Debug, thiserror::Error)]
        #[error("Current directory is not UTF-8")]
        struct CurrentDirIsNotUtf8;

        let trace_id = match std::env::var("BUCK_WRAPPER_UUID") {
            Ok(uuid_str) => {
                TraceId::from_str(&uuid_str).context("invalid trace ID in BUCK_WRAPPER_UUID")?
            }
            _ => TraceId::new(),
        };

        Ok(ClientContext {
            working_dir: self
                .working_dir
                .to_str()
                .ok_or(CurrentDirIsNotUtf8)?
                .to_owned(),
            config_overrides: Default::default(),
            target_platform: Default::default(),
            host_platform: Default::default(),
            oncall: Default::default(),
            disable_starlark_types: false,
            trace_id: format!("{}", trace_id),
            reuse_current_config: false,
        })
    }

    pub fn async_cleanup_context(&self) -> &AsyncCleanupContext {
        &self.process_context.async_cleanup
    }
}
