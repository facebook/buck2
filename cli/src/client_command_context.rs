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
use buck2_client::verbosity::Verbosity;
use buck2_common::result::SharedResult;
use buck2_events::TraceId;
use buck2_server::paths::InvocationPaths;
use cli_proto::client_context::HostPlatformOverride as GrpcHostPlatformOverride;
use cli_proto::ClientContext;
use dice::cycles::DetectCycles;
use gazebo::dupe::Dupe;
use tokio::runtime::Builder;

use crate::commands::common::CommonBuildConfigurationOptions;
use crate::commands::common::HostPlatformOverride;
use crate::daemon::client::replayer::Replayer;
use crate::daemon::client::BuckdClientConnector;
use crate::daemon::client::BuckdConnectOptions;
use crate::AsyncCleanupContext;

pub(crate) struct ClientCommandContext {
    pub(crate) init: fbinit::FacebookInit,
    pub(crate) paths: SharedResult<InvocationPaths>,
    pub(crate) detect_cycles: Option<DetectCycles>,
    pub(crate) replayer: Option<sync_wrapper::SyncWrapper<Replayer>>,
    pub(crate) verbosity: Verbosity,
    pub(crate) replay_speed: Option<f64>,
    pub(crate) async_cleanup_context: AsyncCleanupContext,
}

impl ClientCommandContext {
    pub(crate) fn fbinit(&self) -> fbinit::FacebookInit {
        self.init
    }

    pub(crate) fn paths(&self) -> SharedResult<&InvocationPaths> {
        match &self.paths {
            Ok(p) => Ok(p),
            Err(e) => Err(e.dupe()),
        }
    }

    pub(crate) fn with_runtime<Fut: Future, F: FnOnce(ClientCommandContext) -> Fut>(
        self,
        func: F,
    ) -> <Fut as Future>::Output {
        let runtime = Builder::new_current_thread()
            .enable_all()
            .build()
            .expect("Should be able to start a runtime");
        runtime.block_on(func(self))
    }

    pub(crate) async fn connect_buckd(
        &self,
        options: BuckdConnectOptions,
    ) -> anyhow::Result<BuckdClientConnector> {
        BuckdConnectOptions { ..options }
            .connect(self.paths()?)
            .await
        .context("Failed to connect to buck daemon. Try running `buck2 clean` and your command afterwards. Alternatively, try running `rm -rf ~/.buck/buckd` and your command afterwards")
    }

    pub(crate) fn client_context(
        &self,
        config_opts: &CommonBuildConfigurationOptions,
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
    pub(crate) fn empty_client_context(&self) -> anyhow::Result<ClientContext> {
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
            working_dir: std::env::current_dir()?
                .to_str()
                .ok_or(CurrentDirIsNotUtf8)?
                .to_owned(),
            config_overrides: Default::default(),
            target_platform: Default::default(),
            host_platform: Default::default(),
            oncall: Default::default(),
            disable_starlark_types: false,
            trace_id: format!("{}", trace_id),
        })
    }

    pub(crate) fn async_cleanup_context(&self) -> &AsyncCleanupContext {
        &self.async_cleanup_context
    }
}
