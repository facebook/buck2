/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(dead_code)]

use std::net::SocketAddr;

use buck2_core::async_once_cell::AsyncOnceCell;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_path::AbsPathBuf;
use buck2_core::soft_error;
use buck2_error::BuckErrorContext;
use buck2_error::ErrorTag;
use buck2_error::conversion::from_any_with_tag;
use buck2_health_check_proto::health_check_client::HealthCheckClient;
use buck2_util::properly_reaped_child::ProperlyReapedChild;
use buck2_util::properly_reaped_child::reap_on_drop_command;
use dupe::Dupe;
use futures::FutureExt;
use tokio::net::TcpListener;
use tonic::transport::Channel;

use crate::interface::HealthCheckContextEvent;
use crate::interface::HealthCheckService;
use crate::report::Report;

const CLI_NAME: &str = "buck2-health-check";

struct HealthCheckServerConnection {
    pub(crate) rpc_client: HealthCheckClient<Channel>,
    // The process will be reaped when the client is dropped.
    pub(crate) process: ProperlyReapedChild,
}

pub(crate) struct HealthCheckRpcClient {
    // The connection is lazily created when the first event to update the context or to run checks is received.
    connection: AsyncOnceCell<buck2_error::Result<HealthCheckServerConnection>>,
}

impl HealthCheckRpcClient {
    pub fn new() -> Self {
        Self {
            connection: AsyncOnceCell::new(),
        }
    }

    async fn connection(&self) -> buck2_error::Result<&HealthCheckServerConnection> {
        let init_fut = async move {
            Self::spawn_out_of_process_health_check_server()
                .boxed()
                .await
        };
        match self.connection.get_or_init(init_fut).await {
            Ok(v) => Ok(v),
            Err(e) => Err(e.dupe().into()),
        }
    }

    async fn rpc_client(&self) -> buck2_error::Result<HealthCheckClient<Channel>> {
        Ok(self.connection().await?.rpc_client.clone())
    }

    async fn spawn_out_of_process_health_check_server()
    -> buck2_error::Result<HealthCheckServerConnection> {
        let (addr, tcp_listener) = Self::create_tcp_listener().await?;

        let exe = Self::get_cli_path()?;
        let process = reap_on_drop_command(&exe, &["--io-addr", &addr], None)?;

        let (tcp_stream, _) = tcp_listener.accept().await?;
        let channel = buck2_grpc::make_channel(tcp_stream, "health_check")
            .await
            .map_err(|e| from_any_with_tag(e, ErrorTag::HealthCheck))?;
        let rpc_client = HealthCheckClient::new(channel)
            .max_encoding_message_size(usize::MAX)
            .max_decoding_message_size(usize::MAX);
        Ok(HealthCheckServerConnection {
            rpc_client,
            process,
        })
    }

    async fn create_tcp_listener() -> buck2_error::Result<(String, TcpListener)> {
        let addr: SocketAddr = "127.0.0.1:0"
            .parse()
            .map_err(|e| from_any_with_tag(e, ErrorTag::HealthCheck))?;
        let tcp_listener = TcpListener::bind(addr)
            .await
            .map_err(|e| from_any_with_tag(e, ErrorTag::HealthCheck))?;
        let local_addr = tcp_listener
            .local_addr()
            .map_err(|e| from_any_with_tag(e, ErrorTag::HealthCheck))?;
        Ok((local_addr.to_string(), tcp_listener))
    }

    fn get_cli_path() -> buck2_error::Result<String> {
        if let Ok(overridden_path) = std::env::var("BUCK2_HEALTH_CHECK_CLI_PATH") {
            // If there is a custom CLI path, use it. This is useful for testing new health checks and CLI changes.
            return Ok(overridden_path);
        }

        let exe = AbsPathBuf::new(
            std::env::current_exe().buck_error_context("Cannot get Buck2 executable")?,
        )?;
        let exe = fs_util::canonicalize(&exe).buck_error_context(
            "Failed to canonicalize path to Buck2 executable. Try running `buck2 kill`.",
        )?;

        let exe = exe.as_abs_path();
        let exe_dir = exe
            .parent()
            .buck_error_context("Buck2 executable directory has no parent")?;

        let ext = if cfg!(windows) { ".exe" } else { "" };
        let cli_name = format!("{}{}", CLI_NAME, ext);

        Ok(exe_dir.join(cli_name).to_string())
    }
}

#[async_trait::async_trait]
impl HealthCheckService for HealthCheckRpcClient {
    async fn update_context(&mut self, event: HealthCheckContextEvent) -> buck2_error::Result<()> {
        let rpc_event: buck2_health_check_proto::HealthCheckContextEvent = event.try_into()?;
        self.rpc_client()
            .await?
            .update_context(rpc_event)
            .await
            .map_err(|e| from_any_with_tag(e, ErrorTag::HealthCheck))?;
        Ok(())
    }

    async fn run_checks(&mut self) -> buck2_error::Result<Vec<Report>> {
        let empty_request = buck2_health_check_proto::Empty {};
        let mut reports = Vec::new();
        let response = self
            .rpc_client()
            .await?
            .run_checks(empty_request)
            .await
            .map_err(|e| from_any_with_tag(e, ErrorTag::HealthCheck))?;

        let result = response.into_inner();
        for report in result.reports {
            match Report::try_from(report) {
                Ok(report) => reports.push(report),
                Err(e) => {
                    soft_error!("health_check_rpc_client", e)?;
                }
            }
        }
        Ok(reports)
    }
}
