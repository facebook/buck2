/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(dead_code)]

use buck2_grpc::spawn_oneshot;
use buck2_grpc::to_tonic;
use buck2_grpc::ServerHandle;
use buck2_health_check_proto::health_check_server;
use buck2_health_check_proto::Empty;
use buck2_health_check_proto::HealthCheckContextEvent;
use buck2_health_check_proto::HealthCheckResult;
use tokio::io::AsyncRead;
use tokio::io::AsyncWrite;

pub struct HealthCheckServer {}

impl HealthCheckServer {
    pub fn new() -> Self {
        Self {}
    }
}

#[async_trait::async_trait]
impl health_check_server::HealthCheck for HealthCheckServer {
    async fn update_context(
        &self,
        _request: tonic::Request<HealthCheckContextEvent>,
    ) -> Result<tonic::Response<Empty>, tonic::Status> {
        // TODO(rajneeshl): Forward the calls to a health check executor.
        to_tonic(async move { Ok(Empty {}) }).await
    }

    async fn run_checks(
        &self,
        _request: tonic::Request<Empty>,
    ) -> Result<tonic::Response<HealthCheckResult>, tonic::Status> {
        // TODO(rajneeshl): Forward the calls to a health check executor.
        to_tonic(async move {
            Ok(HealthCheckResult {
                ..Default::default()
            })
        })
        .await
    }
}

pub fn spawn_health_check_server<I>(io: I) -> ServerHandle
where
    I: AsyncRead + AsyncWrite + Send + Unpin + 'static + tonic::transport::server::Connected,
{
    let router = tonic::transport::Server::builder().add_service(
        health_check_server::HealthCheckServer::new(HealthCheckServer::new())
            .max_encoding_message_size(usize::MAX)
            .max_decoding_message_size(usize::MAX),
    );

    spawn_oneshot(io, router)
}
