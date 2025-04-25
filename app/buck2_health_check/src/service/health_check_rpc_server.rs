/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(dead_code)]

use std::sync::Arc;

use buck2_grpc::ServerHandle;
use buck2_grpc::spawn_oneshot;
use buck2_grpc::to_tonic;
use buck2_health_check_proto::Empty;
use buck2_health_check_proto::HealthCheckContextEvent;
use buck2_health_check_proto::HealthCheckResult;
use buck2_health_check_proto::health_check_server;
use tokio::io::AsyncRead;
use tokio::io::AsyncWrite;
use tokio::sync::Mutex;

use crate::service::health_check_executor::HealthCheckExecutor;

pub struct HealthCheckRpcServer {
    executor: Arc<Mutex<HealthCheckExecutor>>,
}

impl HealthCheckRpcServer {
    pub fn new() -> Self {
        Self {
            executor: Arc::new(Mutex::new(HealthCheckExecutor::new())),
        }
    }
}

#[async_trait::async_trait]
impl health_check_server::HealthCheck for HealthCheckRpcServer {
    async fn update_context(
        &self,
        request: tonic::Request<HealthCheckContextEvent>,
    ) -> Result<tonic::Response<Empty>, tonic::Status> {
        to_tonic(async move {
            let event = request.into_inner().try_into()?;
            let mut executor = self.executor.lock().await;
            executor.update_context(event).await?;
            Ok(Empty {})
        })
        .await
    }

    async fn run_checks(
        &self,
        _request: tonic::Request<Empty>,
    ) -> Result<tonic::Response<HealthCheckResult>, tonic::Status> {
        to_tonic(async move {
            let reports = self.executor.lock().await.run_checks().await?;
            Ok(HealthCheckResult {
                reports: reports
                    .into_iter()
                    .map(|report| report.try_into())
                    .collect::<Result<Vec<_>, _>>()?,
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
        health_check_server::HealthCheckServer::new(HealthCheckRpcServer::new())
            .max_encoding_message_size(usize::MAX)
            .max_decoding_message_size(usize::MAX),
    );

    spawn_oneshot(io, router)
}
