/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::task::Context;
use std::task::Poll;
use std::time::Duration;

use anyhow::Context as _;
use buck2_downward_api::DownwardApi;
use buck2_downward_api_proto::downward_api_client;
use buck2_downward_api_proto::downward_api_server;
use buck2_downward_api_proto::ConsoleRequest;
use buck2_downward_api_proto::ExternalEventRequest;
use buck2_downward_api_proto::LogRequest;
use buck2_events::dispatch::with_dispatcher_async;
use buck2_events::dispatch::EventDispatcher;
use buck2_grpc::make_channel;
use buck2_grpc::spawn_oneshot;
use buck2_grpc::to_tonic;
use buck2_grpc::ServerHandle;
use buck2_test_proto::test_orchestrator_client;
use buck2_test_proto::test_orchestrator_server;
use buck2_test_proto::AttachInfoMessageRequest;
use buck2_test_proto::Empty;
use buck2_test_proto::EndOfTestResultsRequest;
use buck2_test_proto::ExecuteResponse2;
use buck2_test_proto::PrepareForLocalExecutionResponse;
use buck2_test_proto::ReportTestResultRequest;
use buck2_test_proto::ReportTestSessionRequest;
use buck2_test_proto::ReportTestsDiscoveredRequest;
use buck2_test_proto::Testing;
use dupe::Dupe;
use futures::future::BoxFuture;
use futures::future::FutureExt;
use host_sharing::HostSharingRequirements;
use sorted_vector_map::SortedVectorMap;
use tokio::io::AsyncRead;
use tokio::io::AsyncWrite;
use tonic::transport::Channel;
use tower_layer::Layer;
use tracing::Level;

use crate::data::ArgValue;
use crate::data::ConfiguredTargetHandle;
use crate::data::DeclaredOutput;
use crate::data::DisplayMetadata;
use crate::data::ExecuteRequest2;
use crate::data::ExecuteResponse;
use crate::data::ExecutorConfigOverride;
use crate::data::PrepareForLocalExecutionResult;
use crate::data::RequiredLocalResources;
use crate::data::TestExecutable;
use crate::data::TestResult;
use crate::protocol::TestOrchestrator;

/// Test runner client to buck2 test orchestrator.
///
/// When running `buck2 test`, buck2 starts a gRPC server
/// and spawns a test runner which connects to buck2 process using this client.
pub struct TestOrchestratorClient {
    test_orchestrator_client: test_orchestrator_client::TestOrchestratorClient<Channel>,
    downward_api_client: downward_api_client::DownwardApiClient<Channel>,
}

impl TestOrchestratorClient {
    pub async fn new<T>(io: T) -> anyhow::Result<Self>
    where
        T: AsyncRead + AsyncWrite + Send + Sync + Unpin + 'static,
    {
        let channel = make_channel(io, "orchestrator").await?;

        Ok(Self {
            test_orchestrator_client: test_orchestrator_client::TestOrchestratorClient::new(
                channel.clone(),
            )
            .max_encoding_message_size(usize::MAX)
            .max_decoding_message_size(usize::MAX),
            downward_api_client: downward_api_client::DownwardApiClient::new(channel)
                .max_encoding_message_size(usize::MAX)
                .max_decoding_message_size(usize::MAX),
        })
    }
}

#[async_trait::async_trait]
impl DownwardApi for TestOrchestratorClient {
    async fn console(&self, level: Level, message: String) -> anyhow::Result<()> {
        let level = level.try_into().context("Invalid `level`")?;

        self.downward_api_client
            .clone()
            .console(ConsoleRequest {
                level: Some(level),
                message,
            })
            .await?;

        Ok(())
    }

    async fn log(&self, level: Level, message: String) -> anyhow::Result<()> {
        let level = level.try_into().context("Invalid `level`")?;

        self.downward_api_client
            .clone()
            .log(LogRequest {
                level: Some(level),
                message,
            })
            .await?;

        Ok(())
    }

    async fn external(&self, data: HashMap<String, String>) -> anyhow::Result<()> {
        let event = data.into();

        self.downward_api_client
            .clone()
            .external_event(ExternalEventRequest { event: Some(event) })
            .await?;

        Ok(())
    }
}

impl TestOrchestratorClient {
    pub async fn execute2(
        &self,
        ui_prints: DisplayMetadata,
        target: ConfiguredTargetHandle,
        cmd: Vec<ArgValue>,
        env: SortedVectorMap<String, ArgValue>,
        timeout: Duration,
        host_sharing_requirements: HostSharingRequirements,
        pre_create_dirs: Vec<DeclaredOutput>,
        executor_override: Option<ExecutorConfigOverride>,
        required_local_resources: RequiredLocalResources,
    ) -> anyhow::Result<ExecuteResponse> {
        let test_executable = TestExecutable {
            ui_prints,
            target,
            cmd,
            env,
            pre_create_dirs,
        };

        let req = ExecuteRequest2 {
            test_executable,
            timeout,
            host_sharing_requirements,
            executor_override,
            required_local_resources,
        };

        let req: buck2_test_proto::ExecuteRequest2 =
            req.try_into().context("Invalid execute request")?;

        let ExecuteResponse2 { response } = self
            .test_orchestrator_client
            .clone()
            .execute2(req)
            .await?
            .into_inner();

        let response = match response.context("Missing `response`")? {
            buck2_test_proto::execute_response2::Response::Result(res) => {
                ExecuteResponse::Result(res.try_into().context("Invalid `result`")?)
            }
            buck2_test_proto::execute_response2::Response::Cancelled(
                buck2_test_proto::Cancelled {},
            ) => ExecuteResponse::Cancelled,
        };

        Ok(response)
    }

    pub async fn report_test_result(&self, result: TestResult) -> anyhow::Result<()> {
        let result = result.try_into().context("Invalid `result`")?;

        self.test_orchestrator_client
            .clone()
            .report_test_result(ReportTestResultRequest {
                result: Some(result),
            })
            .await?;

        Ok(())
    }

    pub async fn report_tests_discovered(
        &self,
        target: ConfiguredTargetHandle,
        suite: String,
        tests: Vec<String>,
    ) -> anyhow::Result<()> {
        let target = target.try_into().context("Invalid `target`")?;

        self.test_orchestrator_client
            .clone()
            .report_tests_discovered(ReportTestsDiscoveredRequest {
                target: Some(target),
                testing: Some(Testing {
                    suite,
                    testcases: tests,
                }),
            })
            .await?;

        Ok(())
    }

    pub async fn report_test_session(&self, session_info: String) -> anyhow::Result<()> {
        self.test_orchestrator_client
            .clone()
            .report_test_session(ReportTestSessionRequest { session_info })
            .await?;

        Ok(())
    }

    pub async fn end_of_test_results(&self, exit_code: i32) -> anyhow::Result<()> {
        self.test_orchestrator_client
            .clone()
            .end_of_test_results(EndOfTestResultsRequest { exit_code })
            .await?;

        Ok(())
    }

    pub async fn prepare_for_local_execution(
        &self,
        ui_prints: DisplayMetadata,
        target: ConfiguredTargetHandle,
        cmd: Vec<ArgValue>,
        env: SortedVectorMap<String, ArgValue>,
        pre_create_dirs: Vec<DeclaredOutput>,
    ) -> anyhow::Result<PrepareForLocalExecutionResult> {
        let executable = TestExecutable {
            ui_prints,
            target,
            cmd,
            env,
            pre_create_dirs,
        };

        let executable: buck2_test_proto::TestExecutable = executable
            .try_into()
            .context("Invalid prepare_for_local_execution request")?;

        let request = buck2_test_proto::PrepareForLocalExecutionRequest {
            test_executable: Some(executable),
        };
        let PrepareForLocalExecutionResponse { result } = self
            .test_orchestrator_client
            .clone()
            .prepare_for_local_execution(request)
            .await?
            .into_inner();

        let result = result
            .context("Missing `result`")?
            .try_into()
            .context("Invalid `result`")?;

        Ok(result)
    }

    pub async fn attach_info_message(&self, message: String) -> anyhow::Result<()> {
        self.test_orchestrator_client
            .clone()
            .attach_info_message(AttachInfoMessageRequest { message })
            .await?;
        Ok(())
    }
}

struct TestOrchestratorService<T: TestOrchestrator> {
    inner: T,
}

#[async_trait::async_trait]
impl<T> test_orchestrator_server::TestOrchestrator for TestOrchestratorService<T>
where
    T: TestOrchestrator + Send + Sync + 'static,
{
    async fn execute2(
        &self,
        request: tonic::Request<buck2_test_proto::ExecuteRequest2>,
    ) -> Result<tonic::Response<ExecuteResponse2>, tonic::Status> {
        to_tonic(async move {
            let ExecuteRequest2 {
                test_executable,
                timeout,
                host_sharing_requirements,
                executor_override,
                required_local_resources,
            } = request
                .into_inner()
                .try_into()
                .context("Invalid execute2 request")?;

            let TestExecutable {
                ui_prints,
                target,
                cmd,
                env,
                pre_create_dirs,
            } = test_executable;

            let response = self
                .inner
                .execute2(
                    ui_prints,
                    target,
                    cmd,
                    env,
                    timeout,
                    host_sharing_requirements,
                    pre_create_dirs,
                    executor_override,
                    required_local_resources,
                )
                .await
                .context("Execution failed")?;

            let response = match response {
                ExecuteResponse::Result(r) => {
                    buck2_test_proto::execute_response2::Response::Result(
                        r.try_into().context("Failed to serialize result")?,
                    )
                }
                ExecuteResponse::Cancelled => {
                    buck2_test_proto::execute_response2::Response::Cancelled(
                        buck2_test_proto::Cancelled {},
                    )
                }
            };

            Ok(ExecuteResponse2 {
                response: Some(response),
            })
        })
        .await
    }

    async fn end_of_test_results(
        &self,
        req: tonic::Request<EndOfTestResultsRequest>,
    ) -> Result<tonic::Response<Empty>, tonic::Status> {
        to_tonic(async move {
            let EndOfTestResultsRequest { exit_code } = req.into_inner();

            self.inner
                .end_of_test_results(exit_code)
                .await
                .context("Failed to report end-of-tests")?;

            Ok(Empty {})
        })
        .await
    }

    async fn report_test_result(
        &self,
        request: tonic::Request<ReportTestResultRequest>,
    ) -> Result<tonic::Response<Empty>, tonic::Status> {
        to_tonic(async move {
            let ReportTestResultRequest { result } = request.into_inner();

            let result = result
                .context("Missing `result`")?
                .try_into()
                .context("Invalid `result`")?;

            self.inner
                .report_test_result(result)
                .await
                .context("Failed to report end-of-tests")?;

            Ok(Empty {})
        })
        .await
    }

    async fn report_tests_discovered(
        &self,
        request: tonic::Request<ReportTestsDiscoveredRequest>,
    ) -> Result<tonic::Response<Empty>, tonic::Status> {
        to_tonic(async move {
            let ReportTestsDiscoveredRequest { target, testing } = request.into_inner();

            let target = target
                .context("Missing `target`")?
                .try_into()
                .context("Invalid `target`")?;

            let Testing { suite, testcases } = testing.context("Missing `testing`")?;

            self.inner
                .report_tests_discovered(target, suite, testcases)
                .await
                .context("Failed to report end-of-tests")?;

            Ok(Empty {})
        })
        .await
    }

    async fn report_test_session(
        &self,
        request: tonic::Request<ReportTestSessionRequest>,
    ) -> Result<tonic::Response<Empty>, tonic::Status> {
        to_tonic(async move {
            let ReportTestSessionRequest { session_info } = request.into_inner();

            self.inner
                .report_test_session(session_info)
                .await
                .context("Failed to report test session summary")?;

            Ok(Empty {})
        })
        .await
    }

    async fn prepare_for_local_execution(
        &self,
        request: tonic::Request<buck2_test_proto::PrepareForLocalExecutionRequest>,
    ) -> Result<tonic::Response<PrepareForLocalExecutionResponse>, tonic::Status> {
        to_tonic(async move {
            let buck2_test_proto::PrepareForLocalExecutionRequest { test_executable } =
                request.into_inner();

            let TestExecutable {
                ui_prints,
                target,
                cmd,
                env,
                pre_create_dirs,
            } = test_executable
                .context("Missing `test_executable`")?
                .try_into()
                .context("Invalid `test_executable`")
                .context("Invalid prepare_for_local_execution request")?;

            let result = self
                .inner
                .prepare_for_local_execution(ui_prints, target, cmd, env, pre_create_dirs)
                .await
                .context("Prepare for local execution failed")?;

            let result = result.try_into().context("Failed to serialize result")?;

            Ok(PrepareForLocalExecutionResponse {
                result: Some(result),
            })
        })
        .await
    }

    async fn attach_info_message(
        &self,
        request: tonic::Request<AttachInfoMessageRequest>,
    ) -> Result<tonic::Response<Empty>, tonic::Status> {
        to_tonic(async move {
            let AttachInfoMessageRequest { message } = request.into_inner();

            self.inner
                .attach_info_message(message)
                .await
                .context("Failed to attach info messages")?;

            Ok(Empty {})
        })
        .await
    }
}

struct DownwardApiService<T: DownwardApi> {
    inner: T,
}

#[async_trait::async_trait]
impl<T> downward_api_server::DownwardApi for DownwardApiService<T>
where
    T: DownwardApi + Send + Sync + 'static,
{
    async fn console(
        &self,
        request: tonic::Request<ConsoleRequest>,
    ) -> Result<tonic::Response<buck2_downward_api_proto::Empty>, tonic::Status> {
        to_tonic(async move {
            let ConsoleRequest { level, message } = request.into_inner();

            let level = level
                .context("Missing `level`")?
                .try_into()
                .context("Invalid `level`")?;

            self.inner
                .console(level, message)
                .await
                .context("Failed to console")?;

            Ok(buck2_downward_api_proto::Empty {})
        })
        .await
    }

    async fn log(
        &self,
        request: tonic::Request<LogRequest>,
    ) -> Result<tonic::Response<buck2_downward_api_proto::Empty>, tonic::Status> {
        to_tonic(async move {
            let LogRequest { level, message } = request.into_inner();

            let level = level
                .context("Missing `level`")?
                .try_into()
                .context("Invalid `level`")?;

            self.inner
                .log(level, message)
                .await
                .context("Failed to log")?;

            Ok(buck2_downward_api_proto::Empty {})
        })
        .await
    }

    async fn external_event(
        &self,
        request: tonic::Request<ExternalEventRequest>,
    ) -> Result<tonic::Response<buck2_downward_api_proto::Empty>, tonic::Status> {
        to_tonic(async move {
            let ExternalEventRequest { event } = request.into_inner();

            let event = event
                .context("Missing `event`")?
                .try_into()
                .context("Invalid `event`")?;

            self.inner
                .external(event)
                .await
                .context("Failed to deliver event")?;

            Ok(buck2_downward_api_proto::Empty {})
        })
        .await
    }
}

pub fn spawn_orchestrator_server<I, O, D>(
    io: I,
    orchestrator: O,
    downward_api: D,
    dispatcher: EventDispatcher,
) -> ServerHandle
where
    I: AsyncRead + AsyncWrite + Send + Unpin + 'static + tonic::transport::server::Connected,
    O: TestOrchestrator + Send + Sync + 'static,
    D: DownwardApi + Send + Sync + 'static,
{
    let router = tonic::transport::Server::builder()
        .layer(EventDispatcherLayer::new(dispatcher))
        .add_service(
            test_orchestrator_server::TestOrchestratorServer::new(TestOrchestratorService {
                inner: orchestrator,
            })
            .max_encoding_message_size(usize::MAX)
            .max_decoding_message_size(usize::MAX),
        )
        .add_service(
            downward_api_server::DownwardApiServer::new(DownwardApiService {
                inner: downward_api,
            })
            .max_encoding_message_size(usize::MAX)
            .max_decoding_message_size(usize::MAX),
        );

    spawn_oneshot(io, router)
}

/// Used to wrap the Tonic server so that the spawned server has its EventDispatcher set.
#[derive(Clone, Dupe)]
pub struct EventDispatcherLayer {
    dispatcher: EventDispatcher,
}

impl EventDispatcherLayer {
    pub fn new(dispatcher: EventDispatcher) -> Self {
        EventDispatcherLayer { dispatcher }
    }
}

impl<S> Layer<S> for EventDispatcherLayer {
    type Service = DispatcherService<S>;

    fn layer(&self, service: S) -> Self::Service {
        DispatcherService {
            service,
            dispatcher: self.dispatcher.dupe(),
        }
    }
}

#[derive(Clone)]
pub struct DispatcherService<S> {
    service: S,
    dispatcher: EventDispatcher,
}

impl<T, Request> tower_service::Service<Request> for DispatcherService<T>
where
    T: tower_service::Service<Request>,
    T::Future: 'static + Send,
    T::Error: 'static,
    T::Response: 'static,
{
    type Response = T::Response;
    type Error = T::Error;
    type Future = BoxFuture<'static, Result<Self::Response, Self::Error>>;

    fn poll_ready(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.service.poll_ready(cx)
    }

    fn call(&mut self, req: Request) -> Self::Future {
        // Call the inner service and get a future that resolves to the response
        let fut = self.service.call(req);

        with_dispatcher_async(self.dispatcher.dupe(), fut).boxed()
    }
}
