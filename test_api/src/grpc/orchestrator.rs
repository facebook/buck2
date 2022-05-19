use std::{collections::HashMap, convert::TryInto, time::Duration};

use anyhow::Context as _;
use downward_api::DownwardApi;
use downward_api_proto::{
    downward_api_client, downward_api_server, ConsoleRequest, ExternalEventRequest, LogRequest,
};
use host_sharing::HostSharingRequirements;
use test_proto::{
    test_orchestrator_client, test_orchestrator_server, Empty, EndOfTestResultsRequest,
    ExecuteResponse2, ReportTestResultRequest, ReportTestSessionRequest,
    ReportTestsDiscoveredRequest, Testing,
};
use tokio::io::{AsyncRead, AsyncWrite};
use tonic::transport::Channel;
use tracing::Level;

use crate::{
    data::{
        ArgValue, ConfiguredTargetHandle, DeclaredOutput, DisplayMetadata, ExecuteRequest2,
        ExecutionResult2, TestResult,
    },
    grpc::{
        channel,
        server::{spawn_oneshot, ServerHandle},
        util::to_tonic,
    },
    protocol::TestOrchestrator,
};

pub struct TestOrchestratorClient {
    test_orchestrator_client: test_orchestrator_client::TestOrchestratorClient<Channel>,
    downward_api_client: downward_api_client::DownwardApiClient<Channel>,
}

impl TestOrchestratorClient {
    pub async fn new<T>(io: T) -> anyhow::Result<Self>
    where
        T: AsyncRead + AsyncWrite + Send + Sync + Unpin + 'static,
    {
        let channel = channel::make_channel(io, "orchestrator").await?;

        Ok(Self {
            test_orchestrator_client: test_orchestrator_client::TestOrchestratorClient::new(
                channel.clone(),
            ),
            downward_api_client: downward_api_client::DownwardApiClient::new(channel),
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

#[async_trait::async_trait]
impl TestOrchestrator for TestOrchestratorClient {
    async fn execute2(
        &self,
        ui_prints: DisplayMetadata,
        target: ConfiguredTargetHandle,
        cmd: Vec<ArgValue>,
        env: HashMap<String, ArgValue>,
        timeout: Duration,
        host_sharing_requirements: HostSharingRequirements,
        pre_create_dirs: Vec<DeclaredOutput>,
    ) -> anyhow::Result<ExecutionResult2> {
        let req = ExecuteRequest2 {
            ui_prints,
            target,
            cmd,
            env,
            timeout,
            host_sharing_requirements,
            pre_create_dirs,
        };

        let req: test_proto::ExecuteRequest2 = req.try_into().context("Invalid execute request")?;

        let ExecuteResponse2 { result } = self
            .test_orchestrator_client
            .clone()
            .execute2(req)
            .await?
            .into_inner();

        let result = result
            .context("Missing `result`")?
            .try_into()
            .context("Invalid `result`")?;

        Ok(result)
    }

    async fn report_test_result(&self, result: TestResult) -> anyhow::Result<()> {
        let result = result.try_into().context("Invalid `result`")?;

        self.test_orchestrator_client
            .clone()
            .report_test_result(ReportTestResultRequest {
                result: Some(result),
            })
            .await?;

        Ok(())
    }

    async fn report_tests_discovered(
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

    async fn report_test_session(
        &self,
        session_id: String,
        session_info: String,
    ) -> anyhow::Result<()> {
        self.test_orchestrator_client
            .clone()
            .report_test_session(ReportTestSessionRequest {
                session_id,
                session_info,
            })
            .await?;

        Ok(())
    }

    async fn end_of_test_results(&self, exit_code: i32) -> anyhow::Result<()> {
        self.test_orchestrator_client
            .clone()
            .end_of_test_results(EndOfTestResultsRequest { exit_code })
            .await?;

        Ok(())
    }
}

pub struct Service<T> {
    inner: T,
}

#[async_trait::async_trait]
impl<T> test_orchestrator_server::TestOrchestrator for Service<T>
where
    T: TestOrchestrator + Send + Sync + 'static,
{
    async fn execute2(
        &self,
        request: tonic::Request<test_proto::ExecuteRequest2>,
    ) -> Result<tonic::Response<ExecuteResponse2>, tonic::Status> {
        to_tonic(async move {
            let ExecuteRequest2 {
                ui_prints,
                target,
                cmd,
                env,
                timeout,
                host_sharing_requirements,
                pre_create_dirs,
            } = request
                .into_inner()
                .try_into()
                .context("Invalid execute2 request")?;

            let result = self
                .inner
                .execute2(
                    ui_prints,
                    target,
                    cmd,
                    env,
                    timeout,
                    host_sharing_requirements,
                    pre_create_dirs,
                )
                .await
                .context("Execution failed")?;

            let result = result.try_into().context("Failed to serialize result")?;

            Ok(ExecuteResponse2 {
                result: Some(result),
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
            let ReportTestSessionRequest {
                session_id,
                session_info,
            } = request.into_inner();

            self.inner
                .report_test_session(session_id, session_info)
                .await
                .context("Failed to report test session summary")?;

            Ok(Empty {})
        })
        .await
    }
}

#[async_trait::async_trait]
impl<T> downward_api_server::DownwardApi for Service<T>
where
    T: DownwardApi + Send + Sync + 'static,
{
    async fn console(
        &self,
        request: tonic::Request<ConsoleRequest>,
    ) -> Result<tonic::Response<downward_api_proto::Empty>, tonic::Status> {
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

            Ok(downward_api_proto::Empty {})
        })
        .await
    }

    async fn log(
        &self,
        request: tonic::Request<LogRequest>,
    ) -> Result<tonic::Response<downward_api_proto::Empty>, tonic::Status> {
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

            Ok(downward_api_proto::Empty {})
        })
        .await
    }

    async fn external_event(
        &self,
        request: tonic::Request<ExternalEventRequest>,
    ) -> Result<tonic::Response<downward_api_proto::Empty>, tonic::Status> {
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

            Ok(downward_api_proto::Empty {})
        })
        .await
    }
}

pub fn spawn_orchestrator_server<I, O, D>(io: I, orchestrator: O, downward_api: D) -> ServerHandle
where
    I: AsyncRead + AsyncWrite + Send + Unpin + 'static + tonic::transport::server::Connected,
    O: TestOrchestrator + Send + Sync + 'static,
    D: DownwardApi + Send + Sync + 'static,
{
    let router = tonic::transport::Server::builder()
        .add_service(test_orchestrator_server::TestOrchestratorServer::new(
            Service {
                inner: orchestrator,
            },
        ))
        .add_service(downward_api_server::DownwardApiServer::new(Service {
            inner: downward_api,
        }));

    spawn_oneshot(io, router)
}
