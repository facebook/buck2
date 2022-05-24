/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    convert::TryFrom,
    ops::{ControlFlow, FromResidual, Try},
    pin::Pin,
    time::{Duration, Instant},
};

use anyhow::Context;
use buck2_core::exit_result::ExitResult;
use buck2_data::BuckEvent;
use cli_proto::{daemon_api_client::*, *};
pub use connect::BuckdConnectOptions;
use futures::{future::BoxFuture, pin_mut, stream, Stream, StreamExt};
use serde::{Deserialize, Serialize};
use thiserror::Error;
use tonic::{transport::Channel, Request, Status};

use crate::{
    daemon::{
        client::events_ctx::{EventsCtx, FileTailers},
        common::ToProtoDuration,
    },
    version::BuckVersion,
};

pub(crate) mod connect;
mod events_ctx;
mod file_tailer;
mod replayer;

pub use replayer::Replayer;

static GRACEFUL_SHUTDOWN_TIMEOUT: Duration = Duration::from_secs(4);
static FORCE_SHUTDOWN_TIMEOUT: Duration = Duration::from_secs(2);

#[derive(Debug, Error)]
enum BuckdCommunicationError {
    #[error("call to daemon returned an unexpected result type. got `{0:?}`")]
    UnexpectedResultType(command_result::Result),
    #[error("buck daemon returned an empty CommandResult")]
    EmptyCommandResult,
    #[error("buck daemon request finished without returning a CommandResult")]
    MissingCommandResult,
    #[error("buckd communication encountered an unexpected error `{0:?}`")]
    TonicError(tonic::Status),
}

impl From<tonic::Status> for BuckdCommunicationError {
    fn from(status: tonic::Status) -> Self {
        match status.code() {
            tonic::Code::Ok => {
                unreachable!("::Ok should be unreachable as it should produce an Ok result")
            }
            // all errors should be encoded into the CommandResult, we must've hit something strange to be here.
            _ => BuckdCommunicationError::TonicError(status),
        }
    }
}

pub enum VersionCheckResult {
    Match,
    Mismatch { expected: String, actual: String },
}

impl VersionCheckResult {
    fn from(expected: String, actual: String) -> Self {
        if expected == actual {
            Self::Match
        } else {
            Self::Mismatch { expected, actual }
        }
    }

    fn is_match(&self) -> bool {
        matches!(self, Self::Match)
    }
}

enum ClientKind {
    Daemon(DaemonApiClient<Channel>),
    Replayer(Pin<Box<Replayer>>),
}

impl ClientKind {
    fn daemon_only_mut(&mut self) -> &mut DaemonApiClient<Channel> {
        match self {
            ClientKind::Daemon(daemon) => daemon,
            ClientKind::Replayer(_) => panic!("Daemon only command called in replay mode!"),
        }
    }
}

/// This provides a thin wrapper around the proto-generated DaemonApiClient and hides
/// some of the complexity/verbosity of making calls with that. For example, the user
/// doesn't need to deal with tonic::Response/Request and this may provide functions
/// that take more primitive types than the protobuf structure itself.
pub struct BuckdClient {
    client: ClientKind,
    info: DaemonProcessInfo,
    pub(crate) events_ctx: EventsCtx,
}

/// The final outcome returned to the client of running a command in the daemon.
///
/// Either "successful", in which case `R`, the response type, is available, or "failure",
/// where a general `CommandError` was returned. Consider this a "failed successfully" indicator.
/// At the point where this is returned, all event processing / logging should be handled.
#[must_use]
pub enum CommandOutcome<R> {
    /// The buckd client successfully returned the expected response.
    ///
    /// Additional processing of this response may be necessary to determine overall success or
    /// failure within the client.
    Success(R),
    /// The buckd client successfully returned a response, but that response was a general failure.
    ///
    /// The user has already been presented an error message, and the CLI should exit with
    /// this status code.
    Failure(Option<u8>),
}

/// Small wrapper used in FromResidual
pub struct CommandFailure(Option<u8>);

/// Allow the usage of '?' when going from a CommandOutcome -> ExitResult
impl<R> Try for CommandOutcome<R> {
    type Output = R;
    type Residual = CommandFailure;

    fn from_output(output: Self::Output) -> Self {
        Self::Success(output)
    }

    fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
        match self {
            CommandOutcome::Success(res) => ControlFlow::Continue(res),
            CommandOutcome::Failure(status) => ControlFlow::Break(CommandFailure(status)),
        }
    }
}

impl FromResidual<CommandFailure> for ExitResult {
    fn from_residual(residual: CommandFailure) -> Self {
        ExitResult::status(residual.0.unwrap_or(1))
    }
}

impl<R> FromResidual<CommandFailure> for CommandOutcome<R> {
    fn from_residual(residual: CommandFailure) -> Self {
        Self::Failure(residual.0)
    }
}

#[derive(Serialize, Deserialize)]
#[allow(clippy::large_enum_variant)]
pub enum StreamValue {
    Result(CommandResult),
    Event(BuckEvent),
}

/// Translates a tonic streaming response into a stream of StreamValues, the set of things that can flow across the gRPC
/// event stream.
fn grpc_to_stream(
    response: anyhow::Result<tonic::Response<tonic::Streaming<CommandProgress>>>,
) -> impl Stream<Item = anyhow::Result<StreamValue>> {
    let stream = match response {
        Ok(response) => response.into_inner(),
        Err(e) => return futures::stream::once(futures::future::ready(Err(e))).left_stream(),
    };

    stream::unfold(stream, |mut stream| async {
        let msg = match stream.message().await {
            Ok(Some(msg)) => msg,
            Ok(None) => return None,
            Err(e) => return Some((Err(e.into()), stream)),
        };
        let value = match msg.progress {
            Some(command_progress::Progress::Event(e)) => Some(Ok(StreamValue::Event(e))),
            Some(command_progress::Progress::Result(res)) => Some(Ok(StreamValue::Result(res))),
            None => Some(Err(BuckdCommunicationError::EmptyCommandResult.into())),
        };

        value.map(|v| (v, stream))
    })
    .right_stream()
}

impl BuckdClient {
    /// Some commands stream events back from the server.
    /// For these commands, we want to be able to manipulate CLI state.
    ///
    /// This command also does the heavy lifting to substitute the `Replayer` for an actual connection.
    async fn stream<T, R: TryFrom<command_result::Result, Error = command_result::Result>>(
        &mut self,
        command: impl for<'a> FnOnce(
            &'a mut DaemonApiClient<Channel>,
            Request<T>,
        ) -> BoxFuture<
            'a,
            Result<tonic::Response<tonic::Streaming<CommandProgress>>, Status>,
        >,
        request: T,
    ) -> anyhow::Result<CommandOutcome<R>> {
        let Self {
            client, events_ctx, ..
        } = self;
        let tailers = FileTailers::new(&events_ctx.daemon_dir)?;
        match client {
            ClientKind::Daemon(ref mut daemon) => {
                let response = command(daemon, Request::new(request))
                    .await
                    .context("Error dispatching request");
                let stream = grpc_to_stream(response);
                pin_mut!(stream);
                events_ctx.unpack_stream(stream, tailers).await
            }
            ClientKind::Replayer(ref mut replayer) => {
                events_ctx.unpack_stream(replayer, tailers).await
            }
        }
    }

    pub async fn kill(&mut self, reason: &str) -> anyhow::Result<()> {
        let request_fut = self
            .client
            .daemon_only_mut()
            .kill(Request::new(KillRequest {
                reason: reason.to_owned(),
                timeout: Some(GRACEFUL_SHUTDOWN_TIMEOUT.to_proto()),
            }));
        let time_to_kill = GRACEFUL_SHUTDOWN_TIMEOUT + FORCE_SHUTDOWN_TIMEOUT;
        let time_req_sent = Instant::now();
        match tokio::time::timeout(time_to_kill, request_fut).await {
            Ok(inner_result) => {
                inner_result?;
            }
            Err(_) => {
                // ignore the timeout, we'll just send a harder kill.
            }
        }
        self.kill_impl(time_to_kill.saturating_sub(time_req_sent.elapsed()))
            .await
    }

    #[cfg(unix)]
    async fn kill_impl(&self, timeout: Duration) -> anyhow::Result<()> {
        use nix::sys::signal::Signal;

        let daemon_pid = nix::unistd::Pid::from_raw(self.info.pid as i32);
        enum WaitFor {
            Exited,
            WaitTimedOut,
            Err(anyhow::Error),
        }
        async fn wait_for(pid: nix::unistd::Pid, timeout: Duration) -> WaitFor {
            let start = Instant::now();
            while Instant::now() - start < timeout {
                match nix::sys::signal::kill(pid, None) {
                    Ok(_) => {}
                    Err(nix::errno::Errno::ESRCH) => {
                        return WaitFor::Exited;
                    }
                    Err(e) => {
                        return WaitFor::Err(anyhow::anyhow!(
                            "unexpected system error waiting for daemon to terminate (`{}`)",
                            e
                        ));
                    }
                }
                tokio::time::sleep(Duration::from_millis(100)).await;
            }
            WaitFor::WaitTimedOut
        }

        match wait_for(daemon_pid, timeout).await {
            WaitFor::Exited => Ok(()),
            WaitFor::Err(e) => Err(e),
            WaitFor::WaitTimedOut => {
                match nix::sys::signal::kill(daemon_pid, Signal::SIGKILL) {
                    Ok(()) => {
                        crate::eprintln!("Graceful shutdown timed out. Sending SIGKILL.")?;
                    }
                    Err(nix::errno::Errno::ESRCH) => return Ok(()),
                    Err(e) => return Err(e).context("Failed to kill daemon"),
                };

                loop {
                    match nix::sys::signal::kill(daemon_pid, None) {
                        Ok(_) => {}
                        Err(nix::errno::Errno::ESRCH) => {
                            return Ok(());
                        }
                        Err(e) => {
                            return Err(anyhow::anyhow!(
                                "unexpected system error waiting for daemon to terminate (`{}`)",
                                e
                            ));
                        }
                    }
                    tokio::time::sleep(Duration::from_millis(100)).await;
                }
            }
        }
    }

    #[cfg(windows)]
    async fn kill_impl(&self, timeout: Duration) -> anyhow::Result<()> {
        use winapi::{
            shared::winerror::WAIT_TIMEOUT,
            um::{
                handleapi::CloseHandle,
                processthreadsapi::{OpenProcess, TerminateProcess},
                synchapi::WaitForSingleObject,
                winbase::WAIT_OBJECT_0,
                winnt::{HANDLE, PROCESS_TERMINATE, SYNCHRONIZE},
            },
        };

        struct HandleWrapper {
            handle: HANDLE,
        }
        impl Drop for HandleWrapper {
            fn drop(&mut self) {
                unsafe { CloseHandle(self.handle) };
            }
        }

        let daemon_pid = self.info.pid as u32;
        let proc_handle = unsafe { OpenProcess(SYNCHRONIZE | PROCESS_TERMINATE, 0, daemon_pid) };
        // If proc_handle is null, proccess died already.
        if proc_handle.is_null() {
            return Ok(());
        }
        let proc_handle = HandleWrapper {
            handle: proc_handle,
        };
        match unsafe { WaitForSingleObject(proc_handle.handle, timeout.as_millis().try_into()?) } {
            WAIT_OBJECT_0 => Ok(()), // process exited successfully
            WAIT_TIMEOUT => {
                // If proccess isn't signalled, terminate it forcefully.
                match unsafe { TerminateProcess(proc_handle.handle, 1) } {
                    0 => Err(anyhow::anyhow!("Failed to kill daemon ({})", daemon_pid)),
                    _ => Ok(()),
                }
            }
            error_code => Err(anyhow::anyhow!(
                "Waiting for daemon proccess failed. Error code: {:#x}",
                error_code
            )),
        }
    }

    pub async fn status(&mut self, snapshot: bool) -> anyhow::Result<StatusResponse> {
        let outcome = self
            .events_ctx
            .unpack_oneshot(|| {
                self.client
                    .daemon_only_mut()
                    .status(Request::new(StatusRequest { snapshot }))
            })
            .await;
        // TODO(nmj): We have a number of things that wish to use status() and return an anyhow::Result,
        // for now we'll just turn a "CommandMessage" into a error, but that's really not what we
        // want long term.
        match outcome? {
            CommandOutcome::Success(r) => Ok(r),
            CommandOutcome::Failure(_) => {
                Err(anyhow::anyhow!("Unexpected failure message in status()"))
            }
        }
    }

    pub async fn clean(
        &mut self,
        req: CleanRequest,
    ) -> anyhow::Result<CommandOutcome<CleanResponse>> {
        self.stream(|d, r| Box::pin(DaemonApiClient::clean(d, r)), req)
            .await
    }

    pub async fn aquery(
        &mut self,
        req: AqueryRequest,
    ) -> anyhow::Result<CommandOutcome<AqueryResponse>> {
        self.stream(|d, r| Box::pin(DaemonApiClient::aquery(d, r)), req)
            .await
    }

    pub async fn cquery(
        &mut self,
        req: CqueryRequest,
    ) -> anyhow::Result<CommandOutcome<CqueryResponse>> {
        self.stream(|d, r| Box::pin(DaemonApiClient::cquery(d, r)), req)
            .await
    }

    pub async fn uquery(
        &mut self,
        req: UqueryRequest,
    ) -> anyhow::Result<CommandOutcome<UqueryResponse>> {
        self.stream(|d, r| Box::pin(DaemonApiClient::uquery(d, r)), req)
            .await
    }

    pub async fn targets(
        &mut self,
        req: TargetsRequest,
    ) -> anyhow::Result<CommandOutcome<TargetsResponse>> {
        self.stream(|d, r| Box::pin(DaemonApiClient::targets(d, r)), req)
            .await
    }

    pub async fn targets_show_outputs(
        &mut self,
        req: TargetsRequest,
    ) -> anyhow::Result<CommandOutcome<TargetsShowOutputsResponse>> {
        self.stream(
            |d, r| Box::pin(DaemonApiClient::targets_show_outputs(d, r)),
            req,
        )
        .await
    }

    pub async fn build(
        &mut self,
        req: BuildRequest,
    ) -> anyhow::Result<CommandOutcome<BuildResponse>> {
        self.stream(|d, r| Box::pin(DaemonApiClient::build(d, r)), req)
            .await
    }

    pub async fn bxl(&mut self, req: BxlRequest) -> anyhow::Result<CommandOutcome<BxlResponse>> {
        self.stream(|d, r| Box::pin(DaemonApiClient::bxl(d, r)), req)
            .await
    }

    pub async fn test(&mut self, req: TestRequest) -> anyhow::Result<CommandOutcome<TestResponse>> {
        self.stream(|d, r| Box::pin(DaemonApiClient::test(d, r)), req)
            .await
    }

    pub async fn install(
        &mut self,
        req: InstallRequest,
    ) -> anyhow::Result<CommandOutcome<InstallResponse>> {
        self.stream(|d, r| Box::pin(DaemonApiClient::install(d, r)), req)
            .await
    }

    pub async fn audit(
        &mut self,
        req: GenericRequest,
    ) -> anyhow::Result<CommandOutcome<GenericResponse>> {
        self.stream(|d, r| Box::pin(DaemonApiClient::audit(d, r)), req)
            .await
    }

    pub async fn materialize(
        &mut self,
        req: MaterializeRequest,
    ) -> anyhow::Result<CommandOutcome<MaterializeResponse>> {
        self.stream(|d, r| Box::pin(DaemonApiClient::materialize(d, r)), req)
            .await
    }

    pub async fn unstable_docs(
        &mut self,
        req: UnstableDocsRequest,
    ) -> anyhow::Result<CommandOutcome<UnstableDocsResponse>> {
        self.stream(|d, r| Box::pin(DaemonApiClient::unstable_docs(d, r)), req)
            .await
    }

    pub async fn unstable_crash(
        &mut self,
        req: UnstableCrashRequest,
    ) -> anyhow::Result<UnstableCrashResponse> {
        let resp = self
            .client
            .daemon_only_mut()
            .unstable_crash(Request::new(req))
            .await?;
        Ok(resp.into_inner())
    }

    pub async fn segfault(&mut self, req: SegfaultRequest) -> anyhow::Result<SegfaultResponse> {
        let resp = self
            .client
            .daemon_only_mut()
            .segfault(Request::new(req))
            .await?;
        Ok(resp.into_inner())
    }

    pub async fn unstable_heap_dump(
        &mut self,
        req: UnstableHeapDumpRequest,
    ) -> anyhow::Result<UnstableHeapDumpResponse> {
        let resp = self
            .client
            .daemon_only_mut()
            .unstable_heap_dump(Request::new(req))
            .await?;
        Ok(resp.into_inner())
    }

    pub async fn unstable_allocator_stats(
        &mut self,
        req: UnstableAllocatorStatsRequest,
    ) -> anyhow::Result<UnstableAllocatorStatsResponse> {
        let resp = self
            .client
            .daemon_only_mut()
            .unstable_allocator_stats(Request::new(req))
            .await?;
        Ok(resp.into_inner())
    }

    pub async fn unstable_dice_dump(
        &mut self,
        req: UnstableDiceDumpRequest,
    ) -> anyhow::Result<UnstableDiceDumpResponse> {
        let resp = self
            .client
            .daemon_only_mut()
            .unstable_dice_dump(Request::new(req))
            .await?;
        Ok(resp.into_inner())
    }

    pub async fn profile(
        &mut self,
        req: ProfileRequest,
    ) -> anyhow::Result<CommandOutcome<ProfileResponse>> {
        self.stream(|d, r| Box::pin(DaemonApiClient::profile2(d, r)), req)
            .await
    }

    pub async fn flush_dep_files(
        &mut self,
        req: FlushDepFilesRequest,
    ) -> anyhow::Result<CommandOutcome<GenericResponse>> {
        self.events_ctx
            .unpack_oneshot(|| {
                self.client
                    .daemon_only_mut()
                    .flush_dep_files(Request::new(req))
            })
            .await
    }

    pub async fn check_version(&mut self) -> anyhow::Result<VersionCheckResult> {
        let status = self.status(false).await?;
        Ok(VersionCheckResult::from(
            BuckVersion::get_unique_id().to_owned(),
            status.process_info.unwrap().version,
        ))
    }
}

#[cfg(test)]
mod tests {
    use std::io::Write;

    use futures::StreamExt;

    use super::*;
    use crate::daemon::client::file_tailer::FileTailer;

    #[tokio::test]
    async fn test_tailer() -> anyhow::Result<()> {
        let mut file = tempfile::NamedTempFile::new()?;
        writeln!(file, "before")?;

        // If we could control the interval for tailer polling, we could reliably
        // test more of the behavior. For now, just test a simple case.
        let (mut receiver, tailer) = FileTailer::tail_file(file.path().to_owned())?;
        writeln!(file, "after")?;
        // have to sleep long enough for a read or else this test is racy.
        tokio::time::sleep(Duration::from_millis(250)).await;
        std::mem::drop(tailer);
        assert_eq!("after\n", &receiver.next().await.unwrap());

        Ok(())
    }
}
