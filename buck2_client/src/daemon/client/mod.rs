/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::pin::Pin;
use std::time::Duration;
use std::time::Instant;

use anyhow::Context;
use cli_proto::daemon_api_client::*;
use cli_proto::*;
pub use connect::BuckdConnectOptions;
use futures::future::BoxFuture;
use futures::pin_mut;
use futures::stream;
use futures::Stream;
use futures::StreamExt;
use tonic::transport::Channel;
use tonic::Request;
use tonic::Status;

use crate::command_outcome::CommandOutcome;
use crate::events_ctx::EventsCtx;
use crate::events_ctx::FileTailers;
use crate::stream_value::StreamValue;
use crate::version::BuckVersion;

pub mod connect;

use crate::replayer::Replayer;

static GRACEFUL_SHUTDOWN_TIMEOUT: Duration = Duration::from_secs(4);
static FORCE_SHUTDOWN_TIMEOUT: Duration = Duration::from_secs(2);

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

/// We need to make sure that all calls to the daemon in buckd flush the tailers after completion.
/// The connector wraps all buckd calls with flushing.
pub struct BuckdClientConnector {
    client: BuckdClient,
}

impl BuckdClientConnector {
    pub async fn with_flushing<Fun, R: 'static>(&mut self, command: Fun) -> anyhow::Result<R>
    where
        for<'a> Fun: FnOnce(&'a mut BuckdClient) -> BoxFuture<'a, R>,
    {
        self.client.open_tailers()?;
        let result = command(&mut self.client).await;

        self.client
            .events_ctx
            .flush(&mut self.client.tailers)
            .await?;
        Ok(result)
    }
}

/// This provides a thin wrapper around the proto-generated DaemonApiClient and hides
/// some of the complexity/verbosity of making calls with that. For example, the user
/// doesn't need to deal with tonic::Response/Request and this may provide functions
/// that take more primitive types than the protobuf structure itself.
pub struct BuckdClient {
    client: ClientKind,
    info: DaemonProcessInfo,
    // TODO(brasselsprouts): events_ctx should own tailers
    tailers: Option<FileTailers>,
    pub events_ctx: EventsCtx,
}

#[derive(Debug, thiserror::Error)]
enum GrpcToStreamError {
    #[error("buck daemon returned an empty CommandProgress")]
    EmptyCommandProgress,
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
            None => Some(Err(GrpcToStreamError::EmptyCommandProgress.into())),
        };

        value.map(|v| (v, stream))
    })
    .right_stream()
}

impl BuckdClient {
    fn open_tailers(&mut self) -> anyhow::Result<()> {
        match &self.client {
            ClientKind::Daemon(..) => {
                let tailers = FileTailers::new(&self.events_ctx.daemon_dir)?;
                self.tailers = Some(tailers);
            }
            ClientKind::Replayer(..) => {
                // Don't open logs if replaying
            }
        }

        Ok(())
    }

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
        match client {
            ClientKind::Daemon(ref mut daemon) => {
                let response = command(daemon, Request::new(request))
                    .await
                    .context("Error dispatching request");
                let stream = grpc_to_stream(response);
                pin_mut!(stream);
                events_ctx.unpack_stream(stream, &mut self.tailers).await
            }
            ClientKind::Replayer(ref mut replayer) => {
                events_ctx.unpack_stream(replayer, &mut self.tailers).await
            }
        }
    }

    pub async fn kill(&mut self, reason: &str) -> anyhow::Result<()> {
        let pid = self.info.pid;
        let request_fut = self
            .client
            .daemon_only_mut()
            .kill(Request::new(KillRequest {
                reason: reason.to_owned(),
                timeout: Some(GRACEFUL_SHUTDOWN_TIMEOUT.try_into()?),
            }));
        let time_to_kill = GRACEFUL_SHUTDOWN_TIMEOUT + FORCE_SHUTDOWN_TIMEOUT;
        let time_req_sent = Instant::now();
        // First we send a Kill request
        match tokio::time::timeout(time_to_kill, request_fut).await {
            Ok(inner_result) => {
                inner_result?;
            }
            Err(_) => {
                // ignore the timeout, we'll just send a harder kill.
            }
        }
        // Then we do a wait_for on the pid, and if that times out, we kill it harder
        Self::kill_impl(pid, time_to_kill.saturating_sub(time_req_sent.elapsed())).await
    }

    #[cfg(unix)]
    async fn kill_impl(pid: i64, timeout: Duration) -> anyhow::Result<()> {
        use nix::sys::signal::Signal;

        let daemon_pid = nix::unistd::Pid::from_raw(pid as i32);
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
    async fn kill_impl(pid: i64, timeout: Duration) -> anyhow::Result<()> {
        use winapi::shared::winerror::WAIT_TIMEOUT;
        use winapi::um::handleapi::CloseHandle;
        use winapi::um::processthreadsapi::OpenProcess;
        use winapi::um::processthreadsapi::TerminateProcess;
        use winapi::um::synchapi::WaitForSingleObject;
        use winapi::um::winbase::WAIT_OBJECT_0;
        use winapi::um::winnt::HANDLE;
        use winapi::um::winnt::PROCESS_TERMINATE;
        use winapi::um::winnt::SYNCHRONIZE;

        struct HandleWrapper {
            handle: HANDLE,
        }
        impl Drop for HandleWrapper {
            fn drop(&mut self) {
                unsafe { CloseHandle(self.handle) };
            }
        }

        let daemon_pid = pid as u32;
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
            // Safe to unwrap tailers here because they are instantiated prior to a command being called.
            .unpack_oneshot(&mut self.tailers, || {
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
}

macro_rules! stream_method {
    ($method: ident, $req: ty, $res: ty) => {
        stream_method!($method, $method, $req, $res);
    };

    ($method: ident, $grpc_method: ident, $req: ty, $res: ty) => {
        pub async fn $method(&mut self, req: $req) -> anyhow::Result<CommandOutcome<$res>> {
            self.stream(|d, r| Box::pin(DaemonApiClient::$grpc_method(d, r)), req)
                .await
        }
    };
}

impl BuckdClient {
    stream_method!(clean, CleanRequest, CleanResponse);
    stream_method!(aquery, AqueryRequest, AqueryResponse);
    stream_method!(cquery, CqueryRequest, CqueryResponse);
    stream_method!(uquery, UqueryRequest, UqueryResponse);
    stream_method!(targets, TargetsRequest, TargetsResponse);
    stream_method!(
        targets_show_outputs,
        TargetsRequest,
        TargetsShowOutputsResponse
    );
    stream_method!(build, BuildRequest, BuildResponse);
    stream_method!(bxl, BxlRequest, BxlResponse);
    stream_method!(test, TestRequest, TestResponse);
    stream_method!(install, InstallRequest, InstallResponse);
    stream_method!(audit, GenericRequest, GenericResponse);
    stream_method!(materialize, MaterializeRequest, MaterializeResponse);
    stream_method!(unstable_docs, UnstableDocsRequest, UnstableDocsResponse);
    stream_method!(profile, profile2, ProfileRequest, ProfileResponse);

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

    pub async fn flush_dep_files(
        &mut self,
        req: FlushDepFilesRequest,
    ) -> anyhow::Result<CommandOutcome<GenericResponse>> {
        self.events_ctx
            .unpack_oneshot(&mut self.tailers, || {
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

    pub async fn lsp(
        &mut self,
        context: ClientContext,
        requests: impl Stream<Item = LspRequest> + Send + Sync + 'static,
    ) -> anyhow::Result<CommandOutcome<LspResponse>> {
        let req = create_client_stream(context, requests);
        self.stream(|d, r| Box::pin(DaemonApiClient::lsp(d, r)), req)
            .await
    }
}

/// Create a stream that is sent over as a parameter via GRPC to the daemon.
///
/// Ensures that we send a proper ClientContext message, and that the inner type is wrapped
/// properly into a [`StreamingRequest`]
fn create_client_stream<
    T: Into<StreamingRequest>,
    InStream: Stream<Item = T> + Send + Sync + 'static,
>(
    context: ClientContext,
    requests: InStream,
) -> impl Stream<Item = StreamingRequest> + Send + Sync + 'static {
    let init_req = StreamingRequest {
        request: Some(streaming_request::Request::Context(context)),
    };
    stream::once(async move { init_req }).chain(requests.map(|request| request.into()))
}

#[cfg(test)]
mod tests {
    use std::io::Write;

    use futures::StreamExt;

    use super::*;
    use crate::file_tailer::FileTailer;

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
