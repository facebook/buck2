/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fs::create_dir_all;
use std::fs::File;
use std::pin::Pin;
use std::time::Duration;
use std::time::Instant;

use anyhow::Context;
use buck2_common::client_utils::retrying;
use buck2_common::daemon_dir::DaemonDir;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::AbsPathBuf;
use cli_proto::daemon_api_client::*;
use cli_proto::*;
use fs2::FileExt;
use futures::future::BoxFuture;
use futures::pin_mut;
use futures::stream;
use futures::Stream;
use futures::StreamExt;
use tonic::transport::Channel;
use tonic::Request;
use tonic::Status;

use crate::command_outcome::CommandOutcome;
use crate::console_interaction_stream::ConsoleInteractionStream;
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

#[derive(Debug, thiserror::Error)]
enum LifecycleError {
    #[error("Missing `{}` file in `{}` directory", BuckdLifecycleLock::BUCKD_LIFECYCLE, _0.display())]
    MissingLifecycle(AbsPathBuf),
}

/// We need to make sure that all calls to the daemon in buckd flush the tailers after completion.
/// The connector wraps all buckd calls with flushing.
pub struct BuckdClientConnector {
    client: BuckdClient,
}

impl BuckdClientConnector {
    pub fn with_flushing(&mut self) -> FlushingBuckdClient<'_> {
        FlushingBuckdClient {
            inner: &mut self.client,
        }
    }
}

pub struct BuckdLifecycleLock {
    daemon_dir: DaemonDir,
    lock_file: File,
}

impl BuckdLifecycleLock {
    const BUCKD_LIFECYCLE: &'static str = "buckd.lifecycle";

    pub async fn lock_with_timeout(
        daemon_dir: DaemonDir,
        timeout: Duration,
    ) -> anyhow::Result<BuckdLifecycleLock> {
        create_dir_all(&daemon_dir.path)?;
        let lifecycle_path = daemon_dir.path.as_path().join(Self::BUCKD_LIFECYCLE);
        let file = File::create(lifecycle_path)?;
        retrying(
            Duration::from_millis(5),
            Duration::from_millis(100),
            timeout,
            async || Ok(file.try_lock_exclusive()?),
        )
        .await?;

        Ok(BuckdLifecycleLock {
            lock_file: file,
            daemon_dir,
        })
    }

    /// Remove everything except `buckd.lifecycle` file which is the lock file.
    fn clean_daemon_dir(&self) -> anyhow::Result<()> {
        let mut seen_lifecycle = false;
        for p in fs_util::read_dir(&self.daemon_dir.path)? {
            let p = p?;
            if p.file_name() == Self::BUCKD_LIFECYCLE {
                seen_lifecycle = true;
                continue;
            }
            fs_util::remove_all(p.path())?;
        }
        if !seen_lifecycle {
            // Self-check.
            return Err(LifecycleError::MissingLifecycle(self.daemon_dir.path.clone()).into());
        }
        Ok(())
    }
}

impl Drop for BuckdLifecycleLock {
    fn drop(&mut self) {
        self.lock_file
            .unlock()
            .expect("Unexpected failure to unlock buckd.lifecycle file.")
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
    pub(crate) events_ctx: EventsCtx,
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
    async fn stream<'i, T, R: TryFrom<command_result::Result, Error = command_result::Result>>(
        &mut self,
        command: impl for<'a> FnOnce(
            &'a mut DaemonApiClient<Channel>,
            Request<T>,
        ) -> BoxFuture<
            'a,
            Result<tonic::Response<tonic::Streaming<CommandProgress>>, Status>,
        >,
        request: T,
        console_interaction: Option<ConsoleInteractionStream<'i>>,
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
                events_ctx
                    .unpack_stream(stream, &mut self.tailers, console_interaction)
                    .await
            }
            ClientKind::Replayer(ref mut replayer) => {
                events_ctx
                    .unpack_stream(replayer, &mut self.tailers, console_interaction)
                    .await
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

    pub async fn check_version(&mut self) -> anyhow::Result<VersionCheckResult> {
        let status = self.status(false).await?;
        Ok(VersionCheckResult::from(
            BuckVersion::get_unique_id().to_owned(),
            status.process_info.unwrap().version,
        ))
    }
}

pub struct FlushingBuckdClient<'a> {
    inner: &'a mut BuckdClient,
}

impl<'a> FlushingBuckdClient<'a> {
    fn enter(&mut self) -> anyhow::Result<()> {
        self.inner.open_tailers()?;
        Ok(())
    }

    async fn exit(&mut self) -> anyhow::Result<()> {
        self.inner.events_ctx.flush(&mut self.inner.tailers).await?;

        Ok(())
    }
}

/// Implement a streaming method with full event reporting.
macro_rules! stream_method {
    ($method: ident, $req: ty, $res: ty) => {
        stream_method!($method, $method, $req, $res);
    };

    ($method: ident, $grpc_method: ident, $req: ty, $res: ty) => {
        pub async fn $method(
            &mut self,
            req: $req,
            console_interaction: Option<ConsoleInteractionStream<'_>>,
        ) -> anyhow::Result<CommandOutcome<$res>> {
            self.enter()?;
            let res = self
                .inner
                .stream(
                    |d, r| Box::pin(DaemonApiClient::$grpc_method(d, r)),
                    req,
                    console_interaction,
                )
                .await;
            self.exit().await?;
            res
        }
    };
}

/// Implement a bi-directional streaming method with full event reporting.
macro_rules! bidirectional_stream_method {
    ($method: ident, $req: ty, $res: ty) => {
        bidirectional_stream_method!($method, $method, $req, $res);
    };

    ($method: ident, $grpc_method: ident, $req: ty, $res: ty) => {
        pub async fn $method(
            &mut self,
            context: ClientContext,
            requests: impl Stream<Item = $req> + Send + Sync + 'static,
        ) -> anyhow::Result<CommandOutcome<$res>> {
            self.enter()?;
            let req = create_client_stream(context, requests);
            let res = self
                .inner
                .stream(|d, r| Box::pin(DaemonApiClient::$method(d, r)), req, None)
                .await;
            self.exit().await?;
            res
        }
    };
}

/// Implement a oneshot method with full event reporting.
macro_rules! oneshot_method {
    ($method: ident, $req: ty, $res: ty) => {
        oneshot_method!($method, $method, $req, $res);
    };

    ($method: ident, $grpc_method: ident, $req: ty, $res: ty) => {
        pub async fn $method(&mut self, req: $req) -> anyhow::Result<CommandOutcome<$res>> {
            self.enter()?;
            let res = self
                .inner
                .events_ctx
                .unpack_oneshot(&mut self.inner.tailers, || {
                    self.inner
                        .client
                        .daemon_only_mut()
                        .$method(Request::new(req))
                })
                .await;
            self.exit().await?;
            res
        }
    };
}

/// Implement a method that does not produce a CommandResult and does not produce any events.
macro_rules! debug_method {
    ($method: ident, $req: ty, $res: ty) => {
        debug_method!($method, $method, $req, $res);
    };

    ($method: ident, $grpc_method: ident, $req: ty, $res: ty) => {
        pub async fn $method(&mut self, req: $req) -> anyhow::Result<$res> {
            self.enter()?;
            let out = self
                .inner
                .client
                .daemon_only_mut()
                .$method(Request::new(req))
                .await;
            self.exit().await?;
            Ok(out?.into_inner())
        }
    };
}

/// Wrap a method that exists on the BuckdClient, with flushing.
macro_rules! wrap_method {
     ($method: ident ($($param: ident : $param_type: ty),*), $res: ty) => {
         pub async fn $method(&mut self, $($param: $param_type)*) -> anyhow::Result<$res> {
             self.enter()?;
             let out = self
                 .inner
                 .$method($($param)*)
                 .await;
             self.exit().await?;
             out
         }
     };
 }

impl<'a> FlushingBuckdClient<'a> {
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

    bidirectional_stream_method!(lsp, LspRequest, LspResponse);

    oneshot_method!(flush_dep_files, FlushDepFilesRequest, GenericResponse);

    debug_method!(unstable_crash, UnstableCrashRequest, UnstableCrashResponse);
    debug_method!(segfault, SegfaultRequest, SegfaultResponse);
    debug_method!(
        unstable_heap_dump,
        UnstableHeapDumpRequest,
        UnstableHeapDumpResponse
    );
    debug_method!(
        unstable_allocator_stats,
        UnstableAllocatorStatsRequest,
        UnstableAllocatorStatsResponse
    );
    debug_method!(
        unstable_dice_dump,
        UnstableDiceDumpRequest,
        UnstableDiceDumpResponse
    );

    wrap_method!(kill(reason: &str), ());
    wrap_method!(status(snapshot: bool), StatusResponse);
    wrap_method!(check_version(), VersionCheckResult);
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

    use buck2_core::fs::paths::AbsPathBuf;
    use futures::StreamExt;

    use super::*;
    use crate::file_tailer::FileTailer;

    #[tokio::test]
    async fn test_tailer() -> anyhow::Result<()> {
        let mut file = tempfile::NamedTempFile::new()?;
        writeln!(file, "before")?;

        // If we could control the interval for tailer polling, we could reliably
        // test more of the behavior. For now, just test a simple case.
        let (mut receiver, tailer) =
            FileTailer::tail_file(AbsPathBuf::new(file.path().to_owned())?)?;
        writeln!(file, "after")?;
        // have to sleep long enough for a read or else this test is racy.
        tokio::time::sleep(Duration::from_millis(250)).await;
        std::mem::drop(tailer);
        assert_eq!("after\n", &receiver.next().await.unwrap());

        Ok(())
    }
}
