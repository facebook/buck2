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
use async_trait::async_trait;
use buck2_cli_proto::daemon_api_client::*;
use buck2_cli_proto::*;
use buck2_common::daemon_dir::DaemonDir;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use fs2::FileExt;
use futures::future::BoxFuture;
use futures::pin_mut;
use futures::stream;
use futures::Stream;
use futures::StreamExt;
use futures::TryStreamExt;
use sysinfo::Pid;
use sysinfo::PidExt;
use sysinfo::ProcessExt;
use sysinfo::ProcessRefreshKind;
use sysinfo::System;
use sysinfo::SystemExt;
use tonic::codegen::InterceptedService;
use tonic::transport::Channel;
use tonic::Request;
use tonic::Status;

use crate::command_outcome::CommandOutcome;
use crate::console_interaction_stream::ConsoleInteractionStream;
use crate::daemon::client::connect::BuckAddAuthTokenInterceptor;
use crate::events_ctx::EventsCtx;
use crate::events_ctx::FileTailers;
use crate::events_ctx::PartialResultCtx;
use crate::events_ctx::PartialResultHandler;
use crate::stream_value::StreamValue;
use crate::version::BuckVersion;

pub mod connect;

use crate::replayer::Replayer;
use crate::startup_deadline::StartupDeadline;

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
    Daemon(DaemonApiClient<InterceptedService<Channel, BuckAddAuthTokenInterceptor>>),
    Replayer(Pin<Box<Replayer>>),
}

impl ClientKind {
    fn daemon_only_mut(
        &mut self,
    ) -> &mut DaemonApiClient<InterceptedService<Channel, BuckAddAuthTokenInterceptor>> {
        match self {
            ClientKind::Daemon(daemon) => daemon,
            ClientKind::Replayer(_) => panic!("Daemon only command called in replay mode!"),
        }
    }
}

#[derive(Debug, thiserror::Error)]
enum LifecycleError {
    #[error("Missing `{}` file in `{}` directory", BuckdLifecycleLock::BUCKD_LIFECYCLE, _0.display())]
    MissingLifecycle(AbsNormPathBuf),
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
        deadline: StartupDeadline,
    ) -> anyhow::Result<BuckdLifecycleLock> {
        create_dir_all(&daemon_dir.path)?;
        let lifecycle_path = daemon_dir.path.as_path().join(Self::BUCKD_LIFECYCLE);
        let file = File::create(lifecycle_path)?;
        deadline
            .retrying(
                "locking buckd lifecycle",
                Duration::from_millis(5),
                Duration::from_millis(100),
                async || Ok(file.try_lock_exclusive()?),
            )
            .await?;

        Ok(BuckdLifecycleLock {
            lock_file: file,
            daemon_dir,
        })
    }

    /// Remove everything except `buckd.lifecycle` file which is the lock file.
    pub fn clean_daemon_dir(&self) -> anyhow::Result<()> {
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
    response: anyhow::Result<tonic::Response<tonic::Streaming<MultiCommandProgress>>>,
) -> impl Stream<Item = anyhow::Result<StreamValue>> {
    let stream = match response {
        Ok(response) => response.into_inner(),
        Err(e) => return futures::stream::once(futures::future::ready(Err(e))).left_stream(),
    };

    let stream = stream
        .map_ok(|e| stream::iter(e.messages.into_iter().map(anyhow::Ok)))
        .try_flatten();

    stream::unfold(stream, |mut stream| async {
        let msg = match stream.try_next().await {
            Ok(Some(msg)) => msg,
            Ok(None) => return None,
            Err(e) => return Some((Err(e), stream)),
        };
        let value = match msg.progress {
            Some(command_progress::Progress::Event(e)) => Some(Ok(StreamValue::Event(e))),
            Some(command_progress::Progress::Result(res)) => Some(Ok(StreamValue::Result(res))),
            Some(command_progress::Progress::PartialResult(res)) => {
                Some(Ok(StreamValue::PartialResult(res)))
            }
            None => Some(Err(GrpcToStreamError::EmptyCommandProgress.into())),
        };

        value.map(|v| (v, stream))
    })
    .right_stream()
}

enum KillBehavior {
    WaitForExit,
    TerminateFirst,
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
    async fn stream<'i, T, Res, Handler, Command>(
        &mut self,
        command: Command,
        request: T,
        partial_result_handler: &mut Handler,
        console_interaction: Option<ConsoleInteractionStream<'i>>,
    ) -> anyhow::Result<CommandOutcome<Res>>
    where
        Command: for<'a> FnOnce(
            &'a mut DaemonApiClient<InterceptedService<Channel, BuckAddAuthTokenInterceptor>>,
            Request<T>,
        ) -> BoxFuture<
            'a,
            Result<tonic::Response<tonic::Streaming<MultiCommandProgress>>, Status>,
        >,
        Res: TryFrom<command_result::Result, Error = command_result::Result>,
        Handler: PartialResultHandler,
    {
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
                    .unpack_stream(
                        partial_result_handler,
                        stream,
                        self.tailers.take(),
                        console_interaction,
                    )
                    .await
            }
            ClientKind::Replayer(ref mut replayer) => {
                events_ctx
                    .unpack_stream(
                        partial_result_handler,
                        replayer,
                        self.tailers.take(),
                        console_interaction,
                    )
                    .await
            }
        }
    }

    pub async fn kill(&mut self, reason: &str) -> anyhow::Result<()> {
        let pid = self.info.pid;
        let callers = get_callers_for_kill();

        let request_fut = self
            .client
            .daemon_only_mut()
            .kill(Request::new(KillRequest {
                reason: reason.to_owned(),
                timeout: Some(GRACEFUL_SHUTDOWN_TIMEOUT.try_into()?),
                caller: if callers.is_empty() {
                    None
                } else {
                    Some(itertools::join(callers.iter(), " -> "))
                },
                callers,
            }));
        let time_to_kill = GRACEFUL_SHUTDOWN_TIMEOUT + FORCE_SHUTDOWN_TIMEOUT;
        let time_req_sent = Instant::now();
        // First we send a Kill request
        let kill_behavior = match tokio::time::timeout(time_to_kill, request_fut).await {
            Ok(inner_result) => {
                match inner_result {
                    Ok(_) => KillBehavior::WaitForExit,
                    Err(e) => {
                        // The kill request can fail if the server is in a bad state and we cannot
                        // authenticate to it.
                        crate::eprintln!("Error requesting graceful shutdown: {}", e)?;
                        // Try an OS-level terminate next.
                        KillBehavior::TerminateFirst
                    }
                }
            }
            Err(_) => KillBehavior::WaitForExit,
        };
        // Then we do a wait_for on the pid, and if that times out, we kill it harder
        Self::kill_impl(
            pid,
            kill_behavior,
            time_to_kill.saturating_sub(time_req_sent.elapsed()),
        )
        .await
    }

    #[cfg(unix)]
    async fn kill_impl(pid: i64, behavior: KillBehavior, timeout: Duration) -> anyhow::Result<()> {
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

        match behavior {
            KillBehavior::TerminateFirst => {
                crate::eprintln!("Sending SIGTERM.")?;
                // We send SIGKILL below even if this fails.
                let _ignored = nix::sys::signal::kill(daemon_pid, Signal::SIGTERM);
            }
            KillBehavior::WaitForExit => {}
        };

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
    async fn kill_impl(pid: i64, behavior: KillBehavior, timeout: Duration) -> anyhow::Result<()> {
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
        // If proc_handle is null, process died already.
        if proc_handle.is_null() {
            return Ok(());
        }
        let proc_handle = HandleWrapper {
            handle: proc_handle,
        };
        let wait_result = match behavior {
            KillBehavior::WaitForExit => unsafe {
                WaitForSingleObject(proc_handle.handle, timeout.as_millis().try_into()?)
            },
            KillBehavior::TerminateFirst => {
                // Don't wait for the process to die first if we were asked to just terminate it,
                // fall through directly to TerminateProcess instead.
                WAIT_TIMEOUT
            }
        };
        match wait_result {
            WAIT_OBJECT_0 => Ok(()), // process exited successfully
            WAIT_TIMEOUT => {
                // If process isn't signalled, terminate it forcefully.
                match unsafe { TerminateProcess(proc_handle.handle, 1) } {
                    0 => Err(anyhow::anyhow!("Failed to kill daemon ({})", daemon_pid)),
                    _ => Ok(()),
                }
            }
            error_code => Err(anyhow::anyhow!(
                "Waiting for daemon process failed. Error code: {:#x}",
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

    pub async fn set_log_filter(&mut self, req: SetLogFilterRequest) -> anyhow::Result<()> {
        self.client
            .daemon_only_mut()
            .set_log_filter(Request::new(req))
            .await?;

        Ok(())
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

enum NoPartialResult {}

impl TryFrom<buck2_cli_proto::partial_result::PartialResult> for NoPartialResult {
    type Error = buck2_cli_proto::partial_result::PartialResult;

    fn try_from(v: buck2_cli_proto::partial_result::PartialResult) -> Result<Self, Self::Error> {
        Err(v)
    }
}

struct NoPartialResultHandler;

#[async_trait]
impl PartialResultHandler for NoPartialResultHandler {
    type PartialResult = NoPartialResult;

    fn new() -> Self {
        Self
    }

    async fn handle_partial_result(
        &mut self,
        _ctx: PartialResultCtx<'_>,
        partial_res: Self::PartialResult,
    ) -> anyhow::Result<()> {
        match partial_res {}
    }
}

/// Receives StdoutBytes, writes them to stdout.
struct StdoutPartialResultHandler;

#[async_trait]
impl PartialResultHandler for StdoutPartialResultHandler {
    type PartialResult = buck2_cli_proto::StdoutBytes;

    fn new() -> Self {
        Self
    }

    async fn handle_partial_result(
        &mut self,
        mut ctx: PartialResultCtx<'_>,
        partial_res: Self::PartialResult,
    ) -> anyhow::Result<()> {
        ctx.stdout(&partial_res.data).await
    }
}

/// Implement a streaming method with full event reporting.
macro_rules! stream_method {
    ($method: ident, $req: ty, $res: ty, $handler: ty) => {
        stream_method!($method, $method, $req, $res, $handler);
    };

    ($method: ident, $grpc_method: ident, $req: ty, $res: ty, $handler: ty) => {
        pub async fn $method(
            &mut self,
            req: $req,
            console_interaction: Option<ConsoleInteractionStream<'_>>,
        ) -> anyhow::Result<CommandOutcome<$res>> {
            self.enter()?;
            let mut handler = <$handler>::new();
            let res = self
                .inner
                .stream(
                    |d, r| Box::pin(DaemonApiClient::$grpc_method(d, r)),
                    req,
                    // For now we only support handlers that can be constructed like so, and we
                    // don't let anything go out. Eventually if we wanted to stream structured
                    // data, that could change.
                    &mut handler,
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
    ($method: ident, $req: ty, $res: ty, $handler: ty) => {
        bidirectional_stream_method!($method, $method, $req, $res, $handler);
    };

    ($method: ident, $grpc_method: ident, $req: ty, $res: ty, $handler: ty) => {
        pub async fn $method(
            &mut self,
            context: ClientContext,
            requests: impl Stream<Item = $req> + Send + Sync + 'static,
        ) -> anyhow::Result<CommandOutcome<$res>> {
            self.enter()?;
            let mut handler = <$handler>::new();
            let req = create_client_stream(context, requests);
            let res = self
                .inner
                .stream(
                    |d, r| Box::pin(DaemonApiClient::$method(d, r)),
                    req,
                    &mut handler,
                    None,
                )
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
    stream_method!(
        aquery,
        AqueryRequest,
        AqueryResponse,
        StdoutPartialResultHandler
    );
    stream_method!(
        cquery,
        CqueryRequest,
        CqueryResponse,
        NoPartialResultHandler
    );
    stream_method!(
        uquery,
        UqueryRequest,
        UqueryResponse,
        NoPartialResultHandler
    );
    stream_method!(
        targets,
        TargetsRequest,
        TargetsResponse,
        StdoutPartialResultHandler
    );
    stream_method!(
        targets_show_outputs,
        TargetsRequest,
        TargetsShowOutputsResponse,
        NoPartialResultHandler
    );
    stream_method!(build, BuildRequest, BuildResponse, NoPartialResultHandler);
    stream_method!(bxl, BxlRequest, BxlResponse, NoPartialResultHandler);
    stream_method!(test, TestRequest, TestResponse, NoPartialResultHandler);
    stream_method!(
        install,
        InstallRequest,
        InstallResponse,
        NoPartialResultHandler
    );
    stream_method!(
        audit,
        GenericRequest,
        GenericResponse,
        StdoutPartialResultHandler
    );
    stream_method!(
        starlark,
        GenericRequest,
        GenericResponse,
        NoPartialResultHandler
    );
    stream_method!(
        materialize,
        MaterializeRequest,
        MaterializeResponse,
        NoPartialResultHandler
    );
    stream_method!(
        clean_stale,
        CleanStaleRequest,
        CleanStaleResponse,
        NoPartialResultHandler
    );
    stream_method!(
        file_status,
        FileStatusRequest,
        GenericResponse,
        NoPartialResultHandler
    );
    stream_method!(
        unstable_docs,
        UnstableDocsRequest,
        UnstableDocsResponse,
        NoPartialResultHandler
    );
    stream_method!(
        profile,
        profile2,
        ProfileRequest,
        ProfileResponse,
        NoPartialResultHandler
    );
    stream_method!(
        allocative,
        AllocativeRequest,
        AllocativeResponse,
        NoPartialResultHandler
    );

    bidirectional_stream_method!(lsp, LspRequest, LspResponse, NoPartialResultHandler);

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
    wrap_method!(set_log_filter(log_filter: SetLogFilterRequest), ());
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

fn get_callers_for_kill() -> Vec<String> {
    /// Add a proess to our parts, and return its parent PID.
    fn push_process(pid: Pid, system: &mut System, process_tree: &mut Vec<String>) -> Option<Pid> {
        system.refresh_process_specifics(pid, ProcessRefreshKind::new());
        let proc = system.process(pid)?;
        let title = shlex::join(proc.cmd().iter().map(|s| s.as_str()));
        process_tree.push(title);
        proc.parent()
    }

    let mut system = System::new();
    let mut process_tree = Vec::new();

    let mut pid = Some(Pid::from_u32(std::process::id()));
    while let Some(p) = pid {
        pid = push_process(p, &mut system, &mut process_tree);
    }

    process_tree.into_iter().rev().collect()
}

#[cfg(test)]
mod tests {
    use std::io::Write;

    use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;

    use super::*;
    use crate::events_ctx::FileTailerEvent;
    use crate::file_tailer::FileTailer;
    use crate::file_tailer::StdoutOrStderr;

    #[tokio::test]
    async fn test_tailer() -> anyhow::Result<()> {
        let mut file = tempfile::NamedTempFile::new()?;
        writeln!(file, "before")?;

        // If we could control the interval for tailer polling, we could reliably
        // test more of the behavior. For now, just test a simple case.
        let (sender, mut receiver) = tokio::sync::mpsc::unbounded_channel();
        let tailer = FileTailer::tail_file(
            AbsNormPathBuf::new(file.path().to_owned())?,
            sender,
            StdoutOrStderr::Stdout,
        )?;
        writeln!(file, "after")?;
        // have to sleep long enough for a read or else this test is racy.
        tokio::time::sleep(Duration::from_millis(250)).await;
        std::mem::drop(tailer);
        assert_eq!(
            FileTailerEvent::Stdout("after\n".to_owned()),
            receiver.recv().await.unwrap()
        );

        Ok(())
    }
}
