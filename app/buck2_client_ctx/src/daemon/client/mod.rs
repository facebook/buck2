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
use std::time::Duration;

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
use crate::subscribers::observer::ErrorCause;

pub mod connect;
pub mod kill;

use crate::startup_deadline::StartupDeadline;

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

    pub fn collect_error_cause(&self) -> ErrorCause {
        for s in &self.client.events_ctx.subscribers {
            if let Some(observer) = s.as_error_observer() {
                return observer.error_cause();
                // TODO: handle the situation where multiple observers exist and their causes disagree
            }
        }

        ErrorCause::Unknown
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
    client: DaemonApiClient<InterceptedService<Channel, BuckAddAuthTokenInterceptor>>,
    info: DaemonProcessInfo,
    daemon_dir: DaemonDir,
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

impl BuckdClient {
    fn open_tailers(&mut self) -> anyhow::Result<()> {
        let tailers = FileTailers::new(&self.daemon_dir)?;
        self.tailers = Some(tailers);

        Ok(())
    }

    /// Some commands stream events back from the server.
    /// For these commands, we want to be able to manipulate CLI state.
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

        let response = command(client, Request::new(request))
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

    pub async fn kill(&mut self, reason: &str) -> anyhow::Result<()> {
        kill::kill(&mut self.client, &self.info, reason).await
    }

    pub async fn status(&mut self, snapshot: bool) -> anyhow::Result<StatusResponse> {
        let outcome = self
            .events_ctx
            // Safe to unwrap tailers here because they are instantiated prior to a command being called.
            .unpack_oneshot(&mut self.tailers, || {
                self.client.status(Request::new(StatusRequest { snapshot }))
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

    pub async fn set_log_filter(&mut self, req: SetLogFilterRequest) -> anyhow::Result<()> {
        self.client.set_log_filter(Request::new(req)).await?;

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

pub enum NoPartialResult {}

impl TryFrom<buck2_cli_proto::partial_result::PartialResult> for NoPartialResult {
    type Error = buck2_cli_proto::partial_result::PartialResult;

    fn try_from(v: buck2_cli_proto::partial_result::PartialResult) -> Result<Self, Self::Error> {
        Err(v)
    }
}

pub struct NoPartialResultHandler;

#[async_trait]
impl PartialResultHandler for NoPartialResultHandler {
    type PartialResult = NoPartialResult;

    async fn handle_partial_result(
        &mut self,
        _ctx: PartialResultCtx<'_>,
        partial_res: Self::PartialResult,
    ) -> anyhow::Result<()> {
        match partial_res {}
    }
}

/// Receives StdoutBytes, writes them to stdout.
pub struct StdoutPartialResultHandler;

#[async_trait]
impl PartialResultHandler for StdoutPartialResultHandler {
    type PartialResult = buck2_cli_proto::StdoutBytes;

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
    ($method: ident, $req: ty, $res: ty, $message: ty) => {
        stream_method!($method, $method, $req, $res, $message);
    };

    ($method: ident, $grpc_method: ident, $req: ty, $res: ty, $message: ty) => {
        pub async fn $method(
            &mut self,
            req: $req,
            console_interaction: Option<ConsoleInteractionStream<'_>>,
            handler: &mut impl PartialResultHandler<PartialResult = $message>,
        ) -> anyhow::Result<CommandOutcome<$res>> {
            self.enter()?;
            let res = self
                .inner
                .stream(
                    |d, r| Box::pin(DaemonApiClient::$grpc_method(d, r)),
                    req,
                    // For now we only support handlers that can be constructed like so, and we
                    // don't let anything go out. Eventually if we wanted to stream structured
                    // data, that could change.
                    handler,
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
    ($method: ident, $req: ty, $res: ty, $message: ty) => {
        bidirectional_stream_method!($method, $method, $req, $res, $message);
    };

    ($method: ident, $grpc_method: ident, $req: ty, $res: ty, $message: ty) => {
        pub async fn $method(
            &mut self,
            context: ClientContext,
            requests: impl Stream<Item = $req> + Send + Sync + 'static,
            handler: &mut impl PartialResultHandler<PartialResult = $message>,
        ) -> anyhow::Result<CommandOutcome<$res>> {
            self.enter()?;
            let req = create_client_stream(context, requests);
            let res = self
                .inner
                .stream(
                    |d, r| Box::pin(DaemonApiClient::$method(d, r)),
                    req,
                    handler,
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
                    self.inner.client.$method(Request::new(req))
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
            let out = self.inner.client.$method(Request::new(req)).await;
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
        buck2_cli_proto::StdoutBytes
    );
    stream_method!(
        cquery,
        CqueryRequest,
        CqueryResponse,
        buck2_cli_proto::StdoutBytes
    );
    stream_method!(
        uquery,
        UqueryRequest,
        UqueryResponse,
        buck2_cli_proto::StdoutBytes
    );
    stream_method!(
        targets,
        TargetsRequest,
        TargetsResponse,
        buck2_cli_proto::StdoutBytes
    );
    stream_method!(
        targets_show_outputs,
        TargetsRequest,
        TargetsShowOutputsResponse,
        NoPartialResult
    );
    stream_method!(
        ctargets,
        ConfiguredTargetsRequest,
        ConfiguredTargetsResponse,
        NoPartialResult
    );
    stream_method!(build, BuildRequest, BuildResponse, NoPartialResult);
    stream_method!(bxl, BxlRequest, BxlResponse, buck2_cli_proto::StdoutBytes);
    stream_method!(test, TestRequest, TestResponse, NoPartialResult);
    stream_method!(install, InstallRequest, InstallResponse, NoPartialResult);
    stream_method!(
        audit,
        GenericRequest,
        GenericResponse,
        buck2_cli_proto::StdoutBytes
    );
    stream_method!(
        starlark,
        GenericRequest,
        GenericResponse,
        buck2_cli_proto::StdoutBytes
    );
    stream_method!(
        materialize,
        MaterializeRequest,
        MaterializeResponse,
        NoPartialResult
    );
    stream_method!(
        clean_stale,
        CleanStaleRequest,
        CleanStaleResponse,
        NoPartialResult
    );
    stream_method!(
        file_status,
        FileStatusRequest,
        GenericResponse,
        NoPartialResult
    );
    stream_method!(
        unstable_docs,
        UnstableDocsRequest,
        UnstableDocsResponse,
        NoPartialResult
    );
    stream_method!(
        profile,
        profile2,
        ProfileRequest,
        ProfileResponse,
        NoPartialResult
    );
    stream_method!(
        allocative,
        AllocativeRequest,
        AllocativeResponse,
        NoPartialResult
    );

    bidirectional_stream_method!(lsp, LspRequest, LspResponse, LspMessage);
    bidirectional_stream_method!(dap, DapRequest, DapResponse, DapMessage);
    bidirectional_stream_method!(
        subscription,
        SubscriptionRequestWrapper,
        SubscriptionCommandResponse,
        SubscriptionResponseWrapper
    );

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
    wrap_method!(set_log_filter(log_filter: SetLogFilterRequest), ());
    stream_method!(trace_io, TraceIoRequest, TraceIoResponse, NoPartialResult);
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
