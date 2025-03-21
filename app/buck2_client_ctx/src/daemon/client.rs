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
use std::mem;
use std::time::Duration;

use async_trait::async_trait;
use buck2_cli_proto::daemon_api_client::*;
use buck2_cli_proto::new_generic::NewGenericRequest;
use buck2_cli_proto::new_generic::NewGenericResponse;
use buck2_cli_proto::*;
use buck2_common::daemon_dir::DaemonDir;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::file_name::FileName;
use buck2_data::error::ErrorTag;
use buck2_error::BuckErrorContext;
use buck2_event_log::stream_value::StreamValue;
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
use crate::events_ctx::PartialResultCtx;
use crate::events_ctx::PartialResultHandler;
use crate::file_tailers::tailers::FileTailers;

pub mod connect;
pub mod kill;

use crate::startup_deadline::StartupDeadline;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Environment)]
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
    pub fn with_flushing(&mut self) -> FlushingBuckdClient {
        FlushingBuckdClient {
            inner: &mut self.client,
        }
    }

    pub fn daemon_constraints(&self) -> &buck2_cli_proto::DaemonConstraints {
        &self.client.constraints
    }
}

pub struct BuckdLifecycleLock {
    daemon_dir: DaemonDir,
    lock_file: File,
}

impl BuckdLifecycleLock {
    const BUCKD_LIFECYCLE: &'static str = "buckd.lifecycle";
    const BUCKD_PREV_DIR: &'static str = "prev";

    pub async fn lock_with_timeout(
        daemon_dir: DaemonDir,
        deadline: StartupDeadline,
    ) -> buck2_error::Result<BuckdLifecycleLock> {
        create_dir_all(&daemon_dir.path)?;
        let lifecycle_path = daemon_dir.path.as_path().join(Self::BUCKD_LIFECYCLE);
        let file = File::create(lifecycle_path)?;
        let fileref = &file;
        deadline
            .retrying(
                "locking buckd lifecycle",
                Duration::from_millis(5),
                Duration::from_millis(100),
                || async { Ok(fs4::FileExt::try_lock_exclusive(fileref)?) },
            )
            .await?;

        Ok(BuckdLifecycleLock {
            lock_file: file,
            daemon_dir,
        })
    }

    /// Remove everything except `buckd.lifecycle` file which is the lock file.
    /// If `keep_prev` is true, backup previous daemon logs to `prev` dir for debugging.
    pub fn clean_daemon_dir(&self, keep_prev: bool) -> buck2_error::Result<()> {
        let prev_daemon_dir = self
            .daemon_dir
            .path
            .join(FileName::new(Self::BUCKD_PREV_DIR).unwrap());
        if keep_prev {
            if prev_daemon_dir.is_dir() {
                fs_util::remove_dir_all(&prev_daemon_dir)?;
            }
            fs_util::create_dir_all(&prev_daemon_dir)?;
        }

        let mut seen_lifecycle = false;
        for p in fs_util::read_dir(&self.daemon_dir.path)? {
            let p = p?;
            if p.file_name() == Self::BUCKD_LIFECYCLE {
                seen_lifecycle = true;
                continue;
            }
            if keep_prev {
                if p.file_name() != Self::BUCKD_PREV_DIR {
                    let file_name = p.file_name();
                    let file_name = FileName::from_os_string(&file_name)?;
                    fs_util::rename(p.path(), prev_daemon_dir.join(file_name))?;
                }
            } else {
                fs_util::remove_all(p.path())?;
            }
        }
        if !seen_lifecycle {
            // Self-check.
            return Err(LifecycleError::MissingLifecycle(self.daemon_dir.path.clone()).into());
        }
        Ok(())
    }

    pub fn daemon_dir(&self) -> &DaemonDir {
        &self.daemon_dir
    }
}

impl Drop for BuckdLifecycleLock {
    fn drop(&mut self) {
        fs4::FileExt::unlock(&self.lock_file)
            .expect("Unexpected failure to unlock buckd.lifecycle file.")
    }
}

/// This provides a thin wrapper around the proto-generated DaemonApiClient and hides
/// some of the complexity/verbosity of making calls with that. For example, the user
/// doesn't need to deal with tonic::Response/Request and this may provide functions
/// that take more primitive types than the protobuf structure itself.
pub struct BuckdClient {
    client: DaemonApiClient<InterceptedService<Channel, BuckAddAuthTokenInterceptor>>,
    constraints: buck2_cli_proto::DaemonConstraints,
    daemon_dir: DaemonDir,
    // TODO(brasselsprouts): events_ctx should own tailers
    tailers: Option<FileTailers>,
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Tier0)]
enum GrpcToStreamError {
    #[error("buck daemon returned an empty CommandProgress")]
    EmptyCommandProgress,
}

/// Convert tonic error to our error.
///
/// This function **must** be used explicitly to convert the error, because we want a tag.
pub(crate) fn tonic_status_to_error(status: tonic::Status) -> buck2_error::Error {
    let mut tags = vec![ErrorTag::ClientGrpc];
    if status.code() == tonic::Code::ResourceExhausted {
        // The error looks like this:
        // ```
        // status: ResourceExhausted
        // message: "Cannot return body with more than 4GB of data but got 4294992775 bytes"
        // details: [], metadata: MetadataMap { headers: {} }
        // ```
        if status
            .message()
            .contains("Cannot return body with more than")
        {
            tags.push(ErrorTag::GrpcResponseMessageTooLarge);
        }
    }
    buck2_error::Error::from(status).tag(tags)
}

/// Translates a tonic streaming response into a stream of StreamValues, the set of things that can flow across the gRPC
/// event stream.
fn grpc_to_stream(
    response: buck2_error::Result<tonic::Response<tonic::Streaming<MultiCommandProgress>>>,
) -> impl Stream<Item = buck2_error::Result<StreamValue>> {
    let stream = match response {
        Ok(response) => response.into_inner(),
        Err(e) => return futures::stream::once(futures::future::ready(Err(e))).left_stream(),
    };

    let stream = stream
        .map_err(tonic_status_to_error)
        .map_ok(|e| stream::iter(e.messages.into_iter().map(buck2_error::Ok)))
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
    fn open_tailers(&mut self) -> buck2_error::Result<()> {
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
        events_ctx: &mut EventsCtx,
        partial_result_handler: &mut Handler,
        console_interaction: Option<ConsoleInteractionStream<'i>>,
    ) -> buck2_error::Result<CommandOutcome<Res>>
    where
        Command: for<'b> FnOnce(
            &'b mut DaemonApiClient<InterceptedService<Channel, BuckAddAuthTokenInterceptor>>,
            Request<T>,
        ) -> BoxFuture<
            'b,
            Result<tonic::Response<tonic::Streaming<MultiCommandProgress>>, Status>,
        >,
        Res: TryFrom<command_result::Result, Error = command_result::Result>,
        Handler: PartialResultHandler,
    {
        let Self { client, .. } = self;

        let response = command(client, Request::new(request))
            .await
            .map_err(tonic_status_to_error)
            .buck_error_context("Error dispatching request");
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

    pub async fn status(
        &mut self,
        events_ctx: &mut EventsCtx,
        snapshot: bool,
    ) -> buck2_error::Result<StatusResponse> {
        let outcome = events_ctx
            // Safe to unwrap tailers here because they are instantiated prior to a command being called.
            .unpack_oneshot(mem::take(&mut self.tailers), {
                self.client.status(Request::new(StatusRequest { snapshot }))
            })
            .await;
        // TODO(nmj): We have a number of things that wish to use status() and return an buck2_error::Result,
        // for now we'll just turn a "CommandMessage" into a error, but that's really not what we
        // want long term.
        match outcome? {
            CommandOutcome::Success(r) => Ok(r),
            CommandOutcome::Failure(_) => Err(buck2_error::buck2_error!(
                buck2_error::ErrorTag::Tier0,
                "Unexpected failure message in status()"
            )),
        }
    }

    pub async fn set_log_filter(
        &mut self,
        _events_ctx: &mut EventsCtx,
        req: SetLogFilterRequest,
    ) -> buck2_error::Result<()> {
        self.client.set_log_filter(Request::new(req)).await?;

        Ok(())
    }
}

pub struct FlushingBuckdClient<'a> {
    inner: &'a mut BuckdClient,
}

impl<'a> FlushingBuckdClient<'a> {
    fn enter(&mut self) -> buck2_error::Result<()> {
        self.inner.open_tailers()?;
        Ok(())
    }

    async fn exit(&mut self, events_ctx: &mut EventsCtx) -> buck2_error::Result<()> {
        events_ctx.flush(mem::take(&mut self.inner.tailers)).await?;

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
    ) -> buck2_error::Result<()> {
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
    ) -> buck2_error::Result<()> {
        ctx.stdout(&partial_res.data).await
    }
}

/// Implement a streaming method with full event reporting.
macro_rules! stream_method {
    ($method: ident, $req: ty, $res: ty, $message: ty) => {
        stream_method!($method, $method, $req, $res, $message);
    };

    ($method: ident, $grpc_method: ident, $req: ty, $res: ty, $message: ty) => {
        pub async fn $method<'i, 'j: 'i>(
            &mut self,
            req: $req,
            events_ctx: &mut EventsCtx,
            console_interaction: Option<ConsoleInteractionStream<'j>>,
            handler: &mut impl PartialResultHandler<PartialResult = $message>,
        ) -> buck2_error::Result<CommandOutcome<$res>> {
            self.enter()?;
            let res = self
                .inner
                .stream(
                    |d, r| Box::pin(DaemonApiClient::$grpc_method(d, r)),
                    req,
                    events_ctx,
                    // For now we only support handlers that can be constructed like so, and we
                    // don't let anything go out. Eventually if we wanted to stream structured
                    // data, that could change.
                    handler,
                    console_interaction,
                )
                .await;
            self.exit(events_ctx).await?;
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
            events_ctx: &mut EventsCtx,
            handler: &mut impl PartialResultHandler<PartialResult = $message>,
        ) -> buck2_error::Result<CommandOutcome<$res>> {
            self.enter()?;
            let req = create_client_stream(context, requests);
            let res = self
                .inner
                .stream(
                    |d, r| Box::pin(DaemonApiClient::$method(d, r)),
                    req,
                    events_ctx,
                    handler,
                    None,
                )
                .await;
            self.exit(events_ctx).await?;
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
        pub async fn $method(
            &mut self,
            req: $req,
            events_ctx: &mut EventsCtx,
        ) -> buck2_error::Result<CommandOutcome<$res>> {
            self.enter()?;
            let res = events_ctx
                .unpack_oneshot(mem::take(&mut self.inner.tailers), {
                    self.inner.client.$method(Request::new(req))
                })
                .await;
            self.exit(events_ctx).await?;
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
        pub async fn $method(
            &mut self,
            req: $req,
            events_ctx: &mut EventsCtx,
        ) -> buck2_error::Result<$res> {
            self.enter()?;
            let out = self.inner.client.$method(Request::new(req)).await;
            self.exit(events_ctx).await?;
            Ok(out?.into_inner())
        }
    };
}

/// Wrap a method that exists on the BuckdClient, with flushing.
macro_rules! wrap_method {
     ($method: ident ($($param: ident : $param_type: ty),*), $res: ty) => {
         pub async fn $method(&mut self, events_ctx: &mut EventsCtx, $($param: $param_type)*) -> buck2_error::Result<$res> {
             self.enter()?;
             let out = self
                 .inner
                 .$method(events_ctx, $($param)*)
                 .await;
             self.exit(events_ctx).await?;
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
        new_generic_impl,
        NewGenericRequestMessage,
        NewGenericResponseMessage,
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
        buck2_cli_proto::StdoutBytes
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

    oneshot_method!(unstable_crash, UnstableCrashRequest, GenericResponse);
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

    wrap_method!(status(snapshot: bool), StatusResponse);
    wrap_method!(set_log_filter(log_filter: SetLogFilterRequest), ());
    stream_method!(trace_io, TraceIoRequest, TraceIoResponse, NoPartialResult);

    pub async fn new_generic(
        &mut self,
        context: buck2_cli_proto::ClientContext,
        req: NewGenericRequest,
        events_ctx: &mut EventsCtx,
        stdin: Option<ConsoleInteractionStream<'_>>,
    ) -> buck2_error::Result<CommandOutcome<NewGenericResponse>> {
        let req = serde_json::to_string(&req)
            .buck_error_context("Could not serialize `NewGenericRequest`")?;
        let req = buck2_cli_proto::NewGenericRequestMessage {
            context: Some(context),
            new_generic_request: req,
        };
        let command_outcome: CommandOutcome<buck2_cli_proto::NewGenericResponseMessage> = self
            .new_generic_impl(req, events_ctx, stdin, &mut NoPartialResultHandler)
            .await?;
        match command_outcome {
            CommandOutcome::Success(resp) => {
                let resp = serde_json::from_str(&resp.new_generic_response)
                    .buck_error_context("Could not deserialize `NewGenericResponse`")?;
                Ok(CommandOutcome::Success(resp))
            }
            CommandOutcome::Failure(code) => Ok(CommandOutcome::Failure(code)),
        }
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
