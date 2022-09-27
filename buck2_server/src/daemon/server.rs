/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(clippy::significant_drop_in_scrutinee)] // FIXME?

use std::future;
use std::path::Path;
use std::pin::Pin;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::task::Context;
use std::task::Poll;
use std::time::Duration;
use std::time::Instant;
use std::time::SystemTime;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_build_api::actions::build_listener;
use buck2_build_api::bxl::calculation::BxlCalculationDyn;
use buck2_build_api::configure_dice::configure_dice_for_buck;
use buck2_build_api::spawner::BuckSpawner;
use buck2_common::invocation_paths::InvocationPaths;
use buck2_common::io::IoProvider;
use buck2_common::legacy_configs::LegacyBuckConfig;
use buck2_common::memory;
use buck2_events::dispatch::EventDispatcher;
use buck2_events::ControlEvent;
use buck2_events::Event;
use buck2_events::EventSource;
use buck2_interpreter::dice::starlark_profiler::StarlarkProfilerConfiguration;
use buck2_interpreter::dice::HasEvents;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use cli_proto::daemon_api_server::*;
use cli_proto::*;
use dice::cycles::DetectCycles;
use dice::Dice;
use futures::channel::mpsc;
use futures::channel::mpsc::UnboundedSender;
use futures::stream;
use futures::Future;
use futures::Stream;
use futures::StreamExt;
use gazebo::prelude::*;
use more_futures::drop::DropTogether;
use more_futures::spawn::spawn_dropcancel;
use starlark::environment::GlobalsBuilder;
use tokio::io::AsyncRead;
use tokio::io::AsyncWrite;
use tonic::transport::server::Connected;
use tonic::transport::Server;
use tonic::Code;
use tonic::Request;
use tonic::Response;
use tonic::Status;
use tracing::debug_span;

use crate::ctx::ServerCommandContext;
use crate::daemon::state::DaemonState;
use crate::daemon::state::DaemonStateDiceConstructor;
use crate::jemalloc_stats::jemalloc_stats;
use crate::lsp::run_lsp_server_command;
use crate::materialize::materialize_command;
use crate::profile::starlark_profiler_configuration_from_request;
use crate::snapshot;
use crate::streaming_request_handler::StreamingRequestHandler;

// TODO(cjhopman): Figure out a reasonable value for this.
static DEFAULT_KILL_TIMEOUT: Duration = Duration::from_millis(500);

pub trait BuckdServerDelegate: Send + Sync {
    fn force_shutdown(&self) -> anyhow::Result<()>;

    fn force_shutdown_with_timeout(&self, timeout: Duration);
}

struct DaemonShutdown {
    delegate: Box<dyn BuckdServerDelegate>,

    /// This channel is used to trigger a graceful shutdown of the grpc server. After
    /// an item is sent on this channel, the server will start rejecting new requests
    /// and once current requests are finished the server will shutdown.
    shutdown_channel: UnboundedSender<()>,
}

impl DaemonShutdown {
    /// Trigger a graceful server shutdown with a timeout. After the timeout expires, a hard shutdown
    /// will be triggered.
    ///
    /// As we might be processing a `kill()` (or other) request, we cannot wait for the server to actually
    /// shutdown (as it will wait for current requests to finish), so this returns immediately.
    fn start_shutdown(&self, timeout: Option<Duration>) {
        let timeout = timeout.unwrap_or(DEFAULT_KILL_TIMEOUT);

        // Ignore errrors on shutdown_channel as that would mean we've already started shutdown;
        let _ = self.shutdown_channel.unbounded_send(());
        self.delegate.force_shutdown_with_timeout(timeout);
    }
}

struct DaemonStateDiceConstructorImpl {
    /// Whether to detect cycles in Dice
    detect_cycles: Option<DetectCycles>,
    bxl_calculations: &'static dyn BxlCalculationDyn,
}

impl DaemonStateDiceConstructor for DaemonStateDiceConstructorImpl {
    fn construct_dice(
        &self,
        io: Arc<dyn IoProvider>,
        root_config: &LegacyBuckConfig,
    ) -> anyhow::Result<Arc<Dice>> {
        configure_dice_for_buck(
            io,
            self.bxl_calculations,
            Some(root_config),
            self.detect_cycles,
        )
    }
}

/// Access to functions which live outside of `buck2_server` crate.
#[async_trait]
pub trait BuckdServerDependencies: Send + Sync + 'static {
    async fn test(
        &self,
        ctx: Box<dyn ServerCommandContextTrait>,
        req: TestRequest,
    ) -> anyhow::Result<TestResponse>;
    async fn build(
        &self,
        ctx: Box<dyn ServerCommandContextTrait>,
        req: BuildRequest,
    ) -> anyhow::Result<BuildResponse>;
    async fn install(
        &self,
        ctx: Box<dyn ServerCommandContextTrait>,
        req: InstallRequest,
    ) -> anyhow::Result<InstallResponse>;
    async fn bxl(
        &self,
        ctx: Box<dyn ServerCommandContextTrait>,
        req: cli_proto::BxlRequest,
    ) -> anyhow::Result<BxlResponse>;
    async fn audit(
        &self,
        ctx: Box<dyn ServerCommandContextTrait>,
        req: cli_proto::GenericRequest,
    ) -> anyhow::Result<cli_proto::GenericResponse>;
    async fn profile(
        &self,
        ctx: Box<dyn ServerCommandContextTrait>,
        req: cli_proto::ProfileRequest,
    ) -> anyhow::Result<cli_proto::ProfileResponse>;
    async fn uquery(
        &self,
        ctx: Box<dyn ServerCommandContextTrait>,
        req: cli_proto::UqueryRequest,
    ) -> anyhow::Result<cli_proto::UqueryResponse>;
    async fn cquery(
        &self,
        ctx: Box<dyn ServerCommandContextTrait>,
        req: cli_proto::CqueryRequest,
    ) -> anyhow::Result<CqueryResponse>;
    async fn aquery(
        &self,
        ctx: Box<dyn ServerCommandContextTrait>,
        req: cli_proto::AqueryRequest,
    ) -> anyhow::Result<cli_proto::AqueryResponse>;
    async fn targets(
        &self,
        ctx: Box<dyn ServerCommandContextTrait>,
        req: TargetsRequest,
    ) -> anyhow::Result<TargetsResponse>;
    async fn targets_show_outputs(
        &self,
        ctx: Box<dyn ServerCommandContextTrait>,
        req: TargetsRequest,
    ) -> anyhow::Result<TargetsShowOutputsResponse>;
    async fn docs(
        &self,
        ctx: Box<dyn ServerCommandContextTrait>,
        req: cli_proto::UnstableDocsRequest,
    ) -> anyhow::Result<cli_proto::UnstableDocsResponse>;
    fn bxl_calculation(&self) -> &'static dyn BxlCalculationDyn;
    fn configure_bxl_file_globals(&self) -> fn(&mut GlobalsBuilder);
}

/// The BuckdServer implements the DaemonApi.
///
/// Simple endpoints are implemented here and complex things will be implemented in a sibling
/// module taking just a ServerCommandContext.
pub struct BuckdServer {
    /// The flag that is set to true when server is shutting down.
    stop_accepting_requests: AtomicBool,
    process_info: DaemonProcessInfo,
    start_time: prost_types::Timestamp,
    start_instant: Instant,
    daemon_shutdown: Arc<DaemonShutdown>,
    daemon_state: Arc<DaemonState>,

    callbacks: &'static dyn BuckdServerDependencies,
}

impl BuckdServer {
    pub async fn run<I, IO, IE>(
        fb: fbinit::FacebookInit,
        paths: InvocationPaths,
        delegate: Box<dyn BuckdServerDelegate>,
        detect_cycles: Option<DetectCycles>,
        process_info: DaemonProcessInfo,
        listener: I,
        callbacks: &'static dyn BuckdServerDependencies,
    ) -> anyhow::Result<()>
    where
        I: Stream<Item = Result<IO, IE>>,
        IO: AsyncRead + AsyncWrite + Connected + Unpin + Send + 'static,
        IE: Into<Box<dyn std::error::Error + Send + Sync>> + Send,
    {
        let now = SystemTime::now();
        let now = now.duration_since(SystemTime::UNIX_EPOCH)?;

        let (shutdown_channel, mut receiver): (UnboundedSender<()>, _) = mpsc::unbounded();

        let api_server = Self {
            stop_accepting_requests: AtomicBool::new(false),
            process_info,
            start_time: prost_types::Timestamp {
                seconds: now.as_secs() as i64,
                nanos: now.subsec_nanos() as i32,
            },
            start_instant: Instant::now(),
            daemon_shutdown: Arc::new(DaemonShutdown {
                delegate,
                shutdown_channel,
            }),
            daemon_state: Arc::new(DaemonState::new(
                fb,
                paths,
                box DaemonStateDiceConstructorImpl {
                    detect_cycles,
                    bxl_calculations: callbacks.bxl_calculation(),
                },
            )?),
            callbacks,
        };

        let server = Server::builder()
            .add_service(DaemonApiServer::new(api_server))
            .serve_with_incoming_shutdown(listener, async move {
                receiver.next().await;
            });

        server.await?;

        Ok(())
    }

    /// Run a request that does bidirectional streaming.
    ///
    /// This mostly just ensures that a client context has been sent first, and passes a client
    /// stream to `func` that converts to the correct type (or returns an error and shuts the
    /// stream down)
    async fn run_bidirectional<Req, Res, Fut, F>(
        &self,
        req: Request<tonic::Streaming<StreamingRequest>>,
        opts: impl StreamingCommandOptions<StreamingRequest>,
        func: F,
    ) -> Result<Response<ResponseStream>, Status>
    where
        F: FnOnce(ServerCommandContext, &ClientContext, StreamingRequestHandler<Req>) -> Fut
            + Send
            + 'static,
        Fut: Future<Output = anyhow::Result<Res>> + Send,
        Req: TryFrom<StreamingRequest, Error = Status> + Send + Sync + 'static,
        Res: Into<command_result::Result> + Send + 'static,
    {
        let mut req = req.into_inner();
        let init_request = match req.message().await? {
            Some(
                m @ StreamingRequest {
                    request: Some(cli_proto::streaming_request::Request::Context(_)),
                },
            ) => Ok(m),
            _ => Err(Status::failed_precondition(
                "no client context message was received",
            )),
        }?;

        let init_request = Request::new(init_request);
        self.run_streaming(init_request, opts, |ctx, init_req| {
            func(
                ctx,
                init_req
                    .client_context()
                    .expect("already checked for a valid context"),
                StreamingRequestHandler::new(req),
            )
        })
        .await
    }

    async fn run_streaming_anyhow<Req, Res, Fut, F>(
        &self,
        req: Request<Req>,
        opts: impl StreamingCommandOptions<Req>,
        func: F,
    ) -> anyhow::Result<Response<ResponseStream>>
    where
        F: FnOnce(ServerCommandContext, Req) -> Fut + Send + 'static,
        Fut: Future<Output = anyhow::Result<Res>> + Send,
        Req: HasClientContext + HasBuildOptions + HasRecordTargetCallStacks + Send + Sync + 'static,
        Res: Into<command_result::Result> + Send + 'static,
    {
        OneshotCommandOptions::pre_run(&opts, self)?;

        let daemon_state = self.daemon_state.dupe();
        let trace_id = req.get_ref().client_context()?.trace_id.parse()?;
        let (events, dispatch) = daemon_state.prepare_events(trace_id).await?;
        let data = daemon_state.data().await?;

        dispatch.instant_event(snapshot::SnapshotCollector::pre_initialization_snapshot(
            data.start_time,
        ));

        let configure_bxl_file_globals = self.callbacks.configure_bxl_file_globals();

        let resp = streaming(req, events, dispatch.dupe(), move |req| async move {
            let result: CommandResult = {
                let result: anyhow::Result<CommandResult> = try {
                    let base_context = daemon_state.prepare_command(dispatch.dupe()).await?;
                    build_listener::scope(base_context.events.dupe(), |build_sender| async {
                        let context = ServerCommandContext::new(
                            base_context,
                            req.client_context()?,
                            build_sender,
                            opts.starlark_profiler_instrumentation_override(&req)?,
                            req.build_options(),
                            daemon_state.paths.buck_out_dir(),
                            req.record_target_call_stacks(),
                            configure_bxl_file_globals,
                        )?;

                        let result = match func(context, req).await {
                            Ok(res) => CommandResult {
                                result: Some(res.into()),
                            },
                            Err(e) => error_to_command_result(e),
                        };

                        Ok(result)
                    })
                    .await?
                };

                match result {
                    Ok(result) => result,
                    Err(e) => error_to_command_result(e),
                }
            };
            dispatch.control_event(ControlEvent::CommandResult(result));
        })
        .await;
        Ok(resp)
    }

    /// Runs a single command (given by the func F). Prior to running the command, calls the
    /// `opts`'s `pre_run` hook.  then bootstraps an event source and command context so that the
    /// invoked function has the ability to stream events to the caller.
    async fn run_streaming<Req, Res, Fut, F>(
        &self,
        req: Request<Req>,
        opts: impl StreamingCommandOptions<Req>,
        func: F,
    ) -> Result<Response<ResponseStream>, Status>
    where
        F: FnOnce(ServerCommandContext, Req) -> Fut + Send + 'static,
        Fut: Future<Output = anyhow::Result<Res>> + Send,
        Req: HasClientContext + HasBuildOptions + HasRecordTargetCallStacks + Send + Sync + 'static,
        Res: Into<command_result::Result> + Send + 'static,
    {
        Ok(self
            .run_streaming_anyhow(req, opts, func)
            .await
            .unwrap_or_else(error_to_response_stream))
    }

    async fn oneshot<
        Req,
        Res: Into<command_result::Result>,
        Fut: Future<Output = anyhow::Result<Res>> + Send,
        F: FnOnce(Req) -> Fut,
    >(
        &self,
        req: Request<Req>,
        opts: impl OneshotCommandOptions,
        func: F,
    ) -> Result<Response<CommandResult>, Status> {
        opts.pre_run(self)?;

        let req = req.into_inner();
        match func(req).await {
            Ok(val) => Ok(Response::new(CommandResult {
                result: Some(val.into()),
            })),
            Err(e) => Ok(Response::new(error_to_command_result(e))),
        }
    }

    /// Checks if the server is accepting requests.
    fn check_if_accepting_requests(&self) -> Result<(), Status> {
        if self.stop_accepting_requests.load(Ordering::Relaxed) {
            Err(Status::failed_precondition(
                "Failed to run command, `buckd` is shutting down soon!",
            ))
        } else {
            Ok(())
        }
    }
}

fn convert_positive_duration(proto_duration: &prost_types::Duration) -> Result<Duration, Status> {
    if proto_duration.seconds < 0 || proto_duration.nanos < 0 {
        return Err(Status::new(
            Code::Unknown,
            format!("received invalid timeout: `{:?}`", proto_duration),
        ));
    }
    Ok(Duration::from_secs(proto_duration.seconds as u64)
        + Duration::from_nanos(proto_duration.nanos as u64))
}

fn error_to_command_result(e: anyhow::Error) -> CommandResult {
    let messages = vec![format!("{:?}", e)];

    CommandResult {
        result: Some(command_result::Result::Error(CommandError { messages })),
    }
}

fn error_to_command_progress(e: anyhow::Error) -> CommandProgress {
    CommandProgress {
        progress: Some(command_progress::Progress::Result(error_to_command_result(
            e,
        ))),
    }
}

fn error_to_response_stream(e: anyhow::Error) -> Response<ResponseStream> {
    tonic::Response::new(Box::pin(stream::once(future::ready(Ok(
        error_to_command_progress(e),
    )))))
}

/// tonic requires the response for a streaming api to be a Sync Stream. With async/await, that requirement is really difficult
/// to meet. This simple wrapper allows us to wrap a non-Sync stream into a Sync one (the inner stream is never accessed in a
/// non-exclusive manner).
struct SyncStream<T: Stream<Item = Result<CommandProgress, Status>> + Send> {
    // SyncWrapper provides a Sync type that only allows (statically checked) exclusive access to
    // the underlying object, this allows using a non-Sync object where a Sync one is required
    // but is never accessed from multiple threads.
    // See https://internals.rust-lang.org/t/what-shall-sync-mean-across-an-await/12020/31
    // and https://github.com/hyperium/tonic/issues/117
    wrapped: sync_wrapper::SyncWrapper<T>,
}

impl<T: Stream<Item = Result<CommandProgress, Status>> + Send> Stream for SyncStream<T> {
    type Item = <T as Stream>::Item;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        // This is a safe pin projection. See https://doc.rust-lang.org/std/pin/index.html#projections-and-structural-pinning
        // Specifically see the requirements when pinning is structural for a field here: https://doc.rust-lang.org/std/pin/index.html#pinning-is-structural-for-field
        unsafe { self.map_unchecked_mut(|a| a.wrapped.get_mut()) }.poll_next(cx)
    }
}

/// Dispatches a request to the given function and returns a stream of responses, suitable for streaming to a client.
#[allow(clippy::mut_mut)] // select! does this internally
async fn streaming<
    Req: Send + Sync + 'static,
    Fut: Future<Output = ()> + Send + 'static,
    F: FnOnce(Req) -> Fut,
    E: EventSource + 'static,
>(
    req: Request<Req>,
    mut events: E,
    dispatcher: EventDispatcher,
    func: F,
) -> Response<ResponseStream>
where
    F: Send + 'static,
    E: Sync,
{
    // This function is responsible for receiving all events coming into an EventSource and reacting accordingly. There
    // are two categories events that can be seen:
    // 1. Control events, which are not to be sent across the gRPC boundary but instruct this function to do something.
    // 2. Buck events, which are to be sent across the gRPC boundary.
    //
    // The function `func` is the computation that we are going to run. It communicates its success or failure using
    // control events; our first step is to spawn it.

    struct EventsCtx {
        dispatcher: EventDispatcher,
    }
    impl HasEvents for EventsCtx {
        fn get_dispatcher(&self) -> &EventDispatcher {
            &self.dispatcher
        }
    }

    let req = req.into_inner();
    let events_ctx = EventsCtx { dispatcher };
    let cancellable = spawn_dropcancel(
        func(req),
        Arc::new(BuckSpawner::default()),
        &events_ctx,
        debug_span!(parent: None, "running-command",),
    );
    let (output_send, output_recv) = tokio::sync::mpsc::unbounded_channel();

    // We run the event consumer on a totally separate tokio runtime to avoid the consumer task from getting stuck behind
    // another tokio task in its lifo task slot. See T96012305 and https://github.com/tokio-rs/tokio/issues/4323 for more
    // information.
    let _merge_task = std::thread::spawn(move || {
        while let Some(next_event) = events.receive() {
            // Note that writes to `output_send` have their errors explicitly ignored here. There is only one reason
            // for a write to a `mpsc::channel` to fail: the receiving end of the channel has already been closed.
            //
            // This function returns the receiving channel back to `tonic` as part of a streaming response. Tonic can
            // drop the stream before it is fully resolved if, for example, the gRPC client disconnects during the
            // command. In this case, we explicitly ignore write errors and let them float off into the void, since no
            // client is listening.
            //
            // TODO(swgillespie) - We should handle client disconnects better.
            match next_event {
                Event::Control(control_event) => {
                    // A control event. This event isn't going to be sent to gRPC, but we do need to react to it. In
                    // this case, the CommandResult event indicates that the spawned computation has produced a result
                    // and will not be producing any more events.
                    match control_event {
                        ControlEvent::CommandResult(result) => {
                            let _ignore = output_send.send(Ok(CommandProgress {
                                progress: Some(command_progress::Progress::Result(result)),
                            }));
                        }
                    }
                    return;
                }
                Event::Buck(buck_event) => {
                    // A buck event. These events should be forwarded directly to gRPC.
                    let _ignore = output_send.send(Ok(CommandProgress {
                        progress: Some(command_progress::Progress::Event(buck_event.into())),
                    }));
                }
            }
        }
    });

    // The stream we ultimately return is the receiving end of the channel that the above task is writing to.
    Response::new(Box::pin(SyncStream {
        wrapped: sync_wrapper::SyncWrapper::new(DropTogether::new(
            tokio_stream::wrappers::UnboundedReceiverStream::new(output_recv),
            cancellable,
        )),
    }))
}

type ResponseStream = Pin<Box<dyn Stream<Item = Result<CommandProgress, Status>> + Send + Sync>>;
#[async_trait]
impl DaemonApi for BuckdServer {
    async fn kill(&self, req: Request<KillRequest>) -> Result<Response<CommandResult>, Status> {
        struct KillRunCommandOptions;

        impl OneshotCommandOptions for KillRunCommandOptions {
            /// kill should be always available
            fn pre_run(&self, _server: &BuckdServer) -> Result<(), Status> {
                Ok(())
            }
        }

        self.oneshot(req, KillRunCommandOptions, move |req| async move {
            self.stop_accepting_requests.store(true, Ordering::Relaxed);

            let timeout = req
                .timeout
                .as_ref()
                .map(convert_positive_duration)
                .transpose()?;

            self.daemon_shutdown.start_shutdown(timeout);
            Ok(KillResponse {})
        })
        .await
    }

    async fn ping(&self, req: Request<PingRequest>) -> Result<Response<CommandResult>, Status> {
        self.oneshot(req, DefaultCommandOptions, move |req| async move {
            match &req.delay {
                Some(delay) => {
                    let delay = convert_positive_duration(delay)?;
                    tokio::time::sleep(delay).await;
                }
                _ => {}
            }

            Ok(PingResponse {})
        })
        .await
    }

    async fn status(&self, req: Request<StatusRequest>) -> Result<Response<CommandResult>, Status> {
        let daemon_state = self.daemon_state.dupe();

        self.oneshot(req, DefaultCommandOptions, move |req| async move {
            let snapshot = if req.snapshot {
                let data = daemon_state.data().await?;
                Some(
                    snapshot::SnapshotCollector::new(
                        data.re_client_manager.dupe(),
                        data.blocking_executor.dupe(),
                        data.start_time,
                    )
                    .create_snapshot(),
                )
            } else {
                None
            };

            let uptime = self.start_instant.elapsed();
            let mut base = StatusResponse {
                process_info: Some(self.process_info.clone()),
                start_time: Some(self.start_time.clone()),
                uptime: Some(uptime.try_into()?),
                snapshot,
                ..Default::default()
            };
            jemalloc_stats(&mut base);
            Ok(base)
        })
        .await
    }

    async fn flush_dep_files(
        &self,
        req: Request<FlushDepFilesRequest>,
    ) -> Result<Response<CommandResult>, Status> {
        self.oneshot(req, DefaultCommandOptions, move |req| async move {
            let FlushDepFilesRequest {} = req;
            buck2_build_api::actions::impls::run::dep_files::flush_dep_files();
            Ok(GenericResponse {})
        })
        .await
    }

    type BuildStream = ResponseStream;
    async fn build(&self, req: Request<BuildRequest>) -> Result<Response<ResponseStream>, Status> {
        let callbacks = self.callbacks;
        self.run_streaming(req, DefaultCommandOptions, |ctx, req| {
            callbacks.build(box ctx, req)
        })
        .await
    }

    type BxlStream = ResponseStream;
    async fn bxl(&self, req: Request<BxlRequest>) -> Result<Response<ResponseStream>, Status> {
        let callbacks = self.callbacks;
        self.run_streaming(req, DefaultCommandOptions, |ctx, req| {
            callbacks.bxl(box ctx, req)
        })
        .await
    }

    type TestStream = ResponseStream;
    async fn test(&self, req: Request<TestRequest>) -> Result<Response<ResponseStream>, Status> {
        let callbacks = self.callbacks;
        self.run_streaming(req, DefaultCommandOptions, |ctx, req| {
            callbacks.test(box ctx, req)
        })
        .await
    }

    type AqueryStream = ResponseStream;
    async fn aquery(
        &self,
        req: Request<AqueryRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        let callbacks = self.callbacks;
        self.run_streaming(req, DefaultCommandOptions, |ctx, req| {
            callbacks.aquery(box ctx, req)
        })
        .await
    }

    type UqueryStream = ResponseStream;
    async fn uquery(
        &self,
        req: Request<UqueryRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        let callbacks = self.callbacks;
        self.run_streaming(req, DefaultCommandOptions, |ctx, req| {
            callbacks.uquery(box ctx, req)
        })
        .await
    }

    type CqueryStream = ResponseStream;
    async fn cquery(
        &self,
        req: Request<CqueryRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        let callbacks = self.callbacks;
        self.run_streaming(req, DefaultCommandOptions, |ctx, req| {
            callbacks.cquery(box ctx, req)
        })
        .await
    }

    type TargetsStream = ResponseStream;
    async fn targets(
        &self,
        req: Request<TargetsRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        let callbacks = self.callbacks;
        self.run_streaming(req, DefaultCommandOptions, |ctx, req| {
            callbacks.targets(box ctx, req)
        })
        .await
    }

    type TargetsShowOutputsStream = ResponseStream;
    async fn targets_show_outputs(
        &self,
        req: Request<TargetsRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        let callbacks = self.callbacks;
        self.run_streaming(req, DefaultCommandOptions, |ctx, req| {
            callbacks.targets_show_outputs(box ctx, req)
        })
        .await
    }

    type AuditStream = ResponseStream;
    async fn audit(
        &self,
        req: Request<GenericRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        let callbacks = self.callbacks;
        self.run_streaming(req, DefaultCommandOptions, |ctx, req| {
            callbacks.audit(box ctx, req)
        })
        .await
    }

    type InstallStream = ResponseStream;
    async fn install(
        &self,
        req: Request<InstallRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        let callbacks = self.callbacks;
        self.run_streaming(req, DefaultCommandOptions, |ctx, req| {
            callbacks.install(box ctx, req)
        })
        .await
    }

    async fn unstable_crash(
        &self,
        _req: Request<UnstableCrashRequest>,
    ) -> Result<Response<UnstableCrashResponse>, Status> {
        panic!("explicitly requested panic (via unstable_crash)");
    }

    async fn segfault(
        &self,
        _req: Request<SegfaultRequest>,
    ) -> Result<Response<SegfaultResponse>, Status> {
        unsafe {
            std::ptr::null_mut::<&'static str>()
                .write("Explicitly requested segfault (via `segfault`)")
        };
        unreachable!()
    }

    async fn unstable_heap_dump(
        &self,
        req: Request<UnstableHeapDumpRequest>,
    ) -> Result<Response<UnstableHeapDumpResponse>, Status> {
        self.check_if_accepting_requests()?;

        let heap_dump = memory::write_heap_to_file(&req.into_inner().destination_path);
        match heap_dump {
            Ok(_) => Ok(Response::new(UnstableHeapDumpResponse {})),
            Err(e) => Err(Status::invalid_argument(format!(
                "failed to perform heap dump: {}",
                e
            ))),
        }
    }

    async fn unstable_allocator_stats(
        &self,
        req: Request<UnstableAllocatorStatsRequest>,
    ) -> Result<Response<UnstableAllocatorStatsResponse>, Status> {
        self.check_if_accepting_requests()?;

        let response = memory::allocator_stats(&req.into_inner().options)
            .context("Failed to retrieve allocator stats");

        match response {
            Ok(response) => Ok(Response::new(UnstableAllocatorStatsResponse { response })),
            Err(e) => Err(Status::invalid_argument(format!("{:#}", e))),
        }
    }

    async fn unstable_dice_dump(
        &self,
        req: Request<UnstableDiceDumpRequest>,
    ) -> Result<Response<UnstableDiceDumpResponse>, Status> {
        self.check_if_accepting_requests()?;

        let inner = req.into_inner();
        let path = inner.destination_path;
        let res: anyhow::Result<_> = try {
            let path = Path::new(&path);
            let format_proto =
                cli_proto::unstable_dice_dump_request::DiceDumpFormat::from_i32(inner.format)
                    .context("Invalid DICE dump format")?;

            self.daemon_state
                .data()
                .await?
                .spawn_dice_dump(path, format_proto)
                .await
                .with_context(|| format!("Failed to perform dice dump to {}", path.display()))?;

            UnstableDiceDumpResponse {}
        };

        res.map(Response::new)
            .map_err(|e| Status::internal(format!("{:#}", e)))
    }

    type UnstableDocsStream = ResponseStream;
    async fn unstable_docs(
        &self,
        req: Request<UnstableDocsRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        let callbacks = self.callbacks;
        self.run_streaming(req, DefaultCommandOptions, |ctx, req| {
            callbacks.docs(box ctx, req)
        })
        .await
    }

    type Profile2Stream = ResponseStream;
    async fn profile2(
        &self,
        req: Request<ProfileRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        struct ProfileCommandOptions;

        impl OneshotCommandOptions for ProfileCommandOptions {}

        impl StreamingCommandOptions<ProfileRequest> for ProfileCommandOptions {
            fn starlark_profiler_instrumentation_override(
                &self,
                req: &ProfileRequest,
            ) -> anyhow::Result<StarlarkProfilerConfiguration> {
                starlark_profiler_configuration_from_request(req)
            }
        }

        let callbacks = self.callbacks;
        self.run_streaming(req, ProfileCommandOptions, |ctx, req| {
            callbacks.profile(box ctx, req)
        })
        .await
    }

    type MaterializeStream = ResponseStream;
    async fn materialize(
        &self,
        req: Request<MaterializeRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        self.run_streaming(req, DefaultCommandOptions, |context, req| {
            materialize_command(context, req)
        })
        .await
    }

    type LspStream = ResponseStream;
    async fn lsp(
        &self,
        req: Request<tonic::Streaming<StreamingRequest>>,
    ) -> Result<Response<Self::LspStream>, Status> {
        self.run_bidirectional(
            req,
            DefaultCommandOptions,
            |ctx, _client_ctx, req: StreamingRequestHandler<LspRequest>| {
                run_lsp_server_command(box ctx, req)
            },
        )
        .await
    }
}

/// Options to configure the execution of a oneshot command (i.e. what happens in `oneshot()`).
trait OneshotCommandOptions: Send + Sync + 'static {
    fn pre_run(&self, server: &BuckdServer) -> Result<(), Status> {
        server.check_if_accepting_requests()
    }
}

/// Options to configure the execution of a streaming command (i.e. what happens in `run_streaming()`).
trait StreamingCommandOptions<Req>: OneshotCommandOptions {
    fn starlark_profiler_instrumentation_override(
        &self,
        _req: &Req,
    ) -> anyhow::Result<StarlarkProfilerConfiguration> {
        Ok(StarlarkProfilerConfiguration::None)
    }
}

/// No-op set of command options.
struct DefaultCommandOptions;

impl OneshotCommandOptions for DefaultCommandOptions {}
impl<Req> StreamingCommandOptions<Req> for DefaultCommandOptions {}
