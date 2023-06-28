/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::future;
use std::io;
use std::path::Path;
use std::pin::Pin;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::task::Context;
use std::task::Poll;
use std::thread;
use std::time::Duration;
use std::time::Instant;
use std::time::SystemTime;

use allocative::Allocative;
use anyhow::Context as _;
use async_trait::async_trait;
use buck2_build_api::configure_dice::configure_dice_for_buck;
use buck2_build_api::spawner::BuckSpawner;
use buck2_cli_proto::daemon_api_server::*;
use buck2_cli_proto::*;
use buck2_common::buckd_connection::BUCK_AUTH_TOKEN_HEADER;
use buck2_common::events::HasEvents;
use buck2_common::invocation_paths::InvocationPaths;
use buck2_common::io::trace::TracingIoProvider;
use buck2_common::io::IoProvider;
use buck2_common::legacy_configs::cells::DaemonStartupConfig;
use buck2_common::legacy_configs::LegacyBuckConfig;
use buck2_common::memory;
use buck2_core::env_helper::EnvHelper;
use buck2_core::error::reload_hard_error_config;
use buck2_core::error::reset_soft_error_counters;
use buck2_core::fs::paths::abs_path::AbsPathBuf;
use buck2_core::logging::LogConfigurationReloadHandle;
use buck2_events::dispatch::EventDispatcher;
use buck2_events::ControlEvent;
use buck2_events::Event;
use buck2_events::EventSource;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute_impl::materializers::sqlite::MaterializerStateIdentity;
use buck2_interpreter::dice::starlark_profiler::StarlarkProfilerConfiguration;
use buck2_profile::starlark_profiler_configuration_from_request;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::NoPartialResult;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::streaming_request_handler::StreamingRequestHandler;
use buck2_server_starlark_debug::run::run_dap_server_command;
use dice::DetectCycles;
use dice::Dice;
use dice::WhichDice;
use dupe::Dupe;
use futures::channel::mpsc;
use futures::channel::mpsc::UnboundedReceiver;
use futures::channel::mpsc::UnboundedSender;
use futures::future::BoxFuture;
use futures::stream;
use futures::Future;
use futures::FutureExt;
use futures::Stream;
use futures::StreamExt;
use futures::TryFutureExt;
use more_futures::cancellation::ExplicitCancellationContext;
use more_futures::drop::DropTogether;
use more_futures::spawn::spawn_cancellable;
use rand::RngCore;
use rand::SeedableRng;
use tokio::sync::oneshot;
use tonic::service::interceptor;
use tonic::service::Interceptor;
use tonic::transport::Server;
use tonic::Code;
use tonic::Request;
use tonic::Response;
use tonic::Status;

use crate::active_commands::ActiveCommand;
use crate::active_commands::ActiveCommandStateWriter;
use crate::clean_stale::clean_stale_command;
use crate::ctx::ServerCommandContext;
use crate::daemon::multi_event_stream::MultiEventStream;
use crate::daemon::server_allocative::spawn_allocative;
use crate::daemon::state::DaemonState;
use crate::file_status::file_status_command;
use crate::lsp::run_lsp_server_command;
use crate::materialize::materialize_command;
use crate::snapshot;
use crate::subscription::run_subscription_server_command;
use crate::trace_io::trace_io_command;

// TODO(cjhopman): Figure out a reasonable value for this.
static DEFAULT_KILL_TIMEOUT: Duration = Duration::from_millis(500);

static DEFAULT_INACTIVITY_TIMEOUT: Duration = Duration::from_secs(4 * 86400);

pub trait BuckdServerDelegate: Allocative + Send + Sync {
    fn force_shutdown_with_timeout(&self, reason: String, timeout: Duration);
}

#[derive(Allocative)]
struct DaemonShutdown {
    delegate: Box<dyn BuckdServerDelegate>,

    /// This channel is used to trigger a graceful shutdown of the grpc server. After
    /// an item is sent on this channel, the server will start rejecting new requests
    /// and once current requests are finished the server will shutdown.
    #[allocative(skip)]
    shutdown_channel: UnboundedSender<()>,
}

impl DaemonShutdown {
    /// Trigger a graceful server shutdown with a timeout. After the timeout expires, a hard shutdown
    /// will be triggered.
    ///
    /// As we might be processing a `kill()` (or other) request, we cannot wait for the server to actually
    /// shutdown (as it will wait for current requests to finish), so this returns immediately.
    fn start_shutdown(&self, reason: buck2_data::DaemonShutdown, timeout: Option<Duration>) {
        crate::active_commands::broadcast_shutdown(&reason);

        let timeout = timeout.unwrap_or(DEFAULT_KILL_TIMEOUT);

        // Ignore errors on shutdown_channel as that would mean we've already started shutdown;
        let _ = self.shutdown_channel.unbounded_send(());
        self.delegate
            .force_shutdown_with_timeout(reason.to_string(), timeout);
    }
}

#[derive(Allocative)]
pub struct BuckdServerInitPreferences {
    pub detect_cycles: Option<DetectCycles>,
    pub which_dice: Option<WhichDice>,
    pub enable_trace_io: bool,
    pub reject_materializer_state: Option<MaterializerStateIdentity>,
    pub daemon_startup_config: DaemonStartupConfig,
}

impl BuckdServerInitPreferences {
    pub async fn construct_dice(
        &self,
        io: Arc<dyn IoProvider>,
        digest_config: DigestConfig,
        root_config: &LegacyBuckConfig,
    ) -> anyhow::Result<Arc<Dice>> {
        configure_dice_for_buck(
            io,
            digest_config,
            Some(root_config),
            self.detect_cycles,
            self.which_dice,
        )
        .await
    }
}

/// Access to functions which live outside of `buck2_server` crate.
#[async_trait]
pub trait BuckdServerDependencies: Send + Sync + 'static {
    async fn test(
        &self,
        ctx: &dyn ServerCommandContextTrait,
        partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
        req: TestRequest,
    ) -> anyhow::Result<TestResponse>;
    async fn build(
        &self,
        ctx: &dyn ServerCommandContextTrait,
        partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
        req: BuildRequest,
    ) -> anyhow::Result<BuildResponse>;
    async fn install(
        &self,
        ctx: &dyn ServerCommandContextTrait,
        partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
        req: InstallRequest,
    ) -> anyhow::Result<InstallResponse>;
    async fn bxl(
        &self,
        ctx: &dyn ServerCommandContextTrait,
        partial_result_dispatcher: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        req: buck2_cli_proto::BxlRequest,
    ) -> anyhow::Result<BxlResponse>;
    async fn audit(
        &self,
        ctx: &dyn ServerCommandContextTrait,
        partial_result_dispatcher: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        req: buck2_cli_proto::GenericRequest,
    ) -> anyhow::Result<buck2_cli_proto::GenericResponse>;
    async fn starlark(
        &self,
        ctx: &dyn ServerCommandContextTrait,
        partial_result_dispatcher: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        req: buck2_cli_proto::GenericRequest,
    ) -> anyhow::Result<buck2_cli_proto::GenericResponse>;
    async fn profile(
        &self,
        ctx: &dyn ServerCommandContextTrait,
        partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
        req: buck2_cli_proto::ProfileRequest,
    ) -> anyhow::Result<buck2_cli_proto::ProfileResponse>;
    async fn uquery(
        &self,
        ctx: &dyn ServerCommandContextTrait,
        partial_result_dispatcher: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        req: buck2_cli_proto::UqueryRequest,
    ) -> anyhow::Result<buck2_cli_proto::UqueryResponse>;
    async fn cquery(
        &self,
        ctx: &dyn ServerCommandContextTrait,
        partial_result_dispatcher: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        req: buck2_cli_proto::CqueryRequest,
    ) -> anyhow::Result<CqueryResponse>;
    async fn aquery(
        &self,
        ctx: &dyn ServerCommandContextTrait,
        partial_result_dispatcher: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        req: buck2_cli_proto::AqueryRequest,
    ) -> anyhow::Result<buck2_cli_proto::AqueryResponse>;
    async fn targets(
        &self,
        ctx: &dyn ServerCommandContextTrait,
        partial_result_dispatcher: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        req: TargetsRequest,
    ) -> anyhow::Result<TargetsResponse>;
    async fn ctargets(
        &self,
        ctx: &dyn ServerCommandContextTrait,
        partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
        req: ConfiguredTargetsRequest,
    ) -> anyhow::Result<ConfiguredTargetsResponse>;
    async fn targets_show_outputs(
        &self,
        ctx: &dyn ServerCommandContextTrait,
        partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
        req: TargetsRequest,
    ) -> anyhow::Result<TargetsShowOutputsResponse>;
    async fn docs(
        &self,
        ctx: &dyn ServerCommandContextTrait,
        partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
        req: buck2_cli_proto::UnstableDocsRequest,
    ) -> anyhow::Result<buck2_cli_proto::UnstableDocsResponse>;
}

#[derive(Clone)]
struct BuckCheckAuthTokenInterceptor {
    auth_token: String,
}

impl Interceptor for BuckCheckAuthTokenInterceptor {
    fn call(&mut self, request: Request<()>) -> Result<Request<()>, Status> {
        let token = match request.metadata().get(BUCK_AUTH_TOKEN_HEADER) {
            Some(token) => token,
            None => return Err(Status::unauthenticated("missing auth token")),
        };
        if !constant_time_eq::constant_time_eq(
            token.as_bytes(),
            self.auth_token.as_str().as_bytes(),
        ) {
            return Err(Status::unauthenticated("invalid auth token"));
        }

        static FAIL_AUTH: EnvHelper<bool> = EnvHelper::new("BUCK2_TEST_FAIL_BUCKD_AUTH");
        if FAIL_AUTH
            .get_copied()
            .unwrap_or_default()
            .unwrap_or_default()
        {
            return Err(Status::unauthenticated("injected auth error"));
        }

        Ok(request)
    }
}

#[derive(Allocative)]
pub(crate) struct BuckdServerData {
    /// The flag that is set to true when server is shutting down.
    stop_accepting_requests: AtomicBool,
    #[allocative(skip)]
    process_info: DaemonProcessInfo,
    base_daemon_constraints: buck2_cli_proto::DaemonConstraints,
    start_time: prost_types::Timestamp,
    start_instant: Instant,
    daemon_shutdown: DaemonShutdown,
    daemon_state: Arc<DaemonState>,
    #[allocative(skip)]
    command_channel: UnboundedSender<()>,
    #[allocative(skip)]
    callbacks: &'static dyn BuckdServerDependencies,
    #[allocative(skip)]
    log_reload_handle: Arc<dyn LogConfigurationReloadHandle>,
}

/// The BuckdServer implements the DaemonApi.
///
/// Simple endpoints are implemented here and complex things will be implemented in a sibling
/// module taking just a ServerCommandContext.
#[derive(Allocative)]
pub struct BuckdServer(Arc<BuckdServerData>);

impl BuckdServer {
    pub async fn run<I>(
        fb: fbinit::FacebookInit,
        log_reload_handle: Arc<dyn LogConfigurationReloadHandle>,
        paths: InvocationPaths,
        delegate: Box<dyn BuckdServerDelegate>,
        init_ctx: BuckdServerInitPreferences,
        process_info: DaemonProcessInfo,
        base_daemon_constraints: buck2_cli_proto::DaemonConstraints,
        listener: I,
        callbacks: &'static dyn BuckdServerDependencies,
    ) -> anyhow::Result<()>
    where
        I: Stream<Item = Result<tokio::net::TcpStream, io::Error>>,
    {
        let now = SystemTime::now();
        let now = now.duration_since(SystemTime::UNIX_EPOCH)?;

        let (shutdown_channel, shutdown_receiver): (UnboundedSender<()>, _) = mpsc::unbounded();
        let (command_channel, command_receiver): (UnboundedSender<()>, _) = mpsc::unbounded();

        let daemon_state = Arc::new(DaemonState::new(fb, paths, init_ctx).await);

        let auth_token = process_info.auth_token.clone();
        let api_server = BuckdServer(Arc::new(BuckdServerData {
            stop_accepting_requests: AtomicBool::new(false),
            process_info,
            base_daemon_constraints,
            start_time: prost_types::Timestamp {
                seconds: now.as_secs() as i64,
                nanos: now.subsec_nanos() as i32,
            },
            start_instant: Instant::now(),
            daemon_shutdown: DaemonShutdown {
                delegate,
                shutdown_channel,
            },
            daemon_state,
            command_channel,
            callbacks,
            log_reload_handle,
        }));

        let shutdown = server_shutdown_signal(command_receiver, shutdown_receiver)?;
        let server = Server::builder()
            .layer(interceptor(BuckCheckAuthTokenInterceptor { auth_token }))
            .add_service(
                DaemonApiServer::new(api_server)
                    .max_encoding_message_size(usize::MAX)
                    .max_decoding_message_size(usize::MAX),
            )
            .serve_with_incoming_shutdown(listener, shutdown);

        server.await?;

        Ok(())
    }

    /// Run a request that does bidirectional streaming.
    ///
    /// This mostly just ensures that a client context has been sent first, and passes a client
    /// stream to `func` that converts to the correct type (or returns an error and shuts the
    /// stream down)
    async fn run_bidirectional<Req, Res, PartialRes, F>(
        &self,
        req: Request<tonic::Streaming<StreamingRequest>>,
        opts: impl StreamingCommandOptions<StreamingRequest>,
        func: F,
    ) -> Result<Response<ResponseStream>, Status>
    where
        F: for<'a> FnOnce(
                &'a ServerCommandContext,
                PartialResultDispatcher<PartialRes>,
                &ClientContext,
                StreamingRequestHandler<Req>,
            ) -> BoxFuture<'a, anyhow::Result<Res>>
            + Send
            + 'static,
        Req: TryFrom<StreamingRequest, Error = anyhow::Error> + Send + Sync + 'static,
        Res: Into<command_result::Result> + Send + 'static,
        PartialRes: Into<partial_result::PartialResult> + Send + 'static,
    {
        let mut req = req.into_inner();
        let init_request = match req.message().await? {
            Some(
                m @ StreamingRequest {
                    request: Some(buck2_cli_proto::streaming_request::Request::Context(_)),
                },
            ) => Ok(m),
            _ => Err(Status::failed_precondition(
                "no client context message was received",
            )),
        }?;

        let init_request = Request::new(init_request);
        self.run_streaming(
            init_request,
            opts,
            |ctx, partial_result_dispatcher, init_req| {
                // TODO: Use the PartialResultDispatcher instead of writing events.
                func(
                    ctx,
                    partial_result_dispatcher,
                    init_req
                        .client_context()
                        .expect("already checked for a valid context"),
                    StreamingRequestHandler::new(req),
                )
            },
        )
        .await
    }

    async fn run_streaming_anyhow<Req, Res, PartialRes, F>(
        &self,
        req: Request<Req>,
        opts: impl StreamingCommandOptions<Req>,
        func: F,
    ) -> anyhow::Result<Response<ResponseStream>>
    where
        F: for<'a> FnOnce(
                &'a ServerCommandContext,
                PartialResultDispatcher<PartialRes>,
                Req,
            ) -> BoxFuture<'a, anyhow::Result<Res>>
            + Send
            + 'static,
        Req: HasClientContext + HasBuildOptions + Send + Sync + 'static,
        Res: Into<command_result::Result> + Send + 'static,
        PartialRes: Into<partial_result::PartialResult> + Send + 'static,
    {
        let client_ctx = req.get_ref().client_context()?;

        // This will reset counters incorrectly if commands are running concurrently.
        // This is fine.
        reset_soft_error_counters();

        reload_hard_error_config(&client_ctx.buck2_hard_error)?;

        OneshotCommandOptions::pre_run(&opts, self)?;

        let daemon_state = self.0.daemon_state.dupe();
        let trace_id = client_ctx.trace_id.parse()?;
        let (events, dispatch) = daemon_state.prepare_events(trace_id).await?;
        let ActiveCommand {
            guard,
            daemon_shutdown_channel,
            state,
        } = ActiveCommand::new(&dispatch, client_ctx);
        let data = daemon_state.data()?;

        dispatch.instant_event(Box::new(
            snapshot::SnapshotCollector::pre_initialization_snapshot(data.start_time),
        ));

        let resp = streaming(
            req,
            events,
            state,
            dispatch.dupe(),
            daemon_shutdown_channel,
            move |req, cancellations| {
                async move {
                    let result: anyhow::Result<Res> = try {
                        let base_context =
                            daemon_state.prepare_command(dispatch.dupe(), guard).await?;

                        let context = ServerCommandContext::new(
                            base_context,
                            req.client_context()?,
                            opts.starlark_profiler_instrumentation_override(&req)?,
                            req.build_options(),
                            daemon_state.paths.buck_out_dir(),
                            cancellations,
                        )?;

                        func(&context, PartialResultDispatcher::new(dispatch.dupe()), req).await?
                    };

                    let result: CommandResult = result_to_command_result(result);
                    dispatch.control_event(ControlEvent::CommandResult(Box::new(result)));
                }
                .boxed()
            },
        )
        .await;
        Ok(resp)
    }

    /// Runs a single command (given by the func F). Prior to running the command, calls the
    /// `opts`'s `pre_run` hook.  then bootstraps an event source and command context so that the
    /// invoked function has the ability to stream events to the caller.
    async fn run_streaming<Req, Res, PartialRes, F>(
        &self,
        req: Request<Req>,
        opts: impl StreamingCommandOptions<Req>,
        func: F,
    ) -> Result<Response<ResponseStream>, Status>
    where
        F: for<'a> FnOnce(
                &'a ServerCommandContext,
                PartialResultDispatcher<PartialRes>,
                Req,
            ) -> BoxFuture<'a, anyhow::Result<Res>>
            + Send
            + 'static,
        Req: HasClientContext + HasBuildOptions + Send + Sync + 'static,
        Res: Into<command_result::Result> + Send + 'static,
        PartialRes: Into<partial_result::PartialResult> + Send + 'static,
    {
        // send signal to register new command time
        _ = self.0.command_channel.unbounded_send(());

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
        let result = func(req).await;
        Ok(Response::new(result_to_command_result(result)))
    }

    /// Checks if the server is accepting requests.
    fn check_if_accepting_requests(&self) -> Result<(), Status> {
        if self.0.stop_accepting_requests.load(Ordering::Relaxed) {
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

fn result_to_command_result<R: Into<command_result::Result>>(
    result: anyhow::Result<R>,
) -> CommandResult {
    match result {
        Ok(result) => CommandResult {
            result: Some(result.into()),
        },
        Err(e) => error_to_command_result(e),
    }
}

fn error_to_command_progress(e: anyhow::Error) -> CommandProgress {
    CommandProgress {
        progress: Some(command_progress::Progress::Result(Box::new(
            error_to_command_result(e),
        ))),
    }
}

fn error_to_response_stream(e: anyhow::Error) -> Response<ResponseStream> {
    tonic::Response::new(Box::pin(stream::once(future::ready(Ok(
        buck2_cli_proto::MultiCommandProgress {
            messages: vec![error_to_command_progress(e)],
        },
    )))))
}

/// tonic requires the response for a streaming api to be a Sync Stream. With async/await, that requirement is really difficult
/// to meet. This simple wrapper allows us to wrap a non-Sync stream into a Sync one (the inner stream is never accessed in a
/// non-exclusive manner).
struct SyncStream<T: Stream + Send> {
    // SyncWrapper provides a Sync type that only allows (statically checked) exclusive access to
    // the underlying object, this allows using a non-Sync object where a Sync one is required
    // but is never accessed from multiple threads.
    // See https://internals.rust-lang.org/t/what-shall-sync-mean-across-an-await/12020/31
    // and https://github.com/hyperium/tonic/issues/117
    wrapped: sync_wrapper::SyncWrapper<T>,
}

impl<T: Stream + Send> Stream for SyncStream<T> {
    type Item = <T as Stream>::Item;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        // This is a safe pin projection. See https://doc.rust-lang.org/std/pin/index.html#projections-and-structural-pinning
        // Specifically see the requirements when pinning is structural for a field here: https://doc.rust-lang.org/std/pin/index.html#pinning-is-structural-for-field
        unsafe { self.map_unchecked_mut(|a| a.wrapped.get_mut()) }.poll_next(cx)
    }
}

fn pump_events<E: EventSource>(
    mut events: E,
    mut state: ActiveCommandStateWriter,
    output_send: tokio::sync::mpsc::UnboundedSender<
        Result<buck2_cli_proto::CommandProgress, tonic::Status>,
    >,
) {
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
                        return;
                    }
                    ControlEvent::PartialResult(result) => {
                        let _ignore = output_send.send(Ok(CommandProgress {
                            progress: Some(command_progress::Progress::PartialResult(Box::new(
                                result,
                            ))),
                        }));
                    }
                }
            }
            Event::Buck(buck_event) => {
                state.peek_event(&buck_event);

                // A buck event. These events should be forwarded directly to gRPC.
                let _ignore = output_send.send(Ok(CommandProgress {
                    progress: Some(command_progress::Progress::Event(buck_event.into())),
                }));
            }
        }
    }
}

/// Dispatches a request to the given function and returns a stream of responses, suitable for streaming to a client.
#[allow(clippy::mut_mut)] // select! does this internally
async fn streaming<
    Req: Send + Sync + 'static,
    F: for<'a> FnOnce(Req, &'a ExplicitCancellationContext) -> BoxFuture<'a, ()>,
    E: EventSource + 'static,
>(
    req: Request<Req>,
    events: E,
    state: ActiveCommandStateWriter,
    dispatcher: EventDispatcher,
    daemon_shutdown_channel: oneshot::Receiver<buck2_data::DaemonShutdown>,
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

    let trace_id = dispatcher.trace_id().dupe();

    let req = req.into_inner();
    let events_ctx = EventsCtx { dispatcher };
    let spawned = spawn_cancellable(
        |cancellations| func(req, cancellations),
        &BuckSpawner,
        &events_ctx,
    );
    let (output_send, output_recv) = tokio::sync::mpsc::unbounded_channel();

    // We run the event consumer on a totally separate tokio runtime to avoid the consumer task from getting stuck behind
    // another tokio task in its lifo task slot. See T96012305 and https://github.com/tokio-rs/tokio/issues/4323 for more
    // information.
    let merge_task = thread::Builder::new()
        .name("pump-events".to_owned())
        .spawn(move || {
            pump_events(events, state, output_send);
        });
    let _merge_task = match merge_task {
        Ok(merge_task) => merge_task,
        Err(e) => {
            return error_to_response_stream(
                anyhow::Error::new(e).context("failed to spawn pump-events"),
            );
        }
    };

    //
    // Note that while this is an event, we don't send it through our normal event
    // processing. The reason for that is that we dont want this event to queue behind any other
    // events in the (2) unbounded channels that form our event pipeline. So, we inject this one
    // directly where Tonic is polling for responses (which, unlike the rest of the pipeline, is
    // not unbounded, and has backpressure).

    let daemon_shutdown_stream = daemon_shutdown_channel
        .map_ok(move |shutdown| CommandProgress {
            progress: Some(command_progress::Progress::Event(Box::new(
                buck2_data::BuckEvent {
                    timestamp: Some(SystemTime::now().into()),
                    trace_id: trace_id.to_string(),
                    span_id: 0,
                    parent_id: 0,
                    data: Some(
                        buck2_data::InstantEvent {
                            data: Some(shutdown.into()),
                        }
                        .into(),
                    ),
                },
            ))),
        })
        .into_stream()
        .filter_map(|e| {
            // If the channel yields an Err, that means we didnt shut down, so for us that is
            // simply something we want to drop from the stream.
            futures::future::ready(e.ok().map(Ok))
        });

    // The stream we ultimately return is the receiving end of the channel that the above task is
    // writing to, plus the shutdown channel.
    let events = futures::stream::select(
        tokio_stream::wrappers::UnboundedReceiverStream::new(output_recv),
        daemon_shutdown_stream,
    );

    let events = MultiEventStream::new(events);

    Response::new(Box::pin(SyncStream {
        wrapped: sync_wrapper::SyncWrapper::new(DropTogether::new(
            events,
            spawned.into_drop_cancel(),
        )),
    }))
}

type ResponseStream =
    Pin<Box<dyn Stream<Item = Result<MultiCommandProgress, Status>> + Send + Sync>>;
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
            self.0
                .stop_accepting_requests
                .store(true, Ordering::Relaxed);

            let timeout = req
                .timeout
                .as_ref()
                .map(convert_positive_duration)
                .transpose()?;

            let reason = buck2_data::DaemonShutdown {
                reason: req.reason,
                callers: req.callers,
            };

            self.0.daemon_shutdown.start_shutdown(reason, timeout);
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

            let mut payload = vec![
                0;
                req.response_payload_size
                    .try_into()
                    .context("requested payload too large")?
            ];
            rand::rngs::SmallRng::seed_from_u64(10).fill_bytes(&mut payload);

            Ok(PingResponse { payload })
        })
        .await
    }

    async fn status(&self, req: Request<StatusRequest>) -> Result<Response<CommandResult>, Status> {
        let daemon_state = self.0.daemon_state.dupe();

        self.oneshot(req, DefaultCommandOptions, move |req| async move {
            let snapshot = if req.snapshot {
                let data = daemon_state.data()?;
                Some(
                    snapshot::SnapshotCollector::new(
                        data.re_client_manager.dupe(),
                        data.blocking_executor.dupe(),
                        data.start_time,
                        data.dice_manager.unsafe_dice().dupe(),
                        data.materializer.dupe(),
                        data.scribe_sink.dupe() as _,
                        data.http_client.dupe(),
                    )
                    .create_snapshot(),
                )
            } else {
                None
            };

            let extra_constraints = daemon_state.data().as_ref().ok().map(|state| {
                buck2_cli_proto::ExtraDaemonConstraints {
                    trace_io_enabled: state.io.as_any().is::<TracingIoProvider>(),
                    materializer_state_identity: state
                        .materializer_state_identity
                        .as_ref()
                        .map(|i| i.to_string()),
                }
            });

            let mut daemon_constraints = self.0.base_daemon_constraints.clone();
            daemon_constraints.extra = extra_constraints;

            let uptime = self.0.start_instant.elapsed();
            let base = StatusResponse {
                process_info: Some(self.0.process_info.clone()),
                start_time: Some(self.0.start_time.clone()),
                uptime: Some(uptime.try_into()?),
                snapshot,
                daemon_constraints: Some(daemon_constraints),
                project_root: daemon_state.paths.project_root().to_string(),
                isolation_dir: daemon_state.paths.isolation.to_string(),
                forkserver_pid: daemon_state
                    .data
                    .as_ref()
                    .ok()
                    .and_then(|state| state.forkserver.as_ref().map(|f| f.pid())),
                supports_vpnless: daemon_state
                    .data()
                    .as_ref()
                    .ok()
                    .map(|state| state.http_client.supports_vpnless()),
                ..Default::default()
            };
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
            buck2_file_watcher::dep_files::flush_dep_files();
            Ok(GenericResponse {})
        })
        .await
    }

    type FileStatusStream = ResponseStream;
    async fn file_status(
        &self,
        req: Request<FileStatusRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        self.run_streaming(
            req,
            DefaultCommandOptions,
            |context, partial_result_dispatcher, req| {
                file_status_command(context, partial_result_dispatcher, req).boxed()
            },
        )
        .await
    }

    type BuildStream = ResponseStream;
    async fn build(&self, req: Request<BuildRequest>) -> Result<Response<ResponseStream>, Status> {
        let callbacks = self.0.callbacks;
        self.run_streaming(
            req,
            DefaultCommandOptions,
            |ctx, partial_result_dispatcher, req| {
                callbacks.build(ctx, partial_result_dispatcher, req)
            },
        )
        .await
    }

    type BxlStream = ResponseStream;
    async fn bxl(&self, req: Request<BxlRequest>) -> Result<Response<ResponseStream>, Status> {
        let callbacks = self.0.callbacks;
        self.run_streaming(
            req,
            DefaultCommandOptions,
            |ctx, partial_result_dispatcher, req| {
                callbacks.bxl(ctx, partial_result_dispatcher, req)
            },
        )
        .await
    }

    type TestStream = ResponseStream;
    async fn test(&self, req: Request<TestRequest>) -> Result<Response<ResponseStream>, Status> {
        let callbacks = self.0.callbacks;
        self.run_streaming(
            req,
            DefaultCommandOptions,
            |ctx, partial_result_dispatcher, req| {
                callbacks.test(ctx, partial_result_dispatcher, req)
            },
        )
        .await
    }

    type AqueryStream = ResponseStream;
    async fn aquery(
        &self,
        req: Request<AqueryRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        let callbacks = self.0.callbacks;
        self.run_streaming(
            req,
            DefaultCommandOptions,
            |ctx, partial_result_dispatcher, req| {
                callbacks.aquery(ctx, partial_result_dispatcher, req)
            },
        )
        .await
    }

    type UqueryStream = ResponseStream;
    async fn uquery(
        &self,
        req: Request<UqueryRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        let callbacks = self.0.callbacks;
        self.run_streaming(
            req,
            DefaultCommandOptions,
            |ctx, partial_result_dispatcher, req| {
                callbacks.uquery(ctx, partial_result_dispatcher, req)
            },
        )
        .await
    }

    type CqueryStream = ResponseStream;
    async fn cquery(
        &self,
        req: Request<CqueryRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        let callbacks = self.0.callbacks;
        self.run_streaming(
            req,
            DefaultCommandOptions,
            |ctx, partial_result_dispatcher, req| {
                callbacks.cquery(ctx, partial_result_dispatcher, req)
            },
        )
        .await
    }

    type TargetsStream = ResponseStream;
    async fn targets(
        &self,
        req: Request<TargetsRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        let callbacks = self.0.callbacks;
        self.run_streaming(
            req,
            DefaultCommandOptions,
            |ctx, partial_result_dispatcher, req| {
                callbacks.targets(ctx, partial_result_dispatcher, req)
            },
        )
        .await
    }

    type CtargetsStream = ResponseStream;
    async fn ctargets(
        &self,
        req: Request<ConfiguredTargetsRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        let callbacks = self.0.callbacks;
        self.run_streaming(
            req,
            DefaultCommandOptions,
            |ctx, partial_result_dispatcher, req| {
                callbacks.ctargets(ctx, partial_result_dispatcher, req)
            },
        )
        .await
    }

    type TargetsShowOutputsStream = ResponseStream;
    async fn targets_show_outputs(
        &self,
        req: Request<TargetsRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        let callbacks = self.0.callbacks;
        self.run_streaming(
            req,
            DefaultCommandOptions,
            |ctx, partial_result_dispatcher, req| {
                callbacks.targets_show_outputs(ctx, partial_result_dispatcher, req)
            },
        )
        .await
    }

    type AuditStream = ResponseStream;
    async fn audit(
        &self,
        req: Request<GenericRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        let callbacks = self.0.callbacks;
        self.run_streaming(
            req,
            DefaultCommandOptions,
            |ctx, partial_result_dispatcher, req| {
                callbacks.audit(ctx, partial_result_dispatcher, req)
            },
        )
        .await
    }

    type StarlarkStream = ResponseStream;
    async fn starlark(
        &self,
        req: Request<GenericRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        let callbacks = self.0.callbacks;
        self.run_streaming(
            req,
            DefaultCommandOptions,
            |ctx, partial_result_dispatcher, req| {
                callbacks.starlark(ctx, partial_result_dispatcher, req)
            },
        )
        .await
    }

    type InstallStream = ResponseStream;
    async fn install(
        &self,
        req: Request<InstallRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        let callbacks = self.0.callbacks;
        self.run_streaming(
            req,
            DefaultCommandOptions,
            |ctx, partial_result_dispatcher, req| {
                callbacks.install(ctx, partial_result_dispatcher, req)
            },
        )
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
                buck2_cli_proto::unstable_dice_dump_request::DiceDumpFormat::from_i32(inner.format)
                    .context("Invalid DICE dump format")?;

            self.0
                .daemon_state
                .data()?
                .spawn_dice_dump(path, format_proto)
                .await
                .with_context(|| format!("Failed to perform dice dump to {}", path.display()))?;

            UnstableDiceDumpResponse {}
        };

        res.map(Response::new)
            .map_err(|e| Status::internal(format!("{:#}", e)))
    }

    type AllocativeStream = ResponseStream;
    async fn allocative(
        &self,
        req: Request<AllocativeRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        self.check_if_accepting_requests()?;

        let res: anyhow::Result<_> = try {
            let client_ctx = req.get_ref().client_context()?;
            let trace_id = client_ctx.trace_id.parse()?;
            let (event_source, dispatcher) = self.0.daemon_state.prepare_events(trace_id).await?;
            let active_command = ActiveCommand::new(&dispatcher, client_ctx);
            (event_source, dispatcher, active_command)
        };

        let (event_source, dispatcher, active_command) = match res {
            Ok(v) => v,
            Err(e) => return Ok(error_to_response_stream(e)),
        };

        let ActiveCommand {
            guard,
            daemon_shutdown_channel,
            state,
        } = active_command;

        let this = self.0.dupe();
        Ok(streaming(
            req,
            event_source,
            state,
            dispatcher.dupe(),
            daemon_shutdown_channel,
            move |req, _| {
                async move {
                    let result = try {
                        spawn_allocative(
                            this,
                            AbsPathBuf::try_from(req.output_path)?,
                            dispatcher.dupe(),
                        )
                        .await?;
                        AllocativeResponse {}
                    };

                    let result: CommandResult = result_to_command_result(result);
                    dispatcher.control_event(ControlEvent::CommandResult(Box::new(result)));

                    drop(guard);
                }
                .boxed()
            },
        )
        .await)
    }

    type UnstableDocsStream = ResponseStream;
    async fn unstable_docs(
        &self,
        req: Request<UnstableDocsRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        let callbacks = self.0.callbacks;
        self.run_streaming(
            req,
            DefaultCommandOptions,
            |ctx, partial_result_dispatcher, req| {
                callbacks.docs(ctx, partial_result_dispatcher, req)
            },
        )
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

        let callbacks = self.0.callbacks;
        self.run_streaming(
            req,
            ProfileCommandOptions,
            |ctx, partial_result_dispatcher, req| {
                callbacks.profile(ctx, partial_result_dispatcher, req)
            },
        )
        .await
    }

    type MaterializeStream = ResponseStream;
    async fn materialize(
        &self,
        req: Request<MaterializeRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        self.run_streaming(
            req,
            DefaultCommandOptions,
            |context, _: PartialResultDispatcher<NoPartialResult>, req| {
                materialize_command(context, req).boxed()
            },
        )
        .await
    }

    type CleanStaleStream = ResponseStream;
    async fn clean_stale(
        &self,
        req: Request<CleanStaleRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        self.run_streaming(
            req,
            DefaultCommandOptions,
            |context, partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>, req| {
                clean_stale_command(context, partial_result_dispatcher, req).boxed()
            },
        )
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
            |ctx,
             partial_result_dispatcher,
             _client_ctx,
             req: StreamingRequestHandler<LspRequest>| {
                run_lsp_server_command(ctx, partial_result_dispatcher, req).boxed()
            },
        )
        .await
    }

    type SubscriptionStream = ResponseStream;
    async fn subscription(
        &self,
        req: Request<tonic::Streaming<StreamingRequest>>,
    ) -> Result<Response<Self::SubscriptionStream>, Status> {
        self.run_bidirectional(
            req,
            DefaultCommandOptions,
            |ctx,
             partial_result_dispatcher,
             _client_ctx,
             req: StreamingRequestHandler<SubscriptionRequestWrapper>| {
                run_subscription_server_command(ctx, partial_result_dispatcher, req).boxed()
            },
        )
        .await
    }

    type DapStream = ResponseStream;
    async fn dap(
        &self,
        req: Request<tonic::Streaming<StreamingRequest>>,
    ) -> Result<Response<Self::DapStream>, Status> {
        self.run_bidirectional(
            req,
            DefaultCommandOptions,
            |ctx,
             partial_result_dispatcher,
             _client_ctx,
             req: StreamingRequestHandler<DapRequest>| {
                run_dap_server_command(ctx, partial_result_dispatcher, req).boxed()
            },
        )
        .await
    }

    async fn set_log_filter(
        &self,
        req: Request<SetLogFilterRequest>,
    ) -> Result<Response<SetLogFilterResponse>, Status> {
        let req = req.into_inner();

        if req.daemon {
            self.0
                .log_reload_handle
                .update_log_filter(&req.log_filter)
                .context("Error updating daemon log filter")
                .map_err(|e| Status::invalid_argument(format!("{:#}", e)))?;
        }

        if req.forkserver {
            if let Ok(data) = self.0.daemon_state.data() {
                if let Some(forkserver) = data.forkserver.as_ref() {
                    forkserver
                        .set_log_filter(req.log_filter)
                        .await
                        .context("Error forwarding daemon log filter to forkserver")
                        .map_err(|e| Status::invalid_argument(format!("{:#}", e)))?;
                }
            }
        }

        Ok(Response::new(SetLogFilterResponse {}))
    }

    type TraceIoStream = ResponseStream;
    async fn trace_io(
        &self,
        req: Request<TraceIoRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        self.run_streaming(
            req,
            DefaultCommandOptions,
            |context, _: PartialResultDispatcher<NoPartialResult>, req| {
                trace_io_command(context, req).boxed()
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

fn server_shutdown_signal(
    command_receiver: UnboundedReceiver<()>,
    mut shutdown_receiver: UnboundedReceiver<()>,
) -> anyhow::Result<impl Future<Output = ()>> {
    static TESTING_INACTIVITY_TIMEOUT: EnvHelper<bool> =
        EnvHelper::new("BUCK2_TESTING_INACTIVITY_TIMEOUT");

    let mut duration = DEFAULT_INACTIVITY_TIMEOUT;
    if *TESTING_INACTIVITY_TIMEOUT.get()?.unwrap_or(&false) {
        duration = Duration::from_secs(1);
    }

    Ok(async move {
        let timeout = inactivity_timeout(command_receiver, duration);
        let shutdown = shutdown_receiver.next();

        futures::pin_mut!(shutdown);
        futures::pin_mut!(timeout);

        futures::future::select(timeout, shutdown).await;
    })
}

async fn inactivity_timeout(mut command_receiver: UnboundedReceiver<()>, duration: Duration) {
    // this restarts the timer everytime there is a new command
    loop {
        let command = command_receiver.next();
        let timer = tokio::time::sleep(duration);

        futures::pin_mut!(command);
        futures::pin_mut!(timer);

        match futures::future::select(command, timer).await {
            futures::future::Either::Left(_) => continue,
            futures::future::Either::Right(_) => break,
        };
    }
}

/// No-op set of command options.
struct DefaultCommandOptions;

impl OneshotCommandOptions for DefaultCommandOptions {}
impl<Req> StreamingCommandOptions<Req> for DefaultCommandOptions {}
