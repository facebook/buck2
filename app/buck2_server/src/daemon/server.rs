/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::future;
use std::io;
use std::path::Path;
use std::pin::Pin;
use std::sync::Arc;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::task::Context;
use std::task::Poll;
use std::time::Duration;
use std::time::Instant;
use std::time::SystemTime;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_build_api::configure_dice::configure_dice_for_buck;
use buck2_build_api::spawner::BuckSpawner;
use buck2_certs::validate::CertState;
use buck2_certs::validate::check_cert_state;
use buck2_certs::validate::validate_certs;
use buck2_cli_proto::daemon_api_server::*;
use buck2_cli_proto::*;
use buck2_common::buckd_connection::BUCK_AUTH_TOKEN_HEADER;
use buck2_common::events::HasEvents;
use buck2_common::init::DaemonStartupConfig;
use buck2_common::invocation_paths::InvocationPaths;
use buck2_common::io::IoProvider;
use buck2_common::io::trace::TracingIoProvider;
use buck2_common::legacy_configs::configs::LegacyBuckConfig;
use buck2_common::memory;
use buck2_common::sqlite::sqlite_db::SqliteIdentity;
use buck2_core::buck2_env;
use buck2_core::error::reload_hard_error_config;
use buck2_core::error::reset_soft_error_counters;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::logging::LogConfigurationReloadHandle;
use buck2_core::pattern::unparsed::UnparsedPatternPredicate;
use buck2_error::BuckErrorContext;
use buck2_events::Event;
use buck2_events::daemon_id::DaemonId;
use buck2_events::dispatch::EventDispatcher;
use buck2_events::source::ChannelEventSource;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::materialize::materializer::MaterializationMethod;
use buck2_execute_impl::executors::local::ForkserverAccess;
use buck2_fs::cwd::WorkingDirectory;
use buck2_fs::fs_util;
use buck2_fs::fs_util::DiskSpaceStats;
use buck2_fs::fs_util::disk_space_stats;
use buck2_fs::paths::abs_path::AbsPathBuf;
use buck2_interpreter::starlark_profiler::config::StarlarkProfilerConfiguration;
use buck2_profile::proto_to_profile_mode;
use buck2_profile::starlark_profiler_configuration_from_request;
use buck2_resource_control::buck_cgroup_tree::BuckCgroupTree;
use buck2_resource_control::buck_cgroup_tree::PreppedBuckCgroups;
use buck2_server_ctx::bxl::BXL_SERVER_COMMANDS;
use buck2_server_ctx::late_bindings::AUDIT_SERVER_COMMAND;
use buck2_server_ctx::late_bindings::OTHER_SERVER_COMMANDS;
use buck2_server_ctx::late_bindings::QUERY_SERVER_COMMANDS;
use buck2_server_ctx::late_bindings::STARLARK_SERVER_COMMAND;
use buck2_server_ctx::late_bindings::TARGETS_SERVER_COMMANDS;
use buck2_server_ctx::partial_result_dispatcher::NoPartialResult;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::streaming_request_handler::StreamingRequestHandler;
use buck2_server_ctx::test_command::TEST_COMMAND;
use buck2_server_starlark_debug::run::run_dap_server_command;
use buck2_test::executor_launcher::get_all_test_executors;
use buck2_util::system_stats::system_memory_stats;
use buck2_util::threads::thread_spawn;
use dice::DetectCycles;
use dice::Dice;
use dice_futures::cancellation::CancellationContext;
use dice_futures::drop::DropTogether;
use dice_futures::spawn::spawn_dropcancel;
use dupe::Dupe;
use futures::Future;
use futures::FutureExt;
use futures::Stream;
use futures::StreamExt;
use futures::TryFutureExt;
use futures::channel::mpsc;
use futures::channel::mpsc::UnboundedReceiver;
use futures::channel::mpsc::UnboundedSender;
use futures::future::BoxFuture;
use futures::stream;
use rand::RngCore;
use rand::SeedableRng;
use tokio::runtime::Handle;
use tokio::sync::oneshot;
use tokio::time::timeout;
use tonic::Code;
use tonic::Request;
use tonic::Response;
use tonic::Status;
use tonic::service::Interceptor;
use tonic::service::interceptor;
use tonic::transport::Server;

use crate::active_commands::ActiveCommand;
use crate::active_commands::ActiveCommandStateWriter;
use crate::clean_stale::clean_stale_command;
use crate::ctx::ServerCommandContext;
use crate::daemon::crash::crash;
use crate::daemon::multi_event_stream::MultiEventStream;
use crate::daemon::server_allocative::spawn_allocative;
use crate::daemon::state::DaemonState;
use crate::file_status::file_status_command;
use crate::lsp::run_lsp_server_command;
use crate::new_generic::new_generic_command;
use crate::profile::profile_command;
use crate::profiling_manager::StarlarkProfilingManager;
use crate::snapshot;
use crate::snapshot::SnapshotCollector;
use crate::subscription::run_subscription_server_command;
use crate::trace_io::trace_io_command;
use crate::version_control_revision;

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
        // It would be better to pass reason to be logged later in a more structured way, but this can't be done via
        // the existing graceful (not forced) shutdown mechanism (serve_with_incoming_shutdown), so logging here instead.
        tracing::warn!("triggered shutdown: {}", reason.reason);
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
    pub enable_trace_io: bool,
    pub reject_materializer_state: Option<SqliteIdentity>,
    pub daemon_startup_config: DaemonStartupConfig,
}

impl BuckdServerInitPreferences {
    pub async fn construct_dice(
        &self,
        io: Arc<dyn IoProvider>,
        digest_config: DigestConfig,
        root_config: &LegacyBuckConfig,
    ) -> buck2_error::Result<Arc<Dice>> {
        configure_dice_for_buck(io, digest_config, Some(root_config), self.detect_cycles).await
    }
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
        if !constant_time_eq::constant_time_eq(token.as_bytes(), self.auth_token.as_bytes()) {
            return Err(Status::unauthenticated("invalid auth token"));
        }

        if buck2_env!("BUCK2_TEST_FAIL_BUCKD_AUTH", bool, applicability = testing).unwrap() {
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
    cert_state: CertState,
    #[allocative(skip)]
    command_channel: UnboundedSender<()>,
    #[allocative(skip)]
    log_reload_handle: Arc<dyn LogConfigurationReloadHandle>,
    #[allocative(skip)]
    rt: Handle,
}

/// The BuckdServer implements the DaemonApi.
///
/// Simple endpoints are implemented here and complex things will be implemented in a sibling
/// module taking just a ServerCommandContext.
#[derive(Allocative)]
pub struct BuckdServer(Arc<BuckdServerData>);

impl BuckdServer {
    pub async fn run(
        fb: fbinit::FacebookInit,
        log_reload_handle: Arc<dyn LogConfigurationReloadHandle>,
        paths: InvocationPaths,
        delegate: Box<dyn BuckdServerDelegate>,
        init_ctx: BuckdServerInitPreferences,
        process_info: DaemonProcessInfo,
        prepped_cgroups: Option<PreppedBuckCgroups>,
        base_daemon_constraints: buck2_cli_proto::DaemonConstraints,
        listener: Pin<Box<dyn Stream<Item = Result<tokio::net::TcpStream, io::Error>> + Send>>,
        rt: Handle,
        daemon_id: DaemonId,
    ) -> buck2_error::Result<()> {
        let now = SystemTime::now();
        let now = now.duration_since(SystemTime::UNIX_EPOCH)?;

        let (shutdown_channel, shutdown_receiver): (UnboundedSender<()>, _) = mpsc::unbounded();
        let (command_channel, command_receiver): (UnboundedSender<()>, _) = mpsc::unbounded();

        let materializations = MaterializationMethod::try_new_from_config_value(
            init_ctx.daemon_startup_config.materializations.as_deref(),
        )?;

        // Create buck-out and potentially chdir to there.
        fs_util::create_dir_all(paths.buck_out_path())
            .buck_error_context("Error creating buck_out_path")?;

        // TODO(scottcao): make this not optional
        let cwd = {
            let dir = WorkingDirectory::open(paths.buck_out_path())?;
            dir.chdir_and_promise_it_will_not_change()?;
            Some(dir)
        };

        let cgroup_tree = if let Some(prepped_cgroups) = prepped_cgroups {
            Some(
                BuckCgroupTree::set_up(
                    prepped_cgroups,
                    &init_ctx.daemon_startup_config.resource_control,
                )
                .await?,
            )
        } else {
            None
        };

        let cert_state = CertState::new().await;
        certs_validation_background_job(cert_state.dupe()).await;

        let daemon_state = Arc::new(
            DaemonState::new(
                fb,
                paths,
                init_ctx,
                &rt,
                materializations,
                cwd,
                cgroup_tree,
                daemon_id,
            )
            .await?,
        );

        #[cfg(fbcode_build)]
        {
            let root_path =
                std::path::PathBuf::from(&daemon_state.paths.project_root().root().as_os_str());
            if !buck2_env!("BUCK2_DISABLE_EDEN_HEALTH_CHECK", bool)?
                && detect_eden::is_eden(root_path).unwrap_or(false)
            {
                tracing::trace!("EdenFS root detected; starting health check job");
                eden_health::edenfs_health_check(fb, daemon_state.paths.roots.project_root.dupe())
                    .await;
            }
        }

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
            cert_state,
            command_channel,
            log_reload_handle,
            rt,
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
            ) -> BoxFuture<'a, buck2_error::Result<Res>>
            + Send
            + 'static,
        Req: TryFrom<StreamingRequest, Error = buck2_error::Error> + Send + Sync + 'static,
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

    async fn run_streaming_fallible<Req, Res, PartialRes, F>(
        &self,
        req: Request<Req>,
        opts: impl StreamingCommandOptions<Req>,
        func: F,
    ) -> buck2_error::Result<Response<ResponseStream>>
    where
        F: for<'a> FnOnce(
                &'a ServerCommandContext,
                PartialResultDispatcher<PartialRes>,
                Req,
            ) -> BoxFuture<'a, buck2_error::Result<Res>>
            + Send
            + 'static,
        Req: HasClientContext + HasBuildOptions + Send + Sync + 'static,
        Res: Into<command_result::Result> + Send + 'static,
        PartialRes: Into<partial_result::PartialResult> + Send + 'static,
    {
        let command_start = Instant::now();

        if buck2_env!("BUCK2_TEST_FAIL_STREAMING", bool, applicability = testing).unwrap() {
            Err(buck2_error::buck2_error!(
                buck2_error::ErrorTag::Input,
                "Injected client streaming error"
            ))?;
        }

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
        } = ActiveCommand::new(&dispatch, client_ctx.sanitized_argv.clone());
        let data = daemon_state.data();

        // Fire off a system-wide event to record the memory usage of this process.
        // TODO(ezgi): add it to oneshot command too
        let system_warning_config = &data.system_warning_config;
        dispatch.instant_event(buck2_data::SystemInfo {
            system_total_memory_bytes: Some(system_memory_stats()),
            memory_pressure_threshold_percent: system_warning_config
                .memory_pressure_threshold_percent,
            total_disk_space_bytes: disk_space_stats(daemon_state.paths.buck_out_path())
                .ok()
                .map(|DiskSpaceStats { total_space, .. }| total_space),
            remaining_disk_space_threshold_gb: system_warning_config
                .remaining_disk_space_threshold_gb,
            min_re_download_bytes_threshold: system_warning_config.min_re_download_bytes_threshold,
            avg_re_download_bytes_per_sec_threshold: system_warning_config
                .avg_re_download_bytes_per_sec_threshold,
            optin_vpn_check_targets_regex: system_warning_config
                .optin_vpn_check_targets_regex
                .clone(),
            enable_stable_revision_check: system_warning_config.enable_stable_revision_check,
            enable_health_check_process_isolation: system_warning_config
                .enable_health_check_process_isolation,
        });

        // Fire off a snapshot before we start doing anything else. We use the metrics emitted here
        // as a baseline.
        let snapshot_collector =
            SnapshotCollector::new(data.dupe(), daemon_state.paths.buck_out_path());
        dispatch.instant_event(Box::new(snapshot_collector.create_snapshot().await));
        let cert_state = self.0.cert_state.dupe();

        let repo_root = daemon_state.paths.project_root().root().to_buf();
        // Spawn an async task to collect expensive info
        // We start collecting immediately, and emit the event as soon as it is ready
        let version_control_revision_collector =
            version_control_revision::spawn_version_control_collector(dispatch.dupe(), repo_root);

        #[cfg(unix)]
        let memory_reporter = daemon_state.data.memory_tracker.as_ref().map(|t| {
            buck2_resource_control::memory_tracker::spawn_memory_reporter(dispatch.dupe(), t.dupe())
        });

        let resp = streaming(
            req,
            events,
            state,
            dispatch.dupe(),
            daemon_shutdown_channel,
            move |req, cancellations| {
                async move {
                    let result: buck2_error::Result<Res> = try {
                        let base_context =
                            daemon_state.prepare_command(dispatch.dupe(), guard).await?;

                        let client_ctx = req.client_context()?;

                        let profiling_manager = StarlarkProfilingManager::new(
                            client_ctx.profile_pattern_opts.as_ref(),
                            opts.starlark_profiler_instrumentation_override(&req)?,
                            &base_context.events,
                        )?;

                        let context = ServerCommandContext::new(
                            base_context,
                            client_ctx,
                            profiling_manager,
                            req.build_options(),
                            &daemon_state.paths,
                            cert_state.dupe(),
                            snapshot_collector,
                            cancellations,
                            command_start,
                        )?;

                        let res =
                            func(&context, PartialResultDispatcher::new(dispatch.dupe()), req)
                                .await;

                        context.finalize().await?;
                        res?
                    };

                    // Do not kill the process prematurely.
                    drop(version_control_revision_collector);
                    #[cfg(unix)]
                    drop(memory_reporter);
                    match result {
                        Ok(_) => dispatch.command_result(result_to_command_result(result)),
                        Err(e) => match check_cert_state(cert_state).await {
                            Some(err) => dispatch.command_result(error_to_command_result(
                                err.context(format!("{e:?}")),
                            )),
                            _ => dispatch.command_result(error_to_command_result(e)),
                        },
                    }
                }
                .boxed()
            },
            &self.0.rt,
        );
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
            ) -> BoxFuture<'a, buck2_error::Result<Res>>
            + Send
            + 'static,
        Req: HasClientContext + HasBuildOptions + Send + Sync + 'static,
        Res: Into<command_result::Result> + Send + 'static,
        PartialRes: Into<partial_result::PartialResult> + Send + 'static,
    {
        // send signal to register new command time
        _ = self.0.command_channel.unbounded_send(());

        match self.run_streaming_fallible(req, opts, func).await {
            Ok(resp) => Ok(resp),
            Err(e) => match check_cert_state(self.0.cert_state.dupe()).await {
                Some(err) => Ok(error_to_response_stream(err.context(format!("{e:?}")))),
                _ => Ok(error_to_response_stream(e)),
            },
        }
    }

    async fn oneshot<
        Req,
        Res: Into<command_result::Result>,
        Fut: Future<Output = buck2_error::Result<Res>> + Send,
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
    #[allow(clippy::result_large_err)]
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

#[allow(clippy::result_large_err)]
fn convert_positive_duration(proto_duration: &prost_types::Duration) -> Result<Duration, Status> {
    if proto_duration.seconds < 0 || proto_duration.nanos < 0 {
        return Err(Status::new(
            Code::Unknown,
            format!("received invalid timeout: `{proto_duration:?}`"),
        ));
    }
    Ok(Duration::from_secs(proto_duration.seconds as u64)
        + Duration::from_nanos(proto_duration.nanos as u64))
}

fn error_to_command_result(e: buck2_error::Error) -> CommandResult {
    CommandResult {
        result: Some(command_result::Result::Error(
            buck2_data::ErrorReport::from(&e),
        )),
    }
}

fn result_to_command_result<R: Into<command_result::Result>>(
    result: buck2_error::Result<R>,
) -> CommandResult {
    match result {
        Ok(result) => CommandResult {
            result: Some(result.into()),
        },
        Err(e) => error_to_command_result(e),
    }
}

fn error_to_command_progress(e: buck2_error::Error) -> CommandProgress {
    CommandProgress {
        progress: Some(command_progress::Progress::Result(Box::new(
            error_to_command_result(e),
        ))),
    }
}

fn error_to_response_stream(e: buck2_error::Error) -> Response<ResponseStream> {
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

fn pump_events(
    mut events: ChannelEventSource,
    mut state: ActiveCommandStateWriter,
    output_send: tokio::sync::mpsc::UnboundedSender<
        Result<buck2_cli_proto::CommandProgress, tonic::Status>,
    >,
) {
    // This function returns the receiving channel back to `tonic` as a streaming response.
    while let Some(next_event) = events.receive() {
        // Ignoring errors from writing to `output_send` because they occur only when
        // the receiving end of the channel is closed. This can happen, for example,
        // if Tonic drops the streaming response due the client disconnecting.
        // In these cases, ignoring the errors is intentional as no client is listening.
        match next_event {
            // The CommandResult event indicates that the spawned
            // computation won't be producing any more events.
            Event::CommandResult(result) => {
                let _ignore = output_send.send(Ok(CommandProgress {
                    progress: Some(command_progress::Progress::Result(result)),
                }));
                return;
            }
            Event::PartialResult(result) => {
                let _ignore = output_send.send(Ok(CommandProgress {
                    progress: Some(command_progress::Progress::PartialResult(Box::new(result))),
                }));
            }
            Event::Buck(buck_event) => {
                state.peek_event(&buck_event);

                let _ignore = output_send.send(Ok(CommandProgress {
                    progress: Some(command_progress::Progress::Event(buck_event.into())),
                }));
            }
        }
    }
}

/// Dispatches a request to the given function and returns a stream of responses, suitable for streaming to a client.
#[allow(clippy::mut_mut)] // select! does this internally
fn streaming<Req, F>(
    req: Request<Req>,
    events: ChannelEventSource,
    state: ActiveCommandStateWriter,
    dispatcher: EventDispatcher,
    daemon_shutdown_channel: oneshot::Receiver<buck2_data::DaemonShutdown>,
    func: F,
    rt: &Handle,
) -> Response<ResponseStream>
where
    Req: Send + Sync + 'static,
    F: for<'a> FnOnce(Req, &'a CancellationContext) -> BoxFuture<'a, ()> + Send + 'static,
{
    // This function is responsible for receiving all events coming into an ChannelEventSource and reacting accordingly.
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

    let spawned = spawn_dropcancel(
        |cancellations| func(req, cancellations),
        &BuckSpawner::new(rt.clone()),
        &events_ctx,
    );
    let (output_send, output_recv) = tokio::sync::mpsc::unbounded_channel();

    // We run the event consumer on new non-tokio thread to avoid the consumer task from getting stuck behind
    // another tokio task in its lifo task slot. See T96012305 and https://github.com/tokio-rs/tokio/issues/4323 for more
    // information.
    let merge_task = thread_spawn("pump-events", move || {
        pump_events(events, state, output_send);
    });
    if let Err(e) = merge_task {
        return error_to_response_stream(
            buck2_error::Error::from(e).context("failed to spawn pump-events"),
        );
    };

    let events = tokio_stream::wrappers::UnboundedReceiverStream::new(output_recv);

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
    let events = futures::stream::select(events, daemon_shutdown_stream);

    let events = MultiEventStream::new(events);

    Response::new(Box::pin(SyncStream {
        wrapped: sync_wrapper::SyncWrapper::new(DropTogether::new(events, spawned)),
    }))
}

struct QueryCommandOptions {
    /// `buck2_cli_proto::ProfileMode`.
    profile_mode: Option<i32>,
}

impl OneshotCommandOptions for QueryCommandOptions {
    fn pre_run(&self, _server: &BuckdServer) -> Result<(), Status> {
        Ok(())
    }
}

impl<Req> StreamingCommandOptions<Req> for QueryCommandOptions {
    fn starlark_profiler_instrumentation_override(
        &self,
        _req: &Req,
    ) -> buck2_error::Result<StarlarkProfilerConfiguration> {
        match self.profile_mode {
            None => Ok(StarlarkProfilerConfiguration::None),
            Some(mode) => {
                let mode = buck2_cli_proto::ProfileMode::try_from(mode)
                    .internal_error("invalid profile mode enum value")?;
                Ok(StarlarkProfilerConfiguration::ProfileLoading(
                    proto_to_profile_mode(mode),
                    // We enable profiling for everything,
                    // but collect results only for subset of packages.
                    UnparsedPatternPredicate::Any,
                ))
            }
        }
    }
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
                    .buck_error_context("requested payload too large")?
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
                Some(
                    snapshot::SnapshotCollector::new(
                        daemon_state.data(),
                        daemon_state.paths.buck_out_path(),
                    )
                    .create_snapshot()
                    .await,
                )
            } else {
                None
            };

            let extra_constraints = buck2_cli_proto::ExtraDaemonConstraints {
                trace_io_enabled: TracingIoProvider::from_io(&*daemon_state.data().io).is_some(),
                materializer_state_identity: daemon_state
                    .data()
                    .materializer_state_identity
                    .as_ref()
                    .map(|i| i.to_string()),
            };

            let mut daemon_constraints = self.0.base_daemon_constraints.clone();
            daemon_constraints.extra = Some(extra_constraints);

            let valid_working_directory = daemon_state.validate_cwd().is_ok();
            let valid_buck_out_mount = daemon_state.validate_buck_out_mount().is_ok();

            let io_provider = daemon_state.data().io.name().to_owned();

            let uptime = Instant::now() - self.0.start_instant;

            let mut base = StatusResponse {
                process_info: Some(self.0.process_info.clone()),
                start_time: Some(self.0.start_time),
                uptime: Some(uptime.try_into()?),
                snapshot,
                daemon_constraints: Some(daemon_constraints),
                project_root: daemon_state.paths.project_root().to_string(),
                isolation_dir: daemon_state.paths.isolation.to_string(),
                forkserver_pid: match &daemon_state.data.forkserver {
                    #[cfg(unix)]
                    ForkserverAccess::Client(f) => Some(f.pid()),
                    ForkserverAccess::None => None,
                },
                supports_vpnless: Some(daemon_state.data().http_client.supports_vpnless()),
                http2: Some(daemon_state.data().http_client.http2()),
                valid_working_directory: Some(valid_working_directory),
                valid_buck_out_mount: Some(valid_buck_out_mount),
                io_provider: Some(io_provider),
                ..Default::default()
            };

            if req.include_tokio_runtime_metrics {
                let tokio_metrics = self.0.rt.metrics();
                let metrics_response = TokioRuntimeMetrics {
                    num_workers: tokio_metrics.num_workers() as u64,
                    num_alive_tasks: tokio_metrics.num_alive_tasks() as u64,
                    global_queue_depth: tokio_metrics.global_queue_depth() as u64,
                };
                base.tokio_runtime_metrics = Some(metrics_response);
            }
            Ok(base)
        })
        .await
    }

    async fn flush_dep_files(
        &self,
        req: Request<FlushDepFilesRequest>,
    ) -> Result<Response<CommandResult>, Status> {
        self.oneshot(req, DefaultCommandOptions, move |req| async move {
            let FlushDepFilesRequest {
                retain_locally_produced_dep_files,
            } = req;
            if retain_locally_produced_dep_files {
                buck2_file_watcher::dep_files::flush_non_local_dep_files();
            } else {
                buck2_file_watcher::dep_files::flush_dep_files();
            }
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
        self.run_streaming(
            req,
            DefaultCommandOptions,
            |ctx, partial_result_dispatcher, req| {
                Box::pin(async {
                    OTHER_SERVER_COMMANDS
                        .get()?
                        .build(ctx, partial_result_dispatcher, req)
                        .await
                })
            },
        )
        .await
    }

    type BxlStream = ResponseStream;
    async fn bxl(&self, req: Request<BxlRequest>) -> Result<Response<ResponseStream>, Status> {
        self.run_streaming(
            req,
            DefaultCommandOptions,
            |ctx, partial_result_dispatcher, req| {
                Box::pin(async {
                    BXL_SERVER_COMMANDS
                        .get()?
                        .bxl(ctx, partial_result_dispatcher, req)
                        .await
                })
            },
        )
        .await
    }

    type TestStream = ResponseStream;
    async fn test(&self, req: Request<TestRequest>) -> Result<Response<ResponseStream>, Status> {
        self.run_streaming(
            req,
            DefaultCommandOptions,
            |ctx, partial_result_dispatcher, req| {
                Box::pin(async { (TEST_COMMAND.get()?)(ctx, partial_result_dispatcher, req).await })
            },
        )
        .await
    }

    type AqueryStream = ResponseStream;
    async fn aquery(
        &self,
        req: Request<AqueryRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        self.run_streaming(
            req,
            DefaultCommandOptions,
            |ctx, partial_result_dispatcher, req| {
                Box::pin(async {
                    QUERY_SERVER_COMMANDS
                        .get()?
                        .aquery(ctx, partial_result_dispatcher, req)
                        .await
                })
            },
        )
        .await
    }

    type UqueryStream = ResponseStream;
    async fn uquery(
        &self,
        req: Request<UqueryRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        self.run_streaming(
            req,
            DefaultCommandOptions,
            |ctx, partial_result_dispatcher, req| {
                Box::pin(async {
                    QUERY_SERVER_COMMANDS
                        .get()?
                        .uquery(ctx, partial_result_dispatcher, req)
                        .await
                })
            },
        )
        .await
    }

    type CqueryStream = ResponseStream;
    async fn cquery(
        &self,
        req: Request<CqueryRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        let profile_mode = req.get_ref().profile_mode;
        self.run_streaming(
            req,
            QueryCommandOptions { profile_mode },
            |ctx, partial_result_dispatcher, req| {
                Box::pin(async {
                    QUERY_SERVER_COMMANDS
                        .get()?
                        .cquery(ctx, partial_result_dispatcher, req)
                        .await
                })
            },
        )
        .await
    }

    type TargetsStream = ResponseStream;
    async fn targets(
        &self,
        req: Request<TargetsRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        self.run_streaming(
            req,
            DefaultCommandOptions,
            |ctx, partial_result_dispatcher, req| {
                Box::pin(async {
                    TARGETS_SERVER_COMMANDS
                        .get()?
                        .targets(ctx, partial_result_dispatcher, req)
                        .await
                })
            },
        )
        .await
    }

    type CtargetsStream = ResponseStream;
    async fn ctargets(
        &self,
        req: Request<ConfiguredTargetsRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        self.run_streaming(
            req,
            DefaultCommandOptions,
            |ctx, partial_result_dispatcher, req| {
                Box::pin(async {
                    TARGETS_SERVER_COMMANDS
                        .get()?
                        .ctargets(ctx, partial_result_dispatcher, req)
                        .await
                })
            },
        )
        .await
    }

    type TargetsShowOutputsStream = ResponseStream;
    async fn targets_show_outputs(
        &self,
        req: Request<TargetsRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        self.run_streaming(
            req,
            DefaultCommandOptions,
            |ctx, partial_result_dispatcher, req| {
                Box::pin(async {
                    TARGETS_SERVER_COMMANDS
                        .get()?
                        .targets_show_outputs(ctx, partial_result_dispatcher, req)
                        .await
                })
            },
        )
        .await
    }

    type AuditStream = ResponseStream;
    async fn audit(
        &self,
        req: Request<GenericRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        self.run_streaming(
            req,
            DefaultCommandOptions,
            |ctx, partial_result_dispatcher, req| {
                Box::pin(async {
                    AUDIT_SERVER_COMMAND
                        .get()?
                        .audit(ctx, partial_result_dispatcher, req)
                        .await
                })
            },
        )
        .await
    }

    type StarlarkStream = ResponseStream;
    async fn starlark(
        &self,
        req: Request<GenericRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        self.run_streaming(
            req,
            DefaultCommandOptions,
            |ctx, partial_result_dispatcher, req| {
                Box::pin(async {
                    STARLARK_SERVER_COMMAND
                        .get()?
                        .starlark(ctx, partial_result_dispatcher, req)
                        .await
                })
            },
        )
        .await
    }

    type InstallStream = ResponseStream;
    async fn install(
        &self,
        req: Request<InstallRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        self.run_streaming(
            req,
            DefaultCommandOptions,
            |ctx, partial_result_dispatcher, req| {
                Box::pin(async {
                    OTHER_SERVER_COMMANDS
                        .get()?
                        .install(ctx, partial_result_dispatcher, req)
                        .await
                })
            },
        )
        .await
    }

    async fn unstable_crash(
        &self,
        req: Request<UnstableCrashRequest>,
    ) -> Result<Response<CommandResult>, Status> {
        self.oneshot(
            req,
            DefaultCommandOptions,
            move |req| async move { crash(req) },
        )
        .await
    }

    async fn unstable_heap_dump(
        &self,
        req: Request<UnstableHeapDumpRequest>,
    ) -> Result<Response<UnstableHeapDumpResponse>, Status> {
        self.check_if_accepting_requests()?;
        let req = req.into_inner();

        memory::write_heap_to_file(&req.destination_path)
            .map_err(|e| Status::invalid_argument(format!("failed to perform heap dump: {e}")))?;
        if let Some(test_executor_destination_path) = req.test_executor_destination_path {
            let test_executors = get_all_test_executors();
            tracing::debug!(
                "currently have {} test executor(s), dumping last one to {}",
                test_executors.len(),
                test_executor_destination_path
            );
            // TODO: Figure out a way to dump all of them and not just the last.
            if let Some(test_executor) = test_executors.last() {
                test_executor
                    .unstable_heap_dump(&test_executor_destination_path)
                    .await
                    .map_err(|e| {
                        Status::invalid_argument(format!("failed to perform heap dump: {e}"))
                    })?;
            }
        }
        Ok(Response::new(UnstableHeapDumpResponse {}))
    }

    async fn unstable_allocator_stats(
        &self,
        req: Request<UnstableAllocatorStatsRequest>,
    ) -> Result<Response<UnstableAllocatorStatsResponse>, Status> {
        self.check_if_accepting_requests()?;

        let response = memory::allocator_stats(&req.into_inner().options)
            .buck_error_context("Failed to retrieve allocator stats");

        match response {
            Ok(response) => Ok(Response::new(UnstableAllocatorStatsResponse { response })),
            Err(e) => Err(Status::invalid_argument(format!("{e:#}"))),
        }
    }

    async fn unstable_dice_dump(
        &self,
        req: Request<UnstableDiceDumpRequest>,
    ) -> Result<Response<UnstableDiceDumpResponse>, Status> {
        self.check_if_accepting_requests()?;

        let inner = req.into_inner();
        let path = inner.destination_path;
        let res: buck2_error::Result<_> = try {
            let path = Path::new(&path);
            let format_proto =
                buck2_cli_proto::unstable_dice_dump_request::DiceDumpFormat::try_from(inner.format)
                    .buck_error_context("Invalid DICE dump format")?;

            self.0
                .daemon_state
                .data()
                .spawn_dice_dump(path, format_proto)
                .await
                .with_buck_error_context(|| {
                    format!("Failed to perform dice dump to {}", path.display())
                })?;

            UnstableDiceDumpResponse {}
        };

        res.map(Response::new)
            .map_err(|e| Status::internal(format!("{e:#}")))
    }

    type AllocativeStream = ResponseStream;
    async fn allocative(
        &self,
        req: Request<AllocativeRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        self.check_if_accepting_requests()?;

        let res: buck2_error::Result<_> = try {
            let client_ctx = req.get_ref().client_context()?;
            let trace_id = client_ctx
                .trace_id
                .parse()
                .map_err(buck2_error::Error::from)?;
            let (event_source, dispatcher) = self.0.daemon_state.prepare_events(trace_id).await?;
            let active_command = ActiveCommand::new(&dispatcher, client_ctx.sanitized_argv.clone());
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
                    dispatcher.command_result(result_to_command_result(result));

                    drop(guard);
                }
                .boxed()
            },
            &self.0.rt,
        ))
    }

    type Profile2Stream = ResponseStream;
    async fn profile2(
        &self,
        req: Request<ProfileRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        struct ProfileCommandOptions {
            project_root: ProjectRoot,
        }

        impl OneshotCommandOptions for ProfileCommandOptions {}

        impl StreamingCommandOptions<ProfileRequest> for ProfileCommandOptions {
            fn starlark_profiler_instrumentation_override(
                &self,
                req: &ProfileRequest,
            ) -> buck2_error::Result<StarlarkProfilerConfiguration> {
                starlark_profiler_configuration_from_request(req, &self.project_root)
            }
        }

        self.run_streaming(
            req,
            ProfileCommandOptions {
                project_root: self.0.daemon_state.paths.project_root().dupe(),
            },
            |ctx, partial_result_dispatcher, req| {
                Box::pin(async {
                    match req.profile_opts.as_ref().expect("Missing profile opts") {
                        buck2_cli_proto::profile_request::ProfileOpts::TargetProfile(_) => {
                            profile_command(ctx, partial_result_dispatcher, req).await
                        }
                        buck2_cli_proto::profile_request::ProfileOpts::BxlProfile(_) => {
                            Ok(BXL_SERVER_COMMANDS
                                .get()?
                                .bxl_profile(ctx, partial_result_dispatcher, req)
                                .await?)
                        }
                    }
                })
            },
        )
        .await
    }

    type NewGenericImplStream = ResponseStream;
    async fn new_generic_impl(
        &self,
        req: Request<NewGenericRequestMessage>,
    ) -> Result<Response<ResponseStream>, Status> {
        self.run_streaming(
            req,
            DefaultCommandOptions,
            |context, partial: PartialResultDispatcher<NoPartialResult>, req| {
                new_generic_command(context, req, partial).boxed()
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
                .buck_error_context("Error updating daemon log filter")
                .map_err(|e| Status::invalid_argument(format!("{e:#}")))?;
        }

        #[cfg(unix)]
        if req.forkserver {
            let data = self.0.daemon_state.data();
            if let ForkserverAccess::Client(forkserver) = &data.forkserver {
                forkserver
                    .set_log_filter(req.log_filter)
                    .await
                    .buck_error_context("Error forwarding daemon log filter to forkserver")
                    .map_err(|e| Status::invalid_argument(format!("{e:#}")))?;
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
    #[allow(clippy::result_large_err)]
    fn pre_run(&self, server: &BuckdServer) -> Result<(), Status> {
        server.check_if_accepting_requests()
    }
}

/// Options to configure the execution of a streaming command (i.e. what happens in `run_streaming()`).
trait StreamingCommandOptions<Req>: OneshotCommandOptions {
    fn starlark_profiler_instrumentation_override(
        &self,
        _req: &Req,
    ) -> buck2_error::Result<StarlarkProfilerConfiguration> {
        Ok(StarlarkProfilerConfiguration::None)
    }
}

fn server_shutdown_signal(
    command_receiver: UnboundedReceiver<()>,
    mut shutdown_receiver: UnboundedReceiver<()>,
) -> buck2_error::Result<impl Future<Output = ()>> {
    let mut duration = DEFAULT_INACTIVITY_TIMEOUT;
    if buck2_env!(
        "BUCK2_TESTING_INACTIVITY_TIMEOUT",
        bool,
        applicability = testing
    )? {
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
    while (timeout(duration, command_receiver.next()).await).is_ok() {}

    tracing::warn!(
        "inactivity timeout elapsed ({:?}), shutting down server",
        duration
    );
}

async fn certs_validation_background_job(cert_state: CertState) {
    tokio::task::spawn(async move {
        const CERTS_VALIDATION_INTERVAL: u64 = 60 * 60; // 1 hour
        loop {
            tokio::time::sleep(Duration::from_secs(CERTS_VALIDATION_INTERVAL)).await;
            let result = validate_certs().await;
            let mut valid = cert_state.state.lock().await;

            *valid = result.is_ok();
        }
    });
}

#[cfg(fbcode_build)]
mod eden_health {
    use std::time::Duration;

    use buck2_core::fs::project::ProjectRoot;
    use buck2_core::soft_error;
    use buck2_eden::connection::EdenConnectionManager;
    use buck2_eden::error::ErrorFromHangingMount;
    use buck2_eden::semaphore;
    use buck2_error::buck2_error;

    pub(crate) async fn edenfs_health_check(fb: fbinit::FacebookInit, root: ProjectRoot) {
        tokio::task::spawn(async move {
            const HEALTH_CHECK_INTERVAL: u64 = 60 * 10; // 10 minutes
            tracing::trace!(
                "spawned EdenFS health check that runs every {} seconds",
                HEALTH_CHECK_INTERVAL
            );
            loop {
                tokio::time::sleep(Duration::from_secs(HEALTH_CHECK_INTERVAL)).await;
                match EdenConnectionManager::new(fb, &root, Some(semaphore::buck2_default())) {
                    Ok(Some(conn)) => {
                        let info = conn
                            .with_eden(|eden| {
                                tracing::trace!("running getDaemonInfo() on EdenFS service");
                                eden.getDaemonInfo()
                            })
                            .await;
                        match info {
                            Err(e) if e.is_caused_by_hanging_mount() => {
                                tracing::error!(
                                    "check hit an EdenFS error caused by a hanging mount: {:#}",
                                    e
                                );
                                soft_error!(
                                    "eden_thrift_health_check_failed",
                                    buck2_error!(buck2_error::ErrorTag::Input, "check failed with: {:#}", e),
                                    quiet: true
                                )
                                .ok();
                            }
                            _ => {
                                tracing::debug!("check determined EdenFS is not hanging");
                            }
                        }
                    }
                    // Only occurs if the .eden dir doesn't exist (indicative of an unmounted mount).
                    // Ignore this case since it's not related to a hanging Eden daemon.
                    Ok(None) => {
                        tracing::debug!("failed to run check: .eden dir does not exist");
                    }
                    // Can occur for a number of reasons, including IO into the Eden mount failing.
                    // This _could_ signal a hanging mount, so we'll report a possible hang.
                    Err(e) => {
                        tracing::error!("check failed to create an EdenFS client: {:#}", e);
                        soft_error!(
                            "eden_thrift_client_creation_failed",
                            buck2_error!(buck2_error::ErrorTag::Input, "client creation failed with: {:#}", e),
                            quiet: true
                        )
                        .ok();
                    }
                }
            }
        });
    }
}

/// No-op set of command options.
struct DefaultCommandOptions;

impl OneshotCommandOptions for DefaultCommandOptions {}

impl<Req> StreamingCommandOptions<Req> for DefaultCommandOptions {}
