/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#![feature(used_with_arg)]

use std::fs;
use std::io;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::SystemTime;

use buck2::exec;
use buck2::panic;
use buck2::process_context::ClientRuntime;
use buck2::process_context::ProcessContext;
use buck2::process_context::SharedProcessContext;
use buck2_build_info::BUCK2_BUILD_INFO;
use buck2_build_info::Buck2BuildInfo;
use buck2_client_ctx::events_ctx::EventsCtx;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::restarter::Restarter;
use buck2_client_ctx::stdin::Stdin;
use buck2_client_ctx::stdio;
use buck2_client_ctx::subscribers::recorder::InvocationRecorder;
use buck2_core::buck2_env;
use buck2_core::fs::working_dir::AbsWorkingDir;
use buck2_core::logging::LogConfigurationReloadHandle;
use buck2_core::logging::init_tracing_for_writer;
use buck2_core::logging::log_file::TracingLogFile;
use buck2_error::conversion::from_any_with_tag;
use buck2_wrapper_common::invocation_id::TraceId;
use dupe::Dupe;

// fbcode likes to set its own allocator in fbcode.default_allocator
// So when we set our own allocator, buck build buck2 or buck2 build buck2 often breaks.
// Making jemalloc the default only when we do a cargo build.
#[global_allocator]
#[cfg(all(any(target_os = "linux", target_os = "macos"), not(buck_build)))]
static ALLOC: tikv_jemallocator::Jemalloc = tikv_jemallocator::Jemalloc;
#[global_allocator]
#[cfg(target_os = "windows")]
static ALLOC: mimalloc::MiMalloc = mimalloc::MiMalloc;

fn init_logging() -> anyhow::Result<Arc<dyn LogConfigurationReloadHandle>> {
    static ENV_TRACING_LOG_FILE_PATH: &str = "BUCK_LOG_TO_FILE_PATH";

    let handle = match std::env::var_os(ENV_TRACING_LOG_FILE_PATH) {
        Some(path) => {
            let path = PathBuf::from(path);
            // we set the writer to stderr first until later, when we have the logdir, set the
            // tracing log sink to that file

            fs::create_dir_all(&path)?;
            let tracing_log = path.join("tracing_log");
            let file = TracingLogFile::new(tracing_log)?;
            init_tracing_for_writer(file)
        }
        _ => init_tracing_for_writer(io::stderr),
    }?;

    #[cfg(fbcode_build)]
    {
        use buck2_event_log::should_upload_log;
        use buck2_events::sink::remote;

        if !should_upload_log()? {
            remote::disable();
        }
    }

    Ok(handle)
}

// When using a cargo build, some essential services (e.g. RE, scribe)
// fall back to slow paths that give terrible performance.
// Therefore, if we are using cargo, warn strongly.
fn check_cargo() {
    if !cfg!(fbcode_build) && !buck2_core::is_open_source() {
        eprintln!("=====================================================================");
        eprintln!("WARNING: You are using Buck v2 compiled with `cargo`, not `buck`.");
        eprintln!("         Some operations may go slower and logging may be impaired.");
        eprintln!("=====================================================================");
        eprintln!();
    }
}

fn print_retry() -> anyhow::Result<()> {
    buck2_client_ctx::eprintln!("============================================================")?;
    buck2_client_ctx::eprintln!("|| Buck2 has detected that it needs to restart to proceed ||")?;
    buck2_client_ctx::eprintln!("|| Your command will now restart.                         ||")?;
    buck2_client_ctx::eprintln!("============================================================")?;
    buck2_client_ctx::eprintln!()?;
    Ok(())
}

fn exec_with_logging(
    trace_id: TraceId,
    start_time: SystemTime,
    restarted_trace_id: Option<TraceId>,
    shared: buck2_error::Result<SharedProcessContext>,
    runtime: &mut ClientRuntime,
) -> (Option<SharedProcessContext>, ExitResult) {
    let args = std::env::args().collect::<Vec<String>>();
    let recorder = InvocationRecorder::new(trace_id.dupe(), restarted_trace_id, start_time, args);
    let mut events_ctx = EventsCtx::new(Some(recorder), vec![]);
    let (shared, res) = match shared {
        Ok(mut shared) => {
            let res = exec(ProcessContext {
                trace_id: trace_id.dupe(),
                events_ctx: &mut events_ctx,
                shared: &mut shared,
                runtime,
                start_time,
            });
            (Some(shared), res)
        }
        Err(e) => (None, e.into()),
    };
    let res = match runtime.get_or_init() {
        Ok(runtime) => events_ctx.finalize_events(trace_id, res, &runtime),
        Err(e) => e.into(),
    };
    (shared, res)
}

// As this main() is used as the entry point for the `buck daemon` command,
// it must be single-threaded. Commands that want to be multi-threaded/async
// will start up their own tokio runtime.
fn main() -> ! {
    buck2_core::client_only::CLIENT_ONLY_VAL.init(cfg!(client_only));
    #[cfg(not(client_only))]
    {
        buck2_analysis::init_late_bindings();
        buck2_anon_target::init_late_bindings();
        buck2_action_impl::init_late_bindings();
        buck2_cmd_audit_server::init_late_bindings();
        buck2_build_api::init_late_bindings();
        buck2_cmd_docs_server::init_late_bindings();
        buck2_external_cells::init_late_bindings();
        buck2_transition::init_late_bindings();
        buck2_build_signals_impl::init_late_bindings();
        buck2_bxl::init_late_bindings();
        buck2_cfg_constructor::init_late_bindings();
        buck2_configured::init_late_bindings();
        buck2_query_impls::init_late_bindings();
        buck2_interpreter_for_build::init_late_bindings();
        buck2_server_commands::init_late_bindings();
        buck2_cmd_targets_server::init_late_bindings();
        buck2_cmd_query_server::init_late_bindings();
        buck2_cmd_starlark_server::init_late_bindings();
        buck2_test::init_late_bindings();
        buck2_validation::init_late_bindings();
        buck2_events::init_late_bindings();
    }
    BUCK2_BUILD_INFO.init(Buck2BuildInfo {
        revision: std::option_env!("BUCK2_SET_EXPLICIT_VERSION"),
        win_internal_version: std::option_env!("BUCK2_WIN_INTERNAL_VERSION"),
        release_timestamp: std::option_env!("BUCK2_RELEASE_TIMESTAMP"),
    });

    // Set up crypto impl once per process
    buck2_certs::certs::setup_cryptography_or_fail();

    fn init_shared_context() -> buck2_error::Result<SharedProcessContext> {
        panic::initialize().map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))?;
        check_cargo();

        // Log the start timestamp
        tracing::debug!("Client initialized logging");

        Ok(SharedProcessContext {
            log_reload_handle: init_logging()
                .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))?,
            stdin: Stdin::new()?,
            working_dir: AbsWorkingDir::current_dir()?,
            args: std::env::args().collect::<Vec<String>>(),
            restarter: Restarter::new(),
            force_want_restart: buck2_env!("FORCE_WANT_RESTART", bool)?,
        })
    }

    fn main_with_result() -> ExitResult {
        let start_time = SystemTime::now();
        let first_trace_id = TraceId::from_env_or_new()?;
        let mut runtime = ClientRuntime::new();
        let shared = init_shared_context();
        let (shared, res) = exec_with_logging(
            first_trace_id.dupe(),
            start_time,
            None,
            shared,
            &mut runtime,
        );

        if let Some(shared) = shared {
            maybe_restart(first_trace_id, res, shared, &mut runtime)
        } else {
            res
        }
    }

    fn maybe_restart(
        first_trace_id: TraceId,
        initial_result: ExitResult,
        shared: SharedProcessContext,
        runtime: &mut ClientRuntime,
    ) -> ExitResult {
        let force_want_restart = shared.force_want_restart;
        let restart = |res| {
            let restart_start_time = SystemTime::now();

            if !force_want_restart && !shared.restarter.should_restart() {
                tracing::debug!("No restart was requested");
                return res;
            }

            if stdio::has_written_to_stdout() {
                tracing::debug!("Cannot restart: wrote to stdout");
                return res;
            }

            if print_retry().is_err() {
                tracing::debug!("Cannot restart: warning message cannot be printed");
                return res;
            }

            let (_, res) = exec_with_logging(
                TraceId::new(),
                restart_start_time,
                Some(first_trace_id),
                Ok(shared),
                runtime,
            );
            res
        };

        if force_want_restart {
            restart(initial_result)
        } else {
            initial_result.or_else(restart)
        }
    }

    main_with_result().report()
}
