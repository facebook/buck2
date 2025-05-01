/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(used_with_arg)]

use std::fs;
use std::io;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::SystemTime;
use std::time::UNIX_EPOCH;

use buck2::exec;
use buck2::panic;
use buck2::process_context::ProcessContext;
use buck2_build_info::BUCK2_BUILD_INFO;
use buck2_build_info::Buck2BuildInfo;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::restarter::Restarter;
use buck2_client_ctx::stdin::Stdin;
use buck2_client_ctx::stdio;
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
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

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
        buck2_audit_server::init_late_bindings();
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

    fn main_with_result() -> ExitResult {
        let start_time = get_unix_timestamp_millis();

        panic::initialize().map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))?;
        check_cargo();

        let force_want_restart = buck2_env!("FORCE_WANT_RESTART", bool)?;

        let log_reload_handle =
            init_logging().map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))?;

        // Log the start timestamp
        tracing::debug!("Client initialized logging");

        let args = std::env::args().collect::<Vec<String>>();
        let cwd = AbsWorkingDir::current_dir()?;
        let mut stdin = Stdin::new()?;
        let mut restarter = Restarter::new();

        let first_trace_id = TraceId::from_env_or_new()?;

        let res = exec(ProcessContext {
            log_reload_handle: &log_reload_handle,
            stdin: &mut stdin,
            working_dir: &cwd,
            start_time,
            args: &args,
            restarter: &mut restarter,
            trace_id: first_trace_id.dupe(),
            restarted_trace_id: None,
        });

        let restart = |res| {
            let restart_start_time = get_unix_timestamp_millis();

            if !force_want_restart && !restarter.should_restart() {
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

            exec(ProcessContext {
                log_reload_handle: &log_reload_handle,
                stdin: &mut stdin,
                start_time: restart_start_time,
                working_dir: &cwd,
                args: &args,
                restarter: &mut restarter,
                trace_id: TraceId::new(),
                restarted_trace_id: Some(first_trace_id),
            })
        };

        if force_want_restart {
            restart(res)
        } else {
            res.or_else(restart)
        }
    }

    pub fn get_unix_timestamp_millis() -> u64 {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis()
            .try_into()
            .unwrap()
    }

    main_with_result().report()
}
