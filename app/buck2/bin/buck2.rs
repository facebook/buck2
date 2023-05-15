/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(async_closure)]
// Plugins
#![cfg_attr(feature = "gazebo_lint", feature(plugin))]
#![cfg_attr(feature = "gazebo_lint", allow(deprecated))] // :(
#![cfg_attr(feature = "gazebo_lint", plugin(gazebo_lint))]

use std::fs;
use std::io;
use std::path::PathBuf;
use std::sync::Arc;

use buck2::exec;
use buck2::panic;
use buck2::process_context::ProcessContext;
use buck2::TracingLogFile;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::restarter::Restarter;
use buck2_client_ctx::stdin::Stdin;
use buck2_client_ctx::stdio;
use buck2_core::env_helper::EnvHelper;
use buck2_core::fs::working_dir::WorkingDir;
use buck2_core::logging::init_tracing_for_writer;
use buck2_core::logging::LogConfigurationReloadHandle;
use buck2_wrapper_common::invocation_id::TraceId;
use dupe::Dupe;
use fbinit::FacebookInit;

// fbcode likes to set its own allocator in fbcode.default_allocator
// So when we set our own allocator, buck build buck2 or buck2 build buck2 often breaks.
// Making jemalloc the default only when we do a cargo build.
#[global_allocator]
#[cfg(all(
    any(target_os = "linux", target_os = "macos"),
    not(fbcode_build),
    not(buck_oss_build)
))]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

fn init_logging(_fb: FacebookInit) -> anyhow::Result<Arc<dyn LogConfigurationReloadHandle>> {
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
        use buck2_client_ctx::subscribers::should_upload_log;
        use buck2_events::sink::scribe;
        use gflags::GflagValue;

        // There are two sources of log spew when building buck2 with Buck and linking against fbcode:
        //   1. folly/logging/xlog, which can be configured via a special configuration string, which we use to
        //      log only critical-level logs. https://github.com/facebook/folly/blob/master/folly/logging/docs/Config.md
        //   2. google log (glog), which is older but still used, which can configured using a flag at runtime.
        //
        // This first call handles the folly config.
        logging::update_logging_config(_fb, "CRITICAL");
        gflags::set_gflag_value(_fb, "minloglevel", GflagValue::U32(5))?;
        gflags::set_gflag_value(_fb, "stderrthreshold", GflagValue::U32(5))?;

        if !should_upload_log()? {
            scribe::disable();
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
#[fbinit::main]
fn main(init: fbinit::FacebookInit) -> ! {
    buck2_action_impl::init_late_bindings();
    buck2_audit_server::init_late_bindings();
    buck2_bxl::init_late_bindings();
    buck2_query_impls::init_late_bindings();
    buck2_interpreter_for_build::init_late_bindings();

    fn main_with_result(init: fbinit::FacebookInit) -> ExitResult {
        panic::initialize(init)?;
        check_cargo();

        static FORCE_WANT_RESTART: EnvHelper<bool> = EnvHelper::new("FORCE_WANT_RESTART");

        let force_want_restart = FORCE_WANT_RESTART.get_copied()?.unwrap_or_default();

        let log_reload_handle = init_logging(init)?;

        let args = std::env::args().collect::<Vec<String>>();
        let cwd = WorkingDir::current_dir()?;
        let mut stdin = Stdin::new()?;
        let mut restarter = Restarter::new();

        let first_trace_id = TraceId::from_env_or_new()?;

        let res = exec(ProcessContext {
            init,
            log_reload_handle: &log_reload_handle,
            stdin: &mut stdin,
            working_dir: &cwd,
            args: &args,
            restarter: &mut restarter,
            trace_id: first_trace_id.dupe(),
            restarted_trace_id: None,
        });

        let restart = |res| {
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
                init,
                log_reload_handle: &log_reload_handle,
                stdin: &mut stdin,
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

    main_with_result(init).report()
}
