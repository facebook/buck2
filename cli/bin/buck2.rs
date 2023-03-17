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

use buck2_client_ctx::exit_result::ExitResult;
use buck2_core::fs::working_dir::WorkingDir;
use buck2_core::logging::init_tracing_for_writer;
use buck2_core::logging::LogConfigurationReloadHandle;
use cli::exec;
use cli::panic;
use cli::TracingLogFile;
use fbinit::FacebookInit;

// fbcode likes to set its own allocator in fbcode.default_allocator
// So when we set our own allocator, buck build buck2 or buck2 build buck2 often breaks.
// Making jemalloc the default only when we do a cargo build.
#[cfg_attr(all(unix, not(fbcode_build), not(buck_oss_build)), global_allocator)]
#[cfg(all(unix, not(fbcode_build), not(buck_oss_build)))]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

fn init_logging(_fb: FacebookInit) -> anyhow::Result<Box<dyn LogConfigurationReloadHandle>> {
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

        // The Buck2 wrapper sets the "BUCK2_ENABLE_SCRIBE" environment variable to indicate that we are being launched
        // in production. When we're in production, enable Scribe logging. We also need to delete the environment
        // variable so that child buck2 invocations (i.e. buck2 run //buck2:buck2) don't inherit this value.
        if let Ok("0") = std::env::var("BUCK2_ENABLE_SCRIBE").as_deref() {
            scribe::disable();
            std::env::remove_var("BUCK2_ENABLE_SCRIBE");
        }

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

// As this main() is used as the entry point for the `buck daemon` command,
// it must be single-threaded. Commands that want to be multi-threaded/async
// will start up their own tokio runtime.
#[fbinit::main]
fn main(init: fbinit::FacebookInit) -> ! {
    fn main_with_result(init: fbinit::FacebookInit) -> ExitResult {
        panic::initialize(init)?;
        check_cargo();
        let log_reload_handle = init_logging(init)?;

        let args = std::env::args().collect::<Vec<String>>();
        let cwd = WorkingDir::current_dir()?;

        exec(args, cwd, init, log_reload_handle, None)
    }

    main_with_result(init).report()
}
