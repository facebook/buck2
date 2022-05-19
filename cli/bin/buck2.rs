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

use std::io;

use anyhow::Context as _;
use buck2_core::exit_result::ExitResult;
use cli::{exec, panic};
use fbinit::FacebookInit;
use tracing_subscriber::EnvFilter;

// fbcode likes to set its own allocator in fbcode.default_allocator
// So when we set our own allocator, buck build buck2 or buck2 build buck2 often breaks.
// Making jemalloc the default only when we do a cargo build.
#[cfg_attr(all(unix, not(fbcode_build)), global_allocator)]
#[cfg(all(unix, not(fbcode_build)))]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

fn init_logging(_fb: FacebookInit) -> anyhow::Result<()> {
    const ENV_VAR: &str = "BUCK_LOG";

    // By default, show warnings/errors.
    // If the user specifies BUCK_LOG, we want to honour that.

    let filter = match std::env::var_os(ENV_VAR) {
        Some(v) => {
            let v = v
                .into_string()
                .ok()
                .with_context(|| format!("Failed to parse ${} as utf-8", ENV_VAR))?;
            EnvFilter::try_new(v)
                .with_context(|| format!("Failed to parse ${} as a filter", ENV_VAR))?
        }
        None => EnvFilter::new("warn"),
    };

    tracing_subscriber::fmt()
        .with_env_filter(filter)
        .with_writer(io::stderr)
        .init();
    #[cfg(fbcode_build)]
    {
        use events::sink::scribe;
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
        // variable so that child buck2 invocations (i.e. buck2 run //buck2/cli:buck2) don't inherit this value.
        if let Ok("0") = std::env::var("BUCK2_ENABLE_SCRIBE").as_deref() {
            scribe::disable();
            std::env::remove_var("BUCK2_ENABLE_SCRIBE");
        }
    }

    Ok(())
}

// When using a cargo build, some essential services (e.g. RE, scribe)
// fall back to slow paths that give terrible performance.
// Therefore, if we are using cargo, warn strongly.
fn check_cargo() {
    if !cfg!(fbcode_build) {
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
fn main(init: fbinit::FacebookInit) -> ExitResult {
    // Need to add the terminate on panic handler _before_
    // panic::initialize so that we report panics for logging, then abort.
    gazebo::terminate_on_panic();
    panic::initialize(init);
    check_cargo();
    init_logging(init)?;

    let args = std::env::args().collect::<Vec<String>>();
    let cwd = std::env::current_dir()?;

    exec(args, cwd, init, None)
}
