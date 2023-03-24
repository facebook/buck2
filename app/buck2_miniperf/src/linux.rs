/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::env;
use std::os::unix::process::ExitStatusExt;
use std::process::Command;

use anyhow::Context as _;
use buck2_miniperf_proto::MiniperfCounter;
use buck2_miniperf_proto::MiniperfCounters;
use buck2_miniperf_proto::MiniperfOutput;
use perf_event::events::Hardware;
use perf_event::Builder;
use smallvec::SmallVec;

/// First argument is an output path to write output data into. The rest is the command to execute.
pub fn main() -> anyhow::Result<()> {
    let mut args = env::args_os();
    args.next().context("No argv0")?;

    // In an ideal world, we would like this to be a pipe. Unfortunately, we can't do that, because
    // to get a pipe here, we'd have to have it not be CLOEXEC (because we need to exec *this*
    // binary). However, we're spawned by a server that creates many such processes concurrently,
    // so that means CLOEXEC must be set when creating the pipe, then unset between fork and exec.
    // To do this while retaining posix_spawn (which is quite a bit faster than fork + exec), we
    // need to dup the FD (which clears CLOEXEC), but the Rust wrapper around posix_spawn
    // (`Command`) does not expose that.
    let out = args.next().context("No output path")?;

    // NOTE: Kernel is not enabled here: we want to report only userspace cycles.
    let mut user_counter = Builder::new()
        .kind(Hardware::INSTRUCTIONS)
        .inherit(true)
        .enable_on_exec()
        .build()?;

    let mut kernel_counter = Builder::new()
        .kind(Hardware::INSTRUCTIONS)
        .include_kernel()
        .exclude_user()
        .inherit(true)
        .enable_on_exec()
        .build()?;

    let status = args.next().context("No process to run").and_then(|bin| {
        Command::new(bin)
            .args(args)
            .status()
            .map_err(anyhow::Error::from)
    });

    let user_value = user_counter
        .read_count_and_time()
        .context("Error reading user_counter")?;

    let kernel_value = kernel_counter
        .read_count_and_time()
        .context("Error reading kernel_counter")?;

    let output = MiniperfOutput {
        raw_exit_code: status.map(|s| s.into_raw()).map_err(|e| e.to_string()),
        counters: Ok(MiniperfCounters {
            user_instructions: MiniperfCounter {
                count: user_value.count,
                time_enabled: user_value.time_enabled,
                time_running: user_value.time_running,
            },
            kernel_instructions: MiniperfCounter {
                count: kernel_value.count,
                time_enabled: kernel_value.time_enabled,
                time_running: kernel_value.time_running,
            },
        }),
    };

    // Stack allocate in the happy path.
    let mut buff = SmallVec::<[u8; MiniperfOutput::EXPECTED_SIZE]>::new();

    bincode::serialize_into(&mut buff, &output)
        .with_context(|| format!("Failed to write to `{:?}`", out))?;

    std::fs::write(&out, &buff).with_context(|| format!("Failed to write to `{:?}`", out))?;

    Ok(())
}
