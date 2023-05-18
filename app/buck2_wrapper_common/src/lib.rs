/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Code shared between `buck2_wrapper` and `buck2`.
//!
//! Careful! The wrapper is not released as part of the regular buck version bumps,
//! meaning code changes here are not "atomically" updated.

use std::collections::HashSet;
use std::thread;
use std::time::Duration;
use std::time::Instant;

use anyhow::Context;
use sysinfo::Pid;
use sysinfo::PidExt;
use sysinfo::Process;
use sysinfo::ProcessExt;
use sysinfo::System;
use sysinfo::SystemExt;
pub mod invocation_id;
pub mod kill;
pub mod winapi_handle;

pub const BUCK2_WRAPPER_ENV_VAR: &str = "BUCK2_WRAPPER";
pub const BUCK_WRAPPER_UUID_ENV_VAR: &str = "BUCK_WRAPPER_UUID";

/// Kills all running Buck2 processes, except this process's hierarchy. Returns whether it
/// succeeded without errors.
pub fn killall(write: impl Fn(String)) -> bool {
    let mut system = System::new();
    system.refresh_processes();

    let mut current_parents = HashSet::new();
    let mut parent = Some(Pid::from_u32(std::process::id()));
    while let Some(pid) = parent {
        // There is a small chance on Windows that the PID of a dead parent
        // was reused by some of its descendants, and this can create a loop.
        if !current_parents.insert(pid) {
            break;
        }
        parent = system.process(pid).and_then(|p| p.parent());
    }

    let mut buck2_processes = Vec::new();
    for (pid, process) in system.processes() {
        let exe_name = process.exe().file_stem().and_then(|s| s.to_str());
        if exe_name == Some("buck2") && !current_parents.contains(pid) {
            buck2_processes.push(process);
        }
    }

    if buck2_processes.is_empty() {
        write("No buck2 processes found".to_owned());
        return true;
    }

    let mut ok = true;

    for process in buck2_processes {
        fn kill(process: &Process) -> anyhow::Result<()> {
            let pid = process.pid();
            let pid = pid
                .as_u32()
                .try_into()
                .with_context(|| format!("Integer overflow converting {}", pid))?;
            kill::kill(pid)?;
            let start = Instant::now();
            // 5 seconds is not enough on macOS to shutdown forkserver.
            // We don't really need to wait for forkserver shutdown,
            // we care about buckd shutdown. But logic to distinguish
            // between forkserver and buckd would be too fragile.
            let timeout_secs = 10;
            while start.elapsed() < Duration::from_secs(timeout_secs) {
                if !kill::process_exists(pid)? {
                    return Ok(());
                }
                thread::sleep(Duration::from_millis(100));
            }
            Err(anyhow::anyhow!(
                "Process {pid} still exists after {timeout_secs}s after kill sent"
            ))
        }

        let result = kill(process);

        if result.is_err() {
            ok = false;
        }

        let status = if result.is_ok() {
            "Killed"
        } else {
            "Failed to kill"
        };

        let mut message = format!(
            "{} {} ({}). {}",
            status,
            process.name(),
            process.pid(),
            process.cmd().join(" "),
        );
        if let Err(error) = result {
            for line in format!("{:?}", error).lines() {
                message.push_str("\n  ");
                message.push_str(line);
            }
        }
        write(message);
    }

    ok
}
