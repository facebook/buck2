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

use sysinfo::Pid;
use sysinfo::PidExt;
use sysinfo::ProcessExt;
use sysinfo::System;
use sysinfo::SystemExt;
pub mod invocation_id;
pub mod kill;
pub mod winapi_handle;
pub(crate) mod winapi_process;

pub const BUCK2_WRAPPER_ENV_VAR: &str = "BUCK2_WRAPPER";
pub const BUCK_WRAPPER_UUID_ENV_VAR: &str = "BUCK_WRAPPER_UUID";

/// Because `sysinfo::Process` is not `Clone`.
struct ProcessInfo {
    pid: u32,
    name: String,
    cmd: Vec<String>,
}

/// Find all buck2 processes in the system.
fn find_buck2_processes() -> Vec<ProcessInfo> {
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
            buck2_processes.push(ProcessInfo {
                pid: pid.as_u32(),
                name: process.name().to_owned(),
                cmd: process.cmd().to_vec(),
            });
        }
    }

    buck2_processes
}

/// Kills all running Buck2 processes, except this process's hierarchy. Returns whether it
/// succeeded without errors.
pub fn killall(write: impl Fn(String)) -> bool {
    let buck2_processes = find_buck2_processes();

    if buck2_processes.is_empty() {
        write("No buck2 processes found".to_owned());
        return true;
    }

    struct Printer<F> {
        write: F,
        /// All processes were killed successfully.
        ok: bool,
    }

    impl<F: Fn(String)> Printer<F> {
        fn fmt_status(&mut self, process: &ProcessInfo, status: &str) -> String {
            format!(
                "{} {} ({}). {}",
                status,
                process.name,
                process.pid,
                process.cmd.join(" "),
            )
        }

        fn failed_to_kill(&mut self, process: &ProcessInfo, error: anyhow::Error) {
            let mut message = self.fmt_status(process, "Failed to kill");
            for line in format!("{:?}", error).lines() {
                message.push_str("\n  ");
                message.push_str(line);
            }
            (self.write)(message);

            self.ok = false;
        }

        fn killed(&mut self, process: &ProcessInfo) {
            let message = self.fmt_status(process, "Killed");
            (self.write)(message);
        }
    }

    let mut printer = Printer { write, ok: true };

    // Send a kill signal and collect the processes that are still alive.

    let mut processes_still_alive: Vec<(ProcessInfo, _)> = Vec::new();
    for process in buck2_processes {
        match kill::kill(process.pid) {
            Ok(handle) => processes_still_alive.push((process, handle)),
            Err(e) => printer.failed_to_kill(&process, e),
        };
    }

    // Wait for the processes to exit.

    let start = Instant::now();
    while !processes_still_alive.is_empty() {
        let timeout_secs = 10;

        processes_still_alive.retain(|(process, handle)| match handle.has_exited() {
            Err(e) => {
                printer.failed_to_kill(process, e);
                false
            }
            Ok(true) => {
                printer.killed(process);
                false
            }
            Ok(false) => true,
        });

        if start.elapsed() > Duration::from_secs(timeout_secs) {
            for process in processes_still_alive {
                printer.failed_to_kill(
                    &process.0,
                    anyhow::anyhow!("Process still alive after {timeout_secs}s after kill sent"),
                );
            }
            break;
        }

        thread::sleep(Duration::from_millis(100));
    }

    printer.ok
}
