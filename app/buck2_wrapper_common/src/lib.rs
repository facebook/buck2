/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#![feature(error_generic_member_access)]

//! Code shared between `buck2_wrapper` and `buck2`.
//!
//! Careful! The wrapper is not released as part of the regular buck version bumps,
//! meaning code changes here are not "atomically" updated.

#![feature(once_cell_try)]

use std::collections::HashSet;
use std::thread;
use std::time::Duration;
use std::time::Instant;

use is_buck2::WhoIsAsking;
use sysinfo::ProcessesToUpdate;
use sysinfo::System;

use crate::is_buck2::is_buck2_exe;
use crate::pid::Pid;

pub mod invocation_id;
pub mod is_buck2;
pub mod kill;
pub mod pid;
#[cfg(unix)]
mod unix;
#[cfg(windows)]
pub mod win;

pub const BUCK2_WRAPPER_ENV_VAR: &str = "BUCK2_WRAPPER";
pub const BUCK_WRAPPER_UUID_ENV_VAR: &str = "BUCK_WRAPPER_UUID";
pub const BUCK_WRAPPER_START_TIME_ENV_VAR: &str = "BUCK_WRAPPER_START_TIME";
pub const EXPERIMENTS_FILENAME: &str = "experiments_from_buck_start";
pub const DOT_BUCKCONFIG_D: &str = ".buckconfig.d";

/// Because `sysinfo::Process` is not `Clone`.
struct ProcessInfo {
    pid: Pid,
    name: String,
    cmd: Vec<String>,
}

/// Get the list of all PIDs on Linux
///
/// As of sysinfo 0.30, the `processes` function returns all posix TIDs (what the kernel calls
/// PIDs), and not just all posix PIDs (what the kernel calls TGIDs). In order to make sure that we
/// don't kill any of the TIDs in our PID, we need to filter the list of TIDs down. This function
/// returns the list of all PIDs on the system.
fn get_all_tgids_linux() -> Option<HashSet<sysinfo::Pid>> {
    if !cfg!(target_os = "linux") {
        return None;
    }

    let Ok(entries) = std::fs::read_dir("/proc") else {
        return Some(HashSet::new());
    };

    let mut all_tgids = HashSet::new();

    for e in entries {
        let Ok(e) = e else {
            continue;
        };
        let Ok(file_type) = e.file_type() else {
            continue;
        };
        if !file_type.is_dir() {
            continue;
        }
        let Some(pid) = e.file_name().to_str().and_then(|s| s.parse::<u32>().ok()) else {
            continue;
        };
        all_tgids.insert(sysinfo::Pid::from_u32(pid));
    }

    Some(all_tgids)
}

/// Find all buck2 processes in the system.
fn find_buck2_processes(who_is_asking: WhoIsAsking) -> Vec<ProcessInfo> {
    let mut system = System::new();
    system.refresh_processes(ProcessesToUpdate::All, true);

    let mut current_parents = HashSet::new();
    let mut parent = Some(sysinfo::Pid::from_u32(std::process::id()));
    while let Some(pid) = parent {
        // There is a small chance on Windows that the PID of a dead parent
        // was reused by some of its descendants, and this can create a loop.
        if !current_parents.insert(pid) {
            break;
        }
        parent = system.process(pid).and_then(|p| p.parent());
    }

    let filtered_proc_list = get_all_tgids_linux();

    let mut buck2_processes = Vec::new();
    for (pid, process) in system.processes() {
        // See comment on `get_all_tgids_linux`
        if let Some(filtered_proc_list) = filtered_proc_list.as_ref() {
            if !filtered_proc_list.contains(pid) {
                continue;
            }
        }
        let Some(exe) = process.exe() else {
            continue;
        };
        if is_buck2_exe(exe, who_is_asking) && !current_parents.contains(pid) {
            let Ok(pid) = Pid::from_u32(pid.as_u32()) else {
                continue;
            };
            buck2_processes.push(ProcessInfo {
                pid,
                name: process.name().to_string_lossy().into_owned(),
                cmd: process
                    .cmd()
                    .iter()
                    .map(|s| s.to_string_lossy().into_owned())
                    .collect(),
            });
        }
    }

    buck2_processes
}

/// Kills all running Buck2 processes, except this process's hierarchy. Returns whether it
/// succeeded without errors.
pub fn killall(who_is_asking: WhoIsAsking, write: impl Fn(String)) -> bool {
    let buck2_processes = find_buck2_processes(who_is_asking);

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
            let cmd = shlex::try_join(process.cmd.iter().map(|s| s.as_str()))
                .expect("Null byte unexpected");
            format!("{} {} ({}). {}", status, process.name, process.pid, cmd,)
        }

        fn failed_to_kill(&mut self, process: &ProcessInfo, error: buck2_error::Error) {
            let mut message = self.fmt_status(process, "Failed to kill");
            for line in format!("{error:?}").lines() {
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
            Ok(Some(handle)) => processes_still_alive.push((process, handle)),
            Ok(None) => {}
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

        if Instant::now() - start > Duration::from_secs(timeout_secs) {
            for process in processes_still_alive {
                printer.failed_to_kill(
                    &process.0,
                    buck2_error::buck2_error!(
                        buck2_error::ErrorTag::DaemonWontDieFromKill,
                        "Process still alive after {timeout_secs}s after kill sent"
                    ),
                );
            }
            break;
        }

        thread::sleep(Duration::from_millis(100));
    }

    printer.ok
}
