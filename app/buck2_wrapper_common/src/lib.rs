/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Code shared between `buck2_wrapper` and `buck2`.
//!
//! Careful! The wrapper is not released as part of the regular buck version bumps,
//! meaning code changes here are not "atomically" updated.

#![feature(once_cell_try)]

use std::thread;
use std::time::Duration;
use std::time::Instant;

use buck2_hash::StdBuckHashSet;
use is_buck2::WhoIsAsking;
use sysinfo::ProcessRefreshKind;
use sysinfo::ProcessesToUpdate;
use sysinfo::System;
use sysinfo::UpdateKind;

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
    isolation_dir: Option<String>,
}

/// Extract the isolation dir from a buck2 process's command line.
///
/// buck2 daemons and clients are launched with the global `--isolation-dir <name>`
/// flag (see `buck2_client_ctx::daemon::client::connect`), accepting both the
/// `--isolation-dir <name>` and `--isolation-dir=<name>` forms. Returns `None` when
/// the flag is absent (e.g. the isolation dir was supplied via the
/// `BUCK_ISOLATION_DIR` env var, which does not appear in argv).
fn parse_isolation_dir(cmd: &[String]) -> Option<String> {
    let mut args = cmd.iter();
    while let Some(arg) = args.next() {
        if let Some(value) = arg.strip_prefix("--isolation-dir=") {
            return Some(value.to_owned());
        }
        if arg == "--isolation-dir" {
            return args.next().cloned();
        }
    }
    None
}

/// Get the list of all PIDs on Linux
///
/// As of sysinfo 0.30, the `processes` function returns all posix TIDs (what the kernel calls
/// PIDs), and not just all posix PIDs (what the kernel calls TGIDs). In order to make sure that we
/// don't kill any of the TIDs in our PID, we need to filter the list of TIDs down. This function
/// returns the list of all PIDs on the system.
fn get_all_tgids_linux() -> Option<StdBuckHashSet<sysinfo::Pid>> {
    if !cfg!(target_os = "linux") {
        return None;
    }

    let Ok(entries) = std::fs::read_dir("/proc") else {
        return Some(StdBuckHashSet::default());
    };

    let mut all_tgids = StdBuckHashSet::default();

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

/// This process and all of its ancestors. Requires the processes to be refreshed in `system`.
fn current_parents(system: &System) -> StdBuckHashSet<sysinfo::Pid> {
    let mut current_parents = StdBuckHashSet::default();
    let mut parent = Some(sysinfo::Pid::from_u32(std::process::id()));
    while let Some(pid) = parent {
        // There is a small chance on Windows that the PID of a dead parent
        // was reused by some of its descendants, and this can create a loop.
        if !current_parents.insert(pid) {
            break;
        }
        parent = system.process(pid).and_then(|p| p.parent());
    }
    current_parents
}

/// Whether `pid` is a buck2 process that [`killall`] would kill: a buck2 executable, and not an
/// ancestor of this process. `None` if the process exists but cannot be identified, in which case
/// callers must not assume either answer.
pub fn is_killable_buck2_process(pid: Pid, who_is_asking: WhoIsAsking) -> Option<bool> {
    let mut system = System::new();
    let linux_tgids =
        get_all_tgids_linux().map(|pids| pids.into_iter().collect::<Vec<sysinfo::Pid>>());
    let processes_to_update = match &linux_tgids {
        Some(pids) => ProcessesToUpdate::Some(pids),
        None => ProcessesToUpdate::All,
    };
    system.refresh_processes_specifics(
        processes_to_update,
        true,
        ProcessRefreshKind::nothing().with_exe(UpdateKind::Always),
    );

    let sys_pid = sysinfo::Pid::from_u32(pid.to_u32());
    let Some(process) = system.process(sys_pid) else {
        return Some(false);
    };
    let exe = process.exe()?;
    Some(is_buck2_exe(exe, who_is_asking) && !current_parents(&system).contains(&sys_pid))
}

/// Find all buck2 processes in the system.
fn find_buck2_processes(who_is_asking: WhoIsAsking) -> Vec<ProcessInfo> {
    let mut system = System::new();
    let linux_tgids =
        get_all_tgids_linux().map(|pids| pids.into_iter().collect::<Vec<sysinfo::Pid>>());
    let processes_to_update = match &linux_tgids {
        Some(pids) => ProcessesToUpdate::Some(pids),
        None => ProcessesToUpdate::All,
    };
    system.refresh_processes_specifics(
        processes_to_update,
        true,
        ProcessRefreshKind::nothing().with_exe(UpdateKind::Always),
    );

    let current_parents = current_parents(&system);

    let mut buck2_processes = Vec::new();
    for (sys_pid, process) in system.processes() {
        let Some(exe) = process.exe() else {
            continue;
        };
        if is_buck2_exe(exe, who_is_asking) && !current_parents.contains(sys_pid) {
            let Ok(pid) = Pid::from_u32(sys_pid.as_u32()) else {
                continue;
            };
            buck2_processes.push((
                *sys_pid,
                ProcessInfo {
                    pid,
                    name: process.name().to_string_lossy().into_owned(),
                    isolation_dir: None,
                },
            ));
        }
    }

    let matched_pids = buck2_processes
        .iter()
        .map(|(pid, _)| *pid)
        .collect::<Vec<_>>();
    if !matched_pids.is_empty() {
        system.refresh_processes_specifics(
            ProcessesToUpdate::Some(&matched_pids),
            true,
            ProcessRefreshKind::nothing().with_cmd(UpdateKind::Always),
        );
    }

    buck2_processes
        .into_iter()
        .map(|(pid, mut process_info)| {
            if let Some(process) = system.process(pid) {
                let cmd = process
                    .cmd()
                    .iter()
                    .map(|s| s.to_string_lossy().into_owned())
                    .collect::<Vec<_>>();
                process_info.isolation_dir = parse_isolation_dir(&cmd);
            }
            process_info
        })
        .collect()
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
            let isolation_dir = match &process.isolation_dir {
                Some(dir) => format!(" ({dir})"),
                None => String::new(),
            };
            format!(
                "{} {} ({}){}",
                status, process.name, process.pid, isolation_dir
            )
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_isolation_dir() {
        for (args, expected) in [
            (
                &["buck2", "--isolation-dir", "custom", "daemon"][..],
                Some("custom"),
            ),
            (
                &["buck2", "--isolation-dir=custom", "daemon"][..],
                Some("custom"),
            ),
            (&["buck2", "daemon"][..], None),
            (&["buck2", "--isolation-dir"][..], None),
        ] {
            let args = args.iter().map(|arg| (*arg).to_owned()).collect::<Vec<_>>();
            assert_eq!(expected.map(str::to_owned), parse_isolation_dir(&args));
        }
    }

    #[test]
    fn test_is_killable_buck2_process_rejects_non_buck2_process() {
        let mut command = if !cfg!(windows) {
            let mut command = buck2_util::process::background_command("sh");
            command.args(["-c", "sleep 10000"]);
            command
        } else {
            let mut command = buck2_util::process::background_command("powershell");
            command.args(["-c", "Start-Sleep -Seconds 10000"]);
            command
        };
        let mut child = command.spawn().unwrap();
        let pid = Pid::from_u32(child.id()).unwrap();

        assert_eq!(
            Some(false),
            is_killable_buck2_process(pid, WhoIsAsking::Buck2),
            "a non-buck2 process must not be killable; pid {pid}"
        );

        child.kill().unwrap();
        child.wait().unwrap();
    }

    #[test]
    fn test_is_killable_buck2_process_reports_exited_process() {
        let mut command = buck2_util::process::background_command("sh");
        command.args(["-c", "exit 0"]);
        let mut child = command.spawn().unwrap();
        let pid = Pid::from_u32(child.id()).unwrap();
        child.wait().unwrap();

        assert_eq!(
            Some(false),
            is_killable_buck2_process(pid, WhoIsAsking::Buck2),
            "an exited process must not be killable; pid {pid}"
        );
    }

    #[test]
    fn test_is_killable_buck2_process_rejects_own_ancestors() {
        // The test binary shares its file stem with the current exe, so it satisfies
        // `is_buck2_exe` and only the ancestor check can reject it.
        let pid = Pid::from_u32(std::process::id()).unwrap();
        assert_eq!(
            Some(false),
            is_killable_buck2_process(pid, WhoIsAsking::Buck2),
            "the current process must never be killable; pid {pid}"
        );
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn test_find_buck2_processes() {
        use std::fs;
        use std::path::PathBuf;
        use std::process::Child;
        use std::process::Stdio;
        use std::time::SystemTime;

        use buck2_util::process::background_command;

        struct ChildGuard {
            child: Child,
            temp_dir: PathBuf,
        }

        impl Drop for ChildGuard {
            fn drop(&mut self) {
                let _ignored = self.child.kill();
                let _ignored = self.child.wait();
                let _ignored = fs::remove_dir_all(&self.temp_dir);
            }
        }

        let nonce = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .expect("system clock should be after the Unix epoch")
            .as_nanos();
        let temp_dir = std::env::temp_dir().join(format!(
            "buck2_wrapper_common_process_scan_{}_{}",
            std::process::id(),
            nonce,
        ));
        fs::create_dir(&temp_dir).expect("temporary test directory should be created");
        let fake_buck2 = temp_dir.join("buck2");
        fs::copy("/bin/sh", &fake_buck2).expect("test buck2 executable should be copied");

        // Under OSS `cargo test` (all tests share one process, unlike buck's
        // per-test process isolation) a sibling test's `fork` can inherit the
        // write fd `fs::copy` briefly holds on `fake_buck2`, making `exec` fail
        // with ETXTBSY until that fd clears. Retry past the window.
        let mut child = None;
        for _ in 0..100 {
            match background_command(&fake_buck2)
                .args(["-c", "read _", "--isolation-dir=process-scan-test"])
                .stdin(Stdio::piped())
                .spawn()
            {
                Ok(spawned) => {
                    child = Some(spawned);
                    break;
                }
                Err(e) if e.raw_os_error() == Some(nix::libc::ETXTBSY) => {
                    thread::sleep(Duration::from_millis(10));
                }
                Err(e) => panic!("fake buck2 process should start: {e:?}"),
            }
        }
        let child = child.expect("fake buck2 process should start before ETXTBSY window closes");
        let child_pid = child.id();
        let _guard = ChildGuard { child, temp_dir };

        let processes = find_buck2_processes(WhoIsAsking::Buck2);
        let child_process = processes
            .iter()
            .find(|process| process.pid.to_u32() == child_pid)
            .expect("scan should find the fake buck2 child process");
        assert_eq!(
            Some("process-scan-test"),
            child_process.isolation_dir.as_deref(),
        );

        let current_pid = std::process::id();
        assert!(
            processes
                .iter()
                .all(|process| process.pid.to_u32() != current_pid),
            "scan should exclude the invoking process",
        );
    }
}
