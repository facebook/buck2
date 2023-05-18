/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;
use std::time::Instant;

use buck2_cli_proto::daemon_api_client::*;
use buck2_cli_proto::*;
use sysinfo::Pid;
use sysinfo::PidExt;
use sysinfo::ProcessExt;
use sysinfo::ProcessRefreshKind;
use sysinfo::System;
use sysinfo::SystemExt;
use tonic::codegen::InterceptedService;
use tonic::transport::Channel;
use tonic::Request;

use crate::daemon::client::connect::BuckAddAuthTokenInterceptor;

const GRACEFUL_SHUTDOWN_TIMEOUT: Duration = Duration::from_secs(4);
/// Kill request does not wait for the process to exit.
const KILL_REQUEST_TIMEOUT: Duration = Duration::from_secs(3);
const FORCE_SHUTDOWN_TIMEOUT: Duration = Duration::from_secs(2);

enum KillBehavior {
    WaitForExit,
    TerminateFirst,
}

pub async fn kill(
    client: &mut DaemonApiClient<InterceptedService<Channel, BuckAddAuthTokenInterceptor>>,
    info: &DaemonProcessInfo,
    reason: &str,
) -> anyhow::Result<()> {
    let pid = info.pid;
    let callers = get_callers_for_kill();

    let request_fut = client.kill(Request::new(KillRequest {
        reason: reason.to_owned(),
        timeout: Some(GRACEFUL_SHUTDOWN_TIMEOUT.try_into()?),
        callers,
    }));
    let time_to_kill = GRACEFUL_SHUTDOWN_TIMEOUT + FORCE_SHUTDOWN_TIMEOUT;
    let time_req_sent = Instant::now();
    // First we send a Kill request
    let kill_behavior = match tokio::time::timeout(KILL_REQUEST_TIMEOUT, request_fut).await {
        Ok(inner_result) => {
            match inner_result {
                Ok(_) => KillBehavior::WaitForExit,
                Err(e) => {
                    // The kill request can fail if the server is in a bad state and we cannot
                    // authenticate to it.
                    crate::eprintln!("Error requesting graceful shutdown: {}", e)?;
                    // Try an OS-level terminate next.
                    KillBehavior::TerminateFirst
                }
            }
        }
        Err(_) => KillBehavior::WaitForExit,
    };
    // Then we do a wait_for on the pid, and if that times out, we kill it harder
    tokio::task::spawn_blocking(move || {
        os_specific::kill_impl(
            pid,
            kill_behavior,
            time_to_kill.saturating_sub(time_req_sent.elapsed()),
        )
    })
    .await?
}

#[cfg(unix)]
mod os_specific {
    use std::thread;
    use std::time::Duration;
    use std::time::Instant;

    use anyhow::Context as _;
    use nix::sys::signal::Signal;
    use sysinfo::Process;
    use sysinfo::ProcessExt;

    use super::KillBehavior;

    fn process_exists(pid: nix::unistd::Pid) -> anyhow::Result<bool> {
        match nix::sys::signal::kill(pid, None) {
            Ok(_) => Ok(true),
            Err(nix::errno::Errno::ESRCH) => Ok(false),
            Err(e) => Err(e)
                .with_context(|| format!("unexpected error checking if process {} exists", pid)),
        }
    }

    pub(super) fn kill_impl(
        pid: i64,
        behavior: KillBehavior,
        timeout: Duration,
    ) -> anyhow::Result<()> {
        let daemon_pid = nix::unistd::Pid::from_raw(
            pid.try_into()
                .with_context(|| format!("Integer overflow converting pid {} to pid_t", pid))?,
        );
        enum WaitFor {
            Exited,
            WaitTimedOut,
        }
        fn wait_for(pid: nix::unistd::Pid, timeout: Duration) -> anyhow::Result<WaitFor> {
            let start = Instant::now();
            while start.elapsed() < timeout {
                if !process_exists(pid)? {
                    return Ok(WaitFor::Exited);
                }
                thread::sleep(Duration::from_millis(100));
            }
            Ok(WaitFor::WaitTimedOut)
        }

        match behavior {
            KillBehavior::TerminateFirst => {
                crate::eprintln!("Sending SIGTERM.")?;
                // We send SIGKILL below even if this fails.
                let _ignored = nix::sys::signal::kill(daemon_pid, Signal::SIGTERM);
            }
            KillBehavior::WaitForExit => {}
        };

        match wait_for(daemon_pid, timeout)? {
            WaitFor::Exited => Ok(()),
            WaitFor::WaitTimedOut => {
                match nix::sys::signal::kill(daemon_pid, Signal::SIGKILL) {
                    Ok(()) => {
                        crate::eprintln!("Graceful shutdown timed out. Sending SIGKILL.")?;
                    }
                    Err(nix::errno::Errno::ESRCH) => return Ok(()),
                    Err(e) => return Err(e).context("Failed to kill daemon"),
                };

                let start = Instant::now();
                loop {
                    if !process_exists(daemon_pid)? {
                        return Ok(());
                    }
                    if start.elapsed() > timeout {
                        // Wait may not terminate for various reasons for example:
                        // - Another process with the same pid was started
                        // - Debugger is attached to the process
                        return Err(anyhow::anyhow!(
                            "Daemon {} did not exit after SIGKILL within timeout",
                            daemon_pid
                        ));
                    }
                    thread::sleep(Duration::from_millis(100));
                }
            }
        }
    }

    pub(super) fn process_creation_time(process: &Process) -> Option<Duration> {
        // Returns process creation time with 1 second precision.
        Some(Duration::from_secs(process.start_time()))
    }
}

#[cfg(windows)]
mod os_specific {
    use std::time::Duration;

    use anyhow::Context as _;
    use sysinfo::PidExt;
    use sysinfo::Process;
    use sysinfo::ProcessExt;
    use winapi::shared::minwindef::FILETIME;
    use winapi::shared::winerror::WAIT_TIMEOUT;
    use winapi::um::handleapi::CloseHandle;
    use winapi::um::processthreadsapi::GetProcessTimes;
    use winapi::um::processthreadsapi::OpenProcess;
    use winapi::um::processthreadsapi::TerminateProcess;
    use winapi::um::synchapi::WaitForSingleObject;
    use winapi::um::winbase::WAIT_OBJECT_0;
    use winapi::um::winnt::HANDLE;
    use winapi::um::winnt::PROCESS_QUERY_INFORMATION;
    use winapi::um::winnt::PROCESS_TERMINATE;
    use winapi::um::winnt::SYNCHRONIZE;

    use super::KillBehavior;

    struct HandleWrapper {
        handle: HANDLE,
    }
    impl Drop for HandleWrapper {
        fn drop(&mut self) {
            unsafe { CloseHandle(self.handle) };
        }
    }

    pub(super) fn kill_impl(
        pid: i64,
        behavior: KillBehavior,
        timeout: Duration,
    ) -> anyhow::Result<()> {
        let daemon_pid: u32 = pid
            .try_into()
            .with_context(|| format!("Integer overflow converting pid {} to u32", pid))?;
        let proc_handle = unsafe { OpenProcess(SYNCHRONIZE | PROCESS_TERMINATE, 0, daemon_pid) };
        // If proc_handle is null, process died already.
        if proc_handle.is_null() {
            return Ok(());
        }
        let proc_handle = HandleWrapper {
            handle: proc_handle,
        };
        let wait_result = match behavior {
            KillBehavior::WaitForExit => unsafe {
                WaitForSingleObject(proc_handle.handle, timeout.as_millis().try_into()?)
            },
            KillBehavior::TerminateFirst => {
                // Don't wait for the process to die first if we were asked to just terminate it,
                // fall through directly to TerminateProcess instead.
                WAIT_TIMEOUT
            }
        };
        match wait_result {
            WAIT_OBJECT_0 => Ok(()), // process exited successfully
            WAIT_TIMEOUT => {
                // If process isn't signalled, terminate it forcefully.
                match unsafe { TerminateProcess(proc_handle.handle, 1) } {
                    0 => Err(anyhow::anyhow!("Failed to kill daemon ({})", daemon_pid)),
                    _ => Ok(()),
                }
            }
            error_code => Err(anyhow::anyhow!(
                "Waiting for daemon process failed. Error code: {:#x}",
                error_code
            )),
        }
    }

    pub(super) fn process_creation_time(process: &Process) -> Option<Duration> {
        // Returns process creation time with 100 ns precision.
        let proc_handle =
            unsafe { OpenProcess(PROCESS_QUERY_INFORMATION, 0, process.pid().as_u32()) };
        if proc_handle.is_null() {
            return None;
        }
        let proc_handle = HandleWrapper {
            handle: proc_handle,
        };
        let mut creation_time: FILETIME = unsafe { std::mem::zeroed() };
        let mut exit_time: FILETIME = unsafe { std::mem::zeroed() };
        let mut kernel_time: FILETIME = unsafe { std::mem::zeroed() };
        let mut user_time: FILETIME = unsafe { std::mem::zeroed() };

        let result = unsafe {
            GetProcessTimes(
                proc_handle.handle,
                &mut creation_time,
                &mut exit_time,
                &mut kernel_time,
                &mut user_time,
            )
        };

        if result != 0 {
            // `creation_time` stores intervals of 100 ns, so multiply by 100 to obtain
            // proper nanoseconds. The u64 type will overflow around the year 2185.
            let intervals = ((creation_time.dwHighDateTime as u64) << 32)
                | (creation_time.dwLowDateTime as u64);
            Some(Duration::from_nanos(intervals * 100))
        } else {
            None
        }
    }
}

fn get_callers_for_kill() -> Vec<String> {
    /// Add a process to our parts and return its parent PID.
    fn push_process(
        pid: Pid,
        creation_time: Duration,
        system: &mut System,
        process_tree: &mut Vec<String>,
    ) -> Option<(Pid, Duration)> {
        // Specifics about this process need to be refreshed by this time.
        let proc = system.process(pid)?;
        let title = shlex::join(proc.cmd().iter().map(|s| s.as_str()));
        process_tree.push(title);
        let parent_pid = proc.parent()?;
        system.refresh_process_specifics(parent_pid, ProcessRefreshKind::new());
        let parent_proc = system.process(parent_pid)?;
        let parent_creation_time = os_specific::process_creation_time(parent_proc)?;
        if parent_creation_time <= creation_time {
            Some((parent_pid, parent_creation_time))
        } else {
            None
        }
    }

    let mut system = System::new();
    let mut process_tree = Vec::new();

    let pid = Pid::from_u32(std::process::id());
    system.refresh_process_specifics(pid, ProcessRefreshKind::new());
    let mut curr = system
        .process(pid)
        .and_then(|proc| Some((pid, os_specific::process_creation_time(proc)?)));
    while let Some((pid, creation_time)) = curr {
        curr = push_process(pid, creation_time, &mut system, &mut process_tree);
    }

    process_tree.into_iter().rev().collect()
}
