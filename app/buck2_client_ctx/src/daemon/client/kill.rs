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

static GRACEFUL_SHUTDOWN_TIMEOUT: Duration = Duration::from_secs(4);
static FORCE_SHUTDOWN_TIMEOUT: Duration = Duration::from_secs(2);

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
    let kill_behavior = match tokio::time::timeout(time_to_kill, request_fut).await {
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
    kill_impl(
        pid,
        kill_behavior,
        time_to_kill.saturating_sub(time_req_sent.elapsed()),
    )
    .await
}

#[cfg(unix)]
async fn kill_impl(pid: i64, behavior: KillBehavior, timeout: Duration) -> anyhow::Result<()> {
    use anyhow::Context as _;
    use nix::sys::signal::Signal;

    let daemon_pid = nix::unistd::Pid::from_raw(pid as i32);
    enum WaitFor {
        Exited,
        WaitTimedOut,
        Err(anyhow::Error),
    }
    async fn wait_for(pid: nix::unistd::Pid, timeout: Duration) -> WaitFor {
        let start = Instant::now();
        while Instant::now() - start < timeout {
            match nix::sys::signal::kill(pid, None) {
                Ok(_) => {}
                Err(nix::errno::Errno::ESRCH) => {
                    return WaitFor::Exited;
                }
                Err(e) => {
                    return WaitFor::Err(anyhow::anyhow!(
                        "unexpected system error waiting for daemon to terminate (`{}`)",
                        e
                    ));
                }
            }
            tokio::time::sleep(Duration::from_millis(100)).await;
        }
        WaitFor::WaitTimedOut
    }

    match behavior {
        KillBehavior::TerminateFirst => {
            crate::eprintln!("Sending SIGTERM.")?;
            // We send SIGKILL below even if this fails.
            let _ignored = nix::sys::signal::kill(daemon_pid, Signal::SIGTERM);
        }
        KillBehavior::WaitForExit => {}
    };

    match wait_for(daemon_pid, timeout).await {
        WaitFor::Exited => Ok(()),
        WaitFor::Err(e) => Err(e),
        WaitFor::WaitTimedOut => {
            match nix::sys::signal::kill(daemon_pid, Signal::SIGKILL) {
                Ok(()) => {
                    crate::eprintln!("Graceful shutdown timed out. Sending SIGKILL.")?;
                }
                Err(nix::errno::Errno::ESRCH) => return Ok(()),
                Err(e) => return Err(e).context("Failed to kill daemon"),
            };

            loop {
                match nix::sys::signal::kill(daemon_pid, None) {
                    Ok(_) => {}
                    Err(nix::errno::Errno::ESRCH) => {
                        return Ok(());
                    }
                    Err(e) => {
                        return Err(anyhow::anyhow!(
                            "unexpected system error waiting for daemon to terminate (`{}`)",
                            e
                        ));
                    }
                }
                tokio::time::sleep(Duration::from_millis(100)).await;
            }
        }
    }
}

#[cfg(windows)]
async fn kill_impl(pid: i64, behavior: KillBehavior, timeout: Duration) -> anyhow::Result<()> {
    use winapi::shared::winerror::WAIT_TIMEOUT;
    use winapi::um::handleapi::CloseHandle;
    use winapi::um::processthreadsapi::OpenProcess;
    use winapi::um::processthreadsapi::TerminateProcess;
    use winapi::um::synchapi::WaitForSingleObject;
    use winapi::um::winbase::WAIT_OBJECT_0;
    use winapi::um::winnt::HANDLE;
    use winapi::um::winnt::PROCESS_TERMINATE;
    use winapi::um::winnt::SYNCHRONIZE;

    struct HandleWrapper {
        handle: HANDLE,
    }
    impl Drop for HandleWrapper {
        fn drop(&mut self) {
            unsafe { CloseHandle(self.handle) };
        }
    }

    let daemon_pid = pid as u32;
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

fn get_callers_for_kill() -> Vec<String> {
    /// Add a proess to our parts, and return its parent PID.
    fn push_process(pid: Pid, system: &mut System, process_tree: &mut Vec<String>) -> Option<Pid> {
        system.refresh_process_specifics(pid, ProcessRefreshKind::new());
        let proc = system.process(pid)?;
        let title = shlex::join(proc.cmd().iter().map(|s| s.as_str()));
        process_tree.push(title);
        proc.parent()
    }

    let mut system = System::new();
    let mut process_tree = Vec::new();

    let mut pid = Some(Pid::from_u32(std::process::id()));
    // FIXME: This while loop of going up the process tree occasionally gets stuck on Windows. Limit to 20 levels.
    let max_process_count = 20;
    while let Some(p) = pid {
        if process_tree.len() > max_process_count {
            break;
        }
        pid = push_process(p, &mut system, &mut process_tree);
    }

    process_tree.into_iter().rev().collect()
}
