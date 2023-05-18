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
use buck2_wrapper_common::kill;
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
    match tokio::time::timeout(KILL_REQUEST_TIMEOUT, request_fut).await {
        Ok(inner_result) => {
            match inner_result {
                Ok(_) => loop {
                    if !kill::process_exists(pid)? {
                        return Ok(());
                    }
                    if time_req_sent.elapsed() > GRACEFUL_SHUTDOWN_TIMEOUT {
                        crate::eprintln!("Timed out waiting for graceful shutdown")?;
                        break;
                    }
                    tokio::time::sleep(Duration::from_millis(100)).await;
                },
                Err(e) => {
                    // The kill request can fail if the server is in a bad state and we cannot
                    // authenticate to it.
                    crate::eprintln!("Error requesting graceful shutdown: {}", e)?;
                }
            }
        }
        Err(e) => {
            let _assert_type: tokio::time::error::Elapsed = e;
            crate::eprintln!("Timed out requesting graceful shutdown")?;
        }
    };
    kill::kill(pid)?;
    while time_req_sent.elapsed() < time_to_kill {
        if !kill::process_exists(pid)? {
            return Ok(());
        }
        tokio::time::sleep(Duration::from_millis(100)).await;
    }
    Err(anyhow::anyhow!(
        "Daemon did not terminate after forceful termination"
    ))
}

#[cfg(unix)]
mod os_specific {

    use std::time::Duration;

    use sysinfo::Process;
    use sysinfo::ProcessExt;

    pub(super) fn process_creation_time(process: &Process) -> Option<Duration> {
        // Returns process creation time with 1 second precision.
        Some(Duration::from_secs(process.start_time()))
    }
}

#[cfg(windows)]
mod os_specific {

    use std::time::Duration;

    use buck2_wrapper_common::winapi_handle::WinapiHandle;
    use sysinfo::PidExt;
    use sysinfo::Process;
    use sysinfo::ProcessExt;
    use winapi::shared::minwindef::FILETIME;
    use winapi::um::processthreadsapi::GetProcessTimes;
    use winapi::um::processthreadsapi::OpenProcess;
    use winapi::um::winnt::PROCESS_QUERY_INFORMATION;

    pub(super) fn process_creation_time(process: &Process) -> Option<Duration> {
        // Returns process creation time with 100 ns precision.
        let proc_handle =
            unsafe { OpenProcess(PROCESS_QUERY_INFORMATION, 0, process.pid().as_u32()) };
        if proc_handle.is_null() {
            return None;
        }
        let proc_handle = unsafe { WinapiHandle::new(proc_handle) };
        let mut creation_time: FILETIME = unsafe { std::mem::zeroed() };
        let mut exit_time: FILETIME = unsafe { std::mem::zeroed() };
        let mut kernel_time: FILETIME = unsafe { std::mem::zeroed() };
        let mut user_time: FILETIME = unsafe { std::mem::zeroed() };

        let result = unsafe {
            GetProcessTimes(
                proc_handle.handle(),
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
