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
use buck2_wrapper_common::pid::Pid;
use sysinfo::ProcessRefreshKind;
use sysinfo::System;
use tonic::codegen::InterceptedService;
use tonic::transport::Channel;
use tonic::Request;

use crate::daemon::client::connect::BuckAddAuthTokenInterceptor;

#[derive(Debug, buck2_error::Error)]
enum KillError {
    #[error("Daemon pid {} did not die after kill within {:.1}s (status: {})", _0, _1.as_secs_f32(), _2)]
    #[buck2(tag = DaemonWontDieFromKill)]
    DidNotDie(Pid, Duration, String),
}

const GRACEFUL_SHUTDOWN_TIMEOUT: Duration = Duration::from_secs(4);
/// Kill request does not wait for the process to exit.
const KILL_REQUEST_TIMEOUT: Duration = Duration::from_secs(3);
const FORCE_SHUTDOWN_TIMEOUT: Duration = Duration::from_secs(10);

pub(crate) async fn kill(
    client: &mut DaemonApiClient<InterceptedService<Channel, BuckAddAuthTokenInterceptor>>,
    info: &DaemonProcessInfo,
    reason: &str,
) -> anyhow::Result<()> {
    let pid = Pid::from_i64(info.pid)?;
    let callers = get_callers_for_kill();

    tracing::debug!("Killing daemon with PID {}", pid);

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
                        crate::eprintln!(
                            "Timed out waiting for graceful shutdown of buck2 daemon pid {}",
                            pid
                        )?;
                        break;
                    }
                    tokio::time::sleep(Duration::from_millis(100)).await;
                },
                Err(e) => {
                    // The kill request can fail if the server is in a bad state and we cannot
                    // authenticate to it.
                    crate::eprintln!(
                        "Error requesting graceful shutdown of buck2 daemon pid {}: {}",
                        pid,
                        e
                    )?;
                }
            }
        }
        Err(e) => {
            let _assert_type: tokio::time::error::Elapsed = e;
            crate::eprintln!(
                "Timed out requesting graceful shutdown of buck2 daemon pid {}",
                pid
            )?;
        }
    };

    hard_kill_impl(pid, time_req_sent, time_to_kill).await
}

pub(crate) async fn hard_kill(info: &DaemonProcessInfo) -> anyhow::Result<()> {
    let pid = Pid::from_i64(info.pid)?;

    hard_kill_impl(pid, Instant::now(), FORCE_SHUTDOWN_TIMEOUT).await
}

pub(crate) async fn hard_kill_until(
    info: &DaemonProcessInfo,
    deadline: Instant,
) -> anyhow::Result<()> {
    let pid = Pid::from_i64(info.pid)?;

    let now = Instant::now();
    hard_kill_impl(pid, now, deadline.saturating_duration_since(now)).await
}

async fn hard_kill_impl(pid: Pid, start_at: Instant, deadline: Duration) -> anyhow::Result<()> {
    tracing::info!(
        "Killing PID {} with status {}",
        pid,
        kill::get_sysinfo_status(pid)
            .map(|s| s.to_string())
            .as_deref()
            .unwrap_or("<unknown>")
    );

    let Some(handle) = kill::kill(pid)? else {
        return Ok(());
    };
    let timestamp_after_kill = Instant::now();
    while start_at.elapsed() < deadline {
        if handle.has_exited()? {
            return Ok(());
        }
        tokio::time::sleep(Duration::from_millis(100)).await;
    }

    // Last chance: we do logging this time.
    let status = kill::get_sysinfo_status(pid).map(|s| s.to_string());
    let status = status.unwrap_or_else(|| "<unknown>".to_owned());
    if handle.has_exited()? {
        return Ok(());
    }

    Err(KillError::DidNotDie(pid, timestamp_after_kill.elapsed(), status).into())
}

fn get_callers_for_kill() -> Vec<String> {
    /// Add a process to our parts and return its parent PID.
    fn push_process(
        pid: sysinfo::Pid,
        creation_time: Duration,
        system: &mut System,
        process_tree: &mut Vec<String>,
    ) -> Option<(sysinfo::Pid, Duration)> {
        // Specifics about this process need to be refreshed by this time.
        let proc = system.process(pid)?;
        let title =
            shlex::try_join(proc.cmd().iter().map(|s| s.as_str())).expect("Null byte unexpected");
        process_tree.push(title);
        let parent_pid = proc.parent()?;
        system.refresh_process_specifics(parent_pid, ProcessRefreshKind::new());
        let parent_proc = system.process(parent_pid)?;
        let parent_creation_time = kill::process_creation_time(parent_proc)?;
        if parent_creation_time <= creation_time {
            Some((parent_pid, parent_creation_time))
        } else {
            None
        }
    }

    let mut system = System::new();
    let mut process_tree = Vec::new();

    let pid = sysinfo::Pid::from_u32(std::process::id());
    system.refresh_process_specifics(pid, ProcessRefreshKind::new());
    let mut curr = system
        .process(pid)
        .and_then(|proc| Some((pid, kill::process_creation_time(proc)?)));
    while let Some((pid, creation_time)) = curr {
        curr = push_process(pid, creation_time, &mut system, &mut process_tree);
    }

    process_tree.into_iter().rev().collect()
}
