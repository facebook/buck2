/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::time::Duration;
use std::time::Instant;

use buck2_cli_proto::daemon_api_client::*;
use buck2_cli_proto::*;
use buck2_data::error::ErrorTag;
use buck2_error::buck2_error;
use buck2_wrapper_common::kill;
use buck2_wrapper_common::pid::Pid;
use sysinfo::ProcessRefreshKind;
use sysinfo::ProcessesToUpdate;
use sysinfo::System;
use tonic::Request;
use tonic::codegen::InterceptedService;
use tonic::transport::Channel;

use crate::daemon::client::BuckdLifecycleLock;
use crate::daemon::client::connect::BuckAddAuthTokenInterceptor;
use crate::daemon::client::connect::BuckdProcessInfo;
use crate::daemon::client::connect::buckd_startup_timeout;
use crate::startup_deadline::StartupDeadline;

const GRACEFUL_SHUTDOWN_TIMEOUT: Duration = Duration::from_secs(4);
/// Kill request does not wait for the process to exit.
const KILL_REQUEST_TIMEOUT: Duration = Duration::from_secs(3);
const FORCE_SHUTDOWN_TIMEOUT: Duration = Duration::from_secs(10);

pub async fn kill_command_impl(
    lifecycle_lock: &BuckdLifecycleLock,
    reason: &str,
) -> buck2_error::Result<()> {
    let process = match BuckdProcessInfo::load(lifecycle_lock.daemon_dir()) {
        Ok(p) => p,
        Err(e) => {
            tracing::debug!("No BuckdProcessInfo: {:#}", e);
            crate::eprintln!("no buckd server running")?;
            return Ok(());
        }
    };

    let buckd = tokio::time::timeout(buckd_startup_timeout()?, async {
        process.create_channel().await?.upgrade().await
    })
    .await;

    let pid = match buckd {
        Ok(Ok(mut buckd)) => {
            crate::eprintln!("killing buckd server")?;
            Some(buckd.kill(reason).await?)
        }
        Ok(Err(e)) => {
            // No time out: we just errored out. This is likely indicative that there is no
            // buckd (i.e. our connection got rejected), so let's check for this and then
            // provide some information.
            let e = e;

            // TODO(minglunli): Look into checking for explicit 'Connection Refused' or something more
            // concretely pointing to `no server running` instead of all transport errors
            if e.has_tag(ErrorTag::ServerTransportError) {
                // OK, looks like the server
                tracing::debug!("Connect failed with a Tonic error: {:#}", e);
                crate::eprintln!("no buckd server running")?;
            } else {
                crate::eprintln!(
                    "unexpected error connecting to Buck2: {:#} \
                            (no buckd server running?)",
                    e
                )?;
            }

            None
        }
        Err(e) => {
            tracing::debug!("Connect timed out: {:#}", e);

            // If we timeout, then considering the generous timeout we give ourselves, then
            // that must mean we're not getting a reply back from Buck, but that we did
            // succeed in opening a connection to it (because if we didn't, we'd have
            // errored out).
            //
            // This means the socket is probably open. We can reasonably got and kill this
            // process if both the PID and the port exist.
            crate::eprintln!("killing unresponsive buckd server")?;
            process.hard_kill().await?;
            Some(process.pid()?)
        }
    };

    if let Some(pid) = pid {
        crate::eprintln!("Buck2 daemon pid {} has exited", pid)?;
    }

    Ok(())
}

pub(crate) async fn kill(
    client: &mut DaemonApiClient<InterceptedService<Channel, BuckAddAuthTokenInterceptor>>,
    info: &DaemonProcessInfo,
    reason: &str,
) -> buck2_error::Result<()> {
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
                    if Instant::now() - time_req_sent > GRACEFUL_SHUTDOWN_TIMEOUT {
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

pub(crate) async fn hard_kill(info: &DaemonProcessInfo) -> buck2_error::Result<()> {
    let pid = Pid::from_i64(info.pid)?;

    hard_kill_impl(pid, Instant::now(), FORCE_SHUTDOWN_TIMEOUT).await
}

pub(crate) async fn hard_kill_until(
    info: &DaemonProcessInfo,
    deadline: &StartupDeadline,
) -> buck2_error::Result<()> {
    let pid = Pid::from_i64(info.pid)?;

    let deadline = deadline.down_deadline()?.deadline();

    let now = Instant::now();
    hard_kill_impl(pid, now, deadline.saturating_duration_since(now)).await
}

async fn hard_kill_impl(
    pid: Pid,
    start_at: Instant,
    deadline: Duration,
) -> buck2_error::Result<()> {
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
    while Instant::now() - start_at < deadline {
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

    let elapsed_s = (Instant::now() - timestamp_after_kill).as_secs_f32();
    Err(buck2_error!(
        ErrorTag::DaemonWontDieFromKill,
        "Daemon pid {pid} did not die after kill within {elapsed_s:.1}s (status: {status})"
    ))
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
        let title = shlex::try_join(proc.cmd().iter().filter_map(|s| s.to_str()))
            .expect("Null byte unexpected");
        process_tree.push(title);
        let parent_pid = proc.parent()?;
        system.refresh_processes_specifics(
            ProcessesToUpdate::Some(&[parent_pid]),
            true,
            ProcessRefreshKind::nothing(),
        );
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
    system.refresh_processes_specifics(
        ProcessesToUpdate::Some(&[pid]),
        true,
        ProcessRefreshKind::nothing(),
    );
    let mut curr = system
        .process(pid)
        .and_then(|proc| Some((pid, kill::process_creation_time(proc)?)));
    while let Some((pid, creation_time)) = curr {
        curr = push_process(pid, creation_time, &mut system, &mut process_tree);
    }

    process_tree.into_iter().rev().collect()
}
