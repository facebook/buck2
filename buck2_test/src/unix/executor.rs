/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io;
use std::os::unix::io::AsRawFd as _;
use std::process::Stdio;

use anyhow::Context as _;
use buck2_core::process::async_background_command;
use tokio::net::UnixStream;
use tokio::process::Child;

pub(crate) async fn spawn(
    name: &str,
    tpx_args: Vec<String>,
) -> anyhow::Result<(Child, UnixStream, UnixStream)> {
    let (executor_client_async_io, executor_server_async_io) =
        UnixStream::pair().context("Failed to create executor channel")?;

    let (orchestrator_client_async_io, orchestrator_server_async_io) =
        UnixStream::pair().context("Failed to create orchestrator channel")?;

    let executor_client_io = executor_client_async_io;
    let executor_server_io = executor_server_async_io
        .into_std()
        .context("Failed to convert executor_server_io to std")?;
    let executor_server_fd = executor_server_io.as_raw_fd().to_string();

    let orchestrator_server_io = orchestrator_server_async_io;
    let orchestrator_client_io = orchestrator_client_async_io
        .into_std()
        .context("Failed to convert orchestrator_client_io to std")?;
    let orchestrator_client_fd = orchestrator_client_io.as_raw_fd().to_string();

    let mut command = async_background_command(name);
    command
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .arg("--executor-fd")
        .arg(executor_server_fd)
        .arg("--orchestrator-fd")
        .arg(orchestrator_client_fd)
        .arg("--")
        .args(tpx_args);

    let fds = [
        executor_server_io.as_raw_fd(),
        orchestrator_client_io.as_raw_fd(),
    ];

    unsafe {
        command.pre_exec(move || {
            // Clear CLOEXEC on the 2 FDs we intend to pass. It's better to set CLOEXEC and
            // clear it specifically here since that ensure that if we spawn anything else
            // concurrently, those FDs will be released on exec.

            for fd in &fds {
                let flags = libc::fcntl(*fd, libc::F_GETFD);
                if flags < 0 {
                    return Err(io::Error::last_os_error());
                }

                if libc::fcntl(*fd, libc::F_SETFD, flags & !libc::FD_CLOEXEC) < 0 {
                    return Err(io::Error::last_os_error());
                }
            }

            Ok(())
        });
    }

    let proc = command
        .spawn()
        .with_context(|| format!("Failed to start {} for OutOfProcessTestExecutor", name))?;

    Ok((proc, executor_client_io, orchestrator_server_io))
}
