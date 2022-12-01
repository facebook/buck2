/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ffi::OsStr;
use std::io;
use std::os::unix::io::AsRawFd;
use std::os::unix::process::CommandExt;
use std::process::Stdio;

use anyhow::Context;
use buck2_core::process::background_command;
use tokio::net::UnixStream;

use crate::client::ForkserverClient;

pub async fn launch_forkserver(
    exe: impl AsRef<OsStr>,
    args: impl IntoIterator<Item = impl AsRef<OsStr>>,
) -> anyhow::Result<ForkserverClient> {
    let (client_io, server_io) =
        UnixStream::pair().context("Failed to create fork server channel")?;

    let server_io = server_io
        .into_std()
        .context("Failed to convert server_io to std")?;

    let exe = exe.as_ref();

    let mut command = background_command(exe);
    command
        .stdin(Stdio::null())
        .stdout(Stdio::inherit()) // TODO
        .stderr(Stdio::inherit()) // TODO
        .arg0("(buck2-forkserver)")
        .args(args)
        .arg("--fd")
        .arg(server_io.as_raw_fd().to_string());

    let fds = [server_io.as_raw_fd()];

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

    let child = command
        .spawn()
        .with_context(|| format!("Failed to start Forkserver `{}`", exe.to_string_lossy()))?;

    let channel = buck2_grpc::make_channel(client_io, "forkserver")
        .await
        .context("Error connecting to Forkserver")?;

    Ok(ForkserverClient::new(child, channel))
}
