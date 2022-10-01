/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cmp;
use std::path::Path;
use std::path::PathBuf;
use std::time::Duration;

use anyhow::Context;
use futures::Future;
use tokio::time::Instant;
use tonic::transport::Channel;
use tonic::transport::Endpoint;

pub static UDS_DAEMON_FILENAME: &str = "buckd.uds";
pub static SOCKET_ADDR: &str = "127.0.0.1";

pub enum ConnectionType {
    Uds { unix_socket: PathBuf },
    Tcp { socket: String, port: String },
}
// This function could potentialy change the working directory briefly and should not be run
// while other threads are running, as directory is a global variable
pub async fn get_channel(
    endpoint: ConnectionType,
    change_to_parent_dir: bool,
) -> anyhow::Result<Channel> {
    match endpoint {
        ConnectionType::Uds { unix_socket } => {
            get_channel_uds(&unix_socket, change_to_parent_dir).await
        }
        ConnectionType::Tcp { socket, port } => get_channel_tcp(&socket, &port).await,
    }
}

#[cfg(unix)]
async fn get_channel_uds(
    unix_socket: &Path,
    change_to_parent_dir: bool,
) -> anyhow::Result<Channel> {
    use buck2_core::fs::fs_util;

    use crate::home_buck_tmp::home_buck_tmp_dir;
    use crate::temp_path::TempPath;

    // Symlink to temp file to connect to unix domain socket
    // since the unix domain socket path is limited to 108 characters.
    // https://man7.org/linux/man-pages/man7/unix.7.html
    if change_to_parent_dir {
        let symlink = TempPath::new_in(home_buck_tmp_dir()?)?;

        fs_util::symlink(unix_socket, symlink.path())?;

        let r = get_channel_uds_no_symlink(symlink.path())
            .await
            .with_context(|| {
                format!(
                    "Failed to connect to unix domain socket `{}` using symlink `{}`",
                    unix_socket.display(),
                    symlink.path().display(),
                )
            })?;

        symlink.close()?;

        Ok(r)
    } else {
        get_channel_uds_no_symlink(unix_socket)
            .await
            .with_context(|| {
                format!(
                    "Failed to connect to unix domain socket `{}`",
                    unix_socket.display()
                )
            })
    }
}

#[cfg(unix)]
async fn get_channel_uds_no_symlink(connect_to: &Path) -> anyhow::Result<Channel> {
    use tonic::codegen::http::Uri;
    use tower::service_fn;

    let io = tokio::net::UnixStream::connect(&connect_to).await?;

    let mut io = Some(io);
    // This URL string is not relevant to the connection. Some URL is required for the function to work but the closure running inside connect_with_connector()
    // deals with connecting to the unix domain socket.
    Ok(Endpoint::try_from("http://[::]:50051")?
        .connect_with_connector(service_fn(move |_: Uri| {
            let io = io
                .take()
                .with_context(|| "Cannot reconnect after connection loss to uds");
            futures::future::ready(io)
        }))
        .await?)
}

#[cfg(windows)]
async fn get_channel_uds(_unix_filename: &Path, _chg_dir: bool) -> anyhow::Result<Channel> {
    Err(anyhow::Error::msg(
        "Unix domain sockets are not supported on Windows",
    ))
}

pub async fn get_channel_tcp(socket_addr: &str, port: &str) -> anyhow::Result<Channel> {
    Endpoint::try_from(format!("http://{}:{}", socket_addr, port))?
        .connect()
        .await
        .with_context(|| format!("failed to connect to port {}", port))
}

pub async fn retrying<L, Fut: Future<Output = anyhow::Result<L>>, F: Fn() -> Fut>(
    initial_delay: Duration,
    max_delay: Duration,
    timeout: Duration,
    func: F,
) -> anyhow::Result<L> {
    let deadline = Instant::now() + timeout;
    let mut wait = initial_delay;
    let mut tries = 0;
    let mut last_error = None;

    loop {
        tries += 1;
        match tokio::time::timeout_at(deadline, func()).await {
            Ok(Ok(val)) => return Ok(val),
            Ok(Err(e)) => {
                last_error = Some(e);
                let now = Instant::now();
                if now >= deadline {
                    return Err(make_error(tries, last_error, timeout));
                }
                tokio::time::sleep_until(cmp::min(deadline, now + wait)).await;
                wait = cmp::min(max_delay, wait * 2);
            }
            Err(_) => return Err(make_error(tries, last_error, timeout)),
        }
    }

    fn make_error(
        tries: u32,
        last_error: Option<anyhow::Error>,
        timeout: Duration,
    ) -> anyhow::Error {
        match last_error {
            Some(e) => e.context(format!(
                "Failed after {} attempts over {:.2}s",
                tries,
                timeout.as_secs_f64()
            )),
            None => {
                // `tries` is zero here.
                anyhow::anyhow!(
                    "Timed out after {:.2}s, unknown error",
                    timeout.as_secs_f64()
                )
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::time::Duration;

    use crate::client_utils::retrying;

    #[tokio::test]
    async fn test_retrying_error_forever() {
        // Testing against real timer is not good,
        // but setting up proper mock timer is too much boilerplate.
        let future = retrying(
            Duration::from_millis(1),
            Duration::from_millis(1),
            Duration::from_millis(1),
            || async { Err(anyhow::anyhow!("test")) },
        );
        let result: anyhow::Result<()> = future.await;
        assert!(result.is_err());
    }
}
