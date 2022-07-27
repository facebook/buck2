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

use anyhow::Context;
use futures::Future;
use thiserror::Error;
use tonic::transport::Channel;
use tonic::transport::Endpoint;

pub static UDS_DAEMON_FILENAME: &str = "buckd.uds";
pub static SOCKET_ADDR: &str = "127.0.0.1";

#[derive(Debug, Error)]
pub(crate) enum ParseError {
    #[error("Failed to parse correct endpoint information {0}")]
    ParseError(String),
}

pub(crate) enum ConnectionType {
    Uds { unix_socket: String },
    Tcp { socket: String, port: String },
}
// This function could potentialy change the working directory briefly and should not be run
// while other threads are running, as directory is a global variable
pub(crate) async fn get_channel(
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
pub(crate) async fn get_channel_uds(
    unix_socket: &str,
    change_to_parent_dir: bool,
) -> anyhow::Result<Channel> {
    use std::path::Path;

    use tonic::codegen::http::Uri;
    use tower::service_fn;

    use crate::daemon::with_current_directory::WithCurrentDirectory;

    // change directory to the daemon directory to connect to unix domain socket
    // then change directory back to the current directory since the unix domain socket
    // path is limited to 108 characters. https://man7.org/linux/man-pages/man7/unix.7.html
    let (connect_to, _with_dir) = if change_to_parent_dir {
        let unix_socket = Path::new(unix_socket);
        let socket_dir = unix_socket.parent().ok_or_else(|| {
            anyhow::anyhow!("Socket path has no parent: `{}`", unix_socket.display())
        })?;
        let filename = unix_socket
            .file_name()
            .ok_or_else(|| anyhow::anyhow!("Invalid socket: `{}`", unix_socket.display()))?
            .to_str()
            .ok_or_else(|| anyhow::anyhow!("Invalid Conversion from Osstr to Str"))?;
        (filename, Some(WithCurrentDirectory::new(socket_dir)))
    } else {
        (unix_socket, None)
    };

    let io = tokio::net::UnixStream::connect(&connect_to)
        .await
        .with_context(|| format!("Failed to connect to unix domain socket `{}`", unix_socket))?;

    let mut io = Some(io);
    // This URL string is not relevant to the connection. Some URL is required for the function to work but the closure running inside connect_with_connector()
    // deals with connecting to the unix domain socket.
    Endpoint::try_from("http://[::]:50051")?
        .connect_with_connector(service_fn(move |_: Uri| {
            let io = io
                .take()
                .with_context(|| "Cannot reconnect after connection loss to uds");
            futures::future::ready(io)
        }))
        .await
        .with_context(|| format!("Failed to connect to unix domain socket `{}`", unix_socket))
}
#[cfg(windows)]
pub async fn get_channel_uds(_unix_filename: &str, _chg_dir: bool) -> anyhow::Result<Channel> {
    Err(anyhow::Error::msg(
        "Unix domain sockets are not supported on Windows",
    ))
}

pub(crate) async fn get_channel_tcp(socket_addr: &str, port: &str) -> anyhow::Result<Channel> {
    Endpoint::try_from(format!("http://{}:{}", socket_addr, port))?
        .connect()
        .await
        .with_context(|| format!("failed to connect to port {}", port))
}

pub(crate) async fn retrying<L, Fut: Future<Output = anyhow::Result<L>>, F: Fn() -> Fut>(
    initial_delay: Duration,
    max_delay: Duration,
    timeout: Duration,
    func: F,
) -> anyhow::Result<L> {
    let start = Instant::now();
    let mut wait = initial_delay;
    let mut tries = 0;
    loop {
        tries += 1;
        match func().await {
            Ok(val) => return Ok(val),
            Err(e) => {
                if start.elapsed() > timeout {
                    return Err(e.context(format!(
                        "Failed after {} attempts over {:.2}s",
                        tries,
                        timeout.as_secs_f64()
                    )));
                }
                tokio::time::sleep(wait).await;
                wait = std::cmp::min(max_delay, wait * 2);
            }
        }
    }
}
