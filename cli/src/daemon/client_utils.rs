/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    convert::TryFrom,
    env,
    path::{Path, PathBuf},
    time::{Duration, Instant},
};

use anyhow::{anyhow, Context};
use futures::Future;
use gazebo::prelude::*;
use thiserror::Error;
use tonic::transport::{Channel, Endpoint};

pub static UDS_FILENAME: &str = "buckd.uds";
pub static SOCKET_ADDR: &str = "0.0.0.0";

pub struct WithCurrentDirectory {
    current_dir: Box<PathBuf>,
}

#[derive(Debug, Error)]
enum ParseError {
    #[error("Failed to parse correct endpoint information {0}")]
    ParseError(String),
}

impl WithCurrentDirectory {
    pub fn new(new_dir: &Path) -> anyhow::Result<Self> {
        let dir = Self {
            current_dir: box (env::current_dir()?),
        };
        match env::set_current_dir(&new_dir) {
            Ok(_e) => Ok(dir),
            Err(e) => Err(anyhow!(e))
                .with_context(|| format!("Failed to change directory to {}", new_dir.display())),
        }
    }
}

impl Drop for WithCurrentDirectory {
    fn drop(&mut self) {
        match env::set_current_dir(&*self.current_dir) {
            Ok(_e) => {}
            Err(e) => panic!("Failed to change directory back {}", e),
        }
    }
}

// This function could potentialy change the working directory briefly and should not be run
// while other threads are running, as directory is a global variable
pub async fn get_channel(endpoint_string: &str) -> anyhow::Result<Channel> {
    let (protocol, endpoint) = endpoint_string.split1(":");
    if protocol == "uds" {
        get_channel_uds(endpoint).await
    } else if protocol == "tcp" {
        get_channel_tcp(endpoint).await
    } else {
        Err(anyhow!(ParseError::ParseError(endpoint.to_owned())))
    }
}

#[cfg(unix)]
pub async fn get_channel_uds(endpoint: &str) -> anyhow::Result<Channel> {
    use tonic::codegen::http::Uri;
    use tower::service_fn;

    let daemon_dir = Path::new(&endpoint).parent().unwrap();
    // change directory to the daemon directory to connect to unix domain socket
    // then change directory back to the current directory since the unix domain socket
    // path is limited to 108 characters. https://man7.org/linux/man-pages/man7/unix.7.html
    let io: tokio::net::UnixStream = {
        let _with_dir = WithCurrentDirectory::new(daemon_dir)?;

        tokio::net::UnixStream::connect(&UDS_FILENAME)
            .await
            .with_context(|| {
                format!("Failed to connect to unix domain socket '{}'", UDS_FILENAME)
            })?
    };

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
        .with_context(|| format!("Failed to connect to unix domain socket '{}'", endpoint))
}

#[cfg(windows)]
pub async fn get_channel_uds(_endpoint: &str) -> anyhow::Result<Channel> {
    Err(anyhow::Error::msg(
        "Unix domain sockets are not supported on Windows",
    ))
}

pub async fn get_channel_tcp(endpoint: &str) -> anyhow::Result<Channel> {
    Endpoint::try_from(format!("http://{}:{}", SOCKET_ADDR, endpoint))?
        .connect()
        .await
        .with_context(|| format!("failed to connect to daemon port {}", endpoint))
}

pub async fn retrying<L, Fut: Future<Output = anyhow::Result<L>>, F: Fn() -> Fut>(
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
