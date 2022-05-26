/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{path::Path, pin::Pin, sync::Arc, task};

use futures::{Stream, TryFutureExt};
use tokio::{
    io::{AsyncRead, AsyncWrite, ReadBuf},
    net::UnixListener,
};
use tonic::transport::server::Connected;

use crate::daemon::client_utils::{WithCurrentDirectory, UDS_DAEMON_FILENAME};

// This function will change the working directory briefly and should not be run
// while other threads are running, as directory is a global variable.
pub async fn create_listener(
    daemon_dir: &Path,
) -> anyhow::Result<(
    String,
    Pin<Box<dyn Stream<Item = Result<UnixStream, std::io::Error>>>>,
)> {
    let uds_path = daemon_dir.join(UDS_DAEMON_FILENAME);

    tokio::fs::create_dir_all(&uds_path.parent().unwrap()).await?;
    if Path::exists(&uds_path) {
        std::fs::remove_file(&uds_path)?;
    }

    let listener = {
        // change directory to the daemon directory to connect to unix domain socket
        // then change directory back to the current directory since the unix domain socket
        // path is limited to 108 characters. https://man7.org/linux/man-pages/man7/unix.7.html
        let uds = {
            let _with_dir = WithCurrentDirectory::new(daemon_dir)?;
            UnixListener::bind(Path::new(UDS_DAEMON_FILENAME))?
        };

        async_stream::stream! {
            loop {
                let item = uds.accept().map_ok(|(st, _)| UnixStream(st)).await;
                yield item;
            }
        }
    };

    Ok((
        format!("{}:{}", "uds", uds_path.to_str().unwrap().to_owned()),
        Box::pin(listener),
    ))
}

#[derive(Debug)]
pub struct UnixStream(pub tokio::net::UnixStream);

impl Connected for UnixStream {}

#[derive(Clone, Debug)]
pub struct UdsConnectInfo {
    pub peer_addr: Option<Arc<tokio::net::unix::SocketAddr>>,
    pub peer_cred: Option<tokio::net::unix::UCred>,
}

impl AsyncRead for UnixStream {
    fn poll_read(
        mut self: Pin<&mut Self>,
        cx: &mut task::Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> task::Poll<std::io::Result<()>> {
        Pin::new(&mut self.0).poll_read(cx, buf)
    }
}

impl AsyncWrite for UnixStream {
    fn poll_write(
        mut self: Pin<&mut Self>,
        cx: &mut task::Context<'_>,
        buf: &[u8],
    ) -> task::Poll<std::io::Result<usize>> {
        Pin::new(&mut self.0).poll_write(cx, buf)
    }

    fn poll_flush(
        mut self: Pin<&mut Self>,
        cx: &mut task::Context<'_>,
    ) -> task::Poll<std::io::Result<()>> {
        Pin::new(&mut self.0).poll_flush(cx)
    }

    fn poll_shutdown(
        mut self: Pin<&mut Self>,
        cx: &mut task::Context<'_>,
    ) -> task::Poll<std::io::Result<()>> {
        Pin::new(&mut self.0).poll_shutdown(cx)
    }
}
