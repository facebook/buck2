/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::net::SocketAddr;
use std::path::Path;
use std::pin::Pin;

use futures::Stream;

use crate::daemon::client_utils::SOCKET_ADDR;

pub(crate) async fn create_listener(
    _daemon_dir: &Path,
) -> anyhow::Result<(
    String,
    Pin<Box<dyn Stream<Item = Result<tokio::net::TcpStream, std::io::Error>>>>,
)> {
    let addr: SocketAddr = format!("{}:0", SOCKET_ADDR).parse()?;
    let tcp_listener = tokio::net::TcpListener::bind(addr).await?;
    let local_addr = tcp_listener.local_addr()?;
    let listener = tokio_stream::wrappers::TcpListenerStream::new(tcp_listener);
    Ok((
        format!("{}:{}", "tcp", local_addr.port() as i32),
        Box::pin(listener),
    ))
}
