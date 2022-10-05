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

use buck2_common::client_utils::SOCKET_ADDR;
use futures::stream::TryStreamExt;
use futures::Stream;

use crate::daemon::tcp_or_unix_stream::TcpOrUnixStream;

pub async fn create_listener(
    _daemon_dir: &Path,
) -> anyhow::Result<(
    String,
    Pin<Box<dyn Stream<Item = Result<TcpOrUnixStream, std::io::Error>>>>,
)> {
    let addr: SocketAddr = format!("{}:0", SOCKET_ADDR).parse()?;
    let tcp_listener = tokio::net::TcpListener::bind(addr).await?;
    let local_addr = tcp_listener.local_addr()?;
    let listener = tokio_stream::wrappers::TcpListenerStream::new(tcp_listener);
    let listener = listener.map_ok(TcpOrUnixStream);
    Ok((
        format!("{}:{}", "tcp", local_addr.port() as i32),
        Box::pin(listener),
    ))
}
