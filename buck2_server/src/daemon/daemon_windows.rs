/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::net::SocketAddr;
use std::path::PathBuf;

use buck2_common::client_utils::SOCKET_ADDR;
use buck2_common::connection_endpoint::ConnectionType;

use crate::daemon::tcp_or_unix_listener::TcpOrUnixListener;

pub fn create_listener(
    _daemon_dir: PathBuf,
) -> anyhow::Result<(ConnectionType, TcpOrUnixListener)> {
    let addr: SocketAddr = format!("{}:0", SOCKET_ADDR).parse()?;
    let tcp_listener = std::net::TcpListener::bind(addr)?;
    tcp_listener.set_nonblocking(true)?;
    let local_addr = tcp_listener.local_addr()?;
    Ok((
        ConnectionType::Tcp {
            port: local_addr.port(),
        },
        TcpOrUnixListener(tcp_listener),
    ))
}
