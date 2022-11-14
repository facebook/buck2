/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub fn create_listener() -> anyhow::Result<(
    buck2_common::buckd_connection::ConnectionType,
    std::net::TcpListener,
)> {
    use std::net::SocketAddr;

    use buck2_common::buckd_connection::ConnectionType;
    use buck2_common::client_utils::SOCKET_ADDR;

    let addr: SocketAddr = format!("{}:0", SOCKET_ADDR).parse()?;
    let tcp_listener = std::net::TcpListener::bind(addr)?;
    tcp_listener.set_nonblocking(true)?;
    let local_addr = tcp_listener.local_addr()?;
    Ok((
        ConnectionType::Tcp {
            port: local_addr.port(),
        },
        tcp_listener,
    ))
}

#[cfg(test)]
mod tests {
    use assert_matches::assert_matches;
    use buck2_common::buckd_connection::ConnectionType;

    use crate::daemon::daemon_tcp::create_listener;

    #[test]
    fn test_create_listener() {
        let (connection_type, _tcp_listener) = create_listener().unwrap();
        assert_matches!(connection_type, ConnectionType::Tcp { .. });
    }
}
