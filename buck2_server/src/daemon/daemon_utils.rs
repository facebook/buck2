/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::PathBuf;

use buck2_common::buckd_connection::ConnectionType;

use crate::daemon::tcp_or_unix_listener::TcpOrUnixListener;

pub fn create_listener(
    _daemon_dir: PathBuf,
) -> anyhow::Result<(ConnectionType, TcpOrUnixListener)> {
    crate::daemon::daemon_tcp::create_listener()
}
