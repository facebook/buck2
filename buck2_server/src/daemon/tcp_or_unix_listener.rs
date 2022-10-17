/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! **std** listener, Unix on Unix and TCP on Windows.

use futures::stream::BoxStream;
use futures::stream::TryStreamExt;

use crate::daemon::tcp_or_unix_stream::TcpOrUnixStream;

pub struct TcpOrUnixListener(pub(crate) std::net::TcpListener);

impl TcpOrUnixListener {
    /// This function can only be called from tokio context.
    pub fn into_accept_stream(
        self,
    ) -> anyhow::Result<BoxStream<'static, Result<TcpOrUnixStream, std::io::Error>>> {
        self.0.set_nonblocking(true)?;

        let listener = {
            let listener = tokio::net::TcpListener::from_std(self.0)?;

            tokio_stream::wrappers::TcpListenerStream::new(listener)
        };

        Ok(Box::pin(listener.map_ok(TcpOrUnixStream)))
    }
}
