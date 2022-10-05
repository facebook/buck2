/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::Path;
use std::path::PathBuf;

use buck2_common::client_utils::UDS_DAEMON_FILENAME;
use buck2_common::home_buck_tmp::home_buck_tmp_dir;
use buck2_common::temp_path::TempPath;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::FileName;
use futures::stream::BoxStream;
use futures::TryFutureExt;
use tokio::net::UnixListener;

use crate::daemon::tcp_or_unix_stream::TcpOrUnixStream;

// This function will change the working directory briefly and should not be run
// while other threads are running, as directory is a global variable.
pub async fn create_listener(
    daemon_dir: PathBuf,
) -> anyhow::Result<(
    String,
    BoxStream<'static, Result<TcpOrUnixStream, std::io::Error>>,
)> {
    let uds_path = daemon_dir.join(UDS_DAEMON_FILENAME);

    tokio::fs::create_dir_all(&uds_path.parent().unwrap()).await?;
    if Path::exists(&uds_path) {
        std::fs::remove_file(&uds_path)?;
    }

    let listener = {
        // Create symlink to the daemon directory to connect to unix domain socket
        // since the unix domain socket path is limited to 108 characters.
        // https://man7.org/linux/man-pages/man7/unix.7.html
        let uds = {
            let socket_dir_symlink = TempPath::new_in(home_buck_tmp_dir()?)?;
            fs_util::symlink(daemon_dir, socket_dir_symlink.path())?;
            let socket_path = socket_dir_symlink
                .path()
                .join(FileName::new(UDS_DAEMON_FILENAME)?);
            let uds = UnixListener::bind(socket_path)?;
            socket_dir_symlink.close()?;
            uds
        };

        async_stream::stream! {
            loop {
                let item = uds.accept().map_ok(|(st, _)| TcpOrUnixStream(st)).await;
                yield item;
            }
        }
    };

    Ok((
        format!("{}:{}", "uds", uds_path.to_str().unwrap().to_owned()),
        Box::pin(listener),
    ))
}
