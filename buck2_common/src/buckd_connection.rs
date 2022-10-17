/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;
use std::path::Path;
use std::path::PathBuf;

use anyhow::Context;
use gazebo::prelude::StrExt;

pub const BUCK_AUTH_TOKEN_HEADER: &str = "x-buck-auth-token";

#[derive(Debug, thiserror::Error)]
enum ConnectionTypeError {
    #[error("Failed to parse correct endpoint information {0}")]
    ParseError(String),
}

#[derive(Debug)]
pub enum ConnectionType {
    Uds { unix_socket: PathBuf },
    Tcp { port: u16 },
}

impl Display for ConnectionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // NOTE: `Display` must be compatible with `parse`.
        match self {
            ConnectionType::Uds { unix_socket } => write!(f, "uds:{}", unix_socket.display()),
            ConnectionType::Tcp { port } => write!(f, "tcp:{}", port),
        }
    }
}

impl ConnectionType {
    pub fn parse(endpoint: &str) -> anyhow::Result<ConnectionType> {
        let (protocol, endpoint) = endpoint.split1(":");
        match protocol {
            "uds" => Ok(ConnectionType::Uds {
                unix_socket: Path::new(endpoint).to_path_buf(),
            }),
            "tcp" => Ok(ConnectionType::Tcp {
                port: endpoint
                    .parse()
                    .with_context(|| format!("port number is incorrect in `{}`", endpoint))?,
            }),
            _ => Err(ConnectionTypeError::ParseError(endpoint.to_owned()).into()),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use crate::buckd_connection::ConnectionType;

    #[test]
    fn test_fmt_parse() {
        assert_eq!(
            "tcp:1719",
            &format!("{}", ConnectionType::Tcp { port: 1719 })
        );

        let path = if cfg!(windows) {
            PathBuf::from("c:\\path")
        } else {
            PathBuf::from("/path")
        };
        assert_eq!(
            &format!("uds:{}", path.display()),
            &format!("{}", ConnectionType::Uds { unix_socket: path })
        );
    }
}
