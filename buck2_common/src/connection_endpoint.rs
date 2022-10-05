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

use anyhow::Context;
use gazebo::prelude::StrExt;

#[derive(Debug, thiserror::Error)]
enum ConnectionTypeError {
    #[error("Failed to parse correct endpoint information {0}")]
    ParseError(String),
}

pub enum ConnectionType {
    Uds { unix_socket: PathBuf },
    Tcp { port: u16 },
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
