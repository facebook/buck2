/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Stdio transport for MCP protocol.
//!
//! MCP uses newline-delimited JSON messages over stdin/stdout.

use tokio::io::AsyncBufReadExt;
use tokio::io::AsyncWriteExt;
use tokio::io::BufReader;

/// Stdio transport for MCP communication.
pub struct StdioTransport {
    stdin: BufReader<tokio::io::Stdin>,
    stdout: tokio::io::Stdout,
}

impl StdioTransport {
    /// Create a new stdio transport.
    pub fn new() -> Self {
        Self {
            stdin: BufReader::new(tokio::io::stdin()),
            stdout: tokio::io::stdout(),
        }
    }

    /// Read a single JSON-RPC message from stdin.
    ///
    /// Returns `None` on EOF.
    pub async fn read_message(&mut self) -> buck2_error::Result<Option<String>> {
        let mut line = String::new();
        match self.stdin.read_line(&mut line).await {
            Ok(0) => Ok(None), // EOF
            Ok(_) => {
                let trimmed = line.trim();
                if trimmed.is_empty() {
                    Ok(Some(String::new()))
                } else {
                    Ok(Some(trimmed.to_owned()))
                }
            }
            Err(e) => Err(e.into()),
        }
    }

    /// Write a JSON-RPC message to stdout.
    pub async fn write_message(&mut self, message: &str) -> buck2_error::Result<()> {
        self.stdout.write_all(message.as_bytes()).await?;
        self.stdout.write_all(b"\n").await?;
        self.stdout.flush().await?;
        Ok(())
    }
}

impl Default for StdioTransport {
    fn default() -> Self {
        Self::new()
    }
}
