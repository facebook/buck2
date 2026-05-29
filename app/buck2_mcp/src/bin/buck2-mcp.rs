/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Buck2 MCP server binary entry point.

use std::env;

use tracing::info;
use tracing_subscriber::EnvFilter;

#[tokio::main]
async fn main() -> buck2_error::Result<()> {
    // Initialize tracing to stderr (stdout is used for MCP protocol)
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .with_writer(std::io::stderr)
        .init();

    info!("Starting Buck2 MCP server");

    let working_dir = env::current_dir()
        .map_err(|e| {
            buck2_error::buck2_error!(
                buck2_error::ErrorTag::Environment,
                "Failed to get current directory: {}",
                e
            )
        })?
        .to_string_lossy()
        .to_string();

    info!("Working directory: {}", working_dir);

    let mut server = buck2_mcp::mcp::server::McpServer::new(working_dir);
    server.run().await
}
