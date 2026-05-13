/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! MCP error types and error code constants.

use crate::mcp::protocol::JsonRpcError;

// Standard JSON-RPC error codes
pub const PARSE_ERROR: i32 = -32700;
pub const INVALID_REQUEST: i32 = -32600;
pub const METHOD_NOT_FOUND: i32 = -32601;
pub const INVALID_PARAMS: i32 = -32602;
pub const INTERNAL_ERROR: i32 = -32603;

// MCP-specific error codes (-32000 to -32099)
pub const DAEMON_CONNECTION_ERROR: i32 = -32001;
pub const BUILD_FAILED: i32 = -32002;
pub const QUERY_FAILED: i32 = -32003;
pub const INVALID_TARGET: i32 = -32004;
pub const OPERATION_NOT_FOUND: i32 = -32005;
pub const OPERATION_CANCELLED: i32 = -32006;

/// MCP-specific errors.
#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
pub enum McpError {
    #[error("Failed to connect to Buck2 daemon: {0}")]
    DaemonConnection(String),

    #[error("Build failed: {0}")]
    BuildFailed(String),

    #[error("Query failed: {0}")]
    QueryFailed(String),

    #[error("Invalid target pattern: {0}")]
    InvalidTarget(String),

    #[error("Operation not found: {0}")]
    OperationNotFound(String),

    #[error("Operation cancelled: {0}")]
    OperationCancelled(String),

    #[error("Protocol error: {0}")]
    Protocol(String),
}

impl From<McpError> for JsonRpcError {
    fn from(err: McpError) -> Self {
        let (code, message) = match &err {
            McpError::DaemonConnection(msg) => (DAEMON_CONNECTION_ERROR, msg.clone()),
            McpError::BuildFailed(msg) => (BUILD_FAILED, msg.clone()),
            McpError::QueryFailed(msg) => (QUERY_FAILED, msg.clone()),
            McpError::InvalidTarget(msg) => (INVALID_PARAMS, msg.clone()),
            McpError::OperationNotFound(msg) => (OPERATION_NOT_FOUND, msg.clone()),
            McpError::OperationCancelled(msg) => (OPERATION_CANCELLED, msg.clone()),
            McpError::Protocol(msg) => (PARSE_ERROR, msg.clone()),
        };

        JsonRpcError {
            code,
            message,
            data: None,
        }
    }
}

impl From<buck2_error::Error> for JsonRpcError {
    fn from(err: buck2_error::Error) -> Self {
        JsonRpcError {
            code: INTERNAL_ERROR,
            message: err.to_string(),
            data: None,
        }
    }
}
