/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Operation management tools for MCP server.

use std::sync::Arc;
use std::time::Duration;

use serde::Deserialize;
use serde_json::Value;
use serde_json::json;

use crate::daemon::client::McpDaemonClient;
use crate::mcp::protocol::Tool;
use crate::mcp::protocol::ToolCallResult;
use crate::operations::manager::OperationManager;
use crate::tools::registry::McpTool;

// ============================================================================
// OperationStatusTool
// ============================================================================

/// Tool to get the status of an async operation.
pub struct OperationStatusTool {
    operation_manager: Arc<OperationManager>,
}

impl OperationStatusTool {
    pub fn new(operation_manager: Arc<OperationManager>) -> Self {
        Self { operation_manager }
    }
}

#[derive(Debug, Deserialize)]
struct OperationStatusArgs {
    operation_id: String,
}

#[async_trait::async_trait]
impl McpTool for OperationStatusTool {
    fn definition(&self) -> Tool {
        Tool {
            name: "operation_status".to_owned(),
            description: Some(
                "Get the current status and progress of an async operation.".to_owned(),
            ),
            input_schema: json!({
                "type": "object",
                "required": ["operation_id"],
                "properties": {
                    "operation_id": {
                        "type": "string",
                        "description": "Operation ID from build_async"
                    }
                }
            }),
        }
    }

    async fn call(
        &self,
        arguments: Value,
        _client: &mut McpDaemonClient,
    ) -> buck2_error::Result<ToolCallResult> {
        let args: OperationStatusArgs = serde_json::from_value(arguments)?;
        let status = self
            .operation_manager
            .get_status(&args.operation_id)
            .await?;
        Ok(ToolCallResult::json(&status)?)
    }
}

// ============================================================================
// OperationResultTool
// ============================================================================

/// Tool to get the result of a completed operation.
pub struct OperationResultTool {
    operation_manager: Arc<OperationManager>,
}

impl OperationResultTool {
    pub fn new(operation_manager: Arc<OperationManager>) -> Self {
        Self { operation_manager }
    }
}

#[derive(Debug, Deserialize)]
struct OperationResultArgs {
    operation_id: String,
    #[serde(default)]
    timeout_secs: Option<f64>,
}

#[async_trait::async_trait]
impl McpTool for OperationResultTool {
    fn definition(&self) -> Tool {
        Tool {
            name: "operation_result".to_owned(),
            description: Some(
                "Get the final result of a completed operation. \
                Optionally wait for completion with a timeout."
                    .to_owned(),
            ),
            input_schema: json!({
                "type": "object",
                "required": ["operation_id"],
                "properties": {
                    "operation_id": {
                        "type": "string"
                    },
                    "timeout_secs": {
                        "type": "number",
                        "description": "Max seconds to wait (0 = don't wait)"
                    }
                }
            }),
        }
    }

    async fn call(
        &self,
        arguments: Value,
        _client: &mut McpDaemonClient,
    ) -> buck2_error::Result<ToolCallResult> {
        let args: OperationResultArgs = serde_json::from_value(arguments)?;

        let timeout = args.timeout_secs.map(Duration::from_secs_f64);
        let status = self
            .operation_manager
            .wait_for_result(&args.operation_id, timeout)
            .await?;

        Ok(ToolCallResult::json(&status)?)
    }
}

// ============================================================================
// OperationCancelTool
// ============================================================================

/// Tool to cancel a running operation.
pub struct OperationCancelTool {
    operation_manager: Arc<OperationManager>,
}

impl OperationCancelTool {
    pub fn new(operation_manager: Arc<OperationManager>) -> Self {
        Self { operation_manager }
    }
}

#[derive(Debug, Deserialize)]
struct OperationCancelArgs {
    operation_id: String,
}

#[async_trait::async_trait]
impl McpTool for OperationCancelTool {
    fn definition(&self) -> Tool {
        Tool {
            name: "operation_cancel".to_owned(),
            description: Some("Cancel a running async operation.".to_owned()),
            input_schema: json!({
                "type": "object",
                "required": ["operation_id"],
                "properties": {
                    "operation_id": {
                        "type": "string"
                    }
                }
            }),
        }
    }

    async fn call(
        &self,
        arguments: Value,
        _client: &mut McpDaemonClient,
    ) -> buck2_error::Result<ToolCallResult> {
        let args: OperationCancelArgs = serde_json::from_value(arguments)?;
        self.operation_manager
            .cancel_operation(&args.operation_id)
            .await?;
        Ok(ToolCallResult::json(&json!({
            "cancelled": true,
            "operation_id": args.operation_id
        }))?)
    }
}

// ============================================================================
// OperationListTool
// ============================================================================

/// Tool to list all active operations.
pub struct OperationListTool {
    operation_manager: Arc<OperationManager>,
}

impl OperationListTool {
    pub fn new(operation_manager: Arc<OperationManager>) -> Self {
        Self { operation_manager }
    }
}

#[derive(Debug, Deserialize)]
struct OperationListArgs {
    #[serde(default)]
    include_completed: bool,
}

#[async_trait::async_trait]
impl McpTool for OperationListTool {
    fn definition(&self) -> Tool {
        Tool {
            name: "operation_list".to_owned(),
            description: Some("List all active and recent operations.".to_owned()),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "include_completed": {
                        "type": "boolean",
                        "default": false
                    }
                }
            }),
        }
    }

    async fn call(
        &self,
        arguments: Value,
        _client: &mut McpDaemonClient,
    ) -> buck2_error::Result<ToolCallResult> {
        let args: OperationListArgs = serde_json::from_value(arguments)?;
        let operations = self
            .operation_manager
            .list_operations(args.include_completed)
            .await;
        Ok(ToolCallResult::json(&json!({
            "operations": operations
        }))?)
    }
}
