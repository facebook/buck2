/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Async build tool for MCP server.

use std::sync::Arc;

use serde::Deserialize;
use serde_json::Value;
use serde_json::json;

use crate::daemon::client::McpDaemonClient;
use crate::mcp::protocol::Tool;
use crate::mcp::protocol::ToolCallResult;
use crate::operations::manager::OperationManager;
use crate::operations::operation::OperationResult;
use crate::tools::registry::McpTool;

/// Async build tool - starts a build and returns an operation ID for polling.
pub struct BuildAsyncTool {
    operation_manager: Arc<OperationManager>,
}

impl BuildAsyncTool {
    pub fn new(operation_manager: Arc<OperationManager>) -> Self {
        Self { operation_manager }
    }
}

#[derive(Debug, Clone, Deserialize)]
struct BuildAsyncArgs {
    /// Target patterns to build
    targets: Vec<String>,
    /// Target platform for configuration
    target_platform: Option<String>,
    /// Whether to show output paths
    #[serde(default = "default_show_output")]
    show_output: bool,
}

fn default_show_output() -> bool {
    true
}

#[async_trait::async_trait]
impl McpTool for BuildAsyncTool {
    fn definition(&self) -> Tool {
        Tool {
            name: "build_async".to_owned(),
            description: Some(
                "Start an async build operation. Returns an operation ID that can be used \
                to poll for status and retrieve the result when complete."
                    .to_owned(),
            ),
            input_schema: json!({
                "type": "object",
                "required": ["targets"],
                "properties": {
                    "targets": {
                        "type": "array",
                        "items": { "type": "string" },
                        "description": "Target patterns to build (e.g., ['//foo:bar', '//baz/...'])"
                    },
                    "target_platform": {
                        "type": "string",
                        "description": "Target platform for configuration"
                    },
                    "show_output": {
                        "type": "boolean",
                        "default": true,
                        "description": "Include output file paths in result"
                    }
                }
            }),
        }
    }

    async fn call(
        &self,
        arguments: Value,
        client: &mut McpDaemonClient,
    ) -> buck2_error::Result<ToolCallResult> {
        let args: BuildAsyncArgs = serde_json::from_value(arguments)?;

        // Create the operation
        let (operation_id, _cancel_receiver) =
            self.operation_manager.create_operation("build").await;

        // Clone the client and operation manager for the spawned task
        let mut client = client.clone();
        let operation_manager = Arc::clone(&self.operation_manager);
        let op_id = operation_id.clone();

        // Spawn the build as a separate tokio task
        tokio::spawn(async move {
            let result = client
                .build(
                    &args.targets,
                    args.target_platform.as_deref(),
                    args.show_output,
                )
                .await;

            match result {
                Ok(output) => {
                    operation_manager
                        .complete_operation(
                            &op_id,
                            OperationResult {
                                success: true,
                                output,
                            },
                        )
                        .await;
                }
                Err(e) => {
                    operation_manager
                        .fail_operation(&op_id, e.to_string())
                        .await;
                }
            }
        });

        Ok(ToolCallResult::json(&json!({
            "operation_id": operation_id,
            "message": "Build operation started. Use operation_status to check progress."
        }))?)
    }
}
