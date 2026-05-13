/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Tool registry for MCP server.

use std::sync::Arc;

use serde_json::Value;

use crate::daemon::client::McpDaemonClient;
use crate::mcp::protocol::Tool;
use crate::mcp::protocol::ToolCallResult;
use crate::operations::manager::OperationManager;
use crate::tools::build::BuildTool;
use crate::tools::build_async::BuildAsyncTool;
use crate::tools::operations::OperationCancelTool;
use crate::tools::operations::OperationListTool;
use crate::tools::operations::OperationResultTool;
use crate::tools::operations::OperationStatusTool;
use crate::tools::query::QueryTool;
use crate::tools::targets::TargetsTool;

/// Trait for MCP tools.
#[async_trait::async_trait]
pub trait McpTool: Send + Sync {
    /// Get the tool definition.
    fn definition(&self) -> Tool;

    /// Call the tool with the given arguments.
    async fn call(
        &self,
        arguments: Value,
        client: &mut McpDaemonClient,
    ) -> buck2_error::Result<ToolCallResult>;
}

/// Registry of available MCP tools.
pub struct ToolRegistry {
    tools: Vec<Box<dyn McpTool>>,
}

impl ToolRegistry {
    /// Create a new tool registry with all available tools.
    pub fn new(operation_manager: Arc<OperationManager>) -> Self {
        Self {
            tools: vec![
                Box::new(QueryTool),
                Box::new(BuildTool),
                Box::new(TargetsTool),
                Box::new(BuildAsyncTool::new(operation_manager.clone())),
                Box::new(OperationStatusTool::new(operation_manager.clone())),
                Box::new(OperationResultTool::new(operation_manager.clone())),
                Box::new(OperationCancelTool::new(operation_manager.clone())),
                Box::new(OperationListTool::new(operation_manager)),
            ],
        }
    }

    /// List all available tools.
    pub fn list_tools(&self) -> Vec<Tool> {
        self.tools.iter().map(|t| t.definition()).collect()
    }

    /// Call a tool by name.
    pub async fn call_tool(
        &self,
        name: &str,
        arguments: Value,
        client: &mut McpDaemonClient,
    ) -> buck2_error::Result<ToolCallResult> {
        let tool = self
            .tools
            .iter()
            .find(|t| t.definition().name == name)
            .ok_or_else(|| {
                buck2_error::buck2_error!(buck2_error::ErrorTag::Input, "Unknown tool: {}", name)
            })?;

        tool.call(arguments, client).await
    }
}
