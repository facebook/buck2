/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Targets tool for MCP server.

use serde::Deserialize;
use serde_json::Value;
use serde_json::json;

use crate::daemon::client::McpDaemonClient;
use crate::mcp::protocol::Tool;
use crate::mcp::protocol::ToolCallResult;
use crate::tools::registry::McpTool;

/// Targets tool - lists and inspects targets matching patterns.
pub struct TargetsTool;

#[derive(Debug, Deserialize)]
struct TargetsArgs {
    /// Target patterns to query
    patterns: Vec<String>,
    /// Output attributes
    output_attributes: Option<Vec<String>>,
    /// Output format: "json" or "text"
    #[serde(default = "default_format")]
    format: String,
}

fn default_format() -> String {
    "json".to_owned()
}

#[async_trait::async_trait]
impl McpTool for TargetsTool {
    fn definition(&self) -> Tool {
        Tool {
            name: "targets".to_owned(),
            description: Some(
                "List and inspect targets matching the specified patterns.".to_owned(),
            ),
            input_schema: json!({
                "type": "object",
                "required": ["patterns"],
                "properties": {
                    "patterns": {
                        "type": "array",
                        "items": { "type": "string" },
                        "description": "Target patterns to list (e.g., ['//foo/...', '//bar:baz'])"
                    },
                    "output_attributes": {
                        "type": "array",
                        "items": { "type": "string" },
                        "description": "Attributes to output (e.g., ['name', 'deps', 'visibility'])"
                    },
                    "format": {
                        "type": "string",
                        "enum": ["json", "text"],
                        "default": "json",
                        "description": "Output format"
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
        let args: TargetsArgs = serde_json::from_value(arguments)?;

        let result = client
            .targets(
                &args.patterns,
                args.output_attributes.as_deref(),
                &args.format,
            )
            .await?;

        Ok(ToolCallResult::text(result))
    }
}
