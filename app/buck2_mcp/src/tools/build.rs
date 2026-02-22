/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Build tool for MCP server.

use serde::Deserialize;
use serde_json::Value;
use serde_json::json;

use crate::daemon::client::McpDaemonClient;
use crate::mcp::protocol::Tool;
use crate::mcp::protocol::ToolCallResult;
use crate::tools::registry::McpTool;

/// Build tool - builds specified Buck2 targets.
pub struct BuildTool;

#[derive(Debug, Deserialize)]
struct BuildArgs {
    /// Target patterns to build
    targets: Vec<String>,
    /// Target platform for configuration
    target_platform: Option<String>,
    /// Whether to show output paths
    #[serde(default)]
    show_output: bool,
}

#[async_trait::async_trait]
impl McpTool for BuildTool {
    fn definition(&self) -> Tool {
        Tool {
            name: "build".to_owned(),
            description: Some(
                "Build the specified Buck2 targets and return build results. \
                This is a synchronous operation that blocks until the build completes."
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
                        "default": false,
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
        let args: BuildArgs = serde_json::from_value(arguments)?;

        let result = client
            .build(
                &args.targets,
                args.target_platform.as_deref(),
                args.show_output,
            )
            .await?;

        Ok(ToolCallResult::text(result))
    }
}
