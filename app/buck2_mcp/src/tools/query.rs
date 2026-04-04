/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Query tool for MCP server.

use serde::Deserialize;
use serde_json::Value;
use serde_json::json;

use crate::daemon::client::McpDaemonClient;
use crate::mcp::protocol::Tool;
use crate::mcp::protocol::ToolCallResult;
use crate::tools::registry::McpTool;

/// Query tool - runs cquery or uquery on the Buck2 target graph.
pub struct QueryTool;

#[derive(Debug, Deserialize)]
struct QueryArgs {
    /// The query expression (e.g., "deps(//foo:bar)")
    query: String,
    /// Query type: "cquery" or "uquery"
    #[serde(default = "default_query_type")]
    query_type: String,
    /// Target platform for cquery (optional)
    target_platform: Option<String>,
    /// Target universe for cquery (optional)
    target_universe: Option<Vec<String>>,
    /// Output attributes to include (optional)
    output_attributes: Option<Vec<String>>,
}

fn default_query_type() -> String {
    "cquery".to_owned()
}

#[async_trait::async_trait]
impl McpTool for QueryTool {
    fn definition(&self) -> Tool {
        Tool {
            name: "query".to_owned(),
            description: Some(
                "Run cquery or uquery on the Buck2 target graph. \
                cquery operates on the configured target graph (with resolved selects), \
                uquery operates on the unconfigured graph."
                    .to_owned(),
            ),
            input_schema: json!({
                "type": "object",
                "required": ["query"],
                "properties": {
                    "query": {
                        "type": "string",
                        "description": "The query expression (e.g., 'deps(//foo:bar)', 'rdeps(//..., //foo:bar)')"
                    },
                    "query_type": {
                        "type": "string",
                        "enum": ["cquery", "uquery"],
                        "default": "cquery",
                        "description": "Type of query: cquery (configured) or uquery (unconfigured)"
                    },
                    "target_platform": {
                        "type": "string",
                        "description": "Target platform for configuration (cquery only)"
                    },
                    "target_universe": {
                        "type": "array",
                        "items": { "type": "string" },
                        "description": "Target universe to limit query scope (cquery only)"
                    },
                    "output_attributes": {
                        "type": "array",
                        "items": { "type": "string" },
                        "description": "Attributes to include in output (e.g., ['name', 'deps', 'srcs'])"
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
        let args: QueryArgs = serde_json::from_value(arguments)?;

        let output = match args.query_type.as_str() {
            "cquery" => {
                client
                    .cquery(
                        &args.query,
                        args.target_platform.as_deref(),
                        args.target_universe.as_deref(),
                        args.output_attributes.as_deref(),
                    )
                    .await?
            }
            "uquery" => {
                client
                    .uquery(&args.query, args.output_attributes.as_deref())
                    .await?
            }
            _ => {
                return Ok(ToolCallResult::error(format!(
                    "Unknown query type: {}",
                    args.query_type
                )));
            }
        };

        Ok(ToolCallResult::text(output))
    }
}
