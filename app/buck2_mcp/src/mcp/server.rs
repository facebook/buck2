/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! MCP server implementation.
//!
//! Handles the main message loop and dispatches requests to tools.

use std::sync::Arc;

use serde_json::Value;
use tracing::debug;
use tracing::error;
use tracing::info;

use crate::daemon::client::McpDaemonClient;
use crate::mcp::protocol::InitializeParams;
use crate::mcp::protocol::InitializeResult;
use crate::mcp::protocol::JsonRpcError;
use crate::mcp::protocol::JsonRpcId;
use crate::mcp::protocol::JsonRpcRequest;
use crate::mcp::protocol::JsonRpcResponse;
use crate::mcp::protocol::PROTOCOL_VERSION;
use crate::mcp::protocol::SERVER_NAME;
use crate::mcp::protocol::ServerCapabilities;
use crate::mcp::protocol::ServerInfo;
use crate::mcp::protocol::ToolCallParams;
use crate::mcp::protocol::ToolsCapability;
use crate::mcp::protocol::ToolsListResult;
use crate::mcp::transport::StdioTransport;
use crate::operations::manager::OperationManager;
use crate::tools::registry::ToolRegistry;

/// MCP server state.
pub struct McpServer {
    transport: StdioTransport,
    tools: ToolRegistry,
    daemon_client: Option<McpDaemonClient>,
    initialized: bool,
    working_dir: String,
}

impl McpServer {
    /// Create a new MCP server.
    pub fn new(working_dir: String) -> Self {
        let operation_manager = Arc::new(OperationManager::new());
        Self {
            transport: StdioTransport::new(),
            tools: ToolRegistry::new(operation_manager),
            daemon_client: None,
            initialized: false,
            working_dir,
        }
    }

    /// Run the MCP server main loop.
    pub async fn run(&mut self) -> buck2_error::Result<()> {
        info!("Buck2 MCP server starting");

        loop {
            let message = match self.transport.read_message().await? {
                Some(msg) if msg.is_empty() => continue, // Skip empty lines
                Some(msg) => msg,
                None => {
                    info!("EOF received, shutting down");
                    break;
                }
            };

            debug!("Received message: {}", message);

            let request: JsonRpcRequest = match serde_json::from_str(&message) {
                Ok(req) => req,
                Err(e) => {
                    let response = JsonRpcResponse::error(
                        JsonRpcId::Null,
                        JsonRpcError::parse_error(format!("Failed to parse request: {}", e)),
                    );
                    self.send_response(&response).await?;
                    continue;
                }
            };

            let response = self.handle_request(request).await;
            self.send_response(&response).await?;
        }

        Ok(())
    }

    async fn send_response(&mut self, response: &JsonRpcResponse) -> buck2_error::Result<()> {
        let json = serde_json::to_string(response)?;
        debug!("Sending response: {}", json);
        self.transport.write_message(&json).await
    }

    async fn handle_request(&mut self, request: JsonRpcRequest) -> JsonRpcResponse {
        let id = request.id.clone();

        match self.dispatch_request(request).await {
            Ok(result) => JsonRpcResponse::success(id, result),
            Err(error) => JsonRpcResponse::error(id, error),
        }
    }

    async fn dispatch_request(&mut self, request: JsonRpcRequest) -> Result<Value, JsonRpcError> {
        match request.method.as_str() {
            "initialize" => self.handle_initialize(request.params).await,
            "initialized" => Ok(Value::Null),
            "tools/list" => self.handle_tools_list().await,
            "tools/call" => self.handle_tools_call(request.params).await,
            "ping" => Ok(Value::Object(serde_json::Map::new())),
            method => Err(JsonRpcError::method_not_found(method)),
        }
    }

    async fn handle_initialize(&mut self, params: Option<Value>) -> Result<Value, JsonRpcError> {
        let _params: InitializeParams = match params {
            Some(p) => serde_json::from_value(p).map_err(|e| {
                JsonRpcError::invalid_params(format!("Invalid initialize params: {}", e))
            })?,
            None => {
                return Err(JsonRpcError::invalid_params("Missing initialize params"));
            }
        };

        info!("Initializing MCP server");

        match McpDaemonClient::connect(&self.working_dir).await {
            Ok(client) => {
                self.daemon_client = Some(client);
                info!("Connected to Buck2 daemon");
            }
            Err(e) => {
                error!("Failed to connect to Buck2 daemon: {}", e);
                // We don't fail initialization, but tools will fail if daemon is needed
            }
        }

        self.initialized = true;

        let result = InitializeResult {
            protocol_version: PROTOCOL_VERSION.to_owned(),
            capabilities: ServerCapabilities {
                tools: Some(ToolsCapability {
                    list_changed: false,
                }),
                resources: None,
                prompts: None,
            },
            server_info: ServerInfo {
                name: SERVER_NAME.to_owned(),
            },
        };

        serde_json::to_value(result)
            .map_err(|e| JsonRpcError::internal_error(format!("Serialization error: {}", e)))
    }

    async fn handle_tools_list(&self) -> Result<Value, JsonRpcError> {
        let result = ToolsListResult {
            tools: self.tools.list_tools(),
        };

        serde_json::to_value(result)
            .map_err(|e| JsonRpcError::internal_error(format!("Serialization error: {}", e)))
    }

    async fn handle_tools_call(&mut self, params: Option<Value>) -> Result<Value, JsonRpcError> {
        let params: ToolCallParams = match params {
            Some(p) => serde_json::from_value(p).map_err(|e| {
                JsonRpcError::invalid_params(format!("Invalid tool call params: {}", e))
            })?,
            None => {
                return Err(JsonRpcError::invalid_params("Missing tool call params"));
            }
        };

        info!("Calling tool: {}", params.name);

        let client = match &mut self.daemon_client {
            Some(c) => c,
            None => match McpDaemonClient::connect(&self.working_dir).await {
                Ok(client) => {
                    self.daemon_client = Some(client);
                    self.daemon_client.as_mut().unwrap()
                }
                Err(e) => {
                    return Err(JsonRpcError::new(
                        crate::mcp::error::DAEMON_CONNECTION_ERROR,
                        format!("Failed to connect to Buck2 daemon: {}", e),
                    ));
                }
            },
        };

        let result = self
            .tools
            .call_tool(&params.name, params.arguments, client)
            .await
            .map_err(|e| JsonRpcError::internal_error(e.to_string()))?;

        serde_json::to_value(result)
            .map_err(|e| JsonRpcError::internal_error(format!("Serialization error: {}", e)))
    }
}
