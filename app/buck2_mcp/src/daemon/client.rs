/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Buck2 daemon client for MCP server.
//!
//! Provides a simplified interface to communicate with the Buck2 daemon
//! for running queries, builds, and other commands.

use std::fs::File;
use std::io::BufReader;
use std::net::Ipv4Addr;

use buck2_cli_proto::BuildRequest;
use buck2_cli_proto::ClientContext;
use buck2_cli_proto::CqueryRequest;
use buck2_cli_proto::DaemonProcessInfo;
use buck2_cli_proto::MultiCommandProgress;
use buck2_cli_proto::QueryOutputFormat;
use buck2_cli_proto::TargetCfg;
use buck2_cli_proto::TargetsRequest;
use buck2_cli_proto::UqueryRequest;
use buck2_cli_proto::daemon_api_client::DaemonApiClient;
use buck2_common::buckd_connection::BUCK_AUTH_TOKEN_HEADER;
use buck2_common::buckd_connection::ConnectionType;
use buck2_common::client_utils::get_channel_tcp;
use buck2_error::BuckErrorContext;
use futures::StreamExt;
use tonic::Request;
use tonic::Status;
use tonic::codegen::InterceptedService;
use tonic::metadata::AsciiMetadataValue;
use tonic::service::Interceptor;
use tonic::transport::Channel;
use tracing::debug;

use crate::mcp::error::McpError;

/// Auth token interceptor for Buck2 daemon.
#[derive(Clone)]
struct AuthInterceptor {
    auth_token: AsciiMetadataValue,
}

impl Interceptor for AuthInterceptor {
    fn call(&mut self, mut request: Request<()>) -> Result<Request<()>, Status> {
        request
            .metadata_mut()
            .append(BUCK_AUTH_TOKEN_HEADER, self.auth_token.clone());
        Ok(request)
    }
}

/// Simplified daemon client for MCP server.
#[derive(Clone)]
pub struct McpDaemonClient {
    client: DaemonApiClient<InterceptedService<Channel, AuthInterceptor>>,
    working_dir: String,
}

impl McpDaemonClient {
    /// Connect to the Buck2 daemon.
    ///
    /// Looks for the daemon info file in the standard location and establishes
    /// a gRPC connection.
    pub async fn connect(working_dir: &str) -> buck2_error::Result<Self> {
        // Find the daemon info file
        let daemon_info = Self::find_daemon_info(working_dir).await?;

        debug!(
            "Connecting to Buck2 daemon at {} (pid {})",
            daemon_info.endpoint, daemon_info.pid
        );

        let connection_type = ConnectionType::parse(&daemon_info.endpoint)?;

        let channel = Self::create_channel(connection_type).await?;

        let auth_token = AsciiMetadataValue::try_from(daemon_info.auth_token).map_err(|e| {
            buck2_error::buck2_error!(buck2_error::ErrorTag::Input, "Invalid auth token: {}", e)
        })?;

        let client = DaemonApiClient::with_interceptor(channel, AuthInterceptor { auth_token })
            .max_encoding_message_size(usize::MAX)
            .max_decoding_message_size(usize::MAX);

        Ok(Self {
            client,
            working_dir: working_dir.to_owned(),
        })
    }

    /// Find and load the daemon info file.
    async fn find_daemon_info(working_dir: &str) -> buck2_error::Result<DaemonProcessInfo> {
        // Try to find .buckd directory starting from working_dir
        // The daemon info is typically at ~/.buck/buckd/<hash>/v2/buckd.info
        // or in the project's .buckd directory

        let home = dirs::home_dir().buck_error_context("Could not find home directory")?;
        let working_path = std::path::Path::new(working_dir);
        let possible_paths = Self::get_possible_daemon_paths(&home, working_path)?;

        for path in possible_paths {
            debug!("Checking for daemon info at {:?}", path);
            if path.exists() {
                let file = File::open(&path).buck_error_context("Failed to open buckd.info")?;
                let reader = BufReader::new(file);
                let info: DaemonProcessInfo = serde_json::from_reader(reader)
                    .buck_error_context("Failed to parse buckd.info")?;
                return Ok(info);
            }
        }

        Err(McpError::DaemonConnection(
            "Buck2 daemon not found. Please start it with 'buck2 status' or any buck2 command."
                .to_owned(),
        )
        .into())
    }

    /// Get possible paths where the daemon info file might be located.
    fn get_possible_daemon_paths(
        home: &std::path::Path,
        working_path: &std::path::Path,
    ) -> buck2_error::Result<Vec<std::path::PathBuf>> {
        let mut paths = Vec::new();

        // Standard Buck2 daemon directory structure:
        // ~/.buck/buckd/<project-hash>/v2/buckd.info

        let buck_dir = home.join(".buck").join("buckd");
        if buck_dir.exists()
            && let Ok(entries) = std::fs::read_dir(&buck_dir)
        {
            for entry in entries.flatten() {
                let v2_path = entry.path().join("v2").join("buckd.info");
                if v2_path.exists() {
                    paths.push(v2_path);
                }
            }
        }

        let local_buckd = working_path.join(".buckd").join("buckd.info");
        if local_buckd.exists() {
            paths.push(local_buckd);
        }

        let mut current = working_path;
        while let Some(parent) = current.parent() {
            let buckd_info = parent.join(".buckd").join("buckd.info");
            if buckd_info.exists() {
                paths.push(buckd_info);
            }
            current = parent;
        }

        Ok(paths)
    }

    /// Create a gRPC channel to the daemon.
    async fn create_channel(connection_type: ConnectionType) -> buck2_error::Result<Channel> {
        match connection_type {
            ConnectionType::Tcp { port } => Ok(get_channel_tcp(Ipv4Addr::LOCALHOST, port).await?),
            ConnectionType::Uds { unix_socket: _ } => {
                // Unix socket support - for now just use TCP
                // In a full implementation, we'd use get_channel_uds
                Err(buck2_error::buck2_error!(
                    buck2_error::ErrorTag::Environment,
                    "Unix socket connections not yet implemented in MCP client"
                ))
            }
        }
    }

    /// Create a client context for daemon requests.
    fn create_client_context(&self, command_name: &str) -> ClientContext {
        ClientContext {
            working_dir: self.working_dir.clone(),
            config_overrides: vec![],
            host_platform: 0,
            host_arch: 0,
            oncall: String::new(),
            disable_starlark_types: false,
            unstable_typecheck: false,
            target_call_stacks: false,
            skip_targets_with_duplicate_names: false,
            trace_id: uuid::Uuid::new_v4().to_string(),
            reuse_current_config: false,
            daemon_uuid: None,
            sanitized_argv: vec![],
            argfiles: vec![],
            buck2_hard_error: String::new(),
            command_name: command_name.to_owned(),
            client_metadata: vec![],
            preemptible: 0,
            host_xcode_version: None,
            representative_config_flags: vec![],
            exit_when: 0,
            profile_pattern_opts: None,
        }
    }

    /// Execute a cquery and return results as a string.
    pub async fn cquery(
        &mut self,
        query: &str,
        target_platform: Option<&str>,
        target_universe: Option<&[String]>,
        output_attributes: Option<&[String]>,
    ) -> buck2_error::Result<String> {
        let request = CqueryRequest {
            context: Some(self.create_client_context("cquery")),
            query: query.to_owned(),
            query_args: vec![],
            output_attributes: output_attributes.map(|a| a.to_vec()).unwrap_or_default(),
            target_universe: target_universe.map(|u| u.to_vec()).unwrap_or_default(),
            target_cfg: Some(TargetCfg {
                target_platform: target_platform.unwrap_or_default().to_owned(),
                cli_modifiers: vec![],
            }),
            show_providers: false,
            unstable_output_format: QueryOutputFormat::Json as i32,
            profile_mode: None,
            profile_output: None,
        };

        let response = self.client.cquery(Request::new(request)).await?;
        self.collect_streaming_output(response.into_inner()).await
    }

    /// Execute a uquery and return results as a string.
    pub async fn uquery(
        &mut self,
        query: &str,
        output_attributes: Option<&[String]>,
    ) -> buck2_error::Result<String> {
        let request = UqueryRequest {
            context: Some(self.create_client_context("uquery")),
            query: query.to_owned(),
            query_args: vec![],
            output_attributes: output_attributes.map(|a| a.to_vec()).unwrap_or_default(),
            unstable_output_format: QueryOutputFormat::Json as i32,
        };

        let response = self.client.uquery(Request::new(request)).await?;
        self.collect_streaming_output(response.into_inner()).await
    }

    /// Execute a build and return results as a string.
    pub async fn build(
        &mut self,
        targets: &[String],
        target_platform: Option<&str>,
        show_output: bool,
    ) -> buck2_error::Result<String> {
        use buck2_cli_proto::build_request;

        let request = BuildRequest {
            context: Some(self.create_client_context("build")),
            target_patterns: targets.to_vec(),
            target_cfg: Some(TargetCfg {
                target_platform: target_platform.unwrap_or_default().to_owned(),
                cli_modifiers: vec![],
            }),
            build_providers: Some(build_request::BuildProviders {
                default_info: build_request::build_providers::Action::Build as i32,
                run_info: build_request::build_providers::Action::BuildIfAvailable as i32,
                test_info: build_request::build_providers::Action::Skip as i32,
            }),
            response_options: Some(build_request::ResponseOptions {
                return_outputs: show_output,
                ..Default::default()
            }),
            build_opts: Some(buck2_cli_proto::CommonBuildOptions::default()),
            final_artifact_materializations: build_request::Materializations::Default as i32,
            final_artifact_uploads: build_request::Uploads::Never as i32,
            target_universe: vec![],
            timeout: None,
        };

        let response = self.client.build(Request::new(request)).await?;
        self.collect_build_output(response.into_inner()).await
    }

    /// Execute a targets command and return results as a string.
    pub async fn targets(
        &mut self,
        patterns: &[String],
        output_attributes: Option<&[String]>,
        format: &str,
    ) -> buck2_error::Result<String> {
        use buck2_cli_proto::targets_request;

        let output_format = match format {
            "json" => targets_request::OutputFormat::Json,
            _ => targets_request::OutputFormat::Text,
        };

        let request = TargetsRequest {
            context: Some(self.create_client_context("targets")),
            target_patterns: patterns.to_vec(),
            target_cfg: Some(TargetCfg::default()),
            output: None,
            output_format: output_format as i32,
            targets: Some(targets_request::Targets::Other(targets_request::Other {
                output_attributes: output_attributes.map(|a| a.to_vec()).unwrap_or_default(),
                target_hash_graph_type: 0,
                target_hash_file_mode: 0,
                target_hash_modified_paths: vec![],
                target_hash_use_fast_hash: true,
                include_default_attributes: false,
                target_hash_recursive: false,
                keep_going: false,
                streaming: false,
                cached: true,
                imports: false,
                package_values: vec![],
            })),
            concurrency: None,
            compression: 0,
        };

        let response = self.client.targets(Request::new(request)).await?;
        self.collect_streaming_output(response.into_inner()).await
    }

    /// Collect streaming output from a command response.
    async fn collect_streaming_output(
        &self,
        mut stream: tonic::Streaming<MultiCommandProgress>,
    ) -> buck2_error::Result<String> {
        use buck2_cli_proto::command_progress;
        use buck2_cli_proto::command_result;
        use buck2_cli_proto::partial_result;

        let mut output = String::new();

        while let Some(progress) = stream.next().await {
            let progress = progress?;
            for msg in progress.messages {
                if let Some(progress) = msg.progress {
                    match progress {
                        command_progress::Progress::PartialResult(partial) => {
                            if let Some(partial_result::PartialResult::StdoutBytes(bytes)) =
                                partial.partial_result
                            {
                                output.push_str(&String::from_utf8_lossy(&bytes.data));
                            }
                        }
                        command_progress::Progress::Result(result) => {
                            if let Some(command_result::Result::Error(err)) = result.result {
                                return Err(McpError::QueryFailed(err.message).into());
                            }
                        }
                        _ => {}
                    }
                }
            }
        }

        Ok(output)
    }

    /// Collect build output from a build command response.
    async fn collect_build_output(
        &self,
        mut stream: tonic::Streaming<MultiCommandProgress>,
    ) -> buck2_error::Result<String> {
        use buck2_cli_proto::command_progress;
        use buck2_cli_proto::command_result;

        let mut result_json = String::new();

        while let Some(progress) = stream.next().await {
            let progress = progress?;
            for msg in progress.messages {
                if let Some(command_progress::Progress::Result(result)) = msg.progress {
                    match result.result {
                        Some(command_result::Result::BuildResponse(build_resp)) => {
                            let targets: Vec<_> = build_resp
                                .build_targets
                                .iter()
                                .map(|t| {
                                    serde_json::json!({
                                        "target": t.target,
                                        "configuration": t.configuration,
                                        "outputs": t.outputs.iter().map(|o| &o.path).collect::<Vec<_>>(),
                                    })
                                })
                                .collect();

                            result_json = serde_json::to_string_pretty(&serde_json::json!({
                                "success": build_resp.errors.is_empty(),
                                "project_root": build_resp.project_root,
                                "targets": targets,
                                "errors": build_resp.errors.iter()
                                    .map(|e| &e.message)
                                    .collect::<Vec<_>>(),
                            }))?;
                        }
                        Some(command_result::Result::Error(err)) => {
                            return Err(McpError::BuildFailed(err.message).into());
                        }
                        _ => {}
                    }
                }
            }
        }

        Ok(result_json)
    }
}
