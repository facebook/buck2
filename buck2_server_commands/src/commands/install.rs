/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::Hash;
use std::hash::Hasher;
use std::net::SocketAddr;
use std::net::TcpListener;
use std::process::Stdio;

use anyhow::Context;
use async_trait::async_trait;
use buck2_build_api::actions::artifact::materializer::ArtifactMaterializer;
use buck2_build_api::actions::artifact::Artifact;
use buck2_build_api::actions::artifact::BaseArtifactKind;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::artifact_groups::ArtifactGroupValues;
use buck2_build_api::calculation::Calculation;
use buck2_build_api::context::HasBuildContextData;
use buck2_build_api::interpreter::rule_defs::cmd_args::AbsCommandLineBuilder;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use buck2_build_api::interpreter::rule_defs::cmd_args::SimpleCommandLineArtifactVisitor;
use buck2_build_api::interpreter::rule_defs::provider::builtin::install_info::InstallInfoCallable;
use buck2_build_api::interpreter::rule_defs::provider::builtin::run_info::RunInfo;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::executor_config::PathSeparatorKind;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_core::directory::DirectoryEntry;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::AbsPathBuf;
use buck2_core::fs::paths::ForwardRelativePathBuf;
use buck2_core::package::Package;
use buck2_core::pattern::ProvidersPattern;
use buck2_core::process::background_command;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_core::target::TargetLabel;
use buck2_core::target::TargetName;
use buck2_execute::artifact::fs::ArtifactFs;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_interpreter_for_build::interpreter::calculation::InterpreterCalculation;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::pattern::parse_patterns_from_cli_args;
use buck2_server_ctx::pattern::resolve_patterns;
use buck2_server_ctx::pattern::target_platform_from_client_context;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use chrono::DateTime;
use chrono::NaiveDateTime;
use chrono::Utc;
use cli_proto::InstallRequest;
use cli_proto::InstallResponse;
use dice::DiceComputations;
use dice::DiceTransaction;
use futures::future::try_join;
use futures::future::try_join_all;
use futures::stream::StreamExt;
use futures::stream::TryStreamExt;
use gazebo::prelude::StrExt;
use gazebo::prelude::*;
use indexmap::IndexMap;
use install_proto::installer_client::InstallerClient;
use install_proto::FileReadyRequest;
use install_proto::InstallInfoRequest;
use install_proto::ShutdownRequest;
use tempfile::Builder;
use tokio::sync::mpsc;
use tonic::transport::Channel;
use tracing::level_filters::LevelFilter;
use tracing_subscriber::EnvFilter;

pub static DEFAULT_SOCKET_ADDR: &str = "0.0.0.0";

#[derive(Debug, thiserror::Error)]
pub enum InstallError {
    #[error("Not install type `{0}` from package `{1}`")]
    NoInstallProvider(TargetName, Package),

    #[error("Installer target `{0}` doesn't expose RunInfo provider")]
    NoRunInfoProvider(TargetName),

    #[error("Error retrieving hash for `{0}`")]
    ErrorRetrievingHash(String),

    #[error(
        "Installer failed to process file ready request for `{install_id}`. Artifact: `{artifact}` located at `{path}`. Error message: `{err}`"
    )]
    ProcessingFileReadyFailure {
        install_id: String,
        artifact: String,
        path: AbsPathBuf,
        err: String,
    },

    #[error("Installer failed for `{install_id}` with `{err}`")]
    InternalInstallerFailure { install_id: String, err: String },

    #[error("Communication with the installer failed with `{err}`")]
    InstallerCommunicationFailure { err: String },
}

async fn get_installer_log_directory(
    server_ctx: &dyn ServerCommandContextTrait,
    ctx: &DiceComputations,
) -> anyhow::Result<AbsPathBuf> {
    let out_path = ctx.get_buck_out_path().await?;
    let filesystem = server_ctx.project_root();
    let buck_out_path = out_path
        .as_forward_relative_path()
        .resolve(filesystem.root());
    let install_log_dir = buck_out_path.join(ForwardRelativePathBuf::unchecked_new(
        "installer".to_owned(),
    ));
    fs_util::create_dir_all(&install_log_dir)?;
    Ok(install_log_dir)
}

pub async fn install_command(
    ctx: Box<dyn ServerCommandContextTrait>,
    req: InstallRequest,
) -> anyhow::Result<InstallResponse> {
    run_server_command(InstallServerCommand { req }, ctx).await
}

struct InstallServerCommand {
    req: InstallRequest,
}

#[async_trait]
impl ServerCommandTemplate for InstallServerCommand {
    type StartEvent = buck2_data::InstallCommandStart;
    type EndEvent = buck2_data::InstallCommandEnd;
    type Response = InstallResponse;

    fn end_event(&self) -> Self::EndEvent {
        buck2_data::InstallCommandEnd {
            unresolved_target_patterns: self.req.target_patterns.clone(),
        }
    }

    async fn command(
        &self,
        server_ctx: Box<dyn ServerCommandContextTrait>,
        ctx: DiceTransaction,
    ) -> anyhow::Result<Self::Response> {
        install(server_ctx, ctx, &self.req).await
    }
}

async fn install(
    server_ctx: Box<dyn ServerCommandContextTrait>,
    ctx: DiceTransaction,
    request: &InstallRequest,
) -> anyhow::Result<InstallResponse> {
    let cwd = server_ctx.working_dir();

    let cell_resolver = ctx.get_cell_resolver().await?;

    let global_target_platform = target_platform_from_client_context(
        request.context.as_ref(),
        &cell_resolver,
        server_ctx.working_dir(),
    )
    .await?;

    // Note <TargetName> does not return the providers
    let parsed_patterns = parse_patterns_from_cli_args::<ProvidersPattern>(
        &request.target_patterns,
        &cell_resolver,
        &ctx.get_legacy_configs().await?,
        cwd,
    )?;
    server_ctx.log_target_pattern(&parsed_patterns);

    let resolved_pattern =
        resolve_patterns(&parsed_patterns, &cell_resolver, &ctx.file_ops()).await?;

    let mut installer_to_files_map = HashMap::new();
    for (package, spec) in resolved_pattern.specs {
        let ctx = &ctx;
        let targets: anyhow::Result<Vec<ProvidersPattern>> = match spec {
            buck2_core::pattern::PackageSpec::Targets(targets) => Ok(targets),
            buck2_core::pattern::PackageSpec::All => {
                let interpreter_results = ctx.get_interpreter_results(&package).await?;
                let targets = interpreter_results
                    .targets()
                    .keys()
                    .duped()
                    .map(|t| (t, ProvidersName::Default))
                    .collect();
                Ok(targets)
            }
        };
        let targets = targets?;
        for (target_name, providers_name) in targets {
            let label = ProvidersLabel::new(
                TargetLabel::new(package.dupe(), target_name.dupe()),
                providers_name,
            );
            let providers_label = ctx
                .get_configured_target(&label, global_target_platform.dupe().as_ref())
                .await?;
            let frozen_providers = ctx
                .get_providers(&providers_label)
                .await?
                .require_compatible()?;
            let providers = frozen_providers.provider_collection();
            match providers.get_provider(InstallInfoCallable::provider_id_t()) {
                Some(install_info) => {
                    let install_id = format!("{}", providers_label.target());
                    let installer_label = install_info.get_installer()?;
                    installer_to_files_map
                        .entry(installer_label)
                        .or_insert_with(Vec::new)
                        .push((install_id, install_info));
                }
                None => {
                    return Err(InstallError::NoInstallProvider(
                        target_name.dupe(),
                        package.dupe(),
                    )
                    .into());
                }
            };
        }
    }

    let install_log_dir = &get_installer_log_directory(&*server_ctx, &ctx).await?;

    let mut install_requests = Vec::with_capacity(installer_to_files_map.len());
    for (installer_label, install_info_vector) in &installer_to_files_map {
        let ctx = &ctx;
        let installer_run_args = &request.installer_run_args;

        let mut install_files_vector = Vec::new();
        for (install_id, install_info) in install_info_vector {
            let install_files = install_info.get_files()?;
            install_files_vector.push((install_id, install_files));
        }

        let handle_install_request_future = async move {
            handle_install_request(
                ctx,
                install_log_dir,
                &install_files_vector,
                installer_label,
                installer_run_args,
            )
            .await
        };
        install_requests.push(handle_install_request_future);
    }

    try_join_all(install_requests)
        .await
        .context("Interaction with installer failed.")?;

    Ok(InstallResponse {})
}

fn get_random_tcp_port() -> anyhow::Result<u16> {
    let bind_address = std::net::Ipv4Addr::LOCALHOST.into();
    let socket_addr = SocketAddr::new(bind_address, 0);
    let tcp_port = TcpListener::bind(socket_addr)?.local_addr()?.port();
    Ok(tcp_port)
}

fn get_timestamp_as_string() -> String {
    let nt = NaiveDateTime::from_timestamp(Utc::now().timestamp(), 0);
    let dt: DateTime<Utc> = DateTime::from_utc(nt, Utc);
    dt.format("%Y%m%d-%H%M%S").to_string()
}

fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}

async fn handle_install_request<'a>(
    ctx: &'a DiceComputations,
    install_log_dir: &AbsPathBuf,
    install_files_slice: &[(&String, IndexMap<&str, Artifact>)],
    installer_label: &ConfiguredProvidersLabel,
    initial_installer_run_args: &[String],
) -> anyhow::Result<()> {
    let (files_tx, files_rx) = mpsc::unbounded_channel();
    let build_files = async move {
        build_files(ctx, install_files_slice, files_tx).await?;
        anyhow::Ok(())
    };
    let build_installer_and_connect = async move {
        let tmp_dir = Builder::new().tempdir()?;
        let uds_socket_filename =
            format!("{}/installer.sock", tmp_dir.into_path().to_str().unwrap());

        // FIXME: The random unused tcp port might be available when get_random_tcp_port() is called,
        // but when the installer tries to bind on it, someone else might bind on it.
        // TODO: choose unused tcp port on installer side.
        // The way communication may happen:
        // 1. buck2 passes a temp file for a tcp port output.
        // 2. installer app choose unused tcp port and writes it into the passed file.
        // 3. buck2 reads tcp port from file and use it to connect to the installer app. (`connect_to_installer` function)
        let tcp_port = get_random_tcp_port()?;

        let installer_log_filename = format!(
            "{}/installer_{}_{}.log",
            install_log_dir,
            get_timestamp_as_string(),
            calculate_hash(&installer_label.target().name())
        );

        let mut installer_run_args: Vec<String> = initial_installer_run_args.to_vec();

        installer_run_args.extend(vec![
            "--named-pipe".to_owned(),
            uds_socket_filename.to_owned(),
            "--tcp-port".to_owned(),
            tcp_port.to_string(),
            "--log-path".to_owned(),
            installer_log_filename.to_owned(),
        ]);

        build_launch_installer(ctx, installer_label, &installer_run_args).await?;

        let client: InstallerClient<Channel> =
            connect_to_installer(uds_socket_filename.to_owned(), tcp_port).await?;
        let artifact_fs = ctx.get_artifact_fs().await?;

        for (install_id, install_files) in install_files_slice {
            send_install_info(client.clone(), install_id, install_files, &artifact_fs).await?;
        }

        let send_files_result = tokio_stream::wrappers::UnboundedReceiverStream::new(files_rx)
            .map(anyhow::Ok)
            .try_for_each_concurrent(None, |file| send_file(file, &artifact_fs, client.clone()))
            .await;
        send_shutdown_command(client.clone()).await?;
        send_files_result.context("Failed to send artifacts to installer")?;
        anyhow::Ok(())
    };
    try_join(build_installer_and_connect, build_files).await?;
    anyhow::Ok(())
}

async fn send_install_info(
    mut client: InstallerClient<Channel>,
    install_id: &str,
    install_files: &IndexMap<&str, Artifact>,
    artifact_fs: &ArtifactFs,
) -> anyhow::Result<()> {
    let mut files_map = HashMap::new();
    for (file_name, artifact) in install_files {
        let artifact_path = &artifact_fs
            .fs()
            .resolve(&artifact_fs.resolve(artifact.get_path())?);
        files_map
            .entry((*file_name).to_owned())
            .or_insert_with(|| artifact_path.to_string());
    }

    let install_info_request = tonic::Request::new(InstallInfoRequest {
        install_id: install_id.to_owned(),
        files: files_map,
    });

    let response_result = client.install(install_info_request).await;
    let install_info_response = match response_result {
        Ok(r) => r.into_inner(),
        Err(status) => {
            return Err(InstallError::InternalInstallerFailure {
                install_id: install_id.to_owned(),
                err: status.message().to_owned(),
            }
            .into());
        }
    };

    if install_info_response.install_id != install_id {
        send_shutdown_command(client.clone()).await?;
        return Err(anyhow::anyhow!(
            "Received install id: {} doesn't match with the sent one: {}",
            install_info_response.install_id,
            &install_id
        ));
    }

    Ok(())
}

async fn send_shutdown_command(mut client: InstallerClient<Channel>) -> anyhow::Result<()> {
    let response_result = client
        .shutdown_server(tonic::Request::new(ShutdownRequest {}))
        .await;

    return match response_result {
        Ok(_) => Ok(()),
        Err(status) => Err(InstallError::InstallerCommunicationFailure {
            err: status.message().to_owned(),
        }
        .into()),
    };
}

async fn build_launch_installer<'a>(
    ctx: &'a DiceComputations,
    providers_label: &ConfiguredProvidersLabel,
    installer_run_args: &[String],
) -> anyhow::Result<()> {
    let frozen_providers = ctx
        .get_providers(providers_label)
        .await?
        .require_compatible()?;

    if let Some(installer_run_info) =
        RunInfo::from_providers(frozen_providers.provider_collection())
    {
        let (inputs, run_args) = {
            let artifact_fs = ctx.get_artifact_fs().await?;
            let mut artifact_visitor = SimpleCommandLineArtifactVisitor::new();
            installer_run_info.visit_artifacts(&mut artifact_visitor)?;
            // Produce arguments for local platform.
            let path_separator = if cfg!(windows) {
                PathSeparatorKind::Windows
            } else {
                PathSeparatorKind::Unix
            };
            let executor_fs = ExecutorFs::new(&artifact_fs, path_separator);
            let mut cli = AbsCommandLineBuilder::new(&executor_fs);
            installer_run_info.add_to_command_line(&mut cli)?;
            let run_args = cli.build();
            (artifact_visitor.inputs, run_args)
        };
        // returns IndexMap<ArtifactGroup,ArtifactGroupValues>;
        try_join_all(inputs.into_iter().map(|input| async move {
            materialize_artifact_group(ctx, &input)
                .await
                .map(|value| (input, value))
        }))
        .await
        .context("Failed to build installer")?;
        background_command(&run_args[0])
            .args(&run_args[1..])
            .args(installer_run_args)
            .stdout(get_stdio()?)
            .stderr(get_stdio()?)
            .spawn()
            .context("Failed to spawn installer")?;

        Ok(())
    } else {
        Err(InstallError::NoRunInfoProvider(providers_label.target().name().to_owned()).into())
    }
}

fn get_stdio() -> anyhow::Result<Stdio> {
    // TODO: handle exit code, std.out and std.err from the install app command,
    // and print outputs in case exit code is not equals 0

    let log_level = match std::env::var("BUCK_LOG") {
        Ok(v) => v,
        Err(_) => "warn".to_owned(),
    };
    let level_filter = EnvFilter::try_new(log_level)?;
    if level_filter.max_level_hint().unwrap() > LevelFilter::INFO {
        return Ok(Stdio::inherit());
    }
    Ok(Stdio::null())
}

#[derive(Debug)]
pub struct FileResult {
    install_id: String,
    name: String,
    artifact: Artifact,
    artifact_value: ArtifactValue,
}

async fn build_files(
    ctx: &DiceComputations,
    install_files_slice: &[(&String, IndexMap<&str, Artifact>)],
    tx: mpsc::UnboundedSender<FileResult>,
) -> anyhow::Result<()> {
    let mut file_outputs = Vec::with_capacity(install_files_slice.len());
    for (install_id, file_info) in install_files_slice {
        for (name, artifact) in file_info.into_iter() {
            file_outputs.push((
                install_id,
                name,
                ArtifactGroup::Artifact(artifact.to_owned()),
                tx.clone(),
            ));
        }
    }

    try_join_all(file_outputs.into_iter().map(
        |(install_id, name, artifact, tx_clone)| async move {
            let artifact_values = materialize_artifact_group(ctx, &artifact).await?;
            for (artifact, artifact_value) in artifact_values.iter() {
                let file_result = FileResult {
                    install_id: (*install_id).to_owned(),
                    name: (*name).to_owned(),
                    artifact: artifact.to_owned(),
                    artifact_value: artifact_value.to_owned(),
                };
                tx_clone.send(file_result)?;
            }
            anyhow::Ok(())
        },
    ))
    .await?;
    Ok(())
}

// TODO @lebentle extract out and make some sort of util
async fn materialize_artifact_group(
    ctx: &DiceComputations,
    artifact_group: &ArtifactGroup,
) -> anyhow::Result<ArtifactGroupValues> {
    let values = ctx
        .ensure_artifact_group(artifact_group)
        .await
        .context("Failed to produce artifacts")?;

    try_join_all(values.iter().filter_map(|(artifact, _value)| {
        if let BaseArtifactKind::Build(artifact) = artifact.as_parts().0 {
            Some(ctx.try_materialize_requested_artifact(artifact, true))
        } else {
            None
        }
    }))
    .await
    .context("Failed to materialize artifacts")?;
    Ok(values)
}

#[cfg(unix)]
async fn connect_to_installer(
    unix_socket: String,
    tcp_port: u16,
) -> anyhow::Result<InstallerClient<Channel>> {
    use std::time::Duration;

    use buck2_common::client_utils::get_channel;
    use buck2_common::client_utils::retrying;
    use buck2_common::client_utils::ConnectionType;

    // These numbers might need to be configured based on the installer
    let initial_delay = Duration::from_millis(100);
    let max_delay = Duration::from_millis(500);
    let timeout = Duration::from_secs(5);

    // try to connect using uds first
    let attempt_channel = retrying(initial_delay, max_delay, timeout, async || {
        get_channel(
            ConnectionType::Uds {
                unix_socket: unix_socket.to_owned(),
            },
            false,
        )
        .await
    })
    .await;
    let channel = match attempt_channel {
        Ok(channel) => channel,
        Err(err) => {
            eprintln!(
                "Failed to connect with UDS: {:#} Falling back to TCP on port: {}",
                err, tcp_port
            );
            retrying(initial_delay, max_delay, timeout, async || {
                get_channel(
                    ConnectionType::Tcp {
                        socket: DEFAULT_SOCKET_ADDR.to_owned(),
                        port: tcp_port.to_string(),
                    },
                    false,
                )
                .await
            })
            .await
            .context("Failed to connect to with TCP and UDS")?
        }
    };
    Ok(InstallerClient::new(channel))
}

#[cfg(windows)]
async fn connect_to_installer(
    _unix_socket: String,
    tcp_port: u16,
) -> anyhow::Result<InstallerClient<Channel>> {
    use tonic::transport::Endpoint;
    let url = format!("http://{}:{}", DEFAULT_SOCKET_ADDR, tcp_port);
    println!("Trying to connect using TCP port: {}", tcp_port);

    let channel = Endpoint::try_from(url)?
        .connect()
        .await
        .context("Failed to connect to installer domain socket file")?;
    let client = InstallerClient::new(channel);
    Ok(client)
}
async fn send_file(
    file: FileResult,
    artifact_fs: &ArtifactFs,
    mut client: InstallerClient<Channel>,
) -> anyhow::Result<()> {
    let install_id = file.install_id;
    let name = file.name;
    let artifact = file.artifact;
    let sha1: String = match &file.artifact_value.entry() {
        DirectoryEntry::Dir(dir) => dir.fingerprint().to_string(),
        // todo(@lebentle) Use for now to unblock exopackage,
        // but should follow symlink and validate the target exists and send that
        DirectoryEntry::Leaf(ActionDirectoryMember::Symlink(symlink)) => {
            format!("re-symlink:{}", symlink.target().as_str())
        }
        DirectoryEntry::Leaf(ActionDirectoryMember::File(file)) => file.digest.to_string(),
        _ => return Err(InstallError::ErrorRetrievingHash(name).into()),
    };
    let (sha1, _size) = sha1.split1(":");
    let path = &artifact_fs
        .fs()
        .resolve(&artifact_fs.resolve(artifact.get_path())?);
    let request = tonic::Request::new(FileReadyRequest {
        install_id: install_id.to_owned(),
        name: name.to_owned(),
        sha1: sha1.to_owned(),
        path: path.to_string(),
    });
    let response_result = client.file_ready(request).await;
    let response = match response_result {
        Ok(r) => r.into_inner(),
        Err(status) => {
            return Err(InstallError::ProcessingFileReadyFailure {
                install_id: install_id.to_owned(),
                artifact: name,
                path: path.to_owned(),
                err: status.message().to_owned(),
            }
            .into());
        }
    };

    if response.install_id != install_id {
        return Err(InstallError::ProcessingFileReadyFailure {
            install_id: install_id.to_owned(),
            artifact: name,
            path: path.to_owned(),
            err: format!(
                "Received install id: {} doesn't match with the sent one: {}",
                response.install_id, &install_id
            ),
        }
        .into());
    }

    if let Some(error_detail) = response.error_detail {
        return Err(InstallError::ProcessingFileReadyFailure {
            install_id: install_id.to_owned(),
            artifact: name,
            path: path.to_owned(),
            err: error_detail.message,
        }
        .into());
    }
    Ok(())
}
