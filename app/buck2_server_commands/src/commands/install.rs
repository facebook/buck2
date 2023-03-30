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
use std::net::Ipv4Addr;
use std::net::SocketAddr;
use std::net::TcpListener;
use std::process::Stdio;

use anyhow::Context;
use async_trait::async_trait;
use buck2_build_api::actions::artifact::artifact_type::Artifact;
use buck2_build_api::actions::artifact::artifact_type::BaseArtifactKind;
use buck2_build_api::actions::artifact::materializer::ArtifactMaterializer;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::artifact_groups::ArtifactGroupValues;
use buck2_build_api::calculation::Calculation;
use buck2_build_api::context::HasBuildContextData;
use buck2_build_api::interpreter::rule_defs::cmd_args::AbsCommandLineContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use buck2_build_api::interpreter::rule_defs::cmd_args::SimpleCommandLineArtifactVisitor;
use buck2_build_api::interpreter::rule_defs::provider::builtin::install_info::InstallInfoCallable;
use buck2_build_api::interpreter::rule_defs::provider::builtin::run_info::RunInfo;
use buck2_cli_proto::InstallRequest;
use buck2_cli_proto::InstallResponse;
use buck2_common::client_utils::get_channel_tcp;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::executor_config::PathSeparatorKind;
use buck2_common::file_ops::FileDigest;
use buck2_common::pattern::resolve::resolve_target_patterns;
use buck2_core::directory::DirectoryEntry;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_core::package::PackageLabel;
use buck2_core::pattern::pattern_type::ConfiguredProvidersPatternExtra;
use buck2_core::pattern::pattern_type::ProvidersPatternExtra;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_core::target::name::TargetName;
use buck2_data::InstallEventInfoEnd;
use buck2_data::InstallEventInfoStart;
use buck2_events::dispatch::span_async;
use buck2_execute::artifact::artifact_dyn::ArtifactDyn;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_execute::materialize::materializer::HasMaterializer;
use buck2_install_proto::installer_client::InstallerClient;
use buck2_install_proto::FileReadyRequest;
use buck2_install_proto::InstallInfoRequest;
use buck2_install_proto::ShutdownRequest;
use buck2_interpreter_for_build::interpreter::calculation::InterpreterCalculation;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::NoPartialResult;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::pattern::parse_patterns_from_cli_args;
use buck2_server_ctx::pattern::target_platform_from_client_context;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use buck2_util::process::background_command;
use chrono::DateTime;
use chrono::NaiveDateTime;
use chrono::Utc;
use dice::DiceComputations;
use dice::DiceTransaction;
use dupe::Dupe;
use futures::future::try_join;
use futures::future::try_join_all;
use futures::future::FutureExt;
use futures::stream::StreamExt;
use futures::stream::TryStreamExt;
use starlark_map::small_map::SmallMap;
use tokio::sync::mpsc;
use tonic::transport::Channel;

#[derive(Debug, thiserror::Error)]
pub enum InstallError {
    #[error("Target {1}:{0} cannot be installed as it does not expose an InstallInfo provider")]
    NoInstallProvider(TargetName, PackageLabel),

    #[error("Installer target `{0}` doesn't expose RunInfo provider")]
    NoRunInfoProvider(TargetName),

    #[error(
        "Installer failed to process file ready request for `{install_id}`. Artifact: `{artifact}` located at `{path}`. Error message: `{err}`\n. More details can be found at `{installer_log}`"
    )]
    ProcessingFileReadyFailure {
        install_id: String,
        artifact: String,
        path: AbsNormPathBuf,
        err: String,
        installer_log: String,
    },

    #[error("Installer failed for `{install_id}` with `{err}`")]
    InternalInstallerFailure { install_id: String, err: String },

    #[error("Communication with the installer failed with `{err}`")]
    InstallerCommunicationFailure { err: String },

    #[error("Incorrect seconds/nanos argument")]
    NativeDateTime,
}

async fn get_installer_log_directory(
    server_ctx: &dyn ServerCommandContextTrait,
    ctx: &DiceComputations,
) -> anyhow::Result<AbsNormPathBuf> {
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
    partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
    req: InstallRequest,
) -> anyhow::Result<InstallResponse> {
    run_server_command(InstallServerCommand { req }, ctx, partial_result_dispatcher).await
}

struct InstallServerCommand {
    req: InstallRequest,
}

#[async_trait]
impl ServerCommandTemplate for InstallServerCommand {
    type StartEvent = buck2_data::InstallCommandStart;
    type EndEvent = buck2_data::InstallCommandEnd;
    type Response = InstallResponse;
    type PartialResult = NoPartialResult;

    fn end_event(&self, _response: &anyhow::Result<Self::Response>) -> Self::EndEvent {
        buck2_data::InstallCommandEnd {
            unresolved_target_patterns: self.req.target_patterns.clone(),
        }
    }

    async fn command<'v>(
        &self,
        server_ctx: &'v dyn ServerCommandContextTrait,
        _partial_result_dispatcher: PartialResultDispatcher<Self::PartialResult>,
        ctx: DiceTransaction,
    ) -> anyhow::Result<Self::Response> {
        install(server_ctx, ctx, &self.req).await
    }

    fn is_success(&self, _response: &Self::Response) -> bool {
        // No response if we failed.
        true
    }
}

async fn install(
    server_ctx: &dyn ServerCommandContextTrait,
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
    let parsed_patterns = parse_patterns_from_cli_args::<ConfiguredProvidersPatternExtra>(
        &ctx,
        &request.target_patterns,
        cwd,
    )
    .await?;
    server_ctx.log_target_pattern(&parsed_patterns);

    ctx.per_transaction_data()
        .get_materializer()
        .log_materializer_state(server_ctx.events());

    let resolved_pattern =
        resolve_target_patterns(&cell_resolver, &parsed_patterns, &ctx.file_ops()).await?;

    let resolved_pattern = resolved_pattern
        .convert_pattern()
        .context("Install with explicit configuration pattern is not supported yet")?;

    let mut installer_to_files_map = HashMap::new();
    for (package, spec) in resolved_pattern.specs {
        let ctx = &ctx;
        let targets: Vec<(TargetName, ProvidersPatternExtra)> = match spec {
            buck2_core::pattern::PackageSpec::Targets(targets) => targets,
            buck2_core::pattern::PackageSpec::All => {
                let interpreter_results = ctx.get_interpreter_results(package.dupe()).await?;
                interpreter_results
                    .targets()
                    .keys()
                    .map(|target| {
                        (
                            target.to_owned(),
                            ProvidersPatternExtra {
                                providers: ProvidersName::Default,
                            },
                        )
                    })
                    .collect()
            }
        };
        for (target_name, providers) in targets {
            let label = providers.into_providers_label(package.dupe(), target_name.as_ref());
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
                        label.target().name().to_owned(),
                        package.dupe(),
                    )
                    .into());
                }
            };
        }
    }

    let install_log_dir = &get_installer_log_directory(server_ctx, &ctx).await?;

    let mut install_requests = Vec::with_capacity(installer_to_files_map.len());
    for (installer_label, install_info_vector) in &installer_to_files_map {
        let ctx = &ctx;
        let installer_run_args = &request.installer_run_args;

        let mut install_files_vector: Vec<(&String, SmallMap<_, _>)> = Vec::new();
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
                request.installer_debug,
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

fn get_timestamp_as_string() -> anyhow::Result<String> {
    let nt = NaiveDateTime::from_timestamp_opt(Utc::now().timestamp(), 0)
        .context(InstallError::NativeDateTime)?;
    let dt: DateTime<Utc> = DateTime::from_utc(nt, Utc);
    Ok(dt.format("%Y%m%d-%H%M%S").to_string())
}

fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}

async fn handle_install_request<'a>(
    ctx: &'a DiceComputations,
    install_log_dir: &AbsNormPathBuf,
    install_files_slice: &[(&String, SmallMap<&str, Artifact>)],
    installer_label: &ConfiguredProvidersLabel,
    initial_installer_run_args: &[String],
    installer_debug: bool,
) -> anyhow::Result<()> {
    let (files_tx, files_rx) = mpsc::unbounded_channel();
    let build_files = async move {
        build_files(ctx, install_files_slice, files_tx).await?;
        anyhow::Ok(())
    };
    let build_installer_and_connect = async move {
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
            get_timestamp_as_string()?,
            calculate_hash(&installer_label.target().name())
        );

        let mut installer_run_args: Vec<String> = initial_installer_run_args.to_vec();

        installer_run_args.extend(vec![
            "--tcp-port".to_owned(),
            tcp_port.to_string(),
            "--log-path".to_owned(),
            installer_log_filename.to_owned(),
        ]);

        build_launch_installer(ctx, installer_label, &installer_run_args, installer_debug).await?;

        let client: InstallerClient<Channel> = connect_to_installer(tcp_port).await?;
        let artifact_fs = ctx.get_artifact_fs().await?;

        for (install_id, install_files) in install_files_slice {
            send_install_info(client.clone(), install_id, install_files, &artifact_fs).await?;
        }

        let send_files_result = tokio_stream::wrappers::UnboundedReceiverStream::new(files_rx)
            .map(anyhow::Ok)
            .try_for_each_concurrent(None, |file| {
                send_file(
                    file,
                    &artifact_fs,
                    client.clone(),
                    installer_log_filename.to_owned(),
                )
            })
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
    install_files: &SmallMap<&str, Artifact>,
    artifact_fs: &ArtifactFs,
) -> anyhow::Result<()> {
    let mut files_map = HashMap::new();
    for (file_name, artifact) in install_files {
        let artifact_path = &artifact_fs
            .fs()
            .resolve(&artifact.resolve_path(artifact_fs)?);
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
    installer_log_console: bool,
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
            let mut run_args = Vec::<String>::new();
            let mut ctx = AbsCommandLineContext::new(&executor_fs);
            installer_run_info.add_to_command_line(&mut run_args, &mut ctx)?;
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
            .stderr(get_stdio(installer_log_console)?)
            .spawn()
            .context("Failed to spawn installer")?;

        Ok(())
    } else {
        Err(InstallError::NoRunInfoProvider(providers_label.target().name().to_owned()).into())
    }
}

fn get_stdio(log_installer_console: bool) -> anyhow::Result<Stdio> {
    if log_installer_console {
        Ok(Stdio::inherit())
    } else {
        Ok(Stdio::null())
    }
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
    install_files_slice: &[(&String, SmallMap<&str, Artifact>)],
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
    let values = ctx.ensure_artifact_group(artifact_group).await?;

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

async fn connect_to_installer(tcp_port: u16) -> anyhow::Result<InstallerClient<Channel>> {
    use std::time::Duration;

    use buck2_common::client_utils::retrying;

    // These numbers might need to be configured based on the installer
    let initial_delay = Duration::from_millis(100);
    let max_delay = Duration::from_millis(500);
    let timeout = Duration::from_secs(120);

    span_async(
        buck2_data::ConnectToInstallerStart {
            tcp_port: tcp_port.into(),
        },
        async move {
            let channel = retrying(initial_delay, max_delay, timeout, async || {
                get_channel_tcp(Ipv4Addr::LOCALHOST, tcp_port).await
            })
            .await
            .context("Failed to connect to the installer using TCP")?;

            Ok(InstallerClient::new(channel))
        }
        .map(|res| (res, buck2_data::ConnectToInstallerEnd {})),
    )
    .await
}

async fn send_file(
    file: FileResult,
    artifact_fs: &ArtifactFs,
    mut client: InstallerClient<Channel>,
    install_log: String,
) -> anyhow::Result<()> {
    let install_id = file.install_id;
    let name = file.name;
    let artifact = file.artifact;

    enum Data<'a> {
        Digest(&'a FileDigest), // NOTE: A misnommer, this is rather BlobDigest.
        Symlink(String),
    }

    let data = match &file.artifact_value.entry() {
        DirectoryEntry::Dir(dir) => Data::Digest(dir.fingerprint().data()),
        DirectoryEntry::Leaf(ActionDirectoryMember::File(file)) => Data::Digest(file.digest.data()),
        DirectoryEntry::Leaf(ActionDirectoryMember::Symlink(symlink)) => {
            // todo(@lebentle) Use for now to unblock exopackage,
            // but should follow symlink and validate the target exists and send that
            Data::Symlink(symlink.target().as_str().to_owned())
        }
        DirectoryEntry::Leaf(ActionDirectoryMember::ExternalSymlink(symlink)) => {
            Data::Symlink(symlink.with_full_target()?.target_str().to_owned())
        }
    };

    let (digest, size, digest_algorithm) = match data {
        Data::Digest(d) => (
            d.raw_digest().to_string(),
            d.size(),
            d.raw_digest().algorithm().to_string(),
        ),
        Data::Symlink(sym) => (format!("re-symlink:{}", sym), 0, "".to_owned()), // Messy :(
    };

    let path = &artifact_fs
        .fs()
        .resolve(&artifact.resolve_path(artifact_fs)?);
    let request = tonic::Request::new(FileReadyRequest {
        install_id: install_id.to_owned(),
        name: name.to_owned(),
        digest,
        digest_algorithm,
        size,
        path: path.to_string(),
    });

    let start = InstallEventInfoStart {
        artifact_name: name.to_owned(),
        file_path: path.to_string(),
    };
    let end = InstallEventInfoEnd {};
    span_async(start, async {
        let mut outcome: anyhow::Result<()> = Ok(());
        let response_result = client.file_ready(request).await;
        let response = match response_result {
            Ok(r) => r.into_inner(),
            Err(status) => {
                return (
                    Err(InstallError::ProcessingFileReadyFailure {
                        install_id: install_id.to_owned(),
                        artifact: name,
                        path: path.to_owned(),
                        err: status.message().to_owned(),
                        installer_log: install_log.to_owned(),
                    }
                    .into()),
                    end,
                );
            }
        };

        if response.install_id != install_id {
            outcome = Err(InstallError::ProcessingFileReadyFailure {
                install_id: install_id.to_owned(),
                artifact: name.to_owned(),
                path: path.to_owned(),
                err: format!(
                    "Received install id: {} doesn't match with the sent one: {}",
                    response.install_id, &install_id
                ),
                installer_log: install_log.to_owned(),
            }
            .into());
        }

        if let Some(error_detail) = response.error_detail {
            outcome = Err(InstallError::ProcessingFileReadyFailure {
                install_id: install_id.to_owned(),
                artifact: name.to_owned(),
                path: path.to_owned(),
                err: error_detail.message,
                installer_log: install_log.to_owned(),
            }
            .into());
        }
        (outcome, end)
    })
    .await?;
    Ok(())
}
