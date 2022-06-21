/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::process::Command;
use std::process::Stdio;

use anyhow::Context;
use buck2_build_api::actions::artifact::Artifact;
use buck2_build_api::actions::artifact::ArtifactFs;
use buck2_build_api::actions::artifact::ArtifactValue;
use buck2_build_api::actions::artifact::BaseArtifactKind;
use buck2_build_api::actions::artifact::ExecutorFs;
use buck2_build_api::actions::directory::ActionDirectoryMember;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::artifact_groups::ArtifactGroupValues;
use buck2_build_api::calculation::Calculation;
use buck2_build_api::execute::materializer::ArtifactMaterializer;
use buck2_build_api::execute::PathSeparatorKind;
use buck2_build_api::interpreter::rule_defs::cmd_args::AbsCommandLineBuilder;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use buck2_build_api::interpreter::rule_defs::cmd_args::SimpleCommandLineArtifactVisitor;
use buck2_build_api::interpreter::rule_defs::provider::builtin::install_info::*;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_core::directory::DirectoryEntry;
use buck2_core::fs::paths::AbsPathBuf;
use buck2_core::package::Package;
use buck2_core::provider::ProvidersLabel;
use buck2_core::provider::ProvidersName;
use buck2_core::target::TargetLabel;
use buck2_core::target::TargetName;
use buck2_interpreter::pattern::*;
use cli_proto::InstallRequest;
use cli_proto::InstallResponse;
use dice::DiceComputations;
use futures::future::try_join;
use futures::future::{self};
use futures::stream::StreamExt;
use futures::stream::TryStreamExt;
use gazebo::prelude::StrExt;
use gazebo::prelude::*;
use indexmap::IndexMap;
use install_proto::installer_client::InstallerClient;
use install_proto::FileReady;
use install_proto::Shutdown;
use starlark::values::FrozenRef;
use tempfile::Builder;
use thiserror::Error;
use tokio::sync::mpsc;
use tonic::transport::Channel;

use crate::daemon::common::parse_patterns_from_cli_args;
use crate::daemon::common::resolve_patterns;
use crate::daemon::common::target_platform_from_client_context;
use crate::daemon::server::ServerCommandContext;

pub static DEFAULT_PORT: &str = "50055";
pub static DEFAULT_SOCKET_ADDR: &str = "0.0.0.0";

#[derive(Debug, Error)]
pub(crate) enum InstallError {
    #[error("Not install type `{0}` from package `{1}`")]
    NoInstallProvider(TargetName, Package),
    #[error("Error retrieving hash for `{0}`")]
    ErrorRetrievingHash(String),
    #[error("Installer failed for `{artifact}` located at `{path}` with `{err}`")]
    InstallerFailure {
        artifact: String,
        path: AbsPathBuf,
        err: String,
    },
}

pub(crate) async fn install(
    server_ctx: ServerCommandContext,
    request: InstallRequest,
) -> anyhow::Result<InstallResponse> {
    let cwd = &server_ctx.working_dir;
    let global_target_platform =
        target_platform_from_client_context(request.context.as_ref(), &server_ctx).await?;

    let ctx = server_ctx.dice_ctx().await?;

    let cell_resolver = ctx.get_cell_resolver().await;

    // Note <TargetName> does not return the providers
    let parsed_patterns =
        parse_patterns_from_cli_args::<ProvidersPattern>(&request.target_patterns, &ctx, cwd)
            .await?;
    let resolved_pattern =
        resolve_patterns(&parsed_patterns, &cell_resolver, &ctx.file_ops()).await?;
    for (package, spec) in resolved_pattern.specs {
        let ctx = &ctx;
        let targets: anyhow::Result<Vec<ProvidersPattern>> = match spec {
            buck2_interpreter::pattern::PackageSpec::Targets(targets) => Ok(targets),
            buck2_interpreter::pattern::PackageSpec::All => {
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
            let target = ctx
                .get_configured_target(&label, global_target_platform.dupe().as_ref())
                .await?;
            let frozen_providers = ctx.get_providers(&target).await?.require_compatible()?;
            let providers = frozen_providers.provider_collection();
            match providers.get_provider(InstallInfoCallable::provider_id_t()) {
                Some(install_info) => {
                    // https://github.com/rust-lang/rust/issues/63033#issuecomment-521234696
                    let mut installer_run_args = Vec::new();
                    for arg in &request.installer_run_args {
                        installer_run_args.push(arg.clone());
                    }
                    build_install(ctx, install_info, installer_run_args).await?;
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
    Ok(InstallResponse {})
}

async fn build_install(
    ctx: &DiceComputations,
    install_info: FrozenRef<'static, FrozenInstallInfo>,
    mut installer_run_args: Vec<String>,
) -> anyhow::Result<()> {
    let (files_tx, files_rx) = mpsc::unbounded_channel();
    let build_files = async move {
        build_files(ctx, install_info.get_files()?, files_tx).await?;
        anyhow::Ok(())
    };
    let build_installer_and_connect = async move {
        let tmp_dir = Builder::new().prefix("buck2_install").tempdir()?;
        let filename = format!("{}/installer.sock", tmp_dir.into_path().to_str().unwrap());

        installer_run_args.extend(vec!["--named-pipe".to_owned(), filename.to_owned()]);
        build_launch_installer(ctx, install_info, installer_run_args).await?;

        // These numbers might need to be configured based on the installer
        // Current time seems to be 0m0.727s for run from buck2
        let client: InstallerClient<Channel> = connect_to_installer(filename.to_owned()).await?;
        let artifact_fs = ctx.get_artifact_fs().await;
        let ret = tokio_stream::wrappers::UnboundedReceiverStream::new(files_rx)
            .map(anyhow::Ok)
            .try_for_each_concurrent(None, |file| send_file(file, &artifact_fs, client.clone()))
            .await;
        client
            .clone()
            .shutdown_server(tonic::Request::new(Shutdown {}))
            .await?;
        ret.context("Failed to send artifacts to installer")?;
        anyhow::Ok(())
    };
    try_join(build_installer_and_connect, build_files).await?;
    anyhow::Ok(())
}

async fn build_launch_installer(
    ctx: &DiceComputations,
    install_info: FrozenRef<'static, FrozenInstallInfo>,
    installer_run_args: Vec<String>,
) -> anyhow::Result<()> {
    let installer_run_info = install_info.get_installer().to_owned();
    let (inputs, run_args) = {
        let artifact_fs = ctx.get_artifact_fs().await;
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
    future::try_join_all(inputs.into_iter().map(|input| async move {
        materialize_artifact_group(ctx, &input)
            .await
            .map(|value| (input, value))
    }))
    .await
    .context("Failed to build installer")?;
    Command::new(&run_args[0])
        .args(&run_args[1..])
        .args(installer_run_args)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .context("Failed to spawn installer")?;
    Ok(())
}

#[derive(Debug)]
pub(crate) struct FileResult {
    name: String,
    artifact: Artifact,
    artifact_value: ArtifactValue,
}

async fn build_files(
    ctx: &DiceComputations,
    files: IndexMap<&str, Artifact>,
    tx: mpsc::UnboundedSender<FileResult>,
) -> anyhow::Result<()> {
    let mut file_outputs = Vec::new();
    for (name, artifact) in files {
        file_outputs.push((name, ArtifactGroup::Artifact(artifact), tx.clone()));
    }
    future::try_join_all(file_outputs.into_iter().map(|input| async move {
        let (name, artifact, tx_clone) = input;
        let artifact_values = materialize_artifact_group(ctx, &artifact).await?;
        for (artifact, artifact_value) in artifact_values.iter() {
            let file_result = FileResult {
                name: name.to_owned(),
                artifact: artifact.to_owned(),
                artifact_value: artifact_value.to_owned(),
            };
            tx_clone.send(file_result)?;
        }
        anyhow::Ok(())
    }))
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

    future::try_join_all(values.iter().filter_map(|(artifact, _value)| {
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
async fn connect_to_installer(unix_socket: String) -> anyhow::Result<InstallerClient<Channel>> {
    use std::time::Duration;

    use super::client_utils::get_channel;
    use super::client_utils::retrying;
    use super::client_utils::ConnectionType;

    // try to connect using uds first
    let attempt_channel = retrying(
        Duration::from_millis(500),
        Duration::from_millis(500),
        Duration::from_secs(5),
        async || {
            get_channel(ConnectionType::Uds {
                unix_socket: unix_socket.to_owned(),
            })
            .await
        },
    )
    .await;
    let channel = match attempt_channel {
        Ok(channel) => channel,
        Err(err) => {
            println!("Failed to connect with UDS: {:#} Falling back to TCP", err);
            retrying(
                Duration::from_millis(500),
                Duration::from_millis(500),
                Duration::from_secs(5),
                async || {
                    get_channel(ConnectionType::Tcp {
                        socket: DEFAULT_SOCKET_ADDR.to_owned(),
                        port: DEFAULT_PORT.to_owned(),
                    })
                    .await
                },
            )
            .await
            .context("Failed to connect to with TCP and UDS")?
        }
    };
    let client = InstallerClient::new(channel);
    Ok(client)
}

#[cfg(windows)]
async fn connect_to_installer(_unix_socket: String) -> anyhow::Result<InstallerClient<Channel>> {
    use tonic::transport::Endpoint;
    let channel = Endpoint::try_from("http://0.0.0.0:50055")?
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
    let name = file.name;
    let artifact = file.artifact;
    let sha1: String = match &file.artifact_value.entry() {
        DirectoryEntry::Dir(dir) => dir.fingerprint().to_string(),
        DirectoryEntry::Leaf(ActionDirectoryMember::File(file)) => file.digest.to_string(),
        _ => return Err(InstallError::ErrorRetrievingHash(name).into()),
    };
    let (sha1, _size) = sha1.split1(":");
    let path = &artifact_fs.fs().resolve(&artifact_fs.resolve(&artifact)?);
    let request = tonic::Request::new(FileReady {
        name: name.to_owned(),
        sha1: sha1.to_owned(),
        path: path.to_string(),
    });
    let response = client.file_ready_request(request).await?.into_inner();
    if response.err {
        return Err(InstallError::InstallerFailure {
            artifact: name,
            path: path.to_owned(),
            err: response.err_msg,
        }
        .into());
    }
    Ok(())
}
