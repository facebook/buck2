/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    process::{Command, Stdio},
    time::Duration,
};

use anyhow::Context;
use buck2_build_api::{
    actions::{
        artifact::{Artifact, ArtifactFs, ArtifactKind, ArtifactValue},
        directory::ActionDirectoryMember,
    },
    artifact_groups::{ArtifactGroup, ArtifactGroupValues},
    calculation::Calculation,
    execute::materializer::ArtifactMaterializer,
    interpreter::rule_defs::{
        cmd_args::builder::{
            AbsCommandLineBuilder, CommandLineArgLike, SimpleCommandLineArtifactVisitor,
        },
        provider::install_info::*,
    },
};
use buck2_common::dice::{cells::HasCellResolver, file_ops::HasFileOps};
use buck2_core::{
    directory::DirectoryEntry,
    fs::paths::AbsPathBuf,
    package::Package,
    provider::{ProvidersLabel, ProvidersName},
    target::{TargetLabel, TargetName},
};
use buck2_interpreter::pattern::*;
use cli_proto::{InstallRequest, InstallResponse};
use dice::DiceComputations;
use futures::{
    future::{self, try_join},
    stream::{StreamExt, TryStreamExt},
};
use gazebo::prelude::*;
use indexmap::IndexMap;
use install_proto::{installer_client::InstallerClient, FileReady, Shutdown};
use rand::{distributions::Alphanumeric, thread_rng, Rng};
use starlark::values::FrozenRef;
use thiserror::Error;
use tokio::sync::mpsc;
use tonic::transport::{Channel, Endpoint};

use crate::daemon::{
    client_utils::retrying,
    common::{parse_patterns_from_cli_args, resolve_patterns, target_platform_from_client_context},
    server::ServerCommandContext,
};

#[derive(Debug, Error)]
pub enum InstallError {
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
    #[error("Error locating `{artifact}` located at `{path}`")]
    MissingArtifact { artifact: String, path: AbsPathBuf },
}

pub async fn install(
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
        let rand_string: String = thread_rng()
            .sample_iter(&Alphanumeric)
            .take(4)
            .map(char::from)
            .collect();
        // TODO: @lebentle change this to be in the same directory of the buck.uds
        let socket = format!("/tmp/installer{}.sock", rand_string);
        installer_run_args.extend(vec!["--named-pipe".to_owned(), socket.clone()]);
        build_launch_installer(ctx, install_info, installer_run_args).await?;

        // These numbers might need to be configured based on the installer
        // Current time seems to be 0m0.727s for run from buck2
        let client = retrying(
            Duration::from_millis(500),
            Duration::from_millis(500),
            Duration::from_secs(5),
            async || connect_to_installer(&socket).await,
        )
        .await?;
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
    Ok(())
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
        let mut cli = AbsCommandLineBuilder::new(&artifact_fs);
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
pub struct FileResult {
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
        if let ArtifactKind::Build(artifact) = artifact.0.as_ref() {
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
async fn connect_to_installer(socket: &str) -> anyhow::Result<InstallerClient<Channel>> {
    use tonic::transport::Uri;
    use tower::service_fn;
    let io: tokio::net::UnixStream = {
        tokio::net::UnixStream::connect(socket)
            .await
            .with_context(|| format!("Failed to connect to unix domain socket '{}'", socket))?
    };
    let mut io = Some(io);
    let channel = Endpoint::try_from("http://[::]:50055")?
        .connect_with_connector(service_fn(move |_: Uri| {
            let io = io
                .take()
                .context("Cannot reconnect after connection loss to uds");
            future::ready(io)
        }))
        .await
        .context("Failed to connect to installer domain socket file")?;
    let client = InstallerClient::new(channel);
    Ok(client)
}

#[cfg(windows)]
async fn connect_to_installer(_socket: &str) -> anyhow::Result<InstallerClient<Channel>> {
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
