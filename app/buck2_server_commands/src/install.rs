/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;
use std::net::Ipv4Addr;
use std::net::SocketAddr;
use std::net::TcpListener;
use std::process::Stdio;
use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;

use async_trait::async_trait;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_build_api::actions::artifact::get_artifact_fs::GetArtifactFs;
use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::context::HasBuildContextData;
use buck2_build_api::interpreter::rule_defs::cmd_args::AbsCommandLineContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::ArtifactPathMapperImpl;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use buck2_build_api::interpreter::rule_defs::cmd_args::SimpleCommandLineArtifactVisitor;
use buck2_build_api::interpreter::rule_defs::provider::builtin::install_info::FrozenInstallInfo;
use buck2_build_api::interpreter::rule_defs::provider::builtin::run_info::FrozenRunInfo;
use buck2_build_api::materialize::HasMaterializationQueueTracker;
use buck2_build_api::materialize::MaterializationAndUploadContext;
use buck2_build_api::materialize::materialize_and_upload_artifact_group;
use buck2_build_api::validation::validation_impl::VALIDATION_IMPL;
use buck2_cli_proto::InstallRequest;
use buck2_cli_proto::InstallResponse;
use buck2_common::client_utils::get_channel_tcp;
use buck2_common::client_utils::retrying;
use buck2_common::file_ops::metadata::FileDigest;
use buck2_common::manifold::Bucket;
use buck2_common::manifold::ManifoldClient;
use buck2_common::manifold::Ttl;
use buck2_common::pattern::parse_from_cli::parse_patterns_with_modifiers_from_cli_args;
use buck2_common::pattern::resolve::ResolveTargetPatterns;
use buck2_core::buck2_env;
use buck2_core::execution_types::executor_config::PathSeparatorKind;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::global_cfg_options::GlobalCfgOptions;
use buck2_core::package::PackageLabel;
use buck2_core::package::PackageLabelWithModifiers;
use buck2_core::pattern::pattern::ModifiersError;
use buck2_core::pattern::pattern::PackageSpec;
use buck2_core::pattern::pattern_type::ConfiguredProvidersPatternExtra;
use buck2_core::pattern::pattern_type::ProvidersPatternExtra;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_core::soft_error;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_core::target::name::TargetName;
use buck2_data::BuildResult;
use buck2_data::InstallEventInfoEnd;
use buck2_data::InstallEventInfoStart;
use buck2_directory::directory::entry::DirectoryEntry;
use buck2_error::BuckErrorContext;
use buck2_error::ErrorTag;
use buck2_error::internal_error;
use buck2_events::dispatch::get_dispatcher;
use buck2_events::dispatch::span_async;
use buck2_events::dispatch::span_async_simple;
use buck2_execute::artifact::artifact_dyn::ArtifactDyn;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_fs::fs_util;
use buck2_fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_fs::paths::file_name::FileName;
use buck2_fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_install_proto::DeviceMetadata;
use buck2_install_proto::FileReadyRequest;
use buck2_install_proto::InstallInfoRequest;
use buck2_install_proto::ShutdownRequest;
use buck2_install_proto::installer_client::InstallerClient;
use buck2_node::nodes::frontend::TargetGraphCalculation;
use buck2_node::target_calculation::ConfiguredTargetCalculation;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::global_cfg_options::global_cfg_options_from_client_context;
use buck2_server_ctx::partial_result_dispatcher::NoPartialResult;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::template::ServerCommandTemplate;
use buck2_server_ctx::template::run_server_command;
use buck2_util::future::try_join_all;
use buck2_util::process::background_command;
use chrono::DateTime;
use chrono::Utc;
use dice::DiceComputations;
use dice::DiceTransaction;
use dupe::Dupe;
use futures::future::FutureExt;
use futures::stream::StreamExt;
use futures::stream::TryStreamExt;
use starlark_map::small_map::SmallMap;
use tokio::sync::Mutex;
use tokio::sync::mpsc;
use tokio_stream::wrappers::UnboundedReceiverStream;
use tonic::transport::Channel;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Install)]
pub(crate) enum InstallError {
    /// Input errors from installer definition
    #[error("Target {1}:{0} cannot be installed as it does not expose an InstallInfo provider")]
    #[buck2(input)]
    NoInstallProvider(TargetName, PackageLabel),

    #[error("Installer target `{0}` doesn't expose RunInfo provider")]
    #[buck2(input)]
    NoRunInfoProvider(TargetName),

    /// Errors from external installer process, may represent infra errors or input errors (ex. no device).
    /// Tagging as input errors in the absence of a way for installers to report infra errors.
    #[error(
        "Installer failed to process file ready request for `{install_id}`. Artifact: `{artifact}` located at `{path}`. Error message: `{err}`\n."
    )]
    #[buck2(input)]
    ProcessingFileReadyFailure {
        install_id: String,
        artifact: String,
        path: AbsNormPathBuf,
        err: String,
    },

    #[error("Installer failed for `{install_id}` with `{err}`")]
    #[buck2(input)]
    InternalInstallerFailure { install_id: String, err: String },

    /// Infra errors
    #[error("Communication with the installer failed with `{err}`")]
    #[buck2(tier0)]
    InstallerCommunicationFailure { err: String },

    #[error("Timed out after {timeout:?} waiting for installer to {action}")]
    #[buck2(environment)]
    RequestTimeout { timeout: Duration, action: String },

    #[error(
        "Tried to use an artifact {artifact} with a content-based path for install, not currently supported!"
    )]
    #[buck2(input)]
    ContentBasedPath { artifact: Artifact },
}

async fn get_installer_log_directory(
    server_ctx: &dyn ServerCommandContextTrait,
    ctx: &mut DiceComputations<'_>,
) -> buck2_error::Result<AbsNormPathBuf> {
    let out_path = ctx.get_buck_out_path().await?;
    let filesystem = server_ctx.project_root();
    let buck_out_path = filesystem
        .root()
        .join(out_path.root().as_forward_relative_path());
    let install_log_dir = buck_out_path.join(ForwardRelativePathBuf::unchecked_new(
        "installer".to_owned(),
    ));
    fs_util::create_dir_all(&install_log_dir)?;
    Ok(install_log_dir)
}

pub(crate) async fn install_command(
    ctx: &dyn ServerCommandContextTrait,
    partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
    req: InstallRequest,
) -> buck2_error::Result<InstallResponse> {
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

    fn end_event(&self, _response: &buck2_error::Result<Self::Response>) -> Self::EndEvent {
        buck2_data::InstallCommandEnd {
            unresolved_target_patterns: self
                .req
                .target_patterns
                .iter()
                .map(|p| buck2_data::TargetPattern { value: p.clone() })
                .collect(),
        }
    }

    async fn command(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        _partial_result_dispatcher: PartialResultDispatcher<Self::PartialResult>,
        ctx: DiceTransaction,
    ) -> buck2_error::Result<Self::Response> {
        install(server_ctx, ctx, &self.req).await
    }

    fn build_result(&self, _response: &Self::Response) -> Option<BuildResult> {
        // TODO report this correctly
        Some(BuildResult {
            build_completed: true,
        })
    }
}

struct InstallRequestData<'a> {
    installer_label: ConfiguredProvidersLabel,
    installed_targets: Vec<(ConfiguredTargetLabel, SmallMap<&'a str, Artifact>)>,
}

fn install_id(installed_target: &ConfiguredTargetLabel) -> String {
    format!("{installed_target}")
}

async fn install(
    server_ctx: &dyn ServerCommandContextTrait,
    mut ctx: DiceTransaction,
    request: &InstallRequest,
) -> buck2_error::Result<InstallResponse> {
    let install_request_data_vec =
        collect_install_request_data(server_ctx, &mut ctx, request).await?;

    let install_log_dir = &get_installer_log_directory(server_ctx, &mut ctx).await?;

    let install_requests = install_request_data_vec.into_iter().map(|data| {
        let installer_run_args = &request.installer_run_args;
        DiceComputations::declare_closure(move |ctx| {
            async move {
                handle_install_request(
                    ctx,
                    install_log_dir,
                    &data,
                    installer_run_args,
                    request.installer_debug,
                )
                .await
            }
            .boxed()
        })
    });

    let install_requests = ctx.compute_many(install_requests);
    try_join_all(install_requests)
        .await
        .buck_error_context("Interaction with installer failed.")?;

    Ok(InstallResponse {})
}

async fn collect_install_request_data<'a>(
    server_ctx: &dyn ServerCommandContextTrait,
    ctx: &mut DiceTransaction,
    request: &InstallRequest,
) -> buck2_error::Result<impl IntoIterator<Item = InstallRequestData<'a>> + use<'a>> {
    let cwd = server_ctx.working_dir();

    let global_cfg_options = global_cfg_options_from_client_context(
        request
            .target_cfg
            .as_ref()
            .ok_or_else(|| internal_error!("target_cfg must be set"))?,
        server_ctx,
        ctx,
    )
    .await?;

    // Note <TargetName> does not return the providers
    let parsed_patterns_with_modifiers = parse_patterns_with_modifiers_from_cli_args::<
        ConfiguredProvidersPatternExtra,
    >(ctx, &request.target_patterns, cwd)
    .await?;
    server_ctx.log_target_pattern_with_modifiers(&parsed_patterns_with_modifiers);
    let resolved_pattern =
        ResolveTargetPatterns::resolve_with_modifiers(ctx, &parsed_patterns_with_modifiers).await?;

    let resolved_pattern = resolved_pattern
        .convert_pattern()
        .buck_error_context("Install with explicit configuration pattern is not supported yet")?;

    let mut installer_to_files_map = HashMap::new();
    for (package_with_modifiers, spec) in resolved_pattern.specs {
        let PackageLabelWithModifiers { package, modifiers } = package_with_modifiers;

        let targets: Vec<(TargetName, ProvidersPatternExtra)> = match spec {
            PackageSpec::Targets(targets) => targets.into_iter().collect(),
            PackageSpec::All() => {
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

        let local_cfg_options = match modifiers.as_slice() {
            Some(modifiers) => {
                if !global_cfg_options.cli_modifiers.is_empty() {
                    return Err(ModifiersError::PatternModifiersWithGlobalModifiers.into());
                }

                GlobalCfgOptions {
                    target_platform: global_cfg_options.target_platform.dupe(),
                    cli_modifiers: modifiers.to_vec().into(),
                }
            }
            None => global_cfg_options.dupe(),
        };

        for (target_name, providers) in targets {
            let label = providers.into_providers_label(package.dupe(), target_name.as_ref());
            let providers_label = ctx
                .get_configured_provider_label(&label, &local_cfg_options)
                .await?;
            let install_info = ctx
                .get_providers(&providers_label)
                .await?
                .require_compatible()?
                .value
                .maybe_map(|c| c.as_ref().builtin_provider_value::<FrozenInstallInfo>());
            match install_info {
                Some(install_info) => {
                    let installer_label = install_info.get_installer()?;
                    installer_to_files_map
                        .entry(installer_label)
                        .or_insert_with(Vec::new)
                        .push((providers_label.target().dupe(), install_info));
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

    let mut request_data_vec = Vec::with_capacity(installer_to_files_map.len());
    for (installer_label, install_info_vector) in installer_to_files_map {
        let mut installed_targets = Vec::with_capacity(install_info_vector.len());
        for (installed_target, install_info) in install_info_vector {
            let install_files = install_info.get_files()?;
            installed_targets.push((installed_target, install_files));
        }
        request_data_vec.push(InstallRequestData {
            installer_label,
            installed_targets,
        });
    }
    Ok(request_data_vec)
}

fn get_random_tcp_port() -> buck2_error::Result<u16> {
    let bind_address = std::net::Ipv4Addr::LOCALHOST.into();
    let socket_addr = SocketAddr::new(bind_address, 0);
    let tcp_port = TcpListener::bind(socket_addr)?.local_addr()?.port();
    Ok(tcp_port)
}

fn get_timestamp_as_string() -> buck2_error::Result<String> {
    let dt = DateTime::from_timestamp(Utc::now().timestamp(), 0).unwrap();
    Ok(dt.format("%Y%m%d-%H%M%S").to_string())
}

fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}

struct InstallResult {
    installer_ready: Instant,
    installer_finished: Instant,
    device_metadata: Arc<Mutex<Vec<DeviceMetadata>>>,
    result: buck2_error::Result<()>,
}

struct ConnectedInstaller<'a> {
    client: InstallerClient<Channel>,
    artifact_fs: ArtifactFs,
    install_request_data: &'a InstallRequestData<'a>,
    device_metadata: Arc<Mutex<Vec<DeviceMetadata>>>,
    installer_ready: Instant,
    timeout: Duration,
    send_timeout: Duration,
}

impl<'a> ConnectedInstaller<'a> {
    async fn connect(
        tcp_port: u16,
        artifact_fs: ArtifactFs,
        install_request_data: &'a InstallRequestData<'a>,
    ) -> buck2_error::Result<Self> {
        let initial_delay = Duration::from_millis(100);
        let max_delay = Duration::from_millis(500);
        let timeout =
            Duration::from_secs(buck2_env!("BUCK2_INSTALLER_TIMEOUT_S", type=u64)?.unwrap_or(120));
        let send_timeout = Duration::from_secs(
            buck2_env!("BUCK2_INSTALLER_SEND_TIMEOUT_S", type=u64)?.unwrap_or(300),
        );

        let client: buck2_error::Result<InstallerClient<Channel>> = span_async_simple(
            buck2_data::ConnectToInstallerStart {
                tcp_port: tcp_port.into(),
            },
            async move {
                let channel = retrying(initial_delay, max_delay, timeout, || async {
                    get_channel_tcp(Ipv4Addr::LOCALHOST, tcp_port).await
                })
                .await
                .buck_error_context("Failed to connect to the installer using TCP")?;

                Ok(InstallerClient::new(channel)
                    .max_encoding_message_size(usize::MAX)
                    .max_decoding_message_size(usize::MAX))
            },
            buck2_data::ConnectToInstallerEnd {},
        )
        .await;

        Ok(Self {
            client: client?,
            artifact_fs,
            install_request_data,
            device_metadata: Arc::new(Mutex::new(Vec::new())),
            installer_ready: Instant::now(),
            timeout,
            send_timeout,
        })
    }

    async fn install(mut self, files_rx: mpsc::UnboundedReceiver<FileResult>) -> InstallResult {
        let install_info_result = self.send_install_info().await;
        if install_info_result.is_err() {
            return self.install_result(install_info_result);
        }

        let send_files_result = self.send_files(files_rx).await;

        let shutdown_result = self.send_shutdown_command().await;
        if shutdown_result.is_err() {
            return self.install_result(shutdown_result);
        }

        self.install_result(
            send_files_result.buck_error_context("Failed to send artifacts to installer"),
        )
    }

    fn install_result(self, result: buck2_error::Result<()>) -> InstallResult {
        InstallResult {
            installer_ready: self.installer_ready,
            installer_finished: Instant::now(),
            device_metadata: self.device_metadata,
            result,
        }
    }

    async fn send_install_info(&mut self) -> buck2_error::Result<()> {
        for (installed_target, install_files) in &self.install_request_data.installed_targets {
            let mut files_map = HashMap::new();
            // TODO(T219919866): Support content-based paths by not passing the path to the installer until
            // the artifact is built.
            for (file_name, artifact) in install_files {
                if artifact.path_resolution_requires_artifact_value() {
                    return Err(InstallError::ContentBasedPath {
                        artifact: artifact.clone(),
                    }
                    .into());
                }
                let artifact_path = &self
                    .artifact_fs
                    .fs()
                    .resolve(&artifact.resolve_path(&self.artifact_fs, None)?);
                files_map
                    .entry((*file_name).to_owned())
                    .or_insert_with(|| artifact_path.to_string());
            }

            let install_id = install_id(installed_target);
            let install_info_request = tonic::Request::new(InstallInfoRequest {
                install_id: install_id.to_owned(),
                files: files_map,
            });

            let response_result =
                tokio::time::timeout(self.timeout, self.client.install(install_info_request))
                    .await
                    .map_err(|_| InstallError::RequestTimeout {
                        timeout: self.timeout,
                        action: "send install metadata".to_owned(),
                    })?;

            let install_info_response = match response_result {
                Ok(r) => r.into_inner(),
                Err(e) => {
                    return Err(InstallError::InternalInstallerFailure {
                        install_id: install_id.to_owned(),
                        err: e.message().to_owned(),
                    }
                    .into());
                }
            };

            if install_info_response.install_id != install_id {
                self.send_shutdown_command().await?;
                return Err(buck2_error::buck2_error!(
                    buck2_error::ErrorTag::InstallIdMismatch,
                    "Received install id: {} doesn't match with the sent one: {}",
                    install_info_response.install_id,
                    &install_id
                ));
            }
        }
        Ok(())
    }

    async fn send_shutdown_command(&self) -> buck2_error::Result<()> {
        let response_result = tokio::time::timeout(
            self.timeout,
            self.client
                .clone()
                .shutdown_server(tonic::Request::new(ShutdownRequest {})),
        )
        .await
        .map_err(|_| InstallError::RequestTimeout {
            timeout: self.timeout,
            action: "shutdown".to_owned(),
        })?;

        match response_result {
            Ok(_) => Ok(()),
            Err(status) => Err(InstallError::InstallerCommunicationFailure {
                err: status.message().to_owned(),
            }
            .into()),
        }
    }

    async fn send_files(
        &mut self,
        files_rx: mpsc::UnboundedReceiver<FileResult>,
    ) -> buck2_error::Result<()> {
        UnboundedReceiverStream::new(files_rx)
            .map(buck2_error::Ok)
            .try_for_each_concurrent(None, |file| self.send_file(file))
            .await
    }

    async fn send_file(&self, file: FileResult) -> buck2_error::Result<()> {
        let install_id = file.install_id;
        let name = file.name;
        let artifact = file.artifact;

        enum Data<'a> {
            Digest(&'a FileDigest), // NOTE: A misnommer, this is rather BlobDigest.
            Symlink(String),
        }

        let data = match &file.artifact_value.entry() {
            DirectoryEntry::Dir(dir) => Data::Digest(dir.fingerprint().data()),
            DirectoryEntry::Leaf(ActionDirectoryMember::File(file)) => {
                Data::Digest(file.digest.data())
            }
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
            Data::Symlink(sym) => (format!("re-symlink:{sym}"), 0, "".to_owned()), // Messy :(
        };

        let path = &self.artifact_fs.fs().resolve(&artifact.resolve_path(
            &self.artifact_fs,
            Some(&file.artifact_value.content_based_path_hash()),
        )?);
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
            let mut outcome: buck2_error::Result<()> = Ok(());

            let response_result = match tokio::time::timeout(
                self.send_timeout,
                self.client.clone().file_ready(request),
            )
            .await
            {
                Ok(Ok(r)) => Ok(r.into_inner()),
                Ok(Err(status)) => Err(InstallError::ProcessingFileReadyFailure {
                    install_id: install_id.to_owned(),
                    artifact: name.to_owned(),
                    path: path.to_owned(),
                    err: status.message().to_owned(),
                }),
                Err(_elapsed) => Err(InstallError::RequestTimeout {
                    timeout: self.send_timeout,
                    action: format!("process {name}"),
                }),
            };

            let mut response = match response_result {
                Ok(response) => response,
                Err(e) => return (Err(e.into()), end),
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
                }
                .into());
            }
            self.device_metadata
                .lock()
                .await
                .append(&mut response.device_metadata);

            if let Some(error_detail) = response.error_detail {
                let mut error: buck2_error::Error = InstallError::ProcessingFileReadyFailure {
                    install_id: install_id.to_owned(),
                    artifact: name.to_owned(),
                    path: path.to_owned(),
                    err: error_detail.message,
                }
                .into();
                let category_tag = if let Ok(category) =
                    buck2_install_proto::ErrorCategory::try_from(error_detail.category)
                {
                    match category {
                        buck2_install_proto::ErrorCategory::Unspecified => {
                            ErrorTag::InstallerUnknown
                        }
                        buck2_install_proto::ErrorCategory::Tier0 => ErrorTag::InstallerTier0,
                        buck2_install_proto::ErrorCategory::Input => ErrorTag::InstallerInput,
                        buck2_install_proto::ErrorCategory::Environment => {
                            ErrorTag::InstallerEnvironment
                        }
                    }
                } else {
                    ErrorTag::InstallerUnknown
                };
                error = error.tag([category_tag]);

                for tag in error_detail.tags {
                    error = error.string_tag(&tag);
                }
                outcome = Err(error);
            }
            (outcome, end)
        })
        .await?;
        Ok(())
    }
}

async fn handle_install_request(
    ctx: &mut DiceComputations<'_>,
    install_log_dir: &AbsNormPathBuf,
    install_request_data: &InstallRequestData<'_>,
    initial_installer_run_args: &[String],
    installer_debug: bool,
) -> buck2_error::Result<()> {
    let (files_tx, files_rx) = mpsc::unbounded_channel();

    let log_filename = format!(
        "installer_{}_{}.log",
        get_timestamp_as_string()?,
        calculate_hash(&install_request_data.installer_label.target().name())
    );
    let log_path = install_log_dir.join(FileName::unchecked_new(&log_filename));
    let log_path_string = log_path.to_string();
    let (artifacts_ready, install_result) = ctx
        .try_compute2(
            |ctx| {
                async move {
                    build_files(ctx, &install_request_data.installed_targets, files_tx).await?;
                    buck2_error::Ok(Instant::now())
                }
                .boxed()
            },
            |ctx| {
                async move {
                    // FIXME: The random unused tcp port might be available when get_random_tcp_port() is called,
                    // but when the installer tries to bind on it, someone else might bind on it.
                    // TODO: choose unused tcp port on installer side.
                    // The way communication may happen:
                    // 1. buck2 passes a temp file for a tcp port output.
                    // 2. installer app choose unused tcp port and writes it into the passed file.
                    // 3. buck2 reads tcp port from file and use it to connect to the installer app. (`connect_to_installer` function)
                    let tcp_port = get_random_tcp_port()?;

                    let mut installer_run_args: Vec<String> = initial_installer_run_args.to_vec();

                    installer_run_args.extend(vec![
                        "--tcp-port".to_owned(),
                        tcp_port.to_string(),
                        "--log-path".to_owned(),
                        log_path_string.to_owned(),
                    ]);

                    build_launch_installer(
                        ctx,
                        &install_request_data.installer_label,
                        &installer_run_args,
                        installer_debug,
                    )
                    .await?;
                    let artifact_fs = ctx.get_artifact_fs().await?;

                    let installer =
                        ConnectedInstaller::connect(tcp_port, artifact_fs, install_request_data)
                            .await?;

                    buck2_error::Ok(installer.install(files_rx).await)
                }
                .boxed()
            },
        )
        .await?;

    let InstallResult {
        installer_ready,
        installer_finished,
        device_metadata,
        result,
    } = install_result;

    let device_metadata: Vec<buck2_data::DeviceMetadata> = device_metadata
        .lock()
        .await
        .iter()
        .map(|metadata| buck2_data::DeviceMetadata {
            entry: metadata
                .entry
                .iter()
                .map(|e| buck2_data::device_metadata::Entry {
                    key: e.key.clone(),
                    value: e.value.clone(),
                })
                .collect(),
        })
        .collect();
    let build_finished = std::cmp::max(installer_ready, artifacts_ready);
    let install_duration = installer_finished - build_finished;

    let mut log_url = None;

    let result = match upload_installer_logs(&log_path).await {
        Ok(url) => {
            let result = result.map_err(|err| err.context(format!("See installer logs at: {url}")));
            log_url = Some(url);
            result
        }
        Err(err) => {
            let _unused = soft_error!("installer_log_upload_failed", err.clone());
            result.map_err(|err| err.context(format!("See installer logs at: {log_path}")))
        }
    };

    get_dispatcher().instant_event(buck2_data::InstallFinished {
        duration: install_duration.try_into().ok(),
        device_metadata,
        log_url,
    });
    result
}

async fn upload_installer_logs(log_path: &AbsNormPathBuf) -> buck2_error::Result<String> {
    let manifold = ManifoldClient::new().await?;
    let trace_id: &str = &get_dispatcher().trace_id().to_string();
    let manifold_filename = format!("flat/{trace_id}.log");
    manifold
        .upload_file(
            log_path,
            manifold_filename,
            Bucket::INSTALLER_LOGS,
            Ttl::from_days(14),
        )
        .await
}

async fn build_launch_installer(
    ctx: &mut DiceComputations<'_>,
    providers_label: &ConfiguredProvidersLabel,
    installer_run_args: &[String],
    installer_log_console: bool,
) -> buck2_error::Result<()> {
    let frozen_providers = ctx
        .get_providers(providers_label)
        .await?
        .require_compatible()?;

    if let Some(installer_run_info) = frozen_providers
        .provider_collection()
        .builtin_provider::<FrozenRunInfo>()
    {
        let artifact_fs = ctx.get_artifact_fs().await?;
        let inputs = {
            // Restrict lifetime of mutable artifact_visitor.
            let mut artifact_visitor = SimpleCommandLineArtifactVisitor::new();
            installer_run_info.visit_artifacts(&mut artifact_visitor)?;
            artifact_visitor.inputs
        };
        let ensured_inputs = ctx
            .try_compute_join(inputs, |ctx, input| {
                async move {
                    materialize_and_upload_artifact_group(
                        ctx,
                        &input,
                        MaterializationAndUploadContext::materialize(),
                        &ctx.per_transaction_data()
                            .get_materialization_queue_tracker(),
                    )
                    .await
                    .map(|value| (input, value))
                }
                .boxed()
            })
            .await
            .buck_error_context("Failed to build installer")?;

        // Produce arguments for local platform.
        let path_separator = if cfg!(windows) {
            PathSeparatorKind::Windows
        } else {
            PathSeparatorKind::Unix
        };
        let executor_fs = ExecutorFs::new(&artifact_fs, path_separator);
        let mut run_args = Vec::<String>::new();
        let mut ctx = AbsCommandLineContext::new(&executor_fs);
        installer_run_info.add_to_command_line(
            &mut run_args,
            &mut ctx,
            &ArtifactPathMapperImpl::from(&ensured_inputs),
        )?;

        let build_id: &str = &get_dispatcher().trace_id().to_string();
        background_command(&run_args[0])
            .args(&run_args[1..])
            .args(installer_run_args)
            .env("BUCK2_UUID", build_id)
            .stderr(get_stdio(installer_log_console)?)
            .spawn()
            .buck_error_context("Failed to spawn installer")?;

        Ok(())
    } else {
        Err(InstallError::NoRunInfoProvider(providers_label.target().name().to_owned()).into())
    }
}

fn get_stdio(log_installer_console: bool) -> buck2_error::Result<Stdio> {
    if log_installer_console {
        Ok(Stdio::inherit())
    } else {
        Ok(Stdio::null())
    }
}

#[derive(Debug)]
pub(crate) struct FileResult {
    install_id: String,
    name: String,
    artifact: Artifact,
    artifact_value: ArtifactValue,
}

async fn build_files(
    ctx: &mut DiceComputations<'_>,
    install_files_slice: &[(ConfiguredTargetLabel, SmallMap<&str, Artifact>)],
    tx: mpsc::UnboundedSender<FileResult>,
) -> buck2_error::Result<()> {
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

    ctx.try_compute_join(
        file_outputs,
        |ctx, (installed_target, name, artifact, tx_clone)| {
            async move {
                let (_, artifact_values) = ctx
                    .try_compute2(
                        |ctx| {
                            async move {
                                VALIDATION_IMPL
                                    .get()?
                                    .validate_target_node_transitively(ctx, installed_target.dupe())
                                    .await
                            }
                            .boxed()
                        },
                        |ctx| {
                            async move {
                                materialize_and_upload_artifact_group(
                                    ctx,
                                    &artifact,
                                    MaterializationAndUploadContext::materialize(),
                                    &ctx.per_transaction_data()
                                        .get_materialization_queue_tracker(),
                                )
                                .await
                            }
                            .boxed()
                        },
                    )
                    .await?;
                for (artifact, artifact_value) in artifact_values.iter() {
                    let install_id = install_id(installed_target);
                    let file_result = FileResult {
                        install_id,
                        name: (*name).to_owned(),
                        artifact: artifact.to_owned(),
                        artifact_value: artifact_value.to_owned(),
                    };
                    tx_clone.send(file_result)?;
                }
                buck2_error::Ok(())
            }
            .boxed()
        },
    )
    .await?;
    Ok(())
}
