/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ops::ControlFlow;
use std::sync::Arc;
use std::time::SystemTime;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_core::directory::DirectoryEntry;
use buck2_core::env_helper::EnvHelper;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_execute::artifact::fs::ArtifactFs;
use buck2_execute::digest::FileDigestToReExt;
use buck2_execute::directory::directory_to_re_tree;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_execute::execute::action_digest::ActionDigest;
use buck2_execute::execute::blobs::ActionBlobs;
use buck2_execute::execute::kind::CommandExecutionKind;
use buck2_execute::execute::manager::CommandExecutionManager;
use buck2_execute::execute::name::ExecutorName;
use buck2_execute::execute::prepared::ActionPaths;
use buck2_execute::execute::prepared::PreparedCommand;
use buck2_execute::execute::prepared::PreparedCommandExecutor;
use buck2_execute::execute::request::CommandExecutionRequest;
use buck2_execute::execute::result::CommandExecutionResult;
use buck2_execute::execute::result::CommandExecutionStatus;
use buck2_execute::materialize::materializer::Materializer;
use buck2_execute::re::knobs::ReExecutorGlobalKnobs;
use buck2_execute::re::manager::ManagedRemoteExecutionClient;
use buck2_node::execute::config::CacheUploadBehavior;
use buck2_node::execute::config::RemoteExecutorUseCase;
use futures::future;
use futures::future::FutureExt;
use gazebo::prelude::*;
use remote_execution as RE;
use remote_execution::DigestWithStatus;
use remote_execution::NamedDigest;
use remote_execution::TActionResult2;
use remote_execution::TCode;
use remote_execution::TDirectory2;
use remote_execution::TExecutedActionMetadata;
use remote_execution::TFile;
use remote_execution::TStatus;
use remote_execution::TTimestamp;
use tracing::info;

use crate::execute::commands::output::CommandStdStreamsExt;
use crate::execute::commands::re::download::download_action_results;

// Whether to throw errors when cache uploads fail (primarily for tests).
static ERROR_ON_CACHE_UPLOAD: EnvHelper<bool> = EnvHelper::new("BUCK2_TEST_ERROR_ON_CACHE_UPLOAD");

/// A PreparedCommandExecutor that will check the action cache before executing any actions using the underlying executor.
pub struct CachingExecutor {
    pub inner: Arc<dyn PreparedCommandExecutor>,
    pub fs: ArtifactFs,
    pub materializer: Arc<dyn Materializer>,
    pub re_client: ManagedRemoteExecutionClient,
    pub upload_all_actions: bool,
    pub knobs: ReExecutorGlobalKnobs,
    pub cache_upload_behavior: CacheUploadBehavior,
}

impl CachingExecutor {
    pub fn new(
        inner: Arc<dyn PreparedCommandExecutor>,
        fs: ArtifactFs,
        materializer: Arc<dyn Materializer>,
        re_client: ManagedRemoteExecutionClient,
        upload_all_actions: bool,
        knobs: ReExecutorGlobalKnobs,
        cache_upload_behavior: CacheUploadBehavior,
    ) -> Self {
        Self {
            inner,
            fs,
            materializer,
            re_client,
            upload_all_actions,
            knobs,
            cache_upload_behavior,
        }
    }

    async fn try_action_cache_fetch(
        &self,
        mut manager: CommandExecutionManager,
        request: &CommandExecutionRequest,
        action_paths: &ActionPaths,
        action_digest: &ActionDigest,
        action_blobs: &ActionBlobs,
    ) -> ControlFlow<CommandExecutionResult, CommandExecutionManager> {
        let re_client = &self.re_client;
        let action_cache_response = manager
            .stage_async(
                buck2_data::CacheQuery {
                    action_digest: action_digest.to_string(),
                },
                re_client.action_cache(action_digest.dupe(), self.re_use_case()),
            )
            .await;

        if self.upload_all_actions {
            match re_client
                .upload(
                    &self.materializer,
                    action_blobs,
                    ProjectRelativePath::empty(),
                    &action_paths.inputs,
                    self.re_use_case(),
                    &self.knobs,
                )
                .await
            {
                Err(e) => {
                    return ControlFlow::Break(manager.error("upload".into(), e));
                }
                Ok(()) => {}
            };
        }

        let response = match action_cache_response {
            Err(e) => return ControlFlow::Break(manager.error("remote_action_cache".into(), e)),
            Ok(Some(response)) => {
                // we were able to go to the action cache, so can skip uploading and running
                info!(
                    "Action result is cached, skipping execution of:\n```\n$ {}\n```\n for action `{}`",
                    request.args().join(" "),
                    action_digest,
                );
                response
            }
            Ok(None) => return ControlFlow::Continue(manager),
        };

        ControlFlow::Break(
            download_action_results(
                request,
                &*self.materializer,
                &self.re_client,
                self.re_use_case(),
                manager,
                // TODO (torozco): We should deduplicate this and ActionExecutionKind.
                buck2_data::CacheHit {
                    action_digest: action_digest.to_string(),
                }
                .into(),
                action_paths,
                request.outputs(),
                action_digest,
                &response,
            )
            .await,
        )
    }

    /// Upload an action result to the RE action cache, assuming conditions for the upload are met:
    /// the action must have been successful and must have run locally (not much point in caching
    /// something that ran on RE and is already cached), and cache uploads must be enabled, both
    /// for this executor and this particular action.
    async fn perform_cache_upload(
        &self,
        request: &CommandExecutionRequest,
        digest: &ActionDigest,
        result: &CommandExecutionResult,
    ) -> anyhow::Result<()> {
        if !self.cache_upload_behavior.is_enabled() {
            return Ok(());
        }

        if !request.allow_cache_upload() {
            return Ok(());
        }

        match &result.report.status {
            CommandExecutionStatus::Success {
                execution_kind: CommandExecutionKind::Local { .. },
            } => {
                // Fall through to the upload.
            }
            _ => {
                // In all other cases, skip.
                return Ok(());
            }
        }

        tracing::debug!("Uploading action result for `{}`", digest);

        let timing = result.report.timing;

        let mut upload_futs = vec![];
        let mut output_files = vec![];
        let mut output_directories = vec![];

        for (output, value) in result.outputs.iter() {
            let output = output.as_ref().resolve(&self.fs);

            match value.entry().as_ref() {
                DirectoryEntry::Leaf(ActionDirectoryMember::File(f)) => {
                    output_files.push(TFile {
                        digest: DigestWithStatus {
                            digest: f.digest.to_re(),
                            status: TStatus {
                                code: TCode::OK,
                                message: String::new(),
                                ..Default::default()
                            },
                            ..Default::default()
                        },
                        name: output.path().to_string(),
                        executable: f.is_executable,
                        ..Default::default()
                    });

                    let fut = async move {
                        self.re_client
                            .upload_files_and_directories(
                                vec![NamedDigest {
                                    name: output.path().to_string(),
                                    digest: f.digest.to_re(),
                                    ..Default::default()
                                }],
                                vec![],
                                vec![],
                                self.re_use_case(),
                            )
                            .await
                    };
                    upload_futs.push(fut.boxed());
                }
                DirectoryEntry::Dir(d) => {
                    let tree = directory_to_re_tree(d);
                    let mut action_blobs = ActionBlobs::new();
                    let tree_digest = action_blobs.add_protobuf_message(&tree);

                    output_directories.push(TDirectory2 {
                        path: output.path().to_string(),
                        tree_digest: tree_digest.to_re(),
                        root_directory_digest: d.fingerprint().to_re(),
                        ..Default::default()
                    });

                    let fut = async move {
                        self.re_client
                            .upload(
                                &self.materializer,
                                &action_blobs,
                                output.path(),
                                &d.dupe().as_immutable(),
                                self.re_use_case(),
                                &self.knobs,
                            )
                            .await
                    };
                    upload_futs.push(fut.boxed());
                }
                DirectoryEntry::Leaf(..) => {
                    // Bail, there is something that is not a file here and we don't handle this.
                    // This will happen if the value is a symlink. The primary output of a command
                    // being a symlink is probably unlikely. Unfortunately, we can't represent this
                    // in RE's action output, so we either have to lie about the output and pretend
                    // it's a file, or bail.
                    return Ok(());
                }
            }
        }

        let uploads = async {
            future::try_join_all(upload_futs)
                .await
                .context("Error uploading outputs")?;

            Ok(())
        };

        let std_streams = async {
            result
                .report
                .std_streams
                .clone()
                .into_re(&self.re_client, self.re_use_case())
                .await
                .context("Error accessing std_streams")
        };

        let ((), std_streams) = future::try_join(uploads, std_streams).await?;

        let worker;

        #[cfg(fbcode_build)]
        {
            worker = hostname::get_hostname()?;
        }

        #[cfg(not(fbcode_build))]
        {
            worker = "".to_owned();
        }

        let (stdout_raw, stdout_digest) = std_streams.stdout.into_raw_or_digest();
        let (stderr_raw, stderr_digest) = std_streams.stderr.into_raw_or_digest();

        let result = TActionResult2 {
            output_files,
            output_directories,
            exit_code: 0,
            stdout_raw,
            stdout_digest,
            stderr_raw,
            stderr_digest,
            execution_metadata: TExecutedActionMetadata {
                worker,
                execution_dir: "".to_owned(),
                execution_start_timestamp: systemtime_to_ttimestamp(timing.start_time)?,
                execution_completed_timestamp: systemtime_to_ttimestamp(
                    timing.start_time + timing.wall_time,
                )?,
                execution_attempts: 1,
                ..Default::default()
            },
            ..Default::default()
        };

        self.re_client
            .write_action_result(digest.as_digest().to_re(), result, self.re_use_case())
            .await?;

        Ok(())
    }
}

#[async_trait]
impl PreparedCommandExecutor for CachingExecutor {
    async fn exec_cmd(
        &self,
        command: &PreparedCommand<'_, '_>,
        manager: CommandExecutionManager,
    ) -> CommandExecutionResult {
        let error_on_cache_upload = match ERROR_ON_CACHE_UPLOAD.get_copied() {
            Ok(r) => r.unwrap_or_default(),
            Err(e) => return manager.error("cache_upload".to_owned(), e),
        };

        let manager = self
            .try_action_cache_fetch(
                manager,
                command.request,
                &command.action_paths,
                &command.prepared_action.action,
                &command.prepared_action.blobs,
            )
            .await?;

        let mut res = self.inner.exec_cmd(command, manager).await;

        let upload_res = self
            .perform_cache_upload(command.request, &command.prepared_action.action, &res)
            .await;

        if let Err(error) = upload_res {
            if error_on_cache_upload {
                res.report.status = CommandExecutionStatus::Error {
                    stage: "cache_upload".to_owned(),
                    error,
                };
            } else {
                tracing::warn!(
                    "Cache upload for `{}` failed: {:#}",
                    command.prepared_action.action,
                    error
                );
            }
        }

        res
    }

    fn re_platform(&self) -> Option<&RE::Platform> {
        self.inner.re_platform()
    }

    fn re_use_case(&self) -> RemoteExecutorUseCase {
        self.inner.re_use_case()
    }

    fn name(&self) -> ExecutorName {
        self.inner.name()
    }
}

fn systemtime_to_ttimestamp(time: SystemTime) -> anyhow::Result<TTimestamp> {
    let duration = time.duration_since(SystemTime::UNIX_EPOCH)?;
    Ok(TTimestamp {
        seconds: duration.as_secs().try_into().context("Invalid duration")?,
        // Max 1B so it won't wrap around.
        nanos: duration.subsec_nanos() as _,
        ..Default::default()
    })
}
