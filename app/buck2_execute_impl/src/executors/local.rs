/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ffi::OsStr;
use std::ffi::OsString;
use std::ops::ControlFlow;
use std::process::Command;
use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;
use std::time::SystemTime;

use async_trait::async_trait;
use buck2_common::file_ops::FileDigestConfig;
use buck2_common::liveliness_observer::LivelinessObserver;
use buck2_common::liveliness_observer::LivelinessObserverExt;
use buck2_common::local_resource_state::LocalResourceHolder;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::abs_path::AbsPath;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::tag_error;
use buck2_core::tag_result;
use buck2_error::buck2_error;
use buck2_error::BuckErrorContext;
use buck2_events::dispatch::get_dispatcher_opt;
use buck2_events::dispatch::EventDispatcher;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::directory::extract_artifact_value;
use buck2_execute::directory::insert_entry;
use buck2_execute::entry::build_entry_from_disk;
use buck2_execute::entry::HashingInfo;
use buck2_execute::execute::action_digest::ActionDigest;
use buck2_execute::execute::blocking::BlockingExecutor;
use buck2_execute::execute::clean_output_paths::CleanOutputPaths;
use buck2_execute::execute::environment_inheritance::EnvironmentInheritance;
use buck2_execute::execute::executor_stage_async;
use buck2_execute::execute::inputs_directory::inputs_directory;
use buck2_execute::execute::kind::CommandExecutionKind;
use buck2_execute::execute::manager::CommandExecutionManager;
use buck2_execute::execute::manager::CommandExecutionManagerExt;
use buck2_execute::execute::manager::CommandExecutionManagerWithClaim;
use buck2_execute::execute::output::CommandStdStreams;
use buck2_execute::execute::prepared::PreparedCommand;
use buck2_execute::execute::prepared::PreparedCommandExecutor;
use buck2_execute::execute::request::CommandExecutionInput;
use buck2_execute::execute::request::CommandExecutionOutput;
use buck2_execute::execute::request::CommandExecutionOutputRef;
use buck2_execute::execute::request::CommandExecutionRequest;
use buck2_execute::execute::request::ExecutorPreference;
use buck2_execute::execute::result::CommandExecutionMetadata;
use buck2_execute::execute::result::CommandExecutionResult;
use buck2_execute::knobs::ExecutorGlobalKnobs;
use buck2_execute::materialize::materializer::MaterializationError;
use buck2_execute::materialize::materializer::Materializer;
use buck2_forkserver::client::ForkserverClient;
use buck2_forkserver::run::gather_output;
use buck2_forkserver::run::maybe_absolutize_exe;
use buck2_forkserver::run::timeout_into_cancellation;
use buck2_forkserver::run::GatherOutputStatus;
use buck2_futures::cancellation::CancellationContext;
use buck2_futures::cancellation::CancellationObserver;
use buck2_util::process::background_command;
use derive_more::From;
use dupe::Dupe;
use futures::future;
use futures::future::select;
use futures::future::FutureExt;
use futures::stream::StreamExt;
use gazebo::prelude::*;
use host_sharing::host_sharing::HostSharingGuard;
use host_sharing::HostSharingBroker;
use host_sharing::HostSharingRequirements;
use indexmap::IndexMap;
use tracing::info;

use crate::executors::worker::WorkerHandle;
use crate::executors::worker::WorkerPool;

#[derive(Debug, buck2_error::Error)]
#[buck2(input)]
enum LocalExecutionError {
    #[error("Args list was empty")]
    NoArgs,

    #[error("Trying to execute a remote-only action on a local executor")]
    RemoteOnlyAction,
}

#[derive(Clone)]
pub struct LocalExecutor {
    artifact_fs: ArtifactFs,
    materializer: Arc<dyn Materializer>,
    blocking_executor: Arc<dyn BlockingExecutor>,
    pub(crate) host_sharing_broker: Arc<HostSharingBroker>,
    root: AbsNormPathBuf,
    #[cfg_attr(not(unix), allow(unused))]
    forkserver: Option<ForkserverClient>,
    #[allow(unused)]
    knobs: ExecutorGlobalKnobs,
    #[allow(unused)]
    worker_pool: Option<Arc<WorkerPool>>,
}

impl LocalExecutor {
    pub fn new(
        artifact_fs: ArtifactFs,
        materializer: Arc<dyn Materializer>,
        blocking_executor: Arc<dyn BlockingExecutor>,
        host_sharing_broker: Arc<HostSharingBroker>,
        root: AbsNormPathBuf,
        forkserver: Option<ForkserverClient>,
        knobs: ExecutorGlobalKnobs,
        worker_pool: Option<Arc<WorkerPool>>,
    ) -> Self {
        Self {
            artifact_fs,
            materializer,
            blocking_executor,
            host_sharing_broker,
            root,
            forkserver,
            knobs,
            worker_pool,
        }
    }

    // Compiler gets confused (on the not(unix) branch only, weirdly) if you use an async fn.
    #[allow(clippy::manual_async_fn)]
    fn exec<'a>(
        &'a self,
        exe: &'a str,
        args: impl IntoIterator<Item = impl AsRef<OsStr> + Send> + Send + 'a,
        env: impl IntoIterator<Item = (impl AsRef<OsStr> + Send, impl AsRef<OsStr> + Send)> + Send + 'a,
        working_directory: &'a ProjectRelativePath,
        timeout: Option<Duration>,
        env_inheritance: Option<&'a EnvironmentInheritance>,
        liveliness_observer: impl LivelinessObserver + 'static,
        disable_miniperf: bool,
        action_digest: &'a str,
    ) -> impl futures::future::Future<
        Output = buck2_error::Result<(GatherOutputStatus, Vec<u8>, Vec<u8>)>,
    > + Send
    + 'a {
        async move {
            let working_directory = self.root.join_cow(working_directory);

            match &self.forkserver {
                Some(forkserver) => {
                    #[cfg(unix)]
                    {
                        unix::exec_via_forkserver(
                            forkserver,
                            exe,
                            args,
                            env,
                            &working_directory,
                            timeout,
                            env_inheritance,
                            liveliness_observer,
                            self.knobs.enable_miniperf && !disable_miniperf,
                            action_digest,
                        )
                        .await
                    }

                    #[cfg(not(unix))]
                    {
                        let _unused = (forkserver, disable_miniperf, action_digest);
                        Err(buck2_error!(
                            buck2_error::ErrorTag::Input,
                            "Forkserver is not supported off-UNIX"
                        ))
                    }
                }

                None => {
                    let exe = maybe_absolutize_exe(exe, &working_directory)?;
                    let mut cmd = background_command(exe.as_ref());
                    cmd.current_dir(working_directory.as_path());
                    cmd.args(args);
                    apply_local_execution_environment(
                        &mut cmd,
                        &working_directory,
                        env,
                        env_inheritance,
                    );
                    let timeout = timeout_into_cancellation(timeout);

                    let alive = liveliness_observer
                        .while_alive()
                        .map(|()| Ok(GatherOutputStatus::Cancelled));

                    let cancellation =
                        select(timeout.boxed(), alive.boxed()).map(|r| r.factor_first().0);

                    gather_output(cmd, cancellation).await
                }
                .with_buck_error_context(|| {
                    format!("Failed to gather output from command: {}", exe)
                }),
            }
        }
    }

    async fn exec_request(
        &self,
        action_digest: &ActionDigest,
        request: &CommandExecutionRequest,
        manager: CommandExecutionManager,
        cancellation: CancellationObserver,
        cancellations: &CancellationContext,
        digest_config: DigestConfig,
        local_resource_holders: &[LocalResourceHolder],
    ) -> CommandExecutionResult {
        let args = &request.all_args_vec();
        if args.is_empty() {
            return manager.error("no_args", LocalExecutionError::NoArgs);
        }

        let executor_stage_result = executor_stage_async(
            buck2_data::LocalStage {
                stage: Some(buck2_data::LocalMaterializeInputs {}.into()),
            },
            async {
                let start = Instant::now();

                let (r1, r2) = future::join(
                    async {
                        materialize_inputs(&self.artifact_fs, self.materializer.as_ref(), request)
                            .await
                    },
                    async {
                        // When user requests to not perform a cleanup for a specific action
                        // output from previous run of that action could actually be used as the
                        // input during current run (e.g. extra output which is an incremental state describing the actual output).
                        if !request.outputs_cleanup {
                            materialize_build_outputs(
                                &self.artifact_fs,
                                self.materializer.as_ref(),
                                request,
                            )
                            .await?;
                            buck2_error::Ok(())
                        } else {
                            Ok(())
                        }
                    },
                )
                .await;

                let scratch_path = r1?.scratch;
                r2?;

                buck2_error::Ok((scratch_path, start.elapsed()))
            },
        )
        .boxed()
        .await;

        let (scratch_path, input_materialization_duration) = match executor_stage_result {
            Ok((scratch_path, input_materialization_duration)) => {
                (scratch_path, input_materialization_duration)
            }
            Err(e) => return manager.error("materialize_inputs_failed", e),
        };

        // TODO: Release here.
        let manager = manager.claim().boxed().await;

        let scratch_path = &scratch_path.0;

        if let Err(e) = executor_stage_async(
            buck2_data::LocalStage {
                stage: Some(buck2_data::LocalPrepareOutputDirs {}.into()),
            },
            async move {
                create_output_dirs(
                    &self.artifact_fs,
                    request,
                    self.materializer.dupe(),
                    self.blocking_executor.dupe(),
                    cancellations,
                )
                .await
                .buck_error_context("Error creating output directories")?;

                buck2_error::Ok(())
            },
        )
        .boxed()
        .await
        {
            return manager.error("prepare_output_dirs_failed", e);
        };

        info!(
            "Local execution command line:\n```\n$ {}\n```",
            args.join(" "),
        );

        let scratch_path_abs;

        let tmpdirs = if let Some(scratch_path) = scratch_path {
            // For the $TMPDIR - important it is absolute
            scratch_path_abs = self.artifact_fs.fs().resolve(scratch_path);

            if cfg!(windows) {
                const MAX_PATH: usize = 260;
                if scratch_path_abs.as_os_str().len() > MAX_PATH {
                    return manager.error(
                        "scratch_dir_too_long",
                        buck2_error!(
                            buck2_error::ErrorTag::Environment,
                            "Scratch directory path is longer than MAX_PATH: {}",
                            scratch_path_abs
                        ),
                    );
                }
                vec![
                    ("TEMP", scratch_path_abs.as_os_str()),
                    ("TMP", scratch_path_abs.as_os_str()),
                ]
            } else {
                vec![("TMPDIR", scratch_path_abs.as_os_str())]
            }
        } else {
            vec![]
        };

        let local_resource_env_vars: Vec<(&str, StrOrOsStr)> = local_resource_holders
            .iter()
            .flat_map(|h| {
                h.as_ref().0.iter().map(|env_var| {
                    (
                        env_var.key.as_str(),
                        StrOrOsStr::from(env_var.value.as_str()),
                    )
                })
            })
            .collect();

        let daemon_uuid: &str = &buck2_events::daemon_id::DAEMON_UUID.to_string();
        let dispatcher = match get_dispatcher_opt() {
            Some(dispatcher) => dispatcher,
            None => {
                return manager.error(
                    "no_dispatcher",
                    buck2_error!(buck2_error::ErrorTag::Tier0, "No dispatcher available"),
                );
            }
        };
        let build_id: &str = &dispatcher.trace_id().to_string();

        let iter_env = || {
            tmpdirs
                .iter()
                .map(|(k, v)| (*k, StrOrOsStr::from(*v)))
                .chain(
                    request
                        .env()
                        .iter()
                        .map(|(k, v)| (k.as_str(), StrOrOsStr::from(v.as_str()))),
                )
                .chain(local_resource_env_vars.iter().copied())
                .chain(std::iter::once((
                    "BUCK2_DAEMON_UUID",
                    StrOrOsStr::from(daemon_uuid),
                )))
                .chain(std::iter::once((
                    "BUCK_BUILD_ID",
                    StrOrOsStr::from(build_id),
                )))
        };
        let liveliness_observer = manager.inner.liveliness_observer.dupe().and(cancellation);

        let (worker, manager) = self
            .initialize_worker(request, manager, dispatcher)
            .boxed()
            .await?;

        let execution_kind = match worker {
            None => CommandExecutionKind::Local {
                digest: action_digest.dupe(),
                command: args.to_vec(),
                env: request.env().clone(),
            },
            Some(_) => CommandExecutionKind::LocalWorker {
                digest: action_digest.dupe(),
                command: request.args().to_vec(),
                env: request.env().clone(),
                fallback_exe: request.exe().to_vec(),
            },
        };

        let (mut timing, res) = executor_stage_async(
            {
                let env = iter_env()
                    .map(|(k, v)| buck2_data::EnvironmentEntry {
                        key: k.to_owned(),
                        value: v.into_string_lossy(),
                    })
                    .collect();
                let stage = match worker {
                    None => buck2_data::LocalExecute {
                        command: Some(buck2_data::LocalCommand {
                            action_digest: action_digest.to_string(),
                            argv: args.to_vec(),
                            env,
                        }),
                    }
                    .into(),
                    Some(_) => buck2_data::WorkerExecute {
                        command: Some(buck2_data::WorkerCommand {
                            action_digest: action_digest.to_string(),
                            argv: request.args().to_vec(),
                            env,
                            fallback_exe: request.exe().to_vec(),
                        }),
                    }
                    .into(),
                };
                buck2_data::LocalStage { stage: Some(stage) }
            },
            async move {
                let execution_start = Instant::now();
                let start_time = SystemTime::now();

                let env = iter_env().map(|(k, v)| (k, v.into_os_str()));
                let r = if let Some(worker) = worker {
                    let env: Vec<(OsString, OsString)> = env
                        .into_iter()
                        .map(|(k, v)| (OsString::from(k), v.to_owned()))
                        .collect();
                    Ok(worker
                        .exec_cmd(request.args(), env, request.timeout())
                        .await)
                } else {
                    self.exec(
                        &args[0],
                        &args[1..],
                        env,
                        request.working_directory(),
                        request.timeout(),
                        request.local_environment_inheritance(),
                        liveliness_observer,
                        request.disable_miniperf(),
                        &action_digest.to_string(),
                    )
                    .await
                };

                let execution_time = execution_start.elapsed();

                let timing = Box::new(CommandExecutionMetadata {
                    wall_time: execution_time,
                    execution_time,
                    start_time,
                    execution_stats: None, // We fill this in later if available.
                    input_materialization_duration,
                    hashing_duration: Duration::ZERO, // We fill hashing info in later if available.
                    hashed_artifacts_count: 0,
                    queue_duration: None,
                });

                (timing, r)
            },
        )
        .boxed()
        .await;

        let (status, stdout, stderr) = match res {
            Ok(res) => res,
            Err(e) => {
                return manager.error("exec_failed", e);
            }
        };

        let std_streams = CommandStdStreams::Local { stdout, stderr };

        match status {
            GatherOutputStatus::Finished {
                exit_code,
                execution_stats,
            } => {
                let (outputs, hashing_time) = match self
                    .calculate_and_declare_output_values(request, digest_config)
                    .boxed()
                    .await
                {
                    Ok((output_values, hashing_time)) => (output_values, hashing_time),
                    Err(e) => {
                        return manager.error("calculate_output_values_failed", e);
                    }
                };

                timing.execution_stats = execution_stats;
                timing.hashing_duration = hashing_time.hashing_duration;
                timing.hashed_artifacts_count = hashing_time.hashed_artifacts_count;

                if exit_code == 0 {
                    manager.success(execution_kind, outputs, std_streams, *timing)
                } else {
                    let manager = check_inputs(
                        manager,
                        &self.artifact_fs,
                        self.blocking_executor.as_ref(),
                        request,
                    )
                    .boxed()
                    .await?;

                    manager.failure(
                        execution_kind,
                        outputs,
                        std_streams,
                        Some(exit_code),
                        *timing,
                        None,
                    )
                }
            }
            GatherOutputStatus::SpawnFailed(reason) => {
                let manager = check_inputs(
                    manager,
                    &self.artifact_fs,
                    self.blocking_executor.as_ref(),
                    request,
                )
                .boxed()
                .await?;

                // We are lying about the std streams here because we don't have a good mechanism
                // to report that the command does not exist, and because that's exactly what RE
                // also does when this happens.
                if matches!(execution_kind, CommandExecutionKind::Local { .. }) {
                    manager.failure(
                        execution_kind,
                        Default::default(),
                        CommandStdStreams::Local {
                            stdout: Default::default(),
                            stderr: format!("Spawning executable `{}` failed: {}", args[0], reason)
                                .into_bytes(),
                        },
                        None,
                        *timing,
                        None,
                    )
                } else {
                    // Workers executing tests often employ a health check to avoid producing
                    // invalid test results. Differentiating a worker spawn failure from a normal
                    // spawn or execution failure allows the test runner to handle this case
                    // accordingly.
                    manager.worker_failure(
                        execution_kind,
                        // Could probably use a better error message.
                        format!("Spawning executable `{}` failed: {}", args[0], reason),
                        *timing,
                    )
                }
            }
            GatherOutputStatus::TimedOut(duration) => {
                manager.timeout(execution_kind, duration, std_streams, *timing, None)
            }
            GatherOutputStatus::Cancelled => manager.cancel_claim(),
        }
    }

    async fn calculate_and_declare_output_values(
        &self,
        request: &CommandExecutionRequest,
        digest_config: DigestConfig,
    ) -> buck2_error::Result<(IndexMap<CommandExecutionOutput, ArtifactValue>, HashingInfo)> {
        let mut builder = inputs_directory(request.inputs(), &self.artifact_fs)?;

        // Read outputs from disk and add them to the builder
        let mut entries = Vec::new();
        let mut total_hashing_time = Duration::ZERO;
        let mut total_hashed_outputs = 0;
        for output in request.outputs() {
            let path = output.resolve(&self.artifact_fs).into_path();
            let abspath = self.root.join(&path);
            let (entry, hashing_info) = build_entry_from_disk(
                abspath,
                FileDigestConfig::build(digest_config.cas_digest_config()),
                self.blocking_executor.as_ref(),
                self.artifact_fs.fs().root(),
            )
            .await
            .with_buck_error_context(|| format!("collecting output {:?}", path))?;
            total_hashing_time += hashing_info.hashing_duration;
            total_hashed_outputs += hashing_info.hashed_artifacts_count;
            if let Some(entry) = entry {
                insert_entry(&mut builder, &path, entry)?;
                entries.push((output.cloned(), path));
            }
        }

        let mut to_declare = vec![];
        let mut mapped_outputs = IndexMap::with_capacity(entries.len());

        for (output, path) in entries {
            let value = extract_artifact_value(&builder, &path, digest_config)?;
            if let Some(value) = value {
                match output {
                    CommandExecutionOutput::BuildArtifact { .. } => {
                        to_declare.push((path, value.dupe()));
                    }
                    CommandExecutionOutput::TestPath { .. } => {
                        // Don't declare those as we don't currently have any form of GC so this
                        // would take up space for nothing, and most importantly, we will never
                        // need them to be in materializer state for e.g. matching as nothing
                        // should depend on them.
                    }
                }

                mapped_outputs.insert(output, value);
            }
        }

        self.materializer.declare_existing(to_declare).await?;

        Ok((
            mapped_outputs,
            HashingInfo {
                hashing_duration: total_hashing_time,
                hashed_artifacts_count: total_hashed_outputs,
            },
        ))
    }

    async fn acquire_worker_permit(
        &self,
        request: &CommandExecutionRequest,
    ) -> Option<HostSharingGuard> {
        if let (Some(worker_spec), Some(worker_pool)) = (request.worker(), self.worker_pool.dupe())
        {
            if let Some(broker) = &worker_pool.get_worker_broker(worker_spec) {
                Some(
                    executor_stage_async(
                        buck2_data::LocalStage {
                            stage: Some(buck2_data::WorkerQueued {}.into()),
                        },
                        broker.acquire(&HostSharingRequirements::default()),
                    )
                    .await,
                )
            } else {
                None
            }
        } else {
            None
        }
    }

    async fn initialize_worker(
        &self,
        request: &CommandExecutionRequest,
        manager: CommandExecutionManagerWithClaim,
        dispatcher: EventDispatcher,
    ) -> ControlFlow<
        CommandExecutionResult,
        (Option<Arc<WorkerHandle>>, CommandExecutionManagerWithClaim),
    > {
        if let (Some(worker_spec), Some(worker_pool), Some(forkserver), true) = (
            request.worker(),
            self.worker_pool.dupe(),
            self.forkserver.dupe(),
            cfg!(unix),
        ) {
            // TODO(ctolliday - T155351378) set worker specific env via WorkerInfo, not from the action
            let env = request
                .env()
                .iter()
                .map(|(k, v)| (OsString::from(k), OsString::from(v)));
            let (new_worker, worker_fut) = worker_pool.get_or_create_worker(
                worker_spec,
                env,
                &self.root,
                forkserver.clone(),
                dispatcher,
            );

            if let Some(Ok(worker)) = worker_fut.peek() {
                return ControlFlow::Continue((Some(worker.clone()), manager));
            }

            // Might make more sense for the stage to always be `WorkerWait` and for `WorkerInit` to be a separate, top level event
            let stage = if new_worker {
                buck2_data::LocalStage {
                    stage: Some(
                        buck2_data::WorkerInit {
                            command: Some(buck2_data::WorkerInitCommand {
                                argv: worker_spec.exe.clone(),
                                env: request
                                    .env()
                                    .iter()
                                    .map(|(k, v)| buck2_data::EnvironmentEntry {
                                        key: k.to_owned(),
                                        value: v.to_owned(),
                                    })
                                    .collect(),
                            }),
                        }
                        .into(),
                    ),
                }
            } else {
                buck2_data::LocalStage {
                    stage: Some(buck2_data::WorkerWait {}.into()),
                }
            };

            match executor_stage_async(stage, worker_fut).await {
                Ok(worker) => ControlFlow::Continue((Some(worker), manager)),
                Err(e) => {
                    let res = {
                        let manager = check_inputs(
                            manager,
                            &self.artifact_fs,
                            self.blocking_executor.as_ref(),
                            request,
                        )
                        .await?;

                        e.to_command_execution_result(request, manager)
                    };
                    ControlFlow::Break(res)
                }
            }
        } else {
            ControlFlow::Continue((None, manager))
        }
    }
}

#[async_trait]
impl PreparedCommandExecutor for LocalExecutor {
    async fn exec_cmd(
        &self,
        command: &PreparedCommand<'_, '_>,
        manager: CommandExecutionManager,
        cancellations: &CancellationContext,
    ) -> CommandExecutionResult {
        let manager = manager.with_execution_kind(CommandExecutionKind::Local {
            digest: command.prepared_action.digest(),
            command: command.request.all_args_vec(),
            env: command.request.env().clone(),
        });
        if command.request.executor_preference().requires_remote() {
            return manager.error("local_prepare", LocalExecutionError::RemoteOnlyAction);
        }

        let PreparedCommand {
            request,
            target: _,
            prepared_action,
            digest_config,
        } = command;

        let local_resource_holders = executor_stage_async(
            buck2_data::LocalStage {
                stage: Some(buck2_data::AcquireLocalResource {}.into()),
            },
            async move {
                let mut holders = vec![];
                // Acquire resources in a sorted way to avoid deadlock.
                // It might happen if 2 tests both requiring resources A and B are run simultaneously and there is only 1 instance of resource per type.
                // If tests are not acquiring them in a sorted way the following situation might happen:
                // Test 1 acquires resource B and test 2 acquires resource A.
                // Now test 1 is waiting on resource B and test 2 is waiting on resource A.
                for r in request.required_local_resources() {
                    holders.push(r.acquire_resource().await);
                }
                holders
            },
        )
        .await;

        let _worker_permit = self.acquire_worker_permit(request).await;

        let _permit = executor_stage_async(
            buck2_data::LocalStage {
                stage: Some(buck2_data::LocalQueued {}.into()),
            },
            self.host_sharing_broker
                .acquire(request.host_sharing_requirements()),
        )
        .await;

        // If we start running something, we don't want this task to get dropped, because if we do
        // we might interfere with e.g. clean up.
        cancellations
            .with_structured_cancellation(|cancellation| {
                Self::exec_request(
                    self,
                    &prepared_action.action_and_blobs.action,
                    request,
                    manager,
                    cancellation,
                    cancellations,
                    *digest_config,
                    &local_resource_holders,
                )
            })
            .await
    }

    fn is_local_execution_possible(&self, _executor_preference: ExecutorPreference) -> bool {
        true
    }
}

/// Either a str or a OsStr, so that we can turn it back into a String without having to check for
/// valid utf-8, while using the same struct.
#[derive(Copy, Clone, Dupe, From)]
enum StrOrOsStr<'a> {
    Str(&'a str),
    OsStr(&'a OsStr),
}

impl<'a> StrOrOsStr<'a> {
    fn into_string_lossy(self) -> String {
        match self {
            Self::Str(s) => s.to_owned(),
            Self::OsStr(s) => s.to_string_lossy().into_owned(),
        }
    }

    fn into_os_str(self) -> &'a OsStr {
        match self {
            Self::Str(s) => OsStr::new(s),
            Self::OsStr(s) => s,
        }
    }
}

pub struct MaterializedInputPaths {
    pub scratch: ScratchPath,
    pub paths: Vec<ProjectRelativePathBuf>,
}

/// Materialize all inputs artifact for CommandExecutionRequest so the command can be executed locally.
///
/// This also discovers the scratch directory if any was passed (if multiple are passed, one of
/// them is returned).
pub async fn materialize_inputs(
    artifact_fs: &ArtifactFs,
    materializer: &dyn Materializer,
    request: &CommandExecutionRequest,
) -> buck2_error::Result<MaterializedInputPaths> {
    let mut paths = vec![];
    let mut scratch = ScratchPath(None);

    for input in request.inputs() {
        match input {
            CommandExecutionInput::Artifact(group) => {
                for (artifact, _) in group.iter() {
                    if artifact.requires_materialization(artifact_fs) {
                        paths.push(artifact.resolve_path(artifact_fs)?);
                    }
                }
            }
            CommandExecutionInput::ActionMetadata(metadata) => {
                let path = artifact_fs
                    .buck_out_path_resolver()
                    .resolve_gen(&metadata.path);
                CleanOutputPaths::clean(std::iter::once(path.as_ref()), artifact_fs.fs())?;
                artifact_fs
                    .fs()
                    .write_file(&path, &metadata.data.0.0, false)?;
            }
            CommandExecutionInput::ScratchPath(path) => {
                let path = artifact_fs.buck_out_path_resolver().resolve_scratch(path);

                // Clean and produce it.
                CleanOutputPaths::clean(std::iter::once(path.as_ref()), artifact_fs.fs())?;
                fs_util::create_dir_all(artifact_fs.fs().resolve(&path))?;

                scratch.0 = Some(path);
            }
        }
    }

    let mut stream = materializer.materialize_many(paths.clone()).await?;
    while let Some(res) = stream.next().await {
        match res {
            Ok(()) => {}
            Err(MaterializationError::NotFound { source }) => {
                let corrupted = source.info.origin.guaranteed_by_action_cache();

                return Err(tag_error!(
                    "cas_missing_fatal",
                    MaterializationError::NotFound { source }.into(),
                    quiet: true,
                    task: false,
                    daemon_in_memory_state_is_corrupted: true,
                    action_cache_is_corrupted: corrupted
                )
                .into());
            }
            Err(e) => {
                return Err(e.into());
            }
        }
    }

    Ok(MaterializedInputPaths { scratch, paths })
}

/// A scratch path discovered during `materialize_inputs`.
pub struct ScratchPath(Option<ProjectRelativePathBuf>);

async fn check_inputs(
    manager: CommandExecutionManagerWithClaim,
    artifact_fs: &ArtifactFs,
    blocking_executor: &dyn BlockingExecutor,
    request: &CommandExecutionRequest,
) -> ControlFlow<CommandExecutionResult, CommandExecutionManagerWithClaim> {
    let res = blocking_executor
        .execute_io_inline(|| {
            for input in request.inputs() {
                match input {
                    CommandExecutionInput::Artifact(group) => {
                        for (artifact, _) in group.iter() {
                            if artifact.requires_materialization(artifact_fs) {
                                let path = artifact.resolve_path(artifact_fs)?;
                                let abs_path = artifact_fs.fs().resolve(&path);

                                // We ignore the result here because while we want to tag it, we'd
                                // prefer to just show the normal error to the user, so we don't
                                // want to propagate it.
                                let _ignored = tag_result!(
                                    "missing_local_inputs",
                                    fs_util::symlink_metadata(&abs_path).buck_error_context("Missing input").map_err(|e| e.into()),
                                    quiet: true,
                                    task: false,
                                    daemon_materializer_state_is_corrupted: true
                                );
                            }
                        }
                    }
                    CommandExecutionInput::ActionMetadata(..) => {
                        // Ignore those here.
                    }
                    CommandExecutionInput::ScratchPath(..) => {
                        // Nothing to look at
                    }
                }
            }

            Ok(())
        })
        .await;

    match res {
        Ok(()) => ControlFlow::Continue(manager),
        Err(err) => ControlFlow::Break(manager.error("local_check_inputs", err)),
    }
}

/// Materialize all output artifact for CommandExecutionRequest.
///
/// Note that the outputs could be from the previous run of the same command if cleanup on the action was not performed.
/// The above is useful when executing incremental actions first remotely and then locally.
/// In that case output from remote execution which is incremental state should be materialized prior local execution.
/// Such incremental state in fact serves as the input while being output as well.
pub(crate) async fn materialize_build_outputs(
    artifact_fs: &ArtifactFs,
    materializer: &dyn Materializer,
    request: &CommandExecutionRequest,
) -> buck2_error::Result<Vec<ProjectRelativePathBuf>> {
    let mut paths = vec![];

    for output in request.outputs() {
        match output {
            CommandExecutionOutputRef::BuildArtifact {
                path,
                output_type: _,
            } => paths.push(artifact_fs.resolve_build(path)),
            CommandExecutionOutputRef::TestPath { path: _, create: _ } => {}
        }
    }

    materializer.ensure_materialized(paths.clone()).await?;

    Ok(paths)
}

/// Create any output dirs requested by the command. Note that this makes no effort to delete
/// the output paths first. Eventually it should, but right now this happens earlier. This
/// would be a separate refactor.
pub async fn create_output_dirs(
    artifact_fs: &ArtifactFs,
    request: &CommandExecutionRequest,
    materializer: Arc<dyn Materializer>,
    blocking_executor: Arc<dyn BlockingExecutor>,
    cancellations: &CancellationContext,
) -> buck2_error::Result<()> {
    let outputs: Vec<_> = request
        .outputs()
        .map(|output| output.resolve(artifact_fs))
        .collect();

    // Invalidate all the output paths this action might provide. Note that this is a bit
    // approximative: we might have previous instances of this action that declared
    // different outputs with a different materialization method that will become invalid
    // now. However, nothing should reference those stale outputs, so while this does not
    // do a good job of cleaning up garbage, it prevents using invalid artifacts.
    let output_paths = outputs.map(|output| output.path.to_owned());
    materializer.invalidate_many(output_paths.clone()).await?;

    if request.outputs_cleanup {
        // TODO(scottcao): Move this deletion logic into materializer itself.
        blocking_executor
            .execute_io(
                Box::new(CleanOutputPaths {
                    paths: output_paths,
                }),
                cancellations,
            )
            .await
            .buck_error_context("Failed to cleanup output directory")?;
    }

    let project_fs = artifact_fs.fs();
    for output in outputs {
        if let Some(path) = output.path_to_create() {
            fs_util::create_dir_all(project_fs.resolve(path))?;
        }
    }

    Ok(())
}

pub fn apply_local_execution_environment(
    builder: &mut impl EnvironmentBuilder,
    working_directory: &AbsPath,
    env: impl IntoIterator<Item = (impl AsRef<OsStr>, impl AsRef<OsStr>)>,
    env_inheritance: Option<&EnvironmentInheritance>,
) {
    if let Some(env_inheritance) = env_inheritance {
        if env_inheritance.clear() {
            builder.clear();
        }

        for key in env_inheritance.exclusions() {
            builder.remove(key);
        }

        for (key, val) in env_inheritance.values() {
            builder.set(key, val);
        }
    }
    for (key, val) in env {
        builder.set(key, val);
    }
    builder.set("PWD", working_directory.as_path());
}

pub trait EnvironmentBuilder {
    fn clear(&mut self);

    fn set<K, V>(&mut self, key: K, val: V)
    where
        K: AsRef<OsStr>,
        V: AsRef<OsStr>;

    fn remove<K>(&mut self, key: K)
    where
        K: AsRef<OsStr>;
}

impl EnvironmentBuilder for Command {
    fn clear(&mut self) {
        Command::env_clear(self);
    }

    fn set<K, V>(&mut self, key: K, val: V)
    where
        K: AsRef<OsStr>,
        V: AsRef<OsStr>,
    {
        Command::env(self, key, val);
    }

    fn remove<K>(&mut self, key: K)
    where
        K: AsRef<OsStr>,
    {
        Command::env_remove(self, key);
    }
}

#[cfg(unix)]
mod unix {
    use std::os::unix::ffi::OsStrExt;

    use super::*;

    pub async fn exec_via_forkserver(
        forkserver: &ForkserverClient,
        exe: impl AsRef<OsStr>,
        args: impl IntoIterator<Item = impl AsRef<OsStr>>,
        env: impl IntoIterator<Item = (impl AsRef<OsStr>, impl AsRef<OsStr>)>,
        working_directory: &AbsPath,
        command_timeout: Option<Duration>,
        env_inheritance: Option<&EnvironmentInheritance>,
        liveliness_observer: impl LivelinessObserver + 'static,
        enable_miniperf: bool,
        action_digest: &str,
    ) -> buck2_error::Result<(GatherOutputStatus, Vec<u8>, Vec<u8>)> {
        let exe = exe.as_ref();

        let mut req = buck2_forkserver_proto::CommandRequest {
            exe: exe.as_bytes().to_vec(),
            argv: args
                .into_iter()
                .map(|s| s.as_ref().as_bytes().to_vec())
                .collect(),
            cwd: Some(buck2_forkserver_proto::WorkingDirectory {
                path: working_directory.as_path().as_os_str().as_bytes().to_vec(),
            }),
            env: vec![],
            timeout: command_timeout.try_map(|d| d.try_into())?,
            enable_miniperf,
            std_redirects: None,
            graceful_shutdown_timeout_s: None,
            action_digest: Some(action_digest.to_owned()),
        };
        apply_local_execution_environment(&mut req, working_directory, env, env_inheritance);
        forkserver
            .execute(req, async move { liveliness_observer.while_alive().await })
            .await
    }

    trait CommandRequestExt {
        fn push_env_directive<D>(&mut self, directive: D)
        where
            D: Into<buck2_forkserver_proto::env_directive::Data>;
    }

    impl CommandRequestExt for buck2_forkserver_proto::CommandRequest {
        fn push_env_directive<D>(&mut self, directive: D)
        where
            D: Into<buck2_forkserver_proto::env_directive::Data>,
        {
            self.env.push(buck2_forkserver_proto::EnvDirective {
                data: Some(directive.into()),
            });
        }
    }

    impl EnvironmentBuilder for buck2_forkserver_proto::CommandRequest {
        fn clear(&mut self) {
            self.push_env_directive(buck2_forkserver_proto::EnvClear {});
        }

        fn set<K, V>(&mut self, key: K, val: V)
        where
            K: AsRef<OsStr>,
            V: AsRef<OsStr>,
        {
            self.push_env_directive(buck2_forkserver_proto::EnvSet {
                key: key.as_ref().as_bytes().to_vec(),
                value: val.as_ref().as_bytes().to_vec(),
            })
        }

        fn remove<K>(&mut self, key: K)
        where
            K: AsRef<OsStr>,
        {
            self.push_env_directive(buck2_forkserver_proto::EnvRemove {
                key: key.as_ref().as_bytes().to_vec(),
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::str;

    use buck2_common::liveliness_observer::NoopLivelinessObserver;
    use buck2_core::cells::cell_root_path::CellRootPathBuf;
    use buck2_core::cells::name::CellName;
    use buck2_core::cells::CellResolver;
    use buck2_core::fs::buck_out_path::BuckOutPathResolver;
    use buck2_core::fs::project::ProjectRoot;
    use buck2_core::fs::project::ProjectRootTemp;
    use buck2_execute::execute::blocking::testing::DummyBlockingExecutor;
    use buck2_execute::materialize::nodisk::NoDiskMaterializer;
    use host_sharing::HostSharingStrategy;

    use super::*;

    fn artifact_fs(project_fs: ProjectRoot) -> ArtifactFs {
        ArtifactFs::new(
            CellResolver::testing_with_name_and_path(
                CellName::testing_new("cell"),
                CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("cell_path".into())),
            ),
            BuckOutPathResolver::new(ProjectRelativePathBuf::unchecked_new("buck_out/v2".into())),
            project_fs,
        )
    }

    fn test_executor() -> buck2_error::Result<(LocalExecutor, AbsNormPathBuf, ProjectRootTemp)> {
        let temp = ProjectRootTemp::new().unwrap();
        let project_fs = temp.path();
        let artifact_fs = artifact_fs(project_fs.dupe());

        let executor = LocalExecutor::new(
            artifact_fs,
            Arc::new(NoDiskMaterializer),
            Arc::new(DummyBlockingExecutor {
                fs: project_fs.dupe(),
            }),
            Arc::new(HostSharingBroker::new(
                HostSharingStrategy::SmallerTasksFirst,
                1,
            )),
            temp.path().root().to_buf(),
            None,
            ExecutorGlobalKnobs::default(),
            None,
        );

        Ok((executor, temp.path().root().to_buf(), temp))
    }

    #[tokio::test]
    async fn test_exec_cmd_environment() -> buck2_error::Result<()> {
        let (executor, root, _tmpdir) = test_executor()?;

        let interpreter = if cfg!(windows) { "powershell" } else { "sh" };
        let (status, stdout, _) = executor
            .exec(
                interpreter,
                ["-c", "echo $PWD; pwd"],
                &HashMap::<String, String>::default(),
                ProjectRelativePath::empty(),
                None,
                None,
                NoopLivelinessObserver::create(),
                false,
                "",
            )
            .await?;
        assert!(matches!(status, GatherOutputStatus::Finished { exit_code, .. } if exit_code == 0));

        let stdout = std::str::from_utf8(&stdout).buck_error_context("Invalid stdout")?;

        if cfg!(windows) {
            let lines: Vec<&str> = stdout.split("\r\n").collect();
            let expected_path = format!("{}", root);

            assert_eq!(lines[3], expected_path);
            assert_eq!(lines[4], expected_path);
        } else {
            assert_eq!(stdout, format!("{}\n{}\n", root, root));
        }

        Ok(())
    }

    #[cfg(unix)] // TODO: something similar on Windows: T123279320
    #[tokio::test]
    async fn test_exec_cmd_environment_filtering() -> buck2_error::Result<()> {
        use buck2_execute::execute::environment_inheritance::EnvironmentInheritance;

        let (executor, _root, _tmpdir) = test_executor()?;

        let (status, stdout, _) = executor
            .exec(
                "sh",
                ["-c", "echo $USER"],
                &HashMap::<String, String>::default(),
                ProjectRelativePath::empty(),
                None,
                Some(&EnvironmentInheritance::empty()),
                NoopLivelinessObserver::create(),
                false,
                "",
            )
            .await?;
        assert!(matches!(status, GatherOutputStatus::Finished { exit_code, .. } if exit_code == 0));
        assert_eq!(stdout, b"\n");

        Ok(())
    }
}
