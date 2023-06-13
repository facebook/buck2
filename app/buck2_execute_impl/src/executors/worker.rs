/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::ffi::OsStr;
use std::ffi::OsString;
use std::fs::File;
use std::sync::Arc;
use std::time::Duration;

use anyhow::Context;
use buck2_common::client_utils::get_channel_uds;
use buck2_common::client_utils::retrying;
use buck2_common::client_utils::RetryError;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::file_name::FileName;
use buck2_events::dispatch::get_dispatcher_opt;
use buck2_execute::execute::kind::CommandExecutionKind;
use buck2_execute::execute::manager::CommandExecutionManagerExt;
use buck2_execute::execute::manager::CommandExecutionManagerWithClaim;
use buck2_execute::execute::output::CommandStdStreams;
use buck2_execute::execute::request::CommandExecutionRequest;
use buck2_execute::execute::request::WorkerId;
use buck2_execute::execute::request::WorkerSpec;
use buck2_execute::execute::result::CommandExecutionMetadata;
use buck2_execute::execute::result::CommandExecutionResult;
use buck2_forkserver::run::maybe_absolutize_exe;
use buck2_forkserver::run::prepare_command;
use buck2_forkserver::run::status_decoder::default_decode_exit_code;
use buck2_forkserver::run::GatherOutputStatus;
use buck2_util::process::background_command;
use buck2_worker_proto::execute_command::EnvironmentEntry;
use buck2_worker_proto::worker_client::WorkerClient;
use buck2_worker_proto::ExecuteCommand;
use buck2_worker_proto::ExecuteResponse;
use futures::future::FutureExt;
use indexmap::IndexMap;
use tokio::process::Child;
use tonic::transport::Channel;

use crate::executors::local::apply_local_execution_environment;

#[derive(thiserror::Error, Debug)]
pub enum WorkerInitError {
    #[error("Worker failed to spawn: {0}")]
    SpawnFailed(anyhow::Error),
    #[error("Worker exited before connecting")]
    EarlyExit {
        exit_code: i32,
        stdout: String,
        stderr: String,
    },
    #[error("Worker failed to connect within `{0:.2}` seconds: {1}")]
    ConnectionTimeout(f64, String),
    /// Any error not related to worker behavior
    #[error("Error initializing worker `{0}`")]
    InternalError(anyhow::Error),
}

impl WorkerInitError {
    pub(crate) fn to_command_execution_result(
        self,
        request: &CommandExecutionRequest,
        manager: CommandExecutionManagerWithClaim,
    ) -> CommandExecutionResult {
        let worker_spec = request.worker().as_ref().unwrap();
        let execution_kind = CommandExecutionKind::LocalWorkerInit {
            command: worker_spec.exe.clone(),
            env: request.env().clone(),
        };

        match self {
            WorkerInitError::EarlyExit {
                exit_code,
                stdout,
                stderr,
            } => {
                let std_streams = CommandStdStreams::Local {
                    stdout: stdout.into(),
                    stderr: stderr.into(),
                };
                // TODO(ctolliday) this should be a new failure type (worker_init_failure), not conflated with a "command failure" which
                // implies that it is the primary command and that exit code != 0
                manager.failure(
                    execution_kind,
                    IndexMap::default(),
                    std_streams,
                    Some(exit_code),
                    CommandExecutionMetadata::default(),
                )
            }
            // TODO(ctolliday) as above, use a new failure type (worker_init_failure) that indicates this is a worker initialization error.
            WorkerInitError::ConnectionTimeout(..) | WorkerInitError::SpawnFailed(..) => manager
                .failure(
                    execution_kind,
                    IndexMap::default(),
                    CommandStdStreams::Local {
                        stdout: Default::default(),
                        stderr: format!("Error initializing worker: {}", self).into_bytes(),
                    },
                    None,
                    CommandExecutionMetadata::default(),
                ),
            WorkerInitError::InternalError(error) => manager.error("get_worker_failed", error),
        }
    }
}

async fn exec_spawn(
    exe: &str,
    args: impl IntoIterator<Item = impl AsRef<OsStr> + Send> + Send,
    env: impl IntoIterator<Item = (impl AsRef<OsStr>, impl AsRef<OsStr>)>,
    root: &AbsNormPathBuf,
    stdout_path: &AbsNormPathBuf,
    stderr_path: &AbsNormPathBuf,
) -> Result<Child, WorkerInitError> {
    // TODO(ctolliday) spawn using forkserver T153604128
    let exe = maybe_absolutize_exe(exe, root).map_err(WorkerInitError::InternalError)?;

    let mut cmd = background_command(exe.as_ref());
    cmd.current_dir(root);
    cmd.args(args);
    apply_local_execution_environment(&mut cmd, root, env, None);

    let mut cmd: tokio::process::Command = prepare_command(cmd);
    let stdout: File = File::create(stdout_path)
        .with_context(|| format!("Failed to create worker log file at {}", stdout_path))
        .map_err(WorkerInitError::InternalError)?;
    let stderr: File = File::create(stderr_path)
        .with_context(|| format!("Failed to create worker log file at {}", stderr_path))
        .map_err(WorkerInitError::InternalError)?;
    cmd.stdout(stdout);
    cmd.stderr(stderr);
    match cmd.spawn() {
        Ok(child) => Ok(child),
        Err(e) => Err(WorkerInitError::SpawnFailed(e.into())),
    }
}

async fn spawn_worker(
    args: &[String],
    env: impl IntoIterator<Item = (OsString, OsString)>,
    worker: &WorkerSpec,
    root: &AbsNormPathBuf,
) -> Result<(WorkerCommandHandle, WorkerCleanupHandle), WorkerInitError> {
    let dispatcher = get_dispatcher_opt().ok_or(WorkerInitError::InternalError(
        anyhow::anyhow!("No dispatcher"),
    ))?;
    // Use fixed length path at /tmp to avoid 108 character limit for unix domain sockets
    let dir_name = format!("{}-{}", dispatcher.trace_id(), worker.id);
    let worker_dir = AbsNormPathBuf::from("/tmp/buck2_worker".to_owned())
        .map_err(WorkerInitError::InternalError)?
        .join(FileName::unchecked_new(&dir_name));
    let socket_path = worker_dir.join(FileName::unchecked_new("socket"));
    if fs_util::try_exists(&worker_dir).map_err(WorkerInitError::InternalError)? {
        return Err(WorkerInitError::InternalError(anyhow::anyhow!(
            "Directory for worker already exists: {:?}",
            worker_dir
        )));
    }
    // TODO(ctolliday) put these in buck-out/<iso>/workers and only use /tmp dir for sockets
    let stdout_path = worker_dir.join(FileName::unchecked_new("stdout"));
    let stderr_path = worker_dir.join(FileName::unchecked_new("stderr"));
    fs_util::create_dir_all(&worker_dir).map_err(WorkerInitError::InternalError)?;

    let args = args.to_vec();
    tracing::info!(
        "Starting worker with logs at {}:\n$ {}\n",
        worker_dir,
        args.join(" ")
    );

    let worker_env = vec![("WORKER_SOCKET", socket_path.as_os_str())]
        .into_iter()
        .map(|(k, v)| (OsString::from(k), OsString::from(v)));
    let env: Vec<(OsString, OsString)> = env.into_iter().chain(worker_env).collect();

    let mut child = exec_spawn(&args[0], &args[1..], env, root, &stdout_path, &stderr_path).await?;
    let initial_delay = Duration::from_millis(50);
    let max_delay = Duration::from_millis(500);
    let timeout = Duration::from_secs(5);
    // TODO(ctolliday) add a span
    let channel = {
        let child = &mut child;
        let stdout_path = &stdout_path;
        let stderr_path = &stderr_path;
        let socket_path = &socket_path;

        let mut check_exit = move || -> Result<(), WorkerInitError> {
            if let Some(exit_status) = child
                .try_wait()
                .map_err(|e| WorkerInitError::InternalError(e.into()))?
            {
                let stdout =
                    fs_util::read_to_string(stdout_path).map_err(WorkerInitError::InternalError)?;
                let stderr =
                    fs_util::read_to_string(stderr_path).map_err(WorkerInitError::InternalError)?;
                return Err(WorkerInitError::EarlyExit {
                    exit_code: default_decode_exit_code(exit_status),
                    stdout,
                    stderr,
                });
            }

            Ok(())
        };

        let check_exit = &mut check_exit;

        match retrying(initial_delay, max_delay, timeout, move || {
            if let Err(e) = check_exit() {
                futures::future::ready(Err(e)).left_future()
            } else {
                // TODO(ctolliday) T153604304
                // add handshake over grpc before returning a handle, to make sure the worker is responding
                async move {
                    get_channel_uds(socket_path, false).await.map_err(|e| {
                        WorkerInitError::ConnectionTimeout(timeout.as_secs_f64(), e.to_string())
                    })
                }
                .right_future()
            }
        })
        .await
        {
            Ok(channel) => Ok(channel),
            Err(e) => {
                if let RetryError::Failed { last_error, .. } = e {
                    return Err(last_error);
                }
                Err(WorkerInitError::ConnectionTimeout(
                    timeout.as_secs_f64(),
                    "Timed out without error.".to_owned(),
                ))
            }
        }?
    };

    tracing::info!("Connected to socket for spawned worker: {}", socket_path);
    let client = WorkerClient::new(channel);
    Ok((
        WorkerCommandHandle {
            client,
            stdout_path,
            stderr_path,
        },
        WorkerCleanupHandle { child, socket_path },
    ))
}

pub struct WorkerPool {
    workers: Arc<tokio::sync::Mutex<HashMap<WorkerId, Arc<WorkerCommandHandle>>>>,
    cleanup: Arc<parking_lot::Mutex<Vec<WorkerCleanupHandle>>>,
}

impl Drop for WorkerPool {
    fn drop(&mut self) {
        tracing::info!("Dropping WorkerPool");
        let cleanup_handles: Vec<WorkerCleanupHandle> = self.cleanup.lock().drain(..).collect();
        tokio::spawn({
            async move {
                for WorkerCleanupHandle {
                    mut child,
                    socket_path,
                } in cleanup_handles
                {
                    tracing::info!("Killing worker {:?} {}", child, socket_path);
                    let _unused = child.kill().await;
                    let _unused = fs_util::remove_file(&socket_path);
                }
            }
        });
    }
}

impl WorkerPool {
    pub fn new() -> WorkerPool {
        tracing::info!("Creating new WorkerPool");
        WorkerPool {
            workers: Arc::new(tokio::sync::Mutex::new(HashMap::default())),
            cleanup: Arc::new(parking_lot::Mutex::new(Vec::new())),
        }
    }

    pub async fn get_or_create_worker(
        &self,
        worker_spec: &WorkerSpec,
        env: impl IntoIterator<Item = (OsString, OsString)>,
        root: &AbsNormPathBuf,
    ) -> Result<Arc<WorkerCommandHandle>, WorkerInitError> {
        let mut workers = self.workers.lock().await;
        if let Some(worker_handle) = workers.get(&worker_spec.id) {
            Ok(worker_handle.clone())
        } else {
            // TODO(ctolliday) keep track of workers that fail to start and don't try to spawn them again
            // TODO(ctolliday) do not lock the entire workers map while spawning/connecting
            let (worker_handle, cleanup_handle) =
                spawn_worker(&worker_spec.exe, env, worker_spec, root).await?;
            let worker_handle = Arc::new(worker_handle);
            workers.insert(worker_spec.id, worker_handle.clone());
            self.cleanup.lock().push(cleanup_handle);
            Ok(worker_handle)
        }
    }
}

pub struct WorkerCommandHandle {
    client: WorkerClient<Channel>,
    stdout_path: AbsNormPathBuf,
    stderr_path: AbsNormPathBuf,
}

pub struct WorkerCleanupHandle {
    child: Child,
    socket_path: AbsNormPathBuf,
}

#[cfg(unix)]
fn env_entries(env: &[(OsString, OsString)]) -> Vec<EnvironmentEntry> {
    use std::os::unix::ffi::OsStrExt;
    env.iter()
        .map(|(k, v)| EnvironmentEntry {
            key: k.as_bytes().into(),
            value: v.as_bytes().into(),
        })
        .collect()
}

#[cfg(not(unix))]
fn env_entries(_env: &[(OsString, OsString)]) -> Vec<EnvironmentEntry> {
    unreachable!("worker should not exist off unix")
}

impl WorkerCommandHandle {
    pub async fn exec_cmd(
        &self,
        args: &[String],
        env: Vec<(OsString, OsString)>,
    ) -> (GatherOutputStatus, Vec<u8>, Vec<u8>) {
        tracing::info!(
            "Sending worker command:\nExecuteCommand {{ argv: {:?}, env: {:?} }}\n",
            args,
            env,
        );
        let argv: Vec<Vec<u8>> = args.iter().map(|s| s.as_str().into()).collect();
        let env: Vec<EnvironmentEntry> = env_entries(&env);

        let request = ExecuteCommand { argv, env };
        let response = self.client.clone().execute(request).await;

        match response {
            Ok(response) => {
                let exec_response: ExecuteResponse = response.into_inner();
                tracing::info!("Worker response:\n{:?}\n", exec_response);
                (
                    GatherOutputStatus::Finished {
                        exit_code: exec_response.exit_code,
                        execution_stats: None,
                    },
                    vec![],
                    exec_response.stderr.into(),
                )
            }
            Err(err) => {
                (
                    GatherOutputStatus::SpawnFailed(format!(
                        "Error sending ExecuteCommand to worker: {:?}, see worker logs:\n{}\n{}",
                        err, self.stdout_path, self.stderr_path,
                    )),
                    // stdout/stderr logs for worker are for multiple commands, probably do not want to dump contents here
                    vec![],
                    vec![],
                )
            }
        }
    }
}
