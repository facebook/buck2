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
use std::os::unix::ffi::OsStrExt;
use std::sync::Arc;
use std::time::Duration;

use anyhow::Context;
use buck2_common::client_utils::get_channel_uds;
use buck2_common::client_utils::retrying;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::file_name::FileName;
use buck2_events::dispatch::get_dispatcher_opt;
use buck2_execute::execute::request::WorkerId;
use buck2_execute::execute::request::WorkerSpec;
use buck2_forkserver::run::prepare_command;
use buck2_forkserver::run::GatherOutputStatus;
use buck2_util::process::background_command;
use buck2_worker_proto::execute_command::EnvironmentEntry;
use buck2_worker_proto::worker_client::WorkerClient;
use buck2_worker_proto::ExecuteCommand;
use buck2_worker_proto::ExecuteResponse;
use futures::future::FutureExt;
use tokio::process::Child;
use tonic::transport::Channel;

use crate::executors::local::apply_local_execution_environment;

async fn exec_spawn(
    exe: &str,
    args: impl IntoIterator<Item = impl AsRef<OsStr> + Send> + Send,
    env: impl IntoIterator<Item = (impl AsRef<OsStr>, impl AsRef<OsStr>)>,
    root: &AbsNormPathBuf,
    stdout_path: &AbsNormPathBuf,
    stderr_path: &AbsNormPathBuf,
) -> anyhow::Result<Child> {
    // TODO(ctolliday) spawn using forkserver T153604128
    let mut cmd = background_command(exe);
    cmd.current_dir(root);
    cmd.args(args);
    apply_local_execution_environment(&mut cmd, root, env, None);

    let mut cmd: tokio::process::Command = prepare_command(cmd);
    cmd.stdout(File::create(stdout_path)?);
    cmd.stderr(File::create(stderr_path)?);
    Ok(cmd.spawn()?)
}

async fn spawn_worker(
    args: &[String],
    env: impl IntoIterator<Item = (OsString, OsString)>,
    worker: &WorkerSpec,
    root: &AbsNormPathBuf,
) -> anyhow::Result<(WorkerCommandHandle, WorkerCleanupHandle)> {
    let dispatcher = get_dispatcher_opt().context("No dispatcher")?;
    // Use fixed length path at /tmp to avoid 108 character limit for unix domain sockets
    let dir_name = format!("{}-{}", dispatcher.trace_id(), worker.id);
    let worker_dir = AbsNormPathBuf::from("/tmp/buck2_worker".to_owned())?
        .join(FileName::unchecked_new(&dir_name));
    let socket_path = worker_dir.join(FileName::unchecked_new("socket"));
    if fs_util::try_exists(&worker_dir)? {
        return Err(anyhow::anyhow!(
            "Directory for worker already exists: {:?}",
            worker_dir
        ));
    }
    // TODO(ctolliday) put these in buck-out/<iso>/workers and only use /tmp dir for sockets
    let stdout_path = worker_dir.join(FileName::unchecked_new("stdout"));
    let stderr_path = worker_dir.join(FileName::unchecked_new("stderr"));
    fs_util::create_dir_all(&worker_dir)?;

    let args = args.to_vec();
    tracing::info!(
        "Starting worker with logs at {:?}:\n$ {}\n",
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

        let mut check_exit = move || {
            if let Some(exit_status) = child.try_wait()? {
                let stdout = fs_util::read_to_string(stdout_path)?;
                let stderr = fs_util::read_to_string(stderr_path)?;
                return Err(anyhow::anyhow!(
                    "Worker exited early with code {}\nstdout: {:?}\nstderr: {:?}",
                    exit_status,
                    stdout,
                    stderr
                ));
            }

            Ok(())
        };

        let check_exit = &mut check_exit;

        retrying(initial_delay, max_delay, timeout, move || {
            if let Err(e) = check_exit() {
                futures::future::ready(Err(e)).left_future()
            } else {
                // TODO(ctolliday) T153604304
                // add handshake over grpc before returning a handle, to make sure the worker is responding
                get_channel_uds(socket_path, false).right_future()
            }
        })
        .await
        .context("Failed to connect to worker socket")?
    };

    tracing::info!("Connected to socket for spawned worker: {:?}", socket_path);
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
                    tracing::info!("Killing worker {:?} {:?}", child, socket_path);
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
    ) -> anyhow::Result<Arc<WorkerCommandHandle>> {
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
        let env: Vec<EnvironmentEntry> = env
            .into_iter()
            .map(|(k, v)| EnvironmentEntry {
                key: k.as_bytes().into(),
                value: v.as_bytes().into(),
            })
            .collect();

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
                        "Error sending ExecuteCommand to worker: {:?}, see worker logs:\n{:?}\n{:?}",
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
