/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::ffi::OsString;
use std::sync::Arc;
use std::time::Duration;

use buck2_common::client_utils::get_channel_uds;
use buck2_common::client_utils::retrying;
use buck2_common::liveliness_observer::LivelinessGuard;
use buck2_common::liveliness_observer::LivelinessObserver;
use buck2_common::result::SharedError;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::file_name::FileName;
use buck2_events::dispatch::EventDispatcher;
use buck2_execute::execute::kind::CommandExecutionKind;
use buck2_execute::execute::manager::CommandExecutionManagerExt;
use buck2_execute::execute::manager::CommandExecutionManagerWithClaim;
use buck2_execute::execute::output::CommandStdStreams;
use buck2_execute::execute::request::CommandExecutionRequest;
use buck2_execute::execute::request::WorkerId;
use buck2_execute::execute::request::WorkerSpec;
use buck2_execute::execute::result::CommandExecutionMetadata;
use buck2_execute::execute::result::CommandExecutionResult;
use buck2_forkserver::client::ForkserverClient;
use buck2_forkserver::run::GatherOutputStatus;
use buck2_worker_proto::execute_command::EnvironmentEntry;
use buck2_worker_proto::worker_client::WorkerClient;
use buck2_worker_proto::ExecuteCommand;
use buck2_worker_proto::ExecuteResponse;
use futures::future::BoxFuture;
use futures::future::Shared;
use futures::FutureExt;
use host_sharing::HostSharingBroker;
use host_sharing::HostSharingStrategy;
use indexmap::IndexMap;
use tokio::task::JoinHandle;
use tonic::transport::Channel;

#[derive(thiserror::Error, Debug)]
pub enum WorkerInitError {
    #[error("Worker failed to spawn: {0}")]
    SpawnFailed(String),
    #[error("Worker exited before connecting")]
    EarlyExit {
        exit_code: Option<i32>,
        stdout: String,
        stderr: String,
    },
    #[error("Worker failed to connect within `{0:.2}` seconds: {1}")]
    ConnectionTimeout(f64, String),
    /// Any error not related to worker behavior
    #[error("Error initializing worker `{0}`")]
    InternalError(SharedError),
}

impl WorkerInitError {
    pub(crate) fn to_command_execution_result(
        &self,
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
                    stdout: stdout.to_owned().into(),
                    stderr: stderr.to_owned().into(),
                };
                // TODO(ctolliday) this should be a new failure type (worker_init_failure), not conflated with a "command failure" which
                // implies that it is the primary command and that exit code != 0
                manager.failure(
                    execution_kind,
                    IndexMap::default(),
                    std_streams,
                    *exit_code,
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
            WorkerInitError::InternalError(error) => {
                manager.error("get_worker_failed", error.clone())
            }
        }
    }
}

#[cfg(unix)]
fn spawn_via_forkserver(
    forkserver: ForkserverClient,
    exe: OsString,
    args: Vec<OsString>,
    env: Vec<(OsString, OsString)>,
    working_directory: AbsNormPathBuf,
    liveliness_observer: impl LivelinessObserver + 'static,
    stdout_path: &AbsNormPathBuf,
    stderr_path: &AbsNormPathBuf,
    socket_path: &AbsNormPathBuf,
) -> JoinHandle<anyhow::Result<GatherOutputStatus>> {
    use std::os::unix::ffi::OsStrExt;

    use crate::executors::local::apply_local_execution_environment;

    let stdout_path = stdout_path.clone();
    let stderr_path = stderr_path.clone();

    let socket_path = socket_path.clone();
    tokio::spawn(async move {
        let mut req = buck2_forkserver_proto::CommandRequest {
            exe: exe.as_bytes().into(),
            argv: args.into_iter().map(|s| s.as_bytes().into()).collect(),
            cwd: Some(buck2_forkserver_proto::WorkingDirectory {
                path: working_directory.as_path().as_os_str().as_bytes().into(),
            }),
            env: vec![],
            timeout: None,
            enable_miniperf: false,
            std_redirects: Some(buck2_forkserver_proto::command_request::StdRedirectPaths {
                stdout: stdout_path.as_os_str().as_bytes().into(),
                stderr: stderr_path.as_os_str().as_bytes().into(),
            }),
        };
        apply_local_execution_environment(&mut req, &working_directory, env, None);
        let res = forkserver
            .execute(req, async move { liveliness_observer.while_alive().await })
            .await
            .map(|(status, _, _)| status);

        // Socket is created by worker so won't exist if initialization fails.
        if fs_util::try_exists(&socket_path)? {
            // TODO(ctolliday) delete directory (after logs are moved to buck-out)
            fs_util::remove_file(&socket_path)?;
        }
        res
    })
}

#[cfg(not(unix))]
fn spawn_via_forkserver(
    _forkserver: ForkserverClient,
    _exe: OsString,
    _args: Vec<OsString>,
    _env: Vec<(OsString, OsString)>,
    _working_directory: AbsNormPathBuf,
    _liveliness_observer: impl LivelinessObserver + 'static,
    _stdout_path: &AbsNormPathBuf,
    _stderr_path: &AbsNormPathBuf,
    _socket_path: &AbsNormPathBuf,
) -> JoinHandle<anyhow::Result<GatherOutputStatus>> {
    unreachable!("workers should not be initialized off unix")
}

async fn spawn_worker(
    worker_spec: &WorkerSpec,
    env: impl IntoIterator<Item = (OsString, OsString)>,
    root: &AbsNormPathBuf,
    forkserver: ForkserverClient,
    dispatcher: EventDispatcher,
) -> Result<WorkerHandle, WorkerInitError> {
    // Use fixed length path at /tmp to avoid 108 character limit for unix domain sockets
    let dir_name = format!("{}-{}", dispatcher.trace_id(), worker_spec.id);
    let worker_dir = AbsNormPathBuf::from("/tmp/buck2_worker".to_owned())
        .map_err(|e| WorkerInitError::InternalError(e.into()))?
        .join(FileName::unchecked_new(&dir_name));
    let socket_path = worker_dir.join(FileName::unchecked_new("socket"));
    if fs_util::try_exists(&worker_dir).map_err(|e| WorkerInitError::InternalError(e.into()))? {
        return Err(WorkerInitError::InternalError(
            anyhow::anyhow!("Directory for worker already exists: {:?}", worker_dir).into(),
        ));
    }
    // TODO(ctolliday) put these in buck-out/<iso>/workers and only use /tmp dir for sockets
    let stdout_path = worker_dir.join(FileName::unchecked_new("stdout"));
    let stderr_path = worker_dir.join(FileName::unchecked_new("stderr"));
    fs_util::create_dir_all(&worker_dir).map_err(|e| WorkerInitError::InternalError(e.into()))?;

    let args = worker_spec.exe.to_vec();
    tracing::info!(
        "Starting worker with logs at {}:\n$ {}\n",
        worker_dir,
        args.join(" ")
    );

    let worker_env = vec![("WORKER_SOCKET", socket_path.as_os_str())]
        .into_iter()
        .map(|(k, v)| (OsString::from(k), OsString::from(v)));
    let env: Vec<(OsString, OsString)> = env.into_iter().chain(worker_env).collect();

    let (liveliness_observer, liveliness_guard) = LivelinessGuard::create();

    let spawn_fut = spawn_via_forkserver(
        forkserver,
        OsString::from(args[0].clone()),
        args[1..].iter().map(OsString::from).collect(),
        env.clone(),
        root.clone(),
        liveliness_observer,
        &stdout_path,
        &stderr_path,
        &socket_path,
    );

    let initial_delay = Duration::from_millis(50);
    let max_delay = Duration::from_millis(500);
    // Might want to make this configurable, and/or measure impact of worker initialization on critical path
    let timeout = Duration::from_secs(60);
    let channel = {
        let stdout_path = &stdout_path;
        let stderr_path = &stderr_path;
        let socket_path = &socket_path;

        let connect = retrying(initial_delay, max_delay, timeout, move || {
            // TODO(ctolliday) T153604304
            // add handshake over grpc before returning a handle, to make sure the worker is responding
            get_channel_uds(socket_path, false)
        });

        let check_exit = async move {
            spawn_fut
                .await
                .map_err(|e| WorkerInitError::InternalError(e.into()))?
        };
        futures::pin_mut!(connect);
        futures::pin_mut!(check_exit);

        match futures::future::select(connect, check_exit).await {
            futures::future::Either::Left((connection_result, _)) => match connection_result {
                Ok(channel) => Ok(channel),
                Err(e) => Err(WorkerInitError::ConnectionTimeout(
                    timeout.as_secs_f64(),
                    e.to_string(),
                )),
            },
            futures::future::Either::Right((command_result, _)) => Err(match command_result {
                Ok(GatherOutputStatus::SpawnFailed(e)) => WorkerInitError::SpawnFailed(e),
                Ok(GatherOutputStatus::Finished { exit_code, .. }) => {
                    let stdout = fs_util::read_to_string(stdout_path)
                        .map_err(|e| WorkerInitError::InternalError(e.into()))?;
                    let stderr = fs_util::read_to_string(stderr_path)
                        .map_err(|e| WorkerInitError::InternalError(e.into()))?;
                    WorkerInitError::EarlyExit {
                        exit_code: Some(exit_code),
                        stdout,
                        stderr,
                    }
                }
                Ok(GatherOutputStatus::Cancelled | GatherOutputStatus::TimedOut(_)) => {
                    WorkerInitError::InternalError(
                        anyhow::anyhow!("Worker cancelled by buck").into(),
                    )
                }
                Err(e) => WorkerInitError::InternalError(e.into()),
            }),
        }?
    };

    tracing::info!("Connected to socket for spawned worker: {}", socket_path);
    let client = WorkerClient::new(channel);
    Ok(WorkerHandle::new(
        client,
        stdout_path,
        stderr_path,
        liveliness_guard,
    ))
}

type WorkerFuture = Shared<BoxFuture<'static, Result<Arc<WorkerHandle>, Arc<WorkerInitError>>>>;

pub struct WorkerPool {
    workers: Arc<parking_lot::Mutex<HashMap<WorkerId, WorkerFuture>>>,
    brokers: Arc<parking_lot::Mutex<HashMap<WorkerId, Arc<HostSharingBroker>>>>,
}

impl WorkerPool {
    pub fn new() -> WorkerPool {
        tracing::info!("Creating new WorkerPool");
        WorkerPool {
            workers: Arc::new(parking_lot::Mutex::new(HashMap::default())),
            brokers: Arc::new(parking_lot::Mutex::new(HashMap::default())),
        }
    }

    pub fn get_worker_broker(&self, worker_spec: &WorkerSpec) -> Option<Arc<HostSharingBroker>> {
        let mut brokers = self.brokers.lock();
        worker_spec.concurrency.map(|concurrency| {
            brokers
                .entry(worker_spec.id)
                .or_insert_with(|| {
                    Arc::new(HostSharingBroker::new(
                        HostSharingStrategy::Fifo,
                        concurrency,
                    ))
                })
                .clone()
        })
    }

    pub fn get_or_create_worker(
        &self,
        worker_spec: &WorkerSpec,
        env: impl IntoIterator<Item = (OsString, OsString)>,
        root: &AbsNormPathBuf,
        forkserver: ForkserverClient,
        dispatcher: EventDispatcher,
    ) -> (bool, WorkerFuture) {
        let mut workers = self.workers.lock();
        if let Some(worker_fut) = workers.get(&worker_spec.id) {
            (false, worker_fut.clone())
        } else {
            let worker_id = worker_spec.id;
            let worker_spec = worker_spec.clone();
            let root = root.clone();
            let env: Vec<(OsString, OsString)> = env.into_iter().collect();
            let fut = async move {
                match spawn_worker(&worker_spec, env, &root, forkserver, dispatcher).await {
                    Ok(worker) => Ok(Arc::new(worker)),
                    Err(e) => Err(Arc::new(e)),
                }
            }
            .boxed()
            .shared();

            workers.insert(worker_id, fut.clone());
            (true, fut)
        }
    }
}

pub struct WorkerHandle {
    client: WorkerClient<Channel>,
    stdout_path: AbsNormPathBuf,
    stderr_path: AbsNormPathBuf,
    _liveliness_guard: LivelinessGuard,
}

impl WorkerHandle {
    fn new(
        client: WorkerClient<Channel>,
        stdout_path: AbsNormPathBuf,
        stderr_path: AbsNormPathBuf,
        liveliness_guard: LivelinessGuard,
    ) -> Self {
        Self {
            client,
            stdout_path,
            stderr_path,
            _liveliness_guard: liveliness_guard,
        }
    }
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

impl WorkerHandle {
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
