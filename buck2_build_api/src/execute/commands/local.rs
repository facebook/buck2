/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::ffi::OsStr;
use std::fs;
use std::io;
use std::path::Path;
use std::path::PathBuf;
use std::pin::Pin;
use std::process::ExitStatus;
use std::process::Stdio;
use std::sync::Arc;
use std::task::Context;
use std::task::Poll;
use std::time::Duration;
use std::time::Instant;
use std::time::SystemTime;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_common::file_ops::FileDigest;
use buck2_common::file_ops::FileMetadata;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_core::directory::DirectoryEntry;
use buck2_core::fs::paths::AbsPathBuf;
use buck2_core::fs::paths::FileNameBuf;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_core::process::async_background_command;
use derive_more::From;
use faccess::PathExt;
use futures::channel::oneshot;
use futures::future;
use futures::future::try_join3;
use futures::future::FusedFuture;
use futures::future::Future;
use futures::future::FutureExt;
use gazebo::prelude::*;
use host_sharing::HostSharingBroker;
use indexmap::IndexMap;
use more_futures::spawn::dropcancel_critical_section;
use pin_project::pin_project;
use remote_execution as RE;
use thiserror::Error;
use tokio::io::AsyncRead;
use tokio::io::AsyncReadExt;
use tokio::io::ReadBuf;
use tokio::process::Child;
use tokio::process::Command;
use tracing::info;

use crate::actions::artifact::ArtifactFs;
use crate::actions::artifact::ArtifactValue;
use crate::actions::directory::extract_artifact_value;
use crate::actions::directory::insert_entry;
use crate::actions::directory::new_symlink;
use crate::actions::directory::ActionDirectoryBuilder;
use crate::actions::directory::ActionDirectoryEntry;
use crate::actions::directory::ActionDirectoryMember;
use crate::execute::blocking::BlockingExecutor;
use crate::execute::commands::inputs_directory;
use crate::execute::commands::output::CommandStdStreams;
use crate::execute::commands::CommandExecutionInput;
use crate::execute::commands::CommandExecutionKind;
use crate::execute::commands::CommandExecutionManager;
use crate::execute::commands::CommandExecutionOutput;
use crate::execute::commands::CommandExecutionOutputRef;
use crate::execute::commands::CommandExecutionRequest;
use crate::execute::commands::CommandExecutionResult;
use crate::execute::commands::CommandExecutionTarget;
use crate::execute::commands::CommandExecutionTimingData;
use crate::execute::commands::EnvironmentInheritance;
use crate::execute::commands::ExecutorName;
use crate::execute::commands::PreparedCommand;
use crate::execute::commands::PreparedCommandExecutor;
use crate::execute::materializer::Materializer;
use crate::execute::CleanOutputPaths;

#[derive(Debug, Error)]
enum LocalExecutionError {
    #[error("Args list was empty")]
    NoArgs,
}

#[derive(Clone)]
pub struct LocalExecutor {
    artifact_fs: ArtifactFs,
    materializer: Arc<dyn Materializer>,
    blocking_executor: Arc<dyn BlockingExecutor>,
    host_sharing_broker: Arc<HostSharingBroker>,
    root: AbsPathBuf,
}

impl LocalExecutor {
    pub fn new(
        artifact_fs: ArtifactFs,
        materializer: Arc<dyn Materializer>,
        blocking_executor: Arc<dyn BlockingExecutor>,
        host_sharing_broker: Arc<HostSharingBroker>,
        root: AbsPathBuf,
    ) -> Self {
        Self {
            artifact_fs,
            materializer,
            blocking_executor,
            host_sharing_broker,
            root,
        }
    }

    async fn exec(
        &self,
        exe: &str,
        args: impl IntoIterator<Item = impl AsRef<OsStr>>,
        env: impl IntoIterator<Item = (impl AsRef<OsStr>, impl AsRef<OsStr>)>,
        working_directory: Option<&ProjectRelativePath>,
        timeout: Option<Duration>,
        env_inheritance: Option<&EnvironmentInheritance>,
    ) -> anyhow::Result<(GatherOutputStatus, Vec<u8>, Vec<u8>)> {
        let root = match working_directory {
            Some(d) => Cow::Owned(self.root.join_unnormalized(d)),
            None => Cow::Borrowed(&self.root),
        };

        let root: &Path = root.as_ref();

        let mut cmd = async_background_command(exe);
        cmd.current_dir(root);
        cmd.args(args);
        apply_local_execution_environment(&mut cmd, root, env, env_inheritance);

        gather_output(cmd, timeout)
            .await
            .with_context(|| format!("Failed to gather output from command: {}", exe))
    }

    async fn exec_request(
        &self,
        action: CommandExecutionTarget<'_>,
        request: &CommandExecutionRequest,
        mut manager: CommandExecutionManager,
    ) -> CommandExecutionResult {
        let claim = match manager.try_claim() {
            None => return manager.claim_rejected(),
            Some(v) => v,
        };

        let args = request.args();
        if args.is_empty() {
            return manager.error("no_args".into(), LocalExecutionError::NoArgs.into());
        }

        match manager
            .stage_async(
                buck2_data::LocalStage {
                    stage: Some(buck2_data::LocalMaterializeInputs {}.into()),
                },
                async {
                    let (r1, r2) = future::join(
                        materialize_inputs(&self.artifact_fs, &self.materializer, request),
                        async {
                            // When user requests to not perform a cleanup for a specific action
                            // output from previous run of that action could actually be used as the
                            // input during current run (e.g. extra output which is an incremental state describing the actual output).
                            if !request.outputs_cleanup {
                                materialize_build_outputs_from_previous_run(
                                    &self.artifact_fs,
                                    &self.materializer,
                                    request,
                                )
                                .await
                            } else {
                                Ok(())
                            }
                        },
                    )
                    .await;
                    r1.and(r2)
                },
            )
            .await
        {
            Ok(_) => {}
            Err(e) => return manager.error("materialize_inputs_failed".into(), e),
        };

        let scratch_dir = self
            .artifact_fs
            .buck_out_path_resolver()
            .resolve_scratch(&action.scratch_dir());
        // For the $TMPDIR - important it is absolute
        let scratch_dir_abs = self.artifact_fs.fs().resolve(&scratch_dir);

        if let Err(e) = manager
            .stage_async(
                buck2_data::LocalStage {
                    stage: Some(buck2_data::LocalPrepareOutputDirs {}.into()),
                },
                async move {
                    // TODO(cjhopman): This should be getting the action exec context so it get use io_blocking_section
                    if request.custom_tmpdir {
                        let project_fs = self.artifact_fs.fs();
                        project_fs.remove_path_recursive(&scratch_dir)?;
                        project_fs.create_dir(&scratch_dir)?;
                    }

                    create_output_dirs(
                        &self.artifact_fs,
                        request,
                        self.materializer.dupe(),
                        self.blocking_executor.dupe(),
                    )
                    .await
                    .context("Error creating output directories")?;

                    Ok(())
                },
            )
            .await
        {
            return manager.error("prepare_output_dirs_failed".into(), e);
        };

        info!(
            "Local execution command line:\n```\n$ {}\n```",
            args.join(" "),
        );

        let tmpdir = if request.custom_tmpdir {
            Some(("TMPDIR", scratch_dir_abs.as_os_str()))
        } else {
            None
        };

        let iter_env = || {
            tmpdir
                .into_iter()
                .map(|(k, v)| (k, StrOrOsStr::from(v)))
                .chain(
                    request
                        .env()
                        .iter()
                        .map(|(k, v)| (k.as_str(), StrOrOsStr::from(v.as_str()))),
                )
        };

        let (timing, res) = manager
            .stage_async(
                buck2_data::LocalStage {
                    stage: Some(
                        buck2_data::LocalExecute {
                            command: Some(buck2_data::LocalCommand {
                                argv: args.to_vec(),
                                env: iter_env()
                                    .map(|(k, v)| buck2_data::local_command::EnvironmentEntry {
                                        key: k.to_owned(),
                                        value: v.into_string_lossy(),
                                    })
                                    .collect(),
                            }),
                        }
                        .into(),
                    ),
                },
                async move {
                    let execution_start = Instant::now();
                    let start_time = SystemTime::now();

                    let env = iter_env().map(|(k, v)| (k, v.into_os_str()));
                    let r = self
                        .exec(
                            &args[0],
                            &args[1..],
                            env,
                            request.working_directory(),
                            request.timeout(),
                            request.local_environment_inheritance(),
                        )
                        .await;

                    let execution_time = execution_start.elapsed();

                    let timing = CommandExecutionTimingData {
                        wall_time: execution_time,
                        execution_time,
                        start_time,
                    };

                    (timing, r)
                },
            )
            .await;

        let execution_kind = CommandExecutionKind::Local {
            command: args.to_vec(),
            env: request.env().clone(),
        };

        let (status, stdout, stderr) = match res {
            Ok(res) => res,
            Err(e) => return manager.error("exec_failed".into(), e), // TODO (torozco): Can this take CommandExecutionKind? Should this be a failure?
        };

        let std_streams = CommandStdStreams::Local { stdout, stderr };

        match self.calculate_output_values(request) {
            Ok(outputs) => match status {
                GatherOutputStatus::Finished(status) => match status.code() {
                    Some(0) => manager.success(claim, execution_kind, outputs, std_streams, timing),

                    v => manager.failure(execution_kind, outputs, std_streams, v),
                },
                GatherOutputStatus::TimedOut(duration) => {
                    manager.timeout(execution_kind, duration, std_streams, timing)
                }
            },
            Err(e) => manager.error("calculate_output_values_failed".into(), e),
        }
    }

    fn calculate_output_values(
        &self,
        request: &CommandExecutionRequest,
    ) -> anyhow::Result<IndexMap<CommandExecutionOutput, ArtifactValue>> {
        let mut builder = inputs_directory(request.inputs(), &self.artifact_fs)?;

        // Read outputs from disk and add them to the builder
        let mut entries = Vec::new();
        for output in request.outputs() {
            let path = output.resolve(&self.artifact_fs).into_path();
            let abspath = self.root.join_unnormalized(&path);
            let entry = self
                .build_entry_from_disk(abspath.to_path_buf())
                .with_context(|| format!("collecting output {:?}", path))?;
            if let Some(entry) = entry {
                insert_entry(&mut builder, path.as_ref(), entry)?;
                entries.push((output.cloned(), path));
            }
        }

        let mut mapped_outputs = IndexMap::with_capacity(entries.len());

        for (output, path) in entries {
            let value = extract_artifact_value(&builder, path.as_ref())?;
            if let Some(value) = value {
                mapped_outputs.insert(output, value);
            }
        }

        Ok(mapped_outputs)
    }

    fn build_entry_from_disk(
        &self,
        mut path: PathBuf,
    ) -> anyhow::Result<Option<ActionDirectoryEntry<ActionDirectoryBuilder>>> {
        fn build_dir_from_disk(disk_path: &mut PathBuf) -> anyhow::Result<ActionDirectoryBuilder> {
            let mut builder = ActionDirectoryBuilder::empty();

            for file in fs::read_dir(&disk_path)? {
                let file = file?;
                let filetype = file.file_type()?;
                let filename = file.file_name();
                disk_path.push(filename.as_os_str());

                let filename = filename
                    .to_str()
                    .context("Filename is not UTF-8")
                    .and_then(|f| FileNameBuf::try_from(f.to_owned()))
                    .with_context(|| format!("Invalid filename: {}", disk_path.display()))?;

                if filetype.is_dir() {
                    let dir = build_dir_from_disk(disk_path)?;
                    builder.insert(filename, DirectoryEntry::Dir(dir))?;
                } else if filetype.is_symlink() {
                    builder.insert(
                        filename,
                        DirectoryEntry::Leaf(new_symlink(fs::read_link(&disk_path)?)),
                    )?;
                } else if filetype.is_file() {
                    let metadata = FileMetadata {
                        digest: TrackedFileDigest::new(FileDigest::from_file(&disk_path)?),
                        is_executable: file.path().executable(),
                    };
                    builder.insert(
                        filename,
                        DirectoryEntry::Leaf(ActionDirectoryMember::File(metadata)),
                    )?;
                }
                disk_path.pop();
            }

            Ok(builder)
        }

        // Get file metadata. If the file is missing, ignore it.
        let m = match fs::symlink_metadata(&path) {
            Ok(m) => m,
            Err(ref err) if err.kind() == std::io::ErrorKind::NotFound => return Ok(None),
            Err(err) => return Err(err.into()),
        };

        let value = if m.file_type().is_symlink() {
            DirectoryEntry::Leaf(new_symlink(fs::read_link(&path)?))
        } else if m.is_file() {
            DirectoryEntry::Leaf(ActionDirectoryMember::File(FileMetadata {
                digest: TrackedFileDigest::new(FileDigest::from_file(&path)?),
                is_executable: path.executable(),
            }))
        } else if m.is_dir() {
            DirectoryEntry::Dir(build_dir_from_disk(&mut path)?)
        } else {
            unimplemented!("Path {:?} is of an unknown file type.", path)
        };
        Ok(Some(value))
    }
}

#[async_trait]
impl PreparedCommandExecutor for LocalExecutor {
    async fn exec_cmd(
        &self,
        command: &PreparedCommand<'_, '_>,
        mut manager: CommandExecutionManager,
    ) -> CommandExecutionResult {
        let PreparedCommand {
            request,
            target,
            action_paths: _action_paths,
            prepared_action: _prepared_action,
        } = command;
        let _permit = manager
            .stage_async(
                buck2_data::LocalStage {
                    stage: Some(buck2_data::LocalQueued {}.into()),
                },
                self.host_sharing_broker
                    .acquire(request.host_sharing_requirements()),
            )
            .await;

        // If we start running something, we don't want this task to get dropped, because if we do
        // we might interfere with e.g. clean up.
        dropcancel_critical_section(Self::exec_request(self, *target, request, manager)).await
    }

    fn re_platform(&self) -> Option<&RE::Platform> {
        None
    }

    fn name(&self) -> ExecutorName {
        ExecutorName("local")
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

enum GatherOutputStatus {
    Finished(ExitStatus),
    TimedOut(Duration),
}

async fn gather_output(
    mut cmd: Command,
    timeout: Option<Duration>,
) -> anyhow::Result<(GatherOutputStatus, Vec<u8>, Vec<u8>)> {
    cmd.stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    #[cfg(windows)]
    cmd.creation_flags(winapi::um::winbase::CREATE_NO_WINDOW);

    let mut child = spawn_retry_txt_busy(cmd, || tokio::time::sleep(Duration::from_millis(50)))
        .await
        .context("Failed to start command")?;

    let (stop_reads, reads_stoped) = oneshot::channel::<()>();
    let reads_stoped = reads_stoped.map(|_| ()).boxed().shared();

    let stdout = child.stdout.take().expect("piped() above");
    let stderr = child.stderr.take().expect("piped() above");

    let stdout = InterruptibleAsyncRead {
        read: stdout,
        interrupt: reads_stoped.clone(),
    };

    let stderr = InterruptibleAsyncRead {
        read: stderr,
        interrupt: reads_stoped.clone(),
    };

    let status = async move {
        let exit_status_result = match timeout {
            Some(t) => match tokio::time::timeout(t, child.wait()).await {
                Ok(r) => r,
                Err(..) => {
                    kill_process(&child).context("Failed to terminate child after timeout")?;
                    return Ok(GatherOutputStatus::TimedOut(t));
                }
            },
            None => child.wait().await,
        };

        exit_status_result
            .map(GatherOutputStatus::Finished)
            .map_err(anyhow::Error::from)
    };

    // Stop reads once the child is done.
    let status = async move {
        let ret = status.await;
        let _ = stop_reads.send(());
        ret
    };

    let (status, stdout, stderr) = try_join3(status, stdout.drain(), stderr.drain())
        .await
        .context("Failed to wait for command to exit")?;

    Ok((status, stdout, stderr))
}

pub fn kill_process(child: &Child) -> anyhow::Result<()> {
    let pid = match child.id() {
        Some(pid) => pid,
        None => {
            // Child just exited, so in this case we don't want to kill anything.
            return Ok(());
        }
    };
    tracing::warn!("Killing process {}", pid);
    kill_process_impl(pid)
}

#[cfg(unix)]
fn kill_process_impl(pid: u32) -> anyhow::Result<()> {
    use nix::sys::signal;
    use nix::sys::signal::Signal;
    use nix::unistd::Pid;

    let pid: i32 = pid.try_into().context("PID does not fit a i32")?;
    signal::kill(Pid::from_raw(pid), Signal::SIGKILL)
        .with_context(|| format!("Failed to kill process {}", pid))
}

#[cfg(windows)]
fn kill_process_impl(pid: u32) -> anyhow::Result<()> {
    use winapi::um::handleapi::CloseHandle;
    use winapi::um::processthreadsapi::OpenProcess;
    use winapi::um::processthreadsapi::TerminateProcess;
    use winapi::um::winnt::PROCESS_TERMINATE;

    let proc_handle = unsafe { OpenProcess(PROCESS_TERMINATE, 0, pid) };
    // If proc_handle is null, proccess died already.
    if proc_handle.is_null() {
        return Ok(());
    }
    let terminate_res = unsafe { TerminateProcess(proc_handle, 1) };
    unsafe { CloseHandle(proc_handle) };
    match terminate_res {
        0 => Err(anyhow::anyhow!("Failed to kill process {}", pid)),
        _ => Ok(()),
    }
}

/// fork-exec is a bit tricky in a busy process. We often have files open to writing just prior to
/// executing them (as we download from RE), and many processes being spawned concurrently. We do
/// close the fds properly before the exec, but what can happn is:
///
/// - Some thread forks
/// - We close the file. At this time we don't have it open, but the forked process does.
/// - We try to exec the file. This fails because the file is open for writing (by the forked
/// process).
/// - The forked process execs. At this point the file is closed (because everything is CLOEXEC).
///
/// The window during which the forked process holds the fd is small, so retrying a couple times
/// here should let us make this work.
///
/// The more correct solution for this here would be to start a fork server in a separate process
/// when we start.  However, until we get there, this should do the trick.
async fn spawn_retry_txt_busy<F, D>(mut cmd: Command, mut delay: F) -> io::Result<Child>
where
    F: FnMut() -> D,
    D: Future<Output = ()>,
{
    let mut attempts = 10;

    loop {
        let res = cmd.spawn();

        let res_errno = res.as_ref().map_err(|e| e.raw_os_error());
        let is_txt_busy = matches!(res_errno, Err(Some(libc::ETXTBSY)));

        if attempts == 0 || !is_txt_busy {
            return res;
        }

        delay().await;

        attempts -= 1;
    }
}

/// An [AsyncRead] that can be interrupted. The underlyng [AsyncRead] will be polled while the the
/// [Future] provided in interrupt hasn't completed. When said future completes, if it returns
/// true, we stop reading and return EOF. If it return false, we keep reading until EOF.
#[pin_project]
struct InterruptibleAsyncRead<I, R> {
    #[pin]
    interrupt: I,
    #[pin]
    read: R,
}

impl<I, R> InterruptibleAsyncRead<I, R>
where
    I: Future<Output = ()> + FusedFuture,
    R: AsyncRead + AsyncReadReady,
{
    async fn drain(mut self) -> anyhow::Result<Vec<u8>>
    where
        R: Unpin,
        I: Unpin,
    {
        // First, read until we are told to stop (or hit EOF).
        let mut ret = Vec::new();
        self.read_to_end(&mut ret).await?;

        // Then, read whatever is left on the pipe.
        self.read
            .read_ready(&mut ret)
            .await
            .context("Error draining pipe")?;

        Ok(ret)
    }
}

impl<I, R> AsyncRead for InterruptibleAsyncRead<I, R>
where
    I: Future<Output = ()> + FusedFuture,
    R: AsyncRead,
{
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> Poll<io::Result<()>> {
        let this = self.project();

        if !this.interrupt.is_terminated() {
            match this.interrupt.poll(cx) {
                Poll::Ready(()) => {
                    // The future resolved and we were told to stop reading. Report EOF.
                    return Poll::Ready(Ok(()));
                }
                Poll::Pending => {
                    // We haven't been interrupted yet.
                }
            }
        }

        this.read.poll_read(cx, buf)
    }
}

/// A trait that allows reading what is left on a given readable.
#[async_trait]
trait AsyncReadReady {
    /// Read what is left on self.
    async fn read_ready(&mut self, buff: &mut Vec<u8>) -> anyhow::Result<()>;
}

#[cfg(unix)]
mod unix_async_read_ready {
    use std::io::Read;
    use std::marker::PhantomData;
    use std::os::unix::io::AsRawFd;
    use std::os::unix::prelude::RawFd;

    use nix::unistd;

    use super::*;

    /// Borrow something to perform sync reads on it. We use this to bypass Tokio when draining an
    /// InterruptibleAsyncRead after it's been interrupted. This lets us ensure that even if Tokio
    /// hasn't completed `select()` on the pipe we want to drain, we'll stil get to execute
    /// `read()` (and potentially get WouldBlock if there is nothing to read and the pipe isn't
    /// ready).
    pub trait AsSyncReader {
        type SyncReader<'a>: Read + 'a
        where
            Self: 'a;

        fn as_sync_reader(&mut self) -> Self::SyncReader<'_>;
    }

    pub struct RawFdReader<'a> {
        fd: RawFd,
        _phantom: PhantomData<&'a ()>,
    }

    impl<'a> Read for RawFdReader<'a> {
        fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
            unistd::read(self.fd, buf).map_err(|e| e.into())
        }
    }

    impl<T> AsSyncReader for T
    where
        T: AsRawFd + Send + 'static,
    {
        type SyncReader<'a> = RawFdReader<'a>;

        fn as_sync_reader(&mut self) -> Self::SyncReader<'_> {
            RawFdReader {
                fd: self.as_raw_fd(),
                _phantom: PhantomData,
            }
        }
    }

    #[async_trait]
    impl<T> AsyncReadReady for T
    where
        T: AsSyncReader + Send,
    {
        async fn read_ready(&mut self, buff: &mut Vec<u8>) -> anyhow::Result<()> {
            match self.as_sync_reader().read_to_end(buff) {
                Err(e) if e.kind() != io::ErrorKind::WouldBlock => Err(e.into()),
                Err(..) => {
                    tracing::debug!("pipe was not closed");
                    Ok(())
                }
                Ok(..) => Ok(()),
            }
        }
    }
}

#[cfg(not(unix))]
mod windows_async_read_ready {
    use super::*;

    #[async_trait]
    impl<T> AsyncReadReady for T
    where
        T: AsyncRead + Send + Unpin,
    {
        /// On Windows, for the time being we just give ourselves a timeout to finish reading.
        /// Ideally this would perform a non-blocking read on self instead like we do on Unix.
        async fn read_ready(&mut self, buff: &mut Vec<u8>) -> anyhow::Result<()> {
            let read = self.read_to_end(buff);
            futures::pin_mut!(read);
            match tokio::time::timeout(Duration::from_millis(1), read).await {
                Ok(Err(e)) => Err(e.into()),
                Err(..) => {
                    tracing::debug!("pipe was not closed");
                    Ok(())
                }
                Ok(Ok(..)) => Ok(()),
            }
        }
    }
}

/// Materialize all inputs artifact for CommandExecutionRequest so the command can be executed locally.
pub async fn materialize_inputs(
    artifact_fs: &ArtifactFs,
    materializer: &Arc<dyn Materializer>,
    request: &CommandExecutionRequest,
) -> anyhow::Result<()> {
    let mut paths = vec![];

    for input in request.inputs() {
        match input {
            CommandExecutionInput::Artifact(group) => {
                for (artifact, _) in group.iter() {
                    if !artifact.is_source() {
                        paths.push(artifact_fs.resolve(artifact)?);
                    }
                }
            }
            CommandExecutionInput::ActionMetadata(metadata) => {
                let path = artifact_fs
                    .buck_out_path_resolver()
                    .resolve_gen(&metadata.path);
                CleanOutputPaths::clean(std::iter::once(path.as_ref()), artifact_fs.fs())?;
                artifact_fs.fs().write_file(&path, &metadata.data, false)?;
            }
        }
    }

    materializer.ensure_materialized(paths).await
}

/// Materialize build outputs from the previous run of the same command.
/// Useful when executing incremental actions first remotely and then locally.
/// In that case output from remote execution which is incremental state should be materialized prior local execution.
/// Such incremental state in fact serves as the input while being output as well.
pub async fn materialize_build_outputs_from_previous_run(
    artifact_fs: &ArtifactFs,
    materializer: &Arc<dyn Materializer>,
    request: &CommandExecutionRequest,
) -> anyhow::Result<()> {
    let mut paths = vec![];

    for output in request.outputs() {
        match output {
            CommandExecutionOutputRef::BuildArtifact(artifact) => {
                paths.push(artifact_fs.resolve_build(artifact));
            }
            CommandExecutionOutputRef::TestPath { path: _, create: _ } => {}
        }
    }

    materializer.ensure_materialized(paths).await
}

/// Create any output dirs requested by the command. Note that this makes no effort to delete
/// the output paths first. Eventually it should, but right now this happens earlier. This
/// would be a separate refactor.
pub async fn create_output_dirs(
    artifact_fs: &ArtifactFs,
    request: &CommandExecutionRequest,
    materializer: Arc<dyn Materializer>,
    blocking_executor: Arc<dyn BlockingExecutor>,
) -> anyhow::Result<()> {
    let outputs: Vec<_> = request
        .outputs()
        .map(|output| output.resolve(artifact_fs))
        .collect();

    if request.outputs_cleanup {
        // Invalidate all the output paths this action might provide. Note that this is a bit
        // approximative: we might have previous instances of this action that declared
        // different outputs with a different materialization method that will become invalid
        // now. However, nothing should reference those stale outputs, so while this does not
        // do a good job of cleaning up garbage, it prevents using invalid artifacts.
        let outputs = outputs.map(|output| output.path.to_owned());
        materializer.invalidate_many(outputs.clone()).await?;

        // TODO(scottcao): Move this deletion logic into materializer itself.
        // Use Eden's clean up API if possible, it is significantly faster on Eden compared with
        // the native method as the API does not load and materialize files or folders
        if let Some(eden_buck_out) = materializer.eden_buck_out() {
            eden_buck_out
                .remove_paths_recursive(artifact_fs.fs(), outputs)
                .await?;
        } else {
            blocking_executor
                .execute_io(box CleanOutputPaths { paths: outputs })
                .await
                .context("Failed to cleanup output directory")?;
        }
    }

    let project_fs = artifact_fs.fs();
    for output in outputs {
        if let Some(path) = output.path_to_create() {
            project_fs.create_dir(path)?;
        }
    }

    Ok(())
}

pub fn apply_local_execution_environment(
    builder: &mut impl EnvironmentBuilder,
    root: &Path,
    env: impl IntoIterator<Item = (impl AsRef<OsStr>, impl AsRef<OsStr>)>,
    env_inheritance: Option<&EnvironmentInheritance>,
) {
    if let Some(env_inheritance) = env_inheritance {
        builder.clear();
        for (key, val) in env_inheritance.iter() {
            builder.set(key, val);
        }
    }
    for (key, val) in env {
        builder.set(key, val);
    }
    builder.set("PWD", root);
}

pub trait EnvironmentBuilder {
    fn clear(&mut self);

    fn set<K, V>(&mut self, key: K, val: V)
    where
        K: AsRef<OsStr>,
        V: AsRef<OsStr>;
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
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::str;
    use std::sync::Arc;
    use std::time::Instant;

    use buck2_core::cells::cell_root_path::CellRootPathBuf;
    use buck2_core::cells::testing::CellResolverExt;
    use buck2_core::cells::CellName;
    use buck2_core::cells::CellResolver;
    use buck2_core::fs::project::ProjectFilesystem;
    use buck2_core::fs::project::ProjectRelativePathBuf;
    use host_sharing::HostSharingStrategy;

    use super::*;
    use crate::execute::blocking::testing::DummyBlockingExecutor;
    use crate::execute::materializer::nodisk::NoDiskMaterializer;
    use crate::path::BuckOutPathResolver;
    use crate::path::BuckPathResolver;

    #[tokio::test]
    async fn test_gather_output() -> anyhow::Result<()> {
        let mut cmd = if cfg!(windows) {
            async_background_command("powershell")
        } else {
            async_background_command("sh")
        };
        cmd.args(&["-c", "echo hello"]);

        let (status, stdout, stderr) = gather_output(cmd, None).await?;
        assert!(matches!(status, GatherOutputStatus::Finished(s) if s.code() == Some(0)));
        assert_eq!(str::from_utf8(&stdout)?.trim(), "hello");
        assert_eq!(stderr, b"");

        Ok(())
    }

    #[tokio::test]
    async fn test_gather_does_not_wait_for_children() -> anyhow::Result<()> {
        // If we wait for sleep, this will time out.
        let mut cmd = if cfg!(windows) {
            async_background_command("powershell")
        } else {
            async_background_command("sh")
        };
        if cfg!(windows) {
            cmd.args(&[
                "-c",
                "Start-Job -ScriptBlock {sleep 10} | Out-Null; echo hello",
            ]);
        } else {
            cmd.args(&["-c", "(sleep 10 &) && echo hello"]);
        }

        let timeout = if cfg!(windows) { 5 } else { 1 };
        let (status, stdout, stderr) =
            gather_output(cmd, Some(Duration::from_secs(timeout))).await?;
        assert!(matches!(status, GatherOutputStatus::Finished(s) if s.code() == Some(0)));
        assert_eq!(str::from_utf8(&stdout)?.trim(), "hello");
        assert_eq!(stderr, b"");

        Ok(())
    }

    #[tokio::test]
    async fn test_gather_output_timeout() -> anyhow::Result<()> {
        let now = Instant::now();

        let mut cmd = if cfg!(windows) {
            async_background_command("powershell")
        } else {
            async_background_command("sh")
        };
        cmd.args(&["-c", "echo hello; sleep 10; echo bye"]);

        let timeout = if cfg!(windows) { 5 } else { 1 };
        let (status, stdout, stderr) =
            gather_output(cmd, Some(Duration::from_secs(timeout))).await?;
        assert!(matches!(status, GatherOutputStatus::TimedOut(..)));
        assert_eq!(str::from_utf8(&stdout)?.trim(), "hello");
        assert_eq!(stderr, b"");

        assert!(now.elapsed() < Duration::from_secs(9)); // Lots of leeway here.

        Ok(())
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn test_spawn_retry_txt_busy() -> anyhow::Result<()> {
        use futures::future;
        use tokio::fs::OpenOptions;
        use tokio::io::AsyncWriteExt;

        let tempdir = tempfile::tempdir()?;
        let bin = tempdir.path().join("bin");

        let mut file = OpenOptions::new()
            .mode(0o755)
            .write(true)
            .create(true)
            .open(&bin)
            .await?;

        file.write_all(b"#!/bin/bash\ntrue\n").await?;

        let cmd = async_background_command(&bin);
        let mut child = spawn_retry_txt_busy(cmd, {
            let mut file = Some(file);
            move || {
                file.take();
                future::ready(())
            }
        })
        .await?;

        let status = child.wait().await?;
        assert_eq!(status.code(), Some(0));

        Ok(())
    }

    #[tokio::test]
    async fn test_spawn_retry_other_error() -> anyhow::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let bin = tempdir.path().join("bin"); // Does not actually exist

        let cmd = async_background_command(&bin);
        let res = spawn_retry_txt_busy(cmd, || async { panic!("Should not be called!") }).await;
        assert!(res.is_err());

        Ok(())
    }

    fn artifact_fs(project_fs: ProjectFilesystem) -> ArtifactFs {
        ArtifactFs::new(
            BuckPathResolver::new(CellResolver::of_names_and_paths(&[(
                CellName::unchecked_new("cell".into()),
                CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("cell_path".into())),
            )])),
            BuckOutPathResolver::new(ProjectRelativePathBuf::unchecked_new("buck_out/v2".into())),
            project_fs,
        )
    }

    fn test_executor() -> anyhow::Result<(LocalExecutor, AbsPathBuf, impl Drop)> {
        let dir = tempfile::tempdir()?;
        let root = AbsPathBuf::try_from(dir.path().canonicalize()?)?;
        let project_fs = ProjectFilesystem::new(root.clone());
        let artifact_fs = artifact_fs(project_fs.clone());

        let executor = LocalExecutor::new(
            artifact_fs,
            Arc::new(NoDiskMaterializer),
            Arc::new(DummyBlockingExecutor { fs: project_fs }),
            Arc::new(HostSharingBroker::new(
                HostSharingStrategy::SmallerTasksFirst,
                1,
            )),
            root.clone(),
        );

        Ok((executor, root, dir))
    }

    #[tokio::test]
    async fn test_exec_cmd_environment() -> anyhow::Result<()> {
        let (executor, root, _tmpdir) = test_executor()?;

        let interpreter = if cfg!(windows) { "powershell" } else { "sh" };
        let (status, stdout, _) = executor
            .exec(
                interpreter,
                &["-c", "echo $PWD; pwd"],
                &HashMap::<String, String>::default(),
                None,
                None,
                None,
            )
            .await?;
        assert!(matches!(status, GatherOutputStatus::Finished(s) if s.code() == Some(0)));

        let stdout = std::str::from_utf8(&stdout).context("Invalid stdout")?;

        if cfg!(windows) {
            let lines: Vec<&str> = stdout.split("\r\n").collect();
            let expected_path = format!("Microsoft.PowerShell.Core\\FileSystem::{}", root);

            assert_eq!(lines[3], expected_path);
            assert_eq!(lines[4], expected_path);
        } else {
            assert_eq!(stdout, format!("{}\n{}\n", root, root));
        }

        Ok(())
    }

    #[cfg(unix)] // TODO: something similar on Windows: T123279320
    #[tokio::test]
    async fn test_exec_cmd_environment_filtering() -> anyhow::Result<()> {
        let (executor, _root, _tmpdir) = test_executor()?;

        let (status, stdout, _) = executor
            .exec(
                "sh",
                &["-c", "echo $USER"],
                &HashMap::<String, String>::default(),
                None,
                None,
                Some(&EnvironmentInheritance::empty()),
            )
            .await?;
        assert!(matches!(status, GatherOutputStatus::Finished(s) if s.code() == Some(0)));
        assert_eq!(stdout, "\n".as_bytes());

        Ok(())
    }

    #[cfg(unix)]
    mod interruptible_async_read {
        use std::io::Read;
        use std::sync::Mutex;

        use assert_matches::assert_matches;
        use gazebo::prelude::*;
        use unix_async_read_ready::AsSyncReader;

        use super::*;

        #[derive(Copy, Clone, Dupe)]
        enum StubAsyncReadState {
            Done,
            Pending,
            Ready(u8),
        }

        /// A stub AsyncRead implementation
        #[derive(Clone, Dupe)]
        #[pin_project]
        struct StubAsyncRead {
            state: Arc<Mutex<StubAsyncReadState>>,
        }

        impl StubAsyncRead {
            fn new() -> Self {
                Self {
                    state: Arc::new(Mutex::new(StubAsyncReadState::Pending)),
                }
            }

            /// Push a byte to this. Doesn't check that this makes sense.
            fn push(&self, byte: u8) {
                let mut state = self.state.lock().unwrap();
                *state = StubAsyncReadState::Ready(byte);
            }

            /// Set this to done. Also doens't check that this makes sense.
            fn done(&self) {
                let mut state = self.state.lock().unwrap();
                *state = StubAsyncReadState::Done;
            }
        }

        impl AsSyncReader for StubAsyncRead {
            type SyncReader<'a> = &'a mut Self;

            fn as_sync_reader(&mut self) -> Self::SyncReader<'_> {
                self
            }
        }

        impl AsyncRead for StubAsyncRead {
            /// A poll_read implementation for our stub. Note that this never stores any wakers,
            /// that's OK because our tests do explicit poll!().
            fn poll_read(
                self: Pin<&mut Self>,
                _: &mut Context<'_>,
                buf: &mut ReadBuf<'_>,
            ) -> Poll<io::Result<()>> {
                let this = self.project();
                let mut state = this.state.lock().unwrap();

                match *state {
                    StubAsyncReadState::Done => Poll::Ready(Ok(())),
                    StubAsyncReadState::Pending => Poll::Pending,
                    StubAsyncReadState::Ready(b) => {
                        buf.put_slice(&[b]);
                        *state = StubAsyncReadState::Pending;
                        Poll::Ready(Ok(()))
                    }
                }
            }
        }

        impl Read for StubAsyncRead {
            fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
                let mut state = self.state.lock().unwrap();

                match *state {
                    StubAsyncReadState::Done => Ok(0),
                    StubAsyncReadState::Pending => Err(io::ErrorKind::WouldBlock.into()),
                    StubAsyncReadState::Ready(b) => {
                        buf[0] = b;
                        *state = StubAsyncReadState::Pending;
                        Ok(1)
                    }
                }
            }
        }

        #[tokio::test]
        async fn test_drain_eof() {
            let read = StubAsyncRead::new();
            let (_close, closed) = oneshot::channel::<()>();
            let interruptible = InterruptibleAsyncRead {
                read: read.dupe(),
                interrupt: closed.map(|_| ()),
            };

            let drain = interruptible.drain();
            futures::pin_mut!(drain);

            // No bytes, it's pending.
            assert_matches!(futures::poll!(drain.as_mut()), Poll::Pending);

            // Add a byte, still pending, because more may come.
            read.push(b'f');
            assert_matches!(futures::poll!(drain.as_mut()), Poll::Pending);

            // Mark our reader done, we expect this to be ready.
            read.done();
            assert_matches!(futures::poll!(drain.as_mut()), Poll::Ready(Ok(ret)) => {
                assert_eq!(&ret, "f".as_bytes());
            });
        }

        #[tokio::test]
        async fn test_drain_interrupt() {
            let read = StubAsyncRead::new();
            let (close, recv) = oneshot::channel::<()>();
            let interruptible = InterruptibleAsyncRead {
                read: read.dupe(),
                interrupt: recv.map(|_| ()),
            };

            let drain = interruptible.drain();
            futures::pin_mut!(drain);

            // No bytes, it's pending.
            assert_matches!(futures::poll!(drain.as_mut()), Poll::Pending);

            // Add a byte, still pending, because more may come.
            read.push(b'f');
            assert_matches!(futures::poll!(drain.as_mut()), Poll::Pending);

            // Close the reader.
            close.send(()).unwrap();

            // Mark our reader done, we expect this to be ready.
            assert_matches!(futures::poll!(drain.as_mut()), Poll::Ready(Ok(ret)) => {
                assert_eq!(&ret, "f".as_bytes());
            });
        }

        #[tokio::test]
        async fn test_drain_finish() {
            let read = StubAsyncRead::new();
            let (close, recv) = oneshot::channel::<()>();
            let interruptible = InterruptibleAsyncRead {
                read: read.dupe(),
                interrupt: recv.map(|_| ()),
            };

            let drain = interruptible.drain();
            futures::pin_mut!(drain);

            // No bytes, it's pending.
            assert_matches!(futures::poll!(drain.as_mut()), Poll::Pending);

            // Add a byte, still pending, because more may come.
            read.push(b'f');
            assert_matches!(futures::poll!(drain.as_mut()), Poll::Pending);

            // Close the reader.
            close.send(()).unwrap();

            // But! Add more stuff.
            read.push(b'o');

            // We now expect this to read what's left.
            assert_matches!(futures::poll!(drain.as_mut()), Poll::Ready(Ok(ret)) => {
                assert_eq!(&ret, "fo".as_bytes());
            });
        }
    }
}
