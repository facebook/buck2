/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

mod interruptible_async_read;
pub mod status_decoder;

use std::borrow::Cow;
use std::path::Path;
use std::pin::Pin;
use std::process::Command;
use std::process::ExitStatus;
use std::process::Stdio;
use std::task::Context;
use std::task::Poll;
use std::time::Duration;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_path::AbsPath;
use bytes::Bytes;
use futures::future::Future;
use futures::future::FutureExt;
use futures::stream::Stream;
use futures::stream::StreamExt;
use futures::stream::TryStreamExt;
use pin_project::pin_project;
use tokio::process::Child;
use tokio_util::codec::BytesCodec;
use tokio_util::codec::FramedRead;

use self::interruptible_async_read::InterruptNotifiable;
use self::interruptible_async_read::InterruptibleAsyncRead;
use self::status_decoder::DecodedStatus;
use self::status_decoder::DefaultStatusDecoder;
use self::status_decoder::StatusDecoder;

#[derive(Debug)]
pub enum GatherOutputStatus {
    /// Contains the exit code.
    Finished {
        exit_code: i32,
        execution_stats: Option<buck2_data::CommandExecutionStats>,
    },
    TimedOut(Duration),
    Cancelled,
    SpawnFailed(String),
}

impl From<DecodedStatus> for GatherOutputStatus {
    fn from(d: DecodedStatus) -> Self {
        match d {
            DecodedStatus::Status {
                exit_code,
                execution_stats,
            } => Self::Finished {
                exit_code,
                execution_stats,
            },
            DecodedStatus::SpawnFailed(v) => Self::SpawnFailed(v),
        }
    }
}

#[derive(Debug)]
pub enum CommandEvent {
    Stdout(Bytes),
    Stderr(Bytes),
    Exit(GatherOutputStatus),
}

enum StdioEvent {
    Stdout(Bytes),
    Stderr(Bytes),
}

impl From<StdioEvent> for CommandEvent {
    fn from(stdio: StdioEvent) -> Self {
        match stdio {
            StdioEvent::Stdout(bytes) => CommandEvent::Stdout(bytes),
            StdioEvent::Stderr(bytes) => CommandEvent::Stderr(bytes),
        }
    }
}

/// This stream will yield [CommandEvent] whenever we have something on stdout or stderr (this is
/// our stdio stream), and it'll finish up the stream with the exit status. This is basically like
/// a select, but with the exit guaranteed to come last.
#[pin_project]
struct CommandEventStream<Status, Stdio> {
    exit: Option<anyhow::Result<GatherOutputStatus>>,

    done: bool,

    #[pin]
    status: futures::future::Fuse<Status>,

    #[pin]
    stdio: futures::stream::Fuse<Stdio>,
}

impl<Status, Stdio> CommandEventStream<Status, Stdio>
where
    Status: Future,
    Stdio: Stream,
{
    fn new(status: Status, stdio: Stdio) -> Self {
        Self {
            exit: None,
            done: false,
            status: status.fuse(),
            stdio: stdio.fuse(),
        }
    }
}

impl<Status, Stdio> Stream for CommandEventStream<Status, Stdio>
where
    Status: Future<Output = anyhow::Result<GatherOutputStatus>>,
    Stdio: Stream<Item = anyhow::Result<StdioEvent>> + InterruptNotifiable,
{
    type Item = anyhow::Result<CommandEvent>;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let mut this = self.project();

        if *this.done {
            return Poll::Ready(None);
        }

        // This future is fused so it's guaranteed to be ready once. If it does, capture the exit
        // status, we'll return it later.
        if let Poll::Ready(status) = this.status.poll(cx) {
            *this.exit = Some(status);
            this.stdio.as_mut().get_pin_mut().notify_interrupt();
        }

        // This stream is also fused, so if it returns None, we'll know it's done for good and we'll
        // return the exit status if it's available.
        if let Some(stdio) = futures::ready!(this.stdio.poll_next(cx)) {
            return Poll::Ready(Some(stdio.map(|event| event.into())));
        }

        // If we got here that means the stream is done. If we have it we return, and if we don't
        // we report we're pending, because we'll have polled it already earlier.
        if let Some(exit) = this.exit.take() {
            *this.done = true;
            return Poll::Ready(Some(exit.map(CommandEvent::Exit)));
        }

        Poll::Pending
    }
}

pub async fn timeout_into_cancellation(
    timeout: Option<Duration>,
) -> anyhow::Result<GatherOutputStatus> {
    match timeout {
        Some(t) => {
            tokio::time::sleep(t).await;
            Ok(GatherOutputStatus::TimedOut(t))
        }
        None => futures::future::pending().await,
    }
}

pub fn stream_command_events<T>(
    process_details: anyhow::Result<ProcessDetails>,
    cancellation: T,
    decoder: impl StatusDecoder,
    kill_process: impl KillProcess,
    stream_stdio: bool,
) -> anyhow::Result<impl Stream<Item = anyhow::Result<CommandEvent>>>
where
    T: Future<Output = anyhow::Result<GatherOutputStatus>> + Send,
{
    let mut process_details = match process_details {
        Ok(process_details) => process_details,
        Err(e) => {
            let event = Ok(CommandEvent::Exit(GatherOutputStatus::SpawnFailed(
                e.to_string(),
            )));
            return Ok(futures::stream::once(futures::future::ready(event)).left_stream());
        }
    };

    let stdio = if stream_stdio {
        let stdout = process_details.child.stdout.take().context("Child stdout is not piped")?;
        let stderr = process_details.child.stderr.take().context("Child stderr is not piped")?;

        #[cfg(unix)]
        type Drainer<R> = self::interruptible_async_read::UnixNonBlockingDrainer<R>;

        // On Windows, for the time being we just give ourselves a timeout to finish reading.
        // Ideally this would perform a non-blocking read on self instead like we do on Unix.
        #[cfg(not(unix))]
        type Drainer<R> = self::interruptible_async_read::TimeoutDrainer<R>;

        let stdout = InterruptibleAsyncRead::<_, Drainer<_>>::new(stdout);
        let stderr = InterruptibleAsyncRead::<_, Drainer<_>>::new(stderr);
        let stdout = FramedRead::new(stdout, BytesCodec::new())
            .map(|data| anyhow::Ok(StdioEvent::Stdout(data?.freeze())));
        let stderr = FramedRead::new(stderr, BytesCodec::new())
            .map(|data| anyhow::Ok(StdioEvent::Stderr(data?.freeze())));

        futures::stream::select(stdout, stderr).left_stream()
    } else {
        futures::stream::empty().right_stream()
    };

    let status = async move {
        enum Outcome {
            Finished(ExitStatus),
            Cancelled(GatherOutputStatus),
        }

        // NOTE: This wrapping here is so that we release the borrow of `child` that stems from
        // `wait()` by the time we call kill_process a few lines down.
        let execute = async {
            let status = process_details.child.wait();
            futures::pin_mut!(status);
            futures::pin_mut!(cancellation);

            anyhow::Ok(match futures::future::select(status, cancellation).await {
                futures::future::Either::Left((status, _)) => Outcome::Finished(status?),
                futures::future::Either::Right((res, _)) => Outcome::Cancelled(res?),
            })
        };

        anyhow::Ok(match execute.await? {
            Outcome::Finished(status) => decoder.decode_status(status).await?.into(),
            Outcome::Cancelled(res) => {
                kill_process
                    .kill(&mut process_details)
                    .await
                    .context("Failed to terminate child after timeout")?;

                decoder
                    .cancel()
                    .await
                    .context("Failed to cancel status decoder after timeout")?;

                // We just killed the child, so this should finish immediately. We should still call
                // this to release any process.
                process_details.child
                    .wait()
                    .await
                    .context("Failed to await child after kill")?;

                res
            }
        })
    };

    Ok(CommandEventStream::new(status, stdio).right_stream())
}

pub(crate) async fn decode_command_event_stream<S>(
    stream: S,
) -> anyhow::Result<(GatherOutputStatus, Vec<u8>, Vec<u8>)>
where
    S: Stream<Item = anyhow::Result<CommandEvent>>,
{
    futures::pin_mut!(stream);

    let mut stdout = Vec::<u8>::new();
    let mut stderr = Vec::<u8>::new();

    while let Some(event) = stream.try_next().await? {
        match event {
            CommandEvent::Stdout(bytes) => stdout.extend(&bytes),
            CommandEvent::Stderr(bytes) => stderr.extend(&bytes),
            CommandEvent::Exit(exit) => return Ok((exit, stdout, stderr)),
        }
    }

    Err(anyhow::Error::msg(
        "Stream did not yield CommandEvent::Exit",
    ))
}

pub async fn gather_output<T>(
    cmd: Command,
    cancellation: T,
) -> anyhow::Result<(GatherOutputStatus, Vec<u8>, Vec<u8>)>
where
    T: Future<Output = anyhow::Result<GatherOutputStatus>> + Send,
{
    let cmd = prepare_command(cmd);

    let process_details = spawn_retry_txt_busy(cmd, || tokio::time::sleep(Duration::from_millis(50))).await;

    let stream = stream_command_events(
        process_details,
        cancellation,
        DefaultStatusDecoder,
        DefaultKillProcess::default(),
        true,
    )?;
    decode_command_event_stream(stream).await
}

#[cfg(windows)]
pub struct WindowsJobHandle {
    job_handle_as_integer: usize,
}

#[cfg(windows)]
impl Drop for WindowsJobHandle {
    fn drop(&mut self) {
        let job_handle = self.job_handle_as_integer as *mut winapi::ctypes::c_void;
        let close_handle_result;
        unsafe {
            close_handle_result = winapi::um::handleapi::CloseHandle(job_handle);
        }

        if close_handle_result == winapi::shared::minwindef::FALSE {
            tracing::info!("Failed to close job handle with return code {}", close_handle_result);
        }
    }
}

#[cfg(windows)]
impl WindowsJobHandle {
    pub fn new(job_handle: *mut winapi::ctypes::c_void) -> WindowsJobHandle {
        WindowsJobHandle {
            job_handle_as_integer: job_handle as usize,
        }
    }

    pub fn get_handle(&self) -> *mut winapi::ctypes::c_void {
        self.job_handle_as_integer as *mut winapi::ctypes::c_void
    }
}

pub struct ProcessDetails {
    child: Child,
    #[cfg(windows)]
    job_handle: WindowsJobHandle,
}

impl ProcessDetails {
    #[cfg(windows)]
    pub fn new(child: tokio::process::Child) -> anyhow::Result<ProcessDetails> {
        let job_handle = add_process_to_job(&child)?;
        resume_process(&child)?;
        Ok(ProcessDetails {
            child,
            job_handle,
        })
    }

    #[cfg(unix)]
    pub fn new(child: tokio::process::Child) -> anyhow::Result<ProcessDetails> {
        Ok(ProcessDetails {
            child,
        })
    }
}

/// Dependency injection for kill. We use this in testing.
#[async_trait]
pub trait KillProcess {
    async fn kill(self, process_details: &mut ProcessDetails) -> anyhow::Result<()>;
}

#[derive(Default)]
pub struct DefaultKillProcess {
    pub graceful_shutdown_timeout_s: Option<u32>,
}

#[async_trait]
impl KillProcess for DefaultKillProcess {
    async fn kill(self, process_details: &mut ProcessDetails) -> anyhow::Result<()> {
        let pid = match process_details.child.id() {
            Some(pid) => pid,
            None => {
                // Child just exited, so in this case we don't want to kill anything.
                return Ok(());
            }
        };
        tracing::info!("Killing process {}", pid);
        #[cfg(unix)]
        {
            if true {
                // On unix we want killpg, so we don't use the default impl.
                // We use `if true` here to do less conditional compilation
                // or conditional dependencies.
                return kill_process_impl(pid, self.graceful_shutdown_timeout_s).await;
            }
        }

        #[cfg(windows)]
        {
            if true {
                // On Windows we use Jobs to manage the processes so we don't
                // use the default impl.
                // We use `if true` here to do less conditional compilation
                // or conditional dependencies.
                return kill_job(&process_details.job_handle);
            }
        }

        process_details.child.start_kill().map_err(anyhow::Error::from)
    }
}

#[cfg(unix)]
async fn kill_process_impl(
    pid: u32,
    graceful_shutdown_timeout_s: Option<u32>,
) -> anyhow::Result<()> {
    use buck2_common::kill_util::try_terminate_process_gracefully;
    use nix::sys::signal;
    use nix::sys::signal::Signal;
    use nix::unistd::Pid;

    let pid: i32 = pid.try_into().context("PID does not fit a i32")?;

    if let Some(graceful_shutdown_timeout_s) = graceful_shutdown_timeout_s {
        try_terminate_process_gracefully(
            pid,
            Duration::from_secs(graceful_shutdown_timeout_s as u64),
        )
        .await
        .with_context(|| format!("Failed to terminate process {} gracefully", pid))
    } else {
        signal::killpg(Pid::from_raw(pid), Signal::SIGKILL)
            .with_context(|| format!("Failed to kill process {}", pid))
    }
}

#[cfg(windows)]
fn kill_job(job_handle: &WindowsJobHandle) -> anyhow::Result<()> {
    unsafe {
        let terminate_job_result = winapi::um::jobapi2::TerminateJobObject(job_handle.get_handle(), 1);

        if terminate_job_result == winapi::shared::minwindef::FALSE {
            Err(anyhow::Error::msg("Failed to terminate job to kill"))
        }
        else {
            Ok(())
        }
    }
}

/// Unify the the behavior of using a relative path for the executable between Unix and Windows. On
/// UNIX, the path is understood to be relative to the cwd of the *spawned process*, whereas on
/// Windows, it's relative ot the cwd of the *spawning* process.
///
/// Here, we unify the two behaviors since we always run our subprocesses with a known cwd: we
/// check if the executable actually exists relative to said cwd, and if it does, we use that.
pub fn maybe_absolutize_exe<'a>(
    exe: &'a (impl AsRef<Path> + ?Sized),
    spawned_process_cwd: &'_ AbsPath,
) -> anyhow::Result<Cow<'a, Path>> {
    let exe = exe.as_ref();

    let abs = spawned_process_cwd.join(exe);
    if fs_util::try_exists(&abs).context("Error absolute-izing executable")? {
        return Ok(abs.into_path_buf().into());
    }

    Ok(exe.into())
}

pub fn prepare_command(mut cmd: Command) -> tokio::process::Command {
    #[cfg(unix)]
    {
        use std::os::unix::process::CommandExt;
        cmd.process_group(0);
    }

    #[cfg(windows)]
    {
        use std::os::windows::process::CommandExt;
        cmd.creation_flags(winapi::um::winbase::CREATE_NO_WINDOW | winapi::um::winbase::CREATE_SUSPENDED);
    }

    cmd.stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    cmd.into()
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
async fn spawn_retry_txt_busy<F, D>(
    mut cmd: tokio::process::Command,
    mut delay: F,
) -> anyhow::Result<ProcessDetails>
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
            return res.map_err(anyhow::Error::from).and_then(ProcessDetails::new);
        }

        delay().await;

        attempts -= 1;
    }
}

#[cfg(windows)]
fn add_process_to_job(child: &tokio::process::Child) -> anyhow::Result<WindowsJobHandle> {
    let raw_process_handle = child.raw_handle().ok_or_else(|| anyhow::Error::msg("Failed to get the raw handle to the process"))?;
    unsafe {
        let raw_job_handle = winapi::um::jobapi2::CreateJobObjectW(std::ptr::null_mut(), std::ptr::null());
        if raw_job_handle.is_null() {
            return Err(anyhow::Error::msg("Failed to create job"));
        }
        let job_handle = WindowsJobHandle::new(raw_job_handle);

        let assign_process_result = winapi::um::jobapi2::AssignProcessToJobObject(raw_job_handle, raw_process_handle);

        if assign_process_result == winapi::shared::minwindef::FALSE {
            Err(anyhow::Error::msg("Failed to assign process to job"))
        }
        else {
            Ok(job_handle)
        }
    }
}

#[cfg(windows)]
fn resume_process(child: &tokio::process::Child) -> anyhow::Result<()> {
    let process_id = child.id().ok_or_else(|| anyhow::Error::msg("Failed to get the process id"))?;
    let main_thread_id = get_main_thread(process_id)?;
    resume_thread(main_thread_id)
}

#[cfg(windows)]
fn get_main_thread(process_id: u32) -> anyhow::Result<winapi::shared::minwindef::DWORD> {
    unsafe {
        let snapshot_handle = winapi::um::tlhelp32::CreateToolhelp32Snapshot(winapi::um::tlhelp32::TH32CS_SNAPTHREAD, 0);

        if snapshot_handle == winapi::um::handleapi::INVALID_HANDLE_VALUE {
            return Err(anyhow::Error::msg("Failed to list threads"))
        }

        let mut thread_entry_32 = winapi::um::tlhelp32::THREADENTRY32 {
            dwSize: std::mem::size_of::<winapi::um::tlhelp32::THREADENTRY32>() as u32,
            cntUsage: 0,
            th32ThreadID: 0,
            th32OwnerProcessID: 0,
            tpBasePri: 0,
            tpDeltaPri: 0,
            dwFlags: 0,
        };
        let raw_pointer_to_thread_entry_32 = &mut thread_entry_32  as *mut winapi::um::tlhelp32::THREADENTRY32;

        let mut main_thread_id : Option<winapi::shared::minwindef::DWORD> = None;

        let mut thread_result = winapi::um::tlhelp32::Thread32First(snapshot_handle, raw_pointer_to_thread_entry_32);
        while thread_result == winapi::shared::minwindef::TRUE {
            if thread_entry_32.dwSize as usize >= std::mem::offset_of!(winapi::um::tlhelp32::THREADENTRY32, th32OwnerProcessID) {
                if thread_entry_32.th32OwnerProcessID == process_id {
                    main_thread_id = Some(thread_entry_32.th32ThreadID);
                    break;
                }
            }
            thread_result = winapi::um::tlhelp32::Thread32Next(snapshot_handle, raw_pointer_to_thread_entry_32);
        }

        let close_handle_result = winapi::um::handleapi::CloseHandle(snapshot_handle);

        if let Some(thread_id) = main_thread_id {
            if close_handle_result == winapi::shared::minwindef::FALSE {
                Err(anyhow::Error::msg("Failed to close thread snapshot handle"))
            }
            else {
                Ok(thread_id)
            }
        }
        else {
            Err(anyhow::Error::msg("Failed to find thread to resume"))
        }
    }
}

#[cfg(windows)]
fn resume_thread(thread_id: winapi::shared::minwindef::DWORD) -> anyhow::Result<()> {
    unsafe {
        let thread_handle = winapi::um::processthreadsapi::OpenThread(winapi::um::winnt::THREAD_SUSPEND_RESUME, winapi::shared::minwindef::FALSE, thread_id);
        if thread_handle.is_null() {
            return Err(anyhow::Error::msg("Failed to open thread to resume"));
        }
        let resume_thread_result = winapi::um::processthreadsapi::ResumeThread(thread_handle);
        let close_handle_result = winapi::um::handleapi::CloseHandle(thread_handle);
        if resume_thread_result == winapi::shared::minwindef::DWORD::MAX {
            Err(anyhow::Error::msg("Failed to resume thread"))
        }
        else if close_handle_result == winapi::shared::minwindef::FALSE {
            Err(anyhow::Error::msg("Failed to close thread handle"))
        }
        else {
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use std::str;
    use std::sync::Arc;
    use std::sync::Mutex;
    use std::time::Instant;

    use assert_matches::assert_matches;
    use buck2_util::process::async_background_command;
    use buck2_util::process::background_command;
    use dupe::Dupe;

    use super::*;

    #[tokio::test]
    async fn test_gather_output() -> anyhow::Result<()> {
        let mut cmd = if cfg!(windows) {
            background_command("powershell")
        } else {
            background_command("sh")
        };
        cmd.args(["-c", "echo hello"]);

        let (status, stdout, stderr) = gather_output(cmd, futures::future::pending()).await?;
        assert!(matches!(status, GatherOutputStatus::Finished { exit_code, .. } if exit_code == 0));
        assert_eq!(str::from_utf8(&stdout)?.trim(), "hello");
        assert_eq!(stderr, b"");

        Ok(())
    }

    #[tokio::test]
    async fn test_gather_does_not_wait_for_children() -> anyhow::Result<()> {
        // If we wait for sleep, this will time out.
        let mut cmd = if cfg!(windows) {
            background_command("powershell")
        } else {
            background_command("sh")
        };
        if cfg!(windows) {
            cmd.args([
                "-c",
                "Start-Job -ScriptBlock {sleep 10} | Out-Null; echo hello",
            ]);
        } else {
            cmd.args(["-c", "(sleep 10 &) && echo hello"]);
        }

        let timeout = if cfg!(windows) { 5 } else { 1 };
        let (status, stdout, stderr) = gather_output(
            cmd,
            timeout_into_cancellation(Some(Duration::from_secs(timeout))),
        )
        .await?;
        assert!(matches!(status, GatherOutputStatus::Finished { exit_code, .. } if exit_code == 0));
        assert_eq!(str::from_utf8(&stdout)?.trim(), "hello");
        assert_eq!(stderr, b"");

        Ok(())
    }

    #[tokio::test]
    async fn test_gather_output_timeout() -> anyhow::Result<()> {
        let now = Instant::now();

        let cmd = if cfg!(windows) {
            let mut cmd = background_command("powershell");
            cmd.args(["-c", "echo hello; sleep 10; echo bye"]);
            cmd
        } else {
            let mut cmd = background_command("sh");
            cmd.args(["-c", "echo hello && sleep 10 && echo bye"]);
            cmd
        };

        let timeout = if cfg!(windows) { 5 } else { 1 };
        let (status, stdout, _stderr) = gather_output(
            cmd,
            timeout_into_cancellation(Some(Duration::from_secs(timeout))),
        )
        .await?;
        assert!(matches!(status, GatherOutputStatus::TimedOut(..)));
        assert_eq!(str::from_utf8(&stdout)?.trim(), "hello");
        // Do not check stderr because stderr may contain a message like:
        // sh: line 1: 41348 Killed: 9

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

        file.write_all(b"#!/usr/bin/env bash\ntrue\n").await?;

        let cmd = async_background_command(&bin);
        let mut process_details = spawn_retry_txt_busy(cmd, {
            let mut file = Some(file);
            move || {
                file.take();
                future::ready(())
            }
        })
        .await?;

        let status = process_details.child.wait().await?;
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

    #[tokio::test]
    async fn test_kill_terminates_process_group() -> anyhow::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let tempfile_buffer = tempdir.path().join("stdout");
        let tempfile = tempfile_buffer.as_path();
        let tempfile_string = match tempfile.to_str() {
            None => Err(anyhow::anyhow!("Failed to create temporary file string")),
            Some(s) => Ok(s),
        }?;

        let cmd = if cfg!(windows) {
            let mut cmd = background_command("powershell");
            cmd.args(["-c", &("&(Get-Process -Id $PID).Path { Sleep 10; \"hello\" > ".to_owned() + tempfile_string + "}")]);
            cmd
        } else {
            let mut cmd = background_command("sh");
            cmd.arg("-c").arg("( ( sleep 10 && echo \"hello\" > ".to_owned() + tempfile_string + " ) )");
            cmd
        };
        let timeout = if cfg!(windows) { 5 } else { 1 };
        let (_status, _stdout, _stderr) =
            gather_output(cmd, timeout_into_cancellation(Some(Duration::from_secs(timeout)))).await?;

        tokio::time::sleep(Duration::from_secs(15)).await;

        if Path::exists(tempfile) {
            Err(anyhow::anyhow!("Subprocess was not killed"))
        } else {
            Ok(())
        }
    }

    #[tokio::test]
    async fn test_stream_command_events_ends() -> anyhow::Result<()> {
        let mut cmd = if cfg!(windows) {
            background_command("powershell")
        } else {
            background_command("sh")
        };
        cmd.args(["-c", "exit 0"]);

        let process_details = prepare_command(cmd).spawn().map_err(anyhow::Error::from).and_then(ProcessDetails::new);
        let mut events = stream_command_events(
            process_details,
            futures::future::pending(),
            DefaultStatusDecoder,
            DefaultKillProcess::default(),
            true,
        )?
        .boxed();
        assert_matches!(events.next().await, Some(Ok(CommandEvent::Exit(..))));
        assert_matches!(futures::poll!(events.next()), Poll::Ready(None));
        Ok(())
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn test_signal_exit_code() -> anyhow::Result<()> {
        use nix::sys::signal::Signal;

        let mut cmd = background_command("sh");
        cmd.arg("-c").arg("kill -KILL \"$$\"");
        let (status, _stdout, _stderr) = gather_output(cmd, futures::future::pending()).await?;

        assert_matches!(
            status,
            GatherOutputStatus::Finished { exit_code, .. } if exit_code == 128 + Signal::SIGKILL as i32
        );

        Ok(())
    }

    #[tokio::test]
    async fn timeout_kills_before_dropping_decoder() -> anyhow::Result<()> {
        struct Kill {
            killed: Arc<Mutex<bool>>,
        }

        #[async_trait]
        impl KillProcess for Kill {
            async fn kill(self, process_details: &mut ProcessDetails) -> anyhow::Result<()> {
                *self.killed.lock().unwrap() = true;

                // We still need to kill the process. On Windows in particular our test will hang
                // if we do not.
                DefaultKillProcess::default().kill(process_details).await
            }
        }

        struct Decoder {
            killed: Arc<Mutex<bool>>,
            cancelled: Arc<Mutex<bool>>,
        }

        #[async_trait::async_trait]
        impl StatusDecoder for Decoder {
            async fn decode_status(self, _status: ExitStatus) -> anyhow::Result<DecodedStatus> {
                panic!("Should not be called in this test since we timeout")
            }

            async fn cancel(self) -> anyhow::Result<()> {
                assert!(*self.killed.lock().unwrap());
                *self.cancelled.lock().unwrap() = true;
                Ok(())
            }
        }

        let killed = Arc::new(Mutex::new(false));
        let cancelled = Arc::new(Mutex::new(false));

        let mut cmd = if cfg!(windows) {
            background_command("powershell")
        } else {
            background_command("sh")
        };
        cmd.args(["-c", "sleep 10000"]);

        let mut cmd = prepare_command(cmd);
        let process_details = cmd.spawn().map_err(anyhow::Error::from).and_then(ProcessDetails::new);

        let stream = stream_command_events(
            process_details,
            timeout_into_cancellation(Some(Duration::from_secs(1))),
            Decoder {
                killed: killed.dupe(),
                cancelled: cancelled.dupe(),
            },
            Kill {
                killed: killed.dupe(),
            },
            true,
        )?;

        let (status, _stdout, _stderr) = decode_command_event_stream(stream).await?;
        assert!(matches!(status, GatherOutputStatus::TimedOut(..)));

        assert!(*killed.lock().unwrap());
        assert!(*cancelled.lock().unwrap());

        Ok(())
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn test_no_stdio_stream_command_events() -> anyhow::Result<()> {
        let mut cmd = background_command("sh");
        cmd.args(["-c", "echo hello"]);

        let mut cmd = prepare_command(cmd);
        let tempdir = tempfile::tempdir()?;
        let stdout = tempdir.path().join("stdout");
        cmd.stdout(std::fs::File::create(stdout.clone())?);

        let process_details = cmd.spawn().map_err(anyhow::Error::from).and_then(ProcessDetails::new);
        let mut events = stream_command_events(
            process_details,
            futures::future::pending(),
            DefaultStatusDecoder,
            DefaultKillProcess::default(),
            false,
        )?
        .boxed();
        assert_matches!(events.next().await, Some(Ok(CommandEvent::Exit(..))));
        assert_matches!(futures::poll!(events.next()), Poll::Ready(None));

        assert_matches!(tokio::fs::read_to_string(stdout).await?.as_str(), "hello\n");

        Ok(())
    }
}
