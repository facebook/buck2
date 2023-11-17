/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

mod interruptible_async_read;
pub mod process_group;
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
use tokio_util::codec::BytesCodec;
use tokio_util::codec::FramedRead;

use self::interruptible_async_read::InterruptNotifiable;
use self::interruptible_async_read::InterruptibleAsyncRead;
use self::status_decoder::DecodedStatus;
use self::status_decoder::DefaultStatusDecoder;
use self::status_decoder::StatusDecoder;
use crate::run::process_group::ProcessGroup;

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
    process_group: anyhow::Result<ProcessGroup>,
    cancellation: T,
    decoder: impl StatusDecoder,
    kill_process: impl KillProcess,
    stream_stdio: bool,
) -> anyhow::Result<impl Stream<Item = anyhow::Result<CommandEvent>>>
where
    T: Future<Output = anyhow::Result<GatherOutputStatus>> + Send,
{
    let mut process_group = match process_group {
        Ok(process_group) => process_group,
        Err(e) => {
            let event = Ok(CommandEvent::Exit(GatherOutputStatus::SpawnFailed(
                e.to_string(),
            )));
            return Ok(futures::stream::once(futures::future::ready(event)).left_stream());
        }
    };

    let stdio = if stream_stdio {
        let stdout = process_group
            .child()
            .stdout
            .take()
            .context("Child stdout is not piped")?;
        let stderr = process_group
            .child()
            .stderr
            .take()
            .context("Child stderr is not piped")?;

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
            let status = process_group.child().wait();
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
                    .kill(&mut process_group)
                    .await
                    .context("Failed to terminate child after timeout")?;

                decoder
                    .cancel()
                    .await
                    .context("Failed to cancel status decoder after timeout")?;

                // We just killed the child, so this should finish immediately. We should still call
                // this to release any process.
                process_group
                    .child()
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

    let process_details =
        spawn_retry_txt_busy(cmd, || tokio::time::sleep(Duration::from_millis(50))).await;

    let stream = stream_command_events(
        process_details,
        cancellation,
        DefaultStatusDecoder,
        DefaultKillProcess::default(),
        true,
    )?;
    decode_command_event_stream(stream).await
}

/// Dependency injection for kill. We use this in testing.
#[async_trait]
pub trait KillProcess {
    async fn kill(self, process: &mut ProcessGroup) -> anyhow::Result<()>;
}

#[derive(Default)]
pub struct DefaultKillProcess {
    pub graceful_shutdown_timeout_s: Option<u32>,
}

#[async_trait]
impl KillProcess for DefaultKillProcess {
    async fn kill(self, process_group: &mut ProcessGroup) -> anyhow::Result<()> {
        let pid = match process_group.child().id() {
            Some(pid) => pid,
            None => {
                // Child just exited, so in this case we don't want to kill anything.
                return Ok(());
            }
        };
        tracing::info!("Killing process {}", pid);

        if cfg!(any(windows, unix)) {
            return process_group.kill(self.graceful_shutdown_timeout_s).await;
        }

        process_group
            .child()
            .start_kill()
            .map_err(anyhow::Error::from)
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
        // On windows we create suspended process to assign it to a job (group) and then resume.
        // This is necessary because the process might finish before we add it to a job
        use std::os::windows::process::CommandExt;
        cmd.creation_flags(
            winapi::um::winbase::CREATE_NO_WINDOW | winapi::um::winbase::CREATE_SUSPENDED,
        );
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
) -> anyhow::Result<ProcessGroup>
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
            return res.map_err(anyhow::Error::from).and_then(ProcessGroup::new);
        }

        delay().await;

        attempts -= 1;
    }
}

#[cfg(test)]
mod tests {
    use std::str;
    use std::str::FromStr;
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
        let mut process_group = spawn_retry_txt_busy(cmd, {
            let mut file = Some(file);
            move || {
                file.take();
                future::ready(())
            }
        })
        .await?;

        let status = process_group.child().wait().await?;
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
        use sysinfo::Pid;
        use sysinfo::ProcessExt;
        use sysinfo::System;
        use sysinfo::SystemExt;

        let cmd = if cfg!(windows) {
            let mut cmd = background_command("powershell");
            cmd.args([
                "-c",
                "echo $PID; Start-Process -FilePath \"powershell\" -Wait -NoNewWindow -ArgumentList \
                'echo $PID; Sleep 1000'",
            ]);
            cmd
        } else {
            let mut cmd = background_command("sh");
            cmd.arg("-c")
                .arg("( ( echo $PPID && echo $$ && sleep 1000 ) )");
            cmd
        };

        // On windows we need more time to run powershell
        let timeout = if cfg!(windows) { 5 } else { 1 };
        let (_status, stdout, _stderr) = gather_output(
            cmd,
            timeout_into_cancellation(Some(Duration::from_secs(timeout))),
        )
        .await?;
        let out = str::from_utf8(&stdout)?;
        let pids: Vec<&str> = out.split('\n').collect();
        let ppid = Pid::from_str(pids.first().context("no ppid")?.trim())?;
        let pid = Pid::from_str(pids.get(1).context("no pid")?.trim())?;
        let sys = System::new_all();

        // we want to check if existed process doesn't have the same parent because of pid reuse
        if let Some(process) = sys.process(pid) {
            if let Some(parent) = process.parent() {
                if parent != ppid {
                    return Ok(());
                }
            }
            return Err(anyhow::anyhow!("PID still exits: {}", pid));
        }
        Ok(())
    }

    #[tokio::test]
    async fn test_stream_command_events_ends() -> anyhow::Result<()> {
        let mut cmd = if cfg!(windows) {
            background_command("powershell")
        } else {
            background_command("sh")
        };
        cmd.args(["-c", "exit 0"]);

        let process = prepare_command(cmd)
            .spawn()
            .map_err(anyhow::Error::from)
            .and_then(ProcessGroup::new);
        let mut events = stream_command_events(
            process,
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
            async fn kill(self, process_group: &mut ProcessGroup) -> anyhow::Result<()> {
                *self.killed.lock().unwrap() = true;

                // We still need to kill the process. On Windows in particular our test will hang
                // if we do not.
                DefaultKillProcess::default().kill(process_group).await
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
        let process = cmd
            .spawn()
            .map_err(anyhow::Error::from)
            .and_then(ProcessGroup::new);

        let stream = stream_command_events(
            process,
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

        let process_group = cmd
            .spawn()
            .map_err(anyhow::Error::from)
            .and_then(ProcessGroup::new);
        let mut events = stream_command_events(
            process_group,
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
