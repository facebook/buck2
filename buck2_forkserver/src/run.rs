/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io;
use std::io::Cursor;
use std::io::Read;
use std::marker::PhantomData;
use std::pin::Pin;
use std::process::ExitStatus;
use std::process::Stdio;
use std::task::Context;
use std::task::Poll;
use std::time::Duration;

use anyhow::Context as _;
use bytes::Bytes;
use futures::future::BoxFuture;
use futures::future::Future;
use futures::future::FutureExt;
use futures::stream::Stream;
use futures::stream::StreamExt;
use futures::stream::TryStreamExt;
use pin_project::pin_project;
use tokio::io::AsyncRead;
use tokio::io::AsyncReadExt;
use tokio::io::ReadBuf;
use tokio::process::Child;
use tokio::process::Command;
use tokio_util::codec::BytesCodec;
use tokio_util::codec::FramedRead;

pub enum GatherOutputStatus {
    Finished(ExitStatus),
    TimedOut(Duration),
}

enum CommandEvent {
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

#[pin_project]
struct CommandEventStream<Status, Stdio> {
    exit: Option<anyhow::Result<GatherOutputStatus>>,

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

        if let Poll::Ready(status) = this.status.poll(cx) {
            *this.exit = Some(status);
            this.stdio.as_mut().get_pin_mut().notify_interrupt();
        }

        if let Some(stdio) = futures::ready!(this.stdio.poll_next(cx)) {
            return Poll::Ready(Some(stdio.map(|event| event.into())));
        }

        if let Some(exit) = this.exit.take() {
            return Poll::Ready(Some(exit.map(CommandEvent::Exit)));
        }

        Poll::Pending
    }
}

trait InterruptNotifiable {
    fn notify_interrupt(self: Pin<&mut Self>);
}

async fn stream_command_events(
    mut cmd: Command,
    timeout: Option<Duration>,
) -> anyhow::Result<impl Stream<Item = anyhow::Result<CommandEvent>>> {
    cmd.stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    #[cfg(windows)]
    cmd.creation_flags(winapi::um::winbase::CREATE_NO_WINDOW);

    let mut child = spawn_retry_txt_busy(cmd, || tokio::time::sleep(Duration::from_millis(50)))
        .await
        .context("Failed to start command")?;

    let stdout = child.stdout.take().expect("piped() above");
    let stderr = child.stderr.take().expect("piped() above");

    #[cfg(unix)]
    type Drainer<R> = unix_non_blocking_drainer::UnixNonBlockingDrainer<R>;

    // On Windows, for the time being we just give ourselves a timeout to finish reading.
    // Ideally this would perform a non-blocking read on self instead like we do on Unix.
    #[cfg(not(unix))]
    type Drainer<R> = TimeoutDrainer<R>;

    let stdout = InterruptibleAsyncRead::<_, Drainer<_>>::new(stdout);
    let stderr = InterruptibleAsyncRead::<_, Drainer<_>>::new(stderr);

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

    let stdout = FramedRead::new(stdout, BytesCodec::new())
        .map(|data| anyhow::Ok(StdioEvent::Stdout(data?.freeze())));
    let stderr = FramedRead::new(stderr, BytesCodec::new())
        .map(|data| anyhow::Ok(StdioEvent::Stderr(data?.freeze())));

    let stdio = futures::stream::select(stdout, stderr);

    Ok(CommandEventStream::new(status, stdio))
}

async fn decode_command_event_stream<S>(
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

pub async fn gather_output(
    cmd: Command,
    timeout: Option<Duration>,
) -> anyhow::Result<(GatherOutputStatus, Vec<u8>, Vec<u8>)> {
    let stream = stream_command_events(cmd, timeout).await?;
    decode_command_event_stream(stream).await
}

fn kill_process(child: &Child) -> anyhow::Result<()> {
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

/// An [AsyncRead] that can be interrupted. The underlying [AsyncRead] will be polled while the
/// [Future] provided in interrupt hasn't completed. When said future completes, then we will
/// proceed to "drain" the reader, which means reading the data that is there but not waiting for
/// any further data to get written.
#[pin_project]
struct InterruptibleAsyncRead<R, D> {
    state: InterruptibleAsyncReadState<R, D>,
}

enum InterruptibleAsyncReadState<R, D> {
    Reading(R),
    Draining(D),
}

impl<R, D> InterruptibleAsyncRead<R, D>
where
    D: DrainerFromReader<R>,
{
    pub fn new(reader: R) -> Self {
        Self {
            state: InterruptibleAsyncReadState::Reading(reader),
        }
    }
}

impl<R, D> AsyncRead for InterruptibleAsyncRead<R, D>
where
    R: AsyncRead + Unpin,
    D: AsyncRead + Unpin + DrainerFromReader<R>,
{
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> Poll<io::Result<()>> {
        let mut this = self.project();

        match &mut this.state {
            InterruptibleAsyncReadState::Reading(reader) => Pin::new(reader).poll_read(cx, buf),
            InterruptibleAsyncReadState::Draining(drainer) => Pin::new(drainer).poll_read(cx, buf),
        }
    }
}

impl<R, D> InterruptNotifiable for InterruptibleAsyncRead<R, D>
where
    D: DrainerFromReader<R>,
{
    fn notify_interrupt(self: Pin<&mut Self>) {
        let this = self.project();

        take_mut::take(this.state, |state| match state {
            InterruptibleAsyncReadState::Reading(reader) => {
                InterruptibleAsyncReadState::Draining(D::from_reader(reader))
            }
            _ => unreachable!(),
        });
    }
}

impl<S1, S2> InterruptNotifiable for futures::stream::Select<S1, S2>
where
    S1: InterruptNotifiable,
    S2: InterruptNotifiable,
{
    fn notify_interrupt(self: Pin<&mut Self>) {
        let (s1, s2) = self.get_pin_mut();
        s1.notify_interrupt();
        s2.notify_interrupt();
    }
}

impl<S, F> InterruptNotifiable for futures::stream::Map<S, F>
where
    S: InterruptNotifiable,
{
    fn notify_interrupt(self: Pin<&mut Self>) {
        self.get_pin_mut().notify_interrupt();
    }
}

impl<T, D> InterruptNotifiable for FramedRead<T, D>
where
    T: InterruptNotifiable,
{
    fn notify_interrupt(self: Pin<&mut Self>) {
        self.get_pin_mut().notify_interrupt();
    }
}

/// This trait represents the ability to transition a Reader (R) to a Drainer (Self). Both are
/// [AsyncRead], but we expect the Drainer (which implements this trait) to not wait longer for
/// more data to be produced.
trait DrainerFromReader<R> {
    fn from_reader(reader: R) -> Self;
}

#[cfg(unix)]
mod unix_non_blocking_drainer {
    use std::os::unix::io::AsRawFd;
    use std::os::unix::prelude::RawFd;

    use nix::unistd;

    use super::*;

    /// Perform sync reads on an existing reader. We use this to bypass Tokio when draining an
    /// InterruptibleAsyncRead after it's been interrupted. This lets us ensure that even if Tokio
    /// hasn't completed `select()` on the pipe we want to drain, we'll stil get to execute
    /// `read()` (and potentially get WouldBlock if there is nothing to read and the pipe isn't
    /// ready).
    pub struct UnixNonBlockingDrainer<R> {
        fd: RawFd,
        // Kept so this is dropped and closed properly.
        _owner: R,
    }

    impl<R> DrainerFromReader<R> for UnixNonBlockingDrainer<R>
    where
        R: AsRawFd + Send + 'static,
    {
        fn from_reader(reader: R) -> Self {
            UnixNonBlockingDrainer {
                fd: reader.as_raw_fd(),
                _owner: reader,
            }
        }
    }

    impl<R> AsyncRead for UnixNonBlockingDrainer<R> {
        fn poll_read(
            self: Pin<&mut Self>,
            _cx: &mut Context<'_>,
            buf: &mut ReadBuf<'_>,
        ) -> Poll<io::Result<()>> {
            Poll::Ready(
                match unistd::read(self.fd, buf.initialize_unfilled()).map_err(io::Error::from) {
                    Err(e) => {
                        if e.kind() == io::ErrorKind::WouldBlock {
                            tracing::debug!("Child did not close its pipe");
                            Ok(())
                        } else {
                            Err(e)
                        }
                    }
                    Ok(n) => {
                        buf.advance(n);
                        Ok(())
                    }
                },
            )
        }
    }
}

#[pin_project]
struct TimeoutDrainer<R> {
    state: TimeoutDrainerState,
    // To have a generic parameter like UnixNonBlockingDrainer does.
    _phantom: PhantomData<R>,
}

enum TimeoutDrainerState {
    Waiting(BoxFuture<'static, io::Result<Vec<u8>>>),
    Draining(Cursor<Vec<u8>>),
}

impl<R> DrainerFromReader<R> for TimeoutDrainer<R>
where
    R: AsyncRead + Unpin + Send + 'static,
{
    fn from_reader(mut reader: R) -> Self {
        let fut = async move {
            let mut buff = Vec::new();
            {
                let do_read = reader.read_to_end(&mut buff);
                futures::pin_mut!(do_read);
                match tokio::time::timeout(Duration::from_secs(1), do_read).await {
                    Ok(Err(e)) => return Err(e),
                    Err(..) => {
                        tracing::debug!("Child did not close its pipe");
                    }
                    Ok(Ok(..)) => {}
                }
            }
            Ok(buff)
        };

        Self {
            state: TimeoutDrainerState::Waiting(fut.boxed()),
            _phantom: PhantomData,
        }
    }
}

impl<R> AsyncRead for TimeoutDrainer<R> {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> Poll<io::Result<()>> {
        let mut this = self.project();

        loop {
            match &mut this.state {
                TimeoutDrainerState::Waiting(fut) => {
                    let bytes = futures::ready!(Pin::new(fut).poll(cx))?;
                    *this.state = TimeoutDrainerState::Draining(Cursor::new(bytes));
                }
                TimeoutDrainerState::Draining(bytes) => {
                    let n = Read::read(bytes, buf.initialize_unfilled())?;
                    buf.advance(n);
                    break Poll::Ready(Ok(()));
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::str;
    use std::sync::Arc;
    use std::time::Instant;

    use buck2_core::process::async_background_command;
    use bytes::Bytes;
    use futures::stream::StreamExt;

    use super::*;

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

    mod interruptible_async_read {
        use std::sync::Mutex;

        use assert_matches::assert_matches;
        use gazebo::prelude::*;

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
            drainer: bool,
        }

        impl DrainerFromReader<StubAsyncRead> for StubAsyncRead {
            fn from_reader(reader: StubAsyncRead) -> Self {
                Self {
                    state: reader.state,
                    drainer: true,
                }
            }
        }

        impl StubAsyncRead {
            fn new() -> Self {
                Self {
                    state: Arc::new(Mutex::new(StubAsyncReadState::Pending)),
                    drainer: false,
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
                    StubAsyncReadState::Pending => {
                        if *this.drainer {
                            Poll::Ready(Ok(()))
                        } else {
                            Poll::Pending
                        }
                    }
                    StubAsyncReadState::Ready(b) => {
                        buf.put_slice(&[b]);
                        *state = StubAsyncReadState::Pending;
                        Poll::Ready(Ok(()))
                    }
                }
            }
        }

        #[pin_project]
        struct ReadAll<R, D> {
            #[pin]
            inner: FramedRead<InterruptibleAsyncRead<R, D>, BytesCodec>,

            buff: Vec<u8>,
        }

        impl<R, D> InterruptNotifiable for ReadAll<R, D>
        where
            D: DrainerFromReader<R>,
        {
            fn notify_interrupt(self: Pin<&mut Self>) {
                let this = self.project();
                this.inner.get_pin_mut().notify_interrupt()
            }
        }

        impl<R, D> Future for ReadAll<R, D>
        where
            R: AsyncRead + Unpin,
            D: AsyncRead + Unpin + DrainerFromReader<R>,
        {
            type Output = anyhow::Result<Vec<u8>>;

            fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
                let mut this = self.project();

                loop {
                    match futures::ready!(this.inner.as_mut().poll_next(cx)?) {
                        Some(bytes) => this.buff.extend(bytes.as_ref()),
                        None => return Poll::Ready(Ok(std::mem::take(this.buff))),
                    }
                }
            }
        }

        fn read_all<R, D>(read: InterruptibleAsyncRead<R, D>) -> ReadAll<R, D>
        where
            R: AsyncRead + Unpin,
            D: AsyncRead + Unpin + DrainerFromReader<R>,
        {
            ReadAll {
                inner: FramedRead::new(read, BytesCodec::new()),
                buff: Vec::new(),
            }
        }

        #[tokio::test]
        async fn test_drain_eof() {
            let read = StubAsyncRead::new();
            let interruptible = InterruptibleAsyncRead::<_, StubAsyncRead>::new(read.dupe());

            let drain = read_all(interruptible);
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
            let interruptible = InterruptibleAsyncRead::<_, StubAsyncRead>::new(read.dupe());

            let drain = read_all(interruptible);
            futures::pin_mut!(drain);

            // No bytes, it's pending.
            assert_matches!(futures::poll!(drain.as_mut()), Poll::Pending);

            // Add a byte, still pending, because more may come.
            read.push(b'f');
            assert_matches!(futures::poll!(drain.as_mut()), Poll::Pending);

            // Close the reader.
            drain.as_mut().notify_interrupt();

            // Mark our reader done, we expect this to be ready.
            assert_matches!(futures::poll!(drain.as_mut()), Poll::Ready(Ok(ret)) => {
                assert_eq!(&ret, "f".as_bytes());
            });
        }

        #[tokio::test]
        async fn test_drain_finish() {
            let read = StubAsyncRead::new();
            let interruptible = InterruptibleAsyncRead::<_, StubAsyncRead>::new(read.dupe());

            let drain = read_all(interruptible);
            futures::pin_mut!(drain);

            // No bytes, it's pending.
            assert_matches!(futures::poll!(drain.as_mut()), Poll::Pending);

            // Add a byte, still pending, because more may come.
            read.push(b'f');
            assert_matches!(futures::poll!(drain.as_mut()), Poll::Pending);

            // Close the reader.
            drain.as_mut().notify_interrupt();

            // But! Add more stuff.
            read.push(b'o');

            // We now expect this to read what's left.
            assert_matches!(futures::poll!(drain.as_mut()), Poll::Ready(Ok(ret)) => {
                assert_eq!(&ret, "fo".as_bytes());
            });
        }
    }

    #[tokio::test]
    async fn test_timeout_drainer() -> anyhow::Result<()> {
        // 64 bytes of a. Tokio allocates a 32 byte buffer for read_to_end so this is good to
        //    ensure we get 2 reads.
        let s = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";

        let reader = tokio_util::io::StreamReader::new(
            tokio_stream::iter(vec![io::Result::Ok(Bytes::from_static(s.as_bytes()))])
                .chain(futures::stream::pending()),
        );

        let mut drainer = TimeoutDrainer::from_reader(reader);
        let mut buff = Vec::new();
        tokio::time::timeout(Duration::from_secs(5), drainer.read_to_end(&mut buff)).await??;
        assert_eq!(s.as_bytes(), buff.as_slice());

        Ok(())
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn test_unix_non_blocking_drainer() -> anyhow::Result<()> {
        // See above.
        let s = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";

        let mut command = async_background_command("sh");
        command
            .args(&["-c", &format!("((sleep 10 && echo extra) &) && echo {}", s)])
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::null());
        let mut command = command.spawn()?;

        let stdout = command.stdout.take().unwrap();
        let mut drainer = unix_non_blocking_drainer::UnixNonBlockingDrainer::from_reader(stdout);

        command.wait().await?;

        let mut buff = String::new();
        drainer.read_to_string(&mut buff).await?;
        assert_eq!(format!("{}\n", s), buff.as_str());

        Ok(())
    }
}
