#![allow(clippy::significant_drop_in_scrutinee)] // FIXME?

use std::io::Stdin;
use std::io::StdinLock;
use std::pin::Pin;
use std::sync::Mutex;
use std::task::Context;
use std::task::Poll;
use std::time::Duration;

use futures::Stream;
use futures::StreamExt;
use tokio::sync::oneshot::error::TryRecvError;

/// Read data from stdin when it's ready, attempt to deserialize it, and output the messages as a [`futures::Stream`]
///
/// Note that this should be the only way that stdin is read, as it locks stdin.
/// It also runs a background thread to read asynchronously.
///
/// Dropping this struct tells the stdin reading thread to stop reading, and cleans the thread up.
pub(crate) struct StdinStream<T: Send + Sync> {
    messages: tokio_stream::wrappers::UnboundedReceiverStream<T>,
    stdio_thread: Option<std::thread::JoinHandle<()>>,
    stop_signal: Option<Mutex<tokio::sync::oneshot::Sender<()>>>,
}

/// Whether or not stdin is ready to read
#[cfg(not(windows))]
fn is_ready(stdin: &Stdin) -> bool {
    use std::os::unix::io::AsRawFd;

    use nix::poll::PollFd;
    use nix::poll::PollFlags;

    let fds = &mut [PollFd::new(stdin.as_raw_fd(), PollFlags::POLLIN)];
    match nix::poll::poll(fds, 1000) {
        Ok(1) => true,
        _ => false,
    }
}

#[cfg(windows)]
fn is_ready(_stdin: &Stdin) -> bool {
    // TODO(nmj): Properly implement this with nonblocking stdin
    true
}

impl<T: Send + Sync + 'static> StdinStream<T> {
    pub(crate) fn new<F>(deserializer: F) -> Self
    where
        F: Fn(&mut StdinLock) -> anyhow::Result<Option<T>> + Send + Sync + 'static,
    {
        let (sender, receiver) = tokio::sync::mpsc::unbounded_channel();
        let (stop_sender, mut stop_receiver) = tokio::sync::oneshot::channel();
        let stdio_thread = std::thread::spawn(move || {
            let raw_stdin = std::io::stdin();
            let mut stdin = raw_stdin.lock();

            loop {
                // Check for shutdown on every loop, otherwise we might enter a state
                // where we're waiting for input, even if we've shut down.
                match stop_receiver.try_recv() {
                    Err(TryRecvError::Empty) => {}
                    _ => break,
                }
                if is_ready(&raw_stdin) {
                    match deserializer(&mut stdin) {
                        Ok(Some(msg)) => {
                            let _ignore = sender.send(msg);
                        }
                        Ok(None) => {
                            // Didn't get a message, so wait just a moment to see if we get one.
                            std::thread::sleep(Duration::from_millis(100));
                        }
                        Err(e) => {
                            let _ignore =
                                crate::eprintln!("Could not read message from stdin: `{}`", e);
                            break;
                        }
                    }
                }
            }
        });
        Self {
            messages: tokio_stream::wrappers::UnboundedReceiverStream::new(receiver),
            stdio_thread: Some(stdio_thread),
            stop_signal: Some(Mutex::new(stop_sender)),
        }
    }
}

impl<T: Send + Sync> Stream for StdinStream<T> {
    type Item = T;

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        self.messages.poll_next_unpin(cx)
    }
}

impl<T: Send + Sync> Drop for StdinStream<T> {
    fn drop(&mut self) {
        if let Some(stop_signal) = self.stop_signal.take() {
            let _ignored = stop_signal.into_inner().unwrap().send(());
        }
        if let Some(handle) = self.stdio_thread.take() {
            handle.join().unwrap_or(());
        }
    }
}
