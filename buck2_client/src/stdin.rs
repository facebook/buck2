use std::io;
use std::io::Read;
use std::pin::Pin;
use std::sync::Arc;
use std::task::Context;
use std::task::Poll;
use std::thread::JoinHandle;

use buck2_core::env_helper::EnvHelper;
use bytes::Bytes;
use futures::stream::Fuse;
use futures::stream::StreamExt;
use gazebo::dupe::Dupe;
use parking_lot::Condvar;
use parking_lot::Mutex;
use pin_project::pin_project;
use tokio::io::AsyncRead;
use tokio::io::ReadBuf;
use tokio::sync::mpsc;
use tokio_stream::wrappers::ReceiverStream;
use tokio_util::io::StreamReader;

use crate::console_interaction_stream::ConsoleInteractionStream;

#[pin_project]
pub struct Stdin {
    #[pin]
    stream: StreamReader<Fuse<ReceiverStream<io::Result<Bytes>>>, Bytes>,

    // We don't start reading until poll_read is called.
    waiter: Option<Arc<Waiter>>,
}

impl AsyncRead for Stdin {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> Poll<io::Result<()>> {
        let this = self.project();
        if let Some(waiter) = this.waiter.take() {
            waiter.release();
        }
        this.stream.poll_read(cx, buf)
    }
}

impl Stdin {
    pub fn new() -> anyhow::Result<Self> {
        static STDIN_BUFFER_SIZE: EnvHelper<usize> = EnvHelper::new("BUCK2_TEST_STDIN_BUFFER_SIZE");
        let buffer_size = STDIN_BUFFER_SIZE.get()?.copied().unwrap_or(8192);
        let (stdin, _handle) = Self::new_for_reader(
            || {
                // NOTE: We ignore send errors since there is no point in reading without a receiver.
                let stdin = std::io::stdin().lock();

                // Disable buffering, since we don't do small reads anyway (we have a 8KB buffer
                // already). We probably need something similar on Windows here.
                #[cfg(unix)]
                let stdin = raw_reader::RawReader::new(stdin);

                stdin
            },
            buffer_size,
        );
        Ok(stdin)
    }

    fn new_for_reader<R, F>(acquire_reader: F, buffer_size: usize) -> (Self, JoinHandle<()>)
    where
        R: Read,
        F: FnOnce() -> R + Send + 'static,
    {
        let waiter = Arc::new(Waiter {
            mutex: Mutex::new(false),
            var: Condvar::new(),
        });

        // Small buffer, this isn't bytes we're buffering, just buffers of bytes. That said, sicne
        // we're on separate threads, give ourselves a bit of buffering.
        let (mut tx, rx) = mpsc::channel(4);

        let handle = std::thread::spawn({
            let waiter = waiter.dupe();
            move || {
                let reader = acquire_reader();
                waiter.block_until_released();
                let _ignored = read_and_forward(reader, &mut tx, buffer_size);
            }
        });

        (
            Self {
                stream: StreamReader::new(ReceiverStream::new(rx).fuse()),
                waiter: Some(waiter),
            },
            handle,
        )
    }

    pub fn console_interaction_stream(&mut self) -> ConsoleInteractionStream<'_> {
        ConsoleInteractionStream::new(self)
    }
}

fn read_and_forward(
    mut io: impl Read,
    tx: &mut mpsc::Sender<io::Result<Bytes>>,
    buffer_size: usize,
) -> Result<(), mpsc::error::SendError<io::Result<Bytes>>> {
    tracing::debug!("stdin: start reading");
    let mut buff = vec![0; buffer_size];

    loop {
        match io.read(&mut buff) {
            Ok(n) => {
                if n > 0 {
                    tracing::debug!("stdin: {} bytes", n);
                    tx.blocking_send(Ok(Bytes::copy_from_slice(&buff[0..n])))?;
                    continue;
                } else {
                    tracing::debug!("eof");
                    break;
                }
            }
            Err(e) => {
                tracing::debug!("err: {:#}", e);
                tx.blocking_send(Err(e))?;
                break;
            }
        }
    }

    Ok(())
}

struct Waiter {
    mutex: Mutex<bool>,
    var: Condvar,
}

impl Waiter {
    fn release(&self) {
        *self.mutex.lock() = true;
        self.var.notify_all();
    }

    fn block_until_released(&self) {
        let mut guard = self.mutex.lock();
        while !*guard {
            self.var.wait(&mut guard);
        }
    }
}

#[cfg(unix)]
mod raw_reader {
    use std::io;
    use std::io::Read;
    use std::os::unix::io::AsRawFd;
    use std::os::unix::io::RawFd;

    use nix::unistd;

    pub struct RawReader<R> {
        fd: RawFd,
        // Keep this alive for as long as we use it, on the assumption that dropping it releases
        // the FD.
        _owner: R,
    }

    impl<R> RawReader<R>
    where
        R: AsRawFd,
    {
        pub fn new(reader: R) -> Self {
            Self {
                fd: reader.as_raw_fd(),
                _owner: reader,
            }
        }
    }

    impl<R> Read for RawReader<R> {
        fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
            unistd::read(self.fd, buf).map_err(io::Error::from)
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io;
    use std::io::Read;

    use super::*;

    struct PanicReader;

    impl Read for PanicReader {
        fn read(&mut self, _buf: &mut [u8]) -> io::Result<usize> {
            panic!("!")
        }
    }

    #[test]
    fn read_waits_for_signal() {
        // If we didn't wait then we'd panic in the reader thread.
        let (_stdin, thread) = Stdin::new_for_reader(|| PanicReader, 8);
        std::thread::sleep(std::time::Duration::from_millis(100));
        assert!(!thread.is_finished());
    }
}
