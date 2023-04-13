/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io;
use std::io::Read;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;
use std::thread::JoinHandle;

use buck2_core::env_helper::EnvHelper;
use bytes::Bytes;
use futures::stream::Fuse;
use futures::stream::StreamExt;
use pin_project::pin_project;
use tokio::io::AsyncRead;
use tokio::io::ReadBuf;
use tokio::sync::mpsc;
use tokio_stream::wrappers::ReceiverStream;
use tokio_util::io::StreamReader;

use crate::common::CommonConsoleOptions;
use crate::console_interaction_stream::ConsoleInteractionStream;

#[pin_project]
pub struct Stdin {
    #[pin]
    stream: StreamReader<Fuse<ReceiverStream<io::Result<Bytes>>>, Bytes>,

    // We don't start reading until poll_read is called.
    state: State,
}

impl AsyncRead for Stdin {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> Poll<io::Result<()>> {
        let this = self.project();
        this.state.ensure_started();
        this.stream.poll_read(cx, buf)
    }
}

impl Stdin {
    pub fn new() -> anyhow::Result<Self> {
        static STDIN_BUFFER_SIZE: EnvHelper<usize> = EnvHelper::new("BUCK2_TEST_STDIN_BUFFER_SIZE");
        let buffer_size = STDIN_BUFFER_SIZE.get()?.copied().unwrap_or(8192);

        // Small buffer, this isn't bytes we're buffering, just buffers of bytes. That said, since
        // we're on separate threads, give ourselves a bit of buffering.
        let (tx, rx) = mpsc::channel(4);

        Ok(Self {
            stream: StreamReader::new(ReceiverStream::new(rx).fuse()),
            state: State::Pending { buffer_size, tx },
        })
    }

    pub fn console_interaction_stream(
        &mut self,
        opts: &CommonConsoleOptions,
    ) -> Option<ConsoleInteractionStream<'_>> {
        if opts.no_interactive_console {
            tracing::debug!("Disabling console interaction: no_interactive_console is set");
            return None;
        }

        ConsoleInteractionStream::new(self)
    }
}

enum State {
    Pending {
        buffer_size: usize,
        tx: mpsc::Sender<io::Result<Bytes>>,
    },
    Started(JoinHandle<()>),
}

impl State {
    fn ensure_started(&mut self) {
        if !matches!(self, Self::Pending { .. }) {
            return;
        }

        take_mut::take(self, |this| {
            match this {
                Self::Pending {
                    buffer_size,
                    mut tx,
                } => {
                    let handle = std::thread::spawn({
                        move || {
                            #[allow(clippy::let_and_return)]
                            let stdin = std::io::stdin().lock();

                            // Disable buffering, since we don't do small reads anyway (we have a 8KB buffer
                            // already). We probably need something similar on Windows here.
                            #[cfg(unix)]
                            let stdin = raw_reader::RawReader::new(stdin);

                            // NOTE: We ignore send errors since there is no point in reading without a receiver.
                            let _ignored = read_and_forward(stdin, &mut tx, buffer_size);
                        }
                    });

                    Self::Started(handle)
                }
                v => v,
            }
        });
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
