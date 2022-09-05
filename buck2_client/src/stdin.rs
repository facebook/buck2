use std::io;
use std::io::Read;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;

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

use crate::console_interaction_stream::ConsoleInteractionStream;

#[pin_project]
pub struct Stdin {
    #[pin]
    stream: StreamReader<Fuse<ReceiverStream<io::Result<Bytes>>>, Bytes>,
}

impl AsyncRead for Stdin {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> Poll<io::Result<()>> {
        self.project().stream.poll_read(cx, buf)
    }
}

impl Stdin {
    pub fn new() -> anyhow::Result<Self> {
        static STDIN_BUFFER_SIZE: EnvHelper<usize> = EnvHelper::new("BUCK2_TEST_STDIN_BUFFER_SIZE");
        let buffer_size = STDIN_BUFFER_SIZE.get()?.copied().unwrap_or(8192);

        // Small buffer, this isn't bytes we're buffering, just buffers of bytes. That said, sicne
        // we're on separate threads, give ourselves a bit of buffering.
        let (mut tx, rx) = mpsc::channel(4);

        std::thread::spawn(move || {
            // NOTE: We ignore send errors since there is no point in reading without a receiver.
            let stdin = std::io::stdin().lock();
            let _ignored = read_and_forward(stdin, &mut tx, buffer_size);
        });

        Ok(Self {
            stream: StreamReader::new(ReceiverStream::new(rx).fuse()),
        })
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
