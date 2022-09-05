use std::io;
use std::io::Read;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;

use bytes::Bytes;
use futures::stream::Fuse;
use futures::stream::StreamExt;
use pin_project::pin_project;
use tokio::io::AsyncRead;
use tokio::io::ReadBuf;
use tokio::sync::mpsc;
use tokio_stream::wrappers::ReceiverStream;
use tokio_util::io::StreamReader;

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
    pub fn new() -> Self {
        // Small buffer, this isn't bytes we're buffering.
        let (mut tx, rx) = mpsc::channel(4);

        std::thread::spawn(move || {
            // NOTE: We ignore send errors since there is no point in reading without a receiver.
            let stdin = std::io::stdin().lock();
            let _ignored = read_and_forward(stdin, &mut tx);
        });

        Self {
            stream: StreamReader::new(ReceiverStream::new(rx).fuse()),
        }
    }
}

fn read_and_forward(
    mut io: impl Read,
    tx: &mut mpsc::Sender<io::Result<Bytes>>,
) -> Result<(), mpsc::error::SendError<io::Result<Bytes>>> {
    let mut buff = vec![0; 8192];

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
