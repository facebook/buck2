use tokio::io::AsyncReadExt;

use crate::stdin::Stdin;

pub struct ConsoleInteractionStream<'a> {
    stdin: &'a mut Stdin,
}

impl<'a> ConsoleInteractionStream<'a> {
    pub fn new(stdin: &'a mut Stdin) -> Self {
        Self { stdin }
    }
}

#[async_trait::async_trait]
pub trait ConsoleInteraction: Send + Sync {
    async fn char(&mut self) -> anyhow::Result<char>;
}

#[async_trait::async_trait]
impl<'a> ConsoleInteraction for ConsoleInteractionStream<'a> {
    async fn char(&mut self) -> anyhow::Result<char> {
        match self.stdin.read_u8().await {
            Ok(c) => Ok(c.into()),
            // NOTE: An EOF here would be reported as "unexpected" because we asked for a u8.
            Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => {
                futures::future::pending().await
            }
            Err(e) => Err(anyhow::Error::from(e).context("Error reading char from console")),
        }
    }
}

pub struct NoopConsoleInteraction;

#[async_trait::async_trait]
impl ConsoleInteraction for NoopConsoleInteraction {
    async fn char(&mut self) -> anyhow::Result<char> {
        futures::future::pending().await
    }
}
