use tokio::io::AsyncReadExt;

use crate::stdin::Stdin;

pub struct ConsoleInteractionStream<'a> {
    stdin: &'a mut Stdin,
    term: Option<InteractiveTerminal>,
}

impl<'a> ConsoleInteractionStream<'a> {
    pub fn new(stdin: &'a mut Stdin) -> Self {
        let term = InteractiveTerminal::enable();
        if let Err(e) = term.as_ref() {
            tracing::warn!("Failed to enable interactive terminal: {:#}", e);
        }

        Self {
            stdin,
            term: term.ok(),
        }
    }
}

impl<'a> Drop for ConsoleInteractionStream<'a> {
    fn drop(&mut self) {
        if let Some(mut term) = self.term.take() {
            if let Err(e) = term.disable() {
                tracing::warn!("Failed to disable interactive terminal: {:#}", e);
            }
        }
    }
}

#[cfg(unix)]
mod interactive_terminal {
    use std::os::unix::io::AsRawFd;

    use anyhow::Context as _;
    use termios::*;

    pub struct InteractiveTerminal {
        orig: Option<Termios>,
    }

    impl InteractiveTerminal {
        pub fn enable() -> anyhow::Result<Self> {
            let fd = std::io::stdin().as_raw_fd();

            if !nix::unistd::isatty(fd).context("Failed to check for TTY")? {
                return Ok(Self { orig: None });
            }

            let orig = Termios::from_fd(fd).context("Failed to access current termios")?;

            let mut termios = orig;

            // Switch to non-canonical mode to get input immediately, and disable echo.
            termios.c_lflag &= !(ICANON | ECHO);

            // Keep blocking reads.
            termios.c_cc[VMIN] = 1;
            termios.c_cc[VTIME] = 0;

            tcsetattr(fd, TCSANOW, &termios).context("Failed to set termios")?;

            Ok(Self { orig: Some(orig) })
        }

        pub fn disable(&mut self) -> anyhow::Result<()> {
            if let Some(orig) = self.orig {
                let fd = std::io::stdin().as_raw_fd();
                tcsetattr(fd, TCSANOW, &orig).context("Failed to reset termios")?;
            }

            Ok(())
        }
    }
}

#[cfg(not(unix))]
mod interactive_terminal {
    pub struct InteractiveTerminal;

    impl InteractiveTerminal {
        pub fn enable() -> anyhow::Result<Self> {
            Ok(Self)
        }

        pub fn disable(&mut self) -> anyhow::Result<()> {
            Ok(())
        }
    }
}

use interactive_terminal::InteractiveTerminal;

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
