use tokio::io::AsyncReadExt;

use crate::stdin::Stdin;

pub struct ConsoleInteractionStream<'a> {
    stdin: &'a mut Stdin,
    term: InteractiveTerminal,
}

impl<'a> ConsoleInteractionStream<'a> {
    pub fn new(stdin: &'a mut Stdin) -> Option<Self> {
        let term = match InteractiveTerminal::enable() {
            Ok(Some(term)) => term,
            Ok(None) => {
                tracing::debug!("Not enabling interactive terminal");
                return None;
            }
            Err(e) => {
                tracing::warn!("Failed to enable interactive terminal: {:#}", e);
                return None;
            }
        };

        Some(Self { stdin, term })
    }
}

impl<'a> Drop for ConsoleInteractionStream<'a> {
    fn drop(&mut self) {
        if let Err(e) = self.term.disable() {
            tracing::warn!("Failed to disable interactive terminal: {:#}", e);
        }
    }
}

#[cfg(unix)]
mod interactive_terminal {
    use std::os::unix::io::AsRawFd;

    use anyhow::Context as _;
    use termios::*;

    pub struct InteractiveTerminal {
        orig: Termios,
    }

    impl InteractiveTerminal {
        pub fn enable() -> anyhow::Result<Option<Self>> {
            let fd = std::io::stdin().as_raw_fd();

            if !nix::unistd::isatty(fd).context("Failed to check stdin for TTY")? {
                return Ok(None);
            }

            // We also check for stderr, since if a user is starting a bunch of bucks in the
            // background those may end up clobbering the termios state and the following can
            // happen:
            //
            // - Process starts 1 buck, which sets noecho.
            // - Process starts another buck, which reads the tty state, reads noecho.
            // - The first buck exits and resets echo.
            // - The second buck exits and resets noecho (since that's what it read)
            //
            // We check stderr becasue if stderr is a TTY the user will see a bunch of consoles
            // interleaving and that would probably tell them something's wrong.
            if !nix::unistd::isatty(std::io::stderr().as_raw_fd())
                .context("Failed to check stderr for TTY")?
            {
                return Ok(None);
            }

            let orig = Termios::from_fd(fd).context("Failed to access current termios")?;

            let mut termios = orig;

            // Switch to non-canonical mode to get input immediately, and disable echo.
            termios.c_lflag &= !(ICANON | ECHO);

            // Keep blocking reads.
            termios.c_cc[VMIN] = 1;
            termios.c_cc[VTIME] = 0;

            tcsetattr(fd, TCSANOW, &termios).context("Failed to set termios")?;

            Ok(Some(Self { orig }))
        }

        pub fn disable(&mut self) -> anyhow::Result<()> {
            let fd = std::io::stdin().as_raw_fd();
            tcsetattr(fd, TCSANOW, &self.orig).context("Failed to reset termios")?;
            Ok(())
        }
    }
}

#[cfg(not(unix))]
mod interactive_terminal {
    pub struct InteractiveTerminal;

    impl InteractiveTerminal {
        pub fn enable() -> anyhow::Result<Option<Self>> {
            Ok(None)
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
            Err(e)
                if e.kind() == std::io::ErrorKind::UnexpectedEof
                    || e.kind() == std::io::ErrorKind::WouldBlock =>
            {
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
