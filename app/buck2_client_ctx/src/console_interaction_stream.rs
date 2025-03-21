/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use strum::EnumIter;
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
    use std::io::IsTerminal;
    use std::os::unix::io::AsRawFd;

    use buck2_error::BuckErrorContext;
    use termios::*;

    pub struct InteractiveTerminal {
        orig: Termios,
    }

    impl InteractiveTerminal {
        pub fn enable() -> buck2_error::Result<Option<Self>> {
            let fd = std::io::stdin().as_raw_fd();

            if !std::io::stdin().is_terminal() {
                return Ok(None);
            }

            // If stdout is redirected, let's also no turn this on since it might be redirected to
            // something that wants stdout. We can always debug those with `debug replay` now.
            if !std::io::stdout().is_terminal() {
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
            // We check stderr because if stderr is a TTY the user will see a bunch of consoles
            // interleaving and that would probably tell them something's wrong.
            if !std::io::stderr().is_terminal() {
                return Ok(None);
            }

            let orig =
                Termios::from_fd(fd).buck_error_context("Failed to access current termios")?;

            let mut termios = orig;

            // Switch to non-canonical mode to get input immediately, and disable echo.
            termios.c_lflag &= !(ICANON | ECHO);

            // Keep blocking reads.
            termios.c_cc[VMIN] = 1;
            termios.c_cc[VTIME] = 0;

            tcsetattr(fd, TCSANOW, &termios).buck_error_context("Failed to set termios")?;

            Ok(Some(Self { orig }))
        }

        pub fn disable(&mut self) -> buck2_error::Result<()> {
            let fd = std::io::stdin().as_raw_fd();
            tcsetattr(fd, TCSANOW, &self.orig).buck_error_context("Failed to reset termios")?;
            Ok(())
        }
    }
}

#[cfg(windows)]
mod interactive_terminal {
    use std::io::IsTerminal;
    use std::os::windows::io::AsRawHandle;

    use buck2_error::BuckErrorContext;
    use winapi::shared::minwindef::DWORD;
    use winapi::um::consoleapi::GetConsoleMode;
    use winapi::um::consoleapi::SetConsoleMode;
    use winapi::um::wincon::ENABLE_ECHO_INPUT;
    use winapi::um::wincon::ENABLE_LINE_INPUT;
    use winapi::um::winnt::HANDLE;

    fn get_console_mode(handle: HANDLE) -> buck2_error::Result<DWORD> {
        let mut mode: DWORD = 0;
        if unsafe { GetConsoleMode(handle, &mut mode) } != 0 {
            Ok(mode)
        } else {
            Err(std::io::Error::last_os_error()).buck_error_context("Failed to get console mode")
        }
    }

    fn set_console_mode(handle: HANDLE, mode: DWORD) -> buck2_error::Result<()> {
        if unsafe { SetConsoleMode(handle, mode) != 0 } {
            Ok(())
        } else {
            Err(std::io::Error::last_os_error()).buck_error_context("Failed to set console mode")
        }
    }

    pub struct InteractiveTerminal {
        mode: DWORD,
    }

    impl InteractiveTerminal {
        pub fn enable() -> buck2_error::Result<Option<Self>> {
            let handle = std::io::stdin().as_raw_handle();

            if !std::io::stdin().is_terminal()
                || !std::io::stdout().is_terminal()
                || !std::io::stderr().is_terminal()
            {
                return Ok(None);
            }

            let mode = get_console_mode(handle)?;
            // Switch to non-canonical mode to get input immediately, and disable echo.
            set_console_mode(handle, mode & !(ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT))?;
            Ok(Some(Self { mode }))
        }

        pub fn disable(&mut self) -> buck2_error::Result<()> {
            let handle = std::io::stdin().as_raw_handle();
            set_console_mode(handle, self.mode)?;
            Ok(())
        }
    }
}

#[cfg(not(any(unix, windows)))]
mod interactive_terminal {
    pub struct InteractiveTerminal;

    impl InteractiveTerminal {
        pub fn enable() -> buck2_error::Result<Option<Self>> {
            Ok(None)
        }

        pub fn disable(&mut self) -> buck2_error::Result<()> {
            Ok(())
        }
    }
}

use interactive_terminal::InteractiveTerminal;

#[derive(Debug, EnumIter)]
pub enum SuperConsoleToggle {
    Dice,
    DebugEvents,
    TwoLinesMode,
    DetailedRE,
    Io,
    TargetConfigurations,
    ExpandedProgress,
    Commands,
    IncrLines,
    DecrLines,
    Help,
}

impl SuperConsoleToggle {
    pub fn description(&self) -> &str {
        match self {
            SuperConsoleToggle::Dice => "DICE",
            SuperConsoleToggle::DebugEvents => "debug events",
            SuperConsoleToggle::TwoLinesMode => "two lines mode",
            SuperConsoleToggle::DetailedRE => "detailed RE",
            SuperConsoleToggle::Io => "I/O counters",
            SuperConsoleToggle::TargetConfigurations => "target configurations",
            SuperConsoleToggle::ExpandedProgress => "expanded progress",
            SuperConsoleToggle::Commands => "commands",
            SuperConsoleToggle::IncrLines => "more lines",
            SuperConsoleToggle::DecrLines => "less lines",
            SuperConsoleToggle::Help => "help",
        }
    }

    pub fn key(&self) -> char {
        match self {
            SuperConsoleToggle::Dice => 'd',
            SuperConsoleToggle::DebugEvents => 'e',
            SuperConsoleToggle::TwoLinesMode => '2',
            SuperConsoleToggle::DetailedRE => 'r',
            SuperConsoleToggle::Io => 'i',
            SuperConsoleToggle::TargetConfigurations => 'p',
            SuperConsoleToggle::ExpandedProgress => 'x',
            SuperConsoleToggle::Commands => 'c',
            SuperConsoleToggle::IncrLines => '+',
            SuperConsoleToggle::DecrLines => '-',
            SuperConsoleToggle::Help => '?',
        }
    }
}

#[async_trait::async_trait]
pub trait SuperConsoleInteraction: Send + Sync {
    async fn toggle(&mut self) -> buck2_error::Result<Option<SuperConsoleToggle>>;
}

#[async_trait::async_trait]
impl<'a> SuperConsoleInteraction for ConsoleInteractionStream<'a> {
    async fn toggle(&mut self) -> buck2_error::Result<Option<SuperConsoleToggle>> {
        match self.stdin.read_u8().await {
            Ok(c) => {
                let c: char = c.into();
                let console_toggle = match c {
                    'd' => Some(SuperConsoleToggle::Dice),
                    'e' => Some(SuperConsoleToggle::DebugEvents),
                    '2' => Some(SuperConsoleToggle::TwoLinesMode),
                    'r' => Some(SuperConsoleToggle::DetailedRE),
                    'i' => Some(SuperConsoleToggle::Io),
                    'p' => Some(SuperConsoleToggle::TargetConfigurations),
                    'x' => Some(SuperConsoleToggle::ExpandedProgress),
                    'c' => Some(SuperConsoleToggle::Commands),
                    '+' => Some(SuperConsoleToggle::IncrLines),
                    '-' => Some(SuperConsoleToggle::DecrLines),
                    '?' | 'h' => Some(SuperConsoleToggle::Help),
                    _ => None,
                };
                Ok(console_toggle)
            }
            // NOTE: An EOF here would be reported as "unexpected" because we asked for a u8.
            Err(e)
                if e.kind() == std::io::ErrorKind::UnexpectedEof
                    || e.kind() == std::io::ErrorKind::WouldBlock =>
            {
                futures::future::pending().await
            }
            Err(e) => Err(buck2_error::Error::from(e).context("Error reading char from console")),
        }
    }
}

pub struct NoopSuperConsoleInteraction;

#[async_trait::async_trait]
impl SuperConsoleInteraction for NoopSuperConsoleInteraction {
    async fn toggle(&mut self) -> buck2_error::Result<Option<SuperConsoleToggle>> {
        futures::future::pending().await
    }
}
