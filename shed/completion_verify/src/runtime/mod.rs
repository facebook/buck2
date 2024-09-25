/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// @lint-ignore-every PATTERNLINT LINTIGNORE

// The code in this module is adapted from [`complete_pty`](https://crates.io/crates/completest_pty).

//! Run completions for your program
//!
//! # Example
//!
//! ```rust,no_run
//! # #[cfg(unix)] {
//! # use std::path::Path;
//! # let bin_root = Path::new("").to_owned();
//! # let completion_script = "";
//! # let home = std::env::current_dir().unwrap();
//! let term = completest_pty::Term::new();
//!
//! let mut runtime = completest_pty::BashRuntime::new(bin_root, home).unwrap();
//! runtime.register("foo", completion_script).unwrap();
//! let output = runtime.complete("foo \t\t", &term).unwrap();
//! # }
//! ```

use std::ffi::OsStr;
use std::io::Read as _;
use std::io::Write as _;
use std::path::PathBuf;
use std::process::Command;
use std::time::Duration;

use ptyprocess::PtyProcess;

use crate::Shell;

/// Terminal that shell's will run completions in
#[derive(Debug)]
pub(crate) struct Term {
    width: u16,
    height: u16,
}

impl Term {
    pub(crate) fn new() -> Self {
        Self {
            width: 120,
            height: 60,
        }
    }
}

/// Zsh runtime
#[derive(Debug)]
pub(crate) struct ZshRuntime {
    home: PathBuf,
}

impl ZshRuntime {
    /// Reuse an existing runtime's home
    pub(crate) fn with_home(home: PathBuf) -> std::io::Result<Self> {
        Ok(Self { home })
    }

    /// Register a completion script
    pub(crate) fn register(&mut self, name: &str, content: &str) -> std::io::Result<()> {
        let path = self.home.join(format!("zsh/_{name}"));
        std::fs::create_dir_all(path.parent().expect("path created with parent"))?;
        std::fs::write(path, content)
    }

    /// Get the output from typing `input` into the shell
    pub(crate) fn complete(&mut self, input: &str, term: &Term) -> std::io::Result<String> {
        let mut command = Shell::Zsh.find()?;
        command.arg("--noglobalrcs");
        command.env("TERM", "xterm").env("ZDOTDIR", &self.home);
        let echo = false;
        comptest(command, echo, input, term)
    }
}

/// Bash runtime
#[derive(Debug)]
pub(crate) struct BashRuntime {
    home: PathBuf,
    config: PathBuf,
}

impl BashRuntime {
    /// Reuse an existing runtime's home
    pub(crate) fn with_home(home: PathBuf) -> std::io::Result<Self> {
        let config_path = home.join(".bashrc");

        Ok(Self {
            home,
            config: config_path,
        })
    }

    /// Register a completion script
    pub(crate) fn register(&mut self, _name: &str, content: &str) -> std::io::Result<()> {
        let mut file = std::fs::OpenOptions::new()
            .append(true)
            .open(&self.config)?;
        writeln!(&mut file, "{content}")?;
        Ok(())
    }

    /// Get the output from typing `input` into the shell
    pub(crate) fn complete(&mut self, input: &str, term: &Term) -> std::io::Result<String> {
        let mut command = Shell::Bash.find()?;
        let inputrc_path = self.home.join(".inputrc");
        command
            .env("TERM", "xterm")
            .env("INPUTRC", &inputrc_path)
            .env("BASH_SILENCE_DEPRECATION_WARNING", "1")
            .args([
                OsStr::new("--noprofile"),
                OsStr::new("--rcfile"),
                self.config.as_os_str(),
            ]);
        let echo = !input.contains("\t\t");
        comptest(command, echo, input, term)
    }
}

/// Fish runtime
#[derive(Debug)]
pub(crate) struct FishRuntime {
    home: PathBuf,
}

impl FishRuntime {
    /// Initialize a new runtime's home
    pub(crate) fn new(home: PathBuf) -> std::io::Result<Self> {
        std::fs::create_dir_all(&home)?;

        let config_path = home.join("fish/config.fish");
        let config = "\
set -U fish_greeting \"\"
set -U fish_autosuggestion_enabled 0
function fish_title
end
function fish_prompt
    printf '%% '
end;
"
        .to_owned();
        std::fs::create_dir_all(config_path.parent().expect("path created with parent"))?;
        std::fs::write(config_path, config)?;

        Self::with_home(home)
    }

    /// Reuse an existing runtime's home
    pub(crate) fn with_home(home: PathBuf) -> std::io::Result<Self> {
        Ok(Self { home })
    }

    /// Register a completion script
    pub(crate) fn register(&mut self, name: &str, content: &str) -> std::io::Result<()> {
        let path = self.home.join(format!("fish/completions/{name}.fish"));
        std::fs::create_dir_all(path.parent().expect("path created with parent"))?;
        std::fs::write(path, content)
    }

    /// Get the output from typing `input` into the shell
    pub(crate) fn complete(&mut self, input: &str, term: &Term) -> std::io::Result<String> {
        let mut command = Shell::Fish.find()?;
        command
            // fish requires TERM to be set.
            .env("TERM", "xterm")
            .env("XDG_CONFIG_HOME", &self.home);
        let echo = false;
        comptest(command, echo, input, term)
    }
}

fn comptest(command: Command, echo: bool, input: &str, term: &Term) -> std::io::Result<String> {
    #![allow(clippy::unwrap_used)] // some unwraps need extra investigation

    // spawn a new process, pass it the input was.
    //
    // This triggers completion loading process which takes some time in shell so we should let it
    // run for some time
    let mut process = PtyProcess::spawn(command)?;
    process.set_window_size(term.width, term.height)?;
    // for some reason bash does not produce anything with echo disabled...
    process.set_echo(echo, None)?;

    let mut parser = vt100::Parser::new(term.height, term.width, 0);

    let mut stream = process.get_raw_handle()?;
    // pass the completion input
    write!(stream, "{}", input)?;
    stream.flush()?;

    let (snd, rcv) = std::sync::mpsc::channel();

    let shutdown = std::sync::atomic::AtomicBool::new(false);
    let shutdown_ref = &shutdown;
    std::thread::scope(|scope| {
        scope.spawn(move || {
            // First wait for anything to be produced. This is usually the prompt
            rcv.recv().unwrap();
            // Then, wait for a potentially extended amount of time for the next data to be
            // produced. This will only not happen if there are no completions to output
            if rcv.recv_timeout(Duration::from_millis(5000)).is_ok() {
                // Finally, wait for shorter intervals until new output stops being produced
                while rcv.recv_timeout(Duration::from_millis(1000)).is_ok() {}
            }

            shutdown_ref.store(true, std::sync::atomic::Ordering::SeqCst);
            process.exit(false).unwrap();
        });

        let mut buf = [0; 2048];
        let mut seen_prompt = false;
        while let Ok(n) = stream.read(&mut buf) {
            if shutdown.load(std::sync::atomic::Ordering::SeqCst) {
                // fish clears completions on process teardown
                break;
            }
            let buf = &buf[..n];
            if buf.is_empty() {
                break;
            }
            parser.process(buf);

            // We know that we will see at least one prompt, so we never need to consider exiting
            // before that comes through
            match seen_prompt {
                false => {
                    if buf.contains(&b'%') {
                        seen_prompt = true;
                        _ = snd.send(());
                    }
                }
                true => {
                    _ = snd.send(());
                }
            }
        }
    });

    let content = parser.screen().contents();
    Ok(content)
}
