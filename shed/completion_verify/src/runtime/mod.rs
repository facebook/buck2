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

#![cfg_attr(docsrs, feature(doc_auto_cfg))]
#![warn(missing_docs)]
#![warn(clippy::print_stderr)]
#![warn(clippy::print_stdout)]
#![cfg(unix)]

use std::ffi::OsStr;
use std::ffi::OsString;
use std::io::Read as _;
use std::io::Write as _;
use std::path::PathBuf;
use std::process::Command;
use std::time::Duration;

use ptyprocess::PtyProcess;

pub use completest::Runtime;
pub use completest::RuntimeBuilder;
pub use completest::Term;

/// Abstract factory for [`ZshRuntime`]
#[derive(Debug)]
#[non_exhaustive]
pub struct ZshRuntimeBuilder {}

impl RuntimeBuilder for ZshRuntimeBuilder {
    type Runtime = ZshRuntime;

    fn name() -> &'static str {
        "zsh"
    }

    fn new(bin_root: PathBuf, home: PathBuf) -> std::io::Result<Self::Runtime> {
        ZshRuntime::new(bin_root, home)
    }

    fn with_home(bin_root: PathBuf, home: PathBuf) -> std::io::Result<Self::Runtime> {
        ZshRuntime::with_home(bin_root, home)
    }
}

/// Zsh runtime
#[derive(Debug)]
#[cfg(unix)] // purely for rustdoc to pick it up
pub struct ZshRuntime {
    path: OsString,
    home: PathBuf,
    timeout: Duration,
}

impl ZshRuntime {
    /// Initialize a new runtime's home
    pub fn new(bin_root: PathBuf, home: PathBuf) -> std::io::Result<Self> {
        std::fs::create_dir_all(&home)?;

        let config_path = home.join(".zshenv");
        let config = "\
fpath=($fpath $ZDOTDIR/zsh)
autoload -U +X compinit && compinit -u # bypass compaudit security checking
precmd_functions=\"\"  # avoid the prompt being overwritten
PS1='%% '
PROMPT='%% '
";
        std::fs::write(config_path, config)?;

        let _ = std::fs::remove_file(home.join(".zcompdump"));

        Self::with_home(bin_root, home)
    }

    /// Reuse an existing runtime's home
    pub fn with_home(bin_root: PathBuf, home: PathBuf) -> std::io::Result<Self> {
        let path = build_path(bin_root);

        Ok(Self {
            path,
            home,
            timeout: Duration::from_millis(100),
        })
    }

    /// Location of the runtime's home directory
    pub fn home(&self) -> &std::path::Path {
        &self.home
    }

    /// Register a completion script
    pub fn register(&mut self, name: &str, content: &str) -> std::io::Result<()> {
        let path = self.home.join(format!("zsh/_{name}"));
        std::fs::create_dir_all(path.parent().expect("path created with parent"))?;
        std::fs::write(path, content)
    }

    /// Get the output from typing `input` into the shell
    pub fn complete(&mut self, input: &str, term: &Term) -> std::io::Result<String> {
        let mut command = Command::new("zsh");
        command.arg("--noglobalrcs");
        command
            .env("PATH", &self.path)
            .env("TERM", "xterm")
            .env("ZDOTDIR", &self.home);
        let echo = false;
        comptest(command, echo, input, term, self.timeout)
    }
}

impl Runtime for ZshRuntime {
    fn home(&self) -> &std::path::Path {
        self.home()
    }

    fn register(&mut self, name: &str, content: &str) -> std::io::Result<()> {
        self.register(name, content)
    }

    fn complete(&mut self, input: &str, term: &Term) -> std::io::Result<String> {
        self.complete(input, term)
    }
}

/// Abstract factory for [`BashRuntime`]
#[derive(Debug)]
#[non_exhaustive]
pub struct BashRuntimeBuilder {}

impl RuntimeBuilder for BashRuntimeBuilder {
    type Runtime = BashRuntime;

    fn name() -> &'static str {
        "bash"
    }

    fn new(bin_root: PathBuf, home: PathBuf) -> std::io::Result<Self::Runtime> {
        BashRuntime::new(bin_root, home)
    }

    fn with_home(bin_root: PathBuf, home: PathBuf) -> std::io::Result<Self::Runtime> {
        BashRuntime::with_home(bin_root, home)
    }
}

/// Bash runtime
#[derive(Debug)]
#[cfg(unix)] // purely for rustdoc to pick it up
pub struct BashRuntime {
    path: OsString,
    home: PathBuf,
    config: PathBuf,
    timeout: Duration,
}

impl BashRuntime {
    /// Initialize a new runtime's home
    pub fn new(bin_root: PathBuf, home: PathBuf) -> std::io::Result<Self> {
        std::fs::create_dir_all(&home)?;

        let config_path = home.join(".bashrc");
        let inputrc_path = home.join(".inputrc");
        let config = "\
PS1='% '
. /etc/bash_completion
"
        .to_owned();
        std::fs::write(config_path, config)?;
        // Ignore ~/.inputrc which may set vi edit mode.
        std::fs::write(
            inputrc_path,
            "# expected empty file to disable loading ~/.inputrc\n",
        )?;

        Self::with_home(bin_root, home)
    }

    /// Reuse an existing runtime's home
    pub fn with_home(bin_root: PathBuf, home: PathBuf) -> std::io::Result<Self> {
        let config_path = home.join(".bashrc");
        let path = build_path(bin_root);

        Ok(Self {
            path,
            home,
            config: config_path,
            timeout: Duration::from_millis(50),
        })
    }

    /// Location of the runtime's home directory
    pub fn home(&self) -> &std::path::Path {
        &self.home
    }

    /// Register a completion script
    pub fn register(&mut self, _name: &str, content: &str) -> std::io::Result<()> {
        let mut file = std::fs::OpenOptions::new()
            .append(true)
            .open(&self.config)?;
        writeln!(&mut file, "{content}")?;
        Ok(())
    }

    /// Get the output from typing `input` into the shell
    pub fn complete(&mut self, input: &str, term: &Term) -> std::io::Result<String> {
        let mut command = Command::new("bash");
        let inputrc_path = self.home.join(".inputrc");
        command
            .env("PATH", &self.path)
            .env("TERM", "xterm")
            .env("INPUTRC", &inputrc_path)
            .args([
                OsStr::new("--noprofile"),
                OsStr::new("--rcfile"),
                self.config.as_os_str(),
            ]);
        let echo = !input.contains("\t\t");
        comptest(command, echo, input, term, self.timeout)
    }
}

impl Runtime for BashRuntime {
    fn home(&self) -> &std::path::Path {
        self.home()
    }

    fn register(&mut self, name: &str, content: &str) -> std::io::Result<()> {
        self.register(name, content)
    }

    fn complete(&mut self, input: &str, term: &Term) -> std::io::Result<String> {
        self.complete(input, term)
    }
}

/// Abstract factory for [`FishRuntime`]
#[derive(Debug)]
#[non_exhaustive]
pub struct FishRuntimeBuilder {}

impl RuntimeBuilder for FishRuntimeBuilder {
    type Runtime = FishRuntime;

    fn name() -> &'static str {
        "fish"
    }

    fn new(bin_root: PathBuf, home: PathBuf) -> std::io::Result<Self::Runtime> {
        FishRuntime::new(bin_root, home)
    }

    fn with_home(bin_root: PathBuf, home: PathBuf) -> std::io::Result<Self::Runtime> {
        FishRuntime::with_home(bin_root, home)
    }
}

/// Fish runtime
#[derive(Debug)]
#[cfg(unix)] // purely for rustdoc to pick it up
pub struct FishRuntime {
    path: OsString,
    home: PathBuf,
    timeout: Duration,
}

impl FishRuntime {
    /// Initialize a new runtime's home
    pub fn new(bin_root: PathBuf, home: PathBuf) -> std::io::Result<Self> {
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

        Self::with_home(bin_root, home)
    }

    /// Reuse an existing runtime's home
    pub fn with_home(bin_root: PathBuf, home: PathBuf) -> std::io::Result<Self> {
        let path = build_path(bin_root);

        Ok(Self {
            path,
            home,
            timeout: Duration::from_millis(50),
        })
    }

    /// Location of the runtime's home directory
    pub fn home(&self) -> &std::path::Path {
        &self.home
    }

    /// Register a completion script
    pub fn register(&mut self, name: &str, content: &str) -> std::io::Result<()> {
        let path = self.home.join(format!("fish/completions/{name}.fish"));
        std::fs::create_dir_all(path.parent().expect("path created with parent"))?;
        std::fs::write(path, content)
    }

    /// Get the output from typing `input` into the shell
    pub fn complete(&mut self, input: &str, term: &Term) -> std::io::Result<String> {
        let mut command = Command::new("fish");
        command
            .env("PATH", &self.path)
            // fish requires TERM to be set.
            .env("TERM", "xterm")
            .env("XDG_CONFIG_HOME", &self.home);
        let echo = false;
        comptest(command, echo, input, term, self.timeout)
    }
}

impl Runtime for FishRuntime {
    fn home(&self) -> &std::path::Path {
        self.home()
    }

    fn register(&mut self, name: &str, content: &str) -> std::io::Result<()> {
        self.register(name, content)
    }

    fn complete(&mut self, input: &str, term: &Term) -> std::io::Result<String> {
        self.complete(input, term)
    }
}

/// Abstract factory for [`ElvishRuntime`]
#[derive(Debug)]
#[non_exhaustive]
pub struct ElvishRuntimeBuilder {}

impl RuntimeBuilder for ElvishRuntimeBuilder {
    type Runtime = ElvishRuntime;

    fn name() -> &'static str {
        "elvish"
    }

    fn new(bin_root: PathBuf, home: PathBuf) -> std::io::Result<Self::Runtime> {
        ElvishRuntime::new(bin_root, home)
    }

    fn with_home(bin_root: PathBuf, home: PathBuf) -> std::io::Result<Self::Runtime> {
        ElvishRuntime::with_home(bin_root, home)
    }
}

/// Elvish runtime
#[derive(Debug)]
#[cfg(unix)] // purely for rustdoc to pick it up
pub struct ElvishRuntime {
    path: OsString,
    home: PathBuf,
    config: PathBuf,
    timeout: Duration,
}

impl ElvishRuntime {
    /// Initialize a new runtime's home
    pub fn new(bin_root: PathBuf, home: PathBuf) -> std::io::Result<Self> {
        std::fs::create_dir_all(&home)?;

        let config_path = home.join("elvish/rc.elv");
        let config = "\
set edit:rprompt = (constantly \"\")
set edit:prompt = (constantly \"% \")
"
        .to_owned();
        std::fs::create_dir_all(config_path.parent().expect("path created with parent"))?;
        std::fs::write(config_path, config)?;

        Self::with_home(bin_root, home)
    }

    /// Reuse an existing runtime's home
    pub fn with_home(bin_root: PathBuf, home: PathBuf) -> std::io::Result<Self> {
        let config_path = home.join("elvish/rc.elv");
        let path = build_path(bin_root);

        Ok(Self {
            path,
            home,
            config: config_path,
            timeout: Duration::from_millis(50),
        })
    }

    /// Location of the runtime's home directory
    pub fn home(&self) -> &std::path::Path {
        &self.home
    }

    /// Register a completion script
    pub fn register(&mut self, _name: &str, content: &str) -> std::io::Result<()> {
        let mut file = std::fs::OpenOptions::new()
            .append(true)
            .open(&self.config)?;
        writeln!(&mut file, "{content}")?;
        Ok(())
    }

    /// Get the output from typing `input` into the shell
    pub fn complete(&mut self, input: &str, term: &Term) -> std::io::Result<String> {
        let mut command = Command::new("elvish");
        command
            .env("PATH", &self.path)
            .env("XDG_CONFIG_HOME", &self.home);
        let echo = false;
        comptest(command, echo, input, term, self.timeout)
    }
}

impl Runtime for ElvishRuntime {
    fn home(&self) -> &std::path::Path {
        self.home()
    }

    fn register(&mut self, name: &str, content: &str) -> std::io::Result<()> {
        self.register(name, content)
    }

    fn complete(&mut self, input: &str, term: &Term) -> std::io::Result<String> {
        self.complete(input, term)
    }
}

fn comptest(
    command: Command,
    echo: bool,
    input: &str,
    term: &Term,
    timeout: Duration,
) -> std::io::Result<String> {
    #![allow(clippy::unwrap_used)] // some unwraps need extra investigation

    // spawn a new process, pass it the input was.
    //
    // This triggers completion loading process which takes some time in shell so we should let it
    // run for some time
    let mut process = PtyProcess::spawn(command)?;
    process.set_window_size(term.get_width(), term.get_height())?;
    // for some reason bash does not produce anything with echo disabled...
    process.set_echo(echo, None)?;

    let mut parser = vt100::Parser::new(term.get_height(), term.get_width(), 0);

    let mut stream = process.get_raw_handle()?;
    // pass the completion input
    write!(stream, "{}", input)?;
    stream.flush()?;

    let (snd, rcv) = std::sync::mpsc::channel();

    let shutdown = std::sync::atomic::AtomicBool::new(false);
    let shutdown_ref = &shutdown;
    std::thread::scope(|scope| {
        scope.spawn(move || {
            // since we don't know when exactly shell is done completing the idea is to wait until
            // something at all is produced, then wait for some duration since the last produced chunk.
            rcv.recv().unwrap();
            while rcv.recv_timeout(timeout).is_ok() {}
            shutdown_ref.store(true, std::sync::atomic::Ordering::SeqCst);
            process.exit(false).unwrap();
        });

        let mut buf = [0; 2048];
        while let Ok(n) = stream.read(&mut buf) {
            if shutdown.load(std::sync::atomic::Ordering::SeqCst) {
                // fish clears completions on process teardown
                break;
            }
            let buf = &buf[..n];
            if buf.is_empty() {
                break;
            }
            let _ = snd.send(());
            parser.process(buf);
        }
    });

    let content = parser.screen().contents();
    Ok(content)
}

fn build_path(bin_root: PathBuf) -> OsString {
    let mut path = bin_root.into_os_string();
    if let Some(existing) = std::env::var_os("PATH") {
        path.push(":");
        path.push(existing);
    }
    path
}

