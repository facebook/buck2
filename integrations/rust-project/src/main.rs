/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

mod buck;
mod cli;
mod diagnostics;
mod json_project;
mod path;
mod progress;
mod scuba;
mod sysroot;
mod target;

use std::io;
use std::io::IsTerminal as _;
use std::path::PathBuf;
use std::str::FromStr;

use clap::ArgAction;
use clap::Parser;
use clap::Subcommand;
use serde::Deserialize;
use tracing_subscriber::filter::LevelFilter;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::EnvFilter;
use tracing_subscriber::Layer;

use crate::cli::ProjectKind;
use crate::json_project::Crate;
use crate::json_project::Dep;

#[derive(Parser, Debug, PartialEq)]
struct Opt {
    #[clap(subcommand)]
    command: Option<Command>,
    /// Print the current version.
    #[arg(short = 'V', long)]
    version: bool,
}

#[derive(Subcommand, Debug, PartialEq)]
enum Command {
    /// Create a new Rust project
    New {
        /// Name of the project being created.
        name: String,
        /// Kinds of Rust projects that can be created
        #[clap(long, value_enum, default_value = "binary")]
        kind: ProjectKind,

        /// Path to create new crate at. The new directory will be created as a
        /// subdirectory.
        path: Option<PathBuf>,
    },
    /// Convert buck's build to a format that rust-analyzer can consume.
    Develop {
        /// Buck targets to include in rust-project.json.
        #[clap(required = true, conflicts_with = "files", num_args=1..)]
        targets: Vec<String>,

        /// Path of the file being developed.
        ///
        /// Used to discover the owning set of targets.
        #[clap(required = true, last = true, num_args=1..)]
        files: Vec<PathBuf>,

        /// Where to write the generated `rust-project.json`.
        ///
        /// If not provided, rust-project will write in the current working directory.
        #[clap(short = 'o', long, value_hint = clap::ValueHint::DirPath, default_value = "rust-project.json")]
        out: PathBuf,

        /// Writes the generated `rust-project.json` to stdout.
        #[clap(long = "stdout", conflicts_with = "out")]
        stdout: bool,

        /// Use a `rustup`-managed sysroot instead of a `.buckconfig`-managed sysroot.
        ///
        /// This option requires the presence of `rustc` in the `$PATH`, as rust-project
        /// will run `rustc --print sysroot` and ignore any other `sysroot` configuration.
        #[clap(long, conflicts_with = "sysroot")]
        prefer_rustup_managed_toolchain: bool,

        /// The directory containing the Rust source code, including std.
        /// Default value is determined based on platform.
        #[clap(short = 's', long)]
        sysroot: Option<PathBuf>,

        /// Pretty-print generated `rust-project.json` file.
        #[clap(short, long)]
        pretty: bool,

        /// Check that there are no cycles in the generated crate graph.
        #[clap(long)]
        check_cycles: bool,

        /// Use paths relative to the project root in `rust-project.json`.
        #[clap(long, hide = true)]
        relative_paths: bool,

        /// The name of the client invoking rust-project, such as 'vscode'.
        #[clap(long)]
        client: Option<String>,

        /// Optional argument specifying build mode.
        #[clap(short = 'm', long)]
        mode: Option<String>,

        /// Command used to run Buck2. Defaults to `buck2`.
        #[clap(long)]
        buck2_command: Option<String>,
    },
    /// `DevelopJson` is a more limited, stripped down [`Command::Develop`].
    ///
    /// This is meant to be called by rust-analyzer directly.
    DevelopJson {
        // FIXME XXX: remove this after everything in fbcode is migrated off
        // of buckconfig implicitly.
        #[cfg(fbcode_build)]
        #[clap(long, default_value = "buckconfig")]
        sysroot_mode: SysrootMode,

        #[cfg(not(fbcode_build))]
        #[clap(long, default_value = "rustc")]
        sysroot_mode: SysrootMode,

        /// The name of the client invoking rust-project, such as 'vscode'.
        #[clap(long)]
        client: Option<String>,

        args: JsonArguments,

        /// Command used to run Buck2. Defaults to `buck2`.
        #[clap(long)]
        buck2_command: Option<String>,
    },
    /// Build the saved file's owning target. This is meant to be used by IDEs to provide diagnostics on save.
    Check {
        /// Optional argument specifying build mode.
        #[clap(short = 'm', long)]
        mode: Option<String>,

        #[clap(short = 'c', long, default_value = "true", action = ArgAction::Set)]
        use_clippy: bool,

        /// The name of the client invoking rust-project, such as 'vscode'.
        #[clap(long)]
        client: Option<String>,

        /// The file saved by the user. `rust-project` will infer the owning target(s) of the saved file and build them.
        saved_file: PathBuf,
        /// Command used to run Buck2. Defaults to `buck2`.
        #[clap(long)]
        buck2_command: Option<String>,
    },
}

/// The 'develop-json' command needs to have 3 modes:
/// 1. Static `.buckconfig` setting
/// 2. Absolute path setting
/// 3. Use `rustc --print=sysroot` ("rustup mode")
/// 4. Run a command and take the output from stdout
#[derive(PartialEq, Clone, Debug, Deserialize)]
enum SysrootMode {
    Rustc,
    Command(Vec<String>),
    FullPath(PathBuf),
    BuckConfig,
}

impl FromStr for SysrootMode {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "rustc" {
            Ok(SysrootMode::Rustc)
        } else if s == "buckconfig" {
            Ok(SysrootMode::BuckConfig)
        } else if s.starts_with("path:") {
            let s = s.trim_start_matches("path:");
            Ok(SysrootMode::FullPath(PathBuf::from(s)))
        } else if s.starts_with("cmd:") {
            let s = s.trim_start_matches("cmd:");
            Ok(SysrootMode::Command(
                s.split_whitespace()
                    .map(|s| s.to_owned())
                    .collect::<Vec<String>>(),
            ))
        } else {
            Err(anyhow::anyhow!("Invalid mode: {}", s))
        }
    }
}

#[derive(PartialEq, Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
enum JsonArguments {
    Path(PathBuf),
    Buildfile(PathBuf),
    Label(String),
}

impl FromStr for JsonArguments {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        serde_json::from_str(s).map_err(|_| {
            anyhow::anyhow!(
                "Expected a JSON object with a key of `path`, `buildfile`, or `label`. Got: {}",
                s
            )
        })
    }
}

fn main() -> Result<(), anyhow::Error> {
    #[cfg(fbcode_build)]
    {
        // SAFETY: This is as safe as using fbinit::main but with slightly less conditional compilation.
        unsafe { fbinit::perform_init() };
    }

    let opt = Opt::parse();

    let filter = EnvFilter::builder()
        .with_default_directive(LevelFilter::INFO.into())
        .from_env()?;

    if opt.version {
        println!("{}", build_info());
        return Ok(());
    }

    let Some(command) = opt.command else {
        eprintln!("Expected a subcommand, see --help for more information.");
        return Ok(());
    };

    let fmt = tracing_subscriber::fmt::layer()
        .with_ansi(io::stderr().is_terminal())
        .with_writer(io::stderr);

    match command {
        c @ Command::Develop { .. } => {
            let subscriber = tracing_subscriber::registry().with(fmt.with_filter(filter));
            tracing::subscriber::set_global_default(subscriber)?;

            let (develop, input, out) = cli::Develop::from_command(c);
            match develop.run(input.clone(), out) {
                Ok(_) => Ok(()),
                Err(e) => {
                    crate::scuba::log_develop_error(&e, input, false);
                    tracing::error!(
                        error = <anyhow::Error as AsRef<
                            dyn std::error::Error + Send + Sync + 'static,
                        >>::as_ref(&e),
                        source = e.source(),
                        kind = "error",
                    );
                    Ok(())
                }
            }
        }
        c @ Command::DevelopJson { .. } => {
            let subscriber = tracing_subscriber::registry()
                .with(progress::ProgressLayer::new(std::io::stdout).with_filter(filter));
            tracing::subscriber::set_global_default(subscriber)?;

            let (develop, input, out) = cli::Develop::from_command(c);
            match develop.run(input.clone(), out) {
                Ok(_) => Ok(()),
                Err(e) => {
                    crate::scuba::log_develop_error(&e, input, true);
                    tracing::error!(
                        error = <anyhow::Error as AsRef<
                            dyn std::error::Error + Send + Sync + 'static,
                        >>::as_ref(&e),
                        source = e.source(),
                        kind = "error",
                    );
                    Ok(())
                }
            }
        }
        Command::New { name, kind, path } => {
            let subscriber = tracing_subscriber::registry().with(fmt.with_filter(filter));
            tracing::subscriber::set_global_default(subscriber)?;

            cli::New { name, kind, path }.run()
        }
        Command::Check {
            mode,
            use_clippy,
            saved_file,
            buck2_command,
            ..
        } => {
            let subscriber = tracing_subscriber::registry().with(fmt.with_filter(filter));
            tracing::subscriber::set_global_default(subscriber)?;

            cli::Check::new(buck2_command, mode, use_clippy, saved_file.clone())
                .run()
                .inspect_err(|e| crate::scuba::log_check_error(&e, &saved_file, use_clippy))
        }
    }
}

#[cfg(not(unix))]
fn build_info() -> String {
    "No build info available.".to_owned()
}

#[cfg(unix)]
fn build_info() -> String {
    match fb_build_info_from_elf() {
        Ok(s) => s,
        Err(_) => "No build info available.".to_owned(),
    }
}

#[cfg(unix)]
fn fb_build_info_from_elf() -> Result<String, anyhow::Error> {
    let bin_path = std::env::current_exe()?;
    let bin_bytes = std::fs::read(&bin_path)?;

    let elf_file = elf::ElfBytes::<elf::endian::AnyEndian>::minimal_parse(&bin_bytes)?;
    let elf_section = elf_file
        .section_header_by_name("fb_build_info")?
        .ok_or(anyhow::anyhow!("no header"))?;

    let (section_bytes, _) = elf_file.section_data(&elf_section)?;
    let section_cstr = std::ffi::CStr::from_bytes_with_nul(section_bytes)?;

    let build_info: serde_json::Value = serde_json::from_str(&section_cstr.to_str()?)?;
    let revision = build_info["revision"].as_str().unwrap_or("(unknown)");
    let build_time = build_info["time"].as_str().unwrap_or("(unknown)");

    Ok(format!("revision: {revision}, build time: {build_time}"))
}

#[test]
fn test_parse_use_clippy() {
    assert!(matches!(
        Opt::try_parse_from([
            "rust-project",
            "check",
            "--use-clippy=true",
            "fbcode/foo.rs",
        ]),
        Ok(Opt {
            command: Some(Command::Check {
                use_clippy: true,
                ..
            }),
            ..
        })
    ));

    assert!(matches!(
        Opt::try_parse_from([
            "rust-project",
            "check",
            "--use-clippy=false",
            "fbcode/foo.rs",
        ]),
        Ok(Opt {
            command: Some(Command::Check {
                use_clippy: false,
                ..
            }),
            ..
        })
    ));
}

#[test]
#[ignore]
fn json_args_pass() {
    let args = JsonArguments::Path(PathBuf::from("buck2/integrations/rust-project/src/main.rs"));
    let expected = Opt {
        command: Some(Command::DevelopJson {
            args,
            sysroot_mode: SysrootMode::Rustc,
            client: None,
            buck2_command: None,
        }),
        version: false,
    };
    let actual = Opt::try_parse_from([
        "rust-project",
        "develop-json",
        "{\"path\":\"buck2/integrations/rust-project/src/main.rs\"}",
    ])
    .expect("Unable to parse args");
    assert_eq!(actual, expected);

    let args = JsonArguments::Label("//buck2/integrations/rust-project:rust-project".to_owned());
    let expected = Opt {
        command: Some(Command::DevelopJson {
            args,
            sysroot_mode: SysrootMode::Rustc,
            client: None,
            buck2_command: None,
        }),
        version: false,
    };
    let actual = Opt::try_parse_from([
        "rust-project",
        "develop-json",
        "{\"label\":\"//buck2/integrations/rust-project:rust-project\"}",
    ])
    .expect("Unable to parse args");
    assert_eq!(actual, expected);

    let args = JsonArguments::Buildfile(PathBuf::from("buck2/integrations/rust-project/BUCK"));
    let expected = Opt {
        command: Some(Command::DevelopJson {
            args,
            sysroot_mode: SysrootMode::Rustc,
            client: None,
            buck2_command: None,
        }),
        version: false,
    };
    let actual = Opt::try_parse_from([
        "rust-project",
        "develop-json",
        "{\"buildfile\":\"buck2/integrations/rust-project/BUCK\"}",
    ])
    .expect("Unable to parse args");
    assert_eq!(actual, expected);
}
