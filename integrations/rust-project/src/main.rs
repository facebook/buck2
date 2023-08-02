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
mod sysroot;
mod target;

use std::io;
use std::io::IsTerminal as _;
use std::path::PathBuf;

use clap::Parser;
use clap::Subcommand;
use tracing_subscriber::filter::LevelFilter;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::EnvFilter;

use crate::cli::ProjectKind;
use crate::json_project::Crate;
use crate::json_project::Dep;

#[derive(Parser, Debug, PartialEq)]
struct Opt {
    #[clap(subcommand)]
    command: Command,
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
        #[clap(required = true, conflicts_with = "files", multiple_values = true)]
        targets: Vec<String>,

        /// Path of the file being developed.
        ///
        /// Used to discover the owning set of targets.
        #[clap(required = true, last = true, multiple_values = true)]
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

        /// Use paths relative to the project root in `rust-project.json`.
        #[clap(long, hide = true)]
        relative_paths: bool,

        /// Optional argument specifying build mode.
        #[clap(short = 'm', long)]
        mode: Option<String>,
    },
    /// Build the saved file's owning target. This is meant to be used by IDEs to provide diagnostics on save.
    Check {
        /// Optional argument specifying build mode.
        #[clap(short = 'm', long)]
        mode: Option<String>,
        #[clap(short = 'c', long, default_value = "true")]
        use_clippy: bool,
        /// The file saved by the user. `rust-project` will infer the owning target(s) of the saved file and build them.
        saved_file: PathBuf,
    },
}

fn main() -> anyhow::Result<()> {
    let filter = EnvFilter::builder()
        .with_default_directive(LevelFilter::INFO.into())
        .from_env()?;
    let subscriber = tracing_subscriber::registry()
        .with(
            tracing_subscriber::fmt::layer()
                .with_ansi(io::stderr().is_terminal())
                .with_writer(io::stderr),
        )
        .with(filter);
    tracing::subscriber::set_global_default(subscriber)?;

    let cli = Opt::parse();

    match cli.command {
        Command::New { name, kind, path } => cli::New { name, kind, path }.run(),
        Command::Check {
            mode,
            use_clippy,
            saved_file,
        } => cli::Check::new(mode, use_clippy, saved_file).run(),
        c @ Command::Develop { .. } => cli::Develop::from(c).run(),
    }
}
