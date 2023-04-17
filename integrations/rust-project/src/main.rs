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
mod json_project;
mod target;

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
        #[arg(required = true, conflicts_with = "files", num_args = 1..)]
        targets: Vec<String>,

        /// Path of the file being developed.
        ///
        /// Used to discover the owning set of targets.
        #[arg(required = true, last = true, num_args = 1..)]
        files: Vec<PathBuf>,

        /// Where to write the generated `rust-project.json`.
        ///
        /// If not provided, rust-project will write in the current working directory.
        #[clap(short = 'o', long, value_hint = clap::ValueHint::DirPath, default_value = "rust-project.json")]
        out: PathBuf,

        /// Writes the generated `rust-project.json` to stdout.
        #[clap(long = "stdout", conflicts_with = "out")]
        stdout: bool,

        /// The directory containing the Rust source code, including std.
        /// Default value is determined based on platform.
        #[clap(short = 's', long)]
        sysroot: Option<PathBuf>,

        /// Pretty-print generated `rust-project.json` file.
        #[clap(short, long)]
        pretty: bool,
    },
}

fn main() -> anyhow::Result<()> {
    let filter = EnvFilter::builder()
        .with_default_directive(LevelFilter::INFO.into())
        .from_env()?;
    let subscriber = tracing_subscriber::registry()
        .with(
            tracing_subscriber::fmt::layer()
                .with_ansi(atty::is(atty::Stream::Stderr))
                .with_writer(std::io::stderr),
        )
        .with(filter);
    tracing::subscriber::set_global_default(subscriber)?;

    let cli = Opt::parse();

    match cli.command {
        Command::New { name, kind, path } => cli::New { name, kind, path }.run(),
        c @ Command::Develop { .. } => cli::Develop::from(c).run(),
    }
}
