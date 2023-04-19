/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fs;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;

use anyhow::Context;
use clap::ValueEnum;
use tracing::info;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum ProjectKind {
    Binary,
    Library,
}

pub struct New {
    pub name: String,
    pub kind: ProjectKind,
    pub path: Option<PathBuf>,
}

impl New {
    pub fn run(self) -> Result<(), anyhow::Error> {
        let name = self.name;

        let (target, kind) = match self.kind {
            ProjectKind::Library => {
                let target = Target::Library { name: name.clone() };
                let lib = EntryFile::Lib(LibFile);
                (target, lib)
            }
            ProjectKind::Binary => {
                let target = Target::Binary { name: name.clone() };
                let main = EntryFile::Main(MainFile);

                (target, main)
            }
        };

        let path = match self.path {
            Some(path) => path,
            None => std::env::current_dir()?,
        };
        let path = path
            .canonicalize()
            .context("Unable to canonicalize current directory")?;

        let project_dir = Path::new(&name);
        let path = path.join(project_dir);

        fs::create_dir(&path).context("Unable to create project directory")?;

        info!(?path);

        // create the `TARGETS` file
        let targets_path = path.join("TARGETS");
        let mut targets_file =
            fs::File::create(targets_path).context("Unable to create `TARGETS` file")?;
        targets_file
            .write_all(target.render().as_bytes())
            .context("Unable to write generated template to `TARGETS` file")?;

        // create src dir
        let src_dur = path.join(Path::new("src"));
        fs::create_dir(&src_dur).context("Unable to create `src/` directory")?;

        match kind {
            EntryFile::Main(main) => {
                let path = src_dur.join(Path::new("main.rs"));
                let mut main_file = fs::File::create(path).context("Unable to create main file")?;

                main_file
                    .write_all(main.render().as_bytes())
                    .context("Unable to write contents of main.rs")?;
            }
            EntryFile::Lib(lib) => {
                let path = src_dur.join(Path::new("lib.rs"));
                let mut lib_file = fs::File::create(path).context("Unable to create main file")?;

                lib_file
                    .write_all(lib.render().as_bytes())
                    .context("Unable to write contents of lib.rs")?;
            }
        }

        Ok(())
    }
}

pub enum EntryFile {
    Main(MainFile),
    Lib(LibFile),
}

pub struct MainFile;

impl MainFile {
    pub fn render(&self) -> String {
        "fn main() {
    println!(\"Hello from Rust at Meta!\");
}
"
        .into()
    }
}

pub struct LibFile;

impl LibFile {
    pub fn render(&self) -> String {
        "pub fn add_numbers(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add_numbers(2, 2);
        assert_eq!(result, 4);
    }
}
"
        .into()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Target {
    Library { name: String },
    Binary { name: String },
}

impl Target {
    pub fn render(&self) -> String {
        match self {
            Target::Library { name } => {
                let template = include_str!("../../templates/TARGETS_LIB");
                template.replace("__NAME_PLACEHOLDER__", name)
            }
            Target::Binary { name } => {
                let template = include_str!("../../templates/TARGETS_BIN");
                template.replace("__NAME_PLACEHOLDER__", name)
            }
        }
    }
}

#[test]
fn test_render() {
    let target = Target::Library {
        name: String::from("a-rust-library"),
    };

    let expected = r#"load("@fbcode_macros//build_defs:rust_library.bzl", "rust_library")

rust_library(
    name = "a-rust-library",
    srcs = glob(["src/**/*.rs"]),
    deps = []
)"#;

    assert_eq!(expected, target.render().replace("\r\n", "\n"));

    let target = Target::Binary {
        name: String::from("a-rust-binary"),
    };

    let expected = r#"load("@fbcode_macros//build_defs:rust_binary.bzl", "rust_binary")

rust_binary(
    name = "a-rust-binary",
    srcs = glob(["src/**/*.rs"]),
    deps = []
)"#;

    assert_eq!(expected, target.render().replace("\r\n", "\n"));
}
