/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::BufWriter;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;

use anyhow::bail;
use tracing::info;

use crate::buck;
use crate::buck::relative_to;
use crate::buck::select_mode;
use crate::buck::to_json_project;
use crate::json_project::Sysroot;
use crate::sysroot::resolve_buckconfig_sysroot;
use crate::sysroot::resolve_rustup_sysroot;
use crate::sysroot::SysrootConfig;
use crate::target::Target;

pub struct Develop {
    pub input: Input,
    pub out: Output,
    pub sysroot: SysrootConfig,
    pub pretty: bool,
    pub relative_paths: bool,
    pub mode: Option<String>,
}

impl From<crate::Command> for Develop {
    fn from(command: crate::Command) -> Self {
        if let crate::Command::Develop {
            targets,
            files,
            out,
            stdout,
            prefer_rustup_managed_toolchain,
            sysroot,
            pretty,
            relative_paths,
            mode,
        } = command
        {
            let input = if !targets.is_empty() {
                Input::Targets(targets)
            } else {
                Input::Files(files)
            };

            let out = if stdout {
                Output::Stdout
            } else {
                Output::Path(out)
            };

            let sysroot = if prefer_rustup_managed_toolchain {
                SysrootConfig::Rustup
            } else if let Some(sysroot) = sysroot {
                SysrootConfig::Sysroot(sysroot)
            } else {
                SysrootConfig::BuckConfig
            };

            return Develop {
                input,
                out,
                sysroot,
                pretty,
                relative_paths,
                mode,
            };
        }

        unreachable!("No other subcommand is supported.")
    }
}

pub enum Input {
    Targets(Vec<String>),
    Files(Vec<PathBuf>),
}

#[derive(Debug)]
pub enum Output {
    Path(PathBuf),
    Stdout,
}

impl Develop {
    pub fn run(self) -> Result<(), anyhow::Error> {
        let Develop {
            input,
            out,
            sysroot,
            pretty,
            relative_paths,
            mode,
        } = self;
        let mode = select_mode(mode);
        let buck = buck::Buck::new(mode);
        let project_root = buck.resolve_project_root()?;

        let targets = match input {
            Input::Targets(targets) => targets.iter().map(Target::new).collect::<Vec<Target>>(),
            // the owners query returns a `HashMap<PathBuf, Vec<Target>>`
            Input::Files(files) => buck.query_owner(files)?.into_values().flatten().collect(),
        };

        if targets.is_empty() {
            bail!("No targets can be inferred for the provided file.");
        }

        let expanded_and_resolved = buck.expand_and_resolve(&targets)?;
        let aliased_libraries =
            buck.query_aliased_libraries(&expanded_and_resolved.expanded_targets)?;

        let sysroot = match &sysroot {
            SysrootConfig::Sysroot(path) => {
                let mut sysroot_path = expand_tilde(path)?.canonicalize()?;
                if relative_paths {
                    sysroot_path = relative_to(&sysroot_path, &project_root);
                }

                Sysroot {
                    sysroot: sysroot_path,
                    sysroot_src: None,
                }
            }
            SysrootConfig::BuckConfig => resolve_buckconfig_sysroot(&project_root, relative_paths)?,
            SysrootConfig::Rustup => resolve_rustup_sysroot()?,
        };
        info!("converting buck info to rust-project.json");
        let rust_project = to_json_project(
            sysroot,
            expanded_and_resolved,
            aliased_libraries,
            relative_paths,
        )?;

        let mut writer: BufWriter<Box<dyn Write>> = match out {
            Output::Path(ref p) => {
                let out = std::fs::File::create(p)?;
                BufWriter::new(Box::new(out))
            }
            Output::Stdout => BufWriter::new(Box::new(std::io::stdout())),
        };

        if pretty {
            serde_json::to_writer_pretty(&mut writer, &rust_project)?;
        } else {
            serde_json::to_writer(&mut writer, &rust_project)?;
        }
        writeln!(writer)?;

        match out {
            Output::Path(p) => info!(file = ?p, "wrote rust-project.json"),
            Output::Stdout => info!("wrote rust-project.json to stdout"),
        }

        Ok(())
    }
}

fn expand_tilde(path: &Path) -> Result<PathBuf, anyhow::Error> {
    if path.starts_with("~") {
        let path = path.strip_prefix("~")?;
        let home = std::env::var("HOME")?;
        let home = PathBuf::from(home);
        Ok(home.join(path))
    } else {
        Ok(path.to_path_buf())
    }
}
