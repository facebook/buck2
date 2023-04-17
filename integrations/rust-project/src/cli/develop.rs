/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;
use std::io::BufWriter;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;

use anyhow::bail;
use tracing::info;

use crate::buck;
use crate::buck::to_json_project;
use crate::target::Target;
use crate::target::TargetInfo;

pub struct Develop {
    pub input: Input,
    pub out: Output,
    pub sysroot: Option<PathBuf>,
    pub pretty: bool,
}

impl From<crate::Command> for Develop {
    fn from(command: crate::Command) -> Self {
        if let crate::Command::Develop {
            targets,
            files,
            out,
            stdout,
            sysroot,
            pretty,
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

            return Develop {
                input,
                out,
                sysroot,
                pretty,
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
        } = self;
        let buck = buck::Buck;

        let targets = match input {
            Input::Targets(targets) => targets.iter().map(Target::new).collect::<Vec<Target>>(),
            // the owners query returns a `HashMap<PathBuf, Vec<Target>>`
            Input::Files(files) => buck.query_owner(files)?.into_values().flatten().collect(),
        };

        if targets.is_empty() {
            bail!("No targets can be inferred for the provided file.");
        }

        tracing::info!("expanding targets");
        let targets = buck.expand_targets(&targets)?;

        if targets.len() <= 10 {
            // printing out 10 targets is pretty reasonable information for the user
            tracing::info!(target_arg = ?targets, targets_num = targets.len(), ?targets, "resolving dependencies");
        } else {
            // after 10 targets, however, things tend to get a bit unwieldy.
            info!(target_arg = ?targets, targets_num = targets.len(), "resolving dependencies")
        }
        let target_map: BTreeMap<Target, TargetInfo> = buck.resolve_deps(&targets)?;
        let aliased_libraries = buck.query_aliased_libraries(&targets)?;
        let proc_macros = buck.query_proc_macros(&targets)?;

        let sysroot = match &sysroot {
            Some(s) => Some(expand_tilde(s)?.canonicalize()?),
            None => None,
        };
        info!("Converting buck info to rust-project.json");
        let rust_project =
            to_json_project(sysroot, targets, target_map, aliased_libraries, proc_macros)?;

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
