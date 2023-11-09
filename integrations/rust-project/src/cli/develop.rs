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

use tracing::info;

use crate::buck;
use crate::buck::relative_to;
use crate::buck::select_mode;
use crate::buck::to_json_project;
use crate::json_project::JsonProject;
use crate::json_project::Sysroot;
use crate::sysroot::resolve_buckconfig_sysroot;
use crate::sysroot::resolve_rustup_sysroot;
use crate::sysroot::SysrootConfig;
use crate::target::Target;

pub struct Develop {
    pub input: Input,
    pub sysroot: SysrootConfig,
    pub relative_paths: bool,
    pub buck: buck::Buck,
}

pub struct OutputCfg {
    out: Output,
    pretty: bool,
}

pub enum Input {
    Targets(Vec<Target>),
    Files(Vec<PathBuf>),
}

#[derive(Debug)]
pub enum Output {
    Path(PathBuf),
    Stdout,
}

impl Develop {
    pub fn new(input: Input) -> Self {
        let mode = select_mode(None);
        let buck = buck::Buck::new(mode);

        Self {
            input,
            sysroot: SysrootConfig::BuckConfig,
            buck,
            relative_paths: false,
        }
    }

    pub fn from_command(command: crate::Command) -> (Develop, OutputCfg) {
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
                let targets = targets.into_iter().map(Target::new).collect();
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

            let mode = select_mode(mode.as_deref());
            let buck = buck::Buck::new(mode);

            let develop = Develop {
                input,
                sysroot,
                relative_paths,
                buck,
            };
            let out = OutputCfg { out, pretty };

            return (develop, out);
        }

        unreachable!("No other subcommand is supported.")
    }
}

impl Develop {
    // the split between "resolve owners" is necessary, in order provide meaningful progress reporting to users.
    pub fn resolve_owners(&self) -> Result<Vec<Target>, anyhow::Error> {
        let Develop { input, buck, .. } = self;

        let targets = match input {
            Input::Targets(targets) => targets.to_owned(),
            // the owners query returns a `HashMap<PathBuf, Vec<Target>>`
            Input::Files(files) => buck.query_owner(files)?.into_values().flatten().collect(),
        };

        Ok(targets)
    }

    pub fn run(&self, targets: Vec<Target>) -> Result<JsonProject, anyhow::Error> {
        let Develop {
            sysroot,
            relative_paths,
            buck,
            ..
        } = self;

        let project_root = buck.resolve_project_root()?;

        info!("building generated code");
        let expanded_and_resolved = buck.expand_and_resolve(&targets)?;
        let aliased_libraries =
            buck.query_aliased_libraries(&expanded_and_resolved.expanded_targets)?;

        info!("fetching sysroot");
        let sysroot = match &sysroot {
            SysrootConfig::Sysroot(path) => {
                let mut sysroot_path = expand_tilde(path)?.canonicalize()?;
                if *relative_paths {
                    sysroot_path = relative_to(&sysroot_path, &project_root);
                }

                Sysroot {
                    sysroot: sysroot_path,
                    sysroot_src: None,
                }
            }
            SysrootConfig::BuckConfig => {
                resolve_buckconfig_sysroot(&project_root, *relative_paths)?
            }
            SysrootConfig::Rustup => resolve_rustup_sysroot()?,
        };
        info!("converting buck info to rust-project.json");
        let rust_project = to_json_project(
            sysroot,
            expanded_and_resolved,
            aliased_libraries,
            *relative_paths,
        )?;
        Ok(rust_project)
    }

    pub fn run_as_cli(self, cfg: OutputCfg) -> Result<(), anyhow::Error> {
        let targets = self.resolve_owners()?;
        let rust_project = self.run(targets)?;

        let mut writer: BufWriter<Box<dyn Write>> = match cfg.out {
            Output::Path(ref p) => {
                let out = std::fs::File::create(p)?;
                BufWriter::new(Box::new(out))
            }
            Output::Stdout => BufWriter::new(Box::new(std::io::stdout())),
        };

        if cfg.pretty {
            serde_json::to_writer_pretty(&mut writer, &rust_project)?;
        } else {
            serde_json::to_writer(&mut writer, &rust_project)?;
        }
        writeln!(writer)?;

        match cfg.out {
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
