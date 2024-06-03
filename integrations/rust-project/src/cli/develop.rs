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

use rustc_hash::FxHashMap;
use rustc_hash::FxHashSet;
use tracing::info;
use tracing::warn;

use crate::buck;
use crate::buck::relative_to;
use crate::buck::select_mode;
use crate::buck::to_json_project;
use crate::json_project::JsonProject;
use crate::json_project::Sysroot;
use crate::path::canonicalize;
use crate::sysroot::resolve_buckconfig_sysroot;
use crate::sysroot::resolve_rustup_sysroot;
use crate::sysroot::SysrootConfig;
use crate::target::Target;
use crate::Command;

#[derive(Debug)]
pub(crate) struct Develop {
    pub(crate) sysroot: SysrootConfig,
    pub(crate) relative_paths: bool,
    pub(crate) buck: buck::Buck,
    pub(crate) check_cycles: bool,
}

pub(crate) struct OutputCfg {
    out: Output,
    pretty: bool,
}

#[derive(Debug)]
pub(crate) enum Input {
    Targets(Vec<Target>),
    Files(Vec<PathBuf>),
}

#[derive(Debug)]
pub(crate) enum Output {
    Path(PathBuf),
    Stdout,
}

impl Develop {
    pub(crate) fn new() -> Self {
        let mode = select_mode(None);
        let buck = buck::Buck::new(mode);

        Self {
            sysroot: SysrootConfig::BuckConfig,
            buck,
            relative_paths: false,
            check_cycles: false,
        }
    }

    pub(crate) fn from_command(command: Command) -> (Develop, Input, OutputCfg) {
        if let crate::Command::Develop {
            files,
            targets,
            out,
            stdout,
            prefer_rustup_managed_toolchain,
            sysroot,
            pretty,
            relative_paths,
            mode,
            check_cycles,
        } = command
        {
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
                sysroot,
                relative_paths,
                buck,
                check_cycles,
            };
            let out = OutputCfg { out, pretty };

            let input = if !targets.is_empty() {
                let targets = targets.into_iter().map(Target::new).collect();
                Input::Targets(targets)
            } else {
                Input::Files(files)
            };

            return (develop, input, out);
        }

        unreachable!("No other subcommand is supported.")
    }
}

const DEFAULT_EXTRA_TARGETS: usize = 50;

impl Develop {
    /// For every Rust file, return the relevant buck targets that should be used to configure rust-analyzer.
    pub(crate) fn related_targets(&self, files: &[PathBuf]) -> Result<Vec<Target>, anyhow::Error> {
        // We always want the targets that directly own these Rust files.
        let direct_owning_targets = dedupe_unittest(&self.resolve_file_targets(files));

        let unique_owning_targets = direct_owning_targets
            .iter()
            .cloned()
            .collect::<FxHashSet<_>>();

        let mut targets = direct_owning_targets;

        // We want to load additional targets from the enclosing buildfile, to help users
        // who have a bunch of small targets in their buildfile. However, we want to set a limit
        // so we don't try to load everything in very large generated buildfiles.
        let max_extra_targets: usize = match std::env::var("RUST_PROJECT_EXTRA_TARGETS") {
            Ok(s) => s.parse::<usize>().unwrap_or(DEFAULT_EXTRA_TARGETS),
            Err(_) => DEFAULT_EXTRA_TARGETS,
        };

        // Include additional targets from the found buildfiles, up to max_extra_targets more.
        let owners = self.resolve_file_owners(files);
        let extra_targets = owners
            .values()
            .flatten()
            .filter(|t| !unique_owning_targets.contains(t))
            .cloned()
            .collect::<Vec<_>>();

        targets.extend(
            dedupe_unittest(&extra_targets)
                .into_iter()
                .take(max_extra_targets),
        );

        Ok(targets)
    }

    fn resolve_file_targets(&self, files: &[PathBuf]) -> Vec<Target> {
        let file_targets = match self.buck.query_owner(files) {
            Ok(targets) => {
                for (file, targets) in targets.iter() {
                    if targets.is_empty() {
                        warn!(file = ?file, "Buck returned zero targets for this file.");
                    }
                }

                targets.into_values().flatten().collect::<Vec<_>>()
            }
            Err(_) => {
                let mut file_targets = vec![];
                for file in files {
                    match self.buck.query_owner(&[file.to_path_buf()]) {
                        Ok(targets) => {
                            file_targets.extend(targets.into_values().flatten());
                        }
                        Err(e) => {
                            warn!(file = ?file, "Could not find a target that owns this file: {:#}", e);
                        }
                    }
                }

                file_targets
            }
        };

        dedupe_targets(&file_targets)
    }

    pub fn resolve_file_owners(&self, files: &[PathBuf]) -> FxHashMap<PathBuf, Vec<Target>> {
        match self.buck.query_owning_buildfile(&files) {
            Ok(owners) => owners,
            Err(_) => {
                let mut owners = FxHashMap::default();

                for file in files {
                    match self.buck.query_owning_buildfile(&[file.to_path_buf()]) {
                        Ok(file_owners) => {
                            owners.extend(file_owners.into_iter());
                        }
                        Err(e) => {
                            warn!(file = ?file, "Could not find a target that owns this file: {}", e);
                        }
                    }
                }

                owners
            }
        }
    }

    pub(crate) fn run(&self, targets: Vec<Target>) -> Result<JsonProject, anyhow::Error> {
        let Develop {
            sysroot,
            relative_paths,
            buck,
            check_cycles,
        } = self;

        let project_root = buck.resolve_project_root()?;

        info!("building generated code");
        let expanded_and_resolved = buck.expand_and_resolve(&targets)?;
        let aliased_libraries =
            buck.query_aliased_libraries(&expanded_and_resolved.expanded_targets)?;

        info!("fetching sysroot");
        let sysroot = match &sysroot {
            SysrootConfig::Sysroot(path) => {
                let mut sysroot_path = canonicalize(expand_tilde(path)?)?;
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
            *check_cycles,
        )?;
        Ok(rust_project)
    }

    pub(crate) fn run_as_cli(self, input: Input, cfg: OutputCfg) -> Result<(), anyhow::Error> {
        let targets = match input {
            Input::Targets(targets) => targets,
            Input::Files(files) => {
                let canonical_files = files
                    .into_iter()
                    .map(|p| match canonicalize(&p) {
                        Ok(path) => path,
                        Err(_) => p,
                    })
                    .collect::<Vec<_>>();

                self.related_targets(&canonical_files)?
            }
        };

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

/// Remove duplicate targets, but preserve order.
fn dedupe_targets(targets: &[Target]) -> Vec<Target> {
    let mut seen = FxHashSet::default();
    let mut unique_targets = vec![];
    for target in targets {
        if !seen.contains(target) {
            seen.insert(target);
            unique_targets.push(target.clone());
        }
    }

    unique_targets
}

// Don't bother with target `foo` if `foo-unittest` is present.
fn dedupe_unittest(targets: &[Target]) -> Vec<Target> {
    let unique_targets = targets.iter().collect::<FxHashSet<_>>();

    targets
        .iter()
        .filter(|t| !unique_targets.contains(&Target::new(format!("{}-unittest", t))))
        .cloned()
        .collect()
}

#[test]
fn test_dedupe_unittest() {
    let targets = vec![
        Target::new("foo-unittest".to_owned()),
        Target::new("bar".to_owned()),
        Target::new("foo".to_owned()),
        Target::new("baz-unittest".to_owned()),
    ];

    let expected = vec![
        Target::new("foo-unittest".to_owned()),
        Target::new("bar".to_owned()),
        Target::new("baz-unittest".to_owned()),
    ];

    assert_eq!(dedupe_unittest(&targets), expected);
}
