/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::PathBuf;
use std::str::FromStr;

use buck2_common::invocation_roots::find_invocation_roots;
use thiserror::Error;

use crate::client_command_context::ClientCommandContext;

#[derive(Debug)]
enum RootKind {
    Package,
    Cell,
    Project,
    Daemon,
}

impl FromStr for RootKind {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "package" => Ok(Self::Package),
            "cell" => Ok(Self::Cell),
            "project" => Ok(Self::Project),
            "daemon" => Ok(Self::Daemon),
            _ => Err("expected one of `package`, `cell`, `project`, or `daemon`".to_owned()),
        }
    }
}

#[derive(Debug, clap::Parser)]
#[clap(about = "Find buck cell, project or package root")]
pub(crate) struct RootCommand {
    #[clap(short, long, help("which root to print"), default_value("cell"), possible_values(&["package", "cell", "project", "daemon"]))]
    kind: RootKind,
    #[clap(
        help(
            "determine the root for a specific directory (if not provided, finds the root for the current directory)"
        ),
        value_name = "PATH",
        parse(from_os_str),
        long
    )]
    dir: Option<PathBuf>,
}

#[derive(Debug, Error)]
enum RootError {
    #[error("Finding package root isn't yet implemented.")]
    PackageRootUnimplemented,
}

impl RootCommand {
    pub(crate) fn exec(
        self,
        _matches: &clap::ArgMatches,
        ctx: ClientCommandContext,
    ) -> anyhow::Result<()> {
        let root = match self.kind {
            RootKind::Package => return Err(RootError::PackageRootUnimplemented.into()),
            RootKind::Cell => match self.dir {
                Some(dir) => find_invocation_roots(&dir)?.cell_root,
                None => ctx.paths()?.cell_root().to_owned(),
            },
            RootKind::Project => match self.dir {
                Some(dir) => find_invocation_roots(&dir)?.project_root.root().to_owned(),
                None => ctx.paths()?.project_root().root().to_owned(),
            },
            RootKind::Daemon => ctx.paths()?.daemon_dir()?,
        };

        crate::println!("{}", root.to_string_lossy())?;
        Ok(())
    }
}
