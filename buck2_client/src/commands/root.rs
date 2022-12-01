/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::str::FromStr;

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::path_arg::PathArg;
use buck2_common::invocation_roots::find_invocation_roots;
use thiserror::Error;

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
pub struct RootCommand {
    #[clap(short, long, help("which root to print"), default_value("cell"), possible_values(&["package", "cell", "project", "daemon"]))]
    kind: RootKind,
    #[clap(
        help(
            "determine the root for a specific directory (if not provided, finds the root for the current directory)"
        ),
        value_name = "PATH",
        long
    )]
    dir: Option<PathArg>,
}

#[derive(Debug, Error)]
enum RootError {
    #[error("Finding package root isn't yet implemented.")]
    PackageRootUnimplemented,
}

impl RootCommand {
    pub fn exec(
        self,
        _matches: &clap::ArgMatches,
        ctx: ClientCommandContext,
    ) -> anyhow::Result<()> {
        let root = match self.kind {
            RootKind::Package => return Err(RootError::PackageRootUnimplemented.into()),
            RootKind::Cell => match self.dir {
                Some(dir) => find_invocation_roots(&dir.resolve(&ctx.working_dir))?.cell_root,
                None => ctx.paths.cell_root().to_owned(),
            },
            RootKind::Project => match self.dir {
                Some(dir) => find_invocation_roots(&dir.resolve(&ctx.working_dir))?
                    .project_root
                    .root()
                    .to_owned(),
                None => ctx.paths.project_root().root().to_owned(),
            },
            RootKind::Daemon => ctx.paths.daemon_dir()?.path,
        };

        buck2_client_ctx::println!("{}", root.to_string_lossy())?;
        Ok(())
    }
}
