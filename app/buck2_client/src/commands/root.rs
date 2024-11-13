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
use buck2_common::argv::Argv;
use buck2_common::argv::SanitizedArgv;
use buck2_common::invocation_roots::find_invocation_roots;

#[derive(Debug, Clone, clap::ValueEnum)]
enum RootKind {
    Cell,
    Project,
    Daemon,
}

impl FromStr for RootKind {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
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
    #[clap(
        short,
        long,
        help("which root to print"),
        default_value("cell"),
        value_enum
    )]
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

impl RootCommand {
    pub fn exec(
        self,
        _matches: &clap::ArgMatches,
        ctx: ClientCommandContext<'_>,
    ) -> anyhow::Result<()> {
        let root = if matches!(self.kind, RootKind::Daemon) {
            ctx.paths()?.daemon_dir()?.path
        } else {
            let roots = match self.dir {
                Some(dir) => find_invocation_roots(&dir.resolve(&ctx.working_dir))?,
                None => ctx.paths()?.roots.clone(),
            };
            match self.kind {
                RootKind::Cell => roots.cell_root,
                RootKind::Project => roots.project_root.root().to_owned(),
                // Handled above
                RootKind::Daemon => unreachable!(),
            }
        };

        buck2_client_ctx::println!("{}", root.to_string_lossy())?;
        Ok(())
    }

    pub fn sanitize_argv(&self, argv: Argv) -> SanitizedArgv {
        argv.no_need_to_sanitize()
    }
}
