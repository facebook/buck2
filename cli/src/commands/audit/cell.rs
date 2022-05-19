/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::Write;

use async_trait::async_trait;
use buck2_common::dice::cells::HasCellResolver;
use buck2_core::cells::CellAlias;
use cli_proto::ClientContext;
use indexmap::IndexMap;
use structopt::StructOpt;

use crate::{
    commands::{
        audit::AuditSubcommand,
        common::{CommonConfigOptions, CommonConsoleOptions, CommonEventLogOptions},
    },
    daemon::server::ServerCommandContext,
};

#[derive(Debug, StructOpt, serde::Serialize, serde::Deserialize)]
#[structopt(
    name = "audit-cell",
    about = "Query information about the [repositories] list in .buckconfig."
)]
pub struct AuditCellCommand {
    #[structopt(long = "json", help = "Output in JSON format")]
    json: bool,

    #[structopt(
        long = "paths-only",
        help = "Don't include the cell name in the output"
    )]
    paths_only: bool,

    #[structopt(
        long = "aliases",
        help = "If enabled and no explicit aliases are passed, will query for all aliases in the working directory cell."
    )]
    aliases: bool,

    #[structopt(
        name = "CELL_ALIASES",
        help = "Cell aliases to query. These aliases will be resolved in the working directory cell."
    )]
    aliases_to_resolve: Vec<String>,
}

#[async_trait]
impl AuditSubcommand for AuditCellCommand {
    async fn server_execute(
        &self,
        mut server_ctx: ServerCommandContext,
        _client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        let ctx = server_ctx.dice_ctx().await?;
        let cells = ctx.get_cell_resolver().await;
        let fs = server_ctx.file_system();
        let cwd = &server_ctx.working_dir;
        let this_cell = cells.get(cells.find(cwd)?).unwrap();

        let mappings: IndexMap<_, _> = {
            if self.aliases_to_resolve.is_empty() {
                if self.aliases {
                    this_cell
                        .cell_alias_resolver()
                        .mappings()
                        .map(|(alias, cell_name)| {
                            (
                                alias.to_string(),
                                fs.resolve(cells.get(cell_name).unwrap().path()),
                            )
                        })
                        .collect()
                } else {
                    cells
                        .cells()
                        .map(|(name, cell)| (name.as_str().to_owned(), fs.resolve(cell.path())))
                        .collect()
                }
            } else {
                let cell_alias_resolver = this_cell.cell_alias_resolver();
                self.aliases_to_resolve
                    .iter()
                    .map(|alias| {
                        Ok((
                            alias.to_owned(),
                            fs.resolve(
                                cells
                                    .get(
                                        cell_alias_resolver
                                            .resolve(&CellAlias::new(alias.to_owned()))?,
                                    )
                                    .unwrap()
                                    .path(),
                            ),
                        ))
                    })
                    .collect::<anyhow::Result<_>>()?
            }
        };

        let mut stdout = server_ctx.stdout()?;
        if self.paths_only {
            if self.json {
                let paths: Vec<_> = mappings.values().collect();
                writeln!(stdout, "{}", serde_json::to_string_pretty(&paths)?)?;
            } else {
                for v in mappings.values() {
                    writeln!(stdout, "{}", v)?;
                }
            }
        } else if self.json {
            writeln!(stdout, "{}", serde_json::to_string_pretty(&mappings)?)?;
        } else {
            for (k, v) in mappings {
                writeln!(stdout, "{}: {}", k, v)?;
            }
        }

        Ok(())
    }

    fn config_opts(&self) -> Option<&CommonConfigOptions> {
        None
    }

    fn console_opts(&self) -> Option<&CommonConsoleOptions> {
        None
    }

    fn event_log_opts(&self) -> Option<&CommonEventLogOptions> {
        None
    }
}
