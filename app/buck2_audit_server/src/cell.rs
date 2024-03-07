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
use buck2_audit::cell::AuditCellCommand;
use buck2_build_api::audit_cell::AUDIT_CELL;
use buck2_cli_proto::ClientContext;
use buck2_common::dice::cells::HasCellResolver;
use buck2_core::cells::CellResolver;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use indexmap::IndexMap;

use crate::AuditSubcommand;

#[async_trait]
impl AuditSubcommand for AuditCellCommand {
    async fn server_execute(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        mut stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        _client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        server_ctx
            .with_dice_ctx(async move |server_ctx, mut ctx| {
                let cells = ctx.get_cell_resolver().await?;
                let fs = server_ctx.project_root();
                let cwd = server_ctx.working_dir();

                let mappings = audit_cell(&self.aliases_to_resolve, self.aliases, &cells, cwd, fs)?;

                let mut stdout = stdout.as_writer();
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
            })
            .await
    }
}

pub(crate) fn audit_cell(
    aliases_to_resolve: &[String],
    aliases: bool,
    cells: &CellResolver,
    cwd: &ProjectRelativePath,
    fs: &ProjectRoot,
) -> anyhow::Result<IndexMap<String, AbsNormPathBuf>> {
    let this_resolver = cells.get_cwd_cell_alias_resolver(cwd)?;
    let mappings: IndexMap<_, _> = {
        if aliases_to_resolve.is_empty() {
            if aliases {
                this_resolver
                    .mappings()
                    .map(|(alias, cell_name)| {
                        (
                            alias.to_string(),
                            fs.resolve(
                                cells
                                    .get(cell_name)
                                    .unwrap()
                                    .path()
                                    .as_project_relative_path(),
                            ),
                        )
                    })
                    .collect()
            } else {
                cells
                    .cells()
                    .map(|(name, cell)| {
                        (
                            name.as_str().to_owned(),
                            fs.resolve(cell.path().as_project_relative_path()),
                        )
                    })
                    .collect()
            }
        } else {
            aliases_to_resolve
                .iter()
                .map(|alias| {
                    Ok((
                        alias.to_owned(),
                        fs.resolve(
                            cells
                                .get(this_resolver.resolve(alias)?)
                                .unwrap()
                                .path()
                                .as_project_relative_path(),
                        ),
                    ))
                })
                .collect::<anyhow::Result<_>>()?
        }
    };
    Ok(mappings)
}

pub(crate) fn init_audit_cell() {
    AUDIT_CELL.init(|aliases_to_resolve, aliases, cells, cwd, fs| {
        audit_cell(aliases_to_resolve, aliases, cells, cwd, fs)
    });
}
