/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::Write;

use buck2_build_api::calculation::Calculation;
use buck2_common::dice::cells::HasCellResolver;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_interpreter::common::StarlarkModulePath;
use buck2_interpreter::parse_import::parse_import_with_config;
use buck2_interpreter::parse_import::ParseImportOptions;
use buck2_server::ctx::ServerCommandContext;
use cli_proto::ClientContext;

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(
    name = "module",
    about = "Inspect Starlark module by fully qualified import string like foo//bar:baz.bzl"
)]
pub(crate) struct StarlarkModuleCommand {
    #[clap(name = "IMPORT_PATH", help = "Module import path")]
    import_path: String,
}

impl StarlarkModuleCommand {
    pub(crate) async fn server_execute(
        &self,
        mut server_ctx: ServerCommandContext,
        _client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        let dice_ctx = server_ctx.dice_ctx().await?;

        let cell_resolver = dice_ctx.get_cell_resolver().await?;
        let current_cell_path = cell_resolver.get_cell_path(&server_ctx.working_dir)?;
        let current_cell = BuildFileCell::new(current_cell_path.cell().clone());

        let cell_alias_resolver = cell_resolver
            .get(current_cell_path.cell())?
            .cell_alias_resolver();

        let path = parse_import_with_config(
            cell_alias_resolver,
            &current_cell_path,
            &self.import_path,
            &ParseImportOptions {
                allow_relative_imports: true,
                // Otherwise `@arg` is expanded as mode file.
                allow_missing_at_symbol: true,
            },
        )?;

        let import_path = ImportPath::new(path, current_cell)?;

        let loaded_module = dice_ctx
            .get_loaded_module(StarlarkModulePath::LoadFile(&import_path))
            .await?;

        let mut stdout = server_ctx.stdout()?;
        writeln!(stdout, "{}", loaded_module.path())?;
        writeln!(stdout)?;
        writeln!(stdout, "Imports:")?;
        for import in loaded_module.imports() {
            writeln!(stdout, "  {}", import)?;
        }
        writeln!(stdout)?;
        write!(stdout, "{}", loaded_module.env().dump_debug())?;
        Ok(())
    }
}
