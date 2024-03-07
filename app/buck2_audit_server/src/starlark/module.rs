/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::Write;

use buck2_audit::starlark::module::StarlarkModuleCommand;
use buck2_cli_proto::ClientContext;
use buck2_common::dice::cells::HasCellResolver;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_interpreter::load_module::InterpreterCalculation;
use buck2_interpreter::parse_import::parse_bzl_path_with_config;
use buck2_interpreter::parse_import::ParseImportOptions;
use buck2_interpreter::parse_import::RelativeImports;
use buck2_interpreter::paths::module::StarlarkModulePath;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;

pub(crate) async fn server_execute(
    command: &StarlarkModuleCommand,
    server_ctx: &dyn ServerCommandContextTrait,
    mut stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
    _client_ctx: ClientContext,
) -> anyhow::Result<()> {
    server_ctx
        .with_dice_ctx(async move |server_ctx, mut dice_ctx| {
            let cell_resolver = dice_ctx.get_cell_resolver().await?;
            let cwd = server_ctx.working_dir();
            let current_cell_path = cell_resolver.get_cell_path(cwd)?;
            let current_cell = BuildFileCell::new(current_cell_path.cell());
            let cell_alias_resolver = cell_resolver.get_cwd_cell_alias_resolver(cwd)?;

            let import_path = parse_bzl_path_with_config(
                cell_alias_resolver,
                &command.import_path,
                &ParseImportOptions {
                    relative_import_option: RelativeImports::Allow {
                        current_dir: &current_cell_path,
                    },
                    // Otherwise `@arg` is expanded as mode file.
                    allow_missing_at_symbol: true,
                },
                current_cell,
            )?;

            let loaded_module = dice_ctx
                .get_loaded_module(StarlarkModulePath::LoadFile(&import_path))
                .await?;

            let mut stdout = stdout.as_writer();
            writeln!(stdout, "{}", loaded_module.path())?;
            writeln!(stdout)?;
            writeln!(stdout, "Imports:")?;
            for import in loaded_module.imports() {
                writeln!(stdout, "  {}", import)?;
            }
            writeln!(stdout)?;
            write!(stdout, "{}", loaded_module.env().dump_debug())?;
            Ok(())
        })
        .await
}
