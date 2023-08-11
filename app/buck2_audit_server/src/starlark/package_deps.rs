/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashSet;
use std::io::Write;

use buck2_audit::starlark::package_deps::StarlarkPackageDepsCommand;
use buck2_cli_proto::ClientContext;
use buck2_common::dice::cells::HasCellResolver;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::pattern::parse_package::parse_package;
use buck2_interpreter::file_loader::LoadedModule;
use buck2_interpreter::load_module::INTERPRETER_CALCULATION_IMPL;
use buck2_interpreter::paths::module::StarlarkModulePath;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;

pub(crate) async fn server_execute(
    command: &StarlarkPackageDepsCommand,
    server_ctx: &dyn ServerCommandContextTrait,
    mut stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
    _client_ctx: ClientContext,
) -> anyhow::Result<()> {
    server_ctx
        .with_dice_ctx(async move |server_ctx, dice_ctx| {
            let cell_resolver = dice_ctx.get_cell_resolver().await?;
            let current_cell_path = cell_resolver.get_cell_path(server_ctx.working_dir())?;
            let current_cell = BuildFileCell::new(current_cell_path.cell());

            let cell_alias_resolver = cell_resolver
                .get(current_cell_path.cell())?
                .cell_alias_resolver();

            let package = parse_package(&command.package, cell_alias_resolver)?;

            let module_deps = INTERPRETER_CALCULATION_IMPL
                .get()?
                .get_module_deps(&dice_ctx, package, current_cell)
                .await?;

            let mut stdout = stdout.as_writer();

            struct Printer {
                first: bool,
                visited: HashSet<ImportPath>,
            }

            impl Printer {
                fn print_module_and_deps(
                    &mut self,
                    module: &LoadedModule,
                    stdout: &mut dyn Write,
                ) -> anyhow::Result<()> {
                    let path = match module.path() {
                        StarlarkModulePath::LoadFile(path) => path,
                        StarlarkModulePath::BxlFile(_) => {
                            return Err(anyhow::anyhow!("bxl be here"));
                        }
                    };

                    if !self.visited.insert(path.clone()) {
                        return Ok(());
                    }

                    for import in module.loaded_modules().map.values() {
                        self.print_module_and_deps(import, stdout)?;
                    }

                    if !self.first {
                        writeln!(stdout)?;
                        writeln!(stdout)?;
                    }
                    self.first = false;

                    writeln!(stdout, "# {}", path)?;
                    writeln!(stdout)?;
                    write!(stdout, "{}", module.env().dump_debug())?;

                    Ok(())
                }
            }

            let mut printer = Printer {
                first: true,
                visited: HashSet::new(),
            };

            for module in module_deps.0.into_iter() {
                printer.print_module_and_deps(&module, &mut stdout)?;
            }

            Ok(())
        })
        .await
}
