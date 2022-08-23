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

use buck2_common::dice::cells::HasCellResolver;
use buck2_common::package_listing::dice::HasPackageListingResolver;
use buck2_common::package_listing::resolver::PackageListingResolver;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::pattern::parse_package::parse_package;
use buck2_interpreter::common::StarlarkModulePath;
use buck2_interpreter::common::StarlarkPath;
use buck2_interpreter::dice::HasCalculationDelegate;
use buck2_interpreter::file_loader::LoadedModule;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use cli_proto::ClientContext;

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(
    name = "module",
    about = "Inspect Starlark package file all bzl dependencies by package name like foo//bar/baz"
)]
pub struct StarlarkPackageDepsCommand {
    #[clap(name = "PACKAGE", help = "Package")]
    package: String,
}

impl StarlarkPackageDepsCommand {
    pub async fn server_execute(
        &self,
        mut server_ctx: Box<dyn ServerCommandContextTrait>,
        _client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        let dice_ctx = server_ctx.dice_ctx().await?;

        let cell_resolver = dice_ctx.get_cell_resolver().await?;
        let current_cell_path = cell_resolver.get_cell_path(server_ctx.working_dir())?;
        let current_cell = BuildFileCell::new(current_cell_path.cell().clone());

        let cell_alias_resolver = cell_resolver
            .get(current_cell_path.cell())?
            .cell_alias_resolver();

        let package = parse_package(&self.package, cell_alias_resolver)?;

        let calc = dice_ctx
            .get_interpreter_calculator(package.cell_name(), &current_cell)
            .await?;

        let build_file_name = dice_ctx
            .get_package_listing_resolver()
            .resolve(&package)
            .await?
            .buildfile()
            .to_owned();

        let (_module, module_deps) = calc
            .prepare_eval(StarlarkPath::BuildFile(&BuildFilePath::new(
                package,
                build_file_name,
            )))
            .await?;

        let mut stdout = server_ctx.stdout()?;

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
                    StarlarkModulePath::BxlFile(_) => return Err(anyhow::anyhow!("bxl be here")),
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
    }
}
