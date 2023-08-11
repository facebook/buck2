/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::io::Write;
use std::sync::Arc;

use anyhow::Context;
use async_recursion::async_recursion;
use async_trait::async_trait;
use buck2_cli_proto::ClientContext;
use buck2_client_ctx::path_arg::PathArg;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::data::HasIoProvider;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::io::IoProvider;
use buck2_core::cells::name::CellName;
use buck2_core::cells::CellResolver;
use buck2_interpreter::file_type::StarlarkFileType;
use buck2_interpreter::paths::path::OwnedStarlarkPath;
use buck2_interpreter_for_build::interpreter::dice_calculation_delegate::HasCalculationDelegate;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use dice::DiceTransaction;
use dupe::Dupe;
use starlark::environment::Globals;
use starlark::typing::Interface;
use starlark::typing::TypingOracle;

use crate::oracle_buck::oracle_buck;
use crate::util::environment::Environment;
use crate::util::paths::starlark_files;
use crate::StarlarkCommandCommonOptions;
use crate::StarlarkOpaqueSubcommand;

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(name = "starlark-typecheck", about = "Run the Starlark typechecker.")]
pub struct StarlarkTypecheckCommand {
    #[clap(flatten)]
    common_opts: StarlarkCommandCommonOptions,

    #[clap(value_name = "PATH", required = true)]
    paths: Vec<PathArg>,
}

struct Cache<'a> {
    // Things we have access to get information
    dice: &'a DiceTransaction,
    io: &'a dyn IoProvider,
    cell_resolver: &'a CellResolver,
    // Things we have access to write information
    stdout: &'a mut (dyn Write + Send + Sync),
    stderr: &'a mut (dyn Write + Send + Sync),
    // Our accumulated state
    oracle: HashMap<(CellName, StarlarkFileType), (Arc<dyn TypingOracle + Send + Sync>, Globals)>,
    cache: HashMap<OwnedStarlarkPath, Interface>,
}

impl<'a> Cache<'a> {
    async fn typecheck(&mut self, path: OwnedStarlarkPath) -> anyhow::Result<()> {
        self.run(path).await?;
        Ok(())
    }

    async fn get_oracle(
        &mut self,
        cell: CellName,
        path_type: StarlarkFileType,
    ) -> anyhow::Result<(Arc<dyn TypingOracle + Send + Sync>, Globals)> {
        match self.oracle.get(&(cell, path_type)) {
            Some((o, g)) => Ok((o.dupe(), g.dupe())),
            None => {
                let globals = Environment::new(cell, path_type, self.dice).await?.globals;
                let res = oracle_buck(&globals);
                self.oracle
                    .insert((cell, path_type), (res.dupe(), globals.dupe()));
                Ok((res, globals))
            }
        }
    }

    async fn get(&mut self, path: OwnedStarlarkPath) -> anyhow::Result<Interface> {
        match self.cache.get(&path) {
            Some(x) => Ok(x.dupe()),
            None => {
                let res = self.run(path.clone()).await?;
                self.cache.insert(path, res.dupe());
                Ok(res)
            }
        }
    }

    #[async_recursion]
    async fn run(&mut self, path: OwnedStarlarkPath) -> anyhow::Result<Interface> {
        let path_ref = path.borrow();
        writeln!(self.stderr, "Type checking: {path_ref}")?;
        let proj_path = self
            .cell_resolver
            .resolve_path(path_ref.path().as_ref().as_ref())?;
        let path_str = proj_path.to_string();
        let src = self
            .io
            .read_file_if_exists(proj_path)
            .await?
            .with_context(|| format!("File not found: `{path_str}`"))?;

        let interp = self
            .dice
            .get_interpreter_calculator(path_ref.cell(), path_ref.build_file_cell())
            .await?;

        let ast = interp.prepare_eval_with_content(path_ref, src)?;
        let mut loads = HashMap::new();
        for x in ast.loads() {
            let y = interp.resolve_load(path_ref, x.module_id).await?;
            let interface = self.get(y.into_starlark_path()).await?;
            loads.insert(x.module_id.to_owned(), interface);
        }
        let (oracle, globals) = self
            .get_oracle(path_ref.cell(), path_ref.file_type())
            .await?;
        let (errors, bindings, interface, approxiomations) =
            ast.typecheck(&*oracle, &globals, &loads);

        if !approxiomations.is_empty() {
            writeln!(self.stderr, "\n\nAPPROXIMATIONS:")?;
            for x in approxiomations {
                writeln!(self.stderr, "{x}")?;
            }
        }

        writeln!(self.stderr, "\n\nBINDINGS:\n{bindings}")?;

        let errors_count = errors.len();
        if errors_count == 0 {
            Ok(interface)
        } else {
            writeln!(self.stdout, "\n\nERRORS:")?;
            for x in errors {
                writeln!(self.stdout, "{x}")?;
            }
            Err(anyhow::anyhow!("Detected {errors_count} errors"))
        }
    }
}

#[async_trait]
impl StarlarkOpaqueSubcommand for StarlarkTypecheckCommand {
    async fn server_execute(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        mut stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        _client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        server_ctx
            .with_dice_ctx(async move |server_ctx, dice| {
                let cell_resolver = dice.get_cell_resolver().await?;
                let fs = dice.file_ops();
                let io = dice.global_data().get_io_provider();

                let files =
                    starlark_files(&self.paths, server_ctx, &cell_resolver, &fs, &*io).await?;
                let mut stdout = stdout.as_writer();
                let mut stderr = server_ctx.stderr()?;
                let mut cache = Cache {
                    dice: &dice,
                    io: &*io,
                    cell_resolver: &cell_resolver,
                    stdout: &mut stdout,
                    stderr: &mut stderr,
                    oracle: HashMap::new(),
                    cache: HashMap::new(),
                };
                for file in files {
                    cache.typecheck(file).await?;
                }
                let file_count = cache.cache.len();
                writeln!(stderr, "Found no type errors in {file_count} files")?;
                Ok(())
            })
            .await
    }

    fn common_opts(&self) -> &StarlarkCommandCommonOptions {
        &self.common_opts
    }
}
