/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::collections::HashSet;
use std::io::Write;
use std::sync::Arc;

use anyhow::Context;
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
use buck2_interpreter::paths::path::StarlarkPath;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use dice::DiceTransaction;
use dupe::Dupe;
use starlark::codemap::FileSpan;
use starlark::errors::Diagnostic;
use starlark::errors::EvalSeverity;
use starlark::errors::Lint;
use starlark::syntax::AstModule;

use crate::util::environment::Environment;
use crate::util::paths::starlark_files;
use crate::StarlarkCommandCommonOptions;
use crate::StarlarkOpaqueSubcommand;

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(name = "starlark-lint", about = "Run the Starlark linter.")]
pub struct StarlarkLintCommand {
    #[clap(flatten)]
    common_opts: StarlarkCommandCommonOptions,

    #[clap(value_name = "PATH", required = true)]
    paths: Vec<PathArg>,
}

/// The cache of names for a path, keyed by its CellName and its path type.
struct Cache<'a> {
    dice: &'a DiceTransaction,
    cached: HashMap<(CellName, StarlarkFileType), Arc<HashSet<String>>>,
}

impl<'a> Cache<'a> {
    pub(crate) fn new(dice: &'a DiceTransaction) -> Cache<'a> {
        Self {
            dice,
            cached: HashMap::new(),
        }
    }

    pub(crate) async fn get_names(
        &mut self,
        path: &StarlarkPath<'_>,
    ) -> anyhow::Result<Arc<HashSet<String>>> {
        let path_type = path.file_type();
        let cell = path.cell();
        if let Some(res) = self.cached.get(&(cell, path_type)) {
            return Ok(res.dupe());
        }
        let env: Environment = Environment::new(cell, path_type, self.dice).await?;
        let res = Arc::new(env.get_names(path_type, self.dice).await?);
        self.cached.insert((cell, path_type), res.dupe());
        Ok(res)
    }
}

async fn lint_file(
    path: &StarlarkPath<'_>,
    cell_resolver: &CellResolver,
    io: &dyn IoProvider,
    cache: &mut Cache<'_>,
) -> anyhow::Result<Vec<Lint>> {
    let dialect = path.file_type().dialect(false);
    let proj_path = cell_resolver.resolve_path(path.path().as_ref().as_ref())?;
    let path_str = proj_path.to_string();
    let content = io
        .read_file_if_exists(proj_path)
        .await?
        .with_context(|| format!("File not found: `{}`", path_str))?;
    match AstModule::parse(&path_str, content.clone(), &dialect) {
        Ok(ast) => Ok(ast.lint(Some(&*cache.get_names(path).await?))),
        Err(err) => {
            // There was a parse error, so we don't want to fail, we want to give a nice error message
            // Do the best we can - it is probably a `Diagnostic`, which gives us more precise info.
            let (span, message) = match err.downcast::<Diagnostic>() {
                Err(err) => (None, err),
                Ok(diag) => (diag.span, diag.message),
            };
            Ok(vec![Lint {
                location: span.unwrap_or_else(|| FileSpan::new(path_str, content)),
                short_name: "parse_error".to_owned(),
                severity: EvalSeverity::Error,
                problem: format!("{:#}", message),
                original: "".to_owned(),
            }])
        }
    }
}

#[async_trait]
impl StarlarkOpaqueSubcommand for StarlarkLintCommand {
    async fn server_execute(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        mut stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        _client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        server_ctx
            .with_dice_ctx(async move |server_ctx, ctx| {
                let cell_resolver = ctx.get_cell_resolver().await?;
                let fs = ctx.file_ops();
                let io = ctx.global_data().get_io_provider();
                let mut cache = Cache::new(&ctx);

                let mut stdout = stdout.as_writer();
                let mut lint_count = 0;
                let files =
                    starlark_files(&self.paths, server_ctx, &cell_resolver, &fs, &*io).await?;
                for file in &files {
                    let lints = lint_file(&file.borrow(), &cell_resolver, &*io, &mut cache).await?;
                    lint_count += lints.len();
                    for lint in lints {
                        writeln!(stdout, "{}", lint)?;
                    }
                }
                if lint_count > 0 {
                    Err(anyhow::anyhow!("Found {} lints", lint_count))
                } else {
                    writeln!(
                        server_ctx.stderr()?,
                        "Found no lints in {} files",
                        files.len()
                    )?;
                    Ok(())
                }
            })
            .await
    }

    fn common_opts(&self) -> &StarlarkCommandCommonOptions {
        &self.common_opts
    }
}
