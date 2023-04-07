/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::Write;

use anyhow::Context;
use async_trait::async_trait;
use buck2_cli_proto::ClientContext;
use buck2_client_ctx::path_arg::PathArg;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::data::HasIoProvider;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::io::IoProvider;
use buck2_core::cells::CellResolver;
use buck2_interpreter::path::StarlarkPath;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use starlark::codemap::FileSpan;
use starlark::errors::Diagnostic;
use starlark::errors::Lint;
use starlark::syntax::AstModule;

use crate::util::globals::CachedGlobals;
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

async fn lint_file(
    path: &StarlarkPath<'_>,
    cell_resolver: &CellResolver,
    io: &dyn IoProvider,
    cached_globals: &mut CachedGlobals<'_>,
) -> anyhow::Result<Vec<Lint>> {
    let dialect = path.file_type().dialect(false);
    let proj_path = cell_resolver.resolve_path(path.path().as_ref().as_ref())?;
    let path_str = proj_path.to_string();
    let content = io
        .read_file_if_exists(proj_path)
        .await?
        .with_context(|| format!("File not found: `{}`", path_str))?;
    match AstModule::parse(&path_str, content.clone(), &dialect) {
        Ok(ast) => Ok(ast.lint(Some(&*cached_globals.get_names(path).await?))),
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
                serious: true,
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
        server_ctx: Box<dyn ServerCommandContextTrait>,
        mut stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        _client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        server_ctx
            .with_dice_ctx(async move |server_ctx, ctx| {
                let cell_resolver = ctx.get_cell_resolver().await?;
                let fs = ctx.file_ops();
                let io = ctx.global_data().get_io_provider();
                let mut cached_globals = CachedGlobals::new(&ctx);

                let mut stdout = stdout.as_writer();
                let mut lint_count = 0;
                let files =
                    starlark_files(&self.paths, server_ctx, &cell_resolver, &fs, &*io).await?;
                for file in &files {
                    let lints =
                        lint_file(&file.borrow(), &cell_resolver, &*io, &mut cached_globals)
                            .await?;
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
