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
use buck2_cli_proto::ClientContext;
use buck2_client_ctx::path_arg::PathArg;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::data::HasIoProvider;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::io::IoProvider;
use buck2_core::cells::CellResolver;
use buck2_interpreter::common::StarlarkPath;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;
use starlark::errors::Lint;
use starlark::syntax::AstModule;

use crate::util::paths::starlark_files;
use crate::StarlarkCommandCommonOptions;
use crate::StarlarkSubcommand;

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
) -> anyhow::Result<Vec<Lint>> {
    let dialect = path.dialect(false);
    let path = cell_resolver.resolve_path(&path.path())?;
    let path_str = path.to_string();
    let content = io.read_file(path).await?;
    let ast = AstModule::parse(&path_str, content, &dialect)?;
    Ok(ast.lint(None))
}

#[async_trait]
impl StarlarkSubcommand for StarlarkLintCommand {
    async fn server_execute(
        &self,
        server_ctx: Box<dyn ServerCommandContextTrait>,
        _client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        server_ctx
            .with_dice_ctx(async move |server_ctx, ctx| {
                let cell_resolver = ctx.get_cell_resolver().await?;
                let fs = ctx.file_ops();
                let io = ctx.global_data().get_io_provider();

                let mut stdout = server_ctx.stdout()?;
                let mut lint_count = 0;
                let files =
                    starlark_files(&self.paths, server_ctx, &cell_resolver, &fs, &*io).await?;
                for file in &files {
                    let lints = lint_file(&file.borrow(), &cell_resolver, &*io).await?;
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
