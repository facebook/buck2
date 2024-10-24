/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashSet;

use buck2_cli_proto::new_generic::DocsOutputFormat;
use buck2_cli_proto::new_generic::DocsResponse;
use buck2_cli_proto::new_generic::DocsStarlarkRequest;
use buck2_common::dice::cells::HasCellResolver;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::CellAliasResolver;
use buck2_interpreter::load_module::InterpreterCalculation;
use buck2_interpreter::parse_import::parse_bzl_path_with_config;
use buck2_interpreter::parse_import::ParseImportOptions;
use buck2_interpreter::parse_import::RelativeImports;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use dice::DiceTransaction;
use futures::FutureExt;

use crate::json;
use crate::markdown::generate_markdown_files;

fn parse_import_paths(
    cell_resolver: &CellAliasResolver,
    current_dir: &CellPath,
    current_cell: BuildFileCell,
    symbol_patterns: &[String],
) -> anyhow::Result<HashSet<ImportPath>> {
    let parse_options = ParseImportOptions {
        allow_missing_at_symbol: true,
        relative_import_option: RelativeImports::Allow { current_dir },
    };

    symbol_patterns
        .iter()
        .map(|symbol_pattern| {
            parse_bzl_path_with_config(cell_resolver, symbol_pattern, &parse_options, current_cell)
        })
        .collect()
}

pub(crate) async fn docs_starlark(
    server_ctx: &dyn ServerCommandContextTrait,
    mut dice_ctx: DiceTransaction,
    request: &DocsStarlarkRequest,
) -> anyhow::Result<DocsResponse> {
    let cell_resolver = dice_ctx.get_cell_resolver().await?;
    let cwd = server_ctx.working_dir();
    let current_cell_path = cell_resolver.get_cell_path(cwd)?;
    let current_cell = BuildFileCell::new(current_cell_path.cell());
    let cell_alias_resolver = dice_ctx
        .get_cell_alias_resolver(current_cell_path.cell())
        .await?;

    let lookups = parse_import_paths(
        &cell_alias_resolver,
        &current_cell_path,
        current_cell,
        &request.symbol_patterns,
    )?;

    let docs: Vec<_> = dice_ctx
        .try_compute_join(lookups, |ctx, import_path| {
            async move {
                let doc = ctx
                    .get_loaded_module_from_import_path(&import_path)
                    .await?
                    .env()
                    .documentation();
                anyhow::Ok((import_path, doc))
            }
            .boxed()
        })
        .await?;

    let json_output = match &request.format {
        DocsOutputFormat::Json => Some(json::to_json(docs)?),
        DocsOutputFormat::Markdown(path) => {
            generate_markdown_files(&path, docs)?;
            None
        }
    };

    Ok(DocsResponse { json_output })
}
