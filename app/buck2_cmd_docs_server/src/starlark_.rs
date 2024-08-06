/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashSet;
use std::path::Path;

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
use dice::DiceComputations;
use dice::DiceTransaction;
use starlark::collections::SmallMap;
use starlark::docs::Doc;
use starlark::docs::DocItem;
use starlark::docs::DocMember;
use starlark::docs::DocModule;
use starlark::docs::Identifier;
use starlark::docs::Location;

use crate::get_builtin_docs;
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

async fn get_docs_from_module(
    ctx: &mut DiceComputations<'_>,
    import_path: &ImportPath,
) -> anyhow::Result<Vec<Doc>> {
    // Do this so that we don't get the '@' in the display if we're printing targets from a
    // different cell root. i.e. `//foo:bar.bzl`, rather than `//foo:bar.bzl @ cell`
    let import_path_string = format!(
        "{}:{}",
        import_path.path().parent().unwrap(),
        import_path.path().path().file_name().unwrap()
    );
    let module = ctx.get_loaded_module_from_import_path(import_path).await?;
    let frozen_module = module.env();
    let module_docs = frozen_module.documentation();

    let mut docs = vec![];

    if let Some(module_doc) = module_docs.docs {
        docs.push(Doc {
            id: Identifier {
                name: import_path_string.clone(),
                location: Some(starlark::docs::Location {
                    path: import_path_string.clone(),
                }),
            },
            item: DocItem::Module(DocModule {
                docs: Some(module_doc),
                members: SmallMap::new(),
            }),
            custom_attrs: Default::default(),
        });
    }
    docs.extend(module_docs.members.into_iter().map(|(symbol, d)| {
        Doc {
            // TODO(nmj): Map this back into the codemap to get a line/column
            id: Identifier {
                name: symbol,
                location: Some(Location {
                    path: import_path_string.clone(),
                }),
            },
            item: match d {
                DocMember::Function(f) => DocItem::Function(f),
                DocMember::Property(p) => DocItem::Property(p),
            },
            custom_attrs: Default::default(),
        }
    }));

    Ok(docs)
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

    let mut docs = if request.retrieve_builtins {
        get_builtin_docs()
    } else {
        vec![]
    };

    let dice_ref = &dice_ctx;
    let module_calcs: Vec<_> =
        lookups
            .iter()
            .map(|import_path| async move {
                get_docs_from_module(&mut dice_ref.clone(), import_path).await
            })
            .collect();

    let modules_docs = buck2_util::future::try_join_all(module_calcs).await?;
    docs.extend(modules_docs.into_iter().flatten());

    let json_output = match &request.format {
        DocsOutputFormat::Json => Some(json::to_json(docs)?),
        DocsOutputFormat::Markdown(path) => {
            let starlark_subdir = Path::new(&request.markdown_starlark_subdir);
            let native_subdir = Path::new(&request.markdown_native_subdir);
            generate_markdown_files(&path, starlark_subdir, native_subdir, docs)?;
            None
        }
    };

    Ok(DocsResponse { json_output })
}
