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
use std::path::Path;
use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;
use buck2_cli_proto::unstable_docs_request;
use buck2_cli_proto::UnstableDocsRequest;
use buck2_cli_proto::UnstableDocsResponse;
use buck2_common::dice::cells::HasCellResolver;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::CellAliasResolver;
use buck2_core::fs::paths::abs_path::AbsPath;
use buck2_interpreter::load_module::InterpreterCalculation;
use buck2_interpreter::parse_import::parse_import_with_config;
use buck2_interpreter::parse_import::ParseImportOptions;
use buck2_interpreter::prelude_path::prelude_path;
use buck2_interpreter_for_build::interpreter::build_defs::starlark_library_extensions_for_buck2;
use buck2_interpreter_for_build::interpreter::global_interpreter_state::GlobalInterpreterState;
use buck2_interpreter_for_build::interpreter::global_interpreter_state::HasGlobalInterpreterState;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::NoPartialResult;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use dice::DiceComputations;
use dice::DiceTransaction;
use dupe::Dupe;
use starlark::collections::SmallMap;
use starlark::docs::get_registered_starlark_docs;
use starlark::docs::Doc;
use starlark::docs::DocItem;
use starlark::docs::DocMember;
use starlark::docs::DocModule;
use starlark::docs::DocProperty;
use starlark::docs::Identifier;
use starlark::docs::Location;
use starlark::environment::Globals;
use starlark::typing::Ty;

use super::bxl_docs::get_builtin_bxl_docs;
use crate::builtin_docs::markdown::generate_markdown_files;

#[derive(Debug, thiserror::Error)]
enum DocsError {
    #[error("Unknown format requested (internal error)")]
    UnknownFormat,
}

fn parse_import_paths(
    cell_resolver: &CellAliasResolver,
    current_dir: &CellPath,
    current_cell: BuildFileCell,
    symbol_patterns: &[String],
) -> anyhow::Result<HashSet<ImportPath>> {
    const PARSE_OPTIONS: ParseImportOptions = ParseImportOptions {
        allow_missing_at_symbol: true,
        allow_relative_imports: true,
    };

    symbol_patterns
        .iter()
        .map(|symbol_pattern| {
            let path = parse_import_with_config(
                cell_resolver,
                current_dir,
                symbol_pattern,
                &PARSE_OPTIONS,
            )?;
            ImportPath::new_with_build_file_cells(path, current_cell)
        })
        .collect()
}

pub(crate) fn builtin_doc<S: ToString>(name: S, directory: &str, item: DocItem) -> Doc {
    let mut custom_attrs = HashMap::new();
    if !directory.is_empty() {
        custom_attrs.insert("directory".to_owned(), directory.to_owned());
    }

    Doc {
        id: Identifier {
            name: name.to_string(),
            location: None,
        },
        item,
        custom_attrs,
    }
}

fn get_builtin_global_starlark_docs() -> Doc {
    let globals = Globals::extended_by(starlark_library_extensions_for_buck2());
    builtin_doc(
        "globals",
        "standard",
        DocItem::Module(globals.documentation()),
    )
}

/// Globals that are in the interpreter, but none of the starlark global symbols.
fn get_builtin_build_docs(interpreter_state: Arc<GlobalInterpreterState>) -> anyhow::Result<Doc> {
    let mut b_o = interpreter_state.extension_file_global_env.documentation();
    let globals = Globals::extended_by(starlark_library_extensions_for_buck2());
    let global_symbols: HashSet<_> = globals.names().map(|s| s.as_str()).collect();
    b_o.members = b_o
        .members
        .into_iter()
        .filter(|(name, _)| !global_symbols.contains(&name.as_str()))
        .collect();
    Ok(builtin_doc("globals", "", DocItem::Module(b_o)))
}

pub fn get_builtin_docs(
    interpreter_state: Arc<GlobalInterpreterState>,
) -> anyhow::Result<Vec<Doc>> {
    let mut all_builtins = vec![
        get_builtin_global_starlark_docs(),
        get_builtin_build_docs(interpreter_state.dupe())?,
    ];

    all_builtins.extend(get_builtin_bxl_docs(interpreter_state)?);
    all_builtins.extend(get_registered_starlark_docs());

    Ok(all_builtins)
}

/// Get the documentation for exported symbols in the prelude
///
/// Creates top level docs for member functions of "native" too,
/// presuming that those symbols don't already exist in `existing_globals`
/// (to avoid re-exporting and overriding the real builtins if there is conflict)
pub async fn get_prelude_docs(
    ctx: &DiceTransaction,
    existing_globals: &HashSet<&str>,
) -> anyhow::Result<Vec<Doc>> {
    let cell_resolver = ctx.get_cell_resolver().await?;
    let prelude_path = prelude_path(&cell_resolver)?;
    get_docs_from_module(ctx, prelude_path.import_path(), Some(existing_globals)).await
}

async fn get_docs_from_module(
    ctx: &DiceComputations,
    import_path: &ImportPath,
    // If we want to promote `native`, what should we exclude
    promote_native: Option<&HashSet<&str>>,
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
    let mut module_docs = frozen_module.documentation();

    // For the prelude, we want to promote `native` symbol up one level
    if let Some(existing_globals) = promote_native {
        for (name, value) in module.extra_globals_from_prelude_for_buck_files()? {
            if !existing_globals.contains(&name) && !module_docs.members.contains_key(name) {
                let doc = match value.to_value().documentation() {
                    Some(DocItem::Function(f)) => DocMember::Function(f),
                    _ => DocMember::Property(DocProperty {
                        docs: None,
                        typ: Ty::any(),
                    }),
                };

                module_docs.members.insert(name.to_owned(), doc);
            }
        }
    }

    let mut docs = vec![];

    if let Some(module_doc) = module_docs.docs {
        docs.push(Doc {
            id: Identifier {
                name: import_path_string.clone(),
                location: Some(starlark::docs::Location {
                    path: import_path_string.clone(),
                    position: None,
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
                    position: None,
                }),
            },
            item: d.to_doc_item(),
            custom_attrs: Default::default(),
        }
    }));

    Ok(docs)
}

pub async fn docs_command(
    context: &dyn ServerCommandContextTrait,
    partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
    req: UnstableDocsRequest,
) -> anyhow::Result<UnstableDocsResponse> {
    run_server_command(
        DocsServerCommand { req },
        context,
        partial_result_dispatcher,
    )
    .await
}

struct DocsServerCommand {
    req: UnstableDocsRequest,
}

#[async_trait]
impl ServerCommandTemplate for DocsServerCommand {
    type StartEvent = buck2_data::DocsCommandStart;
    type EndEvent = buck2_data::DocsCommandEnd;
    type Response = UnstableDocsResponse;
    type PartialResult = NoPartialResult;

    async fn command(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        _partial_result_dispatcher: PartialResultDispatcher<Self::PartialResult>,
        ctx: DiceTransaction,
    ) -> anyhow::Result<Self::Response> {
        docs(server_ctx, ctx, &self.req).await
    }

    fn is_success(&self, _response: &Self::Response) -> bool {
        // No response if we failed.
        true
    }
}

enum Format {
    Json,
    Markdown,
}

impl Format {
    fn from_proto(request: &UnstableDocsRequest) -> anyhow::Result<Format> {
        let format = unstable_docs_request::Format::from_i32(request.format)
            .context("incorrect enum value")?;
        match format {
            unstable_docs_request::Format::Json => Ok(Format::Json),
            unstable_docs_request::Format::Markdown => Ok(Format::Markdown),
            unstable_docs_request::Format::Unknown => Err(DocsError::UnknownFormat.into()),
        }
    }
}

async fn docs(
    server_ctx: &dyn ServerCommandContextTrait,
    dice_ctx: DiceTransaction,
    request: &UnstableDocsRequest,
) -> anyhow::Result<UnstableDocsResponse> {
    let format = Format::from_proto(request)?;

    let cell_resolver = dice_ctx.get_cell_resolver().await?;
    let current_cell_path = cell_resolver.get_cell_path(server_ctx.working_dir())?;
    let current_cell = BuildFileCell::new(current_cell_path.cell());

    let cell_alias_resolver = cell_resolver
        .get(current_cell_path.cell())?
        .cell_alias_resolver();

    let lookups = parse_import_paths(
        cell_alias_resolver,
        &current_cell_path,
        current_cell,
        &request.symbol_patterns,
    )?;

    let mut docs = if request.retrieve_builtins {
        get_builtin_docs(dice_ctx.get_global_interpreter_state().await?.dupe())?
    } else {
        vec![]
    };

    if request.retrieve_prelude {
        let builtin_names = docs.iter().map(|d| d.id.name.as_str()).collect();
        let prelude_docs = get_prelude_docs(&dice_ctx, &builtin_names).await?;
        docs.extend(prelude_docs);
    }

    let module_calcs: Vec<_> = lookups
        .iter()
        .map(|import_path| get_docs_from_module(&dice_ctx, import_path, None))
        .collect();

    let modules_docs = futures::future::try_join_all(module_calcs).await?;
    docs.extend(modules_docs.into_iter().flatten());

    let json_output = match format {
        Format::Json => Some(serde_json::to_string(&docs)?),
        Format::Markdown => {
            let path = AbsPath::new(Path::new(request.markdown_output_path.as_ref().context(
                "`markdown_output_path` must be set when requesting markdown (internal error)",
            )?))?;
            let starlark_subdir = Path::new(&request.markdown_starlark_subdir);
            let native_subdir = Path::new(&request.markdown_native_subdir);
            generate_markdown_files(path, starlark_subdir, native_subdir, docs)?;
            None
        }
    };

    Ok(UnstableDocsResponse { json_output })
}
