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
use std::sync::Arc;

use async_trait::async_trait;
use buck2_build_api::interpreter::context::prelude_path;
use buck2_cli_proto::unstable_docs_response;
use buck2_cli_proto::UnstableDocsRequest;
use buck2_cli_proto::UnstableDocsResponse;
use buck2_common::dice::cells::HasCellResolver;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::CellAliasResolver;
use buck2_interpreter::parse_import::parse_import_with_config;
use buck2_interpreter::parse_import::ParseImportOptions;
use buck2_interpreter_for_build::interpreter::calculation::InterpreterCalculation;
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
use gazebo::prelude::VecExt;
use starlark::collections::SmallMap;
use starlark::docs::get_registered_starlark_docs;
use starlark::docs::Doc;
use starlark::docs::DocItem;
use starlark::docs::DocModule;
use starlark::docs::Identifier;
use starlark::docs::Location;
use starlark::environment::Globals;

use super::bxl_docs::get_builtin_bxl_docs;

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
            ImportPath::new(path, current_cell)
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
    let globals = Globals::extended();
    builtin_doc("globals", "standard", globals.documentation())
}

/// Globals that are in the interpreter, but none of the starlark global symbols.
fn get_builtin_build_docs(interpreter_state: Arc<GlobalInterpreterState>) -> anyhow::Result<Doc> {
    let cleaned_build = match interpreter_state.extension_file_global_env.documentation() {
        DocItem::Module(mut b_o) => {
            let globals = Globals::extended();
            let global_symbols: HashSet<_> = globals.names().map(|s| s.as_str()).collect();
            b_o.members = b_o
                .members
                .into_iter()
                .filter(|(name, _)| !global_symbols.contains(&name.as_str()))
                .collect();
            DocItem::Module(b_o)
        }
        item => item,
    };
    Ok(builtin_doc("globals", "", cleaned_build))
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
    let cell_alias_resolver = cell_resolver.root_cell_instance().cell_alias_resolver();
    let prelude_path = prelude_path(cell_alias_resolver)?;
    get_docs_from_module(ctx, prelude_path, Some(existing_globals)).await
}

async fn get_docs_from_module(
    ctx: &DiceComputations,
    import_path: ImportPath,
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
    let module = ctx.get_loaded_module_from_import_path(&import_path).await?;
    let frozen_module = module.env();
    let mut module_docs = frozen_module.documentation();

    // For the prelude, we want to promote `native` symbol up one level
    if let Some(existing_globals) = promote_native {
        if let Some(native) = frozen_module.get_option("native")? {
            if let Some(DocItem::Object(native)) = native.value().documentation() {
                for (k, v) in native.members {
                    if !existing_globals.contains(k.as_str())
                        && !module_docs.members.contains_key(&k)
                    {
                        module_docs.members.insert(k, v);
                    }
                }
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

async fn docs(
    server_ctx: &dyn ServerCommandContextTrait,
    dice_ctx: DiceTransaction,
    request: &UnstableDocsRequest,
) -> anyhow::Result<UnstableDocsResponse> {
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
        .into_iter()
        .map(|import_path| async { get_docs_from_module(&dice_ctx, import_path, None).await })
        .collect();

    let modules_docs = futures::future::try_join_all(module_calcs).await?;
    docs.extend(modules_docs.into_iter().flatten());

    let docs = docs.into_try_map(|doc| {
        anyhow::Ok(unstable_docs_response::DocItem {
            json: serde_json::to_string(&doc)?,
        })
    })?;

    Ok(UnstableDocsResponse { docs })
}
