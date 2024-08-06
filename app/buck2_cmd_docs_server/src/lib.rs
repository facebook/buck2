/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(error_generic_member_access)]

use std::collections::HashMap;
use std::collections::HashSet;
use std::path::Path;
use std::sync::Arc;

use async_trait::async_trait;
use buck2_cli_proto::new_generic::DocsOutputFormat;
use buck2_cli_proto::new_generic::DocsRequest;
use buck2_cli_proto::new_generic::DocsResponse;
use buck2_common::dice::cells::HasCellResolver;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::CellAliasResolver;
use buck2_interpreter::load_module::InterpreterCalculation;
use buck2_interpreter::parse_import::parse_bzl_path_with_config;
use buck2_interpreter::parse_import::ParseImportOptions;
use buck2_interpreter::parse_import::RelativeImports;
use buck2_interpreter_for_build::interpreter::global_interpreter_state::GlobalInterpreterState;
use buck2_interpreter_for_build::interpreter::global_interpreter_state::HasGlobalInterpreterState;
use buck2_interpreter_for_build::interpreter::globals::starlark_library_extensions_for_buck2;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::late_bindings::DocsServerComamnd;
use buck2_server_ctx::late_bindings::DOCS_SERVER_COMMAND;
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
use starlark::docs::DocModule;
use starlark::docs::Identifier;
use starlark::docs::Location;
use starlark::environment::Globals;

use crate::markdown::generate_markdown_files;

mod json;
mod markdown;

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

fn builtin_doc<S: ToString>(name: S, directory: &str, module: DocModule) -> Doc {
    let mut custom_attrs = HashMap::new();
    if !directory.is_empty() {
        custom_attrs.insert("directory".to_owned(), directory.to_owned());
    }

    Doc {
        id: Identifier {
            name: name.to_string(),
            location: None,
        },
        item: DocItem::Module(module),
        custom_attrs,
    }
}

fn get_builtin_global_starlark_docs() -> DocModule {
    Globals::extended_by(starlark_library_extensions_for_buck2()).documentation()
}

/// Globals that are in the interpreter (including BXL), but none of the starlark global symbols.
fn get_builtin_build_docs(
    interpreter_state: Arc<GlobalInterpreterState>,
) -> anyhow::Result<DocModule> {
    let mut b_o = interpreter_state.global_env.documentation();
    let globals = Globals::extended_by(starlark_library_extensions_for_buck2());
    let global_symbols: HashSet<_> = globals.names().map(|s| s.as_str()).collect();
    b_o.members = b_o
        .members
        .into_iter()
        .filter(|(name, _)| !global_symbols.contains(&name.as_str()))
        .collect();
    Ok(b_o)
}

fn get_builtin_docs(interpreter_state: Arc<GlobalInterpreterState>) -> anyhow::Result<Vec<Doc>> {
    let mut all_builtins = vec![
        builtin_doc("globals", "standard", get_builtin_global_starlark_docs()),
        builtin_doc("globals", "", get_builtin_build_docs(interpreter_state)?),
    ];

    all_builtins.extend(get_registered_starlark_docs());

    Ok(all_builtins)
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
            item: d.to_doc_item(),
            custom_attrs: Default::default(),
        }
    }));

    Ok(docs)
}

struct DocsServerCommandImpl;

#[async_trait::async_trait]
impl DocsServerComamnd for DocsServerCommandImpl {
    async fn docs(
        &self,
        context: &dyn ServerCommandContextTrait,
        partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
        req: DocsRequest,
    ) -> anyhow::Result<DocsResponse> {
        run_server_command(
            DocsServerCommand { req },
            context,
            partial_result_dispatcher,
        )
        .await
    }
}

pub fn init_late_bindings() {
    DOCS_SERVER_COMMAND.init(&DocsServerCommandImpl);
}

struct DocsServerCommand {
    req: DocsRequest,
}

#[async_trait]
impl ServerCommandTemplate for DocsServerCommand {
    type StartEvent = buck2_data::DocsCommandStart;
    type EndEvent = buck2_data::DocsCommandEnd;
    type Response = DocsResponse;
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
    mut dice_ctx: DiceTransaction,
    request: &DocsRequest,
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
        get_builtin_docs(dice_ctx.get_global_interpreter_state().await?.dupe())?
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
