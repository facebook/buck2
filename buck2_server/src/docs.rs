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

use buck2_build_api::actions::artifact::Artifact;
use buck2_build_api::actions::artifact::SourceArtifact;
use buck2_build_api::interpreter::context::prelude_path;
use buck2_build_api::interpreter::rule_defs::artifact::StarlarkArtifact;
use buck2_build_api::interpreter::rule_defs::context::AnalysisContext;
use buck2_build_api::interpreter::rule_defs::provider::callable::ProviderCallable;
use buck2_common::dice::cells::HasCellResolver;
use buck2_core::buck_path::BuckPath;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::cells::CellAliasResolver;
use buck2_core::cells::CellName;
use buck2_core::package::package_relative_path::PackageRelativePathBuf;
use buck2_core::package::Package;
use buck2_interpreter::common::StarlarkModulePath;
use buck2_interpreter::dice::calculation::DiceCalculationDelegate;
use buck2_interpreter::dice::HasCalculationDelegate;
use buck2_interpreter::dice::HasGlobalInterpreterState;
use buck2_interpreter::interpreter::GlobalInterpreterState;
use buck2_interpreter::interpreter::InterpreterConfigForCell;
use buck2_interpreter::parse_import::parse_import_with_config;
use buck2_interpreter::parse_import::ParseImportOptions;
use cli_proto::UnstableDocsRequest;
use cli_proto::UnstableDocsResponse;
use dice::DiceTransaction;
use gazebo::prelude::*;
use starlark::environment::Globals;
use starlark::values::docs::get_registered_docs;
use starlark::values::docs::Doc;
use starlark::values::docs::DocItem;
use starlark::values::docs::Identifier;
use starlark::values::docs::Member;
use starlark::values::StarlarkValue;

use crate::ctx::ServerCommandContext;

fn parse_import_paths(
    cell_resolver: &CellAliasResolver,
    current_dir: &CellPath,
    current_cell: &BuildFileCell,
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
            ImportPath::new(path, current_cell.clone())
        })
        .collect()
}

fn builtin_doc<S: ToString>(name: S, directory: &str, item: DocItem) -> Doc {
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
    builtin_doc("builtins", "", globals.documentation())
}

fn get_builtin_provider_docs() -> Vec<Doc> {
    ProviderCallable::builtin_provider_documentation()
        .into_iter()
        .filter_map(|(name, docs)| docs.map(|item| builtin_doc(name, "providers", item)))
        .collect()
}

/// Globals that are in the interpreter, but none of the starlark global symbols.
fn get_builtin_build_docs(
    cell_alias_resolver: CellAliasResolver,
    interpreter_state: Arc<GlobalInterpreterState>,
) -> anyhow::Result<Doc> {
    let globals = Globals::extended();
    let interpreter_config = InterpreterConfigForCell::new(cell_alias_resolver, interpreter_state)?;
    let cleaned_build = match interpreter_config
        .extension_file_global_env()
        .documentation()
    {
        DocItem::Object(mut b_o) => {
            let global_symbols: HashSet<_> = globals.names().map(|s| s.as_str()).collect();
            b_o.members = b_o
                .members
                .into_iter()
                .filter(|(name, _)| !global_symbols.contains(&name.as_str()))
                .collect();
            DocItem::Object(b_o)
        }
        item => item,
    };
    Ok(builtin_doc("build", "", cleaned_build))
}

fn get_artifact_docs() -> Option<Doc> {
    let pkg = Package::new(
        &CellName::unchecked_new("".to_owned()),
        CellRelativePath::unchecked_new("__native__"),
    );

    // Artifact isn't really exported into globals anywhere, so instantiate it.
    let artifact = StarlarkArtifact::new(Artifact::from(SourceArtifact::new(BuckPath::new(
        pkg,
        PackageRelativePathBuf::unchecked_new("__fake_path__".to_owned()),
    ))));
    artifact
        .documentation()
        .map(|artifact_docs| builtin_doc("Artifact", "", artifact_docs))
}

fn get_ctx_docs() -> Vec<Doc> {
    let mut docs = vec![];
    // Grab the 'ctx', and 'ctx.actions' structs from analysis
    let ctx = AnalysisContext::ctx_documentation();
    if let Some(ctx_docs) = ctx.context {
        docs.push(builtin_doc("ctx", "", ctx_docs));
    }
    if let Some(actions_docs) = ctx.actions {
        docs.push(builtin_doc("ctx.actions", "", actions_docs));
    }
    docs
}

pub fn get_builtin_docs(
    cell_alias_resolver: CellAliasResolver,
    interpreter_state: Arc<GlobalInterpreterState>,
) -> anyhow::Result<Vec<Doc>> {
    let mut all_builtins = vec![
        get_builtin_global_starlark_docs(),
        get_builtin_build_docs(cell_alias_resolver, interpreter_state)?,
    ];

    all_builtins.extend(get_builtin_provider_docs());
    if let Some(artifact) = get_artifact_docs() {
        all_builtins.push(artifact);
    }
    all_builtins.extend(get_ctx_docs());

    all_builtins.extend(get_registered_docs());

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
    let cells = ctx.get_cell_resolver().await?;
    let prelude_path = prelude_path(&cells);
    let interpreter_calculation = ctx
        .get_interpreter_calculator(prelude_path.cell(), prelude_path.build_file_cell())
        .await?;
    let mut prelude_docs = get_docs_from_module(&interpreter_calculation, prelude_path).await?;

    let mut native_reexported = vec![];
    let top_level_symbols = prelude_docs
        .iter()
        .map(|d| (d.id.name.as_str(), d))
        .collect::<HashMap<_, _>>();

    // All members of "native" are also available at the global scope. However, make sure
    // that we don't overwrite already existing symbols with potentially less useful
    // documentation when doing this.
    if let Some(Doc {
        id,
        item: DocItem::Object(o),
        custom_attrs,
    }) = top_level_symbols.get("native")
    {
        // At the moment there is no top level documentation for a stand alone value, only
        // functions, objects, and module docs appear at the top level.
        for (name, member_doc) in &o.members {
            let found_top_level = top_level_symbols.contains_key(name.as_str());
            let found_existing = existing_globals.contains(name.as_str());
            if !found_top_level && !found_existing {
                if let Member::Function(f) = member_doc {
                    native_reexported.push(Doc {
                        id: Identifier {
                            name: name.clone(),
                            ..id.clone()
                        },
                        item: DocItem::Function(f.clone()),
                        custom_attrs: custom_attrs.clone(),
                    });
                }
            }
        }
    }
    prelude_docs.extend(native_reexported);
    Ok(prelude_docs)
}

async fn get_docs_from_module(
    interpreter_calc: &DiceCalculationDelegate<'_>,
    import_path: ImportPath,
) -> anyhow::Result<Vec<Doc>> {
    // Do this so that we don't get the '@' in the display if we're printing targets from a
    // different cell root. i.e. `//foo:bar.bzl`, rather than `//foo:bar.bzl @ fbsource`
    let import_path_string = format!(
        "{}:{}",
        import_path.path().parent().unwrap(),
        import_path.path().path().file_name().unwrap()
    );
    let module = interpreter_calc
        .eval_module(StarlarkModulePath::LoadFile(&import_path))
        .await?;
    let frozen_module = module.env();
    let module_docs = frozen_module.module_documentation();

    let mut docs = vec![];

    if let Some(module_doc) = module_docs.module {
        docs.push(Doc {
            id: Identifier {
                name: import_path_string.clone(),
                location: Some(starlark::values::docs::Location {
                    path: import_path_string.clone(),
                    position: None,
                }),
            },
            item: module_doc,
            custom_attrs: Default::default(),
        });
    }
    docs.extend(module_docs.members.into_iter().filter_map(|(symbol, d)| {
        d.map(|doc_item| {
            Doc {
                // TODO(nmj): Map this back into the codemap to get a line/column
                id: Identifier {
                    name: symbol,
                    location: Some(starlark::values::docs::Location {
                        path: import_path_string.clone(),
                        position: None,
                    }),
                },
                item: doc_item,
                custom_attrs: Default::default(),
            }
        })
    }));

    Ok(docs)
}

pub async fn docs(
    server_ctx: ServerCommandContext,
    request: UnstableDocsRequest,
) -> anyhow::Result<UnstableDocsResponse> {
    let dice_ctx = server_ctx.dice_ctx().await?;
    let cell_resolver = dice_ctx.get_cell_resolver().await?;
    let current_cell_path = cell_resolver.get_cell_path(&server_ctx.working_dir)?;
    let current_cell = BuildFileCell::new(current_cell_path.cell().clone());

    let cell_alias_resolver = cell_resolver
        .get(current_cell_path.cell())?
        .cell_alias_resolver();

    let lookups = parse_import_paths(
        cell_alias_resolver,
        &current_cell_path,
        &current_cell,
        &request.symbol_patterns,
    )?;

    let mut docs = if request.retrieve_builtins {
        get_builtin_docs(
            cell_alias_resolver.dupe(),
            dice_ctx.get_global_interpreter_state().await?.dupe(),
        )?
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
        .map(|import_path| async {
            let interpreter_calc = dice_ctx
                .get_interpreter_calculator(import_path.cell(), import_path.build_file_cell())
                .await?;
            get_docs_from_module(&interpreter_calc, import_path).await
        })
        .collect();

    let modules_docs = futures::future::try_join_all(module_calcs).await?;
    docs.extend(modules_docs.into_iter().flatten());

    let json = serde_json::to_string(&docs)?;
    Ok(UnstableDocsResponse { docs_json: json })
}
