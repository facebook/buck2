/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;

use buck2_cli_proto::new_generic::DocsResponse;
use buck2_cli_proto::new_generic::DocsStarlarkBuiltinsRequest;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_path::AbsPathBuf;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_error::internal_error_anyhow;
use buck2_error::BuckErrorContext;
use buck2_interpreter_for_build::interpreter::globals::register_analysis_natives;
use buck2_interpreter_for_build::interpreter::globals::register_bxl_natives;
use buck2_interpreter_for_build::interpreter::globals::register_load_natives;
use buck2_interpreter_for_build::interpreter::globals::starlark_library_extensions_for_buck2;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use dice::DiceTransaction;
use starlark::docs::multipage::render_markdown_multipage;
use starlark::docs::multipage::DocModuleInfo;
use starlark::docs::DocItem;
use starlark::environment::Globals;
use starlark::environment::GlobalsBuilder;

fn write_docs_to_subdir(
    modules_infos: Vec<DocModuleInfo<'_>>,
    base_path: &str,
) -> anyhow::Result<()> {
    let base_path = AbsPathBuf::new(base_path)?;
    let path_mapper = |p: &str| format!("/docs/api/{}", p);
    let mut docs: BTreeMap<_, _> = render_markdown_multipage(modules_infos, Some(&path_mapper))
        .into_iter()
        .collect();
    while let Some((mut doc_path, rendered)) = docs.pop_first() {
        let mut path = base_path.clone();
        // Map:
        // - "" -> "index.md"
        // - "bxl" -> "bxl/index.md"
        // - "bxl/typename" -> "bxl/typename.md"
        if doc_path.is_empty()
            || docs
                .first_key_value()
                .is_some_and(|(k, _)| k.starts_with(&format!("{}/", doc_path)))
        {
            path.push(
                ForwardRelativePath::new(&doc_path)
                    .internal_error("Doc paths should be forward relative")?
                    .as_path(),
            );
            path.push(ForwardRelativePath::new("index.md").unwrap().as_path());
        } else {
            doc_path.push_str(".md");
            path.push(
                ForwardRelativePath::new(&doc_path)
                    .internal_error("Doc paths should be forward relative")?
                    .as_path(),
            );
        }

        if let Some(parent) = path.parent() {
            fs_util::create_dir_all(parent)?;
        }
        fs_util::write(path, &rendered)?;
    }

    Ok(())
}

pub(crate) async fn docs_starlark_builtins(
    _server_ctx: &dyn ServerCommandContextTrait,
    _dice_ctx: DiceTransaction,
    request: &DocsStarlarkBuiltinsRequest,
) -> anyhow::Result<DocsResponse> {
    let starlark = Globals::extended_by(starlark_library_extensions_for_buck2()).documentation();

    let build = GlobalsBuilder::new()
        .with(register_load_natives)
        .with(register_analysis_natives)
        .build()
        .documentation();

    let mut bxl = GlobalsBuilder::new()
        .with(register_bxl_natives)
        .build()
        .documentation();

    let Some(DocItem::Module(bxl)) = bxl.members.shift_remove("bxl") else {
        return Err(internal_error_anyhow!("bxl namespace should exist"));
    };

    let modules_infos = vec![
        DocModuleInfo {
            module: &starlark,
            name: "Starlark APIs".to_owned(),
            page_path: "starlark".to_owned(),
        },
        DocModuleInfo {
            module: &build,
            name: "Build APIs".to_owned(),
            page_path: "build".to_owned(),
        },
        DocModuleInfo {
            module: &bxl,
            name: "Bxl APIs".to_owned(),
            page_path: "bxl".to_owned(),
        },
    ];

    write_docs_to_subdir(modules_infos, &request.path)?;

    Ok(DocsResponse { json_output: None })
}
