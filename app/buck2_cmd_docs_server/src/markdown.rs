/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;

use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_path::AbsPath;
use buck2_core::fs::paths::abs_path::AbsPathBuf;
use buck2_core::fs::paths::file_name::FileName;
use buck2_events::dispatch::console_message;
use starlark::docs::markdown::render_doc_item;
use starlark::docs::DocItem;
use starlark::docs::DocModule;
use starlark::typing::TypeRenderConfig;

use crate::starlark_::StarlarkFilePath;

fn add_md(mut p: AbsPathBuf) -> AbsPathBuf {
    let mut file = p.file_name().unwrap().to_owned();
    file.push(".md");
    p.pop();
    p.push(file);
    p
}

pub(crate) fn generate_markdown_files(
    output_dir: &AbsPath,
    docs: Vec<(StarlarkFilePath, DocModule)>,
) -> buck2_error::Result<()> {
    let mut outputs = HashMap::new();

    for (path, docs) in docs {
        let rendered = render_doc_item(
            &path.to_string(),
            &DocItem::Module(docs),
            &TypeRenderConfig::Default,
        );
        let cell = FileName::new(path.cell().as_str())?;
        let path = output_dir
            .join(cell)
            .join(path.path().path().as_forward_relative_path().as_path());
        outputs.insert(add_md(path), rendered);
    }

    for (path, contents) in outputs.iter() {
        console_message(format!("Writing to {}", path.display()));

        if let Some(dir) = path.parent() {
            fs_util::create_dir_all(dir)?;
        }
        fs_util::write(path, contents)?;
    }

    Ok(())
}
