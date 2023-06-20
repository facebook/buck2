/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cmp::Ordering;
use std::collections::HashMap;
use std::path::Path;
use std::path::PathBuf;

use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_path::AbsPath;
use buck2_events::dispatch::console_message;
use itertools::Itertools;
use starlark::docs::Doc;
use starlark::docs::DocItem;
use starlark::docs::MarkdownFlavor;
use starlark::docs::RenderMarkdown;

use crate::lsp::output_subdir_for_doc;

#[derive(Clone, Debug, Default)]
struct MarkdownOutput {
    sections: Vec<String>,
}

impl MarkdownOutput {
    fn write_to_file(&self, path: &AbsPath) -> anyhow::Result<String> {
        let mut contents = self.sections.join("\n\n---\n");
        if !contents.ends_with('\n') {
            contents.push('\n');
        }
        if let Some(p) = path.parent() {
            fs_util::create_dir_all(p)?;
        }
        fs_util::write(path, &contents)?;

        Ok(contents)
    }

    /// Convert a buck style path (foo//bar:baz.bzl) to path, failing if someone attempts to traverse upward
    fn path_from_location(location: &str) -> anyhow::Result<PathBuf> {
        Ok(Path::new(&location.replace("//", "/").replace(':', "/")).to_path_buf())
    }

    /// Get the output path for the markdown for a given [`Doc`], whether it's in a starlark file, or a native symbol.
    fn markdown_path_for_doc(
        starlark_subdir: &Path,
        native_subdir: &Path,
        doc: &Doc,
    ) -> anyhow::Result<PathBuf> {
        let subdir = output_subdir_for_doc(doc)?;
        let path = match &doc.id.location {
            Some(loc) => starlark_subdir
                .join(subdir.as_path())
                .join(Self::path_from_location(&loc.path)?),
            None => match &doc.item {
                // Functions all go in one file.
                // Objects get their on file (e.g. each provider, Artifact, etc)
                DocItem::Function(_) | DocItem::Property(_) => {
                    native_subdir.join(subdir.as_path()).join("native")
                }
                DocItem::Module(_) | DocItem::Object(_) => {
                    native_subdir.join(subdir.as_path()).join(&doc.id.name)
                }
            },
        };
        let path = path.with_extension(match path.extension() {
            None => "md".to_owned(),
            Some(e) => format!("{}.md", e.to_str().expect("path if not UTF-8")),
        });
        Ok(path)
    }
}

/// Does the heavy work of processing the docs and writing them to markdown files.
pub(crate) fn generate_markdown_files(
    destination_dir: &AbsPath,
    starlark_subdir: &Path,
    native_subdir: &Path,
    docs: Vec<Doc>,
) -> anyhow::Result<()> {
    let mut outputs = HashMap::new();

    fn item_ordering(l: &Doc, r: &Doc) -> Ordering {
        match (&l.item, &r.item) {
            (DocItem::Module(_), DocItem::Module(_)) => l.id.name.cmp(&r.id.name),
            (DocItem::Module(_), _) => Ordering::Less,
            (_, DocItem::Module(_)) => Ordering::Greater,
            _ => l.id.name.cmp(&r.id.name),
        }
    }

    for doc in docs.into_iter().sorted_by(item_ordering) {
        let markdown_path =
            MarkdownOutput::markdown_path_for_doc(starlark_subdir, native_subdir, &doc)?;
        let markdown_file = outputs
            .entry(markdown_path)
            .or_insert_with(MarkdownOutput::default);
        if let Some(docs) = doc.render_markdown_opt(MarkdownFlavor::DocFile) {
            markdown_file.sections.push(docs);
        }
    }

    for (relative_path, markdown_file) in outputs.iter() {
        let path = destination_dir.join(relative_path);
        console_message(format!("Writing to {}", path.display()));

        markdown_file.write_to_file(&path)?;
    }

    Ok(())
}
