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

use buck2_core::bzl::ImportPath;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_path::AbsPath;
use buck2_events::dispatch::console_message;
use itertools::Itertools;
use starlark::collections::SmallMap;
use starlark::docs::markdown::render_doc_item;
use starlark::docs::DocItem;
use starlark::docs::DocMember;
use starlark::docs::DocModule;

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
    fn markdown_path_for_doc(starlark_subdir: &Path, doc: &Doc) -> anyhow::Result<PathBuf> {
        let path = starlark_subdir.join(Self::path_from_location(&doc.location)?);
        let path = path.with_extension(match path.extension() {
            None => "md".to_owned(),
            Some(e) => format!("{}.md", e.to_str().expect("path if not UTF-8")),
        });
        Ok(path)
    }
}

struct Doc {
    name: String,
    location: String,
    item: DocItem,
}

fn to_docs_list(import_path: &ImportPath, module_docs: DocModule) -> Vec<Doc> {
    // Do this so that we don't get the '@' in the display if we're printing targets from a
    // different cell root. i.e. `//foo:bar.bzl`, rather than `//foo:bar.bzl @ cell`
    let import_path_string = format!(
        "{}:{}",
        import_path.path().parent().unwrap(),
        import_path.path().path().file_name().unwrap()
    );

    let mut docs = vec![];

    if let Some(module_doc) = module_docs.docs {
        docs.push(Doc {
            name: import_path_string.clone(),
            location: import_path_string.clone(),
            item: DocItem::Module(DocModule {
                docs: Some(module_doc),
                members: SmallMap::new(),
            }),
        });
    }
    docs.extend(module_docs.members.into_iter().filter_map(|(symbol, d)| {
        Some(Doc {
            name: symbol,
            location: import_path_string.clone(),
            item: match d.try_as_member_with_collapsed_object().ok()? {
                DocMember::Function(f) => DocItem::Member(DocMember::Function(f)),
                DocMember::Property(p) => DocItem::Member(DocMember::Property(p)),
            },
        })
    }));

    docs
}

/// Does the heavy work of processing the docs and writing them to markdown files.
pub(crate) fn generate_markdown_files(
    destination_dir: &AbsPath,
    starlark_subdir: &Path,
    docs: Vec<(ImportPath, DocModule)>,
) -> anyhow::Result<()> {
    let mut outputs = HashMap::new();

    fn item_ordering(l: &Doc, r: &Doc) -> Ordering {
        match (&l.item, &r.item) {
            (DocItem::Module(_), DocItem::Module(_)) => l.name.cmp(&r.name),
            (DocItem::Module(_), _) => Ordering::Less,
            (_, DocItem::Module(_)) => Ordering::Greater,
            _ => l.name.cmp(&r.name),
        }
    }

    for doc in docs
        .into_iter()
        .flat_map(|(p, d)| to_docs_list(&p, d))
        .sorted_by(item_ordering)
    {
        let markdown_path = MarkdownOutput::markdown_path_for_doc(starlark_subdir, &doc)?;
        let markdown_file = outputs
            .entry(markdown_path)
            .or_insert_with(MarkdownOutput::default);
        markdown_file
            .sections
            .push(render_doc_item(&doc.name, &doc.item));
    }

    for (relative_path, markdown_file) in outputs.iter() {
        let path = destination_dir.join(relative_path);
        console_message(format!("Writing to {}", path.display()));

        markdown_file.write_to_file(&path)?;
    }

    Ok(())
}
