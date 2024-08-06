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
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_events::dispatch::console_message;
use itertools::Itertools;
use starlark::docs::markdown::render_doc_item;
use starlark::docs::Doc;
use starlark::docs::DocItem;

static DOCS_DIRECTORY_KEY: &str = "directory";
static DOCS_BUILTIN_KEY: &str = "builtin";

fn format_custom_attr_error(keys_and_values: &[(String, String)]) -> String {
    let mut ret = "{".to_owned();
    ret.push_str(
        &keys_and_values
            .iter()
            .map(|(k, v)| format!("`{}` => `{}`", k, v))
            .join(", "),
    );
    ret.push('}');
    ret
}

#[derive(Debug, buck2_error::Error)]
enum DocPathError {
    #[error("Directory traversal was found in documentation path `{}` provided for `{}`", .path, .name)]
    InvalidDirectory {
        name: String,
        path: String,
        source: anyhow::Error,
    },
    #[error("Invalid custom attributes were found on `{}`: {}", .name, format_custom_attr_error(.keys_and_values))]
    InvalidCustomAttributes {
        name: String,
        keys_and_values: Vec<(String, String)>,
    },
    #[error("Conflicting custom attributes were found on `{}`: {}", .name, format_custom_attr_error(.keys_and_values))]
    ConflictingCustomAttributes {
        name: String,
        keys_and_values: Vec<(String, String)>,
    },
}

/// Get the output subdirectory for a [`Doc`] based on the `directory` custom attr, if present.
fn output_subdir_for_doc(doc: &Doc) -> anyhow::Result<ForwardRelativePathBuf> {
    let unknown_keys: Vec<_> = doc
        .custom_attrs
        .iter()
        .filter(|(k, _)| *k != DOCS_DIRECTORY_KEY && *k != DOCS_BUILTIN_KEY)
        .map(|(k, v)| (k.clone(), v.clone()))
        .collect();
    if !unknown_keys.is_empty() {
        return Err(DocPathError::InvalidCustomAttributes {
            name: doc.id.name.to_owned(),
            keys_and_values: unknown_keys,
        }
        .into());
    }

    match (
        doc.custom_attrs.get(DOCS_DIRECTORY_KEY),
        doc.custom_attrs.get(DOCS_BUILTIN_KEY),
    ) {
        (Some(path), None) | (None, Some(path)) => {
            match ForwardRelativePathBuf::new(path.to_owned()) {
                Ok(fp) => Ok(fp),
                Err(e) => Err(DocPathError::InvalidDirectory {
                    name: doc.id.name.to_owned(),
                    path: path.to_owned(),
                    source: e,
                }
                .into()),
            }
        }
        (Some(dir), Some(builtin)) => Err(DocPathError::ConflictingCustomAttributes {
            name: doc.id.name.to_owned(),
            keys_and_values: vec![
                (DOCS_DIRECTORY_KEY.to_owned(), dir.clone()),
                (DOCS_BUILTIN_KEY.to_owned(), builtin.clone()),
            ],
        }
        .into()),
        (None, None) => Ok(ForwardRelativePathBuf::new(String::new())?),
    }
}

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
                DocItem::Member(_) => native_subdir.join(subdir.as_path()).join("native"),
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
        markdown_file
            .sections
            .push(render_doc_item(&doc.id.name, &doc.item));
    }

    for (relative_path, markdown_file) in outputs.iter() {
        let path = destination_dir.join(relative_path);
        console_message(format!("Writing to {}", path.display()));

        markdown_file.write_to_file(&path)?;
    }

    Ok(())
}
