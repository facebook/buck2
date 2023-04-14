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
// TODO(nga): `output_subdir_for_doc` is the only function which prevents
//   moving `docs` command to `buck2_client`. Possible solution is to generate docs
//   (or maybe even write to the filesystem) on the server side.
use buck2_server::lsp::output_subdir_for_doc;
use itertools::Itertools;
use starlark::docs::Doc;
use starlark::docs::DocItem;
use starlark::docs::MarkdownFlavor;
use starlark::docs::RenderMarkdown;

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
pub(crate) struct MarkdownFileOptions {
    #[structopt(
        long = "markdown-files-destination-dir",
        required_if_eq("format", "markdown_files")
    )]
    destination_dir: Option<PathBuf>,
    #[structopt(long = "markdown-files-native-subdir", default_value="native", parse(try_from_str = PathBuf::try_from))]
    native_subdir: PathBuf,
    #[structopt(long = "markdown-files-starlark-subdir", default_value="starlark", parse(try_from_str = PathBuf::try_from))]
    starlark_subdir: PathBuf,
}

#[derive(Clone, Debug, Default)]
struct MarkdownOutput {
    sections: Vec<String>,
}

impl MarkdownOutput {
    fn write_to_file(&self, path: &Path) -> anyhow::Result<String> {
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
    fn markdown_path_for_doc(opts: &MarkdownFileOptions, doc: &Doc) -> anyhow::Result<PathBuf> {
        let subdir = output_subdir_for_doc(doc)?;
        let path = match &doc.id.location {
            Some(loc) => opts
                .starlark_subdir
                .join(subdir.as_path())
                .join(Self::path_from_location(&loc.path)?),
            None => match &doc.item {
                // Functions all go in one file.
                // Objects get their on file (e.g. each provider, Artifact, etc)
                DocItem::Function(_) | DocItem::Property(_) => {
                    opts.native_subdir.join(subdir.as_path()).join("native")
                }
                DocItem::Module(_) | DocItem::Object(_) => {
                    opts.native_subdir.join(subdir.as_path()).join(&doc.id.name)
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
    opts: &MarkdownFileOptions,
    docs: Vec<Doc>,
) -> anyhow::Result<()> {
    let destination_dir = opts
        .destination_dir
        .as_ref()
        .expect("clap enforces when --format=markdown_files");
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
        let markdown_path = MarkdownOutput::markdown_path_for_doc(opts, &doc)?;
        let markdown_file = outputs
            .entry(markdown_path)
            .or_insert_with(MarkdownOutput::default);
        if let Some(docs) = doc.render_markdown_opt(MarkdownFlavor::DocFile) {
            markdown_file.sections.push(docs);
        }
    }

    let abs_destination = if destination_dir.is_relative() {
        std::env::current_dir()?.join(destination_dir)
    } else {
        destination_dir.to_owned()
    };

    for (relative_path, markdown_file) in outputs.iter() {
        let path = abs_destination.join(relative_path);
        buck2_client_ctx::eprintln!("Writing to {}", path.to_str().unwrap())?;

        markdown_file.write_to_file(&path)?;
    }

    Ok(())
}
