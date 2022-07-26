use std::cmp::Ordering;
use std::collections::HashMap;
use std::path::Path;
use std::path::PathBuf;

use buck2_core::fs::paths::ForwardRelativePathBuf;
use itertools::Itertools;
use starlark::values::docs::markdown::AsMarkdown;
use starlark::values::docs::markdown::MarkdownFlavor;
use starlark::values::docs::Doc;
use starlark::values::docs::DocItem;

static DOCS_DIRECTORY_KEY: &str = "directory";

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
pub(crate) struct MarkdownFileOptions {
    #[structopt(
        long = "--markdown-files-destination-dir",
        required_if_eq("format", "markdown_files")
    )]
    destination_dir: Option<PathBuf>,
    #[structopt(long = "--markdown-files-native-subdir", default_value="native", parse(try_from_str = PathBuf::try_from))]
    native_subdir: PathBuf,
    #[structopt(long = "--markdown-files-starlark-subdir", default_value="starlark", parse(try_from_str = PathBuf::try_from))]
    starlark_subdir: PathBuf,
}

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

#[derive(Debug, thiserror::Error)]
enum MarkdownError {
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
            std::fs::create_dir_all(p)?;
        }
        std::fs::write(path, &contents)?;

        Ok(contents)
    }

    /// Convert a buck style path (foo//bar:baz.bzl) to path, failing if someone attempts to traverse upward
    fn path_from_location(location: &str) -> anyhow::Result<PathBuf> {
        Ok(Path::new(&location.replace("//", "/").replace(':', "/")).to_path_buf())
    }

    fn output_subdir_for_doc(doc: &Doc) -> anyhow::Result<ForwardRelativePathBuf> {
        let unknown_keys: Vec<_> = doc
            .custom_attrs
            .iter()
            .filter(|(k, _)| *k != DOCS_DIRECTORY_KEY)
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
        if !unknown_keys.is_empty() {
            return Err(MarkdownError::InvalidCustomAttributes {
                name: doc.id.name.to_owned(),
                keys_and_values: unknown_keys,
            }
            .into());
        }

        match doc.custom_attrs.get(DOCS_DIRECTORY_KEY) {
            Some(path) => match ForwardRelativePathBuf::new(path.to_owned()) {
                Ok(fp) => Ok(fp),
                Err(e) => Err(MarkdownError::InvalidDirectory {
                    name: doc.id.name.to_owned(),
                    path: path.to_owned(),
                    source: e,
                }
                .into()),
            },
            None => Ok(ForwardRelativePathBuf::new(String::new())?),
        }
    }

    /// Get the output path for the markdown for a given [`Doc`], whether it's in a starlark file, or a native symbol.
    fn markdown_path_for_doc(opts: &MarkdownFileOptions, doc: &Doc) -> anyhow::Result<PathBuf> {
        let subdir = Self::output_subdir_for_doc(doc)?;
        let path = match &doc.id.location {
            Some(loc) => opts
                .starlark_subdir
                .join(subdir.as_path())
                .join(Self::path_from_location(&loc.path)?),
            None => match &doc.item {
                // Functions all go in one file. Objects get their on file (e.g. each provider,
                // Artifact, etc)
                DocItem::Module(_) | DocItem::Function(_) => {
                    opts.native_subdir.join(subdir.as_path()).join("native")
                }
                DocItem::Object(_) => opts.native_subdir.join(subdir.as_path()).join(&doc.id.name),
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
        if let Some(docs) = doc.generate_markdown(MarkdownFlavor::DocFile) {
            markdown_file.sections.push(docs);
        }
    }

    let abs_destination = if destination_dir.is_relative() {
        std::env::current_dir()?.join(&destination_dir)
    } else {
        destination_dir.to_owned()
    };

    for (relative_path, markdown_file) in outputs.iter() {
        let path = abs_destination.join(relative_path);
        crate::eprintln!("Writing to {}", path.to_str().unwrap())?;

        markdown_file.write_to_file(&path)?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use starlark::values::docs::*;

    use super::*;

    #[test]
    fn writes_docs_to_correct_files() -> anyhow::Result<()> {
        let ds = DocString::from_docstring(DocStringKind::Rust, "Summary\n\nDetails");
        let typ = Some(Type {
            raw_type: "int".to_owned(),
        });

        let docs = vec![
            Doc {
                id: Identifier {
                    name: "a_function".to_owned(),
                    location: None,
                },
                custom_attrs: HashMap::new(),
                item: DocItem::Function(Function {
                    docs: ds.clone(),
                    params: vec![],
                    ret: Return {
                        docs: ds.clone(),
                        typ: typ.clone(),
                    },
                }),
            },
            Doc {
                id: Identifier {
                    name: "BuiltinFoo".to_owned(),
                    location: None,
                },
                custom_attrs: HashMap::new(),
                item: DocItem::Module(Module { docs: ds.clone() }),
            },
            Doc {
                id: Identifier {
                    name: "another_function".to_owned(),
                    location: None,
                },
                custom_attrs: hashmap! { "directory".to_owned() => "subdir".to_owned()},
                item: DocItem::Function(Function {
                    docs: ds.clone(),
                    params: vec![],
                    ret: Return {
                        docs: ds.clone(),
                        typ: typ.clone(),
                    },
                }),
            },
            Doc {
                id: Identifier {
                    name: "last_function".to_owned(),
                    location: Some(Location {
                        path: "foo.bzl".to_owned(),
                        position: None,
                    }),
                },
                custom_attrs: HashMap::new(),
                item: DocItem::Function(Function {
                    docs: ds.clone(),
                    params: vec![],
                    ret: Return {
                        docs: ds.clone(),
                        typ,
                    },
                }),
            },
            Doc {
                id: Identifier {
                    name: "BuiltinFoo".to_owned(),
                    location: Some(Location {
                        path: "foo.bzl".to_owned(),
                        position: None,
                    }),
                },
                custom_attrs: HashMap::new(),
                item: DocItem::Module(Module { docs: ds }),
            },
        ];

        let expected_native = format!(
            "{}\n\n---\n{}\n",
            docs.get(1)
                .unwrap()
                .generate_markdown_or_empty(MarkdownFlavor::DocFile),
            docs.get(0)
                .unwrap()
                .generate_markdown_or_empty(MarkdownFlavor::DocFile)
        );

        let expected_native_subdir = format!(
            "{}\n",
            docs.get(2)
                .unwrap()
                .generate_markdown_or_empty(MarkdownFlavor::DocFile),
        );

        let expected_starlark = format!(
            "{}\n\n---\n{}\n",
            docs.get(4)
                .unwrap()
                .generate_markdown_or_empty(MarkdownFlavor::DocFile),
            docs.get(3)
                .unwrap()
                .generate_markdown_or_empty(MarkdownFlavor::DocFile)
        );

        let temp = tempfile::tempdir()?;

        let opts = MarkdownFileOptions {
            destination_dir: Some(temp.path().to_path_buf()),
            native_subdir: PathBuf::try_from("native")?,
            starlark_subdir: PathBuf::try_from("starlark")?,
        };

        generate_markdown_files(&opts, docs)?;
        let native = std::fs::read_to_string(temp.path().join("native/native.md"))?;
        let native_subdir = std::fs::read_to_string(temp.path().join("native/subdir/native.md"))?;
        let starlark = std::fs::read_to_string(temp.path().join("starlark/foo.bzl.md"))?;

        assert_eq!(expected_native, native);
        assert_eq!(expected_native_subdir, native_subdir);
        assert_eq!(expected_starlark, starlark);

        Ok(())
    }
}
