use std::collections::HashMap;
use std::path::Path;
use std::path::PathBuf;

use buck2_docs_gen::OutputDirectory;
use itertools::Itertools;
use starlark::values::docs::Doc;
use starlark::values::docs::DocItem;
use starlark::values::docs::DocString;
use starlark::values::docs::Function;
use starlark::values::docs::Identifier;
use starlark::values::docs::Member;
use starlark::values::docs::Module;
use starlark::values::docs::Object;
use starlark::values::docs::Param;
use starlark::values::docs::Type;

use crate::daemon::docs::OutputDirAndDoc;

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

/// Does the heavy work of processing the docs and writing them to markdown files.
pub(crate) fn generate_markdown_files(
    opts: &MarkdownFileOptions,
    docs: Vec<OutputDirAndDoc>,
) -> anyhow::Result<()> {
    let destination_dir = opts
        .destination_dir
        .as_ref()
        .expect("clap enforces when --format=markdown_files");
    let mut outputs = HashMap::new();

    for (output_dir, doc) in docs
        .into_iter()
        .sorted_by(|l, r| l.1.id.name.cmp(&r.1.id.name))
    {
        let markdown_path = MarkdownOutput::markdown_path_for_doc(opts, &output_dir, &doc)?;
        let markdown_file = outputs
            .entry(markdown_path)
            .or_insert_with(MarkdownOutput::default);
        match doc.item {
            DocItem::Module(m) => markdown_file.add_module_docs(&doc.id, m),
            DocItem::Object(o) => markdown_file.add_object_docs(&doc.id, o),
            DocItem::Function(f) => markdown_file.add_function_docs(&doc.id, f),
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

#[derive(Clone, Debug, Default)]
struct MarkdownOutput {
    sections: Vec<String>,
}

impl MarkdownOutput {
    /// Grab the summary, details, and a combined string of both for a possible docstring
    ///
    /// # Returns
    /// A tuple of (summary, summary + details that has had all whitespace trimmed)
    /// If either the docstring, or the details are [`None`], then use an empty string instead
    fn docstring_to_strings(docstring: &Option<DocString>) -> (&str, String) {
        let (summary, details) = match docstring {
            None => ("", ""),
            Some(d) => (
                d.summary.as_str(),
                d.details.as_ref().map_or("", |s| s.as_str()),
            ),
        };
        let combined = format!("{}\n\n{}", summary, details).trim().to_owned();
        (summary, combined)
    }

    fn table(headers: &[&str], columns: Vec<Vec<String>>) -> String {
        let mut table = format!("| {} |\n", headers.iter().join(" | "));
        let dashes = format!(
            "|{}|\n",
            headers.iter().map(|s| "-".repeat(s.len() + 2)).join("|")
        );
        table.push_str(&dashes);

        let max_rows = columns.iter().map(Vec::len).max().unwrap_or(0);
        let rows = (0..max_rows)
            .map(|row| {
                // Multiline strings inside of tables are kind of wonky, but if you replace with
                // <br /> it works how you'd want. This, however, *will* have problems with
                // ``` blocks.
                let row_interior = columns
                    .iter()
                    .map(|col| match col.get(row) {
                        Some(s) => s.replace('\n', "<br />"),
                        None => String::new(),
                    })
                    .join(" | ");
                format!("| {} |", row_interior)
            })
            .join("\n");

        table.push_str(&rows);
        table.push('\n');
        table
    }

    /// Get the common representation of a potentially missing type
    fn type_string(typ: &Option<Type>) -> String {
        let typ_string = typ
            .as_ref()
            .map_or_else(String::new, |t| t.raw_type.clone());
        if typ_string.is_empty() {
            "UNKNOWN".to_owned()
        } else {
            typ_string
        }
    }

    /// Get a "type" that a function represents. i.e. a function prototype with no names
    fn function_type_string(function: &Function) -> String {
        let param_types = function
            .params
            .iter()
            .map(|p| match p {
                Param::Arg { typ, .. } => Self::type_string(typ),
                Param::NoArgs => "*".to_owned(),
                Param::Args { typ, .. } => format!("*{}", Self::type_string(typ)),
                Param::Kwargs { typ, .. } => format!("**{}", Self::type_string(typ)),
            })
            .join(", ");
        let ret_type = Self::type_string(&function.ret.typ);
        format!("({}) -> {}", param_types, ret_type)
    }

    /// Get a human friendly representation of a function prototype
    fn function_prototype(name: &str, function: &Function) -> String {
        // For really long function prototypes, this gets unreadable fast if it's all on one line
        let long_form = function.params.len() > 6;
        let (separator, param_prefix, paren_delimiter) = if long_form {
            (",\n", "    ", "\n")
        } else {
            (", ", "", "")
        };
        let parameters = function
            .params
            .iter()
            .map(|p| {
                let param_repr = match p {
                    Param::Arg {
                        name,
                        typ,
                        default_value,
                        ..
                    } => {
                        let type_string = Self::type_string(typ);
                        match default_value {
                            None => format!("{}: {}", name, type_string),
                            Some(d) => format!("{}: {} = {}", name, type_string, d),
                        }
                    }
                    Param::NoArgs => "*".to_owned(),
                    Param::Args { name, typ, .. } => {
                        format!("{}: {}", name, Self::type_string(typ))
                    }
                    Param::Kwargs { name, typ, .. } => {
                        format!("{}: {}", name, Self::type_string(typ))
                    }
                };
                format!("{}{}", param_prefix, param_repr)
            })
            .join(separator);
        let ret_type = Self::type_string(&function.ret.typ);

        format!(
            "## {}\n\n```python\ndef {}({}{}{}) -> {}\n```",
            name, name, paren_delimiter, parameters, paren_delimiter, ret_type
        )
    }

    /// Get the docs for a function. The prototype, the summary, etc. The returned docs are trimmed
    fn function_docs(name: &str, function: &Function) -> String {
        const FUNCTION_MARKDOWN: &str = r###"
{prototype}

{summary}
{parameters}
{details}
{return}
"###;

        let prototype = Self::function_prototype(name, function);
        // TODO(nmj): A the moment, we pull in the raw docstring for starlark functions without
        //            modifying it. This means that, while we have docs for specific params,
        //            if we printed them here, they'd be duplicated. We'll need to fix that
        //            behavior, though, to make sure that things like `rule()` outputs, and
        //            native objects work properly. For now, just print the raw docstring,
        //            and we'll go back and add an Args: section later
        let (summary, details) = match &function.docs {
            Some(d) if d.details.is_some() => (
                d.summary.as_str(),
                format!("### Details\n\n{}", d.details.as_ref().unwrap()),
            ),
            Some(d) => (d.summary.as_str(), String::new()),
            None => ("", String::new()),
        };
        let any_params_have_docs = function.params.iter().any(|p| match p {
            Param::Arg { docs, .. } => docs.is_some(),
            Param::NoArgs => false,
            Param::Args { docs, .. } => docs.is_some(),
            Param::Kwargs { docs, .. } => docs.is_some(),
        });
        let parameters = if any_params_have_docs {
            let (names, docs) = function.params.iter().fold(
                (Vec::new(), Vec::new()),
                |(mut names, mut docs), param| {
                    let details = match param {
                        Param::Arg { name, docs, .. } => {
                            Some((name.as_str(), Self::docstring_to_strings(docs)))
                        }
                        Param::NoArgs => None,
                        Param::Args { name, docs, .. } => {
                            Some((name.as_str(), Self::docstring_to_strings(docs)))
                        }
                        Param::Kwargs { name, docs, .. } => {
                            Some((name.as_str(), Self::docstring_to_strings(docs)))
                        }
                    };
                    if let Some((name, (_, summary_and_details))) = details {
                        names.push(format!("`{}`", name));
                        docs.push(summary_and_details);
                    }
                    (names, docs)
                },
            );
            let table = Self::table(&["Name", "Details"], vec![names, docs]);
            format!("\n### Parameters\n\n{}", table)
        } else {
            "".to_owned()
        };

        let return_docs = match Self::docstring_to_strings(&function.ret.docs) {
            (_, ret_docs) if !ret_docs.is_empty() => format!("\n### Returns\n\n{}", ret_docs),
            _ => String::new(),
        };

        FUNCTION_MARKDOWN
            .replace("{prototype}", &prototype)
            .replace("{summary}", summary)
            .replace("{details}", &details)
            .replace("{parameters}", &parameters)
            .replace("{return}", &return_docs)
            .trim()
            .to_owned()
    }

    fn add_module_docs(&mut self, id: &Identifier, module: Module) {
        const MODULE_MARKDOWN: &str = r###"
# {module_name}

{module_summary_and_details}
"###;

        let (_, summary_and_details) = Self::docstring_to_strings(&module.docs);
        if !summary_and_details.is_empty() {
            let module_name = match &id.location {
                Some(l) => l.path.as_str(),
                None => id.name.as_str(),
            };
            let markdown = MODULE_MARKDOWN
                .replace("{module_name}", module_name)
                .replace("{module_summary_and_details}", &summary_and_details)
                .trim()
                .to_owned();

            self.sections.insert(0, markdown);
        }
    }

    fn add_object_docs(&mut self, id: &Identifier, object: Object) {
        static OBJECT_MARKDOWN: &str = r####"
{name_prefix} {object_name}

{object_summary_and_details}

### Members

{members_table}

{members_details}
"####;

        static HEADERS: &[&str] = &["Member", "Type", "Description"];
        static OBJECT_MEMBER_DETAILS: &str = "## {name} : `{typ}`\n\n{summary_and_details}";

        let name_prefix = match &id.location {
            None => "#",
            Some(_) => "##",
        };
        let (_, summary_and_details) = Self::docstring_to_strings(&object.docs);

        let mut members_details = Vec::new();
        let mut members = Vec::new();
        let mut types = Vec::new();
        let mut summaries = Vec::new();

        for (member, member_doc) in object
            .members
            .into_iter()
            .sorted_by(|(l_m, _), (r_m, _)| l_m.cmp(r_m))
        {
            let (summary, details_section, typ) = match &member_doc {
                Member::Property(p) => {
                    let (summary, summary_and_details) = Self::docstring_to_strings(&p.docs);
                    let typ = Self::type_string(&p.typ);

                    let details_section = OBJECT_MEMBER_DETAILS
                        .replace("{name}", &member)
                        .replace("{typ}", &typ)
                        .replace("{summary_and_details}", &summary_and_details);
                    (summary, details_section, typ)
                }
                Member::Function(f) => {
                    let details_section = Self::function_docs(&member, f);
                    let typ = Self::function_type_string(f);
                    let summary = f.docs.as_ref().map_or("", |d| d.summary.as_str());
                    (summary, details_section, typ)
                }
            };
            members.push(member.to_owned());
            types.push(format!("`{}`", typ));
            summaries.push(summary.to_owned());

            members_details.push(details_section);
        }

        let members_table = Self::table(HEADERS, vec![members, types, summaries]);

        let contents = OBJECT_MARKDOWN
            .replace("{name_prefix}", name_prefix)
            .replace("{object_name}", &id.name)
            .replace("{object_summary_and_details}", &summary_and_details)
            .replace("{members_table}", &members_table)
            .replace("{members_details}", &members_details.join("\n\n---\n"))
            .trim()
            .to_owned();

        self.sections.push(contents);
    }

    fn add_function_docs(&mut self, id: &Identifier, function: Function) {
        let docs = Self::function_docs(&id.name, &function);
        self.sections.push(docs);
    }

    fn markdown_contents(&self) -> String {
        self.sections.join("\n\n---\n")
    }

    fn write_to_file(&self, path: &Path) -> anyhow::Result<String> {
        let mut contents = self.markdown_contents();
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

    /// Get the output path for the markdown for a given [`Doc`], whether it's in a starlark file, or a native symbol.
    fn markdown_path_for_doc(
        opts: &MarkdownFileOptions,
        output_dir: &OutputDirectory,
        doc: &Doc,
    ) -> anyhow::Result<PathBuf> {
        let subdir = output_dir.path();
        let path = match &doc.id.location {
            Some(loc) => opts
                .starlark_subdir
                .join(&subdir)
                .join(Self::path_from_location(&loc.path)?),
            None => match &doc.item {
                // Functions all go in one file. Objects get their on file (e.g. each provider,
                // Artifact, etc)
                DocItem::Module(_) | DocItem::Function(_) => {
                    opts.native_subdir.join(&subdir).join("native")
                }
                DocItem::Object(_) => opts.native_subdir.join(&subdir).join(&doc.id.name),
            },
        };
        let path = path.with_extension(match path.extension() {
            None => "md".to_owned(),
            Some(e) => format!("{}.md", e.to_str().expect("path if not UTF-8")),
        });
        Ok(path)
    }
}

#[cfg(test)]
mod tests {
    use gazebo::prelude::VecExt;
    use starlark::values::docs::*;

    use super::*;

    // Stolen directly from starlark's tests
    macro_rules! test_data {
        ($name:expr) => {
            include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/testdata/", $name,))
        };
    }

    fn param_docstring(name: &str) -> Option<DocString> {
        DocString::from_docstring(
            DocStringKind::Starlark,
            &format!("Docs for param `{}`", name),
        )
    }

    fn simple_docstring(name: &str, details: bool) -> Option<DocString> {
        Some(
            match details {
                true => DocString::from_docstring(
                    DocStringKind::Starlark,
                    &format!(
                        "Summary of {}\n\nDetails about {}. These are\nshort, sweet, and mulitline",
                        name, name
                    ),
                ),
                false => DocString::from_docstring(
                    DocStringKind::Starlark,
                    &format!("Summary of {}", name),
                ),
            }
            .unwrap(),
        )
    }

    fn simple_type(typ: &str) -> Option<Type> {
        Some(Type {
            raw_type: typ.to_owned(),
        })
    }

    fn simple_function(docstring: Option<DocString>) -> Function {
        Function {
            docs: docstring,
            params: vec![
                Param::Arg {
                    name: "a".to_owned(),
                    docs: param_docstring("a"),
                    typ: simple_type("\"string\""),
                    default_value: None,
                },
                Param::Arg {
                    name: "b".to_owned(),
                    docs: param_docstring("b"),
                    typ: None,
                    default_value: None,
                },
                Param::Arg {
                    name: "c".to_owned(),
                    docs: param_docstring("c"),
                    typ: simple_type("\"string\""),
                    default_value: Some("\"c_default\"".to_owned()),
                },
                Param::Arg {
                    name: "d".to_owned(),
                    docs: param_docstring("d"),
                    typ: None,
                    default_value: Some("\"d_default\"".to_owned()),
                },
                Param::NoArgs,
                Param::Args {
                    name: "*args".to_owned(),
                    docs: param_docstring("*args"),
                    typ: simple_type("[\"string\"]"),
                },
                Param::Kwargs {
                    name: "**kwargs".to_owned(),
                    docs: None,
                    typ: None,
                },
            ],
            ret: Return {
                docs: DocString::from_docstring(DocStringKind::Starlark, "returns a value"),
                typ: simple_type("\"string\""),
            },
        }
    }

    fn simple_short_function(docstring: Option<DocString>) -> Function {
        Function {
            docs: docstring,
            params: vec![
                Param::Arg {
                    name: "a".to_owned(),
                    docs: None,
                    typ: simple_type("\"string\""),
                    default_value: None,
                },
                Param::Arg {
                    name: "b".to_owned(),
                    docs: None,
                    typ: simple_type("\"string\""),
                    default_value: None,
                },
            ],
            ret: Return {
                docs: None,
                typ: simple_type("\"string\""),
            },
        }
    }

    fn walk_temp_dir(p: &Path) -> anyhow::Result<Vec<PathBuf>> {
        let mut all_files = vec![];
        std::fs::read_dir(p)?.try_for_each(|res| -> anyhow::Result<()> {
            let path = res?.path();
            if path.is_dir() {
                all_files.extend(walk_temp_dir(&path)?);
            } else {
                all_files.push(path);
            }
            Ok(())
        })?;
        Ok(all_files)
    }

    #[test]
    fn test_generates_correct_markdown() -> anyhow::Result<()> {
        let expected = maplit::hashmap! {
            "native/native.md" => test_data!("native/native.md"),
            "native/SomeInfo.md" => test_data!("native/SomeInfo.md"),
            "native/namespaced/OtherInfo.md" => test_data!("native/namespaced/OtherInfo.md"),
            "starlark/cell/foo/bar/baz.bzl.md" => test_data!("starlark/cell/foo/bar/baz.bzl.md"),
        };

        let sample_location = Some(Location {
            path: "cell//foo/bar:baz.bzl".to_owned(),
            position: None,
        });
        let sample_data = vec![
            // native objects / functions first
            Doc {
                id: Identifier {
                    name: "native".to_owned(),
                    location: None,
                },
                item: DocItem::Module(Module {
                    docs: Some(DocString {
                        summary: "This is where we describe what the native module is".to_owned(),
                        details: Some(
                            concat!(
                                "Any extra details we want to add for the native module\n",
                                "go here. These would be at the top of the file with native\n",
                                "functions"
                            )
                            .to_owned(),
                        ),
                    }),
                }),
                custom_attrs: Default::default(),
            },
            Doc {
                id: Identifier {
                    name: "native_function_1".to_owned(),
                    location: None,
                },
                item: DocItem::Function(simple_function(simple_docstring(
                    "native_function_1",
                    true,
                ))),
                custom_attrs: Default::default(),
            },
            Doc {
                id: Identifier {
                    name: "native_function_2".to_owned(),
                    location: None,
                },
                item: DocItem::Function(simple_function(simple_docstring(
                    "native_function_2",
                    true,
                ))),
                custom_attrs: Default::default(),
            },
            Doc {
                id: Identifier {
                    name: "native_function_3".to_owned(),
                    location: None,
                },
                item: DocItem::Function(simple_function(simple_docstring(
                    "native_function_3",
                    true,
                ))),
                custom_attrs: Default::default(),
            },
            Doc {
                id: Identifier {
                    name: "native_function_4".to_owned(),
                    location: None,
                },
                item: DocItem::Function(simple_short_function(simple_docstring(
                    "native_function_4",
                    true,
                ))),
                custom_attrs: Default::default(),
            },
            Doc {
                id: Identifier {
                    name: "SomeInfo".to_owned(),
                    location: None,
                },
                item: DocItem::Object(Object {
                    docs: simple_docstring("SomeInfo", true),
                    members: vec![
                        (
                            "foo".to_owned(),
                            Member::Property(Property {
                                docs: simple_docstring("SomeInfo.foo", true),
                                typ: simple_type("\"string\""),
                            }),
                        ),
                        (
                            "bar".to_owned(),
                            Member::Property(Property {
                                docs: simple_docstring("SomeInfo.bar", true),
                                typ: None,
                            }),
                        ),
                        (
                            "baz".to_owned(),
                            Member::Property(Property {
                                docs: simple_docstring("SomeInfo.baz", false),
                                typ: simple_type("\"string\""),
                            }),
                        ),
                        (
                            "some_func".to_owned(),
                            Member::Function(simple_function(simple_docstring(
                                "SomeInfo.some_func",
                                true,
                            ))),
                        ),
                    ],
                }),
                custom_attrs: Default::default(),
            },
            // similar data, but in a starlark file
            Doc {
                id: Identifier {
                    name: "fbsource//foo/bar:baz.bzl".to_owned(),
                    location: sample_location.clone(),
                },
                item: DocItem::Module(Module {
                    docs: Some(DocString {
                        summary: "This is where we describe what the baz module is".to_owned(),
                        details: Some(
                            concat!(
                                "Any extra details we want to add for the baz module\n",
                                "go here. These would be at the top of the file with\n",
                                "documented symbols in fbsource//foo/bar:baz.bzl"
                            )
                            .to_owned(),
                        ),
                    }),
                }),
                custom_attrs: Default::default(),
            },
            Doc {
                id: Identifier {
                    name: "user_function_1".to_owned(),
                    location: sample_location.clone(),
                },
                item: DocItem::Function(simple_function(simple_docstring("user_function_1", true))),
                custom_attrs: Default::default(),
            },
            Doc {
                id: Identifier {
                    name: "user_function_2".to_owned(),
                    location: sample_location.clone(),
                },
                item: DocItem::Function(simple_function(simple_docstring("user_function_2", true))),
                custom_attrs: Default::default(),
            },
            Doc {
                id: Identifier {
                    name: "user_function_3".to_owned(),
                    location: sample_location.clone(),
                },
                item: DocItem::Function(simple_function(simple_docstring("user_function_3", true))),
                custom_attrs: Default::default(),
            },
            // Make sure we have "CustomInfo" *after* "user_function_X" to make sure that
            // we sort members properly
            Doc {
                id: Identifier {
                    name: "CustomInfo".to_owned(),
                    location: sample_location,
                },
                item: DocItem::Object(Object {
                    docs: simple_docstring("CustomInfo", true),
                    members: vec![
                        (
                            "foo".to_owned(),
                            Member::Property(Property {
                                docs: simple_docstring("CustomInfo.foo", true),
                                typ: simple_type("\"string\""),
                            }),
                        ),
                        (
                            "bar".to_owned(),
                            Member::Property(Property {
                                docs: simple_docstring("CustomInfo.bar", true),
                                typ: None,
                            }),
                        ),
                        (
                            "baz".to_owned(),
                            Member::Property(Property {
                                docs: simple_docstring("CustomInfo.baz", false),
                                typ: simple_type("\"string\""),
                            }),
                        ),
                        (
                            "some_func".to_owned(),
                            Member::Function(simple_function(simple_docstring(
                                "CustomInfo.some_func",
                                true,
                            ))),
                        ),
                    ],
                }),
                custom_attrs: Default::default(),
            },
        ];
        let mut sample_data = sample_data.into_map(|d| (OutputDirectory::default(), d));
        // Small module, but in a subdir
        sample_data.push((
            OutputDirectory::try_from_string("namespaced").unwrap(),
            Doc {
                id: Identifier {
                    name: "OtherInfo".to_owned(),
                    location: None,
                },
                item: DocItem::Object(Object {
                    docs: simple_docstring("OtherInfo", true),
                    members: vec![],
                }),
                custom_attrs: Default::default(),
            },
        ));

        let temp = tempfile::tempdir()?;

        let opts = MarkdownFileOptions {
            destination_dir: Some(temp.path().to_path_buf()),
            native_subdir: PathBuf::try_from("native")?,
            starlark_subdir: PathBuf::try_from("starlark")?,
        };

        generate_markdown_files(&opts, sample_data)?;

        let all_contents = walk_temp_dir(temp.path())?
            .iter()
            .map(|path| {
                let relative_path = path.strip_prefix(temp.path())?;
                let contents = std::fs::read_to_string(path)?;
                Ok((relative_path.to_path_buf(), contents))
            })
            .collect::<anyhow::Result<HashMap<PathBuf, String>>>()?;

        for (p, contents) in all_contents {
            let filepath = p.to_str().unwrap();
            // Normalize Windows path separators. No-op for other platforms.
            let filepath = filepath.replace('\\', "/");
            let filepath = filepath.as_str();
            let expected_contents = expected
                .get(filepath)
                .unwrap_or_else(|| panic!("Got unexpected path {:?}", p));

            assert_eq!(contents.trim(), expected_contents.trim());
        }

        Ok(())
    }
}
