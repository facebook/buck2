/*
 * Copyright 2019 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use gazebo::prelude::*;
use itertools::Itertools;

use crate::values::docs::Doc;
use crate::values::docs::DocItem;
use crate::values::docs::DocString;
use crate::values::docs::Function;
use crate::values::docs::Identifier;
use crate::values::docs::Member;
use crate::values::docs::Module;
use crate::values::docs::Object;
use crate::values::docs::Param;
use crate::values::docs::Property;
use crate::values::docs::Type;

/// The style of output that is being generated
#[derive(Copy, Clone, Dupe)]
pub enum MarkdownFlavor {
    /// A file that is written out to disk for a website or in repo.
    ///
    /// These pages are generally slightly more detailed (e.g. module summary tables at the top
    /// of the page) and have different formatting due differing use cases.
    DocFile,
    /// A summary that can be shown in the "Hover" event in the LSP.
    LspSummary,
}

/// This object can potentially generate markdown documentation about itself.
pub trait AsMarkdown {
    /// Generate markdown of the given flavor if possible. For some types, there may not be
    /// any useful documentation available.
    fn generate_markdown(&self, flavor: MarkdownFlavor) -> Option<String>;

    /// Convenience method that invokes `AsMarkdown::generate_markdown`, and returns an
    /// empty string if that is `None`
    fn generate_markdown_or_empty(&self, flavor: MarkdownFlavor) -> String {
        self.generate_markdown(flavor).unwrap_or_default()
    }
}

impl AsMarkdown for String {
    fn generate_markdown(&self, _flavor: MarkdownFlavor) -> Option<String> {
        Some(self.clone())
    }
}
impl AsMarkdown for str {
    fn generate_markdown(&self, _flavor: MarkdownFlavor) -> Option<String> {
        Some(self.to_owned())
    }
}
impl<T: AsMarkdown> AsMarkdown for &T {
    fn generate_markdown(&self, flavor: MarkdownFlavor) -> Option<String> {
        (*self).generate_markdown(flavor)
    }
}

/// What to render from a [`DocString`].
enum DSOpts {
    /// Just the summary.
    Summary,
    /// Just the details (if present).
    Details,
    /// Both the summary and the details, separated in an appropriate fashion.
    Combined,
}

/// Renders a docstring in a given fashion.
struct DocStringRenderer<'a>(DSOpts, &'a Option<DocString>);

impl<'a> AsMarkdown for DocStringRenderer<'a> {
    fn generate_markdown(&self, flavor: MarkdownFlavor) -> Option<String> {
        match flavor {
            MarkdownFlavor::DocFile => self.1.as_ref().and_then(|d| match self.0 {
                DSOpts::Summary => Some(d.summary.clone()),
                DSOpts::Details => d.details.clone(),
                DSOpts::Combined => Some(match &d.details {
                    Some(details) => format!("{}\n\n{}", d.summary, details),
                    None => d.summary.clone(),
                }),
            }),
            MarkdownFlavor::LspSummary => None,
        }
    }
}

/// Renders details about a property of an object that has the given name.
struct PropertyDetailsRenderer<'a> {
    name: String,
    p: &'a Property,
}

impl<'a> AsMarkdown for PropertyDetailsRenderer<'a> {
    fn generate_markdown(&self, flavor: MarkdownFlavor) -> Option<String> {
        match flavor {
            MarkdownFlavor::DocFile => {
                let header = format!(
                    "## {} : {}",
                    self.name,
                    Code(box TypeRenderer::Type(&self.p.typ)).generate_markdown_or_empty(flavor)
                );
                let summary =
                    DocStringRenderer(DSOpts::Summary, &self.p.docs).generate_markdown(flavor);
                let details =
                    DocStringRenderer(DSOpts::Details, &self.p.docs).generate_markdown(flavor);

                let mut body = header;
                if let Some(summary) = summary {
                    body.push_str("\n\n");
                    body.push_str(&summary);
                }
                if let Some(details) = details {
                    body.push_str("\n\n");
                    body.push_str(&details);
                }

                return Some(body);
            }
            MarkdownFlavor::LspSummary => None,
        }
    }
}

/// Renders the details panel of a function (either standalone or on an object).
struct FunctionDetailsRenderer<'a> {
    name: String,
    f: &'a Function,
}

impl<'a> FunctionDetailsRenderer<'a> {
    /// If there are any parameter docs to render, render them as a table.
    fn parameters_table(&self, flavor: MarkdownFlavor) -> Option<String> {
        // If we don't have any meaningful parameter docs, just omit the table entirely.
        let has_any_docs = self.f.params.iter().any(|p| match p {
            Param::Arg { docs, .. } => docs.is_some(),
            Param::NoArgs => false,
            Param::Args { docs, .. } => docs.is_some(),
            Param::Kwargs { docs, .. } => docs.is_some(),
        });

        if !has_any_docs {
            return None;
        }

        let header = TableHeader(&["Name", "Details"]);
        let rows = self
            .f
            .params
            .iter()
            .filter_map(|p| match p {
                Param::Arg { name, docs, .. } => {
                    let docs = DocStringRenderer(DSOpts::Combined, docs)
                        .generate_markdown_or_empty(flavor);
                    Some((name.clone(), docs))
                }
                Param::NoArgs => None,
                Param::Args { name, docs, .. } => {
                    let docs = DocStringRenderer(DSOpts::Combined, docs)
                        .generate_markdown_or_empty(flavor);
                    Some((name.clone(), docs))
                }
                Param::Kwargs { name, docs, .. } => {
                    let docs = DocStringRenderer(DSOpts::Combined, docs)
                        .generate_markdown_or_empty(flavor);
                    Some((name.clone(), docs))
                }
            })
            .map(|(name, docs)| TableRow(vec![box Code(box name), box docs]))
            .collect();

        let table = Table(header, rows);
        table.generate_markdown(flavor)
    }
}

impl<'a> AsMarkdown for FunctionDetailsRenderer<'a> {
    fn generate_markdown(&self, flavor: MarkdownFlavor) -> Option<String> {
        match flavor {
            MarkdownFlavor::DocFile => {
                let prototype = CodeBlock {
                    language: Some("python".to_owned()),
                    contents: box TypeRenderer::Function {
                        function_name: Some(self.name.clone()),
                        max_args_before_multiline: Some(6),
                        show_param_details: true,
                        f: self.f,
                    },
                };
                let header = format!(
                    "## {}\n\n{}",
                    self.name,
                    prototype.generate_markdown_or_empty(flavor)
                );
                let summary =
                    DocStringRenderer(DSOpts::Summary, &self.f.docs).generate_markdown(flavor);
                let details =
                    DocStringRenderer(DSOpts::Details, &self.f.docs).generate_markdown(flavor);

                let parameter_docs = self.parameters_table(flavor);
                let return_docs =
                    DocStringRenderer(DSOpts::Combined, &self.f.ret.docs).generate_markdown(flavor);

                let mut body = header;
                if let Some(summary) = summary {
                    body.push_str("\n\n");
                    body.push_str(&summary);
                }
                if let Some(parameter_docs) = parameter_docs {
                    body.push_str("\n\n### Parameters\n\n");
                    body.push_str(&parameter_docs);
                }
                if let Some(details) = details {
                    body.push_str("\n\n### Details\n\n");
                    body.push_str(&details);
                }
                if let Some(returns) = return_docs {
                    body.push_str("\n\n### Returns\n\n");
                    body.push_str(&returns);
                }

                Some(body)
            }
            MarkdownFlavor::LspSummary => None,
        }
    }
}

/// Renders a top level function from a [`Doc`].
struct FunctionRenderer<'a> {
    id: &'a Identifier,
    function: &'a Function,
}

impl<'a> AsMarkdown for FunctionRenderer<'a> {
    fn generate_markdown(&self, flavor: MarkdownFlavor) -> Option<String> {
        match flavor {
            MarkdownFlavor::DocFile => FunctionDetailsRenderer {
                name: self.id.name.clone(),
                f: self.function,
            }
            .generate_markdown(flavor),
            MarkdownFlavor::LspSummary => None,
        }
    }
}

/// Render a top level module.
struct ModuleRenderer<'a> {
    id: &'a Identifier,
    module: &'a Module,
}

impl<'a> AsMarkdown for ModuleRenderer<'a> {
    fn generate_markdown(&self, flavor: MarkdownFlavor) -> Option<String> {
        match flavor {
            MarkdownFlavor::DocFile => {
                let name = match &self.id.location {
                    Some(l) => l.path.as_str(),
                    None => self.id.name.as_str(),
                };
                let docs = DocStringRenderer(DSOpts::Combined, &self.module.docs)
                    .generate_markdown_or_empty(flavor);
                Some(format!("# {}\n\n{}", name, docs))
            }
            MarkdownFlavor::LspSummary => None,
        }
    }
}

/// Render a top level object.
struct ObjectRenderer<'a> {
    id: &'a Identifier,
    object: &'a Object,
}

impl<'a> AsMarkdown for ObjectRenderer<'a> {
    fn generate_markdown(&self, flavor: MarkdownFlavor) -> Option<String> {
        match flavor {
            MarkdownFlavor::DocFile => {
                // If this is a native, top level object, render it with a larger
                // header. Sub objects will be listed along side members, so use
                // smaller headers there.
                let title = match self.id.location.is_none() {
                    true => format!("# {}", self.id.name),
                    false => format!("## {}", self.id.name),
                };
                let summary = DocStringRenderer(DSOpts::Combined, &self.object.docs)
                    .generate_markdown(flavor)
                    .map(|s| format!("\n\n{}", s))
                    .unwrap_or_default();

                let members_header = TableHeader(&["Member", "Type", "Description"]);

                let (members_rows, member_details): (Vec<TableRow>, Vec<String>) = self
                    .object
                    .members
                    .iter()
                    .sorted_by(|(l_m, _), (r_m, _)| l_m.cmp(r_m))
                    .map(|(name, member)| {
                        let (summary, typ) = match member {
                            Member::Property(p) => (&p.docs, TypeRenderer::Type(&p.typ)),
                            Member::Function(f) => (
                                &f.docs,
                                TypeRenderer::Function {
                                    function_name: None,
                                    show_param_details: false,
                                    max_args_before_multiline: None,
                                    f,
                                },
                            ),
                        };
                        let row = TableRow(vec![
                            box name.clone(),
                            box Code(box typ),
                            box DocStringRenderer(DSOpts::Summary, summary),
                        ]);
                        let details = MemberDetails {
                            name: name.clone(),
                            member,
                        }
                        .generate_markdown_or_empty(flavor);
                        (row, details)
                    })
                    .unzip();

                let members_table =
                    Table(members_header, members_rows).generate_markdown_or_empty(flavor);
                let members_details = member_details.join("\n\n---\n");

                let page_body = format!(
                    "{title}{summary}\n\n### Members\n\n{members_table}\n\n\n{members_details}",
                    title = title,
                    summary = summary,
                    members_table = members_table,
                    members_details = members_details
                );
                Some(page_body)
            }
            MarkdownFlavor::LspSummary => None,
        }
    }
}

impl AsMarkdown for Doc {
    fn generate_markdown(&self, flavor: MarkdownFlavor) -> Option<String> {
        match flavor {
            MarkdownFlavor::DocFile => {
                // These just proxy to the Renderer types so we can add extra metadata to them,
                // like the identifier.
                match &self.item {
                    DocItem::Module(m) => ModuleRenderer {
                        id: &self.id,
                        module: m,
                    }
                    .generate_markdown(flavor),
                    DocItem::Object(o) => ObjectRenderer {
                        id: &self.id,
                        object: o,
                    }
                    .generate_markdown(flavor),
                    DocItem::Function(f) => FunctionRenderer {
                        id: &self.id,
                        function: f,
                    }
                    .generate_markdown(flavor),
                }
            }
            MarkdownFlavor::LspSummary => None,
        }
    }
}

/// Details about a member. Proxies to `PropertyDetailsRenderer` and `FunctionDetailsRenderer`
pub struct MemberDetails<'a> {
    name: String,
    member: &'a Member,
}

impl<'a> AsMarkdown for MemberDetails<'a> {
    fn generate_markdown(&self, flavor: MarkdownFlavor) -> Option<String> {
        match flavor {
            MarkdownFlavor::DocFile => match self.member {
                Member::Property(p) => PropertyDetailsRenderer {
                    name: self.name.clone(),
                    p,
                }
                .generate_markdown(flavor),
                Member::Function(f) => FunctionDetailsRenderer {
                    name: self.name.clone(),
                    f,
                }
                .generate_markdown(flavor),
            },
            MarkdownFlavor::LspSummary => None,
        }
    }
}

/// Render a "type". This is either a [`Type`] object, or details about a function to
/// produce a function prototype.
pub enum TypeRenderer<'a> {
    /// A general "type".
    Type(&'a Option<Type>),
    /// A function, with some extra formatting options.
    Function {
        /// If present, then any functions with more parameters than this will have
        /// their prototype split over multiple lines. Otherwise, it is returned as
        /// a single line.
        max_args_before_multiline: Option<usize>,
        /// Whether to show things like the name of the parameter, and default values if present.
        show_param_details: bool,
        /// If provided, print out the function name in the prototype as well.
        function_name: Option<String>,
        f: &'a Function,
    },
}

impl<'a> AsMarkdown for TypeRenderer<'a> {
    fn generate_markdown(&self, flavor: MarkdownFlavor) -> Option<String> {
        fn raw_type(t: &Option<Type>) -> String {
            match t {
                Some(t) if !t.raw_type.is_empty() => t.raw_type.clone(),
                _ => "UNKNOWN".to_owned(),
            }
        }

        match flavor {
            MarkdownFlavor::DocFile => match self {
                TypeRenderer::Type(t) => Some(raw_type(t)),
                TypeRenderer::Function {
                    max_args_before_multiline: max_args_per_line,
                    show_param_details,
                    function_name,
                    f,
                } => {
                    let mut params = f.params.iter().map(|p| match p {
                        Param::Arg {
                            typ,
                            name,
                            default_value,
                            ..
                        } => {
                            let type_string = raw_type(typ);
                            if *show_param_details {
                                match default_value {
                                    Some(v) => format!("{}: {} = {}", name, type_string, v),
                                    None => format!("{}: {}", name, type_string),
                                }
                            } else {
                                type_string
                            }
                        }
                        Param::NoArgs => "*".to_owned(),
                        Param::Args { typ, name, .. } => {
                            let type_string = raw_type(typ);
                            if *show_param_details {
                                format!("{}: {}", name, type_string)
                            } else {
                                format!("*{}", type_string)
                            }
                        }
                        Param::Kwargs { typ, name, .. } => {
                            let type_string = raw_type(typ);
                            if *show_param_details {
                                format!("{}: {}", name, type_string)
                            } else {
                                format!("**{}", type_string)
                            }
                        }
                    });

                    let ret_type = raw_type(&f.ret.typ);
                    let prefix = match function_name {
                        Some(name) => format!("def {}", name),
                        None => String::new(),
                    };
                    match max_args_per_line {
                        Some(i) if *i < f.params.len() => {
                            let chunked_params = params.join(",\n    ");
                            Some(format!(
                                "{}(\n    {}\n) -> {}",
                                prefix, chunked_params, ret_type
                            ))
                        }
                        _ => Some(format!("{}({}) -> {}", prefix, params.join(", "), ret_type)),
                    }
                }
            },
            MarkdownFlavor::LspSummary => None,
        }
    }
}

/// A string that should be put in "`" and be rendered literally.
pub struct Code<'a>(Box<dyn AsMarkdown + 'a>);

impl<'a> AsMarkdown for Code<'a> {
    fn generate_markdown(&self, flavor: MarkdownFlavor) -> Option<String> {
        match flavor {
            MarkdownFlavor::DocFile => self
                .0
                .generate_markdown(flavor)
                .map(|md| format!("`{}`", md)),
            MarkdownFlavor::LspSummary => None,
        }
    }
}

/// A code block that optionally has a language. Note that this will always take multiple
/// lines, so may not be ideal for tables at the moment.
pub struct CodeBlock<'a> {
    language: Option<String>,
    contents: Box<dyn AsMarkdown + 'a>,
}

impl<'a> AsMarkdown for CodeBlock<'a> {
    fn generate_markdown(&self, flavor: MarkdownFlavor) -> Option<String> {
        match flavor {
            MarkdownFlavor::DocFile => self.contents.generate_markdown(flavor).map(|contents| {
                format!(
                    "```{}\n{}\n```",
                    self.language.clone().unwrap_or_default(),
                    contents
                )
            }),
            MarkdownFlavor::LspSummary => None,
        }
    }
}

/// A table.
pub struct Table<'a>(TableHeader<'a>, Vec<TableRow<'a>>);

impl<'a> AsMarkdown for Table<'a> {
    fn generate_markdown(&self, flavor: MarkdownFlavor) -> Option<String> {
        match flavor {
            MarkdownFlavor::DocFile => {
                let rows = self
                    .1
                    .iter()
                    .filter_map(|row| row.generate_markdown(flavor))
                    .join("\n");

                self.0
                    .generate_markdown(flavor)
                    .map(|header| format!("{}\n{}", header, rows))
            }
            MarkdownFlavor::LspSummary => None,
        }
    }
}

/// The header of a table including all decoration needed. (`<thead>`, `| --- |` rows, etc)
pub struct TableHeader<'a>(&'a [&'a str]);

impl<'a> AsMarkdown for TableHeader<'a> {
    fn generate_markdown(&self, flavor: MarkdownFlavor) -> Option<String> {
        match flavor {
            MarkdownFlavor::DocFile => {
                let (header_row, dashes_row): (Vec<_>, Vec<_>) = self
                    .0
                    .iter()
                    .map(|col| {
                        let header_cell = col.generate_markdown_or_empty(flavor);
                        let dashes_cell = "-".repeat(header_cell.len());
                        (header_cell, dashes_cell)
                    })
                    .unzip();
                Some(format!(
                    "| {} |\n|-{}-|",
                    header_row.join(" | "),
                    dashes_row.join("-|-")
                ))
            }
            MarkdownFlavor::LspSummary => None,
        }
    }
}

/// A row for a table with all decoration handled. Does not handled multi-line cells at the moment
/// due to a restriction in the default markdown table syntax.
pub struct TableRow<'a>(Vec<Box<dyn AsMarkdown + 'a>>);

impl<'a> AsMarkdown for TableRow<'a> {
    fn generate_markdown(&self, flavor: MarkdownFlavor) -> Option<String> {
        match flavor {
            MarkdownFlavor::DocFile => {
                let inner = self
                    .0
                    .iter()
                    .map(|cell| cell.generate_markdown_or_empty(flavor))
                    .join(" | ");
                Some(format!("| {} |", inner))
            }
            MarkdownFlavor::LspSummary => None,
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use crate::values::docs::markdown::AsMarkdown;
    use crate::values::docs::markdown::Code;
    use crate::values::docs::markdown::CodeBlock;
    use crate::values::docs::markdown::DSOpts;
    use crate::values::docs::markdown::DocStringRenderer;
    use crate::values::docs::markdown::FunctionDetailsRenderer;
    use crate::values::docs::markdown::MarkdownFlavor;
    use crate::values::docs::markdown::PropertyDetailsRenderer;
    use crate::values::docs::markdown::Table;
    use crate::values::docs::markdown::TableHeader;
    use crate::values::docs::markdown::TableRow;
    use crate::values::docs::markdown::TypeRenderer;
    use crate::values::docs::Doc;
    use crate::values::docs::DocItem;
    use crate::values::docs::DocString;
    use crate::values::docs::DocStringKind;
    use crate::values::docs::Function;
    use crate::values::docs::Identifier;
    use crate::values::docs::Location;
    use crate::values::docs::Member;
    use crate::values::docs::Module;
    use crate::values::docs::Object;
    use crate::values::docs::Param;
    use crate::values::docs::Property;
    use crate::values::docs::Return;
    use crate::values::docs::Type;

    fn render(renderer: &dyn AsMarkdown) -> String {
        renderer.generate_markdown(MarkdownFlavor::DocFile).unwrap()
    }

    fn render_ds_summary(ds: &Option<DocString>) -> String {
        render(&DocStringRenderer(DSOpts::Summary, ds))
    }
    fn render_ds_details(ds: &Option<DocString>) -> String {
        render(&DocStringRenderer(DSOpts::Details, ds))
    }
    fn render_ds_combined(ds: &Option<DocString>) -> String {
        render(&DocStringRenderer(DSOpts::Combined, ds))
    }
    fn sample_ds() -> Option<DocString> {
        DocString::from_docstring(DocStringKind::Rust, "Summary\n\nDetails")
    }
    fn sample_ds_no_details() -> Option<DocString> {
        DocString::from_docstring(DocStringKind::Rust, "Summary")
    }
    fn sample_type() -> Option<Type> {
        Some(Type {
            raw_type: "int".to_owned(),
        })
    }

    #[test]
    fn doc_file_code_block() {
        let expected_no_lang = textwrap::dedent(
            r#"
            ```
            foo
            bar
            ```
            "#,
        )
        .trim()
        .to_owned();

        let expected_python = textwrap::dedent(
            r#"
            ```python
            foo
            bar
            ```"#,
        )
        .trim()
        .to_owned();

        let no_lang = CodeBlock {
            language: None,
            contents: box "foo\nbar".to_owned(),
        };
        let python = CodeBlock {
            language: Some("python".to_owned()),
            contents: box "foo\nbar".to_owned(),
        };

        assert_eq!(expected_no_lang, render(&no_lang));
        assert_eq!(expected_python, render(&python));
    }

    #[test]
    fn doc_file_doc_string() {
        let without_docstring = None;
        let without_details = sample_ds_no_details();
        let with_details = sample_ds();

        assert_eq!(
            None,
            DocStringRenderer(DSOpts::Summary, &without_docstring)
                .generate_markdown(MarkdownFlavor::DocFile)
        );
        assert_eq!(
            None,
            DocStringRenderer(DSOpts::Details, &without_docstring)
                .generate_markdown(MarkdownFlavor::DocFile)
        );
        assert_eq!(
            None,
            DocStringRenderer(DSOpts::Combined, &without_docstring)
                .generate_markdown(MarkdownFlavor::DocFile)
        );

        assert_eq!(
            Some("Summary".to_owned()),
            DocStringRenderer(DSOpts::Summary, &without_details)
                .generate_markdown(MarkdownFlavor::DocFile)
        );
        assert_eq!(
            None,
            DocStringRenderer(DSOpts::Details, &without_details)
                .generate_markdown(MarkdownFlavor::DocFile)
        );
        assert_eq!(
            Some("Summary".to_owned()),
            DocStringRenderer(DSOpts::Combined, &without_details)
                .generate_markdown(MarkdownFlavor::DocFile)
        );

        assert_eq!(
            Some("Summary".to_owned()),
            DocStringRenderer(DSOpts::Summary, &with_details)
                .generate_markdown(MarkdownFlavor::DocFile)
        );
        assert_eq!(
            Some("Details".to_owned()),
            DocStringRenderer(DSOpts::Details, &with_details)
                .generate_markdown(MarkdownFlavor::DocFile)
        );
        assert_eq!(
            Some("Summary\n\nDetails".to_owned()),
            DocStringRenderer(DSOpts::Combined, &with_details)
                .generate_markdown(MarkdownFlavor::DocFile)
        );
    }

    #[test]
    fn doc_file_function_details() {
        let ds = sample_ds();
        let ds_no_details = sample_ds_no_details();
        let typ = sample_type();

        fn params(with_docs: bool) -> Vec<Param> {
            let ds = if with_docs { sample_ds() } else { None };
            let typ = sample_type();
            vec![
                Param::Arg {
                    docs: ds.clone(),
                    typ: typ.clone(),
                    name: "p1".to_owned(),
                    default_value: Some("1".to_owned()),
                },
                Param::Arg {
                    docs: ds.clone(),
                    typ: typ.clone(),
                    name: "p2".to_owned(),
                    default_value: None,
                },
                Param::NoArgs,
                Param::Args {
                    docs: ds.clone(),
                    typ: typ.clone(),
                    name: "*p3".to_owned(),
                },
                Param::Kwargs {
                    docs: ds,
                    typ,
                    name: "**p4".to_owned(),
                },
            ]
        }

        let f1 = Function {
            docs: None,
            params: params(false),
            ret: Return {
                typ: typ.clone(),
                docs: None,
            },
        };
        let f2 = Function {
            docs: ds_no_details.clone(),
            params: params(false),
            ret: Return {
                typ: typ.clone(),
                docs: None,
            },
        };
        let f3 = Function {
            docs: ds.clone(),
            params: params(true),
            ret: Return {
                typ: typ.clone(),
                docs: None,
            },
        };
        let f4 = Function {
            docs: ds.clone(),
            params: params(true),
            ret: Return {
                typ,
                docs: ds.clone(),
            },
        };

        fn prototype(name: &str, f: &Function) -> String {
            render(&CodeBlock {
                language: Some("python".to_owned()),
                contents: box TypeRenderer::Function {
                    function_name: Some(name.to_owned()),
                    show_param_details: true,
                    max_args_before_multiline: Some(6),
                    f,
                },
            })
        }
        let f1_prototype = prototype("f1", &f1);
        let f2_prototype = prototype("f2", &f2);
        let f3_prototype = prototype("f3", &f3);
        let f4_prototype = prototype("f4", &f4);

        let rendered_params = render(&Table(
            TableHeader(&["Name", "Details"]),
            vec![
                TableRow(vec![
                    box Code(box "p1".to_owned()),
                    box render_ds_combined(&ds),
                ]),
                TableRow(vec![
                    box Code(box "p2".to_owned()),
                    box render_ds_combined(&ds),
                ]),
                TableRow(vec![
                    box Code(box "*p3".to_owned()),
                    box render_ds_combined(&ds),
                ]),
                TableRow(vec![
                    box Code(box "**p4".to_owned()),
                    box render_ds_combined(&ds),
                ]),
            ],
        ));

        let expected_f1 = format!("## f1\n\n{prototype}", prototype = f1_prototype);
        let expected_f2 = format!(
            "## f2\n\n{prototype}\n\n{summary}",
            prototype = f2_prototype,
            summary = render_ds_summary(&ds_no_details)
        );
        let expected_f3 = format!(
            "## f3\n\n{prototype}\n\n{summary}\n\n### Parameters\n\n{parameters}\n\n### Details\n\n{details}",
            prototype = f3_prototype,
            summary = render_ds_summary(&ds),
            parameters = rendered_params,
            details = render_ds_details(&ds)
        );
        let expected_f4 = format!(
            "## f4\n\n{prototype}\n\n{summary}\n\n### Parameters\n\n{parameters}\n\n### Details\n\n{details}\n\n### Returns\n\n{returns}",
            prototype = f4_prototype,
            summary = render_ds_summary(&ds),
            parameters = rendered_params,
            details = render_ds_details(&ds),
            returns = render_ds_combined(&ds)
        );

        assert_eq!(
            expected_f1,
            render(&FunctionDetailsRenderer {
                name: "f1".to_owned(),
                f: &f1
            })
        );
        assert_eq!(
            expected_f2,
            render(&FunctionDetailsRenderer {
                name: "f2".to_owned(),
                f: &f2
            })
        );
        assert_eq!(
            expected_f3,
            render(&FunctionDetailsRenderer {
                name: "f3".to_owned(),
                f: &f3
            })
        );
        assert_eq!(
            expected_f4,
            render(&FunctionDetailsRenderer {
                name: "f4".to_owned(),
                f: &f4
            })
        );
    }

    #[test]
    fn doc_file_literal() {
        assert_eq!("`foo`", render(&Code(box "foo".to_owned())));
    }

    #[test]
    fn doc_file_module() {
        let ds = sample_ds();
        let ds_render = render_ds_combined(&ds);

        let doc_without_loc = Doc {
            id: Identifier {
                name: "some_module".to_owned(),
                location: None,
            },
            item: DocItem::Module(Module { docs: ds.clone() }),
            custom_attrs: HashMap::default(),
        };
        let doc_with_loc = Doc {
            id: Identifier {
                name: "some_module".to_owned(),
                location: Some(Location {
                    path: "/foo/bar/baz.bzl".to_owned(),
                    position: None,
                }),
            },
            item: DocItem::Module(Module { docs: ds }),
            custom_attrs: HashMap::default(),
        };

        let expected_doc_without_loc = format!("# some_module\n\n{}", ds_render);
        let expected_doc_with_loc = format!("# /foo/bar/baz.bzl\n\n{}", ds_render);

        assert_eq!(expected_doc_without_loc, render(&doc_without_loc));
        assert_eq!(expected_doc_with_loc, render(&doc_with_loc));
    }

    #[test]
    fn doc_file_object() {
        let ds = sample_ds();
        let typ = sample_type();

        let p1 = Property {
            docs: ds.clone(),
            typ: None,
        };
        let p2 = Property {
            docs: ds.clone(),
            typ: typ.clone(),
        };
        let f1 = Function {
            docs: ds.clone(),
            params: vec![Param::Arg {
                docs: ds.clone(),
                typ: typ.clone(),
                name: "p1".to_owned(),
                default_value: None,
            }],
            ret: Return {
                typ,
                docs: ds.clone(),
            },
        };

        let member_table = render(&Table(
            TableHeader(&["Member", "Type", "Description"]),
            vec![
                TableRow(vec![
                    box "f1".to_owned(),
                    box Code(box TypeRenderer::Function {
                        show_param_details: false,
                        max_args_before_multiline: None,
                        function_name: None,
                        f: &f1,
                    }),
                    box render_ds_summary(&ds),
                ]),
                TableRow(vec![
                    box "p1".to_owned(),
                    box Code(box TypeRenderer::Type(&p1.typ)),
                    box render_ds_summary(&ds),
                ]),
                TableRow(vec![
                    box "p2".to_owned(),
                    box Code(box TypeRenderer::Type(&p2.typ)),
                    box render_ds_summary(&ds),
                ]),
            ],
        ));
        let p1_details = render(&PropertyDetailsRenderer {
            name: "p1".to_owned(),
            p: &p1,
        });
        let p2_details = render(&PropertyDetailsRenderer {
            name: "p2".to_owned(),
            p: &p2,
        });
        let f1_details = render(&FunctionDetailsRenderer {
            name: "f1".to_owned(),
            f: &f1,
        });

        let expected_without_docs_root = format!(
            "# foo1\n\n### Members\n\n{member_table}\n\n\n{f1}\n\n---\n{p1}\n\n---\n{p2}",
            member_table = member_table,
            p1 = p1_details,
            p2 = p2_details,
            f1 = f1_details,
        );
        let expected_without_docs_non_root = format!(
            "## foo2\n\n### Members\n\n{member_table}\n\n\n{f1}\n\n---\n{p1}\n\n---\n{p2}",
            member_table = member_table,
            p1 = p1_details,
            p2 = p2_details,
            f1 = f1_details,
        );
        let expected_with_docs_root = format!(
            "# foo3\n\n{ds}\n\n### Members\n\n{member_table}\n\n\n{f1}\n\n---\n{p1}\n\n---\n{p2}",
            ds = render_ds_combined(&ds),
            member_table = member_table,
            p1 = p1_details,
            p2 = p2_details,
            f1 = f1_details,
        );

        let members = vec![
            ("p1".to_owned(), Member::Property(p1)),
            ("p2".to_owned(), Member::Property(p2)),
            ("f1".to_owned(), Member::Function(f1)),
        ];

        let without_docs_root = Doc {
            id: Identifier {
                name: "foo1".to_owned(),
                location: None,
            },
            item: DocItem::Object(Object {
                docs: None,
                members: members.clone(),
            }),
            custom_attrs: HashMap::default(),
        };
        let without_docs_not_root = Doc {
            id: Identifier {
                name: "foo2".to_owned(),
                location: Some(Location {
                    path: "/foo.bzl".to_owned(),
                    position: None,
                }),
            },
            item: DocItem::Object(Object {
                docs: None,
                members: members.clone(),
            }),
            custom_attrs: HashMap::default(),
        };
        let with_docs_root = Doc {
            id: Identifier {
                name: "foo3".to_owned(),
                location: None,
            },
            item: DocItem::Object(Object { docs: ds, members }),
            custom_attrs: HashMap::default(),
        };

        assert_eq!(expected_without_docs_root, render(&without_docs_root));
        assert_eq!(
            expected_without_docs_non_root,
            render(&without_docs_not_root)
        );
        assert_eq!(expected_with_docs_root, render(&with_docs_root));
    }

    #[test]
    fn doc_file_property_details() {
        let ds = sample_ds();
        let ds_no_details = sample_ds_no_details();
        let typ = sample_type();

        let expected_no_docs = format!("## foo1 : {}", render(&Code(box TypeRenderer::Type(&typ))));
        let expected_no_details = format!(
            "## foo2 : {}\n\n{}",
            render(&Code(box TypeRenderer::Type(&typ))),
            render_ds_summary(&ds_no_details)
        );
        let expected_with_docs = format!(
            "## foo3 : {}\n\n{}\n\n{}",
            render(&Code(box TypeRenderer::Type(&typ))),
            render_ds_summary(&ds),
            render_ds_details(&ds)
        );

        assert_eq!(
            expected_no_docs,
            render(&PropertyDetailsRenderer {
                name: "foo1".to_owned(),
                p: &Property {
                    docs: None,
                    typ: typ.clone()
                }
            })
        );
        assert_eq!(
            expected_no_details,
            render(&PropertyDetailsRenderer {
                name: "foo2".to_owned(),
                p: &Property {
                    docs: ds_no_details,
                    typ: typ.clone()
                }
            })
        );
        assert_eq!(
            expected_with_docs,
            render(&PropertyDetailsRenderer {
                name: "foo3".to_owned(),
                p: &Property { docs: ds, typ }
            })
        );
    }

    #[test]
    fn doc_file_table() {
        let header = TableHeader(&["column1", "col2", "column3"]);
        let rows = vec![
            TableRow(vec![
                box "h1".to_owned(),
                box "h2".to_owned(),
                box Code(box "h3".to_owned()),
            ]),
            TableRow(vec![
                box "h4".to_owned(),
                box "h5".to_owned(),
                box Code(box "h6".to_owned()),
            ]),
        ];
        let expected = format!(
            "{}\n{}\n{}",
            render(&header),
            render(rows.get(0).unwrap()),
            render(rows.get(1).unwrap())
        );

        let table = Table(header, rows);

        assert_eq!(expected, render(&table));
    }

    #[test]
    fn doc_file_table_header() {
        let expected = "| column1 | col2 | column3 |\n|---------|------|---------|";
        let header = TableHeader(&["column1", "col2", "column3"]);

        assert_eq!(expected, render(&header));
    }

    #[test]
    fn doc_file_table_row() {
        let expected = "| h1 | h2 | `h3` |";
        let row = TableRow(vec![
            box "h1".to_owned(),
            box "h2".to_owned(),
            box Code(box "h3".to_owned()),
        ]);

        assert_eq!(expected, render(&row));
    }

    #[test]
    fn doc_file_type_property() {
        let ds_no_details = sample_ds_no_details();
        let render_no_details = render_ds_combined(&ds_no_details);
        let ds_with_both = sample_ds();
        let render_with_both = render_ds_combined(&ds_with_both);
        let typ = sample_type();

        let no_docs = Property {
            docs: None,
            typ: typ.clone(),
        };
        let no_details = Property {
            docs: ds_no_details,
            typ: typ.clone(),
        };
        let with_summary_and_details = Property {
            docs: ds_with_both,
            typ,
        };

        let expected_no_docs = "## foo : `int`";
        let expected_no_details = format!("## foo : `int`\n\n{}", render_no_details);
        let expected_with_summary_and_details = format!("## foo : `int`\n\n{}", render_with_both);

        assert_eq!(
            expected_no_docs,
            render(&PropertyDetailsRenderer {
                name: "foo".to_owned(),
                p: &no_docs
            })
        );
        assert_eq!(
            expected_no_details,
            render(&PropertyDetailsRenderer {
                name: "foo".to_owned(),
                p: &no_details
            })
        );
        assert_eq!(
            expected_with_summary_and_details,
            render(&PropertyDetailsRenderer {
                name: "foo".to_owned(),
                p: &with_summary_and_details
            })
        );
    }
}
