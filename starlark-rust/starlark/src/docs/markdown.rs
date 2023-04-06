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

use std::collections::HashMap;

use dupe::Dupe;
use itertools::Itertools;

use crate::docs::Doc;
use crate::docs::DocItem;
use crate::docs::DocString;
use crate::docs::Function;
use crate::docs::Identifier;
use crate::docs::Member;
use crate::docs::Module;
use crate::docs::Object;
use crate::docs::Param;
use crate::docs::Property;
use crate::docs::Type;

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
pub trait RenderMarkdown {
    /// Generate markdown of the given flavor if possible. For some types, there may not be
    /// any useful documentation available.
    fn render_markdown_opt(&self, flavor: MarkdownFlavor) -> Option<String>;

    /// Convenience method that invokes `RenderMarkdown::render_markdown_opt`, and returns an
    /// empty string if that is `None`
    fn render_markdown(&self, flavor: MarkdownFlavor) -> String {
        self.render_markdown_opt(flavor).unwrap_or_default()
    }
}

impl RenderMarkdown for String {
    fn render_markdown_opt(&self, _flavor: MarkdownFlavor) -> Option<String> {
        Some(self.clone())
    }
}

impl RenderMarkdown for str {
    fn render_markdown_opt(&self, _flavor: MarkdownFlavor) -> Option<String> {
        Some(self.to_owned())
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

fn render_doc_string(opts: DSOpts, string: &Option<DocString>) -> Option<String> {
    string.as_ref().and_then(|d| match opts {
        DSOpts::Summary => Some(d.summary.clone()),
        DSOpts::Details => d.details.clone(),
        DSOpts::Combined => Some(match &d.details {
            Some(details) => format!("{}\n\n{}", d.summary, details),
            None => d.summary.clone(),
        }),
    })
}

fn render_property(name: &str, property: &Property) -> String {
    let mut header = format!("## {}", name);
    if property.typ.is_some() {
        header += &format!(
            " : `{}`",
            TypeRenderer::Type(&property.typ).render_markdown(MarkdownFlavor::DocFile)
        );
    };
    let summary = render_doc_string(DSOpts::Summary, &property.docs);
    let details = render_doc_string(DSOpts::Details, &property.docs);

    let mut body = header;
    if let Some(summary) = summary {
        body.push_str("\n\n");
        body.push_str(&summary);
    }
    if let Some(details) = details {
        body.push_str("\n\n");
        body.push_str(&details);
    }

    body
}

/// If there are any parameter docs to render, render them as a list.
fn render_function_parameters(params: &[Param]) -> Option<String> {
    // Filter out parameters without docs
    let has_docs: Vec<_> = params
        .iter()
        .filter(|p| match p {
            Param::Arg { docs, .. } => docs.is_some(),
            Param::NoArgs => false,
            Param::Args { docs, .. } => docs.is_some(),
            Param::Kwargs { docs, .. } => docs.is_some(),
        })
        .collect();

    if has_docs.is_empty() {
        return None;
    }

    let param_list: String = has_docs
        .iter()
        .filter_map(|p| match p {
            Param::Arg { name, docs, .. } => {
                Some((name.clone(), render_doc_string(DSOpts::Combined, docs)))
            }
            Param::NoArgs => None,
            Param::Args { name, docs, .. } => {
                Some((name.clone(), render_doc_string(DSOpts::Combined, docs)))
            }
            Param::Kwargs { name, docs, .. } => {
                Some((name.clone(), render_doc_string(DSOpts::Combined, docs)))
            }
        })
        .map(|(name, docs)| {
            ParamList(name, docs.unwrap_or_default()).render_markdown(MarkdownFlavor::DocFile)
        })
        .collect();
    Some(param_list)
}

fn render_function(name: &str, function: &Function) -> String {
    let prototype = CodeBlock {
        language: Some("python".to_owned()),
        contents: Box::new(TypeRenderer::Function {
            function_name: name,
            f: function,
        }),
    };
    let header = format!(
        "## {}\n\n{}",
        name,
        prototype.render_markdown(MarkdownFlavor::DocFile)
    );
    let summary = render_doc_string(DSOpts::Summary, &function.docs);
    let details = render_doc_string(DSOpts::Details, &function.docs);

    let parameter_docs = render_function_parameters(&function.params);
    let return_docs = render_doc_string(DSOpts::Combined, &function.ret.docs);

    let mut body = header;
    if let Some(summary) = summary {
        body.push_str("\n\n");
        body.push_str(&summary);
    }
    if let Some(parameter_docs) = parameter_docs {
        body.push_str("\n\n#### Parameters\n\n");
        body.push_str(&parameter_docs);
    }
    if let Some(details) = details {
        body.push_str("\n\n#### Details\n\n");
        body.push_str(&details);
    }
    if let Some(returns) = return_docs {
        body.push_str("\n\n#### Returns\n\n");
        body.push_str(&returns);
    }

    body
}

/// Render a top level module.
fn render_module(name: &str, module: &Module) -> String {
    let docs = render_doc_string(DSOpts::Combined, &module.docs).unwrap_or_default();
    let mut res = format!("# {}\n\n{}", name, docs);

    for (k, v) in &module.members {
        res.push('\n');
        match v {
            Some(v) => res.push_str(
                &(Doc {
                    id: Identifier {
                        name: k.clone(),
                        location: None,
                    },
                    item: v.clone(),
                    custom_attrs: HashMap::new(),
                }
                .render_markdown_opt(MarkdownFlavor::DocFile)
                .unwrap_or_default()),
            ),
            None => res.push_str(&format!("{}: UNKNOWN", k)),
        }
        res.push('\n');
    }

    res
}

fn render_object(name: &str, object: &Object) -> String {
    // If this is a native, top level object, render it with a larger
    // header. Sub objects will be listed along side members, so use
    // smaller headers there.
    let title = format!("# {name}");
    let summary = render_doc_string(DSOpts::Combined, &object.docs)
        .map(|s| format!("\n\n{}", s))
        .unwrap_or_default();

    let member_details: Vec<String> = object
        .members
        .iter()
        .sorted_by(|(l_m, _), (r_m, _)| l_m.cmp(r_m))
        .map(|(name, member)| render_member(name, member))
        .collect();
    let members_details = member_details.join("\n\n---\n\n");

    format!("{title}{summary}\n\n{members_details}")
}

fn render_doc_item(name: &str, item: &DocItem) -> String {
    match &item {
        DocItem::Module(m) => render_module(name, m),
        DocItem::Object(o) => render_object(name, o),
        DocItem::Function(f) => render_function(name, f),
        DocItem::Property(p) => render_property(name, p),
    }
}

impl RenderMarkdown for Doc {
    fn render_markdown_opt(&self, flavor: MarkdownFlavor) -> Option<String> {
        match flavor {
            MarkdownFlavor::DocFile => Some(render_doc_item(&self.id.name, &self.item)),
            MarkdownFlavor::LspSummary => None,
        }
    }
}

fn render_member(name: &str, member: &Member) -> String {
    match member {
        Member::Property(p) => render_property(name, p),
        Member::Function(f) => render_function(name, f),
    }
}

/// Any functions with more parameters than this will have
/// their prototype split over multiple lines. Otherwise, it is returned as
/// a single line.
const MAX_ARGS_BEFORE_MULTILINE: usize = 3;

/// Render a "type". This is either a [`Type`] object, or details about a function to
/// produce a function prototype.
enum TypeRenderer<'a> {
    /// A general "type".
    Type(&'a Option<Type>),
    /// A function, with some extra formatting options.
    Function {
        /// The function name in the prototype as well.
        function_name: &'a str,
        f: &'a Function,
    },
}

impl<'a> RenderMarkdown for TypeRenderer<'a> {
    fn render_markdown_opt(&self, flavor: MarkdownFlavor) -> Option<String> {
        fn raw_type(t: &Option<Type>) -> String {
            match t {
                Some(t) if !t.raw_type.is_empty() => t.raw_type.clone(),
                _ => "\"\"".to_owned(),
            }
        }

        fn raw_type_prefix(prefix: &str, t: &Option<Type>) -> String {
            if t.is_some() {
                format!("{prefix}{}", raw_type(t))
            } else {
                String::new()
            }
        }

        match flavor {
            MarkdownFlavor::DocFile => match self {
                TypeRenderer::Type(t) => Some(raw_type(t)),
                TypeRenderer::Function { function_name, f } => {
                    let mut params = f.params.iter().map(|p| match p {
                        Param::Arg {
                            typ,
                            name,
                            default_value,
                            ..
                        } => {
                            let type_string = raw_type_prefix(": ", typ);
                            match default_value {
                                Some(v) => format!("{}{} = {}", name, type_string, v),
                                None => format!("{}{}", name, type_string),
                            }
                        }
                        Param::NoArgs => "*".to_owned(),
                        Param::Args { typ, name, .. } => {
                            format!("{}{}", name, raw_type_prefix(": ", typ))
                        }
                        Param::Kwargs { typ, name, .. } => {
                            format!("{}{}", name, raw_type_prefix(": ", typ))
                        }
                    });

                    let ret_type = raw_type_prefix(" -> ", &f.ret.typ);
                    let prefix = format!("def {}", function_name);
                    if MAX_ARGS_BEFORE_MULTILINE < f.params.len() {
                        let chunked_params = params.join(",\n    ");
                        Some(format!(
                            "{}(\n    {}\n){}",
                            prefix, chunked_params, ret_type
                        ))
                    } else {
                        Some(format!("{}({}){}", prefix, params.join(", "), ret_type))
                    }
                }
            },
            MarkdownFlavor::LspSummary => None,
        }
    }
}

/// A code block that optionally has a language. Note that this will always take multiple
/// lines, so may not be ideal for tables at the moment.
struct CodeBlock<'a> {
    language: Option<String>,
    contents: Box<dyn RenderMarkdown + 'a>,
}

impl<'a> RenderMarkdown for CodeBlock<'a> {
    fn render_markdown_opt(&self, flavor: MarkdownFlavor) -> Option<String> {
        match flavor {
            MarkdownFlavor::DocFile => self.contents.render_markdown_opt(flavor).map(|contents| {
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

/// A simple list to represent "`parameter`: definition" pais
struct ParamList(String, String);

impl RenderMarkdown for ParamList {
    fn render_markdown_opt(&self, flavor: MarkdownFlavor) -> Option<String> {
        match flavor {
            MarkdownFlavor::DocFile => Some(format!("* `{}`: {}\n", self.0, self.1,)),
            MarkdownFlavor::LspSummary => None,
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use starlark_map::small_map::SmallMap;
    use starlark_map::smallmap;

    use super::*;
    use crate::docs::Doc;
    use crate::docs::DocItem;
    use crate::docs::DocString;
    use crate::docs::DocStringKind;
    use crate::docs::Function;
    use crate::docs::Identifier;
    use crate::docs::Location;
    use crate::docs::Member;
    use crate::docs::Module;
    use crate::docs::Object;
    use crate::docs::Param;
    use crate::docs::Property;
    use crate::docs::Return;
    use crate::docs::Type;

    fn render(renderer: &dyn RenderMarkdown) -> String {
        renderer
            .render_markdown_opt(MarkdownFlavor::DocFile)
            .unwrap()
    }

    fn render_ds_summary(ds: &Option<DocString>) -> String {
        render_doc_string(DSOpts::Summary, ds).unwrap_or_default()
    }
    fn render_ds_details(ds: &Option<DocString>) -> String {
        render_doc_string(DSOpts::Details, ds).unwrap_or_default()
    }
    fn render_ds_combined(ds: &Option<DocString>) -> String {
        render_doc_string(DSOpts::Combined, ds).unwrap_or_default()
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
            contents: Box::new("foo\nbar".to_owned()),
        };
        let python = CodeBlock {
            language: Some("python".to_owned()),
            contents: Box::new("foo\nbar".to_owned()),
        };

        assert_eq!(expected_no_lang, render(&no_lang));
        assert_eq!(expected_python, render(&python));
    }

    #[test]
    fn doc_file_doc_string() {
        let without_docstring = None;
        let without_details = sample_ds_no_details();
        let with_details = sample_ds();

        assert_eq!(None, render_doc_string(DSOpts::Summary, &without_docstring));
        assert_eq!(None, render_doc_string(DSOpts::Details, &without_docstring));
        assert_eq!(
            None,
            render_doc_string(DSOpts::Combined, &without_docstring)
        );

        assert_eq!(
            Some("Summary".to_owned()),
            render_doc_string(DSOpts::Summary, &without_details)
        );
        assert_eq!(None, render_doc_string(DSOpts::Details, &without_details));
        assert_eq!(
            Some("Summary".to_owned()),
            render_doc_string(DSOpts::Combined, &without_details)
        );

        assert_eq!(
            Some("Summary".to_owned()),
            render_doc_string(DSOpts::Summary, &with_details)
        );
        assert_eq!(
            Some("Details".to_owned()),
            render_doc_string(DSOpts::Details, &with_details)
        );
        assert_eq!(
            Some("Summary\n\nDetails".to_owned()),
            render_doc_string(DSOpts::Combined, &with_details)
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
                contents: Box::new(TypeRenderer::Function {
                    function_name: name,
                    f,
                }),
            })
        }
        let f1_prototype = prototype("f1", &f1);
        let f2_prototype = prototype("f2", &f2);
        let f3_prototype = prototype("f3", &f3);
        let f4_prototype = prototype("f4", &f4);

        let rendered_params: String = vec![
            ParamList("p1".to_owned(), render_ds_combined(&ds)),
            ParamList("p2".to_owned(), render_ds_combined(&ds)),
            ParamList("*p3".to_owned(), render_ds_combined(&ds)),
            ParamList("**p4".to_owned(), render_ds_combined(&ds)),
        ]
        .iter()
        .map(|p| render(p))
        .collect();

        let expected_f1 = format!("## f1\n\n{prototype}", prototype = f1_prototype);
        let expected_f2 = format!(
            "## f2\n\n{prototype}\n\n{summary}",
            prototype = f2_prototype,
            summary = render_ds_summary(&ds_no_details)
        );
        let expected_f3 = format!(
            "## f3\n\n{prototype}\n\n{summary}\n\n#### Parameters\n\n{parameters}\n\n#### Details\n\n{details}",
            prototype = f3_prototype,
            summary = render_ds_summary(&ds),
            parameters = rendered_params,
            details = render_ds_details(&ds)
        );
        let expected_f4 = format!(
            "## f4\n\n{prototype}\n\n{summary}\n\n#### Parameters\n\n{parameters}\n\n#### Details\n\n{details}\n\n#### Returns\n\n{returns}",
            prototype = f4_prototype,
            summary = render_ds_summary(&ds),
            parameters = rendered_params,
            details = render_ds_details(&ds),
            returns = render_ds_combined(&ds)
        );

        assert_eq!(expected_f1, render_function("f1", &f1));
        assert_eq!(expected_f2, render_function("f2", &f2));
        assert_eq!(expected_f3, render_function("f3", &f3));
        assert_eq!(expected_f4, render_function("f4", &f4));
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
            item: DocItem::Module(Module {
                docs: ds.clone(),
                members: SmallMap::new(),
            }),
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
            item: DocItem::Module(Module {
                docs: ds,
                members: SmallMap::new(),
            }),
            custom_attrs: HashMap::default(),
        };

        let expected_doc_without_loc = format!("# some_module\n\n{}", ds_render);
        assert_eq!(expected_doc_without_loc, render(&doc_without_loc));
        assert_eq!(expected_doc_without_loc, render(&doc_with_loc));
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

        let p1_details = render_property("p1", &p1);
        let p2_details = render_property("p2", &p2);
        let f1_details = render_function("f1", &f1);

        let expected_without_docs_root = format!(
            "# foo1\n\n{f1}\n\n---\n\n{p1}\n\n---\n\n{p2}",
            p1 = p1_details,
            p2 = p2_details,
            f1 = f1_details,
        );
        let expected_without_docs_non_root = format!(
            "# foo2\n\n{f1}\n\n---\n\n{p1}\n\n---\n\n{p2}",
            p1 = p1_details,
            p2 = p2_details,
            f1 = f1_details,
        );
        let expected_with_docs_root = format!(
            "# foo3\n\n{ds}\n\n{f1}\n\n---\n\n{p1}\n\n---\n\n{p2}",
            ds = render_ds_combined(&ds),
            p1 = p1_details,
            p2 = p2_details,
            f1 = f1_details,
        );

        let members = smallmap! {
            "p1".to_owned() => Member::Property(p1),
            "p2".to_owned() => Member::Property(p2),
            "f1".to_owned() => Member::Function(f1),
        };

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

        let expected_no_docs = format!("## foo1 : `{}`", render(&TypeRenderer::Type(&typ)));
        let expected_no_details = format!(
            "## foo2 : `{}`\n\n{}",
            render(&TypeRenderer::Type(&typ)),
            render_ds_summary(&ds_no_details)
        );
        let expected_with_docs = format!(
            "## foo3 : `{}`\n\n{}\n\n{}",
            render(&TypeRenderer::Type(&typ)),
            render_ds_summary(&ds),
            render_ds_details(&ds)
        );

        assert_eq!(
            expected_no_docs,
            render_property(
                "foo1",
                &Property {
                    docs: None,
                    typ: typ.clone()
                }
            )
        );
        assert_eq!(
            expected_no_details,
            render_property(
                "foo2",
                &Property {
                    docs: ds_no_details,
                    typ: typ.clone()
                }
            )
        );
        assert_eq!(
            expected_with_docs,
            render_property("foo3", &Property { docs: ds, typ })
        );
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

        assert_eq!(expected_no_docs, render_property("foo", &no_docs));
        assert_eq!(expected_no_details, render_property("foo", &no_details));
        assert_eq!(
            expected_with_summary_and_details,
            render_property("foo", &with_summary_and_details)
        );
    }
}
