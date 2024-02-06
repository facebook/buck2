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

use std::fmt::Write;
use std::slice;

use dupe::Dupe;
use itertools::Itertools;
use starlark_map::small_map::SmallMap;

use crate::docs::Doc;
use crate::docs::DocFunction;
use crate::docs::DocItem;
use crate::docs::DocMember;
use crate::docs::DocModule;
use crate::docs::DocObject;
use crate::docs::DocParam;
use crate::docs::DocProperty;
use crate::docs::DocString;
use crate::typing::Ty;

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

/// Function names can have underscores in them, which are markdown,
/// so escape them if we render them outside a codeblock.
fn escape_name(name: &str) -> String {
    name.replace('_', "\\_")
}

fn render_property(name: &str, property: &DocProperty) -> String {
    let prototype = render_code_block(&format!(
        "{name}: {}",
        TypeRenderer::Type(&property.typ).render_markdown(MarkdownFlavor::DocFile)
    ));
    let header = format!("## {}\n\n{prototype}", escape_name(name));
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
fn render_function_parameters(params: &[DocParam]) -> Option<String> {
    // Filter out parameters without docs
    let has_docs: Vec<_> = params
        .iter()
        .filter(|p| match p {
            DocParam::Arg { docs, .. } => docs.is_some(),
            DocParam::NoArgs | DocParam::OnlyPosBefore => false,
            DocParam::Args { docs, .. } => docs.is_some(),
            DocParam::Kwargs { docs, .. } => docs.is_some(),
        })
        .collect();

    if has_docs.is_empty() {
        return None;
    }

    let param_list: String = has_docs
        .iter()
        .filter_map(|p| match p {
            DocParam::Arg { name, docs, .. } => Some((name, docs)),
            DocParam::NoArgs | DocParam::OnlyPosBefore => None,
            DocParam::Args { name, docs, .. } => Some((name, docs)),
            DocParam::Kwargs { name, docs, .. } => Some((name, docs)),
        })
        .fold(String::new(), |mut output, (name, docs)| {
            let docs = render_doc_string(DSOpts::Combined, docs).unwrap_or_default();

            let mut lines_iter = docs.lines();
            let first_line = lines_iter.next().unwrap();
            let rest_of_lines: Vec<&str> = lines_iter.collect();

            let _ = writeln!(output, "* `{name}`: {first_line}");
            for line in &rest_of_lines {
                let _ = writeln!(output, "  {line}");
            }
            output
        });
    Some(param_list)
}

fn render_function(name: &str, function: &DocFunction) -> String {
    let prototype = render_code_block(
        &(TypeRenderer::Function {
            function_name: name,
            f: function,
        }
        .render_markdown(MarkdownFlavor::DocFile)),
    );
    let header = format!("## {}\n\n{prototype}", escape_name(name));
    let summary = render_doc_string(DSOpts::Summary, &function.docs);
    let details = render_doc_string(DSOpts::Details, &function.docs);

    let parameter_docs = render_function_parameters(&function.params);
    let return_docs = render_doc_string(DSOpts::Combined, &function.ret.docs);

    let mut body = header;
    if let Some(summary) = &summary {
        body.push_str("\n\n");
        body.push_str(summary);
    }
    if let Some(parameter_docs) = &parameter_docs {
        body.push_str("\n\n#### Parameters\n\n");
        body.push_str(parameter_docs);
    }
    if let Some(returns) = &return_docs {
        body.push_str("\n\n#### Returns\n\n");
        body.push_str(returns);
    }
    if let Some(dot_type) = function.as_type.as_ref().and_then(|t| t.as_name()) {
        body.push_str("\n\n#### `.type` attribute\n\n");
        body.push_str(&format!("Produces `{dot_type:?}`"));
    }
    if let Some(details) = &details {
        if parameter_docs.is_some() || return_docs.is_some() || function.as_type.is_some() {
            body.push_str("\n\n#### Details\n\n");
        } else {
            // No need to aggressively separate the defaults from the summary if there
            // was nothing in between them. Just let it flow.
            body.push_str("\n\n");
        }
        body.push_str(details);
    }

    body
}

fn render_members(
    name: &str,
    object: bool,
    docs: &Option<DocString>,
    members: &SmallMap<String, DocMember>,
) -> String {
    // If this is a native, top level object, render it with a larger
    // header. Sub objects will be listed along side members, so use
    // smaller headers there.
    let title = if object {
        format!("# `{name}` type")
    } else {
        format!("# {name}")
    };
    let summary = render_doc_string(DSOpts::Combined, docs)
        .map(|s| format!("\n\n{}", s))
        .unwrap_or_default();

    let prefix = if object {
        format!("{name}.")
    } else {
        String::new()
    };

    let member_details: Vec<String> = members
        .iter()
        .sorted_by(|(l_m, _), (r_m, _)| l_m.cmp(r_m))
        .map(|(child, member)| render_member(&format!("{prefix}{child}"), member))
        .collect();
    let members_details = member_details.join("\n\n---\n\n");

    format!("{title}{summary}\n\n{members_details}")
}

/// Render a top level module.
fn render_module(name: &str, module: &DocModule) -> String {
    render_members(name, false, &module.docs, &module.members)
}

fn render_object(name: &str, object: &DocObject) -> String {
    render_members(name, true, &object.docs, &object.members)
}

/// Used by LSP.
pub fn render_doc_item(name: &str, item: &DocItem) -> String {
    match item {
        DocItem::Module(m) => render_module(name, m),
        DocItem::Object(o) => render_object(name, o),
        DocItem::Function(f) => render_function(name, f),
        DocItem::Property(p) => render_property(name, p),
    }
}

/// Used by LSP.
pub fn render_doc_member(name: &str, item: &DocMember) -> String {
    match item {
        DocMember::Function(f) => render_function(name, f),
        DocMember::Property(p) => render_property(name, p),
    }
}

/// Used by LSP.
pub fn render_doc_param(item: &DocParam) -> String {
    render_function_parameters(slice::from_ref(item)).unwrap_or_default()
}

impl RenderMarkdown for Doc {
    fn render_markdown_opt(&self, flavor: MarkdownFlavor) -> Option<String> {
        match flavor {
            MarkdownFlavor::DocFile => Some(render_doc_item(&self.id.name, &self.item)),
            MarkdownFlavor::LspSummary => None,
        }
    }
}

fn render_member(name: &str, member: &DocMember) -> String {
    match member {
        DocMember::Property(p) => render_property(name, p),
        DocMember::Function(f) => render_function(name, f),
    }
}

/// Any functions with more parameters than this will have
/// their prototype split over multiple lines. Otherwise, it is returned as
/// a single line.
const MAX_ARGS_BEFORE_MULTILINE: usize = 3;

/// If the prototype ends up longer than this length, we'll split it anyway
const MAX_LENGTH_BEFORE_MULTILINE: usize = 80;

/// Render a "type". This is either a [`Type`] object, or details about a function to
/// produce a function prototype.
enum TypeRenderer<'a> {
    /// A general "type".
    Type(&'a Ty),
    /// A function, with some extra formatting options.
    Function {
        /// The function name in the prototype as well.
        function_name: &'a str,
        f: &'a DocFunction,
    },
}

impl<'a> RenderMarkdown for TypeRenderer<'a> {
    fn render_markdown_opt(&self, flavor: MarkdownFlavor) -> Option<String> {
        fn raw_type(t: &Ty) -> String {
            t.to_string()
        }

        fn raw_type_prefix(prefix: &str, t: &Ty) -> String {
            if t.is_any() {
                String::new()
            } else {
                format!("{prefix}{}", raw_type(t))
            }
        }

        match flavor {
            MarkdownFlavor::DocFile => match self {
                TypeRenderer::Type(t) => Some(raw_type(t)),
                TypeRenderer::Function { function_name, f } => {
                    let mut params = f.params.iter().map(|p| match p {
                        DocParam::Arg {
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
                        DocParam::NoArgs => "*".to_owned(),
                        DocParam::OnlyPosBefore => "/".to_owned(),
                        DocParam::Args { typ, name, .. } => {
                            format!("{}{}", name, raw_type_prefix(": ", typ))
                        }
                        DocParam::Kwargs { typ, name, .. } => {
                            format!("{}{}", name, raw_type_prefix(": ", typ))
                        }
                    });

                    let ret_type = raw_type_prefix(" -> ", &f.ret.typ);
                    let prefix = format!("def {}", function_name);
                    let single_line_result =
                        format!("{}({}){}", prefix, params.clone().join(", "), ret_type);

                    if f.params.len() > MAX_ARGS_BEFORE_MULTILINE
                        || single_line_result.len() > MAX_LENGTH_BEFORE_MULTILINE
                    {
                        let chunked_params = params.join(",\n    ");
                        Some(format!(
                            "{}(\n    {}\n){}",
                            prefix, chunked_params, ret_type
                        ))
                    } else {
                        Some(single_line_result)
                    }
                }
            },
            MarkdownFlavor::LspSummary => None,
        }
    }
}

fn render_code_block(contents: &str) -> String {
    format!("```python\n{contents}\n```")
}
