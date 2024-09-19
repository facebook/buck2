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

use itertools::Itertools;

use crate::docs::DocFunction;
use crate::docs::DocItem;
use crate::docs::DocMember;
use crate::docs::DocParam;
use crate::docs::DocProperty;
use crate::docs::DocString;
use crate::typing::Ty;

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
        TypeRenderer::Type(&property.typ).render_markdown()
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
    let mut param_list: Option<String> = None;
    for p in params {
        let (name, docs) = match p {
            DocParam::Arg { name, docs, .. } => (name, docs),
            DocParam::OnlyNamedAfter | DocParam::OnlyPosBefore => continue,
            DocParam::Args { name, docs, .. } => (name, docs),
            DocParam::Kwargs { name, docs, .. } => (name, docs),
        };

        if docs.is_none() {
            continue;
        }

        let param_list = param_list.get_or_insert_with(String::new);

        let docs = render_doc_string(DSOpts::Combined, docs).unwrap_or_default();

        let mut lines_iter = docs.lines();
        if let Some(first_line) = lines_iter.next() {
            let _ = writeln!(param_list, "* `{name}`: {first_line}");
            for line in lines_iter {
                let _ = writeln!(param_list, "  {line}");
            }
        } else {
            let _ = writeln!(param_list, "* `{name}`");
        }
    }

    param_list
}

fn render_function(name: &str, function: &DocFunction) -> String {
    let prototype = render_code_block(
        &(TypeRenderer::Function {
            function_name: name,
            f: function,
        }
        .render_markdown()),
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

fn render_members<'a>(
    name: &str,
    docs: &Option<DocString>,
    prefix: &str,
    members: impl IntoIterator<Item = (&'a str, DocMember)>,
) -> String {
    let summary = render_doc_string(DSOpts::Combined, docs)
        .map(|s| format!("\n\n{}", s))
        .unwrap_or_default();

    let member_details: Vec<String> = members
        .into_iter()
        .sorted_by(|(l_m, _), (r_m, _)| l_m.cmp(r_m))
        .map(|(child, member)| render_doc_member(&format!("{prefix}{child}"), &member))
        .collect();
    let members_details = member_details.join("\n\n---\n\n");

    format!("# {name}{summary}\n\n{members_details}")
}

/// Used by LSP.
pub fn render_doc_item(name: &str, item: &DocItem) -> String {
    match item {
        DocItem::Module(m) => render_members(
            name,
            &m.docs,
            "",
            m.members.iter().filter_map(|(n, m)| {
                m.try_as_member_with_collapsed_object()
                    .ok()
                    .map(|m| (&**n, m))
            }),
        ),
        DocItem::Type(o) => render_members(
            &format!("`{name}` type"),
            &o.docs,
            &format!("{name}."),
            o.members.iter().map(|(n, m)| (&**n, m.clone())),
        ),
        DocItem::Member(DocMember::Function(f)) => render_function(name, f),
        DocItem::Member(DocMember::Property(p)) => render_property(name, p),
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

impl<'a> TypeRenderer<'a> {
    fn render_markdown(&self) -> String {
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

        match self {
            TypeRenderer::Type(t) => raw_type(t),
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
                    DocParam::OnlyNamedAfter => "*".to_owned(),
                    DocParam::OnlyPosBefore => "/".to_owned(),
                    DocParam::Args {
                        tuple_elem_ty,
                        name,
                        ..
                    } => {
                        format!("{}{}", name, raw_type_prefix(": ", tuple_elem_ty))
                    }
                    DocParam::Kwargs {
                        dict_value_ty,
                        name,
                        ..
                    } => {
                        format!("{}{}", name, raw_type_prefix(": ", dict_value_ty))
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
                    format!("{}(\n    {}\n){}", prefix, chunked_params, ret_type)
                } else {
                    single_line_result
                }
            }
        }
    }
}

fn render_code_block(contents: &str) -> String {
    format!("```python\n{contents}\n```")
}
