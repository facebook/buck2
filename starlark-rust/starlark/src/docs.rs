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

//! Types supporting documentation for code written in or for Starlark.

// TODO(nga): document it
#![allow(missing_docs)]

pub mod markdown;
mod parse;

use std::collections::HashMap;

use allocative::Allocative;
use itertools::Itertools;
pub use markdown::MarkdownFlavor;
pub use markdown::RenderMarkdown;
pub use parse::DocStringKind;
use serde::Serialize;
pub use starlark_derive::StarlarkDocs;
use starlark_map::small_map::SmallMap;

use crate as starlark;
use crate::typing::Ty;
use crate::values::StarlarkValue;
use crate::values::Trace;
use crate::values::Value;

/// There have been bugs around line endings in the textwrap crate. Just join
/// into a single string, and trim the line endings.
fn wrap_trimmed(s: &str, width: usize) -> String {
    textwrap::wrap(s, width).join("\n").trim_end().to_owned()
}

/// There have been bugs around line endings in the textwrap crate. Just trim the line endings.
fn indent_trimmed(s: &str, prefix: &str) -> String {
    textwrap::indent(s, prefix).trim_end().to_owned()
}

/// The documentation provided by a user for a specific module, object, function, etc.
#[derive(Debug, Clone, PartialEq, Serialize, Trace, Default, Allocative)]
pub struct DocString {
    /// The first line of a doc string. This has whitespace trimmed from it.
    pub summary: String,
    /// The contents of a doc string that follow the summary, and a single blank line.
    /// This also has whitespace trimmed from it, and it is dedented.
    pub details: Option<String>,
}

impl DocString {
    /// Render this docstring as a "starlark" docstring.
    fn render_as_code(&self) -> String {
        let s = match &self.details {
            Some(details) => {
                format!("{}\n\n{}", self.summary, details)
            }
            None => self.summary.clone(),
        };
        wrap_trimmed(&s, 80)
    }

    /// Render the docstring as in `render_as_code`, but surround it in triple quotes,
    /// a common convention in starlark docstrings.
    fn render_as_quoted_code(&self) -> String {
        format!("\"\"\"\n{}\n\"\"\"", self.render_as_code())
    }
}

/// The file a symbol resides in, and if available its location within that file.
#[derive(Debug, Clone, PartialEq, Serialize, Default)]
pub struct Location {
    /// `path` is a string that can be passed into `load()` statements.
    pub path: String,
}

/// The main identifier for a symbol.
#[derive(Debug, Clone, PartialEq, Serialize, Default)]
pub struct Identifier {
    /// The name of the symbol (e.g. the function name, a name or path for a module, etc).
    pub name: String,
    /// Where the symbol is located, or absent if it is a built-in symbol.
    pub location: Option<Location>,
}

/// Documents a full module.
#[derive(Debug, Clone, PartialEq, Serialize, Default, Allocative)]
pub struct DocModule {
    /// In general, this should be the first statement of a loaded file, if that statement is
    /// a string literal.
    pub docs: Option<DocString>,
    /// A mapping of top level symbols to their documentation, if any.
    pub members: SmallMap<String, DocMember>,
}

impl DocModule {
    pub(crate) fn render_as_code(&self) -> String {
        let mut res = self
            .docs
            .as_ref()
            .map(DocString::render_as_quoted_code)
            .unwrap_or_default();
        for (k, v) in &self.members {
            res.push('\n');
            res.push_str(&(Doc::named_item(k.clone(), v.clone().to_doc_item())).render_as_code());
            res.push('\n');
        }
        res
    }
}

/// Documents a single function.
#[derive(Debug, Clone, PartialEq, Default, Serialize, Allocative)]
pub struct DocFunction {
    /// Documentation for the function. If parsed, this should generally be the first statement
    /// of a function's body if that statement is a string literal. Any sections like "Args:",
    /// "Returns", etc are kept intact. It is up to the consumer to remove these sections if
    /// they are present.
    pub docs: Option<DocString>,
    /// The parameters that this function takes. Docs for these parameters should generally be
    /// extracted from the main docstring's details.
    pub params: Vec<DocParam>,
    /// Details about what this function returns.
    pub ret: DocReturn,
    /// Does this function act as type?
    pub as_type: Option<Ty>,
}

impl DocFunction {
    fn starlark_docstring(&self) -> Option<String> {
        let mut docs = String::new();
        if let Some(main_docs) = self.docs.as_ref().map(DocString::render_as_code) {
            docs.push_str(&main_docs);
        }

        let args_indentation_count = self
            .params
            .iter()
            .map(|p| match p {
                DocParam::NoArgs | DocParam::OnlyPosBefore => 0,
                DocParam::Arg { name, .. }
                | DocParam::Args { name, .. }
                | DocParam::Kwargs { name, .. } => name.len() + 2,
            })
            .max()
            .unwrap_or_default();
        let args_indentation = " ".repeat(args_indentation_count);

        let args_docs = self
            .params
            .iter()
            .filter_map(|p| p.starlark_docstring(&args_indentation))
            .join("\n");
        if !args_docs.is_empty() {
            let indented = indent_trimmed(&args_docs, "    ");
            docs.push_str(&format!("\n\nArgs:\n{}", indented));
        }

        if let Some(ret_docs) = self.ret.starlark_docstring() {
            let indented = indent_trimmed(&ret_docs, "    ");
            docs.push_str(&format!("\n\nRet:\n{}", indented));
        }
        if docs.is_empty() {
            None
        } else {
            Some(indent_trimmed(
                &format!("\"\"\"\n{}\n\"\"\"", docs.trim_start()),
                "    ",
            ))
        }
    }

    pub(crate) fn render_as_code(&self, name: &str) -> String {
        let params: Vec<_> = self.params.iter().map(DocParam::render_as_code).collect();
        let spacer_len = if params.is_empty() {
            0
        } else {
            (params.len() - 1) * 2
        };
        let params_len = params.iter().map(|a| a.len()).sum::<usize>() + spacer_len;
        let params = if params_len > 60 {
            format!("(\n{}\n)", indent_trimmed(&params.join(",\n"), "    "))
        } else {
            format!("({})", params.join(", "))
        };
        let docstring = self
            .starlark_docstring()
            .map(|mut ds| {
                ds.push('\n');
                ds
            })
            .unwrap_or_default();
        let ret = Some(&self.ret.typ)
            .filter(|t| t != &&Ty::any())
            .map(|t| format!(" -> {}", t))
            .unwrap_or_default();

        format!("def {}{}{}:\n{}    pass", name, params, ret, docstring)
    }

    /// Used by LSP.
    pub fn find_param_with_name(&self, param_name: &str) -> Option<&DocParam> {
        self.params.iter().find(|p| match p {
            DocParam::Arg { name, .. }
            | DocParam::Args { name, .. }
            | DocParam::Kwargs { name, .. }
                if name == param_name =>
            {
                true
            }
            _ => false,
        })
    }
}

/// A single parameter of a function.
#[derive(Debug, Clone, PartialEq, Serialize, Allocative)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum DocParam {
    /// A regular parameter that may or may not have a default value.
    Arg {
        name: String,
        docs: Option<DocString>,
        #[serde(rename = "type")]
        typ: Ty,
        /// If present, this parameter has a default value. This is the `repr()` of that value.
        default_value: Option<String>,
    },
    /// Represents the "*" argument.
    NoArgs,
    /// Represents the "/" argument from [PEP 570](https://peps.python.org/pep-0570/).
    OnlyPosBefore,
    /// Represents the "*args" style of argument.
    Args {
        name: String,
        docs: Option<DocString>,
        #[serde(rename = "type")]
        tuple_elem_ty: Ty,
    },
    /// Represents the "**kwargs" style of argument.
    Kwargs {
        name: String,
        docs: Option<DocString>,
        #[serde(rename = "type")]
        dict_value_ty: Ty,
    },
}

impl DocParam {
    fn starlark_docstring(&self, max_indentation: &str) -> Option<String> {
        let (name, docs) = match self {
            DocParam::Arg { name, docs, .. } => Some((name, docs)),
            DocParam::NoArgs | DocParam::OnlyPosBefore => None,
            DocParam::Args { name, docs, .. } => Some((name, docs)),
            DocParam::Kwargs { name, docs, .. } => Some((name, docs)),
        }?;
        let rendered_docs = docs.as_ref()?.render_as_code();
        let mut indented = indent_trimmed(&rendered_docs, max_indentation);
        indented.replace_range(..name.len() + 2, &format!("{}: ", name));
        Some(indented)
    }

    fn render_as_code(&self) -> String {
        match self {
            DocParam::Arg {
                name,
                typ,
                default_value,
                ..
            } => match (typ, default_value.as_ref()) {
                (t, Some(default)) if t.is_any() => format!("{} = {}", name, default),
                (t, None) if t.is_any() => name.clone(),
                (t, Some(default)) => format!("{}: {} = {}", name, t, default),
                (t, None) => format!("{}: {}", name, t),
            },
            DocParam::NoArgs => "*".to_owned(),
            DocParam::OnlyPosBefore => "/".to_owned(),
            DocParam::Args {
                name,
                tuple_elem_ty: typ,
                ..
            }
            | DocParam::Kwargs {
                name,
                dict_value_ty: typ,
                ..
            } => match typ {
                t if t.is_any() => name.clone(),
                typ => format!("{}: {}", name, typ),
            },
        }
    }
}

/// Details about the return value of a function.
#[derive(Debug, Clone, PartialEq, Serialize, Allocative)]
pub struct DocReturn {
    /// Extra semantic details around the returned value's meaning.
    pub docs: Option<DocString>,
    #[serde(rename = "type")]
    pub typ: Ty,
}

impl Default for DocReturn {
    fn default() -> Self {
        DocReturn {
            docs: None,
            typ: Ty::any(),
        }
    }
}

impl DocReturn {
    fn starlark_docstring(&self) -> Option<String> {
        self.docs.as_ref().map(DocString::render_as_code)
    }
}

/// A single property of an object. These are explicitly not functions (see [`DocMember`]).
#[derive(Debug, Clone, PartialEq, Serialize, Allocative)]
pub struct DocProperty {
    pub docs: Option<DocString>,
    #[serde(rename = "type")]
    pub typ: Ty,
}

impl DocProperty {
    pub(crate) fn render_as_code(&self, name: &str) -> String {
        match (
            &self.typ,
            self.docs.as_ref().map(DocString::render_as_quoted_code),
        ) {
            // TODO(nmj): The starlark syntax needs to be updated to support type
            //            annotations on values as python does. Afterward, use these
            //            format strings.
            // (Some(t), Some(ds)) => {
            //     format!("{}\n_{}: {} = None", ds, name, t.raw_type)
            // }
            // (Some(t), None) => format!(r#"_{}: {} = None"#, name, t.raw_type),
            (t, Some(ds)) if t.is_any() => format!("{}\n_{} = None", ds, name),
            (t, None) if t.is_any() => format!("_{} = None", name),
            (t, Some(ds)) => {
                format!("{}\n# type: {}\n_{} = None", ds, t, name)
            }
            (t, None) => format!("# type: {}\n_{} = None", t, name),
        }
    }
}

/// A named member of an object.
#[derive(Debug, Clone, PartialEq, Serialize, Allocative)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum DocMember {
    Property(DocProperty),
    Function(DocFunction),
}

impl DocMember {
    pub(crate) fn from_value(value: Value) -> Self {
        // If we have a value which is a complex type, the right type to put in the docs is not the type
        // it represents, but it's just a property we should point at
        match value.documentation() {
            Some(DocItem::Function(x)) => DocMember::Function(x),
            Some(DocItem::Property(x)) => DocMember::Property(x),
            _ => DocMember::Property(DocProperty {
                docs: None,
                typ: value.get_type_starlark_repr(),
            }),
        }
    }

    pub fn to_doc_item(self) -> DocItem {
        match self {
            DocMember::Property(x) => DocItem::Property(x),
            DocMember::Function(x) => DocItem::Function(x),
        }
    }
}

/// An object with named functions/properties.
#[derive(Debug, Clone, PartialEq, Serialize, Default, Allocative)]
pub struct DocObject {
    pub docs: Option<DocString>,
    /// Name and details of each member of this object.
    pub members: SmallMap<String, DocMember>,
}

impl DocObject {
    pub(crate) fn render_as_code(&self, name: &str) -> String {
        let summary = self
            .docs
            .as_ref()
            .map(|ds| {
                let mut s = ds.render_as_quoted_code();
                s.push('\n');
                s
            })
            .unwrap_or_default();

        let member_docs = self
            .members
            .iter()
            .map(|(name, member)| match member {
                DocMember::Property(p) => p.render_as_code(name),
                DocMember::Function(f) => f.render_as_code(&format!("_{}", name)),
            })
            .join("\n\n");

        let exported_struct_members = self
            .members
            .iter()
            .map(|(name, _)| format!("    {} = _{},", name, name))
            .join("\n");
        let exported_struct = if !exported_struct_members.is_empty() {
            format!(
                "{}{} = struct(\n{}\n)",
                summary, name, exported_struct_members
            )
        } else {
            String::new()
        };

        format!("{}\n\n{}", member_docs, exported_struct)
            .trim()
            .to_owned()
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Allocative)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum DocItem {
    Module(DocModule),
    Object(DocObject),
    Function(DocFunction),
    Property(DocProperty),
}

impl DocItem {
    /// Get the underlying [`DocString`] for this item, if it exists.
    pub fn get_doc_string(&self) -> Option<&DocString> {
        match self {
            DocItem::Module(m) => m.docs.as_ref(),
            DocItem::Object(o) => o.docs.as_ref(),
            DocItem::Function(f) => f.docs.as_ref(),
            DocItem::Property(p) => p.docs.as_ref(),
        }
    }

    /// Get the summary of the underlying [`DocString`] for this item, if it exists.
    pub fn get_doc_summary(&self) -> Option<&str> {
        self.get_doc_string().map(|ds| ds.summary.as_str())
    }
}

impl DocMember {
    /// Get the underlying [`DocString`] for this item, if it exists.
    pub fn get_doc_string(&self) -> Option<&DocString> {
        match self {
            DocMember::Function(f) => f.docs.as_ref(),
            DocMember::Property(p) => p.docs.as_ref(),
        }
    }

    /// Get the summary of the underlying [`DocString`] for this item, if it exists.
    pub fn get_doc_summary(&self) -> Option<&str> {
        self.get_doc_string().map(|ds| ds.summary.as_str())
    }
}

impl DocParam {
    /// Get the underlying [`DocString`] for this item, if it exists.
    pub fn get_doc_string(&self) -> Option<&DocString> {
        match self {
            DocParam::Arg { docs, .. }
            | DocParam::Args { docs, .. }
            | DocParam::Kwargs { docs, .. } => docs.as_ref(),
            _ => None,
        }
    }

    /// Get the summary of the underlying [`DocString`] for this item, if it exists.
    pub fn get_doc_summary(&self) -> Option<&str> {
        self.get_doc_string().map(|ds| ds.summary.as_str())
    }
}

/// The main structure that represents the documentation for a given symbol / module.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Doc {
    pub id: Identifier,
    pub item: DocItem,
    /// Custom key-value pairs that are not interpreted directly by starlark, and can be
    /// used as arbitrary data for documentation tooling.
    pub custom_attrs: HashMap<String, String>,
}

impl Doc {
    pub fn named_item(name: String, item: DocItem) -> Self {
        Doc {
            id: Identifier {
                name,
                location: None,
            },
            item,
            custom_attrs: HashMap::new(),
        }
    }

    /// Render a starlark code representation of this documentation object.
    ///
    /// Function bodies for these consist of a single "pass" statement, and objects
    /// are represented as structs.
    pub fn render_as_code(&self) -> String {
        match &self.item {
            DocItem::Module(m) => m.render_as_code(),
            DocItem::Object(o) => o.render_as_code(&self.id.name),
            DocItem::Function(f) => f.render_as_code(&self.id.name),
            DocItem::Property(p) => p.render_as_code(&self.id.name),
        }
    }
}

/// Render a series of [`Doc`] objects into a "starlark" file.
///
/// Function bodies for these consist of a single "pass" statement, and objects
/// are represented as structs.
///
/// The returned array may not be in the same order as the originally provided docs.
/// They are in the order that they should appear in the rendered starlark file.
pub fn render_docs_as_code(docs: &[Doc]) -> String {
    let (modules, non_modules): (Vec<_>, Vec<_>) = docs
        .iter()
        .partition(|d| matches!(d.item, DocItem::Module(_)));
    modules
        .into_iter()
        .chain(non_modules)
        .map(|d| d.render_as_code())
        .join("\n\n")
}

/// Get documentation for all items registered with `#[derive(StarlarkDocs)]`
///
/// Note: Because `StarlarkDocs` uses the inventory crate under the hood, in statically linked
/// binaries, documentation from all compiled crates in the binary will be included.
///
/// For dynamically linked binaries, documentation will only be able to retrieved after the crate's
/// library is `dlopen()`ed.
pub fn get_registered_starlark_docs() -> Vec<Doc> {
    inventory::iter::<RegisteredDoc>
        .into_iter()
        .filter_map(|d| (d.getter)())
        .collect()
}

#[doc(hidden)]
pub struct RegisteredDoc {
    pub getter: fn() -> Option<Doc>,
}

inventory::collect!(RegisteredDoc);

impl RegisteredDoc {
    /// This function is called from generated code.
    pub fn for_type<'v, T: StarlarkValue<'v>>(custom_attrs: &[(&str, &str)]) -> Option<Doc> {
        let name = T::TYPE.to_owned();
        let id = Identifier {
            name,
            location: None,
        };
        let item = DocItem::Object(T::get_methods()?.documentation());
        let custom_attrs = custom_attrs
            .iter()
            .map(|(k, v)| ((*k).to_owned(), (*v).to_owned()))
            .collect();
        Some(Doc {
            id,
            item,
            custom_attrs,
        })
    }
}
