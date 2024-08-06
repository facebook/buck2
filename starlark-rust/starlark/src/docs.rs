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

pub mod code;
pub mod markdown;
mod parse;

use std::collections::HashMap;

use allocative::Allocative;
pub use code::render_docs_as_code;
pub use markdown::MarkdownFlavor;
pub use markdown::RenderMarkdown;
pub use parse::DocStringKind;
pub use starlark_derive::StarlarkDocs;
use starlark_map::small_map::SmallMap;

use crate as starlark;
use crate::typing::Ty;
use crate::values::StarlarkValue;
use crate::values::Trace;
use crate::values::Value;

/// The documentation provided by a user for a specific module, object, function, etc.
#[derive(Debug, Clone, PartialEq, Trace, Default, Allocative)]
pub struct DocString {
    /// The first line of a doc string. This has whitespace trimmed from it.
    pub summary: String,
    /// The contents of a doc string that follow the summary, and a single blank line.
    /// This also has whitespace trimmed from it, and it is dedented.
    pub details: Option<String>,
}

/// The file a symbol resides in, and if available its location within that file.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Location {
    /// `path` is a string that can be passed into `load()` statements.
    pub path: String,
}

/// The main identifier for a symbol.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Identifier {
    /// The name of the symbol (e.g. the function name, a name or path for a module, etc).
    pub name: String,
    /// Where the symbol is located, or absent if it is a built-in symbol.
    pub location: Option<Location>,
}

/// Documents a full module.
#[derive(Debug, Clone, PartialEq, Default, Allocative)]
pub struct DocModule {
    /// In general, this should be the first statement of a loaded file, if that statement is
    /// a string literal.
    pub docs: Option<DocString>,
    /// A mapping of top level symbols to their documentation, if any.
    pub members: SmallMap<String, DocMember>,
}

/// Documents a single function.
#[derive(Debug, Clone, PartialEq, Default, Allocative)]
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
#[derive(Debug, Clone, PartialEq, Allocative)]
pub enum DocParam {
    /// A regular parameter that may or may not have a default value.
    Arg {
        name: String,
        docs: Option<DocString>,
        typ: Ty,
        /// If present, this parameter has a default value. This is the `repr()` of that value.
        default_value: Option<String>,
    },
    /// Represents the "*" argument from [PEP 3102](https://peps.python.org/pep-3102/).
    OnlyNamedAfter,
    /// Represents the "/" argument from [PEP 570](https://peps.python.org/pep-0570/).
    OnlyPosBefore,
    /// Represents the "*args" style of argument.
    Args {
        name: String,
        docs: Option<DocString>,
        tuple_elem_ty: Ty,
    },
    /// Represents the "**kwargs" style of argument.
    Kwargs {
        name: String,
        docs: Option<DocString>,
        dict_value_ty: Ty,
    },
}

/// Details about the return value of a function.
#[derive(Debug, Clone, PartialEq, Allocative)]
pub struct DocReturn {
    /// Extra semantic details around the returned value's meaning.
    pub docs: Option<DocString>,
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

/// A single property of an object. These are explicitly not functions (see [`DocMember`]).
#[derive(Debug, Clone, PartialEq, Allocative)]
pub struct DocProperty {
    pub docs: Option<DocString>,
    pub typ: Ty,
}

/// A named member of an object.
#[derive(Debug, Clone, PartialEq, Allocative)]
pub enum DocMember {
    Property(DocProperty),
    Function(DocFunction),
}

impl DocMember {
    pub(crate) fn from_value(value: Value) -> Self {
        // If we have a value which is a complex type, the right type to put in the docs is not the type
        // it represents, but it's just a property we should point at
        match value.documentation() {
            Some(DocItem::Member(x)) => x,
            _ => DocMember::Property(DocProperty {
                docs: None,
                typ: value.get_type_starlark_repr(),
            }),
        }
    }
}

/// An object with named functions/properties.
#[derive(Debug, Clone, PartialEq, Default, Allocative)]
pub struct DocObject {
    pub docs: Option<DocString>,
    /// Name and details of each member of this object.
    pub members: SmallMap<String, DocMember>,
}

#[derive(Debug, Clone, PartialEq, Allocative)]
pub enum DocItem {
    Module(DocModule),
    Object(DocObject),
    Member(DocMember),
}

impl DocItem {
    /// Get the underlying [`DocString`] for this item, if it exists.
    pub fn get_doc_string(&self) -> Option<&DocString> {
        match self {
            DocItem::Module(m) => m.docs.as_ref(),
            DocItem::Object(o) => o.docs.as_ref(),
            DocItem::Member(DocMember::Function(f)) => f.docs.as_ref(),
            DocItem::Member(DocMember::Property(p)) => p.docs.as_ref(),
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
#[derive(Debug, Clone, PartialEq)]
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
