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

use std::collections::HashMap;

use gazebo::prelude::*;
use itertools::Itertools;
use once_cell::sync::Lazy;
use regex::Regex;
use regex::RegexBuilder;
use serde::Deserialize;
use serde::Serialize;
pub use starlark_derive::StarlarkDocs;

use crate as starlark;
use crate::codemap::Spanned;
use crate::syntax::ast::AstLiteral;
use crate::syntax::ast::AstPayload;
use crate::syntax::ast::AstStmtP;
use crate::syntax::ast::ExprP;
use crate::syntax::ast::StmtP;
use crate::values::Trace;

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
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Trace, Default)]
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
    /// a common convetion in starlark docstrings.
    fn render_as_quoted_code(&self) -> String {
        format!("\"\"\"\n{}\n\"\"\"", self.render_as_code())
    }
}

/// Controls the formatting to use when parsing `DocString`s from raw docstrings
#[derive(Copy, Clone, Dupe)]
pub enum DocStringKind {
    /// Docstrings provided by users in starlark files, following python-y documentation style.
    ///
    /// For functions, they are the piece in `"""` that come right after the `def foo():` line,
    /// and they have sections for additional details. An example from a starlark file might be:
    ///
    /// ```starlark
    /// """ Module level docs here """
    ///
    /// def some_function(val: "string") -> "string":
    ///     """ This function takes a string and returns it.
    ///
    ///     This is where an explanation might go, but I have none
    ///
    ///     Args:
    ///         val: This is the value that gets returned
    ///
    ///     Returns:
    ///         The original value, because identity functions are fun.
    /// ```
    Starlark,
    /// Docstrings used with `#[starlark_module]` in rust.
    ///
    /// These are the documentation strings prefixed by `///` (like these docs) on
    /// `#[starlark_module]`, and the functions / attributes within it. It supports
    /// a section `# Arguments`, and `# Returns`, and removes some lines from code
    /// blocks that are valid for rustdoc, but not useful for people using these
    /// functions via starlark. An example might be something like:
    ///
    /// ```ignore
    /// /// These are where the module / object level docs go
    /// #[starlark_module]
    /// fn add_some_value(builder: &mut GlobalsBuilder) {
    ///     /// attr1 is an attribute that does nothing interesting.
    ///     #[attribute]
    ///     fn attr1(_this: Value<'v>) -> String {
    ///         Ok("attr1".to_owned())
    ///     }
    ///     /// Copies a string
    ///     ///
    ///     /// This is where details would be, if this were
    ///     /// a more interesting function.
    ///     ///
    ///     /// # Arguments
    ///     /// * `s`: This is string that is returned.
    ///     ///
    ///     /// # Returns
    ///     /// The a copy of the original string.
    ///     fn copy_string(s: &str) -> String {
    ///         Ok(s.to_owned())
    ///     }
    /// }
    /// ```
    Rust,
}

impl DocString {
    /// Extracts the docstring from a function or module body, iff the first
    /// statement is a string literal.
    pub(crate) fn extract_raw_starlark_docstring<P: AstPayload>(
        body: &AstStmtP<P>,
    ) -> Option<String> {
        if let StmtP::Statements(stmts) = &body.node {
            if let Some(Spanned {
                node:
                    StmtP::Expression(Spanned {
                        node: ExprP::Literal(AstLiteral::String(s)),
                        ..
                    }),
                ..
            }) = stmts.first()
            {
                return Some(s.node.to_owned());
            }
        };
        None
    }

    /// Do common work to parse a docstring (dedenting, splitting summary and details, etc)
    pub fn from_docstring(kind: DocStringKind, user_docstring: &str) -> Option<DocString> {
        let trimmed_docs = user_docstring.trim();
        if trimmed_docs.is_empty() {
            None
        } else {
            static SPLIT_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"\n[ ]*\n").unwrap());
            let split: Option<(&str, &str)> = SPLIT_RE.splitn(trimmed_docs, 2).collect_tuple();
            let (summary, details) = match split {
                Some((summary, details)) if !summary.is_empty() && !details.is_empty() => {
                    // Dedent the details separately so that people can have the summary on the
                    // same line as the opening quotes, and the details indented on subsequent
                    // lines.
                    let details = match kind {
                        DocStringKind::Starlark => textwrap::dedent(details).trim().to_owned(),
                        DocStringKind::Rust => {
                            Self::remove_rust_comments(textwrap::dedent(details).trim())
                        }
                    };
                    (summary, Some(details))
                }
                _ => (trimmed_docs, None),
            };

            // Remove any newlines (and surrounding whitespace) in the summary, and
            // replace them with a single space.
            static NEWLINES_RE: Lazy<Regex> =
                Lazy::new(|| Regex::new(r"(\S)\s*\n\s*(\S)").unwrap());
            let summary = NEWLINES_RE.replace_all(summary, r"$1 $2").to_string();

            Some(DocString { summary, details })
        }
    }

    /// Removes rustdoc-style commented out lines from code blocks.
    fn remove_rust_comments(details: &str) -> String {
        static CODEBLOCK_RE: Lazy<Regex> = Lazy::new(|| {
            RegexBuilder::new(r"```(\w*)\n.*?```")
                .dot_matches_new_line(true)
                .build()
                .expect("regex to compile")
        });
        static COMMENT_RE: Lazy<Regex> = Lazy::new(|| {
            RegexBuilder::new(r"^# .*$\n")
                .multi_line(true)
                .build()
                .expect("regex to compile")
        });
        CODEBLOCK_RE
            .replace_all(details, |caps: &regex::Captures| {
                match caps.get(1).expect("language group").as_str() {
                    "" | "rust" => COMMENT_RE
                        .replace_all(caps.get(0).expect("$0 to exist").as_str(), "")
                        .to_string(),
                    _ => caps.get(0).expect("$0 to exist").as_str().to_owned(),
                }
            })
            .to_string()
    }

    /// Join lines up, dedent them, and trim them
    fn join_and_dedent_lines(lines: &[String]) -> String {
        textwrap::dedent(&lines.join("\n")).trim().to_owned()
    }

    /// Parse out parameter docs from an "Args:" section of a docstring
    ///
    /// `args_section` should be dedented, and generally should just be the `args` key of
    /// the `DocString::parse_params()` function call. This is done as a separate function
    /// to reduce the number of times that sections are parsed out of docstring (e.g. if
    /// a user wants both the `Args:` and `Returns:` sections)
    pub fn parse_params(kind: DocStringKind, args_section: &str) -> HashMap<String, String> {
        static STARLARK_ARG_RE: Lazy<Regex> =
            Lazy::new(|| Regex::new(r"^(\*{0,2}\w+):\s*(.*)").unwrap());
        static RUST_ARG_RE: Lazy<Regex> =
            Lazy::new(|| Regex::new(r"^(?:\* )?`(\w+)`:?\s*(.*)").unwrap());

        static INDENTED_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^(?:\s|$)").unwrap());

        let arg_re = match kind {
            DocStringKind::Starlark => &STARLARK_ARG_RE,
            DocStringKind::Rust => &RUST_ARG_RE,
        };

        let mut ret = HashMap::new();
        let mut current_arg = None;
        let mut current_text = vec![];

        for line in args_section.lines() {
            if let Some(matches) = arg_re.captures(line) {
                if let Some(a) = current_arg.take() {
                    ret.insert(a, Self::join_and_dedent_lines(&current_text));
                }

                current_arg = Some(matches.get(1).unwrap().as_str().to_owned());

                let doc_match = matches.get(2).unwrap();
                current_text = vec![format!(
                    "{}{}",
                    " ".repeat(doc_match.start()),
                    doc_match.as_str()
                )];
            } else if current_arg.is_some() && INDENTED_RE.is_match(line) {
                current_text.push(line.to_owned());
            }
        }

        if let Some(a) = current_arg.take() {
            ret.insert(a, Self::join_and_dedent_lines(&current_text));
        }

        ret
    }

    /// Parse the sections out of a docstring's `details` text, and remove the requested sections from the text.
    ///
    /// "sections" are the various things in doc strings like "Arguments:", "Returns:", etc
    ///
    /// # Returns
    /// - A new instance of `DocString`, with the requested sections, if found, removed.
    /// - A mapping of section name, converted to lower case, to the cleaned up section text
    ///     i.e. dedented, section header not present, etc for any found sections.
    fn parse_and_remove_sections(
        self,
        kind: DocStringKind,
        requested_sections: &[&str],
    ) -> (Self, HashMap<String, String>) {
        let mut sections = HashMap::new();

        let mut finish_section =
            |current_section: &mut Option<String>, current_section_text: &mut Vec<String>| {
                if let Some(s) = current_section.take() {
                    sections.insert(s, DocString::join_and_dedent_lines(current_section_text));
                    current_section_text.clear();
                }
            };

        static STARLARK_SECTION_RE: Lazy<Regex> =
            Lazy::new(|| Regex::new(r"^([\w -]+):\s*$").unwrap());
        static STARLARK_INDENTED_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^(?:\s|$)").unwrap());
        static RUST_SECTION_RE: Lazy<Regex> =
            Lazy::new(|| Regex::new(r"^# ([\w -]+)\s*$").unwrap());
        static RUST_INDENTED_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^.*").unwrap());

        let (section_re, indented_re) = match kind {
            DocStringKind::Starlark => (&STARLARK_SECTION_RE, &STARLARK_INDENTED_RE),
            DocStringKind::Rust => (&RUST_SECTION_RE, &RUST_INDENTED_RE),
        };

        if let Some(details) = self.details {
            let mut new_details = vec![];
            let mut current_section = None;
            let mut current_section_text = vec![];

            for line in details.lines() {
                if let Some(matches) = section_re.captures(line) {
                    finish_section(&mut current_section, &mut current_section_text);

                    let found_section = matches.get(1).unwrap().as_str().to_ascii_lowercase();
                    if requested_sections.contains(&found_section.as_str()) {
                        current_section = Some(found_section);
                    } else {
                        new_details.push(line.to_owned());
                    }
                } else if current_section.is_some() && indented_re.is_match(line) {
                    current_section_text.push(line.to_owned());
                } else {
                    new_details.push(line.to_owned());
                    finish_section(&mut current_section, &mut current_section_text);
                }
            }

            finish_section(&mut current_section, &mut current_section_text);

            let joined_details = new_details.join("\n").trim().to_owned();
            let details = match joined_details.is_empty() {
                true => None,
                false => Some(joined_details),
            };
            (
                Self {
                    summary: self.summary,
                    details,
                },
                sections,
            )
        } else {
            (self, sections)
        }
    }
}

/// Line / column for where in a file a symbol is.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct Pos {
    /// Line number, zero based.
    pub line: usize,
    /// Column number, zero based.
    pub column: usize,
}

/// The file a symbol resides in, and if available its location within that file.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct Location {
    /// `path` is a string that can be passed into `load()` statements.
    pub path: String,
    /// Location of the symbol within the file.
    pub position: Option<Pos>,
}

/// The main identifier for a symbol.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct Identifier {
    /// The name of the symbol (e.g. the function name, a name or path for a module, etc).
    pub name: String,
    /// Where the symbol is located, or absent if it is a built-in symbol.
    pub location: Option<Location>,
}

/// The type of a given parameter, field, etc.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct Type {
    /// The type string that one would find in a starlark expression.
    pub raw_type: String,
}

/// Documents a full module.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct Module {
    /// In general, this should be the first statement of a loaded file, if that statement is
    /// a string literal.
    pub docs: Option<DocString>,
}

impl Module {
    fn render_as_code(&self) -> String {
        self.docs
            .as_ref()
            .map(DocString::render_as_quoted_code)
            .unwrap_or_default()
    }
}

/// Documents a single function.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct Function {
    /// Documentation for the function. If parsed, this should generally be the first statement
    /// of a function's body if that statement is a string literal. Any sections like "Args:",
    /// "Returns", etc are kept intact. It is up to the consumer to remove these sections if
    /// they are present.
    pub docs: Option<DocString>,
    /// The parameters that this function takes. Docs for these parameters should generally be
    /// extracted from the main docstring's details.
    pub params: Vec<Param>,
    /// Details about what this function returns.
    pub ret: Return,
}

impl Function {
    fn starlark_docstring(&self) -> Option<String> {
        let mut docs = String::new();
        if let Some(main_docs) = self.docs.as_ref().map(DocString::render_as_code) {
            docs.push_str(&main_docs);
        }

        let args_indentation_count = self
            .params
            .iter()
            .map(|p| match p {
                Param::NoArgs => 0,
                Param::Arg { name, .. } | Param::Args { name, .. } | Param::Kwargs { name, .. } => {
                    name.len() + 2
                }
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

    fn render_as_code(&self, name: &str) -> String {
        let params: Vec<_> = self.params.iter().map(Param::render_as_code).collect();
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
        let ret = self
            .ret
            .typ
            .as_ref()
            .map(|t| format!(" -> {}", t.raw_type))
            .unwrap_or_default();

        format!("def {}{}{}:\n{}    pass", name, params, ret, docstring)
    }

    /// Parses function documentation out of a docstring
    ///
    /// # Arguments
    /// * `kind`: The kind of docstring. This determines the formatting that is parsed.
    /// * `params_producer`: A function that takes a mapping of parameter names -> docstrings,
    ///                      and creates a vec of params. These are then returned in the
    ///                      main `Function` object.
    /// * `return_type`: The return type. This is pulled from typing info / directly from users,
    ///                  so it cannot be inferred generically.
    /// * `raw_docstring`: The raw docstring to be parsed and potentially modified,
    ///                    removing the sections detailing arguments and return values.
    ///                    The format is determined by `kind`.
    pub fn from_docstring<F: FnOnce(HashMap<String, Option<DocString>>) -> Vec<Param>>(
        kind: DocStringKind,
        params_producer: F,
        return_type: Option<Type>,
        raw_docstring: Option<&str>,
    ) -> Self {
        match raw_docstring.and_then(|raw| DocString::from_docstring(kind, raw)) {
            Some(ds) => {
                let (function_docstring, sections) =
                    ds.parse_and_remove_sections(kind, &["arguments", "args", "returns", "return"]);

                let arg_docs = match sections.get("arguments").or_else(|| sections.get("args")) {
                    Some(args) => Self::parse_params(kind, args)
                        .into_iter()
                        .map(|(name, raw)| (name, DocString::from_docstring(kind, &raw)))
                        .collect(),
                    None => HashMap::new(),
                };
                let params = params_producer(arg_docs);

                let return_docs = sections
                    .get("return")
                    .or_else(|| sections.get("returns"))
                    .and_then(|raw| DocString::from_docstring(kind, raw));

                Function {
                    docs: Some(function_docstring),
                    params,
                    ret: Return {
                        docs: return_docs,
                        typ: return_type,
                    },
                }
            }
            None => Function {
                docs: None,
                params: params_producer(HashMap::new()),
                ret: Return {
                    docs: None,
                    typ: return_type,
                },
            },
        }
    }

    /// Parse out parameter docs from an "Args:" section of a docstring
    ///
    /// `args_section` should be dedented, and generally should just be the `args` key of
    /// the `DocString::parse_and_remove_sections()` function call. This is done as a
    /// separate function to reduce the number of times that sections are parsed out of
    /// docstring (e.g. if a user wants both the `Args:` and `Returns:` sections)
    fn parse_params(kind: DocStringKind, args_section: &str) -> HashMap<String, String> {
        static STARLARK_ARG_RE: Lazy<Regex> =
            Lazy::new(|| Regex::new(r"^(\*{0,2}\w+):\s*(.*)").unwrap());
        static RUST_ARG_RE: Lazy<Regex> =
            Lazy::new(|| Regex::new(r"^(?:\* )?`(\w+)`:?\s*(.*)").unwrap());

        static INDENTED_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^(?:\s|$)").unwrap());

        let arg_re = match kind {
            DocStringKind::Starlark => &STARLARK_ARG_RE,
            DocStringKind::Rust => &RUST_ARG_RE,
        };

        let mut ret = HashMap::new();
        let mut current_arg = None;
        let mut current_text = vec![];

        for line in args_section.lines() {
            if let Some(matches) = arg_re.captures(line) {
                if let Some(a) = current_arg.take() {
                    ret.insert(a, DocString::join_and_dedent_lines(&current_text));
                }

                current_arg = Some(matches.get(1).unwrap().as_str().to_owned());

                let doc_match = matches.get(2).unwrap();
                current_text = vec![format!(
                    "{}{}",
                    " ".repeat(doc_match.start()),
                    doc_match.as_str()
                )];
            } else if current_arg.is_some() && INDENTED_RE.is_match(line) {
                current_text.push(line.to_owned());
            }
        }

        if let Some(a) = current_arg.take() {
            ret.insert(a, DocString::join_and_dedent_lines(&current_text));
        }

        ret
    }
}

/// A single parameter of a function.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum Param {
    /// A regular parameter that may or may not have a default value.
    Arg {
        name: String,
        docs: Option<DocString>,
        #[serde(rename = "type")]
        typ: Option<Type>,
        /// If present, this parameter has a default value. This is the `repr()` of that value.
        default_value: Option<String>,
    },
    /// Represents the "*" argument.
    NoArgs,
    /// Represents the "*args" style of argument.
    Args {
        name: String,
        docs: Option<DocString>,
        #[serde(rename = "type")]
        typ: Option<Type>,
    },
    /// Represents the "**kwargs" style of argument.
    Kwargs {
        name: String,
        docs: Option<DocString>,
        #[serde(rename = "type")]
        typ: Option<Type>,
    },
}

impl Param {
    fn starlark_docstring(&self, max_indentation: &str) -> Option<String> {
        let (name, docs) = match self {
            Param::Arg { name, docs, .. } => Some((name, docs)),
            Param::NoArgs => None,
            Param::Args { name, docs, .. } => Some((name, docs)),
            Param::Kwargs { name, docs, .. } => Some((name, docs)),
        }?;
        let rendered_docs = docs.as_ref()?.render_as_code();
        let mut indented = indent_trimmed(&rendered_docs, max_indentation);
        indented.replace_range(..name.len() + 2, &format!("{}: ", name));
        Some(indented)
    }

    fn render_as_code(&self) -> String {
        match self {
            Param::Arg {
                name,
                typ,
                default_value,
                ..
            } => match (typ.as_ref(), default_value.as_ref()) {
                (Some(t), Some(default)) => format!("{}: {} = {}", name, t.raw_type, default),
                (Some(t), None) => format!("{}: {}", name, t.raw_type),
                (None, Some(default)) => format!("{} = {}", name, default),
                (None, None) => name.clone(),
            },
            Param::NoArgs => "*".to_owned(),
            Param::Args { name, typ, .. } | Param::Kwargs { name, typ, .. } => match typ.as_ref() {
                Some(typ) => format!("{}: {}", name, typ.raw_type),
                None => name.clone(),
            },
        }
    }
}

/// Details about the return value of a function.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct Return {
    /// Extra semantic details around the returned value's meaning.
    pub docs: Option<DocString>,
    #[serde(rename = "type")]
    pub typ: Option<Type>,
}

impl Return {
    fn starlark_docstring(&self) -> Option<String> {
        self.docs.as_ref().map(DocString::render_as_code)
    }
}

/// A single property of an object. These are explicitly not functions (see [`Member`]).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct Property {
    pub docs: Option<DocString>,
    #[serde(rename = "type")]
    pub typ: Option<Type>,
}

impl Property {
    fn render_as_code(&self, name: &str) -> String {
        match (
            self.typ.as_ref(),
            self.docs.as_ref().map(DocString::render_as_quoted_code),
        ) {
            // TODO(nmj): The starlark syntax needs to be updated to support type
            //            annotations on values as python does. Afterward, use these
            //            format strings.
            // (Some(t), Some(ds)) => {
            //     format!("{}\n_{}: {} = None", ds, name, t.raw_type)
            // }
            // (Some(t), None) => format!(r#"_{}: {} = None"#, name, t.raw_type),
            (Some(t), Some(ds)) => {
                format!("{}\n# type: {}\n_{} = None", ds, t.raw_type, name)
            }
            (Some(t), None) => format!("# type: {}\n_{} = None", t.raw_type, name),

            (None, Some(ds)) => format!("{}\n_{} = None", ds, name),
            (None, None) => format!("_{} = None", name),
        }
    }
}

/// A named member of an object.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum Member {
    Property(Property),
    Function(Function),
}

/// An object with named functions/properties.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct Object {
    pub docs: Option<DocString>,
    /// Name and details of each member of this object.
    pub members: Vec<(String, Member)>,
}

impl Object {
    fn render_as_code(&self, name: &str) -> String {
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
                Member::Property(p) => p.render_as_code(name),
                Member::Function(f) => f.render_as_code(&format!("_{}", name)),
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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum DocItem {
    Module(Module),
    Object(Object),
    Function(Function),
}

/// The main structure that represents the documentation for a given symbol / module.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Doc {
    pub id: Identifier,
    pub item: DocItem,
    /// Custom key-value pairs that are not interpreted directly by starlark, and can be
    /// used as arbitrary data for documentation tooling.
    pub custom_attrs: HashMap<String, String>,
}

impl Doc {
    /// Render a starlark code representation of this documentation object.
    ///
    /// Function bodies for these consist of a single "pass" statement, and objects
    /// are represented as structs.
    pub fn render_as_code(&self) -> String {
        match &self.item {
            DocItem::Module(m) => m.render_as_code(),
            DocItem::Object(o) => o.render_as_code(&self.id.name),
            DocItem::Function(f) => f.render_as_code(&self.id.name),
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
pub fn render_docs_as_code(docs: &[Doc]) -> Vec<String> {
    let (modules, non_modules): (Vec<_>, Vec<_>) = docs.iter().partition(|d| match d.item {
        DocItem::Module(_) => true,
        _ => false,
    });
    modules
        .into_iter()
        .chain(non_modules.into_iter())
        .map(|d| d.render_as_code())
        .collect()
}

/// Get documentation for all items registered with `#[derive(StarlarkDocs)]`
///
/// Note: Because `StarlarkDocs` uses the inventory crate under the hood, in statically linked
/// binaries, documentation from all compiled crates in the binary will be included.
///
/// For dynamically linked binaries, documentation will only be able to retrieved after the crate's
/// library is `dlopen()`ed.
pub fn get_registered_docs() -> Vec<Doc> {
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

#[cfg(test)]
mod tests {
    use std::fmt::Display;
    use std::fmt::Formatter;

    use gazebo::any::ProvidesStaticType;
    use starlark_derive::starlark_module;

    use super::*;
    use crate as starlark;
    use crate::environment::GlobalsBuilder;
    use crate::environment::GlobalsStatic;
    use crate::environment::Methods;
    use crate::environment::MethodsBuilder;
    use crate::environment::MethodsStatic;
    use crate::values::NoSerialize;
    use crate::values::StarlarkValue;
    use crate::values::Value;

    /// These are where the module docs go
    ///
    /// This is what is passed to users for an object, so be careful
    /// not to register two modules for a single object.
    #[starlark_module]
    fn add_some_value(builder: &mut MethodsBuilder) {
        /// Docs for attr1
        #[starlark(attribute)]
        fn attr1<'v>(this: Value<'v>) -> anyhow::Result<String> {
            Ok("attr1".to_owned())
        }

        #[starlark(attribute)]
        fn attr2<'v>(this: Value<'v>) -> anyhow::Result<String> {
            Ok("attr2".to_owned())
        }

        /// Docs for func1
        ///
        /// # Arguments
        ///     * `foo`: Docs for foo
        ///
        /// # Returns
        /// The string 'func1'
        fn func1<'v>(this: Value<'v>, foo: String) -> anyhow::Result<String> {
            let _ignore = (this, foo);
            Ok("func1".to_owned())
        }

        fn func2<'v>(this: Value<'v>) -> anyhow::Result<String> {
            let _ = this;
            Ok("func2".to_owned())
        }
    }

    /// These are where the module docs go
    ///
    /// This is what is passed to users for an object, so be careful
    /// not to register two modules for a single object.
    #[starlark_module]
    fn add_some_global_value(builder: &mut GlobalsBuilder) {
        /// Docs for func1
        ///
        /// # Arguments
        ///     * `foo`: Docs for foo
        ///
        /// # Returns
        /// The string 'func1'
        fn func1(foo: String) -> anyhow::Result<String> {
            let _ignore = foo;
            Ok("func1".to_owned())
        }

        fn func2() -> anyhow::Result<String> {
            Ok("func2".to_owned())
        }

        /// A function with only positional arguments.
        fn func3(
            #[starlark(require = pos)] a1: i32,
            #[starlark(require = pos)] a2: Option<i32>,
            #[starlark(require = pos, default = 1)] step: i32,
        ) -> anyhow::Result<String> {
            let _x = (a1, a2, step);
            Ok("func3".to_owned())
        }
    }

    #[derive(Debug, ProvidesStaticType, NoSerialize)]
    struct SomeValue {}

    impl Display for SomeValue {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            f.write_str("SomeValue()")
        }
    }

    starlark_simple_value!(SomeValue);

    impl<'v> StarlarkValue<'v> for SomeValue {
        starlark_type!("some_value");

        fn get_methods() -> Option<&'static Methods> {
            static RES: MethodsStatic = MethodsStatic::new();
            RES.methods(add_some_value)
        }
    }

    #[test]
    fn globals_docs_work() {
        let mut globals_builder = GlobalsBuilder::new();
        static RES: GlobalsStatic = GlobalsStatic::new();
        RES.populate(add_some_global_value, &mut globals_builder);
        let globals = globals_builder.build();
        let docs = globals.documentation();

        let string_typ = Some(Type {
            raw_type: "str.type".to_owned(),
        });
        let expected_object = super::Object {
            docs: DocString::from_docstring(
                DocStringKind::Rust,
                "These are where the module docs go\n\nThis is what is passed to users for an object, so be careful\nnot to register two modules for a single object.",
            ),
            members: vec![
                (
                    "func1".to_owned(),
                    super::Member::Function(super::Function {
                        docs: DocString::from_docstring(DocStringKind::Rust, "Docs for func1"),
                        params: vec![Param::Arg {
                            name: "foo".to_owned(),
                            docs: DocString::from_docstring(DocStringKind::Rust, "Docs for foo"),
                            typ: string_typ.clone(),
                            default_value: None,
                        }],
                        ret: Return {
                            docs: DocString::from_docstring(
                                DocStringKind::Rust,
                                "The string 'func1'",
                            ),
                            typ: string_typ.clone(),
                        },
                    }),
                ),
                (
                    "func2".to_owned(),
                    super::Member::Function(super::Function {
                        docs: None,
                        params: vec![],
                        ret: Return {
                            docs: None,
                            typ: string_typ.clone(),
                        },
                    }),
                ),
                (
                    "func3".to_owned(),
                    super::Member::Function(super::Function {
                        docs: DocString::from_docstring(
                            DocStringKind::Rust,
                            "A function with only positional arguments.",
                        ),
                        params: vec![
                            Param::Arg {
                                name: "a1".to_owned(),
                                docs: None,
                                typ: Some(Type {
                                    raw_type: "int.type".to_owned(),
                                }),
                                default_value: None,
                            },
                            Param::Arg {
                                name: "a2".to_owned(),
                                docs: None,
                                typ: Some(Type {
                                    raw_type: "[None, int.type]".to_owned(),
                                }),
                                default_value: Some("None".to_owned()),
                            },
                            Param::Arg {
                                name: "step".to_owned(),
                                docs: None,
                                typ: Some(Type {
                                    raw_type: "int.type".to_owned(),
                                }),
                                // TODO: This should actually show '1'...
                                default_value: Some("None".to_owned()),
                            },
                        ],
                        ret: Return {
                            docs: None,
                            typ: string_typ,
                        },
                    }),
                ),
            ],
        };
        let expected = DocItem::Object(expected_object);

        assert_eq!(expected, docs);
    }

    #[test]
    fn methods_docs_work() {
        let docs = SomeValue {}.documentation();
        let string_typ = Some(Type {
            raw_type: "str.type".to_owned(),
        });
        let expected_object = super::Object {
            docs: DocString::from_docstring(
                DocStringKind::Rust,
                "These are where the module docs go\n\nThis is what is passed to users for an object, so be careful\nnot to register two modules for a single object.",
            ),
            members: vec![
                (
                    "attr1".to_owned(),
                    super::Member::Property(super::Property {
                        docs: DocString::from_docstring(DocStringKind::Rust, "Docs for attr1"),
                        typ: string_typ.clone(),
                    }),
                ),
                (
                    "attr2".to_owned(),
                    super::Member::Property(super::Property {
                        docs: None,
                        typ: string_typ.clone(),
                    }),
                ),
                (
                    "func1".to_owned(),
                    super::Member::Function(super::Function {
                        docs: DocString::from_docstring(DocStringKind::Rust, "Docs for func1"),
                        params: vec![Param::Arg {
                            name: "foo".to_owned(),
                            docs: DocString::from_docstring(DocStringKind::Rust, "Docs for foo"),
                            typ: string_typ.clone(),
                            default_value: None,
                        }],
                        ret: Return {
                            docs: DocString::from_docstring(
                                DocStringKind::Rust,
                                "The string 'func1'",
                            ),
                            typ: string_typ.clone(),
                        },
                    }),
                ),
                (
                    "func2".to_owned(),
                    super::Member::Function(super::Function {
                        docs: None,
                        params: vec![],
                        ret: Return {
                            docs: None,
                            typ: string_typ,
                        },
                    }),
                ),
            ],
        };
        let expected = Some(DocItem::Object(expected_object));

        assert_eq!(expected, docs);
    }

    #[test]
    fn parses_starlark_docstring() {
        assert_eq!(
            DocString::from_docstring(DocStringKind::Starlark, " "),
            None
        );
        assert_eq!(
            DocString::from_docstring(
                DocStringKind::Starlark,
                " \n\nThis should be the summary\n\n"
            ),
            Some(DocString {
                summary: "This should be the summary".to_owned(),
                details: None,
            })
        );
        assert_eq!(
            DocString::from_docstring(
                DocStringKind::Starlark,
                " \n\nThis should be the summary\n\n "
            ),
            Some(DocString {
                summary: "This should be the summary".to_owned(),
                details: None,
            })
        );
        assert_eq!(
            DocString::from_docstring(
                DocStringKind::Starlark,
                "Summary line here\n    \nDetails after some spaces\n\nand some more newlines"
            ),
            Some(DocString {
                summary: "Summary line here".to_owned(),
                details: Some("Details after some spaces\n\nand some more newlines".to_owned()),
            })
        );
        assert_eq!(
            DocString::from_docstring(
                DocStringKind::Starlark,
                r#"
        This is the summary.
          It has multiple lines and some spaces, and should be collapsed

        This should be a multiline set of details.
        It should be:
            - Dedented
            - Trimmed
            - Split properly from the summary

"#
            ),
            Some(DocString {
                summary: "This is the summary. It has multiple lines and some spaces, and should be collapsed".to_owned(),
                details: Some(
                    concat!(
                        "This should be a multiline set of details.\n",
                        "It should be:\n",
                        "    - Dedented\n",
                        "    - Trimmed\n",
                        "    - Split properly from the summary"
                    )
                    .to_owned()
                ),
            })
        );
        assert_eq!(
            DocString::from_docstring(
                DocStringKind::Starlark,
                r#"This is a summary line that is not dedented like the 'details'

        Typing the first line right after the """ in python docstrings is common,
        while putting the rest of the docstring indented. Just support both so it
        doesn't surprise anyone.
        "#
            ),
            Some(DocString {
                summary: "This is a summary line that is not dedented like the 'details'"
                    .to_owned(),
                details: Some(
                    concat!(
                "Typing the first line right after the \"\"\" in python docstrings is common,\n",
                "while putting the rest of the docstring indented. Just support both so it\n",
                "doesn't surprise anyone."
                )
                    .to_owned()
                ),
            })
        );
    }

    #[test]
    fn parses_rust_docstring() {
        let raw = r#"
        This is the summary line
          that sometimes is split on two lines

        This is the second part. It has some code blocks

        ```
        # foo() {
        "bar"
        # }
        ```

        ```python
        # This is a python comment. Leave it be
        print(1)
        ```

        ```rust
        # other_foo() {
        "other_bar"
        # }
        ```
        "#;

        let parsed = DocString::from_docstring(DocStringKind::Rust, raw).unwrap();
        assert_eq!(
            "This is the summary line that sometimes is split on two lines",
            parsed.summary
        );
        assert_eq!(
            concat!(
                "This is the second part. It has some code blocks\n\n",
                "```\n",
                "\"bar\"\n",
                "```\n\n",
                "```python\n",
                "# This is a python comment. Leave it be\n",
                "print(1)\n",
                "```\n\n",
                "```rust\n",
                "\"other_bar\"",
                "\n```"
            ),
            parsed.details.unwrap()
        );
    }

    #[test]
    fn parses_and_removes_sections_from_starlark_docstring() {
        let raw_docs = r#"This is an example docstring

        We have some details up here that should not be parsed

        Some empty section:
        Example:
            First line of the section

            A newline with no space after it before the second one,
                and a third that's indented further.
        This is not in the example section

        Last:
            This is something in the last section
        "#;
        let expected_docstring = DocString::from_docstring(
            DocStringKind::Starlark,
            r#"This is an example docstring

            We have some details up here that should not be parsed

            Some empty section:
            This is not in the example section

            Last:
                This is something in the last section
            "#,
        )
        .unwrap();

        let expected_sections = HashMap::from([(
            "example".to_owned(),
            concat!(
                "First line of the section\n\n",
                "A newline with no space after it before the second one,\n",
                "    and a third that's indented further."
            )
            .to_owned(),
        )]);

        let ds = DocString::from_docstring(DocStringKind::Starlark, raw_docs).unwrap();
        let (new_ds, sections) =
            ds.parse_and_remove_sections(DocStringKind::Starlark, &["example"]);

        assert_eq!(new_ds, expected_docstring);
        assert_eq!(sections, expected_sections);
    }

    #[test]
    fn parses_and_removes_sections_from_rust_docstring() {
        let raw_docs = r#"This is an example docstring

        We have some details up here that should not be parsed

        # Some Section

        ```
        # This is a commented out line in a codeblock
        fn some_func() {}
        ```

        # Example
        First line of the section

        Note that, unlike starlark doc strings,
        we don't require indentation. The end of a
        section is either a new section appearing,
        or the end of the string.

        # Last
        This is something in the last section
        "#;
        let expected_docstring = DocString::from_docstring(
            DocStringKind::Rust,
            r#"This is an example docstring

        We have some details up here that should not be parsed

        # Some Section

        ```
        fn some_func() {}
        ```

        # Last
        This is something in the last section
        "#,
        )
        .unwrap();

        let expected_sections = HashMap::from([(
            "example".to_owned(),
            concat!(
                "First line of the section\n\n",
                "Note that, unlike starlark doc strings,\n",
                "we don't require indentation. The end of a\n",
                "section is either a new section appearing,\n",
                "or the end of the string.",
            )
            .to_owned(),
        )]);

        let ds = DocString::from_docstring(DocStringKind::Rust, raw_docs).unwrap();
        let (new_ds, sections) = ds.parse_and_remove_sections(DocStringKind::Rust, &["example"]);

        assert_eq!(new_ds, expected_docstring);
        assert_eq!(sections, expected_sections);
    }

    #[test]
    fn parses_starlark_function_docstring() {
        let docstring = r#"This is an example docstring

        Details here

        Args:
            arg_foo: The argument named foo
            arg_bar: The argument named bar. It has
                     a longer doc string that spans
                     over three lines
            *args: Docs for args
            **kwargs: Docs for kwargs

        Returns:
            A value
        "#;

        let kind = DocStringKind::Starlark;
        let return_type = Some(Type {
            raw_type: "int".to_owned(),
        });
        let expected = Function {
            docs: DocString::from_docstring(kind, "This is an example docstring\n\nDetails here"),
            params: vec![
                Param::Arg {
                    name: "**kwargs".to_owned(),
                    docs: DocString::from_docstring(kind, "Docs for kwargs"),
                    typ: None,
                    default_value: None,
                },
                Param::Arg {
                    name: "*args".to_owned(),
                    docs: DocString::from_docstring(kind, "Docs for args"),
                    typ: None,
                    default_value: None,
                },
                Param::Arg {
                    name: "arg_bar".to_owned(),
                    docs: DocString::from_docstring(
                        kind,
                        concat!(
                            "The argument named bar. It has\n",
                            "a longer doc string that spans\n",
                            "over three lines"
                        ),
                    ),
                    typ: None,
                    default_value: None,
                },
                Param::Arg {
                    name: "arg_foo".to_owned(),
                    docs: DocString::from_docstring(kind, "The argument named foo"),
                    typ: None,
                    default_value: None,
                },
            ],
            ret: Return {
                docs: DocString::from_docstring(kind, "A value"),
                typ: return_type.clone(),
            },
        };

        let function_docs = Function::from_docstring(
            kind,
            |param_docs| {
                param_docs
                    .into_iter()
                    .sorted_by(|l, r| Ord::cmp(&l.0, &r.0))
                    .map(|(name, docs)| Param::Arg {
                        name,
                        docs,
                        typ: None,
                        default_value: None,
                    })
                    .collect()
            },
            return_type,
            Some(docstring),
        );

        assert_eq!(expected, function_docs);
    }

    #[test]
    fn parses_rust_function_docstring() {
        let docstring = r#"This is an example docstring

        Details here

        # Arguments
        * `arg_foo`: The argument named foo
        `arg_bar`: The argument named bar. It has
                   a longer doc string that spans
                   over three lines

        # Returns
        A value
        "#;

        let kind = DocStringKind::Rust;
        let return_type = Some(Type {
            raw_type: "int".to_owned(),
        });
        let expected = Function {
            docs: DocString::from_docstring(kind, "This is an example docstring\n\nDetails here"),
            params: vec![
                Param::Arg {
                    name: "arg_bar".to_owned(),
                    docs: DocString::from_docstring(
                        kind,
                        concat!(
                            "The argument named bar. It has\n",
                            "a longer doc string that spans\n",
                            "over three lines"
                        ),
                    ),
                    typ: None,
                    default_value: None,
                },
                Param::Arg {
                    name: "arg_foo".to_owned(),
                    docs: DocString::from_docstring(kind, "The argument named foo"),
                    typ: None,
                    default_value: None,
                },
            ],
            ret: Return {
                docs: DocString::from_docstring(kind, "A value"),
                typ: return_type.clone(),
            },
        };

        let function_docs = Function::from_docstring(
            kind,
            |param_docs| {
                param_docs
                    .into_iter()
                    .sorted_by(|l, r| Ord::cmp(&l.0, &r.0))
                    .map(|(name, docs)| Param::Arg {
                        name,
                        docs,
                        typ: None,
                        default_value: None,
                    })
                    .collect()
            },
            return_type,
            Some(docstring),
        );

        assert_eq!(expected, function_docs);
    }

    #[test]
    fn renders_starlark() {
        let ds = DocString::from_docstring(DocStringKind::Rust, "Summary\n\nSome details");
        let typ = Some(Type {
            raw_type: "\"int\"".to_owned(),
        });

        let docs = vec![
            Doc {
                id: Identifier {
                    name: "MyObject".to_owned(),
                    location: None,
                },
                item: DocItem::Object(Object {
                    docs: ds.clone(),
                    members: vec![
                        (
                            "prop1".to_owned(),
                            Member::Property(Property {
                                docs: ds.clone(),
                                typ: typ.clone(),
                            }),
                        ),
                        (
                            "prop2".to_owned(),
                            Member::Property(Property {
                                docs: None,
                                typ: None,
                            }),
                        ),
                        (
                            "func1".to_owned(),
                            Member::Function(Function {
                                docs: ds.clone(),
                                params: vec![],
                                ret: Return {
                                    docs: ds.clone(),
                                    typ: typ.clone(),
                                },
                            }),
                        ),
                    ],
                }),
                custom_attrs: Default::default(),
            },
            Doc {
                id: Identifier {
                    name: "function1".to_owned(),
                    location: None,
                },
                item: DocItem::Function(Function {
                    docs: ds.clone(),
                    params: vec![
                        Param::Arg {
                            name: "arg1".to_owned(),
                            docs: ds.clone(),
                            typ: typ.clone(),
                            default_value: Some("1".to_owned()),
                        },
                        Param::Arg {
                            name: "arg2".to_owned(),
                            docs: ds.clone(),
                            typ: typ.clone(),
                            default_value: None,
                        },
                        Param::NoArgs,
                        Param::Args {
                            name: "*args".to_owned(),
                            docs: ds.clone(),
                            typ: typ.clone(),
                        },
                        Param::Args {
                            name: "*kwargs".to_owned(),
                            docs: ds.clone(),
                            typ: typ.clone(),
                        },
                    ],
                    ret: Return {
                        docs: ds.clone(),
                        typ,
                    },
                }),
                custom_attrs: Default::default(),
            },
            Doc {
                id: Identifier {
                    name: "some_module".to_owned(),
                    location: None,
                },
                item: DocItem::Module(Module { docs: ds }),
                custom_attrs: Default::default(),
            },
        ];

        let expected = format!(
            r#"
        """
        {summary}

        {details}
        """

        """
        {summary}

        {details}
        """
        # type: "int"
        _prop1 = None

        _prop2 = None

        def _func1() -> "int":
            """
            {summary}

            {details}

            Ret:
                {summary}

                {details}
            """
            pass

        """
        {summary}

        {details}
        """
        MyObject = struct(
            prop1 = _prop1,
            prop2 = _prop2,
            func1 = _func1,
        )

        def function1(
            arg1: "int" = 1,
            arg2: "int",
            *,
            *args: "int",
            *kwargs: "int"
        ) -> "int":
            """
            {summary}

            {details}

            Args:
                arg1:    {summary}

                         {details}
                arg2:    {summary}

                         {details}
                *args:   {summary}

                         {details}
                *kwargs: {summary}

                         {details}

            Ret:
                {summary}

                {details}
            """
            pass
        "#,
            summary = "Summary",
            details = "Some details"
        );
        let expected = textwrap::dedent(&expected).trim().to_owned();

        let rendered = render_docs_as_code(&docs).join("\n\n");

        assert_eq!(expected, rendered);
    }
}
