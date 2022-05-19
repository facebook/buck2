/*
 * Copyright 2018 The Starlark in Rust Authors.
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

use std::{fs, path::Path};

use anyhow::anyhow;
use gazebo::prelude::*;
use lalrpop_util as lu;

use crate::{
    codemap::{CodeMap, FileSpan, Pos, Span},
    errors::Diagnostic,
    syntax::{
        ast::{AstModule, AstStmt, Stmt},
        dialect::Dialect,
        grammar::StarlarkParser,
        lexer::{Lexer, Token},
    },
};

fn one_of(expected: &[String]) -> String {
    let mut result = String::new();
    for (i, e) in expected.iter().enumerate() {
        let sep = match i {
            0 => "one of",
            _ if i < expected.len() - 1 => ",",
            // Last expected message to be written
            _ => " or",
        };
        result.push_str(&format!("{} {}", sep, e));
    }
    result
}

/// Convert the error to a codemap diagnostic.
///
/// To build this diagnostic, the method needs the file span corresponding
/// to the parsed file.
pub(crate) fn parse_error_add_span(
    err: lu::ParseError<usize, Token, anyhow::Error>,
    len: usize,
    codemap: &CodeMap,
) -> anyhow::Error {
    if let lu::ParseError::User { error } = err {
        return error;
    }

    let message = match &err {
        lu::ParseError::InvalidToken { .. } => "Parse error: invalid token".to_owned(),
        lu::ParseError::UnrecognizedToken {
            token: (_x, t, ..),
            expected,
        } => format!(
            "Parse error: unexpected {} here, expected {}",
            t,
            one_of(expected)
        ),
        lu::ParseError::ExtraToken { token: (_x, t, ..) } => {
            format!("Parse error: extraneous token {}", t)
        }
        lu::ParseError::UnrecognizedEOF { .. } => "Parse error: unexpected end of file".to_owned(),
        lu::ParseError::User { .. } => unreachable!(),
    };
    let span = match &err {
        lu::ParseError::InvalidToken { location } => {
            Span::new(Pos::new(*location as u32), Pos::new(*location as u32))
        }
        lu::ParseError::UnrecognizedToken {
            token: (x, .., y), ..
        } => Span::new(Pos::new(*x as u32), Pos::new(*y as u32)),
        lu::ParseError::UnrecognizedEOF { .. } => {
            Span::new(Pos::new(len as u32), Pos::new(len as u32))
        }
        lu::ParseError::ExtraToken { token: (x, .., y) } => {
            Span::new(Pos::new(*x as u32), Pos::new(*y as u32))
        }
        lu::ParseError::User { .. } => unreachable!(),
    };

    Diagnostic::new(anyhow!(message), span, codemap)
}

impl AstModule {
    fn create(
        codemap: CodeMap,
        statement: AstStmt,
        dialect: &Dialect,
    ) -> anyhow::Result<AstModule> {
        Stmt::validate(&codemap, &statement, dialect)?;
        Ok(AstModule {
            codemap,
            statement,
            dialect: dialect.clone(),
        })
    }

    /// Parse a file stored on disk. For details see [`parse`](AstModule::parse).
    pub fn parse_file(path: &Path, dialect: &Dialect) -> anyhow::Result<Self> {
        let content = fs::read_to_string(path)?;
        Self::parse(&path.to_string_lossy(), content, dialect)
    }

    /// Parse a Starlark module to produce an [`AstModule`], or an error if there are syntax errors.
    /// The `filename` is for error messages only, and does not have to be a valid file.
    /// The [`Dialect`] selects which Starlark constructs are valid.
    ///
    /// Errors will be reported using the [`Diagnostic`] type. For example:
    ///
    /// ```
    /// use starlark::syntax::{AstModule, Dialect};
    /// use starlark::errors::Diagnostic;
    ///
    /// let err: anyhow::Error = AstModule::parse("filename", "\n(unmatched".to_owned(), &Dialect::Standard).unwrap_err();
    /// let err: Diagnostic = err.downcast::<Diagnostic>().unwrap();
    /// assert_eq!(err.span.unwrap().to_string(), "filename:2:11");
    /// ```
    pub fn parse(filename: &str, content: String, dialect: &Dialect) -> anyhow::Result<Self> {
        let codemap = CodeMap::new(filename.to_owned(), content);
        let lexer = Lexer::new(codemap.source(), dialect, codemap.dupe());
        match StarlarkParser::new().parse(&codemap, dialect, lexer) {
            Ok(v) => Ok(AstModule::create(codemap, v, dialect)?),
            Err(p) => Err(parse_error_add_span(p, codemap.source().len(), &codemap)),
        }
    }

    /// Return the file names of all the `load` statements in the module.
    /// If the [`Dialect`] had [`enable_load`](Dialect::enable_load) set to [`false`] this will be an empty list.
    pub fn loads(&self) -> Vec<&str> {
        // We know that `load` statements must be at the top-level, so no need to descend inside `if`, `for`, `def` etc.
        // There is a suggestion that `load` statements should be at the top of a file, but we tolerate that not being true.
        fn f<'a>(ast: &'a AstStmt, vec: &mut Vec<&'a str>) {
            match &ast.node {
                Stmt::Load(load) => vec.push(&load.module.node),
                Stmt::Statements(stmts) => {
                    for s in stmts {
                        f(s, vec);
                    }
                }
                _ => {}
            }
        }

        let mut loads = Vec::new();
        f(&self.statement, &mut loads);
        loads
    }

    /// Look up a [`Span`] contained in this module to a [`FileSpan`].
    pub(crate) fn file_span(&self, x: Span) -> FileSpan {
        self.codemap.file_span(x)
    }
}
