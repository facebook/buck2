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

use std::fmt::Write;
use std::fs;
use std::path::Path;

use derivative::Derivative;
use dupe::Dupe;
use lalrpop_util as lu;

use crate::codemap::CodeMap;
use crate::codemap::FileSpan;
use crate::codemap::Pos;
use crate::codemap::Span;
use crate::errors::Diagnostic;
use crate::eval::compiler::EvalException;
use crate::syntax::ast::AstStmt;
use crate::syntax::ast::Stmt;
use crate::syntax::grammar::StarlarkParser;
use crate::syntax::lexer::Lexer;
use crate::syntax::lexer::Token;
use crate::syntax::state::ParserState;
use crate::syntax::AstLoad;
use crate::syntax::Dialect;

fn one_of(expected: &[String]) -> String {
    let mut result = String::new();
    for (i, e) in expected.iter().enumerate() {
        let sep = match i {
            0 => "one of",
            _ if i < expected.len() - 1 => ",",
            // Last expected message to be written
            _ => " or",
        };
        write!(result, "{} {}", sep, e).unwrap();
    }
    result
}

/// Convert the error to a codemap diagnostic.
///
/// To build this diagnostic, the method needs the file span corresponding
/// to the parsed file.
fn parse_error_add_span(
    err: lu::ParseError<usize, Token, EvalException>,
    pos: usize,
    codemap: &CodeMap,
) -> anyhow::Error {
    let (message, span) = match err {
        lu::ParseError::InvalidToken { location } => (
            "Parse error: invalid token".to_owned(),
            Span::new(Pos::new(location as u32), Pos::new(location as u32)),
        ),
        lu::ParseError::UnrecognizedToken {
            token: (x, t, y),
            expected,
        } => (
            format!(
                "Parse error: unexpected {} here, expected {}",
                t,
                one_of(&expected)
            ),
            Span::new(Pos::new(x as u32), Pos::new(y as u32)),
        ),
        lu::ParseError::UnrecognizedEOF { .. } => (
            "Parse error: unexpected end of file".to_owned(),
            Span::new(Pos::new(pos as u32), Pos::new(pos as u32)),
        ),
        lu::ParseError::ExtraToken { token: (x, t, y) } => (
            format!("Parse error: extraneous token {}", t),
            Span::new(Pos::new(x as u32), Pos::new(y as u32)),
        ),
        lu::ParseError::User { error } => return error.into_anyhow(),
    };

    Diagnostic::new(anyhow::anyhow!(message), span, codemap)
}

/// A representation of a Starlark module abstract syntax tree.
///
/// Created with either [`parse`](AstModule::parse) or [`parse_file`](AstModule::parse_file),
/// and evaluated with [`eval_module`](crate::eval::Evaluator::eval_module).
///
/// The internal details (statements/expressions) are deliberately omitted, as they change
/// more regularly. A few methods to obtain information about the AST are provided.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct AstModule {
    #[derivative(Debug = "ignore")]
    pub(crate) codemap: CodeMap,
    pub(crate) statement: AstStmt,
    pub(crate) dialect: Dialect,
}

impl AstModule {
    fn create(
        codemap: CodeMap,
        statement: AstStmt,
        dialect: &Dialect,
    ) -> anyhow::Result<AstModule> {
        Stmt::validate(&codemap, &statement, dialect).map_err(EvalException::into_anyhow)?;
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
        let mut errors = Vec::new();
        match StarlarkParser::new().parse(
            &mut ParserState {
                codemap: &codemap,
                dialect,
                errors: &mut errors,
            },
            lexer.filter(|t| match t {
                Ok((_, Token::Comment(_), _)) => false,
                _ => true,
            }),
        ) {
            Ok(v) => {
                if let Some(err) = errors.into_iter().next() {
                    return Err(err.into_anyhow());
                }
                Ok(AstModule::create(codemap, v, dialect)?)
            }
            Err(p) => Err(parse_error_add_span(p, codemap.source().len(), &codemap)),
        }
    }

    /// Return the file names of all the `load` statements in the module.
    /// If the [`Dialect`] had [`enable_load`](Dialect::enable_load) set to [`false`] this will be an empty list.
    pub fn loads(&self) -> Vec<AstLoad> {
        // We know that `load` statements must be at the top-level, so no need to descend inside `if`, `for`, `def` etc.
        // There is a suggestion that `load` statements should be at the top of a file, but we tolerate that not being true.
        fn f<'a>(ast: &'a AstStmt, codemap: &CodeMap, vec: &mut Vec<AstLoad<'a>>) {
            match &ast.node {
                Stmt::Load(load) => vec.push(AstLoad {
                    span: FileSpan {
                        file: codemap.dupe(),
                        span: load.module.span,
                    },
                    module_id: &load.module.node,
                    symbols: load
                        .args
                        .iter()
                        .map(|(name, sym)| (name.node.0.as_str(), sym.node.as_str()))
                        .collect(),
                }),
                Stmt::Statements(stmts) => {
                    for s in stmts {
                        f(s, codemap, vec);
                    }
                }
                _ => {}
            }
        }

        let mut loads = Vec::new();
        f(&self.statement, &self.codemap, &mut loads);
        loads
    }

    /// Look up a [`Span`] contained in this module to a [`FileSpan`].
    pub(crate) fn file_span(&self, x: Span) -> FileSpan {
        self.codemap.file_span(x)
    }

    /// Locations where statements occur.
    pub fn stmt_locations(&self) -> Vec<FileSpan> {
        fn go(x: &AstStmt, codemap: &CodeMap, res: &mut Vec<FileSpan>) {
            match &**x {
                Stmt::Statements(_) => {} // These are not interesting statements that come up
                _ => res.push(FileSpan {
                    span: x.span,
                    file: codemap.dupe(),
                }),
            }
            x.visit_stmt(|x| go(x, codemap, res))
        }

        let mut res = Vec::new();
        go(&self.statement, &self.codemap, &mut res);
        res
    }
}

#[cfg(test)]
mod tests {
    use crate::assert;
    use crate::slice_vec_ext::SliceExt;

    #[test]
    fn test_locations() {
        fn get(code: &str) -> String {
            assert::parse_ast(code)
                .stmt_locations()
                .map(|x| x.resolve_span().to_string())
                .join(" ")
        }

        assert_eq!(&get("foo"), "1:1-4");
        assert_eq!(&get("foo\ndef x():\n   pass"), "1:1-4 2:1-3:8 3:4-8");
    }
}
