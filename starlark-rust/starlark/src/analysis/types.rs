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

use std::fmt;
use std::fmt::Display;

use gazebo::prelude::*;
use gazebo::variants::VariantName;
use lsp_types::Diagnostic;
use lsp_types::DiagnosticSeverity;
use lsp_types::NumberOrString;
use lsp_types::Range;
use serde::Serialize;

use crate::codemap::CodeMap;
use crate::codemap::FileSpan;
use crate::codemap::ResolvedSpan;
use crate::codemap::Span;
use crate::errors::Diagnostic as StarlarkDiagnostic;

pub(crate) trait LintWarning: Display + VariantName {
    fn is_serious(&self) -> bool;
}

/// A private version of lint without the inner trait erased, useful so we can test
/// using full matching, but then erase the internal details when exporting to users.
pub(crate) struct LintT<T> {
    pub location: FileSpan,
    pub original: String,
    pub problem: T,
}

/// A lint produced by [`AstModule::lint`](crate::syntax::AstModule::lint).
#[derive(Debug)]
pub struct Lint {
    /// Which code location does this lint refer to.
    pub location: FileSpan,
    /// kebab-case constant describing this issue, e.g. `missing-return`.
    pub short_name: String,
    /// Is this code highly-likely to be wrong, rather
    /// than merely stylistically non-ideal.
    pub serious: bool,
    /// A description of the underlying problem.
    pub problem: String,
    /// The source code at [`location`](Lint::location).
    pub original: String,
}

impl Display for Lint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.location, self.problem)
    }
}

impl<T: Display> Display for LintT<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.location, self.problem)
    }
}

impl<T: LintWarning> LintT<T> {
    pub(crate) fn new(codemap: &CodeMap, span: Span, problem: T) -> Self {
        let location = codemap.file_span(span);
        Self {
            original: location.file.source_span(span).to_owned(),
            location,
            problem,
        }
    }

    pub(crate) fn erase(self) -> Lint {
        Lint {
            location: self.location,
            short_name: kebab(self.problem.variant_name()),
            serious: self.problem.is_serious(),
            problem: self.problem.to_string(),
            original: self.original,
        }
    }
}

/// A standardised set of severities.
#[derive(Debug, Serialize, Dupe, Clone, Copy)]
#[serde(rename_all = "lowercase")]
pub enum EvalSeverity {
    /// An error while the program was being parsed.
    Error,
    /// The program parsed, but might not execute without error.
    Warning,
    /// This could be changed, but is not a blocking problem.
    Advice,
    /// This should be ignored
    Disabled,
}

impl Display for EvalSeverity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            EvalSeverity::Error => "Error",
            EvalSeverity::Warning => "Warning",
            EvalSeverity::Advice => "Advice",
            EvalSeverity::Disabled => "Disabled",
        })
    }
}

impl From<EvalSeverity> for DiagnosticSeverity {
    fn from(s: EvalSeverity) -> Self {
        match s {
            EvalSeverity::Error => DiagnosticSeverity::ERROR,
            EvalSeverity::Warning => DiagnosticSeverity::WARNING,
            EvalSeverity::Advice => DiagnosticSeverity::HINT,
            EvalSeverity::Disabled => DiagnosticSeverity::INFORMATION,
        }
    }
}

#[derive(Debug, Clone)]
/// Potential problems that occurred while parsing a starlark program.
pub struct EvalMessage {
    /// The path to the starlark program
    pub path: String,
    /// If present, where in the program the problem occurred.
    pub span: Option<ResolvedSpan>,
    /// How severed the problem is.
    pub severity: EvalSeverity,
    /// The general name of the issue.
    pub name: String,
    /// The details of the issue, generally displayed to the user.
    pub description: String,
    /// The full error details.
    pub full_error_with_span: Option<String>,
    /// The text referred to by `.span`
    pub original: Option<String>,
}

impl Display for EvalMessage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}:", self.severity, self.path)?;
        if let Some(span) = self.span {
            write!(f, "{}", span)?;
        }
        write!(f, " {}", self.description)
    }
}

impl EvalMessage {
    /// Convert from an `anyhow::Error`, including some type checking, to an `EvalMessage`
    pub fn from_anyhow(file: &str, x: &anyhow::Error) -> Self {
        match x.downcast_ref::<StarlarkDiagnostic>() {
            Some(
                d @ StarlarkDiagnostic {
                    message,
                    span: Some(span),
                    ..
                },
            ) => {
                let original = span.source_span().to_owned();
                let resolved_span = span.resolve_span();
                Self {
                    path: span.filename().to_owned(),
                    span: Some(resolved_span),
                    severity: EvalSeverity::Error,
                    name: "error".to_owned(),
                    description: format!("{:#}", message),
                    full_error_with_span: Some(d.to_string()),
                    original: Some(original),
                }
            }
            _ => Self {
                path: file.to_owned(),
                span: None,
                severity: EvalSeverity::Error,
                name: "error".to_owned(),
                description: format!("{:#}", x),
                full_error_with_span: None,
                original: None,
            },
        }
    }
}

impl From<Lint> for EvalMessage {
    fn from(x: Lint) -> Self {
        Self {
            path: x.location.filename().to_owned(),
            span: Some(x.location.resolve_span()),
            severity: if x.serious {
                EvalSeverity::Warning
            } else {
                // Start with all non-serious errors disabled, and ramp up from there
                EvalSeverity::Disabled
            },
            name: x.short_name,
            description: x.problem,
            full_error_with_span: None,
            original: Some(x.original),
        }
    }
}

impl From<EvalMessage> for Diagnostic {
    fn from(x: EvalMessage) -> Self {
        let range = match x.span {
            Some(s) => s.into(),
            _ => Range::default(),
        };
        Diagnostic::new(
            range,
            Some(x.severity.into()),
            Some(NumberOrString::String(x.name)),
            None,
            x.description,
            None,
            None,
        )
    }
}

fn kebab(xs: &str) -> String {
    let mut res = String::new();
    for x in xs.chars() {
        if x.is_uppercase() {
            if !res.is_empty() {
                res.push('-');
            }
            for y in x.to_lowercase() {
                res.push(y);
            }
        } else {
            res.push(x);
        }
    }
    res
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lint_kebab() {
        assert_eq!(kebab("Unreachable"), "unreachable");
        assert_eq!(kebab("UsingIgnored"), "using-ignored");
        assert_eq!(
            kebab("MissingReturnExpression"),
            "missing-return-expression"
        );
        assert_eq!(
            kebab("DuplicateTopLevelAssign"),
            "duplicate-top-level-assign"
        );
    }
}
