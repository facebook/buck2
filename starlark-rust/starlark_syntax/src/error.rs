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

use crate::call_stack::CallStack;
use crate::codemap::CodeMap;
use crate::codemap::FileSpan;
use crate::codemap::Span;
use crate::diagnostic::diagnostic_display;
use crate::diagnostic::Diagnostic;

/// An error produced by starlark.
///
/// This error is composed of an error kind, together with some diagnostic information indicating
/// where it occurred.
///
/// In order to prevent accidental conversions to `anyhow::Error`, this type intentionally does not
/// implement `std::error::Error`. That should probably change in the future.
#[derive(Debug)]
pub struct Error(Box<ErrorInner>);

impl Error {
    /// Create a new error
    pub fn new(kind: ErrorKind) -> Self {
        Self(Box::new(ErrorInner {
            kind,
            diagnostic: None,
        }))
    }

    /// Create a new error with a span
    pub fn new_spanned(kind: ErrorKind, span: Span, codemap: &CodeMap) -> Self {
        Self(Box::new(ErrorInner {
            kind,
            diagnostic: Some(Diagnostic {
                span: Some(codemap.file_span(span)),
                call_stack: CallStack::default(),
            }),
        }))
    }

    /// Create a new error with no diagnostic and of kind [`ErrorKind::Other`]
    pub fn new_other(e: impl std::error::Error + Send + Sync + 'static) -> Self {
        Self(Box::new(ErrorInner {
            kind: ErrorKind::Other(anyhow::Error::new(e)),
            diagnostic: None,
        }))
    }

    /// The kind of this error
    pub fn kind(&self) -> &ErrorKind {
        &self.0.kind
    }

    /// Convert the error into the underlying kind
    pub fn into_kind(self) -> ErrorKind {
        self.0.kind
    }

    pub fn has_diagnostic(&self) -> bool {
        self.0.diagnostic.is_some()
    }

    /// Convert this error into an `anyhow::Error`
    pub fn into_anyhow(self) -> anyhow::Error {
        struct Wrapped(Error);

        impl fmt::Display for Wrapped {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                fmt::Display::fmt(&self.0, f)
            }
        }

        impl fmt::Debug for Wrapped {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                fmt::Debug::fmt(&self.0, f)
            }
        }

        impl std::error::Error for Wrapped {
            fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
                self.0.kind().source()
            }
        }

        anyhow::Error::new(Wrapped(self))
    }

    /// Returns a value that can be used to format this error without including the diagnostic
    /// information
    ///
    /// This is the same as [`kind`](crate::Error::kind), just a bit more explicit.
    pub fn without_diagnostic<'a>(&'a self) -> impl fmt::Debug + fmt::Display + 'a {
        &self.0.kind
    }

    pub fn span(&self) -> Option<&FileSpan> {
        self.0.diagnostic.as_ref().and_then(|d| d.span.as_ref())
    }

    fn ensure_diagnostic(&mut self) -> &mut Diagnostic {
        self.0.diagnostic.get_or_insert_with(|| Diagnostic {
            span: None,
            call_stack: Default::default(),
        })
    }

    /// Set the span, unless it's already been set.
    pub fn set_span(&mut self, span: Span, codemap: &CodeMap) {
        let d = self.ensure_diagnostic();
        if d.span.is_none() {
            d.span = Some(codemap.file_span(span));
        }
    }

    /// Set the `call_stack` field, unless it's already been set.
    pub fn set_call_stack(&mut self, call_stack: impl FnOnce() -> CallStack) {
        let d = self.ensure_diagnostic();
        if d.call_stack.is_empty() {
            d.call_stack = call_stack();
        }
    }

    /// Print an error to the stderr stream. If the error has diagnostic information it will use
    /// color-codes when printing.
    ///
    /// Note that this function doesn't print any context information if the error is a diagnostic,
    /// so you might prefer to use `eprintln!("{:#}"), err)` if you suspect there is useful context
    /// (although you won't get pretty colors).
    pub fn eprint(&self) {
        if let Some(diag) = &self.0.diagnostic {
            let mut stderr = String::new();
            diagnostic_display(self.without_diagnostic(), diag, true, &mut stderr, true).unwrap();
            eprint!("{}", stderr);
        } else {
            eprintln!("{:#}", self)
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(diag) = &self.0.diagnostic {
            // Not showing the context trace without `{:#}` or `{:?}` is the same thing that anyhow does
            let with_context = f.alternate() && self.0.kind.source().is_some();
            diagnostic_display(self.without_diagnostic(), diag, false, f, with_context)
        } else {
            fmt::Display::fmt(&self.without_diagnostic(), f)
        }
    }
}

#[derive(Debug)]
struct ErrorInner {
    kind: ErrorKind,
    diagnostic: Option<Diagnostic>,
}

/// The different kinds of errors that can be produced by starlark
#[non_exhaustive]
pub enum ErrorKind {
    /// Fallback option
    ///
    /// This is used in two cases:
    ///  1. For errors produced by starlark which have not yet been assigned their own kind
    ///  2. When a native function invoked as a part of starlark evaluation returns a
    ///     `anyhow::Error`
    Other(anyhow::Error),
}

impl ErrorKind {
    /// The source of the error, akin to `[std::error::Error::source]`
    pub fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Other(e) => e.source(),
        }
    }
}

impl fmt::Debug for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Other(e) => fmt::Debug::fmt(e, f),
        }
    }
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Other(e) => fmt::Display::fmt(e, f),
        }
    }
}

impl From<anyhow::Error> for Error {
    fn from(e: anyhow::Error) -> Self {
        Self(Box::new(ErrorInner {
            kind: ErrorKind::Other(e),
            diagnostic: None,
        }))
    }
}
