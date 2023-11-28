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

use std::error::Error;
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

use crate::call_stack::CallStack;
use crate::codemap::CodeMap;
use crate::codemap::FileSpan;
use crate::codemap::Span;
use crate::span_display::span_display;

/// A diagnostic, but without the error message attached
#[derive(Debug)]
pub struct DiagnosticNoError {
    /// Location where the error originated.
    pub span: Option<FileSpan>,

    /// Call stack where the error originated.
    pub call_stack: CallStack,
}

/// An error plus its origination location and call stack.
///
/// The underlying [`message`](Diagnostic::message) is an [`anyhow::Error`].
/// The [`Diagnostic`] structure itself usually stored within an [`anyhow::Error`].
#[derive(Debug)]
pub struct Diagnostic {
    /// Underlying error for the [`Diagnostic`].
    /// Should _never_ be of type [`Diagnostic`] itself.
    pub message: anyhow::Error,
    /// The actual data about the diagnostic
    pub data: DiagnosticNoError,
}

impl Error for Diagnostic {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        self.message.source()
    }

    // TODO(nga): figure out how to do it with unstable rust.
    // fn backtrace(&self) -> Option<&std::backtrace::Backtrace> {
    //     Some(self.message.backtrace())
    // }
}

impl Diagnostic {
    /// Create a new [`Diagnostic`] containing an underlying error and span.
    /// If the given `message` is already a [`Diagnostic`] with a [`Span`],
    /// the new span will be ignored and the original `message` returned.
    pub fn new(message: impl Into<anyhow::Error>, span: Span, codemap: &CodeMap) -> anyhow::Error {
        Self::modify(message.into(), |d| d.set_span(span, codemap))
    }

    pub fn span(&self) -> Option<&FileSpan> {
        self.data.span.as_ref()
    }

    /// Modify an error by attaching diagnostic information to it - e.g. `span`/`call_stack`.
    /// If given an [`anyhow::Error`] which is a [`Diagnostic`], it will add the information to the
    /// existing [`Diagnostic`]. If not, it will wrap the error in [`Diagnostic`].
    #[cold]
    pub fn modify(mut err: anyhow::Error, f: impl FnOnce(&mut Diagnostic)) -> anyhow::Error {
        match err.downcast_mut::<Diagnostic>() {
            Some(diag) => {
                f(diag);
                err
            }
            _ => {
                let mut err = Self {
                    message: err,
                    data: DiagnosticNoError {
                        span: None,
                        call_stack: CallStack::default(),
                    },
                };
                f(&mut err);
                err.into()
            }
        }
    }

    /// Set the diagnostic's span, unless it's already been set.
    pub fn set_span(&mut self, span: Span, codemap: &CodeMap) {
        if self.data.span.is_none() {
            // We want the best span, which is likely the first person to set it
            self.data.span = Some(codemap.file_span(span));
        }
    }

    /// Set the diagnostic's call stack, unless it's already been set.
    pub fn set_call_stack(&mut self, call_stack: impl FnOnce() -> CallStack) {
        if self.data.call_stack.is_empty() {
            // We want the best call stack, which is likely the first person to set it
            self.data.call_stack = call_stack();
        }
    }

    /// Print an error to the stderr stream. If the error is a [`Diagnostic`] it will use
    /// color-codes when printing.
    ///
    /// Note that this function doesn't print any context information if the error is a
    /// [`Diagnostic`], so you might prefer to use `eprintln!("{:#}"), err)`
    /// if you suspect there is useful context (although you won't get pretty colors).
    pub fn eprint(err: &anyhow::Error) {
        match err.downcast_ref::<Diagnostic>() {
            None => eprintln!("{:#}", err),
            Some(diag) => diagnostic_stderr(&diag.message, &diag.data),
        }
    }
}

impl DiagnosticNoError {
    /// Gets annotated snippets for a [`Diagnostic`].
    fn get_display_list<'a>(&'a self, annotation_label: &'a str, color: bool) -> impl Display + 'a {
        span_display(
            self.span.as_ref().map(|s| s.as_ref()),
            annotation_label,
            color,
        )
    }
}

impl Display for Diagnostic {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // Not showing the context trace without `{:#}` or `{:?}` is the same thing that anyhow does
        let with_context = f.alternate() && self.message.source().is_some();
        diagnostic_display(&self.message, &self.data, false, f, with_context)
    }
}

/////////////////////////////////////////////////////////////////////
// DISPLAY RELATED UTILITIES
// Since formatting these types is difficult, we reuse the Rust compiler
// variants by doing a conversion using annotate-snippets
// (https://github.com/rust-lang/annotate-snippets-rs)

fn diagnostic_display(
    message: impl std::fmt::Debug + Display,
    diagnostic: &DiagnosticNoError,
    color: bool,
    f: &mut dyn fmt::Write,
    with_context: bool,
) -> fmt::Result {
    write!(f, "{}", diagnostic.call_stack)?;
    let annotation_label = format!("{}", message);
    // I set color to false here to make the comparison easier with tests (coloring
    // adds in pretty strange unicode chars).
    let display_list = diagnostic.get_display_list(&annotation_label, color);
    writeln!(f, "{}", display_list)?;
    // Print out the `Caused by:` trace (if exists) and rust backtrace (if enabled).
    // The trace printed comes from an [`anyhow::Error`] that is not a [`Diagnostic`].
    if with_context {
        writeln!(f, "\n\n{:?}", message)?;
    }

    Ok(())
}

fn diagnostic_stderr(message: impl std::fmt::Debug + Display, diagnostic: &DiagnosticNoError) {
    let mut stderr = String::new();
    diagnostic_display(message, diagnostic, true, &mut stderr, true).unwrap();
    eprint!("{}", stderr);
}
