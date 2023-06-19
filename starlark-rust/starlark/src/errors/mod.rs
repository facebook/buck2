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

//! Error types used by Starlark, mostly [`Diagnostic`].

use std::error::Error;
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

use annotate_snippets::display_list::DisplayList;
use annotate_snippets::display_list::FormatOptions;
use annotate_snippets::snippet::Annotation;
use annotate_snippets::snippet::AnnotationType;
use annotate_snippets::snippet::Slice;
use annotate_snippets::snippet::Snippet;
use annotate_snippets::snippet::SourceAnnotation;

pub use crate::analysis::EvalMessage;
pub use crate::analysis::EvalSeverity;
pub use crate::analysis::Lint;
use crate::codemap::CodeMap;
use crate::codemap::FileSpan;
use crate::codemap::Span;
pub use crate::errors::frame::Frame;
use crate::eval::CallStack;
use crate::values::string::fast_string;

pub(crate) mod did_you_mean;
pub(crate) mod frame;

/// An error plus its origination location and call stack.
///
/// The underlying [`message`](Diagnostic::message) is an [`anyhow::Error`].
/// The [`Diagnostic`] structure itself usually stored within an [`anyhow::Error`].
#[derive(Debug)]
pub struct Diagnostic {
    /// Underlying error for the [`Diagnostic`].
    /// Should _never_ be of type [`Diagnostic`] itself.
    pub message: anyhow::Error,

    /// Location where the error originated.
    pub span: Option<FileSpan>,

    /// Call stack where the error originated.
    pub call_stack: CallStack,
}

impl Error for Diagnostic {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        // We do have an underlying source (namely `self.message`), but if we return
        // it then `anyhow` will print it with `{:#}`, and we already print it in our
        // `Display`, which would cause it to appear twice.
        // Therefore, we say we have no source.
        None
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
    pub(crate) fn new(
        message: impl Into<anyhow::Error>,
        span: Span,
        codemap: &CodeMap,
    ) -> anyhow::Error {
        Self::modify(message.into(), |d| d.set_span(span, codemap))
    }

    /// Modify an error by attaching diagnostic information to it - e.g. `span`/`call_stack`.
    /// If given an [`anyhow::Error`] which is a [`Diagnostic`], it will add the information to the
    /// existing [`Diagnostic`]. If not, it will wrap the error in [`Diagnostic`].
    pub fn modify(mut err: anyhow::Error, f: impl FnOnce(&mut Diagnostic)) -> anyhow::Error {
        match err.downcast_mut::<Diagnostic>() {
            Some(diag) => {
                f(diag);
                err
            }
            _ => {
                let mut err = Self {
                    message: err,
                    span: None,
                    call_stack: CallStack::default(),
                };
                f(&mut err);
                err.into()
            }
        }
    }

    /// Set the [`Diagnostic::span`] field, unless it's already been set.
    pub(crate) fn set_span(&mut self, span: Span, codemap: &CodeMap) {
        if self.span.is_none() {
            // We want the best span, which is likely the first person to set it
            self.span = Some(codemap.file_span(span));
        }
    }

    /// Set the [`Diagnostic::call_stack`] field, unless it's already been set.
    pub fn set_call_stack(&mut self, call_stack: impl FnOnce() -> CallStack) {
        if self.call_stack.is_empty() {
            // We want the best call stack, which is likely the first person to set it
            self.call_stack = call_stack();
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
            Some(diag) => diagnostic_stderr(diag),
        }
    }

    /// Gets annotated snippets for a [`Diagnostic`].
    fn get_display_list<'a>(&'a self, annotation_label: &'a str, color: bool) -> DisplayList<'a> {
        fn convert_span_to_slice<'a>(span: &'a FileSpan) -> Slice<'a> {
            let region = span.resolve_span();

            // we want the source_span to capture any whitespace ahead of the diagnostic span to
            // get the column numbers correct in the DisplayList, and any trailing source code
            // on the last line for context.
            let first_line_span = span.file.line_span(region.begin_line);
            let last_line_span = span.file.line_span(region.end_line);
            let source_span = span.span.merge(first_line_span).merge(last_line_span);
            let source = span.file.source_span(source_span);

            // We want to highlight the span, which needs to be relative to source, and in
            // characters.
            // Our spans are in terms of bytes, but our resolved spans in terms of characters.
            let range_start_chars = region.begin_column;
            let range_len_chars = fast_string::len(span.source_span()).0;

            Slice {
                source,
                line_start: 1 + region.begin_line,
                origin: Some(span.file.filename()),
                fold: false,
                annotations: vec![SourceAnnotation {
                    label: "",
                    annotation_type: AnnotationType::Error,
                    range: (range_start_chars, range_start_chars + range_len_chars),
                }],
            }
        }

        let slice = self.span.as_ref().map(convert_span_to_slice);

        let snippet = Snippet {
            title: Some(Annotation {
                label: Some(annotation_label),
                id: None,
                annotation_type: AnnotationType::Error,
            }),
            footer: Vec::new(),
            slices: slice.map(|s| vec![s]).unwrap_or_default(),
            opt: FormatOptions {
                color,
                ..Default::default()
            },
        };

        DisplayList::from(snippet)
    }
}

impl Display for Diagnostic {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        diagnostic_display(self, false, f)
    }
}

/////////////////////////////////////////////////////////////////////
// DISPLAY RELATED UTILITIES
// Since formatting these types is difficult, we reuse the Rust compiler
// variants by doing a conversion using annotate-snippets
// (https://github.com/rust-lang/annotate-snippets-rs)

fn diagnostic_display(diagnostic: &Diagnostic, color: bool, f: &mut dyn fmt::Write) -> fmt::Result {
    write!(f, "{}", diagnostic.call_stack)?;
    let annotation_label = format!("{}", diagnostic.message);
    // I set color to false here to make the comparison easier with tests (coloring
    // adds in pretty strange unicode chars).
    let display_list = diagnostic.get_display_list(&annotation_label, color);
    writeln!(f, "{}", display_list)?;
    // Print out the `Caused by:` trace (if exists) and rust backtrace (if enabled).
    // The trace printed comes from an [`anyhow::Error`] that is not a [`Diagnostic`].
    if diagnostic.message.source().is_some() {
        writeln!(f, "\n\n{:?}", diagnostic.message)?;
    }

    Ok(())
}

fn diagnostic_stderr(diagnostic: &Diagnostic) {
    let mut stderr = String::new();
    diagnostic_display(diagnostic, true, &mut stderr).unwrap();
    eprint!("{}", stderr);
}
