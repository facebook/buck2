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
use crate::eval::CallStack;
use crate::values::string::fast_string;
use crate::values::string::CharIndex;

pub(crate) mod did_you_mean;

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

    /// Call stack of what called what. Most recent frames are at the end.
    pub call_stack: CallStack,
}

/// A frame of the call-stack.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Frame {
    /// The name of the entry on the call-stack.
    pub name: String,
    /// The location of the definition, or [`None`] for native Rust functions.
    pub location: Option<FileSpan>,
}

impl Display for Frame {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(&self.name)?;
        if let Some(loc) = &self.location {
            write!(f, " (called from {})", loc)?;
        }
        Ok(())
    }
}

fn truncate_snippet(snippet: &str, max_len: usize) -> (&str, &str) {
    let ddd = "...";
    assert!(max_len >= ddd.len());
    match fast_string::split_at(snippet, CharIndex(max_len - ddd.len())) {
        None => (snippet, ""),
        Some((_, b)) if b.chars().nth(3).is_none() => (snippet, ""),
        Some((a, _)) => (a, "..."),
    }
}

impl Frame {
    pub(crate) fn write_two_lines(
        &self,
        indent: &str,
        caller: &str,
        write: &mut dyn fmt::Write,
    ) -> fmt::Result {
        if let Some(location) = &self.location {
            let line = location
                .file
                .source_line_at_pos(location.span.begin())
                .trim();
            let (line, ddd) = truncate_snippet(line, 80);
            writeln!(
                write,
                "{}* {}:{}, in {}",
                indent,
                location.file.filename(),
                location.file.find_line(location.span.begin()) + 1,
                // Note we print caller function here as in Python, not callee,
                // so in the stack trace, top frame is printed without executed function name.
                caller,
            )?;
            writeln!(write, "{}    {}{}", indent, line, ddd)?;
        } else {
            // Python just omits builtin functions in the traceback.
            writeln!(write, "{}File <builtin>, in {}", indent, caller)?;
        }
        Ok(())
    }
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
}

impl Display for Diagnostic {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        diagnostic_display(self, f)
    }
}

/////////////////////////////////////////////////////////////////////
// DISPLAY RELATED UTILITIES
// Since formatting these types is difficult, we reuse the Rust compiler
// variants by doing a conversion using annotate-snippets
// (https://github.com/rust-lang/annotate-snippets-rs)

fn get_display_list_for_diagnostic<'a>(
    annotation_label: &'a str,
    x: &'a Diagnostic,
    color: bool,
) -> DisplayList<'a> {
    fn convert_span_to_range_relative_to_first_line(
        diagnostic_span: Span,
        start_column: usize,
    ) -> (usize, usize) {
        let span_length = diagnostic_span.len() as usize;
        (start_column, start_column + span_length)
    }

    fn convert_span_to_slice<'a>(span: &'a FileSpan) -> Slice<'a> {
        let region = span.resolve_span();

        // we want the source_span to capture any whitespace ahead of the diagnostic span to
        // get the column numbers correct in the DisplayList, and any trailing source code
        // on the last line for context.
        let first_line_span = span.file.line_span(region.begin_line);
        let last_line_span = span.file.line_span(region.end_line);
        let source_span = span.span.merge(first_line_span).merge(last_line_span);

        Slice {
            source: span.file.source_span(source_span),
            line_start: 1 + region.begin_line,
            origin: Some(span.file.filename()),
            fold: false,
            annotations: vec![SourceAnnotation {
                label: "",
                annotation_type: AnnotationType::Error,
                range: convert_span_to_range_relative_to_first_line(span.span, region.begin_column),
            }],
        }
    }

    let slice = x.span.as_ref().map(convert_span_to_slice);

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

fn diagnostic_display(diagnostic: &Diagnostic, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{}", &diagnostic.call_stack)?;
    let annotation_label = format!("{:#}", diagnostic.message);
    // I set color to false here to make the comparison easier with tests (coloring
    // adds in pretty strange unicode chars).
    let display_list = get_display_list_for_diagnostic(&annotation_label, diagnostic, false);
    writeln!(f, "{}", display_list)
}

fn diagnostic_stderr(diagnostic: &Diagnostic) {
    eprint!("{}", diagnostic.call_stack);
    let annotation_label = format!("{:#}", diagnostic.message);
    let display_list = get_display_list_for_diagnostic(&annotation_label, diagnostic, true);
    eprintln!("{}", display_list);
}

#[cfg(test)]
mod tests {
    use crate::errors::truncate_snippet;

    #[test]
    fn test_truncate_snippet() {
        assert_eq!(("", ""), truncate_snippet("", 5));
        assert_eq!(("a", ""), truncate_snippet("a", 5));
        assert_eq!(("ab", ""), truncate_snippet("ab", 5));
        assert_eq!(("abc", ""), truncate_snippet("abc", 5));
        assert_eq!(("abcd", ""), truncate_snippet("abcd", 5));
        assert_eq!(("abcde", ""), truncate_snippet("abcde", 5));
        assert_eq!(("ab", "..."), truncate_snippet("abcdef", 5));
        assert_eq!(("ab", "..."), truncate_snippet("abcdefg", 5));
        assert_eq!(("ab", "..."), truncate_snippet("abcdefgh", 5));
        assert_eq!(("ab", "..."), truncate_snippet("abcdefghi", 5));
        assert_eq!(("Київ", ""), truncate_snippet("Київ", 5));
        assert_eq!(("па", "..."), truncate_snippet("паляниця", 5));
    }
}
