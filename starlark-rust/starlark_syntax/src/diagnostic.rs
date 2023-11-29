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

use crate::call_stack::CallStack;
use crate::codemap::FileSpan;
use crate::span_display::span_display;

/// A description of where in starlark execution the error happened.
#[derive(Debug)]
pub struct Diagnostic {
    /// Location where the error originated.
    pub span: Option<FileSpan>,

    /// Call stack where the error originated.
    pub call_stack: CallStack,
}

impl Diagnostic {
    /// Gets annotated snippets for a [`Diagnostic`].
    fn get_display_list<'a>(&'a self, annotation_label: &'a str, color: bool) -> impl Display + 'a {
        span_display(
            self.span.as_ref().map(|s| s.as_ref()),
            annotation_label,
            color,
        )
    }
}

/////////////////////////////////////////////////////////////////////
// DISPLAY RELATED UTILITIES
// Since formatting these types is difficult, we reuse the Rust compiler
// variants by doing a conversion using annotate-snippets
// (https://github.com/rust-lang/annotate-snippets-rs)

pub(crate) fn diagnostic_display(
    message: impl std::fmt::Debug + Display,
    diagnostic: &Diagnostic,
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
