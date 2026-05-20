/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;

use starlark_syntax::codemap::FileSpan;

use crate::context_value::ContextValue;
use crate::context_value::StarlarkContext;
use crate::error::ErrorKind;
use crate::format::DisplaySegment;
use crate::format::absorb_starlark_labels;

/// A diagnostic rendered for LSP. This is specific to which
/// file was being evaluated at the time. For example, we don't
/// pollute the printout with a rendering of the span that the
/// diagnostic is attached to and placed next to in the editor.
pub struct LspDiagnostic {
    /// Generally Some; however may be None if we could not find
    /// any spans for this file in the entire error trace.
    /// We look everywhere, through all the call stacks, etc.
    pub span: Option<FileSpan>,
    /// Rendered from anyhow::Error, it's a stack of error context.
    pub rendered: String,
}

impl fmt::Debug for LspDiagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(span) = self.span.as_ref() {
            write!(f, "{}: ", span)?;
        }
        write!(f, "{}", self.rendered.trim())
    }
}

impl crate::Error {
    /// Transform into a list of LSP diagnostics for the file being
    /// evaluated-on-save.
    ///
    /// For the same error, we give a different rendering depending
    /// on the file that is being evaluated. For example, if there is
    /// a fail() in a .bzl file, you'll see the error attached to a
    /// span of the call when you evaluate that .bzl file.
    /// But evaluate a BUCK file that imports the same .bzl file,
    /// and you see "eval error during load". Technically these would
    /// be different errors but you'll also see the context frames
    /// rendered with full source listings from the BUCK file, e.g.
    ///
    /// error: fail: message here
    ///   --> blah.bzl:10:2-15
    ///    |
    /// 10 | fail("message here")
    ///    |
    ///
    /// Whereas if you render this in blah.bzl, you do not need to see
    /// a printout of the code you are literally already looking at.
    /// So we skip rendering this.
    ///
    pub fn into_lsp_diagnostics(self, eval_file_project_relative_path: &str) -> Vec<LspDiagnostic> {
        {
            let mut buck2_error = self;
            let mut full_context_stack = vec![];

            loop {
                match buck2_error.0.as_ref() {
                    ErrorKind::Root(_root) => {
                        full_context_stack
                            .push(ContextValue::Dyn(format!("{buck2_error:?}").into()));
                        break;
                    }
                    ErrorKind::WithContext(context_value, inner) => {
                        // When we see a context
                        match context_value {
                            ContextValue::Tags(..) | ContextValue::StringTag(..) => (), // ignore
                            _ => {
                                full_context_stack.push(context_value.clone());
                            }
                        }

                        buck2_error = inner.clone();
                    }
                    _ => return vec![],
                }
            }

            let mut context_span = None;
            let mut context_stack: Vec<ContextValue> = Vec::new();
            let mut stacks = vec![];

            // rev => first iteration is root cause
            //
            // If root cause is in this file, we want to gather C3 and
            // C2 with it, giving [C3, C2, Root]. This should highlight
            // the span for Root.
            //
            // Then, additionally, we want [Context1, Context0], highlighted on
            // the span for Context1.
            //
            //     C0
            //
            //     Caused by:
            //         C1 this.bzl:29:4-7
            //         C2
            //         C3
            //         Root     this.bzl:80:9-16
            //
            //
            // If the root is not in this file:
            //
            //     C0
            //
            //     Caused by:
            //         C1 this.bzl:29:4-7
            //         C2
            //         C3 this.bzl:80:9-16  (e.g. attr label parsing)
            //         C4                   (some native buck context)
            //         Root                 (some native buck error)
            //
            //
            // Then we should gather [C2, C3, C4, Root] in one origin diagnostic.
            // We call C3 the proximal cause for this file.
            //
            //
            // For a BUCK file loading some other file with an eval error, and
            // LSP wants diagnostics for the BUCK file:
            //
            //     C0  From load at thing/BUCK:4
            //
            //     Caused by:
            //
            //         C1 other.bzl:123:4-5
            //         C2
            //         C3 another.bzl:456:7-8
            //         Root
            //
            // Then you apply the same algorithm as for gathering [C2, C3, C4, Root]
            // above and you get [C0, C1, C2, C3, Root] highlighted at C0's span.
            // We call C0 the proximal cause for thing/BUCK.
            //
            let mut seen_proximal = false;
            for ctx in full_context_stack.into_iter().rev() {
                if let ContextValue::StarlarkError(sc) = &ctx {
                    if let Some(span) = sc.find_span_in_file(eval_file_project_relative_path) {
                        if seen_proximal && (context_span.is_some() || !context_stack.is_empty()) {
                            // The last non-SC item in context_stack is the label
                            // for this SC (from .context("msg").context(SC) convention).
                            // It belongs with the new diagnostic, not the previous one.
                            let label = match context_stack.last() {
                                Some(ContextValue::StarlarkError(_)) | None => None,
                                _ => context_stack.pop(),
                            };
                            stacks.push((
                                context_span.replace(span.clone()),
                                std::mem::take(&mut context_stack),
                            ));
                            if let Some(l) = label {
                                context_stack.push(l);
                            }
                        }
                        context_span.replace(span.clone());
                        seen_proximal = true;
                    }
                }
                context_stack.push(ctx);
            }
            if !context_stack.is_empty() {
                stacks.push((context_span.clone(), context_stack));
            }

            let is_eval_file = |sc: &StarlarkContext| {
                sc.find_span_in_file(eval_file_project_relative_path)
                    .is_some()
            };

            let format_stack = |stack: &mut Vec<ContextValue>| -> Option<String> {
                // Convert to Segments. Eval-file SCs are skipped (the LSP
                // span already covers them). Non-eval-file SCs are always
                // kept for rendering, even if show_span_in_buck_output is
                // false (the user can't see those files).
                let segments: Vec<DisplaySegment> = stack
                    .drain(..)
                    .filter_map(|ctx| match &ctx {
                        ContextValue::StarlarkError(sc) if is_eval_file(sc) => None,
                        ContextValue::StarlarkError(sc) => Some(DisplaySegment::Starlark {
                            sc: sc.clone(),
                            label: None,
                        }),
                        _ => ctx.display().map(DisplaySegment::String),
                    })
                    .collect();

                let segments = absorb_starlark_labels(segments);

                let mut iter = segments.into_iter().map(DisplaySegment::render);
                let root = iter.next()?;
                let mut err = anyhow::format_err!("{}", root);
                for part in iter {
                    err = err.context(part);
                }
                Some(format!("{err:?}"))
            };

            let mut out = vec![];
            let mut leftover_stack = Vec::new();

            for (span, mut stack) in stacks {
                leftover_stack.append(&mut stack);
                let Some(span) = span else {
                    continue;
                };
                if let Some(rendered) = format_stack(&mut leftover_stack) {
                    out.push(LspDiagnostic {
                        span: Some(span),
                        rendered,
                    });
                }
            }

            if !leftover_stack.is_empty() {
                if let Some(rendered) = format_stack(&mut leftover_stack) {
                    out.push(LspDiagnostic {
                        span: None,
                        rendered,
                    });
                }
            }

            out
        }
    }
}
