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
                    if sc.replaces_root_error {
                        context_stack.pop();
                    }
                    if let Some(span) = sc.find_span_in_file(eval_file_project_relative_path) {
                        if seen_proximal && (context_span.is_some() || !context_stack.is_empty()) {
                            stacks.push((
                                context_span.replace(span.clone()),
                                std::mem::take(&mut context_stack),
                            ));
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

            let format_sc = |sc: &StarlarkContext| -> String {
                if is_eval_file(sc) {
                    sc.error_msg.clone()
                } else {
                    format!("{sc}")
                }
            };

            let format_ctx = |ctx: &ContextValue| -> String {
                if let ContextValue::StarlarkError(sc) = ctx {
                    format_sc(sc)
                } else {
                    format!("{ctx}")
                }
            };

            let format_stack = |stack: &mut Vec<ContextValue>| {
                let mut iter = stack.drain(..);
                let root = iter.next().unwrap();

                let mut err = anyhow::format_err!("{}", format_ctx(&root));
                let mut file_relevant = None;
                let mut write_fr = false;
                if let ContextValue::StarlarkError(sc) = &root
                    && is_eval_file(sc)
                {
                    file_relevant = Some(sc.error_msg.clone());
                }
                for ctx in iter {
                    if let ContextValue::StarlarkError(sc) = &ctx
                        && is_eval_file(sc)
                    {
                        err = err.context(format_sc(sc));
                        file_relevant = Some(sc.error_msg.clone());
                        write_fr = false;
                        continue;
                    }
                    err = err.context(format!("{ctx}"));
                    write_fr = true;
                }

                // Lift the file-relevant one from which we derived the span
                // to the top so it becomes the error message in the editor
                if let Some(fr) = file_relevant.filter(|_| write_fr) {
                    err = err.context(fr);
                }
                format!("{err:?}")
            };

            let mut out = vec![];
            let mut leftover_stack = Vec::new();

            for (span, mut stack) in stacks {
                leftover_stack.append(&mut stack);
                let Some(span) = span else {
                    continue;
                };
                let rendered = format_stack(&mut leftover_stack);
                out.push(LspDiagnostic {
                    span: Some(span),
                    rendered,
                });
            }

            if !leftover_stack.is_empty() {
                let rendered = format_stack(&mut leftover_stack);
                out.push(LspDiagnostic {
                    span: None,
                    rendered,
                });
            }

            out
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::starlark_error::create_starlark_context;

    #[test]
    fn into_lsp_diagnostic() {
        use starlark_syntax::codemap::CodeMap;
        use starlark_syntax::codemap::Pos;
        use starlark_syntax::codemap::Span;
        // these errors are all a bit nonsense, they're made up.
        let bzl_file = CodeMap::new(
            "path/to/test.bzl".to_owned(),
            "# invalid\ndef and(): pass\ncall()".to_owned(),
        );
        let buck_file = CodeMap::new(
            "somecell/BUCK".to_owned(),
            r#"load("@path//to:test.bzl")"#.to_owned(),
        );
        let def_span = Span::new(Pos::new(14), Pos::new(17));
        let call_span = Span::new(Pos::new(26), Pos::new(30));
        let origin = starlark_syntax::Error::new_spanned(
            starlark_syntax::ErrorKind::Parser(anyhow::format_err!("root cause",)),
            def_span,
            &bzl_file,
        );
        let buck = crate::Error::from(origin)
            .context("inner context during call")
            .context(create_starlark_context(
                format!(
                    "called at {}",
                    bzl_file.file_span(call_span).resolve_span().begin,
                ),
                Some(bzl_file.file_span(call_span)),
                true,
            ))
            .context("outer context")
            .context(create_starlark_context(
                format!("From load at {}:1", buck_file.filename(),),
                Some(buck_file.file_span(buck_file.full_span())),
                true,
            ));
        let diag_bzl = buck.clone().into_lsp_diagnostics(bzl_file.filename());
        eprintln!("diag from bzl: {:#?}", diag_bzl);

        eprintln!();
        eprintln!();
        eprintln!("------------------------------------------------");
        eprintln!("------------------------------------------------");
        eprintln!("------------------------------------------------");
        eprintln!();
        let diag_buck = buck.clone().into_lsp_diagnostics(buck_file.filename());
        eprintln!("diag from BUCK: {:#?}", diag_buck);
    }
}
