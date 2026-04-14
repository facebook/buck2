/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::error::Error as StdError;
use std::fmt;
use std::sync::Arc;

use dupe::Dupe;

use crate::__for_macro::ContextValue;
use crate::DynLateFormat;
use crate::context_value::StarlarkContext;
use crate::error::ErrorKind;

/// We currently implement formatting in the laziest way possible - we convert to an equivalent
/// `anyhow::Error` and format that.
///
/// In the long term, this is not what we want to do. Writing our own error formatter is not that
/// hard and will give us a huge amount of flexibility. However, the goal right now is to get large
/// amounts of `anyhow` compatibility with minimal work, and this achieves that.
///
/// If `should_late_format` is set, attempts to use the late formatter instead of the standard one.
/// That might not be present, so additionally returns a bool which indicates whether the late
/// formatter was used.
pub(crate) fn into_anyhow_for_format(
    mut error: &crate::Error,
    should_late_format: bool,
) -> (anyhow::Error, bool) {
    let mut context_stack = Vec::new();
    let mut was_late_formatted = false;

    let base = loop {
        match error.0.as_ref() {
            ErrorKind::Root(root) => {
                break AnyhowWrapperForFormat::Root(root.description().to_owned());
            }
            ErrorKind::WithContext(context, inner) => {
                context_stack.push(context);
                error = inner;
            }
            ErrorKind::Emitted(late_format, inner) => {
                if should_late_format {
                    was_late_formatted = true;
                    break AnyhowWrapperForFormat::LateFormat(late_format.dupe());
                }
                error = inner;
            }
        }
    };

    let root_msg = match &base {
        AnyhowWrapperForFormat::Root(s) => s.clone(),
        AnyhowWrapperForFormat::LateFormat(_) => String::new(),
    };

    let chain = build_display_chain(&context_stack, &root_msg);

    // Build the anyhow error from the chain. The first element replaces the
    // root if a StarlarkContext absorbed it; otherwise it IS the root message.
    let mut iter = chain.into_iter();
    let first = iter.next().unwrap_or_default();
    let mut out: anyhow::Error = if first == root_msg {
        base.into()
    } else {
        // Root was absorbed into a starlark span display.
        AnyhowWrapperForFormat::Root(first).into()
    };
    for seg in iter {
        out = out.context(seg);
    }
    (out, was_late_formatted)
}

// --- Intermediate representation for building the display chain ---

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum DisplaySegment {
    /// A plain context string.
    String(String),
    /// A starlark span + call stack. The label is the `.context("msg")`
    /// that immediately precedes the StarlarkContext in the error chain.
    Starlark {
        sc: StarlarkContext,
        label: Option<String>,
    },
}

impl DisplaySegment {
    pub(crate) fn render(self) -> String {
        match self {
            Self::String(s) => s,
            Self::Starlark { sc, label } => sc.display_with_message(&label.unwrap_or_default()),
        }
    }
}

/// Walk the context stack (inner to outer) and build a display chain.
///
/// Returns a Vec<String> where the first element is the root (possibly
/// absorbed into a starlark display) and subsequent elements are context
/// layers from inner to outer.
///
/// StarlarkContexts consume the preceding context as their span label
/// (the `.context("msg").context(SC)` convention). Adjacent starlark
/// contexts get their call stacks concatenated, with the inner SC's
/// label emitted as a plain context.
fn build_display_chain(context_stack: &[&ContextValue], root_msg: &str) -> Vec<String> {
    let mut segments: Vec<DisplaySegment> = vec![DisplaySegment::String(root_msg.to_owned())];
    segments.extend(context_stack.iter().rev().filter_map(|cv| match cv {
        ContextValue::StarlarkError(sc) if sc.show_span_in_buck_output => {
            Some(DisplaySegment::Starlark {
                sc: sc.clone(),
                label: None,
            })
        }
        ContextValue::StarlarkError(_) => None,
        _ => cv.display().map(DisplaySegment::String),
    }));

    let segments = absorb_starlark_labels(segments);
    let segments = concat_starlark_segments(segments);
    segments.into_iter().map(DisplaySegment::render).collect()
}

/// Each Starlark segment absorbs the immediately preceding String segment
/// as its label (the `.context("msg").context(SC)` convention). This also accounts
/// for starlark context immediately after the root error.
pub(crate) fn absorb_starlark_labels(segments: Vec<DisplaySegment>) -> Vec<DisplaySegment> {
    let mut result: Vec<DisplaySegment> = Vec::with_capacity(segments.len());
    for seg in segments {
        if let DisplaySegment::Starlark { sc, .. } = seg {
            let label = match result.last() {
                Some(DisplaySegment::String(_)) => match result.pop() {
                    Some(DisplaySegment::String(s)) => Some(s),
                    _ => None,
                },
                _ => None,
            };
            result.push(DisplaySegment::Starlark { sc, label });
        } else {
            result.push(seg);
        }
    }
    result
}

/// Merge adjacent Starlark segments by concatenating their call stacks.
/// The inner segment's label is emitted as a Context before the merged segment.
pub(crate) fn concat_starlark_segments(segments: Vec<DisplaySegment>) -> Vec<DisplaySegment> {
    let mut result: Vec<DisplaySegment> = Vec::with_capacity(segments.len());
    for seg in segments {
        if let DisplaySegment::Starlark {
            sc: new_sc,
            label: new_label,
        } = seg
        {
            if let Some(DisplaySegment::Starlark { .. }) = result.last() {
                // Adjacent: merge inner (prev) into outer (new).
                let prev = result.pop().unwrap();
                if let DisplaySegment::Starlark {
                    sc: prev_sc,
                    label: prev_label,
                } = prev
                {
                    // If the outer has a label, the inner's label becomes
                    // a plain context. If the outer has no label, it
                    // inherits the inner's (like the old error_msg behavior).
                    let merged_label = if new_label.is_some() {
                        if let Some(l) = prev_label {
                            result.push(DisplaySegment::String(l));
                        }
                        new_label
                    } else {
                        prev_label
                    };
                    result.push(DisplaySegment::Starlark {
                        sc: new_sc.concat(Some(prev_sc)),
                        label: merged_label,
                    });
                }
            } else {
                result.push(DisplaySegment::Starlark {
                    sc: new_sc,
                    label: new_label,
                });
            }
        } else {
            result.push(seg);
        }
    }
    result
}

#[test]
fn test_concat_starlark_segments() {
    let sc = StarlarkContext {
        call_stack: starlark_syntax::call_stack::CallStack { frames: vec![] },
        span: None,
        show_span_in_buck_output: true,
    };
    assert_eq!(
        concat_starlark_segments(vec![
            DisplaySegment::String("root".into()),
            DisplaySegment::Starlark {
                sc: sc.clone(),
                label: Some("first".into()),
            },
            DisplaySegment::Starlark {
                sc: sc.clone(),
                label: Some("second".into()),
            },
        ]),
        vec![
            DisplaySegment::String("root".into()),
            DisplaySegment::String("first".into()),
            DisplaySegment::Starlark {
                sc: sc.clone(),
                label: Some("second".into()),
            }
        ]
    );
}

impl fmt::Debug for crate::Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&into_anyhow_for_format(self, false).0, f)
    }
}

impl fmt::Display for crate::Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&into_anyhow_for_format(self, false).0, f)
    }
}

enum AnyhowWrapperForFormat {
    Root(String),
    LateFormat(Arc<DynLateFormat>),
}

impl fmt::Debug for AnyhowWrapperForFormat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Root(root) => fmt::Display::fmt(root, f),
            Self::LateFormat(late_format) => late_format(f),
        }
    }
}

impl fmt::Display for AnyhowWrapperForFormat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Root(root) => fmt::Display::fmt(root, f),
            Self::LateFormat(late_format) => late_format(f),
        }
    }
}

impl StdError for AnyhowWrapperForFormat {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        None
    }
}
