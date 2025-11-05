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
use std::sync::Arc;

use smallvec::SmallVec;
use starlark_syntax::call_stack::CallStack;
use starlark_syntax::codemap::FileSpan;
use starlark_syntax::span_display::span_display;

#[derive(allocative::Allocative, Clone)]
pub enum ContextValue {
    Dyn(Arc<str>),
    Tags(SmallVec<[crate::ErrorTag; 1]>),
    Typed(Arc<dyn TypedContext>),
    StarlarkError(StarlarkContext),
    StringTag(StringTag),
}

#[derive(allocative::Allocative, Debug, Clone, Eq, PartialEq)]
pub struct StringTag {
    pub tag: String,
}

impl ContextValue {
    /// Returns whether the context should be included in the error message
    pub(crate) fn should_display(&self) -> bool {
        match self {
            Self::Dyn(..) => true,
            Self::Typed(e) => e.should_display(),
            Self::Tags(_) => false,
            Self::StringTag(..) => false,
            Self::StarlarkError(..) => false,
        }
    }

    #[cfg(test)]
    pub(crate) fn assert_eq(&self, other: &Self) {
        match (self, other) {
            (ContextValue::Dyn(a), ContextValue::Dyn(b)) => {
                assert_eq!(a, b);
            }
            (ContextValue::Tags(a), ContextValue::Tags(b)) => {
                assert_eq!(a, b);
            }
            (ContextValue::Typed(left), ContextValue::Typed(right)) => {
                assert!(left.eq(&**right))
            }
            (ContextValue::StringTag(a), ContextValue::StringTag(b)) => {
                assert_eq!(a, b);
            }
            (ContextValue::StarlarkError(a), ContextValue::StarlarkError(b)) => {
                assert_eq!(a, b);
            }
            (_, _) => panic!("context variants don't match!"),
        }
    }
}

impl std::fmt::Display for ContextValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Dyn(v) => f.write_str(v),
            Self::Tags(tags) => write!(f, "{tags:?}"),
            Self::Typed(v) => std::fmt::Display::fmt(v, f),
            Self::StringTag(v) => f.write_str(&v.tag),
            Self::StarlarkError(v) => write!(f, "{v}"),
        }
    }
}

impl std::fmt::Debug for ContextValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        std::fmt::Display::fmt(&self, f)
    }
}

impl From<String> for ContextValue {
    fn from(value: String) -> Self {
        ContextValue::Dyn(value.into())
    }
}

impl From<&str> for ContextValue {
    fn from(value: &str) -> Self {
        ContextValue::Dyn(value.into())
    }
}

pub trait TypedContext:
    allocative::Allocative + Send + Sync + std::fmt::Display + std::any::Any + 'static
{
    fn eq(&self, other: &dyn TypedContext) -> bool;

    fn should_display(&self) -> bool {
        true
    }
}

impl<T: TypedContext> From<T> for ContextValue {
    fn from(value: T) -> Self {
        ContextValue::Typed(Arc::new(value))
    }
}

#[derive(Clone, allocative::Allocative, Debug, PartialEq, Eq, Hash)]
pub struct StarlarkContext {
    pub call_stack: CallStack,
    /// May be empty when we try to inject some StarlarkContext and there is already a StarlarkContext
    /// near the top of the error.
    pub error_msg: String,
    pub span: Option<FileSpan>,
    /// If true, we render the span / call stack in buck output.
    /// Otherwise, data is only for LSP to fish out.
    pub show_span_in_buck_output: bool,
    ///
    /// We don't render the root error if `replaces_root_error` is set.
    /// We render this in preference because it has a starlark
    /// span and call stack.
    ///
    /// This is only to be set if error_msg contains a rendering of the root error.
    ///
    /// This field enables us to keep an actual StarlarkContext tha can be rendered
    /// as an LSP span instead of literally replacing the root context with a
    /// stringified span.
    ///
    pub replaces_root_error: bool,
}

impl StarlarkContext {
    /// Concatenate call stacks, but drop the error messages.
    ///
    /// Pass the inner context as the argument, i.e. the one closer to the root cause.
    pub fn concat(&self, inner: Option<Self>) -> Self {
        if let Some(inner) = inner {
            let frames = self
                .call_stack
                .frames
                .iter()
                .chain(inner.call_stack.frames.iter())
                .cloned()
                .collect();

            Self {
                call_stack: CallStack { frames },
                error_msg: self.error_msg.clone(),
                span: inner.span.clone(),
                replaces_root_error: inner.replaces_root_error,
                show_span_in_buck_output: true,
            }
        } else {
            self.clone()
        }
    }

    pub fn find_span_in_file(&self, filename: &str) -> Option<FileSpan> {
        self.span
            .as_ref()
            .filter(|x| x.filename() == filename)
            .or_else(|| {
                self.call_stack
                    .frames
                    .iter()
                    .filter_map(|f| f.location.as_ref())
                    .find(|span| span.filename() == filename)
            })
            .cloned()
    }
}

impl std::fmt::Display for StarlarkContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let span = span_display(
            self.span.as_ref().map(|s| s.as_ref()),
            self.error_msg.as_str(),
            false,
        );
        write!(f, "{}\n{}", self.call_stack, span)
    }
}

#[cfg(test)]
mod tests {
    use crate::ErrorTag;
    use crate::Tier;
    use crate::{self as buck2_error};

    #[derive(buck2_error_derive::Error, Debug)]
    #[error("test error")]
    #[buck2(tag = TestOnly)]
    struct TestError;

    #[test]
    fn test_category_not_in_formatting() {
        let e: crate::Error = TestError.into();
        let e = e.context("foo");
        let e2 = e.clone().tag([ErrorTag::Input]);
        assert_eq!(format!("{e:#}"), format!("{:#}", e2));
    }

    #[test]
    fn test_category_infra_preferred() {
        let e: crate::Error = TestError.into();
        let e = e.clone().tag([ErrorTag::Input]);
        assert_eq!(e.get_tier(), Some(Tier::Tier0));
    }
}
