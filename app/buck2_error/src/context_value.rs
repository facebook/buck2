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
    pub(crate) fn display(&self) -> Option<String> {
        match self {
            Self::Dyn(v) => Some(format!("{}", v)),
            Self::Typed(e) => e.display(),
            Self::Tags(_) => None,
            Self::StringTag(..) => None,
            Self::StarlarkError(..) => None,
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

impl std::fmt::Debug for ContextValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Dyn(v) => f.write_str(v),
            Self::Tags(tags) => write!(f, "{tags:?}"),
            Self::Typed(v) => write!(f, "{}", v.display().unwrap_or_default()),
            Self::StringTag(v) => f.write_str(&v.tag),
            Self::StarlarkError(v) => write!(f, "{}", v.display_with_message("")),
        }
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

pub trait TypedContext: allocative::Allocative + Send + Sync + std::any::Any + 'static {
    fn eq(&self, other: &dyn TypedContext) -> bool;

    fn display(&self) -> Option<String>;
}

impl<T: TypedContext> From<T> for ContextValue {
    fn from(value: T) -> Self {
        ContextValue::Typed(Arc::new(value))
    }
}

#[derive(Clone, allocative::Allocative, Debug, PartialEq, Eq, Hash)]
pub struct StarlarkContext {
    pub call_stack: CallStack,
    pub span: Option<FileSpan>,
    /// If true, we render the span / call stack in buck output.
    /// Otherwise, data is only for LSP to fish out.
    pub show_span_in_buck_output: bool,
}

impl StarlarkContext {
    /// Concatenate call stacks.
    ///
    /// Pass the inner context as the argument, i.e. the one closer to the root cause.
    pub fn concat(mut self, inner: Option<Self>) -> Self {
        if let Some(mut inner) = inner {
            self.call_stack.frames.append(&mut inner.call_stack.frames);

            Self {
                call_stack: self.call_stack,
                span: inner.span,
                show_span_in_buck_output: true,
            }
        } else {
            self
        }
    }

    pub fn display_with_message(&self, message: &str) -> String {
        let span = span_display(self.span.as_ref().map(|s| s.as_ref()), message, false);
        if self.call_stack.is_empty() {
            format!("{}", span)
        } else {
            let call_stack = self.call_stack.to_string();
            format!("{}\n{}", span, call_stack)
        }
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
