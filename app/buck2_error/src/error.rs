/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any::Any;
use std::fmt;
use std::sync::Arc;

use crate::context_value::ContextValue;
use crate::root::ErrorRoot;
use crate::ErrorType;
use crate::UniqueRootId;

/// The core error type provided by this crate.
///
/// While this type has many of the features of `anyhow::Error`, in most places you should continue
/// to use `anyhow`. This type is only expected to appear on a small number of APIs which require a
/// clonable error.
#[derive(allocative::Allocative, Clone, dupe::Dupe)]
pub struct Error(pub(crate) Arc<ErrorKind>);

/// The actual error representation.
///
/// The representation is expected to take on a significant bit of additional complexity in the
/// future - the current version is an initial MVP.
///
/// Right now, this type can represent an error root, together with a stack of context information.
#[derive(allocative::Allocative)]
pub(crate) enum ErrorKind {
    // This `Arc` should ideally be a `Box`. However, that doesn't work right now because of the
    // implementation of `into_anyhow_for_format`.
    Root(ErrorRoot),
    /// For now we use untyped context to maximize compatibility with anyhow.
    WithContext(ContextValue, Error),
    /// Indicates that the error has been emitted, ie shown to the user.
    Emitted(Error),
}

impl Error {
    pub fn new<E: std::error::Error + Send + Sync + 'static>(e: E) -> Self {
        Self(Arc::new(ErrorKind::Root(ErrorRoot::new(e, None, None))))
    }

    pub fn new_with_options<E: std::error::Error + Send + Sync + 'static>(
        e: E,
        f: Option<fn(&E, &mut fmt::Formatter<'_>) -> fmt::Result>,
        t: Option<ErrorType>,
    ) -> Self {
        Self(Arc::new(ErrorKind::Root(ErrorRoot::new(e, f, t))))
    }

    fn iter_kinds<'a>(&'a self) -> impl Iterator<Item = &'a ErrorKind> {
        let mut cur = Some(self);
        std::iter::from_fn(move || {
            let out = cur?;
            match &*out.0 {
                ErrorKind::WithContext(_, next) | ErrorKind::Emitted(next) => cur = Some(next),
                ErrorKind::Root(_) => cur = None,
            };
            Some(out.0.as_ref())
        })
    }

    fn root(&self) -> &ErrorRoot {
        let Some(ErrorKind::Root(r)) = self.iter_kinds().last() else {
            unreachable!()
        };
        r
    }

    pub(crate) fn iter_context<'a>(&'a self) -> impl Iterator<Item = &'a ContextValue> {
        self.iter_kinds().filter_map(|kind| match kind {
            ErrorKind::WithContext(ctx, _) => Some(ctx),
            _ => None,
        })
    }

    pub fn mark_emitted(self) -> Self {
        Self(Arc::new(ErrorKind::Emitted(self)))
    }

    pub fn is_emitted(&self) -> bool {
        self.iter_kinds()
            .any(|kind| matches!(kind, ErrorKind::Emitted(_)))
    }

    /// Returns possible additional late formatting for this error
    ///
    /// Most errors are only shown to the user once. However, some errors, specifically action
    /// errors, are shown to the user twice: Once when the error occurs, and again at the end of the
    /// build in the form of a short "Failed to build target" summary.
    ///
    /// In cases like these, this function returns the additional information to show to the user at
    /// the end of the build.
    pub fn get_late_format<'a>(&'a self) -> Option<impl fmt::Display + fmt::Debug + 'a> {
        crate::format::into_anyhow_for_format(self, true)
    }

    pub fn get_error_type(&self) -> Option<ErrorType> {
        self.root().error_type()
    }

    /// Only intended to be used for debugging, helps to understand the structure of the error
    pub fn get_stack_for_debug(&self) -> String {
        use fmt::Write;
        let mut s = String::new();
        for kind in self.iter_kinds() {
            match kind {
                ErrorKind::Root(r) => {
                    writeln!(
                        s,
                        "ROOT: late format: {}:\n{:#?}",
                        r.into_anyhow_for_late_format().is_some(),
                        r
                    )
                    .unwrap();
                }
                ErrorKind::Emitted(_) => {
                    writeln!(s, "EMITTED").unwrap();
                }
                ErrorKind::WithContext(ctx, _) => {
                    writeln!(s, "CONTEXT: {:#}", ctx.as_display_for_debugging()).unwrap();
                }
            }
        }
        s
    }

    pub fn downcast_ref<T: fmt::Display + fmt::Debug + Send + Sync + 'static>(&self) -> Option<&T> {
        self.iter_kinds().find_map(|kind| match kind {
            ErrorKind::Root(r) => r.downcast_ref(),
            ErrorKind::WithContext(ContextValue::Dyn(ctx), _) => {
                (ctx.as_ref() as &dyn Any).downcast_ref()
            }
            // Intentionally don't support downcasting for other `ContextValue` variants, it should
            // not be necessary
            ErrorKind::WithContext(_, _) => None,
            ErrorKind::Emitted(_) => None,
        })
    }

    pub fn root_id(&self) -> UniqueRootId {
        self.root().id()
    }
}

#[cfg(test)]
mod tests {
    #[derive(Debug, thiserror::Error)]
    #[error("Test")]
    struct TestError;

    #[test]
    fn test_emitted_works() {
        let e: crate::Error = TestError.into();
        assert!(!e.is_emitted());
        let e = e.mark_emitted();
        assert!(e.is_emitted());
        let e: anyhow::Error = e.into();
        let e: crate::Error = e.context("context").into();
        assert!(e.is_emitted());
    }

    #[derive(Debug, thiserror::Error)]
    #[error("Context A")]
    struct ContextA;

    #[derive(Debug, thiserror::Error)]
    #[error("Context B")]
    struct ContextB;

    #[derive(Debug, thiserror::Error)]
    #[error("Context C")]
    struct ContextC;

    #[test]
    fn test_downcast() {
        let e: anyhow::Error = TestError.into();
        let e = e.context(ContextA);
        let e: crate::Error = e.into();
        let e = e.context(ContextB);
        let e: anyhow::Error = e.into();
        let e = e.context(ContextC);
        let e: crate::Error = e.into();

        // Context added via `buck2_error` can always be accessed via downcasting. Context added via
        // `anyhow` cannot - it can only be accessed before the first time the error is converted to
        // `buck2_error`.
        assert!(e.downcast_ref::<TestError>().is_some());
        assert!(e.downcast_ref::<ContextA>().is_some());
        assert!(e.downcast_ref::<ContextB>().is_some());
        assert!(e.downcast_ref::<ContextC>().is_none());
    }

    #[test]
    fn test_late_format_has_context() {
        let e = crate::Error::new_with_options(
            TestError,
            Some(|_e, f| write!(f, "Late formatted!")),
            None,
        );
        let e = e.context(ContextA).context(ContextB);

        let s = format!("{:#}", e.get_late_format().unwrap());
        assert!(s.contains("Late formatted!"));
        assert!(s.contains("Context A"));
        assert!(s.contains("Context B"));
    }

    #[test]
    fn test_root_id() {
        let e1: crate::Error = TestError.into();
        let e1x = e1.clone().context("context");
        let e1y = e1.clone().context("context2");

        let e2: crate::Error = TestError.into();

        assert_eq!(e1.root_id(), e1x.root_id());
        assert_eq!(e1.root_id(), e1y.root_id());
        assert_eq!(e1x.root_id(), e1y.root_id());

        assert_ne!(e1.root_id(), e2.root_id());
    }
}
