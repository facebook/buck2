/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::fmt::Display;
use std::sync::Arc;

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
    WithContext(
        #[allocative(skip)] Arc<dyn Display + Send + Sync + 'static>,
        Error,
    ),
    /// Indicates that the error has been emitted, ie shown to the user.
    Emitted(Error),
}

type DynLateFormat = dyn Fn(&(dyn std::error::Error + 'static), &mut fmt::Formatter<'_>) -> fmt::Result
    + Send
    + Sync
    + 'static;

#[derive(allocative::Allocative)]
pub(crate) struct ErrorRoot {
    #[allocative(skip)]
    inner: Arc<dyn std::error::Error + Send + Sync + 'static>,
    #[allocative(skip)] // FIXME(JakobDegen): "Implementation is not general enough"
    late_format: Option<Box<DynLateFormat>>,
}

impl ErrorRoot {
    pub(crate) fn inner(&self) -> &Arc<dyn std::error::Error + Send + Sync + 'static> {
        &self.inner
    }
}

impl Error {
    pub fn new<E: std::error::Error + Send + Sync + 'static>(e: E) -> Self {
        Self::new_from_arc(Arc::new(e), None)
    }

    pub fn new_with_late_format<E: std::error::Error + Send + Sync + 'static>(
        e: E,
        f: impl Fn(&E, &mut fmt::Formatter<'_>) -> fmt::Result + Send + Sync + 'static,
    ) -> Self {
        Self::new_from_arc(
            Arc::new(e),
            Some(Box::new(
                move |e: &(dyn std::error::Error + 'static), fmt| f(e.downcast_ref().unwrap(), fmt),
            )),
        )
    }

    pub(crate) fn new_from_arc(
        arc: Arc<dyn std::error::Error + Send + Sync + 'static>,
        late_format: Option<Box<DynLateFormat>>,
    ) -> Self {
        Self(Arc::new(ErrorKind::Root(ErrorRoot {
            inner: arc,
            late_format,
        })))
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
    pub fn get_late_format<'a>(&'a self) -> Option<impl fmt::Display + 'a> {
        struct DisplayWrapper<'a>(&'a (dyn std::error::Error + 'static), &'a DynLateFormat);

        impl<'a> fmt::Display for DisplayWrapper<'a> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.1(self.0, f)
            }
        }

        let root = self.root();
        Some(DisplayWrapper(root.inner(), root.late_format.as_ref()?))
    }
}
