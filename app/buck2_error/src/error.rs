/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

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

#[derive(allocative::Allocative)]
pub(crate) struct ErrorRoot {
    #[allocative(skip)]
    pub(crate) inner: Arc<dyn std::error::Error + Send + Sync + 'static>,
}

impl ErrorRoot {
    pub(crate) fn inner(&self) -> &Arc<dyn std::error::Error + Send + Sync + 'static> {
        &self.inner
    }
}

impl Error {
    pub fn new<E: std::error::Error + Send + Sync + 'static>(e: E) -> Self {
        Self(Arc::new(ErrorKind::Root(ErrorRoot { inner: Arc::new(e) })))
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

    pub fn mark_emitted(self) -> Self {
        Self(Arc::new(ErrorKind::Emitted(self)))
    }

    pub fn is_emitted(&self) -> bool {
        self.iter_kinds()
            .any(|kind| matches!(kind, ErrorKind::Emitted(_)))
    }
}
