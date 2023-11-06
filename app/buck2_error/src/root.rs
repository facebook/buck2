/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::sync::Arc;

use mappable_rc::Marc;

use crate::ErrorType;

static NEXT_ROOT_ID: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

/// Uniquely identifies an instance of an error root
///
/// This can be used to deduplicate errors, ie determine that they are caused by the same thing.
///
/// Note that while this type implements `Hash` and `Debug`, the behavior of both implementations is
/// unstable across executions, and one should be careful not to cause non-determinism with it.
#[derive(
    allocative::Allocative,
    Copy,
    Clone,
    Debug,
    dupe::Dupe,
    PartialEq,
    Eq,
    Hash
)]
pub struct UniqueRootId(u64);

type DynLateFormat =
    dyn Fn(&anyhow::Error, &mut fmt::Formatter<'_>) -> fmt::Result + Send + Sync + 'static;

#[derive(allocative::Allocative)]
pub(crate) struct ErrorRoot {
    id: UniqueRootId,
    #[allocative(skip)]
    inner: Marc<anyhow::Error>,
    #[allocative(skip)] // FIXME(JakobDegen): "Implementation is not general enough"
    late_format: Option<Arc<DynLateFormat>>,
    error_type: Option<ErrorType>,
    source_location: Option<String>,
}

impl ErrorRoot {
    pub(crate) fn new<E: std::error::Error + Send + Sync + 'static>(
        inner: E,
        late_format: Option<fn(&E, &mut fmt::Formatter<'_>) -> fmt::Result>,
        error_type: Option<ErrorType>,
        source_location: Option<String>,
    ) -> Self {
        let id = UniqueRootId(NEXT_ROOT_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed));
        let inner = Marc::new(anyhow::Error::new(inner));
        // Have to write this kind of weird to get the compiler to infer a higher ranked closure
        let Some(late_format) = late_format else {
            return Self {
                id,
                inner,
                late_format: None,
                error_type,
                source_location,
            };
        };
        Self {
            id,
            inner,
            late_format: Some(Arc::new(move |e: &anyhow::Error, fmt| {
                late_format(e.downcast_ref().unwrap(), fmt)
            })),
            error_type,
            source_location,
        }
    }

    /// Should not typically be used. Use the appropriate `anyhow::Error: From<crate::Error>`
    /// instead.
    pub(crate) fn new_anyhow(e: Marc<anyhow::Error>, source_location: Option<String>) -> Self {
        Self {
            id: UniqueRootId(NEXT_ROOT_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed)),
            inner: e,
            late_format: None,
            error_type: None,
            source_location,
        }
    }

    pub(crate) fn into_anyhow_for_late_format<'a>(&'a self) -> Option<anyhow::Error> {
        self.late_format
            .clone()
            .map(|f| AnyhowWrapperForFormat(self.inner.clone(), Some(f)).into())
    }

    pub(crate) fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.inner.source()
    }

    pub(crate) fn into_anyhow_for_format(&self) -> anyhow::Error {
        AnyhowWrapperForFormat(self.inner.clone(), None).into()
    }

    /// Equality comparison for use in tests only
    #[cfg(test)]
    pub(crate) fn test_equal(&self, other: &Self) -> bool {
        Marc::ptr_eq(&self.inner, &other.inner)
    }

    pub(crate) fn downcast_ref<T: fmt::Display + fmt::Debug + Send + Sync + 'static>(
        &self,
    ) -> Option<&T> {
        self.inner.downcast_ref()
    }

    pub(crate) fn id(&self) -> UniqueRootId {
        self.id
    }

    pub(crate) fn error_type(&self) -> Option<ErrorType> {
        self.error_type
    }

    pub(crate) fn source_location(&self) -> Option<&str> {
        self.source_location.as_deref()
    }
}

impl fmt::Debug for ErrorRoot {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.inner, f)
    }
}

struct AnyhowWrapperForFormat(Marc<anyhow::Error>, Option<Arc<DynLateFormat>>);

impl fmt::Debug for AnyhowWrapperForFormat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}

impl fmt::Display for AnyhowWrapperForFormat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(late_format) = &self.1 {
            late_format(&self.0, f)
        } else {
            fmt::Display::fmt(&self.0, f)
        }
    }
}

impl std::error::Error for AnyhowWrapperForFormat {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        std::error::Error::source(&**self.0)
    }
}
