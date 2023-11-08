/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::error::Error as StdError;
use std::fmt;

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

#[derive(allocative::Allocative)]
pub(crate) struct ErrorRoot {
    id: UniqueRootId,
    #[allocative(skip)]
    inner: Marc<anyhow::Error>,
    error_type: Option<ErrorType>,
    source_location: Option<String>,
}

impl ErrorRoot {
    pub(crate) fn new<E: StdError + Send + Sync + 'static>(
        inner: E,
        error_type: Option<ErrorType>,
        source_location: Option<String>,
    ) -> Self {
        let id = UniqueRootId(NEXT_ROOT_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed));
        let inner = Marc::new(anyhow::Error::new(inner));
        Self {
            id,
            inner,
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
            error_type: None,
            source_location,
        }
    }

    pub(crate) fn source(&self) -> Option<&(dyn StdError + 'static)> {
        self.inner.source()
    }

    pub(crate) fn inner(&self) -> Marc<anyhow::Error> {
        self.inner.clone()
    }

    /// Equality comparison for use in tests only
    #[cfg(test)]
    pub(crate) fn test_equal(&self, other: &Self) -> bool {
        Marc::ptr_eq(&self.inner, &other.inner)
    }

    pub(crate) fn downcast_ref<T: StdError + Send + Sync + 'static>(&self) -> Option<&T> {
        // We attempt to downcast using two different strategies. The second strategy below is to
        // just walk the source chain of the error and try downcasting each one in turn. This works
        // for almost everything, except for things added via `anyhow::Context`, since
        // `anyhow::Context` produces source chains that don't correctly reflect the context type.
        // To mitigate this, we first try a regular `anyhow::Error::downcast_ref`.
        if let Some(e) = self.inner.downcast_ref::<T>() {
            return Some(e);
        }
        // Occasionally when recovering a `buck2_error::Error` from an `anyhow::Error` or another
        // type, we might wrap a `Marc<dyn StdError + Send + Sync + 'static>` in a `anyhow::Error`
        // to be able to fit it into the `Marc<anyhow::Error>` that this type expects. That means
        // that in order to recover correct downcasting, we need to "unwrap" that. This is a bit
        // ugly - the alternative though would be to allow this type to store either a
        // `Marc<anyhow::Error>` or a `Marc<dyn StdError + Send + Sync + 'static>`, which might be
        // better? It's not clear though
        let e: &(dyn StdError + 'static) = (*self.inner).as_ref();
        let e = if let Some(e) = e.downcast_ref::<Marc<dyn StdError + Send + Sync + 'static>>() {
            &**e
        } else {
            e
        };
        let mut cur = e;
        loop {
            if let Some(out) = cur.downcast_ref::<T>() {
                return Some(out);
            }
            cur = cur.source()?;
        }
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
