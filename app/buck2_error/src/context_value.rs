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
pub(crate) trait DisplayAndAny:
    fmt::Display + std::any::Any + Send + Sync + 'static
{
}

impl<T: fmt::Display + std::any::Any + Send + Sync + 'static> DisplayAndAny for T {}

#[derive(allocative::Allocative)]
pub(crate) enum ContextValue {
    Dyn(#[allocative(skip)] Arc<dyn DisplayAndAny>),
}

impl ContextValue {
    /// Returns a value that should be included in the error message
    pub(crate) fn as_display(&self) -> Option<Arc<dyn DisplayAndAny>> {
        match self {
            Self::Dyn(v) => Some(Arc::clone(v)),
        }
    }

    /// A way to display the context - this is only used to assist in debugging
    pub(crate) fn as_display_for_debugging(&self) -> Arc<dyn DisplayAndAny> {
        match self {
            Self::Dyn(v) => Arc::clone(v),
        }
    }

    #[cfg(test)]
    pub(crate) fn assert_eq(&self, other: &Self) {
        match (self, other) {
            (ContextValue::Dyn(a), ContextValue::Dyn(b)) => {
                assert_eq!(format!("{}", a), format!("{}", b))
            }
        }
    }
}
