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

use smallvec::SmallVec;

#[derive(allocative::Allocative)]
pub enum ContextValue {
    Dyn(Arc<str>),
    Category(Category),
    Tags(SmallVec<[crate::ErrorTag; 1]>),
}

impl ContextValue {
    /// Returns a value that should be included in the error message
    pub(crate) fn as_display(&self) -> Option<Arc<str>> {
        match self {
            Self::Dyn(v) => Some(Arc::clone(v)),
            // Displaying the category in the middle of an error message doesn't seem useful
            Self::Category(_) => None,
            Self::Tags(_) => None,
        }
    }

    /// A way to display the context - this is only used to assist in debugging
    pub(crate) fn as_display_for_debugging(&self) -> Arc<str> {
        match self {
            Self::Dyn(v) => Arc::clone(v),
            Self::Category(category) => format!("{:?}", category).into(),
            Self::Tags(tags) => format!("{:?}", tags).into(),
        }
    }

    #[cfg(test)]
    pub(crate) fn assert_eq(&self, other: &Self) {
        match (self, other) {
            (ContextValue::Dyn(a), ContextValue::Dyn(b)) => {
                assert_eq!(a, b);
            }
            (ContextValue::Category(a), ContextValue::Category(b)) => {
                assert_eq!(a, b);
            }
            (ContextValue::Tags(a), ContextValue::Tags(b)) => {
                assert_eq!(a, b);
            }
            (_, _) => panic!("context variants don't match!"),
        }
    }
}

impl<T: fmt::Display> From<T> for ContextValue {
    fn from(value: T) -> Self {
        ContextValue::Dyn(format!("{}", value).into())
    }
}

#[derive(allocative::Allocative, PartialEq, Eq, Copy, Clone, Debug)]
pub enum Category {
    User,
    Infra,
}

impl From<Category> for ContextValue {
    fn from(value: Category) -> Self {
        ContextValue::Category(value)
    }
}

#[cfg(test)]
mod tests {
    use crate as buck2_error;

    #[derive(buck2_error_derive::Error, Debug)]
    #[error("test error")]
    struct TestError;

    #[test]
    fn test_category_not_in_formatting() {
        let e: crate::Error = TestError.into();
        let e = e.context("foo");
        let e2 = e.clone().context(crate::Category::User);
        assert_eq!(format!("{:#}", e), format!("{:#}", e2));
    }

    #[test]
    fn test_category_infra_preferred() {
        let e: crate::Error = TestError.into();
        let e = e
            .clone()
            .context(crate::Category::Infra)
            .context(crate::Category::User);
        assert_eq!(e.get_category(), Some(crate::Category::Infra));
    }
}
