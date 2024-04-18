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
    Tier(Tier),
    Tags(SmallVec<[crate::ErrorTag; 1]>),
}

impl ContextValue {
    /// Returns a value that should be included in the error message
    pub(crate) fn as_display(&self) -> Option<Arc<str>> {
        match self {
            Self::Dyn(v) => Some(Arc::clone(v)),
            // Displaying the category in the middle of an error message doesn't seem useful
            Self::Tier(_) => None,
            Self::Tags(_) => None,
        }
    }

    /// A way to display the context - this is only used to assist in debugging
    pub(crate) fn as_display_for_debugging(&self) -> Arc<str> {
        match self {
            Self::Dyn(v) => Arc::clone(v),
            Self::Tier(category) => format!("{:?}", category).into(),
            Self::Tags(tags) => format!("{:?}", tags).into(),
        }
    }

    #[cfg(test)]
    pub(crate) fn assert_eq(&self, other: &Self) {
        match (self, other) {
            (ContextValue::Dyn(a), ContextValue::Dyn(b)) => {
                assert_eq!(a, b);
            }
            (ContextValue::Tier(a), ContextValue::Tier(b)) => {
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

#[derive(
    allocative::Allocative,
    PartialEq,
    Eq,
    Copy,
    Clone,
    Debug,
    PartialOrd,
    Ord
)]
pub enum Tier {
    Input,
    Tier0,
}

impl Tier {
    pub fn combine(self, other: Option<Tier>) -> Tier {
        let Some(other) = other else { return self };
        std::cmp::max(self, other)
    }
}

impl From<Tier> for ContextValue {
    fn from(value: Tier) -> Self {
        ContextValue::Tier(value)
    }
}

#[cfg(test)]
mod tests {
    use crate::Tier;
    use crate::{self as buck2_error};

    #[derive(buck2_error_derive::Error, Debug)]
    #[error("test error")]
    struct TestError;

    #[test]
    fn test_category_not_in_formatting() {
        let e: crate::Error = TestError.into();
        let e = e.context("foo");
        let e2 = e.clone().context(crate::Tier::Input);
        assert_eq!(format!("{:#}", e), format!("{:#}", e2));
    }

    #[test]
    fn test_category_infra_preferred() {
        let e: crate::Error = TestError.into();
        let e = e
            .clone()
            .context(crate::Tier::Tier0)
            .context(crate::Tier::Input);
        assert_eq!(e.get_tier(), Some(crate::Tier::Tier0));
    }

    #[test]
    fn test_combine() {
        assert_eq!(Tier::Input.combine(None), Tier::Input);
        assert_eq!(Tier::Input.combine(Some(Tier::Input)), Tier::Input);
        assert_eq!(Tier::Input.combine(Some(Tier::Tier0)), Tier::Tier0);
        assert_eq!(Tier::Tier0.combine(Some(Tier::Input)), Tier::Tier0);
    }
}
