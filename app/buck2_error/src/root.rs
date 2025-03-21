/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

use buck2_data::error::ErrorTag;

use crate::source_location::SourceLocation;

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
    description: String,
    error_tag: ErrorTag,
    source_location: SourceLocation,
    action_error: Option<buck2_data::ActionError>,
}

impl ErrorRoot {
    pub(crate) fn new(
        description: String,
        error_tag: ErrorTag,
        source_location: SourceLocation,
        action_error: Option<buck2_data::ActionError>,
    ) -> Self {
        let id = UniqueRootId(NEXT_ROOT_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed));
        Self {
            id,
            description,
            error_tag,
            source_location,
            action_error,
        }
    }

    pub(crate) fn description(&self) -> &str {
        self.description.as_ref()
    }

    /// Equality comparison for use in tests only
    #[cfg(test)]
    pub(crate) fn test_equal(&self, other: &Self) -> bool {
        self.description == other.description
    }

    pub(crate) fn id(&self) -> UniqueRootId {
        self.id
    }

    pub(crate) fn error_tag(&self) -> ErrorTag {
        self.error_tag
    }

    pub(crate) fn source_location(&self) -> &SourceLocation {
        &self.source_location
    }

    pub fn action_error(&self) -> Option<&buck2_data::ActionError> {
        self.action_error.as_ref()
    }
}

impl fmt::Debug for ErrorRoot {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.description, f)
    }
}
