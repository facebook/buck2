/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::pattern::ParsedPattern;
use buck2_core::pattern::TargetPattern;
use buck2_core::target::TargetLabel;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum VisibilityError {
    #[error(
        "`{0}` is not visible to `{1}` (run `buck2 uquery --output-attribute visibility {0}` to check the visibility)"
    )]
    NotVisibleTo(TargetLabel, TargetLabel),
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct VisibilityPattern(pub ParsedPattern<TargetPattern>);

/// Represents the visibility spec of a target. Note that targets in the same package will ignore the
/// visibility spec of each other.
#[derive(Debug, Eq, PartialEq, Hash)]
pub enum VisibilitySpecification {
    Public,
    // Default is used when a target doesn't specify any visibility.
    Default,
    VisibleTo(Vec<VisibilityPattern>),
}

impl VisibilitySpecification {
    pub fn is_visible_to(&self, target: &TargetLabel) -> bool {
        match self {
            VisibilitySpecification::Public => true,
            VisibilitySpecification::Default => false,
            VisibilitySpecification::VisibleTo(patterns) => {
                for pattern in patterns {
                    if pattern.0.matches(target) {
                        return true;
                    }
                }
                false
            }
        }
    }
}
