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

use allocative::Allocative;
use buck2_core::pattern::ParsedPattern;
use buck2_core::pattern::TargetPatternExtra;
use buck2_core::target::label::TargetLabel;
use gazebo::prelude::SliceExt;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum VisibilityError {
    #[error(
        "`{0}` is not visible to `{1}` (run `buck2 uquery --output-attribute visibility {0}` to check the visibility)"
    )]
    NotVisibleTo(TargetLabel, TargetLabel),
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, Allocative, derive_more::Display)]
pub struct VisibilityPattern(pub ParsedPattern<TargetPatternExtra>);

/// Represents the visibility spec of a target. Note that targets in the same package will ignore the
/// visibility spec of each other.
#[derive(Debug, Eq, PartialEq, Hash, Clone, Allocative)]
pub enum VisibilitySpecification {
    Public,
    // Default is used when a target doesn't specify any visibility.
    Default,
    VisibleTo(Box<Box<[VisibilityPattern]>>),
}

impl VisibilitySpecification {
    pub fn is_visible_to(&self, target: &TargetLabel) -> bool {
        match self {
            VisibilitySpecification::Public => true,
            VisibilitySpecification::Default => false,
            VisibilitySpecification::VisibleTo(patterns) => {
                for pattern in &***patterns {
                    if pattern.0.matches(target) {
                        return true;
                    }
                }
                false
            }
        }
    }

    pub(crate) fn to_json(&self) -> serde_json::Value {
        let list = match self {
            VisibilitySpecification::Public => vec![serde_json::Value::String("PUBLIC".to_owned())],
            VisibilitySpecification::Default => Vec::new(),
            VisibilitySpecification::VisibleTo(patterns) => {
                patterns.map(|p| serde_json::Value::String(p.to_string()))
            }
        };
        serde_json::Value::Array(list)
    }
}

impl Display for VisibilitySpecification {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VisibilitySpecification::Public => write!(f, "[\"PUBLIC\"]"),
            VisibilitySpecification::Default => write!(f, "[]"),
            VisibilitySpecification::VisibleTo(patterns) => {
                write!(f, "[")?;
                for (i, pattern) in patterns.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "\"{}\"", pattern)?;
                }
                write!(f, "]")
            }
        }
    }
}
