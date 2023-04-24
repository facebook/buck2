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
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_core::pattern::ParsedPattern;
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

impl VisibilityPattern {
    pub const PUBLIC: &'static str = "PUBLIC";
}

/// Represents the visibility spec of a target. Note that targets in the same package will ignore the
/// visibility spec of each other.
#[derive(Default, Debug, Eq, PartialEq, Hash, Clone, Allocative)]
pub enum VisibilitySpecification {
    Public,
    // Default is used when a target doesn't specify any visibility.
    #[default]
    Default,
    VisibleTo(Box<Box<[VisibilityPattern]>>),
}

#[derive(Default, Debug, Eq, PartialEq, Hash, Clone, Allocative)]
pub enum WithinViewSpecification {
    // Default is used when a target doesn't specify any visibility.
    #[default]
    Public,
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
            VisibilitySpecification::Public => vec![serde_json::Value::String(
                VisibilityPattern::PUBLIC.to_owned(),
            )],
            VisibilitySpecification::Default => Vec::new(),
            VisibilitySpecification::VisibleTo(patterns) => {
                patterns.map(|p| serde_json::Value::String(p.to_string()))
            }
        };
        serde_json::Value::Array(list)
    }

    pub fn extend_with(&self, other: &VisibilitySpecification) -> VisibilitySpecification {
        match (self, other) {
            (VisibilitySpecification::Public, _) | (_, VisibilitySpecification::Public) => {
                VisibilitySpecification::Public
            }
            (VisibilitySpecification::Default, other) => other.clone(),
            (this, VisibilitySpecification::Default) => this.clone(),
            (
                VisibilitySpecification::VisibleTo(this),
                VisibilitySpecification::VisibleTo(other),
            ) => VisibilitySpecification::VisibleTo(Box::new(
                this.iter().chain(&***other).cloned().collect(),
            )),
        }
    }
}

impl Display for VisibilitySpecification {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VisibilitySpecification::Public => write!(f, "[\"{}\"]", VisibilityPattern::PUBLIC),
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

impl WithinViewSpecification {
    pub fn extend_with(&self, other: &WithinViewSpecification) -> WithinViewSpecification {
        match (self, other) {
            (WithinViewSpecification::Public, _) | (_, WithinViewSpecification::Public) => {
                WithinViewSpecification::Public
            }
            (WithinViewSpecification::Default, other) => other.clone(),
            (this, WithinViewSpecification::Default) => this.clone(),
            (
                WithinViewSpecification::VisibleTo(this),
                WithinViewSpecification::VisibleTo(other),
            ) => WithinViewSpecification::VisibleTo(Box::new(
                this.iter().chain(&***other).cloned().collect(),
            )),
        }
    }
}
