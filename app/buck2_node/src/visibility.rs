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
use std::fmt::Formatter;

use allocative::Allocative;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_core::pattern::ParsedPattern;
use buck2_core::target::label::TargetLabel;
use buck2_util::arc_str::ThinArcSlice;
use dupe::Dupe;
use gazebo::prelude::SliceExt;
use thiserror::Error;

use crate::attrs::attr_type::any_matches::AnyMatches;

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

    pub fn testing_new(pattern: &str) -> VisibilityPattern {
        VisibilityPattern(ParsedPattern::testing_parse(pattern))
    }
}

#[derive(derive_more::Display)]
#[display(fmt = "\"{}\"", _0)]
struct VisibilityPatternQuoted<'a>(&'a VisibilityPattern);

#[derive(Debug, Eq, PartialEq, Hash, Clone, Dupe, Allocative)]
pub enum VisibilityPatternList {
    Public,
    List(ThinArcSlice<VisibilityPattern>),
}

impl VisibilityPatternList {
    fn is_empty(&self) -> bool {
        match self {
            VisibilityPatternList::Public => false,
            VisibilityPatternList::List(patterns) => patterns.is_empty(),
        }
    }

    fn to_json(&self) -> serde_json::Value {
        let list = match self {
            VisibilityPatternList::Public => vec![serde_json::Value::String(
                VisibilityPattern::PUBLIC.to_owned(),
            )],
            VisibilityPatternList::List(patterns) => {
                patterns.map(|p| serde_json::Value::String(p.to_string()))
            }
        };
        serde_json::Value::Array(list)
    }

    fn extend_with(&self, other: &VisibilityPatternList) -> VisibilityPatternList {
        match (self, other) {
            (VisibilityPatternList::Public, _) | (_, VisibilityPatternList::Public) => {
                VisibilityPatternList::Public
            }
            (VisibilityPatternList::List(this), VisibilityPatternList::List(other)) => {
                VisibilityPatternList::List(this.iter().chain(other).cloned().collect())
            }
        }
    }

    fn testing_parse(patterns: &[&str]) -> VisibilityPatternList {
        if patterns.contains(&VisibilityPattern::PUBLIC) {
            VisibilityPatternList::Public
        } else {
            VisibilityPatternList::List(
                patterns
                    .iter()
                    .map(|p| VisibilityPattern::testing_new(p))
                    .collect(),
            )
        }
    }
}

impl Display for VisibilityPatternList {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            VisibilityPatternList::Public => write!(f, "[\"{}\"]", VisibilityPattern::PUBLIC),
            VisibilityPatternList::List(patterns) => display_container::fmt_container(
                f,
                "[",
                "]",
                patterns.iter().map(VisibilityPatternQuoted),
            ),
        }
    }
}

impl AnyMatches for VisibilityPatternList {
    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool> {
        match self {
            VisibilityPatternList::Public => filter(VisibilityPattern::PUBLIC),
            VisibilityPatternList::List(patterns) => {
                for p in patterns {
                    if filter(&p.to_string())? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
        }
    }
}

/// Represents the visibility spec of a target. Note that targets in the same package will ignore the
/// visibility spec of each other.
#[derive(Debug, Eq, PartialEq, Hash, Clone, Dupe, Allocative)]
pub struct VisibilitySpecification(pub VisibilityPatternList);

impl Default for VisibilitySpecification {
    fn default() -> Self {
        Self::DEFAULT
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, Dupe, Allocative)]
pub struct WithinViewSpecification(pub VisibilityPatternList);

impl Default for WithinViewSpecification {
    fn default() -> Self {
        Self::PUBLIC
    }
}

impl VisibilitySpecification {
    pub const DEFAULT: VisibilitySpecification =
        VisibilitySpecification(VisibilityPatternList::List(ThinArcSlice::empty()));

    pub fn is_visible_to(&self, target: &TargetLabel) -> bool {
        match &self.0 {
            VisibilityPatternList::Public => true,
            VisibilityPatternList::List(patterns) => {
                for pattern in patterns {
                    if pattern.0.matches(target) {
                        return true;
                    }
                }
                false
            }
        }
    }

    pub(crate) fn to_json(&self) -> serde_json::Value {
        self.0.to_json()
    }

    pub fn extend_with(&self, other: &VisibilitySpecification) -> VisibilitySpecification {
        VisibilitySpecification(self.0.extend_with(&other.0))
    }

    pub fn testing_parse(patterns: &[&str]) -> VisibilitySpecification {
        VisibilitySpecification(VisibilityPatternList::testing_parse(patterns))
    }
}

impl Display for VisibilitySpecification {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl WithinViewSpecification {
    pub const PUBLIC: WithinViewSpecification =
        WithinViewSpecification(VisibilityPatternList::Public);

    pub fn extend_with(&self, other: &WithinViewSpecification) -> WithinViewSpecification {
        WithinViewSpecification(self.0.extend_with(&other.0))
    }

    pub fn to_json(&self) -> serde_json::Value {
        self.0.to_json()
    }
}

impl Display for WithinViewSpecification {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl AnyMatches for VisibilitySpecification {
    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool> {
        self.0.any_matches(filter)
    }
}

impl AnyMatches for WithinViewSpecification {
    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool> {
        self.0.any_matches(filter)
    }
}

pub struct VisibilityWithinViewBuilder {
    cap: usize,
    seen_public: bool,
    patterns: Option<Vec<VisibilityPattern>>,
}

impl VisibilityWithinViewBuilder {
    pub fn with_capacity(cap: usize) -> VisibilityWithinViewBuilder {
        VisibilityWithinViewBuilder {
            cap,
            seen_public: false,
            patterns: None,
        }
    }

    pub fn add_public(&mut self) {
        self.seen_public = true;
    }

    pub fn add(&mut self, pattern: VisibilityPattern) {
        if !self.seen_public {
            self.patterns
                .get_or_insert_with(|| Vec::with_capacity(self.cap))
                .push(pattern);
        }
    }

    fn build_list(self) -> VisibilityPatternList {
        if self.seen_public {
            VisibilityPatternList::Public
        } else {
            VisibilityPatternList::List(ThinArcSlice::from_iter(self.patterns.unwrap_or_default()))
        }
    }

    pub fn build_visibility(self) -> VisibilitySpecification {
        VisibilitySpecification(self.build_list())
    }

    pub fn build_within_view(self) -> WithinViewSpecification {
        let list = self.build_list();
        if list.is_empty() {
            WithinViewSpecification::PUBLIC
        } else {
            WithinViewSpecification(list)
        }
    }
}
