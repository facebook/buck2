/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

use allocative::Allocative;
use buck2_core::pattern::pattern::ParsedPattern;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_core::target::label::label::TargetLabel;
use buck2_error::internal_error;
use buck2_util::arc_str::ThinArcSlice;
use dupe::Dupe;
use gazebo::prelude::SliceExt;
use pagable::Pagable;

use crate::attrs::attr_type::any_matches::AnyMatches;

#[derive(Debug, buck2_error::Error)]
pub enum VisibilityError {
    #[error(
        "`{0}` is not visible to `{1}` (run `buck2 uquery --output-attribute visibility {0}` to check the visibility)"
    )]
    #[buck2(input, tag = Visibility)]
    NotVisibleTo(TargetLabel, TargetLabel),
}

#[derive(
    Debug,
    Eq,
    PartialEq,
    Hash,
    Clone,
    Allocative,
    derive_more::Display,
    Pagable
)]
pub struct VisibilityPattern(pub ParsedPattern<TargetPatternExtra>);

impl VisibilityPattern {
    pub const PUBLIC: &'static str = "PUBLIC";

    pub fn testing_new(pattern: &str) -> VisibilityPattern {
        VisibilityPattern(ParsedPattern::testing_parse(pattern))
    }
}

#[derive(derive_more::Display)]
#[display("\"{}\"", _0)]
struct VisibilityPatternQuoted<'a>(&'a VisibilityPattern);

#[derive(Debug, Eq, PartialEq, Hash, Clone, Dupe, Allocative, Pagable)]
pub enum VisibilityPatternList {
    Public,
    List(ThinArcSlice<VisibilityPattern>),
    /// All sub-lists must match.
    Intersection(ThinArcSlice<VisibilityPatternList>),
}

impl VisibilityPatternList {
    fn is_empty(&self) -> bool {
        match self {
            VisibilityPatternList::Public => false,
            VisibilityPatternList::List(patterns) => patterns.is_empty(),
            VisibilityPatternList::Intersection(sub_lists) => {
                sub_lists.iter().any(|s| s.is_empty())
            }
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
            VisibilityPatternList::Intersection(_) => {
                unreachable!("Intersection not supported in JSON serialization")
            }
        };
        serde_json::Value::Array(list)
    }

    fn extend_with(
        &self,
        other: &VisibilityPatternList,
    ) -> buck2_error::Result<VisibilityPatternList> {
        match (self, other) {
            (VisibilityPatternList::Intersection(_), _)
            | (_, VisibilityPatternList::Intersection(_)) => Err(internal_error!(
                "`extend_with` does not support Intersection"
            )),
            (VisibilityPatternList::Public, _) | (_, VisibilityPatternList::Public) => {
                Ok(VisibilityPatternList::Public)
            }
            (VisibilityPatternList::List(this), VisibilityPatternList::List(other)) => Ok(
                VisibilityPatternList::List(this.iter().chain(other).cloned().collect()),
            ),
        }
    }

    pub fn intersect_with(&self, other: &VisibilityPatternList) -> VisibilityPatternList {
        match (self, other) {
            (VisibilityPatternList::Public, x) | (x, VisibilityPatternList::Public) => x.dupe(),
            _ => Self::intersect_many(vec![self.dupe(), other.dupe()]),
        }
    }

    fn intersect_many(constraints: Vec<VisibilityPatternList>) -> VisibilityPatternList {
        let mut flat: Vec<VisibilityPatternList> = Vec::with_capacity(constraints.len());
        for c in constraints {
            match c {
                VisibilityPatternList::Public => {}
                VisibilityPatternList::List(_) => flat.push(c),
                VisibilityPatternList::Intersection(sub_lists) => {
                    flat.extend(sub_lists.iter().map(Dupe::dupe));
                }
            }
        }
        match flat.len() {
            0 => VisibilityPatternList::Public,
            1 => flat.pop().unwrap(),
            _ => VisibilityPatternList::Intersection(flat.into_iter().collect()),
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

    pub fn matches_target(&self, target: &TargetLabel) -> bool {
        match self {
            VisibilityPatternList::Public => true,
            VisibilityPatternList::List(patterns) => {
                for pattern in patterns {
                    if pattern.0.matches(target) {
                        return true;
                    }
                }
                false
            }
            VisibilityPatternList::Intersection(sub_lists) => {
                sub_lists.iter().all(|s| s.matches_target(target))
            }
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
            VisibilityPatternList::Intersection(sub_lists) => {
                let mut first = true;
                for s in sub_lists.iter() {
                    if !first {
                        write!(f, " AND ")?;
                    }
                    first = false;
                    Display::fmt(s, f)?;
                }
                Ok(())
            }
        }
    }
}

impl AnyMatches for VisibilityPatternList {
    fn any_matches(
        &self,
        filter: &dyn Fn(&str) -> buck2_error::Result<bool>,
    ) -> buck2_error::Result<bool> {
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
            VisibilityPatternList::Intersection(_) => Err(internal_error!(
                "Intersection not supported in `any_matches`"
            )),
        }
    }
}

/// Represents the visibility spec of a target. Note that targets in the same package will ignore the
/// visibility spec of each other.
#[derive(Debug, Eq, PartialEq, Hash, Clone, Dupe, Allocative, Pagable)]
pub struct VisibilitySpecification(pub VisibilityPatternList);

impl Default for VisibilitySpecification {
    fn default() -> Self {
        Self::DEFAULT
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, Dupe, Allocative, Pagable)]
pub struct WithinViewSpecification(pub VisibilityPatternList);

impl Default for WithinViewSpecification {
    fn default() -> Self {
        Self::PUBLIC
    }
}

impl VisibilitySpecification {
    pub const DEFAULT: VisibilitySpecification =
        VisibilitySpecification(VisibilityPatternList::List(ThinArcSlice::empty()));

    pub(crate) fn to_json(&self) -> serde_json::Value {
        self.0.to_json()
    }

    pub fn extend_with(
        &self,
        other: &VisibilitySpecification,
    ) -> buck2_error::Result<VisibilitySpecification> {
        Ok(VisibilitySpecification(self.0.extend_with(&other.0)?))
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

    pub fn extend_with(
        &self,
        other: &WithinViewSpecification,
    ) -> buck2_error::Result<WithinViewSpecification> {
        Ok(WithinViewSpecification(self.0.extend_with(&other.0)?))
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
    fn any_matches(
        &self,
        filter: &dyn Fn(&str) -> buck2_error::Result<bool>,
    ) -> buck2_error::Result<bool> {
        self.0.any_matches(filter)
    }
}

impl AnyMatches for WithinViewSpecification {
    fn any_matches(
        &self,
        filter: &dyn Fn(&str) -> buck2_error::Result<bool>,
    ) -> buck2_error::Result<bool> {
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

#[cfg(test)]
mod tests {
    use buck2_core::pattern::pattern::ParsedPattern;
    use buck2_core::target::label::label::TargetLabel;

    use super::*;

    fn label(s: &str) -> TargetLabel {
        ParsedPattern::testing_parse(s)
            .as_target_label(s)
            .expect("test label parses")
    }

    #[test]
    fn intersect_with_public_is_identity() {
        let list = VisibilityPatternList::testing_parse(&["root//foo:"]);
        assert_eq!(VisibilityPatternList::Public.intersect_with(&list), list);
        assert_eq!(list.intersect_with(&VisibilityPatternList::Public), list);
    }

    #[test]
    fn intersect_with_lists_requires_both_to_match() {
        let a = VisibilityPatternList::testing_parse(&["root//foo:"]);
        let b = VisibilityPatternList::testing_parse(&["root//bar:"]);
        let a_and_b = a.intersect_with(&b);

        assert!(matches!(a_and_b, VisibilityPatternList::Intersection(_)));
        assert!(!a_and_b.matches_target(&label("root//foo:foo")));
        assert!(!a_and_b.matches_target(&label("root//bar:bar")));

        let overlap = VisibilityPatternList::testing_parse(&["root//foo:", "root//bar:"]);
        assert!(
            overlap
                .intersect_with(&a)
                .matches_target(&label("root//foo:foo"))
        );
        assert!(
            !overlap
                .intersect_with(&a)
                .matches_target(&label("root//bar:bar"))
        );
    }

    #[test]
    fn intersect_many_matches_all_constraints() {
        let a = VisibilityPatternList::testing_parse(&["root//baz:", "root//foo:"]);
        let b = VisibilityPatternList::testing_parse(&["root//baz:", "root//bar:"]);
        let c = VisibilityPatternList::testing_parse(&["root//baz:"]);
        let intersection = a.intersect_with(&b).intersect_with(&c);

        assert!(intersection.matches_target(&label("root//baz:baz")));
        assert!(!intersection.matches_target(&label("root//foo:foo")));
        assert!(!intersection.matches_target(&label("root//bar:bar")));
    }

    #[test]
    fn extend_with_intersection_returns_internal_error() {
        let a = VisibilityPatternList::testing_parse(&["root//foo:"]);
        let b = VisibilityPatternList::testing_parse(&["root//bar:"]);
        let intersection = a.intersect_with(&b);
        let public = VisibilityPatternList::Public;

        assert!(public.extend_with(&intersection).is_err());
        assert!(intersection.extend_with(&public).is_err());
    }
}
