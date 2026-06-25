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
use buck2_core::pattern::package::PackagePattern;
use buck2_core::pattern::pattern::ParsedPattern;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_core::target::label::label::TargetLabel;
use buck2_core::target::name_glob::TargetNameGlob;
use buck2_error::internal_error;
use buck2_util::arc_str::ThinArcSlice;
use dupe::Dupe;
use pagable::Pagable;
use serde::Serialize;
use starlark::any::ProvidesStaticType;
use starlark::starlark_simple_value;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkPagable;
use starlark::values::StarlarkValue;
use starlark::values::Value;
use starlark::values::starlark_value;
use starlark::values::string::StarlarkStr;

use crate::attrs::attr_type::any_matches::AnyMatches;

#[derive(Debug, buck2_error::Error)]
pub enum VisibilityError {
    #[error(
        "`{0}` is not visible to `{1}` (run `buck2 uquery --output-attribute visibility {0}` to check the visibility)"
    )]
    #[buck2(input, tag = Visibility)]
    NotVisibleTo(TargetLabel, TargetLabel),

    #[error(
        "`{0}` is not visible to `{1}` (visibility = {2}). Capped to {3} by `enforce_visibility_intersection()` in an ancestor PACKAGE."
    )]
    #[buck2(input, tag = Visibility)]
    NotVisibleToWithCap(
        TargetLabel,
        TargetLabel,
        VisibilitySpecification,
        VisibilityPatternList,
    ),
}

#[derive(Debug, buck2_error::Error)]
#[buck2(input)]
enum StarlarkTargetNameGlobError {
    #[error("`target_name_glob(...)` requires at least one entry in `name_globs`")]
    EmptyNameGlobs,
    #[error(
        "Invalid `within` in `target_name_glob`: expected a package pattern \
         (e.g. `//pkg:`) or recursive pattern (e.g. `//pkg/...`), got `{0}`"
    )]
    InvalidWithinScope(String),
    #[error(
        "`PUBLIC` is not a valid `within` scope in `target_name_glob` (it is a \
         target name, not a package pattern); omit `within` to match every package"
    )]
    PublicNotAllowedInWithin,
}

#[derive(
    Debug,
    Eq,
    PartialEq,
    Hash,
    Clone,
    Allocative,
    Pagable,
    derive_more::Display
)]
pub enum VisibilityPattern {
    Parsed(ParsedPattern<TargetPatternExtra>),
    TargetNameGlob(TargetNameGlobRecord),
}

/// Convert a parsed pattern into a `within` scope for a `target_name_glob`.
/// `Target` patterns (`//pkg:name`) can never match a package, so they are
/// rejected — the original [`ParsedPattern`] is handed back for error reporting —
/// rather than silently never matching.
fn within_scope_from_parsed(
    pattern: ParsedPattern<TargetPatternExtra>,
) -> Result<PackagePattern, ParsedPattern<TargetPatternExtra>> {
    match pattern {
        ParsedPattern::Package(p) => Ok(PackagePattern::Package(p)),
        ParsedPattern::Recursive(p) => Ok(PackagePattern::Recursive(p)),
        p @ ParsedPattern::Target(..) => Err(p),
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, Dupe, Allocative, Pagable)]
pub struct TargetNameGlobRecord {
    pub within: ThinArcSlice<PackagePattern>,
    pub name_globs: ThinArcSlice<TargetNameGlob>,
}

/// Renders a `target_name_glob(["..."], within = ["..."])` call that round-trips
/// back into the same Starlark expression. `name_globs` is rendered positionally
/// to match the constructor (where it is positional-only); `within` is
/// pre-rendered to canonical pattern strings by the caller.
fn fmt_target_name_glob<'a>(
    f: &mut Formatter<'_>,
    name_globs: impl Iterator<Item = &'a str>,
    within: impl ExactSizeIterator<Item = String>,
) -> fmt::Result {
    write!(f, "target_name_glob(")?;
    display_container::fmt_container(f, "[", "]", name_globs.map(StarlarkStr::repr))?;
    if within.len() != 0 {
        write!(f, ", within = ")?;
        display_container::fmt_container(f, "[", "]", within.map(|w| StarlarkStr::repr(&w)))?;
    }
    write!(f, ")")
}

impl Display for TargetNameGlobRecord {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        fmt_target_name_glob(
            f,
            self.name_globs.iter().map(|g| g.pattern()),
            self.within.iter().map(ToString::to_string),
        )
    }
}

impl Serialize for TargetNameGlobRecord {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // `to_json` is the single source of truth for the JSON shape; serializing
        // the built `Value` keeps the `to_json` call sites infallible. `Display`
        // and Starlark `repr` render `target_name_glob(...)` call syntax separately.
        self.to_json().serialize(serializer)
    }
}

/// The Starlark value produced by the `target_name_glob(...)` constructor — the
/// raw, pre-coercion form. The globs are validated, but `within` is still the
/// unparsed pattern strings the user passed. The visibility coercer and the
/// `PACKAGE` parser turn this into a [`TargetNameGlobRecord`] via
/// [`StarlarkTargetNameGlob::coerce`], resolving each `within` string against
/// the enclosing file's cell aliases. Keeping this distinct from the coerced
/// record keeps the Starlark layer from depending on the coerced-attr type.
///
/// `NoSerialize`: as a Starlark value a `target_name_glob(...)` has no JSON form,
/// so BXL `json.encode` on it errors. The `uquery --output-attribute ... --json`
/// shape instead comes from [`TargetNameGlobRecord`]'s `Serialize` impl.
#[derive(Debug, ProvidesStaticType, Allocative, StarlarkPagable, NoSerialize)]
pub struct StarlarkTargetNameGlob {
    #[starlark_pagable(pagable)]
    name_globs: ThinArcSlice<TargetNameGlob>,
    #[starlark_pagable(pagable)]
    within: ThinArcSlice<String>,
}

starlark_simple_value!(StarlarkTargetNameGlob);

impl StarlarkTargetNameGlob {
    /// Construct from the raw constructor arguments. Validates `name_globs`;
    /// `within` strings are kept verbatim and parsed later by
    /// [`StarlarkTargetNameGlob::coerce`]. Deferring `within` validation matches
    /// how target labels behave: an invalid scope is reported at each use site
    /// rather than at the (possibly distant) `target_name_glob()` call.
    pub fn try_new(name_globs: Vec<String>, within: Vec<String>) -> buck2_error::Result<Self> {
        if name_globs.is_empty() {
            return Err(StarlarkTargetNameGlobError::EmptyNameGlobs.into());
        }
        let name_globs = name_globs
            .into_iter()
            .map(TargetNameGlob::try_new)
            .collect::<Result<ThinArcSlice<TargetNameGlob>, _>>()?;
        Ok(Self {
            name_globs,
            within: ThinArcSlice::from_iter(within),
        })
    }

    /// Coerce into the [`TargetNameGlobRecord`], parsing each `within` string
    /// into a [`PackagePattern`] with the supplied cell-aware pattern parser.
    /// The visibility coercer passes `ctx.coerce_target_pattern`; the `PACKAGE`
    /// parser passes `ParsedPattern::parse_precise`.
    pub fn coerce(
        &self,
        parse_within: impl Fn(&str) -> buck2_error::Result<ParsedPattern<TargetPatternExtra>>,
    ) -> buck2_error::Result<TargetNameGlobRecord> {
        let within = self
            .within
            .iter()
            .map(|within_str| {
                // Reject `PUBLIC` before parsing so the error names it
                // specifically (it is a target name, not a package pattern).
                if within_str.as_str() == VisibilityPattern::PUBLIC {
                    return Err(StarlarkTargetNameGlobError::PublicNotAllowedInWithin.into());
                }
                within_scope_from_parsed(parse_within(within_str)?).map_err(|_| {
                    StarlarkTargetNameGlobError::InvalidWithinScope(within_str.clone()).into()
                })
            })
            .collect::<buck2_error::Result<ThinArcSlice<PackagePattern>>>()?;
        Ok(TargetNameGlobRecord {
            within,
            name_globs: self.name_globs.dupe(),
        })
    }
}

impl Display for StarlarkTargetNameGlob {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        fmt_target_name_glob(
            f,
            self.name_globs.iter().map(|g| g.pattern()),
            self.within.iter().cloned(),
        )
    }
}

#[starlark_value(type = "target_name_glob")]
impl<'v> StarlarkValue<'v> for StarlarkTargetNameGlob {}

impl VisibilityPattern {
    pub const PUBLIC: &'static str = "PUBLIC";

    pub fn testing_new(pattern: &str) -> VisibilityPattern {
        VisibilityPattern::Parsed(ParsedPattern::testing_parse(pattern))
    }

    pub fn testing_new_record(within: &[&str], name_globs: &[&str]) -> VisibilityPattern {
        let within_scopes: ThinArcSlice<PackagePattern> = within
            .iter()
            .map(|s| {
                within_scope_from_parsed(ParsedPattern::testing_parse(s))
                    .expect("testing within pattern must be Package or Recursive, not Target")
            })
            .collect();
        let globs: ThinArcSlice<TargetNameGlob> = name_globs
            .iter()
            .map(|g| TargetNameGlob::try_new((*g).to_owned()).unwrap())
            .collect();
        VisibilityPattern::TargetNameGlob(TargetNameGlobRecord {
            within: within_scopes,
            name_globs: globs,
        })
    }
}

impl TargetNameGlobRecord {
    /// Canonical JSON form for `uquery --output-attribute` / BXL: a
    /// `__type`-tagged object that round-trips back into `target_name_glob(...)`.
    /// The single source of truth for the JSON shape (`Serialize` delegates
    /// here); building the `Value` is infallible, so callers need no `Result`.
    pub(crate) fn to_json(&self) -> serde_json::Value {
        let mut obj = serde_json::Map::new();
        obj.insert(
            "__type".to_owned(),
            <StarlarkTargetNameGlob as StarlarkValue>::TYPE.into(),
        );
        obj.insert(
            "name_globs".to_owned(),
            self.name_globs.iter().map(|g| g.pattern()).collect(),
        );
        obj.insert(
            "within".to_owned(),
            self.within.iter().map(|p| p.to_string()).collect(),
        );
        obj.into()
    }

    pub fn matches(&self, target: &TargetLabel) -> buck2_error::Result<bool> {
        let within_matches =
            self.within.is_empty() || self.within.iter().any(|scope| scope.matches(target.pkg()));
        if !within_matches {
            return Ok(false);
        }
        for g in self.name_globs.iter() {
            if g.matches(target.name())? {
                return Ok(true);
            }
        }
        Ok(false)
    }

    pub fn alloc_starlark_value<'v>(&self, heap: Heap<'v>) -> Value<'v> {
        // `within` scopes render back to their canonical string form.
        heap.alloc(StarlarkTargetNameGlob {
            name_globs: self.name_globs.dupe(),
            within: self.within.iter().map(ToString::to_string).collect(),
        })
    }
}

struct VisibilityPatternQuoted<'a>(&'a VisibilityPattern);

impl Display for VisibilityPatternQuoted<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // `Parsed` patterns need surrounding quotes when they appear inside a
        // visibility list literal (`["//pkg:", ...]`). `TargetNameGlob` already
        // emits its own `target_name_glob(...)` syntax and must not be quoted.
        match self.0 {
            VisibilityPattern::Parsed(p) => write!(f, "\"{}\"", p),
            VisibilityPattern::TargetNameGlob(r) => Display::fmt(r, f),
        }
    }
}

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

    pub fn to_json(&self) -> serde_json::Value {
        let list = match self {
            VisibilityPatternList::Public => vec![serde_json::Value::String(
                VisibilityPattern::PUBLIC.to_owned(),
            )],
            VisibilityPatternList::List(patterns) => patterns
                .iter()
                .map(|p| match p {
                    VisibilityPattern::Parsed(p) => serde_json::Value::String(p.to_string()),
                    VisibilityPattern::TargetNameGlob(r) => r.to_json(),
                })
                .collect(),
            VisibilityPatternList::Intersection(sub_lists) => {
                let parts: Vec<serde_json::Value> = sub_lists.iter().map(|s| s.to_json()).collect();
                return serde_json::json!({ "intersection": parts });
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

    pub fn matches_target(&self, target: &TargetLabel) -> buck2_error::Result<bool> {
        match self {
            VisibilityPatternList::Public => Ok(true),
            VisibilityPatternList::List(patterns) => {
                for pattern in patterns.iter() {
                    let matches = match pattern {
                        VisibilityPattern::Parsed(p) => p.matches(target),
                        VisibilityPattern::TargetNameGlob(r) => r.matches(target)?,
                    };
                    if matches {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            VisibilityPatternList::Intersection(sub_lists) => {
                for s in sub_lists.iter() {
                    if !s.matches_target(target)? {
                        return Ok(false);
                    }
                }
                Ok(true)
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

    pub fn to_json(&self) -> serde_json::Value {
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
        assert!(!a_and_b.matches_target(&label("root//foo:foo")).unwrap());
        assert!(!a_and_b.matches_target(&label("root//bar:bar")).unwrap());

        let overlap = VisibilityPatternList::testing_parse(&["root//foo:", "root//bar:"]);
        assert!(
            overlap
                .intersect_with(&a)
                .matches_target(&label("root//foo:foo"))
                .unwrap()
        );
        assert!(
            !overlap
                .intersect_with(&a)
                .matches_target(&label("root//bar:bar"))
                .unwrap()
        );
    }

    #[test]
    fn intersect_many_matches_all_constraints() {
        let a = VisibilityPatternList::testing_parse(&["root//baz:", "root//foo:"]);
        let b = VisibilityPatternList::testing_parse(&["root//baz:", "root//bar:"]);
        let c = VisibilityPatternList::testing_parse(&["root//baz:"]);
        let intersection = a.intersect_with(&b).intersect_with(&c);

        assert!(
            intersection
                .matches_target(&label("root//baz:baz"))
                .unwrap()
        );
        assert!(
            !intersection
                .matches_target(&label("root//foo:foo"))
                .unwrap()
        );
        assert!(
            !intersection
                .matches_target(&label("root//bar:bar"))
                .unwrap()
        );
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

    #[test]
    fn visibility_record_matches_unscoped_target_names() {
        let list = VisibilityPatternList::List(
            [VisibilityPattern::testing_new_record(&[], &["*-test"])]
                .into_iter()
                .collect(),
        );

        assert!(list.matches_target(&label("root//foo:unit-test")).unwrap());
        assert!(list.matches_target(&label("root//bar:unit-test")).unwrap());
        // Negative: full-match semantics — `-testfoo` ends after `-test`, so
        // suffix `-test` must terminate the string, not appear in the middle.
        assert!(
            !list
                .matches_target(&label("root//foo:unit-testfoo"))
                .unwrap()
        );
        assert!(!list.matches_target(&label("root//foo:library")).unwrap());
    }

    #[test]
    fn visibility_record_matches_package_scope() {
        let list = VisibilityPatternList::List(
            [VisibilityPattern::testing_new_record(
                &["root//foo:"],
                &["*-test"],
            )]
            .into_iter()
            .collect(),
        );

        assert!(list.matches_target(&label("root//foo:unit-test")).unwrap());
        // Negative: glob would match if scope ignored, but package mismatch rules it out.
        assert!(
            !list
                .matches_target(&label("root//foo:unit-testfoo"))
                .unwrap()
        );
        assert!(
            !list
                .matches_target(&label("root//foo/sub:unit-test"))
                .unwrap()
        );
        assert!(!list.matches_target(&label("root//bar:unit-test")).unwrap());
    }

    #[test]
    fn visibility_record_matches_recursive_scope() {
        let list = VisibilityPatternList::List(
            [VisibilityPattern::testing_new_record(
                &["root//foo/..."],
                &["*-test"],
            )]
            .into_iter()
            .collect(),
        );

        assert!(list.matches_target(&label("root//foo:unit-test")).unwrap());
        assert!(
            list.matches_target(&label("root//foo/sub:unit-test"))
                .unwrap()
        );
        // Negative: full-match within the recursive scope.
        assert!(
            !list
                .matches_target(&label("root//foo:unit-testfoo"))
                .unwrap()
        );
        assert!(!list.matches_target(&label("root//bar:unit-test")).unwrap());
    }

    #[test]
    fn visibility_record_matches_any_of_multiple_globs() {
        // The list semantics is "any glob matches" — both `*-test` and
        // `*-deprecated` count as hits, but unrelated names do not.
        let list = VisibilityPatternList::List(
            [VisibilityPattern::testing_new_record(
                &[],
                &["*-test", "*-deprecated"],
            )]
            .into_iter()
            .collect(),
        );

        assert!(list.matches_target(&label("root//foo:unit-test")).unwrap());
        assert!(
            list.matches_target(&label("root//bar:legacy-deprecated"))
                .unwrap()
        );
        // Negative: neither glob matches.
        assert!(!list.matches_target(&label("root//foo:library")).unwrap());
        // Negative: full-match — the `-test` glob must end the string.
        assert!(
            !list
                .matches_target(&label("root//foo:unit-testfoo"))
                .unwrap()
        );
    }

    #[test]
    fn visibility_record_intersection_requires_all_constraints() {
        let glob = VisibilityPatternList::List(
            [VisibilityPattern::testing_new_record(&[], &["*-test"])]
                .into_iter()
                .collect(),
        );
        let package = VisibilityPatternList::testing_parse(&["root//foo:"]);
        let intersection = glob.intersect_with(&package);

        assert!(
            intersection
                .matches_target(&label("root//foo:unit-test"))
                .unwrap()
        );
        assert!(
            !intersection
                .matches_target(&label("root//foo:library"))
                .unwrap()
        );
        assert!(
            !intersection
                .matches_target(&label("root//bar:unit-test"))
                .unwrap()
        );
    }

    #[test]
    fn target_name_glob_record_serializes_empty_within() {
        let VisibilityPattern::TargetNameGlob(record) =
            VisibilityPattern::testing_new_record(&[], &["*-test"])
        else {
            panic!("testing_new_record builds a Record");
        };

        assert_eq!(
            record.to_json(),
            serde_json::json!({
                "__type": "target_name_glob",
                "name_globs": ["*-test"],
                "within": [],
            })
        );
    }

    #[test]
    fn starlark_target_name_glob_rejects_empty_list() {
        let err = StarlarkTargetNameGlob::try_new(vec![], vec![]).expect_err("empty list rejected");
        let msg = format!("{err:#}");
        assert!(
            msg.contains("at least one entry"),
            "expected empty-list error, got: {msg}"
        );
    }

    #[test]
    fn starlark_target_name_glob_repr_escapes_within() {
        // The raw `within` strings are unvalidated user input (coercion happens
        // later), so `repr`/`Display` must emit a properly escaped Starlark
        // string literal — not Rust's `Debug` form.
        let value =
            StarlarkTargetNameGlob::try_new(vec!["*-test".to_owned()], vec!["a\"b".to_owned()])
                .expect("valid globs");
        let repr = value.to_string();
        assert!(
            repr.contains(r#"["a\"b"]"#),
            "within must be escaped as a Starlark string literal, got: {repr}"
        );
    }

    #[test]
    fn package_pattern_recursive_cell_root_round_trips() {
        // Cell-root recursive `within` must render as `root//...` (the empty-path
        // branch of `PackagePattern`'s `Display`).
        let VisibilityPattern::TargetNameGlob(record) =
            VisibilityPattern::testing_new_record(&["root//..."], &["*-test"])
        else {
            panic!("testing_new_record builds a Record");
        };
        assert_eq!(
            record.to_json(),
            serde_json::json!({
                "__type": "target_name_glob",
                "name_globs": ["*-test"],
                "within": ["root//..."],
            })
        );
    }

    #[test]
    fn within_view_preserves_single_record() {
        // A `within_view` holding only a `target_name_glob` must NOT collapse to
        // PUBLIC — only a genuinely empty list does.
        let mut builder = VisibilityWithinViewBuilder::with_capacity(1);
        builder.add(VisibilityPattern::testing_new_record(&[], &["*-test"]));
        let within_view = builder.build_within_view();
        assert!(
            matches!(within_view.0, VisibilityPatternList::List(_)),
            "single-record within_view must stay a List, not become Public"
        );
        assert!(
            within_view
                .0
                .matches_target(&label("root//foo:unit-test"))
                .unwrap()
        );
    }

    #[test]
    fn extend_with_carries_records() {
        // Extending a list that contains a `Record` must carry the Record through.
        let globs = VisibilityPatternList::List(
            [VisibilityPattern::testing_new_record(&[], &["*-test"])]
                .into_iter()
                .collect(),
        );
        let other = VisibilityPatternList::testing_parse(&["root//foo:"]);
        let extended = globs.extend_with(&other).unwrap();

        // The Record (name glob) and the plain pattern both still match.
        assert!(
            extended
                .matches_target(&label("root//bar:unit-test"))
                .unwrap()
        );
        assert!(extended.matches_target(&label("root//foo:lib")).unwrap());
        // Negative: neither matches.
        assert!(!extended.matches_target(&label("root//bar:lib")).unwrap());
    }

    #[test]
    fn visibility_record_matches_any_of_multiple_within_scopes() {
        // `within` is OR-ed: a target matches if its package is in ANY listed
        // scope (and its name matches a glob).
        let list = VisibilityPatternList::List(
            [VisibilityPattern::testing_new_record(
                &["root//foo:", "root//bar/..."],
                &["*-test"],
            )]
            .into_iter()
            .collect(),
        );

        // First scope (exact package).
        assert!(list.matches_target(&label("root//foo:unit-test")).unwrap());
        // Second scope (recursive prefix).
        assert!(
            list.matches_target(&label("root//bar/sub:unit-test"))
                .unwrap()
        );
        // Negative: name matches a glob but package is in neither scope.
        assert!(!list.matches_target(&label("root//baz:unit-test")).unwrap());
        // Negative: package is in a scope but the name matches no glob.
        assert!(!list.matches_target(&label("root//foo:library")).unwrap());
    }

    #[test]
    fn starlark_target_name_glob_serializes_within_as_list() {
        let VisibilityPattern::TargetNameGlob(record) =
            VisibilityPattern::testing_new_record(&["root//foo:", "root//bar/..."], &["*-test"])
        else {
            panic!("testing_new_record builds a Record");
        };

        assert_eq!(
            record.to_json(),
            serde_json::json!({
                "__type": "target_name_glob",
                "name_globs": ["*-test"],
                "within": ["root//foo:", "root//bar/..."],
            })
        );
    }

    #[test]
    fn starlark_target_name_glob_coerce_resolves_within() {
        let value = StarlarkTargetNameGlob::try_new(
            vec!["*-test".to_owned()],
            vec!["root//foo:".to_owned(), "root//bar/...".to_owned()],
        )
        .expect("valid globs");

        let record = value
            .coerce(|s| Ok(ParsedPattern::testing_parse(s)))
            .expect("within scopes resolve");

        // OR over the two resolved scopes (plus the name glob).
        assert!(record.matches(&label("root//foo:unit-test")).unwrap());
        assert!(record.matches(&label("root//bar/sub:unit-test")).unwrap());
        // Negative: name matches but package is outside both scopes.
        assert!(!record.matches(&label("root//baz:unit-test")).unwrap());
        // Negative: package in scope but name matches no glob.
        assert!(!record.matches(&label("root//foo:library")).unwrap());
    }

    #[test]
    fn starlark_target_name_glob_coerce_rejects_target_within() {
        let value = StarlarkTargetNameGlob::try_new(
            vec!["*-test".to_owned()],
            // A target pattern (not package/recursive) is not a valid `within`.
            vec!["root//foo:bar".to_owned()],
        )
        .expect("valid globs");

        let err = value
            .coerce(|s| Ok(ParsedPattern::testing_parse(s)))
            .expect_err("target pattern in within must be rejected");
        assert!(
            format!("{err:#}").contains("expected a package pattern"),
            "expected InvalidWithinScope error, got: {err:#}"
        );
    }

    #[test]
    fn starlark_target_name_glob_coerce_rejects_public_within() {
        // `PUBLIC` is a valid target *name* (so it is fine in `name_globs`), but
        // it is not a valid `within` scope and must be rejected with a clear
        // message rather than a generic parse error.
        let value =
            StarlarkTargetNameGlob::try_new(vec!["*-test".to_owned()], vec!["PUBLIC".to_owned()])
                .expect("valid globs");

        let err = value
            .coerce(|s| Ok(ParsedPattern::testing_parse(s)))
            .expect_err("`PUBLIC` must be rejected as a within scope");
        assert!(
            format!("{err:#}").contains("`PUBLIC` is not a valid `within`"),
            "expected PublicNotAllowedInWithin error, got: {err:#}"
        );
    }

    #[test]
    fn starlark_target_name_glob_repr_renders_name_globs_positionally() {
        // `name_globs` is positional-only in the constructor, so the repr must
        // render it positionally (no `name_globs = `) to round-trip.
        let value = StarlarkTargetNameGlob::try_new(
            vec!["*-test".to_owned()],
            vec!["root//foo:".to_owned()],
        )
        .expect("valid globs");
        assert_eq!(
            value.to_string(),
            r#"target_name_glob(["*-test"], within = ["root//foo:"])"#,
        );
    }
}
