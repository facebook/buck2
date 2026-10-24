/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Decoded views of attribute values.

use std::borrow::Cow;
use std::fmt::Display;

use buck2_core::configuration::compatibility::ResultMaybeCompatible;
use buck2_core::configuration::config_setting::ConfigSettingData;
use buck2_core::package::PackageLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::soft_error;
use buck2_error::buck2_error;
use buck2_error::internal_error;
use buck2_util::strong_hasher::StrongHash128;
use itertools::Itertools;
use ref_cast::RefCast;
use serde::Serialize;
use serde::Serializer;
use smallvec::SmallVec;

use crate::attrs::arena::CoercedAttrRefKind;
use crate::attrs::arena::arg::ArgPartView;
use crate::attrs::arena::kinds;
use crate::attrs::arena::kinds::decode;
use crate::attrs::arena::kinds::dispatch;
use crate::attrs::arena::kinds::dispatch_value;
use crate::attrs::arena::schema::EqParts;
use crate::attrs::arena::schema::Kind;
use crate::attrs::arena::semantics::ConcatClass;
use crate::attrs::arena::semantics::ConcatLeaves;
use crate::attrs::arena::semantics::Configure;
use crate::attrs::arena::semantics::Fmt;
use crate::attrs::arena::semantics::JsonKey;
use crate::attrs::arena::semantics::MatchesAny;
use crate::attrs::arena::semantics::SerializeKind;
use crate::attrs::arena::semantics::ToJson;
use crate::attrs::arena::semantics::Traverse;
use crate::attrs::arena::semantics::TraverseConfigured;
use crate::attrs::arena::storage::AttrToken;
use crate::attrs::arena::storage::AttrWord;
use crate::attrs::arena::storage::CoercedAttrArena;
use crate::attrs::arena::storage::RefRun;
use crate::attrs::attr_type::AttrType;
use crate::attrs::configuration_context::AttrConfigurationContext;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::configured_traversal::ConfiguredAttrTraversal;
use crate::attrs::display::AttrDisplayWithContext;
use crate::attrs::display::AttrDisplayWithContextExt;
use crate::attrs::fmt_context::AttrFmtContext;
use crate::attrs::serialize::AttrSerializeWithContext;
use crate::attrs::special_attr::SpecialAttr;
use crate::attrs::traversal::CoercedAttrTraversal;
use crate::configuration::resolved::ConfigurationSettingKey;
use crate::metadata::map::MetadataMap;
use crate::modifiers::CfgModifiers;
use crate::visibility::VisibilitySpecification;
use crate::visibility::WithinViewSpecification;

/// A decoded (shallow) view of one attribute value. Children decode lazily via
/// further `view` calls.
#[derive(Copy, Clone)]
pub struct CoercedAttrRef<'a> {
    pub(super) arena: &'a CoercedAttrArena,
    pub(super) r: AttrWord,
}

/// Content equality: the same kind with equal parts, across arenas.
impl PartialEq for CoercedAttrRef<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.content_eq(other)
    }
}

/// A run of child refs viewed as items of `T`: a sequence's values, a
/// dict's (key, value) pairs, or an arg's parts. `len`/`iter` only; the
/// items decode lazily.
pub struct Refs<'a, T> {
    pub(super) run: RefRun<'a>,
    pub(super) _item: std::marker::PhantomData<T>,
}

impl<T> Copy for Refs<'_, T> {}
impl<T> Clone for Refs<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}

/// How a [`Refs`] run decodes: `STRIDE` refs per item.
pub trait RefItem: Sized {
    const STRIDE: u32;
    type Item<'a>;
    /// Input form of the items for encoding, borrowed.
    type Spec: ?Sized;
    fn item<'a>(refs: Refs<'a, Self>, i: u32) -> Self::Item<'a>;
    fn tokens(spec: &Self::Spec) -> impl Iterator<Item = AttrToken> + '_;
}

/// One child value per ref.
pub struct Values;

impl RefItem for Values {
    const STRIDE: u32 = 1;
    type Item<'a> = CoercedAttrRef<'a>;
    type Spec = [AttrToken];
    fn item<'a>(refs: Refs<'a, Self>, i: u32) -> CoercedAttrRef<'a> {
        refs.run.arena.view(refs.run.get(i))
    }
    fn tokens(spec: &[AttrToken]) -> impl Iterator<Item = AttrToken> + '_ {
        spec.iter().copied()
    }
}

/// A (key, value) pair per two refs.
pub struct Pairs;

impl RefItem for Pairs {
    const STRIDE: u32 = 2;
    type Item<'a> = (CoercedAttrRef<'a>, CoercedAttrRef<'a>);
    type Spec = [(AttrToken, AttrToken)];
    fn item<'a>(refs: Refs<'a, Self>, i: u32) -> Self::Item<'a> {
        let run = refs.run;
        (
            run.arena.view(run.get(2 * i)),
            run.arena.view(run.get(2 * i + 1)),
        )
    }
    fn tokens(spec: &[(AttrToken, AttrToken)]) -> impl Iterator<Item = AttrToken> + '_ {
        spec.iter().flat_map(|(k, v)| [*k, *v])
    }
}

/// One arg part (`ArgSimple` or `ArgMacro` record) per ref.
pub struct ArgParts;

impl RefItem for ArgParts {
    const STRIDE: u32 = 1;
    type Item<'a> = ArgPartView<'a>;
    type Spec = [AttrToken];
    fn item<'a>(refs: Refs<'a, Self>, i: u32) -> ArgPartView<'a> {
        refs.run.arena.arg_part_at(refs.run.get(i))
    }
    fn tokens(spec: &[AttrToken]) -> impl Iterator<Item = AttrToken> + '_ {
        spec.iter().copied()
    }
}

impl<'a, T: RefItem> Refs<'a, T> {
    pub fn len(&self) -> usize {
        (self.run.len() / T::STRIDE) as usize
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = T::Item<'a>> + use<'a, T> {
        let refs = *self;
        (0..self.len() as u32).map(move |i| T::item(refs, i))
    }

    /// Every child ref in storage order, whatever the item shape.
    pub(super) fn children(
        &self,
    ) -> impl ExactSizeIterator<Item = CoercedAttrRef<'a>> + use<'a, T> {
        let arena = self.run.arena;
        self.run.iter().map(move |r| arena.view(r))
    }
}

/// View of a selector value's (configuration setting key, value) entries.
#[derive(Copy, Clone)]
pub struct AttrRefSelectorEntries<'a> {
    /// Providers-label table indices, from the shared `SelectorKeys` record.
    pub(super) keys: RefRun<'a>,
    /// Value refs, one per key, stored in the selector record.
    pub(super) values: RefRun<'a>,
}

impl<'a> AttrRefSelectorEntries<'a> {
    pub fn len(&self) -> usize {
        self.values.len() as usize
    }

    pub fn iter(
        &self,
    ) -> impl ExactSizeIterator<Item = (&'a ProvidersLabel, CoercedAttrRef<'a>)> + 'a {
        let arena = self.values.arena;
        self.keys
            .iter_raw()
            .zip(self.values.iter())
            .map(move |(k, v)| (&arena.providers_labels[k as usize], arena.view(v)))
    }
}

impl<'a> CoercedAttrRef<'a> {
    /// The ref itself: equal words in one arena are the same value.
    pub(crate) fn word(&self) -> AttrWord {
        self.r
    }

    pub fn kind(&self) -> CoercedAttrRefKind<'a> {
        decode(self.arena, self.r)
    }
}

impl<'a> CoercedAttrRef<'a> {
    /// The decoded parts if this value is of kind `K`: the typed downcast,
    /// for readers that want one kind rather than the sum `kind()` gives.
    pub(crate) fn downcast<K: Kind>(&self) -> Option<K::View<'a>> {
        (self.arena.kind_of(self.r) == K::KIND).then(|| K::decode(self.arena, self.r))
    }

    pub fn unpack_visibility(&self) -> Option<&'a VisibilitySpecification> {
        self.downcast::<kinds::Visibility>()
    }

    pub fn unpack_within_view(&self) -> Option<&'a WithinViewSpecification> {
        self.downcast::<kinds::WithinView>()
    }

    pub fn unpack_metadata(&self) -> Option<&'a MetadataMap> {
        self.downcast::<kinds::Metadata>()
    }

    pub fn unpack_target_modifiers(&self) -> Option<&'a CfgModifiers> {
        self.downcast::<kinds::Modifiers>()
    }

    /// Whether evaluating this value can ever yield `None`, directly or
    /// through a `select()` branch.
    pub fn may_return_none(&self) -> bool {
        self.downcast::<kinds::None>().is_some()
            || self
                .downcast::<kinds::Selector>()
                .is_some_and(|(entries, default)| {
                    entries.iter().any(|(_, v)| v.may_return_none())
                        || default.is_some_and(|d| d.may_return_none())
                })
    }

    /// The stored identity-grade content hash, if this ref was interned as
    /// a root. Equal content yields equal hashes across arenas and processes.
    pub fn root_strong_hash(&self) -> Option<StrongHash128> {
        self.arena.root_strong_hash(self.r)
    }

    /// Structural equality across (possibly different) arenas. Refs are
    /// arena-local, so cutoff comparisons between old and new values must
    /// compare content.
    pub fn content_eq(&self, other: &CoercedAttrRef<'_>) -> bool {
        let kind = self.arena.kind_of(self.r);
        kind == other.arena.kind_of(other.r)
            && dispatch(
                kind,
                EqParts {
                    a: *self,
                    b: *other,
                },
            )
    }

    /// Type-threaded traversal over the value's deps. Callback borrows come
    /// from the arena's side tables, so they carry the arena's lifetime.
    pub fn traverse(
        &self,
        t: &AttrType,
        pkg: Option<PackageLabel>,
        traversal: &mut dyn CoercedAttrTraversal<'a>,
    ) -> buck2_error::Result<()> {
        let kind = self.arena.kind_of(self.r);
        if kind.is_scalar() {
            return Ok(());
        }
        dispatch_value(
            kind,
            Traverse {
                v: *self,
                t,
                pkg,
                tr: traversal,
            },
        )
    }
}

impl<'a> CoercedAttrRef<'a> {
    /// Builds the `ConfiguredAttr` straight off the arena: selects resolve on
    /// the view, and only the configured form is materialized.
    pub fn configure(
        &self,
        t: &AttrType,
        ctx: &dyn AttrConfigurationContext,
        attr_name: Option<&str>,
    ) -> ResultMaybeCompatible<ConfiguredAttr> {
        dispatch_value(
            self.arena.kind_of(self.r),
            Configure {
                v: *self,
                t,
                ctx,
                name: attr_name,
            },
        )
    }

    /// The error for a select where no condition matched and no default
    /// was set.
    pub(crate) fn select_no_match_error(
        ctx: &dyn AttrConfigurationContext,
        entries: AttrRefSelectorEntries<'a>,
    ) -> buck2_error::Error {
        buck2_error!(
            buck2_error::ErrorTag::Input,
            "None of {} conditions matched configuration `{}` and no default was set:\n{}",
            entries.len(),
            ctx.cfg().cfg(),
            entries.iter().map(|(k, _)| format!("  {k}")).join("\n"),
        )
    }

    /// Resolve a select against the configuration: most specific matching
    /// entry, or `Ok(None)` for "use the default / no match".
    pub(crate) fn select_view(
        ctx: &dyn AttrConfigurationContext,
        select: CoercedAttrRef<'a>,
        entries: AttrRefSelectorEntries<'a>,
        default: Option<CoercedAttrRef<'a>>,
        attr_name: Option<&str>,
    ) -> buck2_error::Result<Option<CoercedAttrRef<'a>>> {
        let matched_cfg_keys = ctx.matched_cfg_keys();
        let resolved: SmallVec<[(&ProvidersLabel, &ConfigSettingData, CoercedAttrRef<'a>); 17]> =
            entries
                .iter()
                .filter_map(|(k, v)| {
                    matched_cfg_keys
                        .setting_matches(ConfigurationSettingKey::ref_cast(k))
                        .map(|d| (k, d, v))
                })
                .collect();

        let mut iter = resolved.iter();
        let Some(first) = iter.next() else {
            return Ok(default);
        };
        let mut matching = first;
        let mut slow = None;
        for entry in iter {
            let (_, conf, _) = entry;
            let (_, prev_conf, _) = matching;
            if conf.refines(prev_conf) {
                matching = entry;
            } else if prev_conf.refines(conf) {
                // keep `matching`
            } else {
                slow = Some(Self::select_view_slow(&resolved)?);
                break;
            }
        }
        let selected = slow.unwrap_or(matching.2);
        if !selected.content_eq(&first.2) {
            Self::first_match_differs(ctx, select, &resolved, selected, attr_name);
        }
        Ok(Some(selected))
    }

    /// Records a select whose first matching entry has a different value
    /// than its most specific one: data for migrating selects to
    /// first-match.
    fn first_match_differs(
        ctx: &dyn AttrConfigurationContext,
        select: CoercedAttrRef<'a>,
        resolved: &[(&ProvidersLabel, &ConfigSettingData, CoercedAttrRef<'a>)],
        selected: CoercedAttrRef<'a>,
        attr_name: Option<&str>,
    ) {
        let first_key = resolved[0].0;
        let most_specific_key = resolved
            .iter()
            .find(|(_, _, v)| v.content_eq(&selected))
            .map_or("<unknown>".to_owned(), |(k, _, _)| k.to_string());
        let all_matched_keys = resolved
            .iter()
            .map(|(k, _, _)| format!("  `{k}`"))
            .join("\n");
        let target_info = ctx
            .target_label()
            .map_or("<unknown>".to_owned(), |l| l.to_string());
        let _unused = soft_error!(
            "select_first_match_differs",
            buck2_error!(
                buck2_error::ErrorTag::Input,
                "Target `{}` (configuration `{}`), attribute `{}`:\n\
                First matching select key `{}` has different value than most specific match `{}`.\n\
                All matched keys:\n{}\n\
                Select expression: {}",
                target_info,
                ctx.cfg().cfg(),
                attr_name.unwrap_or("<unknown>"),
                first_key,
                most_specific_key,
                all_matched_keys,
                select.as_display_no_ctx()
            ),
            quiet: true,
            error_on_oss: true,
        );
    }

    pub(super) fn select_view_slow(
        resolved: &[(&ProvidersLabel, &ConfigSettingData, CoercedAttrRef<'a>)],
    ) -> buck2_error::Result<CoercedAttrRef<'a>> {
        let mut entries: Vec<(&ProvidersLabel, &ConfigSettingData, CoercedAttrRef<'a>)> =
            Vec::new();
        for (k, d, v) in resolved {
            if entries.iter().any(|(_, prev_d, _)| prev_d.refines(d)) {
                continue;
            }
            entries.retain(|(_, prev_d, _)| !d.refines(prev_d));
            entries.push((k, d, *v));
        }
        match entries.as_slice() {
            [] => Err(internal_error!(
                "no entries after slow select the most specific"
            )),
            [(.., x)] => Ok(*x),
            multiple_entries => {
                let (first_key, _, first_value) = &multiple_entries[0];
                let different_value_entry = multiple_entries
                    .iter()
                    .skip(1)
                    .find(|(_, _, v)| !v.content_eq(first_value));
                if let Some((different_key, _, _)) = different_value_entry {
                    Err(buck2_error!(
                        buck2_error::ErrorTag::Input,
                        "Both select keys `{first_key}` and `{different_key}` match the configuration, but neither is more specific and they have different values"
                    ))
                } else {
                    Ok(*first_value)
                }
            }
        }
    }

    /// Fused [`Self::configure`] + `ConfiguredAttr::traverse`: visit the deps
    /// the configured form of this value would contain, without
    /// materializing `ConfiguredAttr` trees. Containers, selects, and concat
    /// recurse natively (scalars cost nothing); dep-like leaves configure
    /// just the leaf value and traverse it, reusing the per-dep-kind
    /// dispatch of the owned path.
    ///
    /// Concat items are validated as `ConfiguredAttr::concat` would (one
    /// concat-able kind, equal oneof variants, no duplicate dict keys):
    /// configure-time errors must surface here, before the node exists,
    /// because later attr reads assume configuration succeeds.
    pub fn traverse_configured(
        &self,
        t: &AttrType,
        pkg: PackageLabel,
        ctx: &dyn AttrConfigurationContext,
        traversal: &mut dyn ConfiguredAttrTraversal,
        attr_name: Option<&str>,
    ) -> ResultMaybeCompatible<()> {
        dispatch_value(
            self.arena.kind_of(self.r),
            TraverseConfigured {
                v: *self,
                t,
                pkg,
                ctx,
                tr: traversal,
                name: attr_name,
            },
        )
    }

    /// The leaves `ConfiguredAttr::concat` would see for this value: selects
    /// resolved, nested concats inlined.
    pub(crate) fn concat_leaves(
        &self,
        ctx: &dyn AttrConfigurationContext,
        out: &mut SmallVec<[CoercedAttrRef<'a>; 4]>,
    ) -> ResultMaybeCompatible<()> {
        dispatch_value(
            self.arena.kind_of(self.r),
            ConcatLeaves { v: *self, ctx, out },
        )
    }

    /// The class of values this one concatenates with, if any.
    pub(crate) fn concat_class(&self) -> Option<&'static str> {
        dispatch_value(self.arena.kind_of(self.r), ConcatClass)
    }
}

impl<'a> CoercedAttrRef<'a> {
    /// The value's JSON rendering.
    pub fn to_json(
        &self,
        ctx: &crate::attrs::fmt_context::AttrFmtContext,
    ) -> buck2_error::Result<serde_json::Value> {
        dispatch_value(self.arena.kind_of(self.r), ToJson { v: *self, ctx })
    }

    /// Whether any leaf's string form matches the filter.
    pub fn any_matches(
        &self,
        filter: &dyn Fn(&str) -> buck2_error::Result<bool>,
    ) -> buck2_error::Result<bool> {
        dispatch_value(self.arena.kind_of(self.r), MatchesAny { v: *self, filter })
    }

    /// The configuration-dep label, or `None` for an explicit `None` value.
    ///
    /// Errors on any other kind: only for attributes the coercer has already
    /// verified to be a (non-configurable) configuration dep.
    pub fn unpack_configuration_dep(&self) -> buck2_error::Result<Option<&'a ProvidersLabel>> {
        if self.downcast::<kinds::None>().is_some() {
            return Ok(None);
        }
        self.downcast::<kinds::ConfigurationDep>()
            .ok_or_else(|| {
                internal_error!("should be a configuration dep: verified by the attr coercer")
            })
            .map(Some)
    }

    /// The list elements, or `None` when the value is not a list.
    pub fn unpack_list(&self) -> Option<impl Iterator<Item = CoercedAttrRef<'a>> + use<'a>> {
        self.downcast::<kinds::List>().map(|items| items.iter())
    }

    /// The string content, or `None` when the value is not a string.
    pub fn unpack_str(&self) -> Option<&'a str> {
        self.downcast::<kinds::String>()
    }
}

/// `as_display` for owned [`CoercedAttrRef`]s: the standard adapter borrows its
/// value, which cannot outlive a closure mapping `Copy` views.
pub(crate) struct AttrRefDisplay<'a, 'c> {
    pub(crate) ctx: &'c AttrFmtContext,
    pub(crate) r: CoercedAttrRef<'a>,
}

impl Display for AttrRefDisplay<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        AttrDisplayWithContext::fmt(&self.r, self.ctx, f)
    }
}

impl crate::attrs::display::AttrDisplayWithContext for CoercedAttrRef<'_> {
    fn fmt(
        &self,
        ctx: &crate::attrs::fmt_context::AttrFmtContext,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        dispatch_value(self.arena.kind_of(self.r), Fmt { v: *self, ctx, f })
    }
}

impl std::fmt::Debug for CoercedAttrRef<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CoercedAttrRef({})", self.as_display_no_ctx())
    }
}

/// An attribute value handed to a callback: a normal attr, which the package
/// arena stores, or a special attr, which is built per call and served as
/// built — nothing reads a special attr's identity, so nothing is retained.
#[derive(Debug)]
pub enum AttrView<'a> {
    Normal(CoercedAttrRef<'a>),
    Special(SpecialAttr),
}

impl AttrView<'_> {
    pub fn any_matches(
        &self,
        filter: &dyn Fn(&str) -> buck2_error::Result<bool>,
    ) -> buck2_error::Result<bool> {
        match self {
            AttrView::Normal(r) => r.any_matches(filter),
            AttrView::Special(a) => a.any_matches(filter),
        }
    }
}

impl crate::attrs::display::AttrDisplayWithContext for AttrView<'_> {
    fn fmt(&self, ctx: &AttrFmtContext, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AttrView::Normal(r) => AttrDisplayWithContext::fmt(r, ctx, f),
            AttrView::Special(a) => AttrDisplayWithContext::fmt(a, ctx, f),
        }
    }
}

impl AttrSerializeWithContext for AttrView<'_> {
    fn serialize_with_ctx<S>(&self, ctx: &AttrFmtContext, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            AttrView::Normal(r) => r.serialize_with_ctx(ctx, s),
            AttrView::Special(a) => a.to_json().map_err(serde::ser::Error::custom)?.serialize(s),
        }
    }
}

/// A value rendered in place of its parent, so nested values serialize without
/// an intermediate tree.
pub(super) struct AttrRefSerialize<'a> {
    pub(super) attr: CoercedAttrRef<'a>,
    pub(super) ctx: &'a AttrFmtContext,
}

impl Serialize for AttrRefSerialize<'_> {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.attr.serialize_with_ctx(self.ctx, s)
    }
}

impl<'a> CoercedAttrRef<'a> {
    /// The value's rendering when it is used as a JSON object key.
    ///
    /// Errors for values that render as anything but a string.
    pub(crate) fn json_key(&self, ctx: &AttrFmtContext) -> buck2_error::Result<Cow<'a, str>> {
        dispatch_value(self.arena.kind_of(self.r), JsonKey { v: *self, ctx })
    }
}

impl AttrSerializeWithContext for CoercedAttrRef<'_> {
    fn serialize_with_ctx<S>(&self, ctx: &AttrFmtContext, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        dispatch_value(
            self.arena.kind_of(self.r),
            SerializeKind {
                v: *self,
                ctx,
                ser: s,
            },
        )
    }
}
