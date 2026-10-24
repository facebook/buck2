/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! What each kind's values mean: dependency traversal, configuration,
//! rendering, JSON and substring search.

use std::borrow::Cow;
use std::collections::BTreeMap;

use buck2_core::configuration::compatibility::ResultMaybeCompatible;
use buck2_core::package::PackageLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_error::internal_error;
use display_container::fmt_container;
use serde::Serializer;
use serde::ser::SerializeMap;
use serde::ser::SerializeSeq;
use serde_json::to_value;
use smallvec::SmallVec;

use crate::attrs::arena::schema::Kind;
use crate::attrs::arena::view::AttrRefDisplay;
use crate::attrs::arena::view::AttrRefSerialize;
use crate::attrs::arena::view::CoercedAttrRef;
use crate::attrs::arena::view::Refs;
use crate::attrs::arena::view::Values;
use crate::attrs::attr_type::AttrType;
use crate::attrs::attr_type::AttrTypeInner;
use crate::attrs::configuration_context::AttrConfigurationContext;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::configured_traversal::ConfiguredAttrTraversal;
use crate::attrs::display::AttrDisplayWithContextExt;
use crate::attrs::fmt_context::AttrFmtContext;
use crate::attrs::traversal::CoercedAttrTraversal;

/// What a kind's values mean: to dependency traversal, configuration,
/// rendering, JSON and substring search. One impl per kind, in the file of
/// the attribute type it belongs to; a kind without one does not compile.
///
/// Each operation receives the value's decoded parts and the operation's
/// operands (the value itself among them) as one struct, so an impl binds
/// only what it uses.
pub(crate) trait Semantics: Kind {
    /// The class of values `+` concatenates: leaves of one class
    /// concatenate; `None` means addition is not supported.
    const CONCAT: Option<&'static str> = None;

    fn view<'a>(v: CoercedAttrRef<'a>) -> Self::View<'a> {
        Self::decode(v.arena, v.r)
    }

    /// Type-threaded traversal over the value's deps.
    fn traverse<'a>(parts: Self::View<'a>, op: Traverse<'_, 'a>) -> buck2_error::Result<()>;

    /// The configured form of the value.
    fn configure(
        parts: Self::View<'_>,
        op: Configure<'_, '_>,
    ) -> ResultMaybeCompatible<ConfiguredAttr>;

    /// The deps of the configured form, without materializing it. Leaves
    /// configure the small configured value and traverse that; containers,
    /// selects and concats recurse.
    fn traverse_configured(
        parts: Self::View<'_>,
        TraverseConfigured {
            v,
            t,
            pkg,
            ctx,
            tr,
            name,
        }: TraverseConfigured<'_, '_>,
    ) -> ResultMaybeCompatible<()> {
        // `Any`-typed values cannot contain deps.
        if matches!(inner_type(t).0.inner, AttrTypeInner::Any(_)) {
            return ResultMaybeCompatible::Compatible(());
        }
        let configured = Self::configure(parts, Configure { v, t, ctx, name })?;
        if let Err(e) = configured.traverse(pkg, tr) {
            return e.into();
        }
        ResultMaybeCompatible::Compatible(())
    }

    /// The leaves `ConfiguredAttr::concat` would see: selects resolved,
    /// nested concats inlined. A plain value is its own leaf.
    fn concat_leaves<'a>(
        _parts: Self::View<'a>,
        ConcatLeaves { v, out, .. }: ConcatLeaves<'_, 'a>,
    ) -> ResultMaybeCompatible<()> {
        out.push(v);
        ResultMaybeCompatible::Compatible(())
    }

    fn to_json(parts: Self::View<'_>, op: ToJson<'_, '_>)
    -> buck2_error::Result<serde_json::Value>;

    fn fmt(parts: Self::View<'_>, op: Fmt<'_, '_, '_>) -> std::fmt::Result;

    /// Whether any leaf's string form matches the filter.
    fn any_matches(parts: Self::View<'_>, op: MatchesAny<'_, '_>) -> buck2_error::Result<bool>;

    /// The value's rendering as a JSON object key; an error for values that
    /// render as anything but a string.
    fn json_key<'a>(
        _parts: Self::View<'a>,
        _op: JsonKey<'_, 'a>,
    ) -> buck2_error::Result<Cow<'a, str>> {
        Err(internal_error!("dict keys must render to JSON strings"))
    }

    /// Serialize the value's JSON form without building a `serde_json::Value`.
    fn serialize<S: Serializer>(
        parts: Self::View<'_>,
        op: SerializeKind<'_, '_, S>,
    ) -> Result<S::Ok, S::Error>;
}

pub(crate) fn ser_err<S: Serializer>(e: buck2_error::Error) -> S::Error {
    serde::ser::Error::custom(format!("{e}"))
}

/// Keyed values collect into a `BTreeMap` to match `serde_json::Map`: keys
/// emit in sorted order, and a repeated key keeps its last value.
pub(crate) fn ser_keyed<'a, S: Serializer>(
    entries: impl Iterator<Item = (Cow<'a, str>, CoercedAttrRef<'a>)>,
    ctx: &'a AttrFmtContext,
    s: S,
) -> Result<S::Ok, S::Error> {
    let sorted: BTreeMap<Cow<'a, str>, CoercedAttrRef<'a>> = entries.collect();
    let mut map = s.serialize_map(Some(sorted.len()))?;
    for (k, v) in sorted {
        map.serialize_entry(&k, &AttrRefSerialize { attr: v, ctx })?;
    }
    map.end()
}

pub(crate) fn ser_seq<'a, S: Serializer>(
    items: Refs<'a, Values>,
    ctx: &'a AttrFmtContext,
    s: S,
) -> Result<S::Ok, S::Error> {
    let mut seq = s.serialize_seq(Some(items.len()))?;
    for v in items.iter() {
        seq.serialize_element(&AttrRefSerialize { attr: v, ctx })?;
    }
    seq.end()
}

/// The attribute type with `Option` layers removed: values do not record
/// optionality.
pub(crate) fn inner_type(t: &AttrType) -> &AttrType {
    let mut t = t;
    while let AttrTypeInner::Option(opt) = &t.0.inner {
        t = &opt.inner;
    }
    t
}

/// A value whose kind does not fit the attribute type it is read with:
/// the coercer established the pairing, so this is a logic error.
pub(crate) fn mismatch(v: CoercedAttrRef<'_>, t: &AttrType) -> buck2_error::Error {
    internal_error!(
        "attr arena: value `{}` does not fit attribute type `{}`",
        v.as_display_no_ctx(),
        t
    )
}

/// Selects the [`Semantics`] type for a value's tag at runtime,
/// monomorphizing the visitor per kind. The record-internal kinds are not
/// attribute values and never reach a value operation.
pub(super) trait ValueVisitor {
    type Out;
    fn visit<K: Semantics + 'static>(self) -> Self::Out;
}

/// `String` and `EnumVariant` render alike.
pub(crate) fn fmt_text(
    s: &str,
    ctx: &AttrFmtContext,
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    if ctx.options.exclude_quotes {
        f.write_str(s)
    } else {
        write!(f, "\"{s}\"")
    }
}

pub(crate) fn fmt_seq(
    items: Refs<'_, Values>,
    open: &str,
    close: &str,
    ctx: &AttrFmtContext,
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    fmt_container(
        f,
        open,
        close,
        items.iter().map(|v| AttrRefDisplay { ctx, r: v }),
    )
}

pub(crate) fn json_seq(
    items: Refs<'_, Values>,
    ctx: &AttrFmtContext,
) -> buck2_error::Result<serde_json::Value> {
    Ok(serde_json::Value::Array(
        items
            .iter()
            .map(|v| v.to_json(ctx))
            .collect::<buck2_error::Result<_>>()?,
    ))
}

pub(crate) fn any_matches_seq(
    items: Refs<'_, Values>,
    filter: &dyn Fn(&str) -> buck2_error::Result<bool>,
) -> buck2_error::Result<bool> {
    for v in items.iter() {
        if v.any_matches(filter)? {
            return Ok(true);
        }
    }
    Ok(false)
}

pub(crate) fn json_label(l: &ProvidersLabel) -> buck2_error::Result<serde_json::Value> {
    Ok(to_value(l.to_string())?)
}

/// The operands of one value operation, as [`dispatch_value`] hands them to
/// the kind's [`Semantics`]; `v` is the value itself.
pub(crate) struct Traverse<'x, 'a> {
    pub(crate) v: CoercedAttrRef<'a>,
    pub(crate) t: &'x AttrType,
    pub(crate) pkg: Option<PackageLabel>,
    pub(crate) tr: &'x mut dyn CoercedAttrTraversal<'a>,
}

impl ValueVisitor for Traverse<'_, '_> {
    type Out = buck2_error::Result<()>;
    fn visit<K: Semantics + 'static>(self) -> Self::Out {
        K::traverse(K::view(self.v), self)
    }
}

pub(crate) struct Configure<'x, 'a> {
    pub(crate) v: CoercedAttrRef<'a>,
    pub(crate) t: &'x AttrType,
    pub(crate) ctx: &'x dyn AttrConfigurationContext,
    pub(crate) name: Option<&'x str>,
}

impl ValueVisitor for Configure<'_, '_> {
    type Out = ResultMaybeCompatible<ConfiguredAttr>;
    fn visit<K: Semantics + 'static>(self) -> Self::Out {
        K::configure(K::view(self.v), self)
    }
}

pub(crate) struct TraverseConfigured<'x, 'a> {
    pub(crate) v: CoercedAttrRef<'a>,
    pub(crate) t: &'x AttrType,
    pub(crate) pkg: PackageLabel,
    pub(crate) ctx: &'x dyn AttrConfigurationContext,
    pub(crate) tr: &'x mut dyn ConfiguredAttrTraversal,
    pub(crate) name: Option<&'x str>,
}

impl ValueVisitor for TraverseConfigured<'_, '_> {
    type Out = ResultMaybeCompatible<()>;
    fn visit<K: Semantics + 'static>(self) -> Self::Out {
        K::traverse_configured(K::view(self.v), self)
    }
}

pub(crate) struct ConcatLeaves<'x, 'a> {
    pub(crate) v: CoercedAttrRef<'a>,
    pub(crate) ctx: &'x dyn AttrConfigurationContext,
    pub(crate) out: &'x mut SmallVec<[CoercedAttrRef<'a>; 4]>,
}

impl ValueVisitor for ConcatLeaves<'_, '_> {
    type Out = ResultMaybeCompatible<()>;
    fn visit<K: Semantics + 'static>(self) -> Self::Out {
        K::concat_leaves(K::view(self.v), self)
    }
}

pub(crate) struct ConcatClass;

impl ValueVisitor for ConcatClass {
    type Out = Option<&'static str>;
    fn visit<K: Semantics + 'static>(self) -> Self::Out {
        K::CONCAT
    }
}

pub(crate) struct ToJson<'x, 'a> {
    pub(crate) v: CoercedAttrRef<'a>,
    pub(crate) ctx: &'x AttrFmtContext,
}

impl ValueVisitor for ToJson<'_, '_> {
    type Out = buck2_error::Result<serde_json::Value>;
    fn visit<K: Semantics + 'static>(self) -> Self::Out {
        K::to_json(K::view(self.v), self)
    }
}

pub(crate) struct Fmt<'x, 'y, 'a> {
    pub(crate) v: CoercedAttrRef<'a>,
    pub(crate) ctx: &'x AttrFmtContext,
    pub(crate) f: &'x mut std::fmt::Formatter<'y>,
}

impl ValueVisitor for Fmt<'_, '_, '_> {
    type Out = std::fmt::Result;
    fn visit<K: Semantics + 'static>(self) -> Self::Out {
        K::fmt(K::view(self.v), self)
    }
}

pub(crate) struct MatchesAny<'x, 'a> {
    pub(crate) v: CoercedAttrRef<'a>,
    pub(crate) filter: &'x dyn Fn(&str) -> buck2_error::Result<bool>,
}

impl ValueVisitor for MatchesAny<'_, '_> {
    type Out = buck2_error::Result<bool>;
    fn visit<K: Semantics + 'static>(self) -> Self::Out {
        K::any_matches(K::view(self.v), self)
    }
}

pub(crate) struct JsonKey<'x, 'a> {
    pub(crate) v: CoercedAttrRef<'a>,
    pub(crate) ctx: &'x AttrFmtContext,
}

impl<'a> ValueVisitor for JsonKey<'_, 'a> {
    type Out = buck2_error::Result<Cow<'a, str>>;
    fn visit<K: Semantics + 'static>(self) -> Self::Out {
        K::json_key(K::view(self.v), self)
    }
}

/// [`AttrSerializeWithContext`] for a value: generic over the serializer.
pub(crate) struct SerializeKind<'x, 'a, S> {
    pub(crate) v: CoercedAttrRef<'a>,
    pub(crate) ctx: &'x AttrFmtContext,
    pub(crate) ser: S,
}

impl<S: Serializer> ValueVisitor for SerializeKind<'_, '_, S> {
    type Out = Result<S::Ok, S::Error>;
    fn visit<K: Semantics + 'static>(self) -> Self::Out {
        K::serialize(K::view(self.v), self)
    }
}
