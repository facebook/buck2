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
use std::fmt::Display;

use buck2_core::configuration::compatibility::IncompatiblePlatformReasonCause;
use buck2_core::configuration::compatibility::ResultMaybeCompatible;
use buck2_core::package::PackageLabel;
use buck2_core::package::source_path::SourcePathRef;
use buck2_core::provider::label::ProvidersLabel;
use buck2_error::buck2_error;
use buck2_error::internal_error;
use buck2_util::arc_str::ArcStr;
use display_container::fmt_container;
use display_container::fmt_keyed_container;
use dupe::Dupe;
use itertools::Itertools;
use serde::Serialize;
use serde::Serializer;
use serde::ser::SerializeMap;
use serde::ser::SerializeSeq;
use serde_json::to_value;
use smallvec::SmallVec;

use crate::attrs::arena::arg::ArgMacroView;
use crate::attrs::arena::arg::ArgPartView;
use crate::attrs::arena::query_expr::QueryView;
use crate::attrs::arena::schema::Kind;
use crate::attrs::arena::schema::KindVisitor;
use crate::attrs::arena::schema::kinds;
use crate::attrs::arena::view::AttrRefDisplay;
use crate::attrs::arena::view::AttrRefSelectorEntries;
use crate::attrs::arena::view::AttrRefSerialize;
use crate::attrs::arena::view::CoercedAttrRef;
use crate::attrs::arena::view::Refs;
use crate::attrs::arena::view::Values;
use crate::attrs::attr_type::AttrType;
use crate::attrs::attr_type::AttrTypeInner;
use crate::attrs::attr_type::any_matches::AnyMatches;
use crate::attrs::attr_type::arg::ConfiguredStringWithMacros;
use crate::attrs::attr_type::arg::MacroDepKind;
use crate::attrs::attr_type::arg::StringWithMacros;
use crate::attrs::attr_type::attr_config::source_file_display;
use crate::attrs::attr_type::bool::BoolLiteral;
use crate::attrs::attr_type::configuration_dep::ConfigurationDepAttrType;
use crate::attrs::attr_type::configuration_dep::ConfigurationDepKind;
use crate::attrs::attr_type::dep::DepAttr;
use crate::attrs::attr_type::dict::DictLiteral;
use crate::attrs::attr_type::label::LabelAttrType;
use crate::attrs::attr_type::list::ListLiteral;
use crate::attrs::attr_type::string::StringLiteral;
use crate::attrs::attr_type::tuple::TupleLiteral;
use crate::attrs::coerced_attr::select_fail_to_json;
use crate::attrs::coerced_attr::select_incompatible_to_json;
use crate::attrs::configuration_context::AttrConfigurationContext;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::configured_traversal::ConfiguredAttrTraversal;
use crate::attrs::display::AttrDisplayWithContext;
use crate::attrs::display::AttrDisplayWithContextExt;
use crate::attrs::fmt_context::AttrFmtContext;
use crate::attrs::serialize::AttrSerializeWithContext;
use crate::attrs::traversal::CoercedAttrTraversal;

/// What a kind's values mean: to dependency traversal, configuration,
/// rendering, JSON and substring search. One impl per kind, beside its
/// row, through [`semantics!`]; a kind without one does not compile.
pub(super) trait Semantics: Kind {
    /// The class of values `+` concatenates: leaves of one class
    /// concatenate; `None` means addition is not supported.
    const CONCAT: Option<&'static str> = None;

    fn parts<'a>(v: CoercedAttrRef<'a>) -> Self::Parts<'a> {
        Self::decode(v.arena, v.r)
    }

    /// Type-threaded traversal over the value's deps.
    fn traverse<'a>(
        v: CoercedAttrRef<'a>,
        t: &AttrType,
        pkg: Option<PackageLabel>,
        tr: &mut dyn CoercedAttrTraversal<'a>,
    ) -> buck2_error::Result<()>;

    /// The configured form of the value.
    fn configure(
        v: CoercedAttrRef<'_>,
        t: &AttrType,
        ctx: &dyn AttrConfigurationContext,
        name: Option<&str>,
    ) -> ResultMaybeCompatible<ConfiguredAttr>;

    /// The deps of the configured form, without materializing it. Leaves
    /// configure the small configured value and traverse that; containers,
    /// selects and concats recurse.
    fn traverse_configured(
        v: CoercedAttrRef<'_>,
        t: &AttrType,
        pkg: PackageLabel,
        ctx: &dyn AttrConfigurationContext,
        tr: &mut dyn ConfiguredAttrTraversal,
        name: Option<&str>,
    ) -> ResultMaybeCompatible<()> {
        // `Any`-typed values cannot contain deps.
        if matches!(inner_type(t).0.inner, AttrTypeInner::Any(_)) {
            return ResultMaybeCompatible::Compatible(());
        }
        let configured = Self::configure(v, t, ctx, name)?;
        if let Err(e) = configured.traverse(pkg, tr) {
            return e.into();
        }
        ResultMaybeCompatible::Compatible(())
    }

    /// The leaves `ConfiguredAttr::concat` would see: selects resolved,
    /// nested concats inlined. A plain value is its own leaf.
    fn concat_leaves<'a>(
        v: CoercedAttrRef<'a>,
        _ctx: &dyn AttrConfigurationContext,
        out: &mut SmallVec<[CoercedAttrRef<'a>; 4]>,
    ) -> ResultMaybeCompatible<()> {
        out.push(v);
        ResultMaybeCompatible::Compatible(())
    }

    fn to_json(
        v: CoercedAttrRef<'_>,
        ctx: &AttrFmtContext,
    ) -> buck2_error::Result<serde_json::Value>;

    fn fmt(
        v: CoercedAttrRef<'_>,
        ctx: &AttrFmtContext,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result;

    /// Whether any leaf's string form matches the filter.
    fn any_matches(
        v: CoercedAttrRef<'_>,
        filter: &dyn Fn(&str) -> buck2_error::Result<bool>,
    ) -> buck2_error::Result<bool>;

    /// The value's rendering as a JSON object key; an error for values that
    /// render as anything but a string.
    fn json_key<'a>(
        v: CoercedAttrRef<'a>,
        ctx: &AttrFmtContext,
    ) -> buck2_error::Result<Cow<'a, str>> {
        let _ = (v, ctx);
        Err(internal_error!("dict keys must render to JSON strings"))
    }

    /// Serialize the value's JSON form without building a `serde_json::Value`.
    fn serialize<S: Serializer>(
        v: CoercedAttrRef<'_>,
        ctx: &AttrFmtContext,
        ser: S,
    ) -> Result<S::Ok, S::Error>;
}

pub(super) fn ser_err<S: Serializer>(e: buck2_error::Error) -> S::Error {
    serde::ser::Error::custom(format!("{e}"))
}

/// Keyed values collect into a `BTreeMap` to match `serde_json::Map`: keys
/// emit in sorted order, and a repeated key keeps its last value.
pub(super) fn ser_keyed<'a, S: Serializer>(
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

pub(super) fn ser_seq<'a, S: Serializer>(
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
pub(super) fn inner_type(t: &AttrType) -> &AttrType {
    let mut t = t;
    while let AttrTypeInner::Option(opt) = &t.0.inner {
        t = &opt.inner;
    }
    t
}

/// A value whose kind does not fit the attribute type it is read with:
/// the coercer established the pairing, so this is a logic error.
pub(super) fn mismatch(v: CoercedAttrRef<'_>, t: &AttrType) -> buck2_error::Error {
    internal_error!(
        "attr arena: value `{}` does not fit attribute type `{}`",
        v.as_display_no_ctx(),
        t
    )
}

/// The `attrs.query()` view of a `Query` value.
pub(super) fn query_view(v: CoercedAttrRef<'_>) -> QueryView<'_> {
    QueryView {
        arena: v.arena,
        r: v.r,
    }
}

/// `impl Semantics` for one kind from its method bodies. Sections are in a
/// fixed order and each binds the decoded parts to its pattern; a section
/// the trait provides a default for may be omitted.
macro_rules! semantics {
    ($kind:ident $(: concat $concat:literal)? {
        $( traverse($tv:ident, $tp:pat_param, $tt:ident, $tpkg:ident, $ttr:ident) $traverse:block )?
        $( configure($cv:ident, $cp:pat_param, $ct:ident, $cctx:ident, $cname:ident) $configure:block )?
        $( traverse_configured($xv:ident, $xp:pat_param, $xt:ident, $xpkg:ident, $xctx:ident, $xtr:ident, $xname:ident) $tc:block )?
        $( concat_leaves($lv:ident, $lp:pat_param, $lctx:ident, $lout:ident) $leaves:block )?
        $( json($jv:ident, $jp:pat_param, $jctx:ident) $json:block )?
        $( fmt($fv:ident, $fp:pat_param, $fctx:ident, $ff:ident) $fmt:block )?
        $( any_matches($mv:ident, $mp:pat_param, $filter:ident) $matches:block )?
        $( json_key($kv:ident, $kp:pat_param, $kctx:ident) $key:block )?
        $( serialize<$S:ident>($sv:ident, $sp:pat_param, $sctx:ident, $ss:ident) $serialize:block )?
    }) => {
        impl Semantics for kinds::$kind {
            $( const CONCAT: Option<&'static str> = Some($concat); )?
            $(
                #[allow(unused_variables)]
                fn traverse<'a>(
                    $tv: CoercedAttrRef<'a>,
                    $tt: &AttrType,
                    $tpkg: Option<PackageLabel>,
                    $ttr: &mut dyn CoercedAttrTraversal<'a>,
                ) -> buck2_error::Result<()> {
                    let $tp = Self::parts($tv);
                    $traverse
                }
            )?
            $(
                #[allow(unused_variables)]
                fn configure(
                    $cv: CoercedAttrRef<'_>,
                    $ct: &AttrType,
                    $cctx: &dyn AttrConfigurationContext,
                    $cname: Option<&str>,
                ) -> ResultMaybeCompatible<ConfiguredAttr> {
                    let $cp = Self::parts($cv);
                    $configure
                }
            )?
            $(
                #[allow(unused_variables)]
                fn traverse_configured(
                    $xv: CoercedAttrRef<'_>,
                    $xt: &AttrType,
                    $xpkg: PackageLabel,
                    $xctx: &dyn AttrConfigurationContext,
                    $xtr: &mut dyn ConfiguredAttrTraversal,
                    $xname: Option<&str>,
                ) -> ResultMaybeCompatible<()> {
                    let $xp = Self::parts($xv);
                    $tc
                }
            )?
            $(
                #[allow(unused_variables)]
                fn concat_leaves<'a>(
                    $lv: CoercedAttrRef<'a>,
                    $lctx: &dyn AttrConfigurationContext,
                    $lout: &mut SmallVec<[CoercedAttrRef<'a>; 4]>,
                ) -> ResultMaybeCompatible<()> {
                    let $lp = Self::parts($lv);
                    $leaves
                }
            )?
            $(
                #[allow(unused_variables)]
                fn to_json(
                    $jv: CoercedAttrRef<'_>,
                    $jctx: &AttrFmtContext,
                ) -> buck2_error::Result<serde_json::Value> {
                    let $jp = Self::parts($jv);
                    $json
                }
            )?
            $(
                #[allow(unused_variables)]
                fn fmt(
                    $fv: CoercedAttrRef<'_>,
                    $fctx: &AttrFmtContext,
                    $ff: &mut std::fmt::Formatter<'_>,
                ) -> std::fmt::Result {
                    let $fp = Self::parts($fv);
                    $fmt
                }
            )?
            $(
                #[allow(unused_variables)]
                fn any_matches(
                    $mv: CoercedAttrRef<'_>,
                    $filter: &dyn Fn(&str) -> buck2_error::Result<bool>,
                ) -> buck2_error::Result<bool> {
                    let $mp = Self::parts($mv);
                    $matches
                }
            )?
            $(
                #[allow(unused_variables)]
                fn json_key<'a>(
                    $kv: CoercedAttrRef<'a>,
                    $kctx: &AttrFmtContext,
                ) -> buck2_error::Result<Cow<'a, str>> {
                    let $kp = Self::parts($kv);
                    $key
                }
            )?
            $(
                #[allow(unused_variables)]
                fn serialize<$S: Serializer>(
                    $sv: CoercedAttrRef<'_>,
                    $sctx: &AttrFmtContext,
                    $ss: $S,
                ) -> Result<$S::Ok, $S::Error> {
                    let $sp = Self::parts($sv);
                    $serialize
                }
            )?
        }
    };
}

/// Record kinds that are never attribute values: they have no semantics.
pub(super) trait NotAValue: Kind {}

impl NotAValue for kinds::SelectorKeys {}
impl NotAValue for kinds::ArgMacro {}
impl NotAValue for kinds::QueryExpr {}

impl<K: NotAValue> Semantics for K {
    fn traverse<'a>(
        _v: CoercedAttrRef<'a>,
        _t: &AttrType,
        _pkg: Option<PackageLabel>,
        _tr: &mut dyn CoercedAttrTraversal<'a>,
    ) -> buck2_error::Result<()> {
        unreachable!("not an attribute value")
    }
    fn configure(
        _v: CoercedAttrRef<'_>,
        _t: &AttrType,
        _ctx: &dyn AttrConfigurationContext,
        _name: Option<&str>,
    ) -> ResultMaybeCompatible<ConfiguredAttr> {
        unreachable!("not an attribute value")
    }
    fn to_json(
        _v: CoercedAttrRef<'_>,
        _ctx: &AttrFmtContext,
    ) -> buck2_error::Result<serde_json::Value> {
        unreachable!("not an attribute value")
    }
    fn fmt(
        _v: CoercedAttrRef<'_>,
        _ctx: &AttrFmtContext,
        _f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        unreachable!("not an attribute value")
    }
    fn any_matches(
        _v: CoercedAttrRef<'_>,
        _filter: &dyn Fn(&str) -> buck2_error::Result<bool>,
    ) -> buck2_error::Result<bool> {
        unreachable!("not an attribute value")
    }
    fn serialize<S: Serializer>(
        _v: CoercedAttrRef<'_>,
        _ctx: &AttrFmtContext,
        _ser: S,
    ) -> Result<S::Ok, S::Error> {
        unreachable!("not an attribute value")
    }
}

// ---- scalars: no deps, configure to themselves.

semantics!(None {
    traverse(v, (), t, pkg, tr) { Ok(()) }
    configure(v, (), t, ctx, name) { ResultMaybeCompatible::Compatible(ConfiguredAttr::None) }
    traverse_configured(v, (), t, pkg, ctx, tr, name) { ResultMaybeCompatible::Compatible(()) }
    json(v, (), ctx) { Ok(serde_json::Value::Null) }
    fmt(v, (), ctx, f) { write!(f, "None") }
    any_matches(v, (), filter) { Ok(false) }
    serialize<S>(v, (), ctx, ser) { ser.serialize_unit() }
});

semantics!(BoolFalse {
    traverse(v, (), t, pkg, tr) { Ok(()) }
    configure(v, (), t, ctx, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Bool(_) | AttrTypeInner::Any(_) => {
                ResultMaybeCompatible::Compatible(ConfiguredAttr::Bool(BoolLiteral(false)))
            }
            _ => mismatch(v, t).into(),
        }
    }
    traverse_configured(v, (), t, pkg, ctx, tr, name) { ResultMaybeCompatible::Compatible(()) }
    json(v, (), ctx) { Ok(to_value(false)?) }
    fmt(v, (), ctx, f) { write!(f, "{}", BoolLiteral(false)) }
    any_matches(v, (), filter) { BoolLiteral(false).any_matches(filter) }
    serialize<S>(v, (), ctx, ser) { ser.serialize_bool(false) }
});

semantics!(BoolTrue {
    traverse(v, (), t, pkg, tr) { Ok(()) }
    configure(v, (), t, ctx, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Bool(_) | AttrTypeInner::Any(_) => {
                ResultMaybeCompatible::Compatible(ConfiguredAttr::Bool(BoolLiteral(true)))
            }
            _ => mismatch(v, t).into(),
        }
    }
    traverse_configured(v, (), t, pkg, ctx, tr, name) { ResultMaybeCompatible::Compatible(()) }
    json(v, (), ctx) { Ok(to_value(true)?) }
    fmt(v, (), ctx, f) { write!(f, "{}", BoolLiteral(true)) }
    any_matches(v, (), filter) { BoolLiteral(true).any_matches(filter) }
    serialize<S>(v, (), ctx, ser) { ser.serialize_bool(true) }
});

semantics!(Int {
    traverse(v, i, t, pkg, tr) { Ok(()) }
    configure(v, i, t, ctx, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Int(_) | AttrTypeInner::Any(_) => {
                ResultMaybeCompatible::Compatible(ConfiguredAttr::Int(i))
            }
            _ => mismatch(v, t).into(),
        }
    }
    traverse_configured(v, i, t, pkg, ctx, tr, name) { ResultMaybeCompatible::Compatible(()) }
    json(v, i, ctx) { Ok(to_value(i)?) }
    fmt(v, i, ctx, f) { write!(f, "{i}") }
    any_matches(v, i, filter) { filter(&i.to_string()) }
    serialize<S>(v, i, ctx, ser) { ser.serialize_i64(i) }
});

/// `String` and `EnumVariant` render alike.
pub(super) fn fmt_text(
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

semantics!(String: concat "string" {
    traverse(v, (s,), t, pkg, tr) { Ok(()) }
    configure(v, (s,), t, ctx, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::String(_) | AttrTypeInner::Any(_) => ResultMaybeCompatible::Compatible(
                ConfiguredAttr::String(StringLiteral(ArcStr::from(s))),
            ),
            _ => mismatch(v, t).into(),
        }
    }
    traverse_configured(v, (s,), t, pkg, ctx, tr, name) { ResultMaybeCompatible::Compatible(()) }
    json(v, (s,), ctx) { Ok(to_value(s)?) }
    fmt(v, (s,), ctx, f) { fmt_text(s, ctx, f) }
    any_matches(v, (s,), filter) { filter(s) }
    json_key(v, (s,), ctx) { Ok(Cow::Borrowed(s)) }
    serialize<S>(v, (s,), ctx, ser) { ser.serialize_str(s) }
});

semantics!(EnumVariant {
    traverse(v, (s,), t, pkg, tr) { Ok(()) }
    configure(v, (s,), t, ctx, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Enum(_) => ResultMaybeCompatible::Compatible(
                ConfiguredAttr::EnumVariant(StringLiteral(ArcStr::from(s))),
            ),
            _ => mismatch(v, t).into(),
        }
    }
    traverse_configured(v, (s,), t, pkg, ctx, tr, name) { ResultMaybeCompatible::Compatible(()) }
    json(v, (s,), ctx) { Ok(to_value(s)?) }
    fmt(v, (s,), ctx, f) { fmt_text(s, ctx, f) }
    any_matches(v, (s,), filter) { filter(s) }
    json_key(v, (s,), ctx) { Ok(Cow::Borrowed(s)) }
    serialize<S>(v, (s,), ctx, ser) { ser.serialize_str(s) }
});

semantics!(ArgSimple: concat "arg" {
    traverse(v, (s,), t, pkg, tr) { Ok(()) }
    configure(v, (s,), t, ctx, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Arg(ty) => {
                ResultMaybeCompatible::Compatible(ConfiguredAttr::Arg(ConfiguredStringWithMacros {
                    string_with_macros: StringWithMacros::StringPart(ArcStr::from(s)),
                    anon_target_compatible: ty.anon_target_compatible,
                }))
            }
            _ => mismatch(v, t).into(),
        }
    }
    traverse_configured(v, (s,), t, pkg, ctx, tr, name) { ResultMaybeCompatible::Compatible(()) }
    json(v, (s,), ctx) { Ok(to_value(s)?) }
    fmt(v, (s,), ctx, f) { write!(f, "\"{s}\"") }
    any_matches(v, (s,), filter) { filter(s) }
    json_key(v, (s,), ctx) { Ok(Cow::Borrowed(s)) }
    serialize<S>(v, (s,), ctx, ser) { ser.serialize_str(s) }
});

// ---- select outcomes: type-transparent results of resolving a select.

semantics!(SelectFail {
    traverse(v, (m,), t, pkg, tr) { Ok(()) }
    configure(v, (m,), t, ctx, name) { select_fail(m) }
    traverse_configured(v, (m,), t, pkg, ctx, tr, name) { select_fail(m) }
    concat_leaves(v, (m,), ctx, out) { select_fail(m) }
    json(v, (m,), ctx) { select_fail_to_json(m) }
    fmt(v, (m,), ctx, f) { write!(f, "select_fail(\"{m}\")") }
    any_matches(v, (m,), filter) { Ok(false) }
    serialize<S>(v, (m,), ctx, ser) { select_fail_to_json(m).map_err(ser_err::<S>)?.serialize(ser) }
});

semantics!(SelectIncompatible {
    traverse(v, (m,), t, pkg, tr) { Ok(()) }
    configure(v, (m,), t, ctx, name) { select_incompatible(m, ctx) }
    traverse_configured(v, (m,), t, pkg, ctx, tr, name) { select_incompatible(m, ctx) }
    concat_leaves(v, (m,), ctx, out) { select_incompatible(m, ctx) }
    json(v, (m,), ctx) { select_incompatible_to_json(m) }
    fmt(v, (m,), ctx, f) { write!(f, "select_incompatible(\"{m}\")") }
    any_matches(v, (m,), filter) { Ok(false) }
    serialize<S>(v, (m,), ctx, ser) {
        select_incompatible_to_json(m).map_err(ser_err::<S>)?.serialize(ser)
    }
});

pub(super) fn select_fail<T>(message: &str) -> ResultMaybeCompatible<T> {
    buck2_error!(
        buck2_error::ErrorTag::Input,
        "select resolved to select_fail(): {message}"
    )
    .into()
}

pub(super) fn select_incompatible<T>(
    message: &str,
    ctx: &dyn AttrConfigurationContext,
) -> ResultMaybeCompatible<T> {
    ResultMaybeCompatible::Incompatible(ctx.incompatible_platform_reason(
        IncompatiblePlatformReasonCause::SelectIncompatible(ArcStr::from(message)),
    ))
}

// ---- containers.

pub(super) fn fmt_seq(
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

pub(super) fn json_seq(
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

pub(super) fn any_matches_seq(
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

semantics!(List: concat "list" {
    traverse(v, (items,), t, pkg, tr) {
        let inner = inner_type(t);
        let item_t = match &inner.0.inner {
            AttrTypeInner::Any(_) => inner,
            AttrTypeInner::List(ty) => &ty.inner,
            _ => return Err(mismatch(v, t)),
        };
        for item in items.iter() {
            item.traverse(item_t, pkg, tr)?;
        }
        Ok(())
    }
    configure(v, (items,), t, ctx, name) {
        let inner = inner_type(t);
        let item_t = match &inner.0.inner {
            AttrTypeInner::Any(_) => AttrType::any_ref(),
            AttrTypeInner::List(ty) => &ty.inner,
            _ => return mismatch(v, t).into(),
        };
        ResultMaybeCompatible::Compatible(ConfiguredAttr::List(ListLiteral(
            items
                .iter()
                .map(|item| item.configure(item_t, ctx, name))
                .collect::<ResultMaybeCompatible<Vec<_>>>()?
                .into(),
        )))
    }
    traverse_configured(v, (items,), t, pkg, ctx, tr, name) {
        let item_t = match &inner_type(t).0.inner {
            AttrTypeInner::Any(_) => return ResultMaybeCompatible::Compatible(()),
            AttrTypeInner::List(ty) => &ty.inner,
            _ => return mismatch(v, t).into(),
        };
        for item in items.iter() {
            item.traverse_configured(item_t, pkg, ctx, tr, name)?;
        }
        ResultMaybeCompatible::Compatible(())
    }
    json(v, (items,), ctx) { json_seq(items, ctx) }
    fmt(v, (items,), ctx, f) { fmt_seq(items, "[", "]", ctx, f) }
    any_matches(v, (items,), filter) { any_matches_seq(items, filter) }
    serialize<S>(v, (items,), ctx, ser) { ser_seq(items, ctx, ser) }
});

semantics!(Tuple {
    traverse(v, (items,), t, pkg, tr) {
        let inner = inner_type(t);
        match &inner.0.inner {
            AttrTypeInner::Any(_) => {
                for item in items.iter() {
                    item.traverse(inner, pkg, tr)?;
                }
            }
            AttrTypeInner::Tuple(ty) => {
                if items.len() != ty.xs.len() {
                    return Err(internal_error!("Inconsistent number of elements in tuple"));
                }
                for (item, item_t) in items.iter().zip(&ty.xs) {
                    item.traverse(item_t, pkg, tr)?;
                }
            }
            _ => return Err(mismatch(v, t)),
        }
        Ok(())
    }
    configure(v, (items,), t, ctx, name) {
        let configured = match &inner_type(t).0.inner {
            AttrTypeInner::Any(_) => items
                .iter()
                .map(|item| item.configure(AttrType::any_ref(), ctx, name))
                .collect::<ResultMaybeCompatible<Vec<_>>>()?
                .into(),
            AttrTypeInner::Tuple(ty) => {
                if items.len() != ty.xs.len() {
                    return internal_error!("Inconsistent number of elements in tuple").into();
                }
                items
                    .iter()
                    .zip(&ty.xs)
                    .map(|(item, item_t)| item.configure(item_t, ctx, name))
                    .collect::<ResultMaybeCompatible<_>>()?
            }
            _ => return mismatch(v, t).into(),
        };
        ResultMaybeCompatible::Compatible(ConfiguredAttr::Tuple(TupleLiteral(configured)))
    }
    traverse_configured(v, (items,), t, pkg, ctx, tr, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Any(_) => {}
            AttrTypeInner::Tuple(ty) => {
                if items.len() != ty.xs.len() {
                    return internal_error!("Inconsistent number of elements in tuple").into();
                }
                for (item, item_t) in items.iter().zip(&ty.xs) {
                    item.traverse_configured(item_t, pkg, ctx, tr, name)?;
                }
            }
            _ => return mismatch(v, t).into(),
        }
        ResultMaybeCompatible::Compatible(())
    }
    json(v, (items,), ctx) { json_seq(items, ctx) }
    fmt(v, (items,), ctx, f) { fmt_seq(items, "(", ")", ctx, f) }
    any_matches(v, (items,), filter) { any_matches_seq(items, filter) }
    serialize<S>(v, (items,), ctx, ser) { ser_seq(items, ctx, ser) }
});

semantics!(Dict: concat "dict" {
    traverse(v, (pairs,), t, pkg, tr) {
        let inner = inner_type(t);
        let (kt, vt) = match &inner.0.inner {
            AttrTypeInner::Any(_) => (inner, inner),
            AttrTypeInner::Dict(ty) => (&ty.key, &ty.value),
            _ => return Err(mismatch(v, t)),
        };
        for (k, val) in pairs.iter() {
            k.traverse(kt, pkg, tr)?;
            val.traverse(vt, pkg, tr)?;
        }
        Ok(())
    }
    configure(v, (pairs,), t, ctx, name) {
        let (kt, vt) = match &inner_type(t).0.inner {
            AttrTypeInner::Any(_) => (AttrType::any_ref(), AttrType::any_ref()),
            AttrTypeInner::Dict(ty) => (&ty.key, &ty.value),
            _ => return mismatch(v, t).into(),
        };
        ResultMaybeCompatible::Compatible(ConfiguredAttr::Dict(DictLiteral(
            pairs
                .iter()
                .map(|(k, val)| {
                    let k2 = k.configure(kt, ctx, name)?;
                    let v2 = val.configure(vt, ctx, name)?;
                    ResultMaybeCompatible::Compatible((k2, v2))
                })
                .collect::<ResultMaybeCompatible<Vec<_>>>()?
                .into(),
        )))
    }
    traverse_configured(v, (pairs,), t, pkg, ctx, tr, name) {
        let (kt, vt) = match &inner_type(t).0.inner {
            AttrTypeInner::Any(_) => return ResultMaybeCompatible::Compatible(()),
            AttrTypeInner::Dict(ty) => (&ty.key, &ty.value),
            _ => return mismatch(v, t).into(),
        };
        for (k, val) in pairs.iter() {
            k.traverse_configured(kt, pkg, ctx, tr, name)?;
            val.traverse_configured(vt, pkg, ctx, tr, name)?;
        }
        ResultMaybeCompatible::Compatible(())
    }
    json(v, (pairs,), ctx) {
        let mut res = serde_json::Map::with_capacity(pairs.len());
        for (k, val) in pairs.iter() {
            res.insert(
                k.to_json(ctx)?
                    .as_str()
                    .ok_or_else(|| internal_error!("dict keys must render to JSON strings"))?
                    .to_owned(),
                val.to_json(ctx)?,
            );
        }
        Ok(serde_json::Value::Object(res))
    }
    fmt(v, (pairs,), ctx, f) {
        fmt_keyed_container(
            f,
            "{",
            "}",
            ": ",
            pairs
                .iter()
                .map(|(k, val)| (AttrRefDisplay { ctx, r: k }, AttrRefDisplay { ctx, r: val })),
        )
    }
    any_matches(v, (pairs,), filter) {
        for (k, val) in pairs.iter() {
            if k.any_matches(filter)? || val.any_matches(filter)? {
                return Ok(true);
            }
        }
        Ok(false)
    }
    serialize<S>(v, (pairs,), ctx, ser) {
        ser_keyed(
            pairs
                .iter()
                .map(|(k, val)| Ok((k.json_key(ctx)?, val)))
                .collect::<buck2_error::Result<Vec<_>>>()
                .map_err(ser_err::<S>)?
                .into_iter(),
            ctx,
            ser,
        )
    }
});

semantics!(OneOf {
    traverse(v, (idx, inner), t, pkg, tr) {
        match &inner_type(t).0.inner {
            AttrTypeInner::OneOf(ty) => {
                let item_t = ty
                    .xs
                    .get(idx as usize)
                    .ok_or_else(|| internal_error!("invalid oneof index"))?;
                inner.traverse(item_t, pkg, tr)
            }
            _ => Err(mismatch(v, t)),
        }
    }
    configure(v, (idx, inner), t, ctx, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::OneOf(ty) => {
                let item_t = match ty.xs.get(idx as usize) {
                    Some(ty) => ty,
                    None => return internal_error!("invalid oneof index").into(),
                };
                let configured = inner.configure(item_t, ctx, name)?;
                ResultMaybeCompatible::Compatible(ConfiguredAttr::OneOf(Box::new(configured), idx))
            }
            _ => mismatch(v, t).into(),
        }
    }
    traverse_configured(v, (idx, inner), t, pkg, ctx, tr, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Any(_) => ResultMaybeCompatible::Compatible(()),
            AttrTypeInner::OneOf(ty) => {
                let item_t = match ty.xs.get(idx as usize) {
                    Some(ty) => ty,
                    None => return internal_error!("invalid oneof index").into(),
                };
                inner.traverse_configured(item_t, pkg, ctx, tr, name)
            }
            _ => mismatch(v, t).into(),
        }
    }
    json(v, (idx, inner), ctx) { inner.to_json(ctx) }
    fmt(v, (idx, inner), ctx, f) { AttrDisplayWithContext::fmt(&inner, ctx, f) }
    any_matches(v, (idx, inner), filter) { inner.any_matches(filter) }
    json_key(v, (idx, inner), ctx) { inner.json_key(ctx) }
    serialize<S>(v, (idx, inner), ctx, ser) { inner.serialize_with_ctx(ctx, ser) }
});

// ---- select and concat: type-transparent, resolved before the type is
// consulted.

semantics!(Selector {
    traverse(v, (entries, default), t, pkg, tr) {
        for (key, val) in entries.iter() {
            tr.configuration_dep(key, ConfigurationDepKind::SelectKey)?;
            val.traverse(t, pkg, tr)?;
        }
        if let Some(d) = default {
            d.traverse(t, pkg, tr)?;
        }
        Ok(())
    }
    configure(v, (entries, default), t, ctx, name) {
        match CoercedAttrRef::select_view(ctx, v, entries, default, name)? {
            Some(selected) => selected.configure(t, ctx, name),
            None => CoercedAttrRef::select_no_match_error(ctx, entries).into(),
        }
    }
    traverse_configured(v, (entries, default), t, pkg, ctx, tr, name) {
        match CoercedAttrRef::select_view(ctx, v, entries, default, name) {
            Ok(Some(selected)) => selected.traverse_configured(t, pkg, ctx, tr, name),
            Ok(None) => CoercedAttrRef::select_no_match_error(ctx, entries).into(),
            Err(e) => e.into(),
        }
    }
    concat_leaves(v, (entries, default), ctx, out) {
        match CoercedAttrRef::select_view(ctx, v, entries, default, None) {
            Ok(Some(selected)) => selected.concat_leaves(ctx, out),
            Ok(None) => CoercedAttrRef::select_no_match_error(ctx, entries).into(),
            Err(e) => e.into(),
        }
    }
    json(v, (entries, default), ctx) {
        let mut map = serde_json::Map::new();
        for (key, value) in entries.iter() {
            map.insert(key.to_string(), value.to_json(ctx)?);
        }
        if let Some(default) = default {
            map.insert("DEFAULT".to_owned(), default.to_json(ctx)?);
        }
        Ok(serde_json::Value::Object(serde_json::Map::from_iter([
            (
                "__type".to_owned(),
                serde_json::Value::String("selector".to_owned()),
            ),
            ("entries".to_owned(), serde_json::Value::Object(map)),
        ])))
    }
    fmt(v, (entries, default), ctx, f) {
        write!(f, "select(")?;
        fmt_keyed_container(
            f,
            "{",
            "}",
            ": ",
            entries
                .iter()
                .map(|(k, val)| (format!("\"{k}\""), AttrRefDisplay { ctx, r: val }))
                .chain(default.map(|d| ("\"DEFAULT\"".to_owned(), AttrRefDisplay { ctx, r: d }))),
        )?;
        write!(f, ")")
    }
    any_matches(v, (entries, default), filter) {
        for (_, val) in entries.iter() {
            if val.any_matches(filter)? {
                return Ok(true);
            }
        }
        if let Some(default) = default {
            if default.any_matches(filter)? {
                return Ok(true);
            }
        }
        Ok(false)
    }
    serialize<S>(v, (entries, default), ctx, ser) {
        struct Entries<'a> {
            entries: AttrRefSelectorEntries<'a>,
            default: Option<CoercedAttrRef<'a>>,
            ctx: &'a AttrFmtContext,
        }
        impl Serialize for Entries<'_> {
            fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
                ser_keyed(
                    self.entries
                        .iter()
                        .map(|(k, v)| (Cow::Owned(k.to_string()), v))
                        .chain(self.default.map(|d| (Cow::Borrowed("DEFAULT"), d))),
                    self.ctx,
                    s,
                )
            }
        }
        let mut map = ser.serialize_map(Some(2))?;
        map.serialize_entry("__type", "selector")?;
        map.serialize_entry("entries", &Entries { entries, default, ctx })?;
        map.end()
    }
});

semantics!(Concat {
    traverse(v, (items,), t, pkg, tr) {
        for item in items.iter() {
            item.traverse(t, pkg, tr)?;
        }
        Ok(())
    }
    configure(v, (items,), t, ctx, name) {
        let singleton = items.len() == 1;
        let mut configured = Vec::with_capacity(items.len());
        for item in items.iter() {
            configured.push(item.configure(t, ctx, name)?);
        }
        let mut iter = configured.into_iter();
        let first = match iter.next() {
            Some(first) => first,
            None => return internal_error!("concat with no items").into(),
        };
        ResultMaybeCompatible::Compatible(if singleton {
            first
        } else {
            first.concat(t, &mut iter.map(Ok))?
        })
    }
    traverse_configured(v, (items,), t, pkg, ctx, tr, name) {
        let mut leaves: SmallVec<[CoercedAttrRef<'_>; 4]> = SmallVec::new();
        for item in items.iter() {
            item.concat_leaves(ctx, &mut leaves)?;
        }
        if leaves.len() > 1
            && let Err(e) = validate_concat(t, &leaves)
        {
            return e.into();
        }
        for leaf in leaves {
            leaf.traverse_configured(t, pkg, ctx, tr, name)?;
        }
        ResultMaybeCompatible::Compatible(())
    }
    concat_leaves(v, (items,), ctx, out) {
        for item in items.iter() {
            item.concat_leaves(ctx, out)?;
        }
        ResultMaybeCompatible::Compatible(())
    }
    json(v, (items,), ctx) {
        Ok(serde_json::Value::Object(serde_json::Map::from_iter([
            (
                "__type".to_owned(),
                serde_json::Value::String("concat".to_owned()),
            ),
            ("items".to_owned(), json_seq(items, ctx)?),
        ])))
    }
    fmt(v, (items,), ctx, f) {
        items
            .iter()
            .map(|a| AttrRefDisplay { ctx, r: a })
            .format("+")
            .fmt(f)
    }
    any_matches(v, (items,), filter) { any_matches_seq(items, filter) }
    serialize<S>(v, (items,), ctx, ser) {
        struct Items<'a> {
            items: Refs<'a, Values>,
            ctx: &'a AttrFmtContext,
        }
        impl Serialize for Items<'_> {
            fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
                ser_seq(self.items, self.ctx, s)
            }
        }
        let mut map = ser.serialize_map(Some(2))?;
        map.serialize_entry("__type", "concat")?;
        map.serialize_entry("items", &Items { items, ctx })?;
        map.end()
    }
});

/// The checks `ConfiguredAttr::concat` makes on its items, applied to the
/// resolved leaves: one concat class throughout, equal oneof variants, no
/// dict key on both sides.
pub(super) fn validate_concat<'a>(
    t: &AttrType,
    leaves: &[CoercedAttrRef<'a>],
) -> buck2_error::Result<()> {
    let Some((first, rest)) = leaves.split_first() else {
        return Ok(());
    };
    let t = inner_type(t);
    if let AttrTypeInner::OneOf(oneof) = &t.0.inner {
        let mut inner: SmallVec<[CoercedAttrRef<'a>; 4]> = SmallVec::with_capacity(leaves.len());
        let mut variant = None;
        for leaf in leaves {
            let Some((i, v)) = leaf.parts::<kinds::OneOf>() else {
                return Err(internal_error!(
                    "expecting oneof variant, got: {}",
                    leaf.as_display_no_ctx()
                ));
            };
            match variant {
                None => variant = Some(i),
                Some(first_i) if first_i != i => {
                    return Err(buck2_error!(
                        buck2_error::ErrorTag::Input,
                        "Cannot concatenate values coerced/configured \
                        to different oneof variants: `{}` and `{}`",
                        oneof.get(first_i)?,
                        oneof.get(i)?
                    ));
                }
                Some(_) => {}
            }
            inner.push(v);
        }
        return match variant {
            Some(i) => validate_concat(oneof.get(i)?, &inner),
            None => Ok(()),
        };
    }
    if first.parts::<kinds::OneOf>().is_some() {
        return Err(internal_error!(
            "Inconsistent attr value (`{}`) and attr type (`{}`)",
            first.as_display_no_ctx(),
            t
        ));
    }
    let Some(class) = first.concat_class() else {
        return Err(buck2_error!(
            buck2_error::ErrorTag::Input,
            "addition not supported for this attribute type `{}`",
            first.as_display_no_ctx()
        ));
    };
    for leaf in rest {
        if leaf.concat_class() != Some(class) {
            return Err(buck2_error!(
                buck2_error::ErrorTag::Input,
                "addition not supported for these attribute type `{}` and value `{}`",
                class,
                leaf.as_display_no_ctx()
            ));
        }
    }
    if let Some((first_pairs,)) = first.parts::<kinds::Dict>() {
        let mut seen: Vec<CoercedAttrRef<'a>> = first_pairs.iter().map(|(k, _)| k).collect();
        for leaf in rest {
            let (pairs,) = leaf
                .parts::<kinds::Dict>()
                .expect("should be a dict: same concat class as the first leaf");
            for (k, _) in pairs.iter() {
                if seen.iter().any(|s| s.r == k.r || s.content_eq(&k)) {
                    return Err(buck2_error!(
                        buck2_error::ErrorTag::Input,
                        "got same key in both sides of dictionary concat (key `{}`)",
                        k.as_display_no_ctx()
                    ));
                }
                seen.push(k);
            }
        }
    }
    Ok(())
}

// ---- dep-like leaves: configure the leaf and traverse the small
// configured value (the trait's default `traverse_configured`).

pub(super) fn json_label(l: &ProvidersLabel) -> buck2_error::Result<serde_json::Value> {
    Ok(to_value(l.to_string())?)
}

semantics!(Dep {
    traverse(v, (l,), t, pkg, tr) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Dep(ty) => DepAttr::<ProvidersLabel>::traverse(l, ty, tr),
            _ => Err(mismatch(v, t)),
        }
    }
    configure(v, (l,), t, ctx, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Dep(ty) => ResultMaybeCompatible::Compatible(ty.configure(l, ctx)?),
            _ => mismatch(v, t).into(),
        }
    }
    json(v, (l,), ctx) { json_label(l) }
    fmt(v, (l,), ctx, f) { write!(f, "\"{l}\"") }
    any_matches(v, (l,), filter) { filter(&l.to_string()) }
    json_key(v, (l,), ctx) { Ok(Cow::Owned(l.to_string())) }
    serialize<S>(v, (l,), ctx, ser) { ser.serialize_str(&l.to_string()) }
});

semantics!(Label {
    traverse(v, (l,), t, pkg, tr) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Label(_) => tr.label(l),
            _ => Err(mismatch(v, t)),
        }
    }
    configure(v, (l,), t, ctx, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Label(_) => ResultMaybeCompatible::Compatible(LabelAttrType::configure(ctx, l)?),
            _ => mismatch(v, t).into(),
        }
    }
    json(v, (l,), ctx) { json_label(l) }
    fmt(v, (l,), ctx, f) { write!(f, "\"{l}\"") }
    any_matches(v, (l,), filter) { filter(&l.to_string()) }
    json_key(v, (l,), ctx) { Ok(Cow::Owned(l.to_string())) }
    serialize<S>(v, (l,), ctx, ser) { ser.serialize_str(&l.to_string()) }
});

semantics!(SourceLabel {
    traverse(v, (l,), t, pkg, tr) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Source(_) => tr.dep(l),
            _ => Err(mismatch(v, t)),
        }
    }
    configure(v, (l,), t, ctx, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Source(_) => ResultMaybeCompatible::Compatible(ConfiguredAttr::SourceLabel(
                l.configure_pair(ctx.cfg().cfg_pair().dupe()),
            )),
            _ => mismatch(v, t).into(),
        }
    }
    traverse_configured(v, (l,), t, pkg, ctx, tr, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Any(_) => {}
            AttrTypeInner::Source(_) => {
                let configured = l.configure_pair(ctx.cfg().cfg_pair().dupe());
                if let Err(e) = tr.dep(&configured) {
                    return e.into();
                }
            }
            _ => return mismatch(v, t).into(),
        }
        ResultMaybeCompatible::Compatible(())
    }
    json(v, (l,), ctx) { json_label(l) }
    fmt(v, (l,), ctx, f) { write!(f, "\"{l}\"") }
    any_matches(v, (l,), filter) { filter(&l.to_string()) }
    json_key(v, (l,), ctx) { Ok(Cow::Owned(l.to_string())) }
    serialize<S>(v, (l,), ctx, ser) { ser.serialize_str(&l.to_string()) }
});

semantics!(ConfigurationDep {
    traverse(v, (l,), t, pkg, tr) {
        match &inner_type(t).0.inner {
            AttrTypeInner::ConfigurationDep(ty) => tr.configuration_dep(l, ty.0),
            _ => Err(mismatch(v, t)),
        }
    }
    configure(v, (l,), t, ctx, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::ConfigurationDep(_) => {
                ResultMaybeCompatible::Compatible(ConfigurationDepAttrType::configure(ctx, l)?)
            }
            _ => mismatch(v, t).into(),
        }
    }
    json(v, (l,), ctx) { json_label(l) }
    fmt(v, (l,), ctx, f) { write!(f, "\"{l}\"") }
    any_matches(v, (l,), filter) { filter(&l.to_string()) }
    json_key(v, (l,), ctx) { Ok(Cow::Owned(l.to_string())) }
    serialize<S>(v, (l,), ctx, ser) { ser.serialize_str(&l.to_string()) }
});

semantics!(SplitTransitionDep {
    traverse(v, (l,), t, pkg, tr) {
        match &inner_type(t).0.inner {
            AttrTypeInner::SplitTransitionDep(ty) => tr.split_transition_dep(l, &ty.transition),
            _ => Err(mismatch(v, t)),
        }
    }
    configure(v, (l,), t, ctx, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::SplitTransitionDep(ty) => ResultMaybeCompatible::Compatible(ty.configure(l, ctx)?),
            _ => mismatch(v, t).into(),
        }
    }
    json(v, (l,), ctx) { json_label(l) }
    fmt(v, (l,), ctx, f) { std::fmt::Display::fmt(l, f) }
    any_matches(v, (l,), filter) { filter(&l.to_string()) }
    json_key(v, (l,), ctx) { Ok(Cow::Owned(l.to_string())) }
    serialize<S>(v, (l,), ctx, ser) { ser.serialize_str(&l.to_string()) }
});

semantics!(PluginDep {
    traverse(v, (l,), t, pkg, tr) {
        match &inner_type(t).0.inner {
            AttrTypeInner::PluginDep(ty) => tr.plugin_dep(l, ty.kind()),
            _ => Err(mismatch(v, t)),
        }
    }
    configure(v, (l,), t, ctx, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::PluginDep(ty) => ResultMaybeCompatible::Compatible(ConfiguredAttr::PluginDep(
                l.dupe(),
                ty.kind().dupe(),
            )),
            _ => mismatch(v, t).into(),
        }
    }
    traverse_configured(v, (l,), t, pkg, ctx, tr, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Any(_) => {}
            AttrTypeInner::PluginDep(ty) => {
                if let Err(e) = tr.plugin_dep(l, ty.kind()) {
                    return e.into();
                }
            }
            _ => return mismatch(v, t).into(),
        }
        ResultMaybeCompatible::Compatible(())
    }
    json(v, (l,), ctx) { Ok(to_value(l.to_string())?) }
    fmt(v, (l,), ctx, f) { write!(f, "\"{l}\"") }
    any_matches(v, (l,), filter) { filter(&l.to_string()) }
    json_key(v, (l,), ctx) { Ok(Cow::Owned(l.to_string())) }
    serialize<S>(v, (l,), ctx, ser) { ser.serialize_str(&l.to_string()) }
});

semantics!(SourceFile {
    traverse(v, (p,), t, pkg, tr) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Source(_) => {
                let pkg = pkg.ok_or_else(|| {
                    internal_error!("Expected a package when traversing coerced source attribute")
                })?;
                for x in p.inputs() {
                    tr.input(SourcePathRef::new(pkg, x))?;
                }
                Ok(())
            }
            _ => Err(mismatch(v, t)),
        }
    }
    configure(v, (p,), t, ctx, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Source(_) => ResultMaybeCompatible::Compatible(ConfiguredAttr::SourceFile(p.clone())),
            _ => mismatch(v, t).into(),
        }
    }
    traverse_configured(v, (p,), t, pkg, ctx, tr, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Any(_) => {}
            AttrTypeInner::Source(_) => {
                for x in p.inputs() {
                    if let Err(e) = tr.input(SourcePathRef::new(pkg, x)) {
                        return e.into();
                    }
                }
            }
            _ => return mismatch(v, t).into(),
        }
        ResultMaybeCompatible::Compatible(())
    }
    json(v, (p,), ctx) { Ok(to_value(source_file_display(ctx, p).to_string())?) }
    fmt(v, (p,), ctx, f) { write!(f, "\"{}\"", source_file_display(ctx, p)) }
    any_matches(v, (p,), filter) { filter(&p.path().to_string()) }
    json_key(v, (p,), ctx) { Ok(Cow::Owned(source_file_display(ctx, p).to_string())) }
    serialize<S>(v, (p,), ctx, ser) { ser.serialize_str(&source_file_display(ctx, p).to_string()) }
});

semantics!(ArgParts: concat "arg" {
    traverse(v, (parts,), t, pkg, tr) {
        if !matches!(&inner_type(t).0.inner, AttrTypeInner::Arg(_)) {
            return Err(mismatch(v, t));
        }
        for part in parts.iter() {
            let ArgPartView::Macro { mac, .. } = part else {
                continue;
            };
            match mac {
                ArgMacroView::UserKeyedPlaceholder { label, .. } => tr.dep(label)?,
                ArgMacroView::Location { label, dep_kind: MacroDepKind::Exec } => tr.exec_dep(label)?,
                ArgMacroView::Location { label, dep_kind: MacroDepKind::Toolchain } => {
                    tr.toolchain_dep(label)?
                }
                ArgMacroView::Location { label, dep_kind: MacroDepKind::Regular } => tr.dep(label)?,
                ArgMacroView::Exe { label, exec_dep: true } => tr.exec_dep(label)?,
                ArgMacroView::Exe { label, exec_dep: false } => tr.dep(label)?,
                ArgMacroView::Source(path) => {
                    let pkg = pkg.ok_or_else(|| {
                        internal_error!("Expected a package when traversing arg source macro")
                    })?;
                    for x in path.inputs() {
                        tr.input(SourcePathRef::new(pkg, x))?;
                    }
                }
                // Literal labels only, in expression order.
                ArgMacroView::Query(q) => {
                    let mut labels = Vec::new();
                    q.literal_labels(&mut labels);
                    for label in labels {
                        tr.dep(label)?;
                    }
                }
                ArgMacroView::UserUnkeyedPlaceholder(_) => {}
            }
        }
        Ok(())
    }
    configure(v, (parts,), t, ctx, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Arg(ty) => {
                let configured = parts
                    .iter()
                    .map(|p| p.configure(ctx))
                    .collect::<buck2_error::Result<Box<[_]>>>()?;
                ResultMaybeCompatible::Compatible(ConfiguredAttr::Arg(ConfiguredStringWithMacros {
                    string_with_macros: StringWithMacros::ManyParts(configured),
                    anon_target_compatible: ty.anon_target_compatible,
                }))
            }
            _ => mismatch(v, t).into(),
        }
    }
    json(v, (parts,), ctx) { Ok(to_value(parts.to_string())?) }
    fmt(v, (parts,), ctx, f) { write!(f, "\"{parts}\"") }
    any_matches(v, (parts,), filter) { filter(&parts.to_string()) }
    json_key(v, (parts,), ctx) { Ok(Cow::Owned(parts.to_string())) }
    serialize<S>(v, (parts,), ctx, ser) { ser.serialize_str(&parts.to_string()) }
});

semantics!(Query {
    traverse(v, (expr,), t, pkg, tr) {
        // Queries have no inputs or configuration deps: literal labels only,
        // in expression order.
        if !matches!(&inner_type(t).0.inner, AttrTypeInner::Query(_)) {
            return Err(mismatch(v, t));
        }
        let mut labels = Vec::new();
        expr.labels(&mut labels);
        for label in labels {
            tr.dep(label)?;
        }
        Ok(())
    }
    configure(v, (expr,), t, ctx, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Query(_) => ResultMaybeCompatible::Compatible(ConfiguredAttr::Query(
                Box::new(query_view(v).configure(ctx)),
            )),
            _ => mismatch(v, t).into(),
        }
    }
    json(v, (expr,), ctx) { Ok(to_value(query_view(v).query().0)?) }
    fmt(v, (expr,), ctx, f) { write!(f, "\"{}\"", query_view(v).query().0) }
    any_matches(v, (expr,), filter) { filter(&query_view(v).query().0) }
    serialize<S>(v, (expr,), ctx, ser) { ser.serialize_str(&query_view(v).query().0) }
});

semantics!(ExplicitConfiguredDep {
    traverse(v, (label, platform), t, pkg, tr) {
        match &inner_type(t).0.inner {
            AttrTypeInner::ConfiguredDep(_) => {
                tr.dep(label)?;
                tr.configuration_dep(
                    &ProvidersLabel::default_for(platform.dupe()),
                    ConfigurationDepKind::ConfiguredDepPlatform,
                )
            }
            _ => Err(mismatch(v, t)),
        }
    }
    configure(v, (label, platform), t, ctx, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::ConfiguredDep(ty) => {
                ResultMaybeCompatible::Compatible(ty.configure(ctx, label, platform.dupe())?)
            }
            _ => mismatch(v, t).into(),
        }
    }
    json(v, (label, platform), ctx) { Ok(to_value([label.to_string(), platform.to_string()])?) }
    fmt(v, (label, platform), ctx, f) { write!(f, "({label}, {platform})") }
    any_matches(v, (label, platform), filter) { filter(&format!("({label}, {platform})")) }
    serialize<S>(v, (label, platform), ctx, ser) {
        [label.to_string(), platform.to_string()].serialize(ser)
    }
});

semantics!(TransitionDep {
    traverse(v, (dep, transition), t, pkg, tr) {
        match &inner_type(t).0.inner {
            AttrTypeInner::TransitionDep(ty) => ty.traverse(dep, transition, tr),
            _ => Err(mismatch(v, t)),
        }
    }
    configure(v, (dep, transition), t, ctx, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::TransitionDep(ty) => {
                ResultMaybeCompatible::Compatible(ty.configure(dep, transition, ctx)?)
            }
            _ => mismatch(v, t).into(),
        }
    }
    json(v, (dep, transition), ctx) {
        match transition {
            Some(tr) => Ok(to_value([dep.to_string(), tr.to_string()])?),
            None => Ok(to_value(dep.to_string())?),
        }
    }
    fmt(v, (dep, transition), ctx, f) { std::fmt::Display::fmt(dep, f) }
    any_matches(v, (dep, transition), filter) { filter(&dep.to_string()) }
    serialize<S>(v, (dep, transition), ctx, ser) {
        match transition {
            Some(tr) => [dep.to_string(), tr.to_string()].serialize(ser),
            None => ser.serialize_str(&dep.to_string()),
        }
    }
});

// ---- whole values: configure to a clone of themselves, no deps.

semantics!(Visibility {
    traverse(v, (s,), t, pkg, tr) { Ok(()) }
    configure(v, (s,), t, ctx, name) { ResultMaybeCompatible::Compatible(ConfiguredAttr::Visibility(s.clone())) }
    traverse_configured(v, (s,), t, pkg, ctx, tr, name) { ResultMaybeCompatible::Compatible(()) }
    json(v, (s,), ctx) { Ok(s.to_json()) }
    fmt(v, (s,), ctx, f) { write!(f, "{}", s) }
    any_matches(v, (s,), filter) { s.any_matches(filter) }
    serialize<S>(v, (s,), ctx, ser) { s.to_json().serialize(ser) }
});

semantics!(WithinView {
    traverse(v, (s,), t, pkg, tr) { Ok(()) }
    configure(v, (s,), t, ctx, name) { ResultMaybeCompatible::Compatible(ConfiguredAttr::WithinView(s.clone())) }
    traverse_configured(v, (s,), t, pkg, ctx, tr, name) { ResultMaybeCompatible::Compatible(()) }
    json(v, (s,), ctx) { Ok(s.to_json()) }
    fmt(v, (s,), ctx, f) { write!(f, "{}", s) }
    any_matches(v, (s,), filter) { s.any_matches(filter) }
    serialize<S>(v, (s,), ctx, ser) { s.to_json().serialize(ser) }
});

semantics!(Modifiers {
    traverse(v, (m,), t, pkg, tr) { Ok(()) }
    configure(v, (m,), t, ctx, name) { ResultMaybeCompatible::Compatible(ConfiguredAttr::TargetModifiers(m.dupe())) }
    traverse_configured(v, (m,), t, pkg, ctx, tr, name) { ResultMaybeCompatible::Compatible(()) }
    json(v, (m,), ctx) { Ok(m.to_json()) }
    fmt(v, (m,), ctx, f) { write!(f, "{}", m) }
    any_matches(v, (m,), filter) { m.any_matches(filter) }
    serialize<S>(v, (m,), ctx, ser) { m.to_json().serialize(ser) }
});

semantics!(Metadata {
    traverse(v, (m,), t, pkg, tr) { Ok(()) }
    configure(v, (m,), t, ctx, name) { ResultMaybeCompatible::Compatible(ConfiguredAttr::Metadata(m.clone())) }
    traverse_configured(v, (m,), t, pkg, ctx, tr, name) { ResultMaybeCompatible::Compatible(()) }
    json(v, (m,), ctx) { Ok(m.to_value()) }
    fmt(v, (m,), ctx, f) { write!(f, "{}", m) }
    any_matches(v, (m,), filter) { m.any_matches(filter) }
    serialize<S>(v, (m,), ctx, ser) { m.to_value().serialize(ser) }
});

/// One semantic entry point as a visitor: [`dispatch`] runs it for the
/// value's kind.
macro_rules! semantic_visitor {
    ($name:ident<$($lt:lifetime),*> { $($f:ident : $ty:ty),* $(,)? } -> $out:ty, |$k:ident, $s:ident| $body:expr) => {
        pub(super) struct $name<$($lt),*> {
            $(pub(super) $f: $ty,)*
        }

        impl<$($lt),*> KindVisitor for $name<$($lt),*> {
            type Out = $out;
            fn visit<$k: Semantics + 'static>($s) -> $out {
                $body
            }
        }
    };
}

semantic_visitor!(Traverse<'x, 'a> {
    v: CoercedAttrRef<'a>, t: &'x AttrType, pkg: Option<PackageLabel>, tr: &'x mut dyn CoercedAttrTraversal<'a>,
} -> buck2_error::Result<()>, |K, self| K::traverse(self.v, self.t, self.pkg, self.tr));

semantic_visitor!(Configure<'x, 'a> {
    v: CoercedAttrRef<'a>, t: &'x AttrType, ctx: &'x dyn AttrConfigurationContext, name: Option<&'x str>,
} -> ResultMaybeCompatible<ConfiguredAttr>, |K, self| K::configure(self.v, self.t, self.ctx, self.name));

semantic_visitor!(TraverseConfigured<'x, 'a> {
    v: CoercedAttrRef<'a>, t: &'x AttrType, pkg: PackageLabel, ctx: &'x dyn AttrConfigurationContext,
    tr: &'x mut dyn ConfiguredAttrTraversal, name: Option<&'x str>,
} -> ResultMaybeCompatible<()>, |K, self| K::traverse_configured(self.v, self.t, self.pkg, self.ctx, self.tr, self.name));

semantic_visitor!(ConcatLeaves<'x, 'a> {
    v: CoercedAttrRef<'a>, ctx: &'x dyn AttrConfigurationContext, out: &'x mut SmallVec<[CoercedAttrRef<'a>; 4]>,
} -> ResultMaybeCompatible<()>, |K, self| K::concat_leaves(self.v, self.ctx, self.out));

semantic_visitor!(ToJson<'x, 'a> {
    v: CoercedAttrRef<'a>, ctx: &'x AttrFmtContext,
} -> buck2_error::Result<serde_json::Value>, |K, self| K::to_json(self.v, self.ctx));

semantic_visitor!(Fmt<'x, 'y, 'a> {
    v: CoercedAttrRef<'a>, ctx: &'x AttrFmtContext, f: &'x mut std::fmt::Formatter<'y>,
} -> std::fmt::Result, |K, self| K::fmt(self.v, self.ctx, self.f));

semantic_visitor!(MatchesAny<'x, 'a> {
    v: CoercedAttrRef<'a>, filter: &'x dyn Fn(&str) -> buck2_error::Result<bool>,
} -> buck2_error::Result<bool>, |K, self| K::any_matches(self.v, self.filter));

semantic_visitor!(ConcatClass<> {} -> Option<&'static str>, |K, self| K::CONCAT);

semantic_visitor!(JsonKey<'x, 'a> {
    v: CoercedAttrRef<'a>, ctx: &'x AttrFmtContext,
} -> buck2_error::Result<Cow<'a, str>>, |K, self| K::json_key(self.v, self.ctx));

/// [`AttrSerializeWithContext`] for a value: generic over the serializer.
pub(super) struct SerializeKind<'x, 'a, S> {
    pub(super) v: CoercedAttrRef<'a>,
    pub(super) ctx: &'x AttrFmtContext,
    pub(super) s: S,
}

impl<S: Serializer> KindVisitor for SerializeKind<'_, '_, S> {
    type Out = Result<S::Ok, S::Error>;
    fn visit<K: Semantics + 'static>(self) -> Self::Out {
        K::serialize(self.v, self.ctx, self.s)
    }
}
