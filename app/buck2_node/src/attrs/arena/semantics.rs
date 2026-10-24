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

use crate::attrs::arena::query_expr::QueryView;
use crate::attrs::arena::schema::Kind;
use crate::attrs::arena::schema::KindVisitor;
use crate::attrs::arena::schema::kinds;
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
/// the attribute type it belongs to, through [`impl_semantics!`]; a kind
/// without one does not compile.
pub(crate) trait Semantics: Kind {
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

/// The `attrs.query()` view of a `Query` value.
pub(crate) fn query_view(v: CoercedAttrRef<'_>) -> QueryView<'_> {
    QueryView {
        arena: v.arena,
        r: v.r,
    }
}

/// `impl Semantics` for one kind from its method bodies. Sections are in a
/// fixed order and each binds the decoded parts to its pattern; a section
/// the trait provides a default for may be omitted.
macro_rules! impl_semantics {
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
        impl $crate::attrs::arena::semantics::Semantics for $crate::attrs::arena::kinds::$kind {
            $( const CONCAT: Option<&'static str> = Some($concat); )?
            $(
                #[allow(unused_variables)]
                fn traverse<'a>(
                    $tv: $crate::attrs::arena::CoercedAttrRef<'a>,
                    $tt: &$crate::attrs::attr_type::AttrType,
                    $tpkg: Option<::buck2_core::package::PackageLabel>,
                    $ttr: &mut dyn $crate::attrs::traversal::CoercedAttrTraversal<'a>,
                ) -> ::buck2_error::Result<()> {
                    let $tp = Self::parts($tv);
                    $traverse
                }
            )?
            $(
                #[allow(unused_variables)]
                fn configure(
                    $cv: $crate::attrs::arena::CoercedAttrRef<'_>,
                    $ct: &$crate::attrs::attr_type::AttrType,
                    $cctx: &dyn $crate::attrs::configuration_context::AttrConfigurationContext,
                    $cname: Option<&str>,
                ) -> ::buck2_core::configuration::compatibility::ResultMaybeCompatible<$crate::attrs::configured_attr::ConfiguredAttr> {
                    let $cp = Self::parts($cv);
                    $configure
                }
            )?
            $(
                #[allow(unused_variables)]
                fn traverse_configured(
                    $xv: $crate::attrs::arena::CoercedAttrRef<'_>,
                    $xt: &$crate::attrs::attr_type::AttrType,
                    $xpkg: ::buck2_core::package::PackageLabel,
                    $xctx: &dyn $crate::attrs::configuration_context::AttrConfigurationContext,
                    $xtr: &mut dyn $crate::attrs::configured_traversal::ConfiguredAttrTraversal,
                    $xname: Option<&str>,
                ) -> ::buck2_core::configuration::compatibility::ResultMaybeCompatible<()> {
                    let $xp = Self::parts($xv);
                    $tc
                }
            )?
            $(
                #[allow(unused_variables)]
                fn concat_leaves<'a>(
                    $lv: $crate::attrs::arena::CoercedAttrRef<'a>,
                    $lctx: &dyn $crate::attrs::configuration_context::AttrConfigurationContext,
                    $lout: &mut ::smallvec::SmallVec<[$crate::attrs::arena::CoercedAttrRef<'a>; 4]>,
                ) -> ::buck2_core::configuration::compatibility::ResultMaybeCompatible<()> {
                    let $lp = Self::parts($lv);
                    $leaves
                }
            )?
            $(
                #[allow(unused_variables)]
                fn to_json(
                    $jv: $crate::attrs::arena::CoercedAttrRef<'_>,
                    $jctx: &$crate::attrs::fmt_context::AttrFmtContext,
                ) -> ::buck2_error::Result<::serde_json::Value> {
                    let $jp = Self::parts($jv);
                    $json
                }
            )?
            $(
                #[allow(unused_variables)]
                fn fmt(
                    $fv: $crate::attrs::arena::CoercedAttrRef<'_>,
                    $fctx: &$crate::attrs::fmt_context::AttrFmtContext,
                    $ff: &mut ::std::fmt::Formatter<'_>,
                ) -> ::std::fmt::Result {
                    let $fp = Self::parts($fv);
                    $fmt
                }
            )?
            $(
                #[allow(unused_variables)]
                fn any_matches(
                    $mv: $crate::attrs::arena::CoercedAttrRef<'_>,
                    $filter: &dyn Fn(&str) -> ::buck2_error::Result<bool>,
                ) -> ::buck2_error::Result<bool> {
                    let $mp = Self::parts($mv);
                    $matches
                }
            )?
            $(
                #[allow(unused_variables)]
                fn json_key<'a>(
                    $kv: $crate::attrs::arena::CoercedAttrRef<'a>,
                    $kctx: &$crate::attrs::fmt_context::AttrFmtContext,
                ) -> ::buck2_error::Result<::std::borrow::Cow<'a, str>> {
                    let $kp = Self::parts($kv);
                    $key
                }
            )?
            $(
                #[allow(unused_variables)]
                fn serialize<$S: ::serde::Serializer>(
                    $sv: $crate::attrs::arena::CoercedAttrRef<'_>,
                    $sctx: &$crate::attrs::fmt_context::AttrFmtContext,
                    $ss: $S,
                ) -> Result<$S::Ok, $S::Error> {
                    let $sp = Self::parts($sv);
                    $serialize
                }
            )?
        }
    };
}
pub(crate) use impl_semantics;

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
