/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use core::fmt;
use std::borrow::Cow;
use std::fmt::Display;

use buck2_core::configuration::compatibility::IncompatiblePlatformReasonCause;
use buck2_core::configuration::compatibility::ResultMaybeCompatible;
use buck2_error::buck2_error;
use buck2_error::internal_error;
use buck2_hash::BuckHasherBuilder;
use buck2_hash::BuckMutSet;
use buck2_util::arc_str::ArcStr;
use display_container::fmt_keyed_container;
use itertools::Itertools;
use serde::Serialize;
use serde::Serializer;
use serde::ser::SerializeMap;
use smallvec::SmallVec;

use crate::attrs::arena::AttrRefDisplay;
use crate::attrs::arena::AttrRefSelectorEntries;
use crate::attrs::arena::CoercedAttrRef;
use crate::attrs::arena::Refs;
use crate::attrs::arena::Values;
use crate::attrs::arena::kinds;
use crate::attrs::arena::semantics::ConcatLeaves;
use crate::attrs::arena::semantics::Configure;
use crate::attrs::arena::semantics::Fmt;
use crate::attrs::arena::semantics::MatchesAny;
use crate::attrs::arena::semantics::Semantics;
use crate::attrs::arena::semantics::SerializeKind;
use crate::attrs::arena::semantics::ToJson;
use crate::attrs::arena::semantics::Traverse;
use crate::attrs::arena::semantics::TraverseConfigured;
use crate::attrs::arena::semantics::any_matches_seq;
use crate::attrs::arena::semantics::inner_type;
use crate::attrs::arena::semantics::json_seq;
use crate::attrs::arena::semantics::ser_err;
use crate::attrs::arena::semantics::ser_keyed;
use crate::attrs::arena::semantics::ser_seq;
use crate::attrs::attr_type::AttrType;
use crate::attrs::attr_type::AttrTypeInner;
use crate::attrs::attr_type::configuration_dep::ConfigurationDepKind;
use crate::attrs::configuration_context::AttrConfigurationContext;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::display::AttrDisplayWithContextExt;
use crate::attrs::fmt_context::AttrFmtContext;
use crate::configuration::resolved::ConfigurationSettingKey;

#[derive(Debug, Eq, PartialEq)]
pub enum CoercedSelectorKeyRef<'a> {
    Target(&'a ConfigurationSettingKey),
    Default,
}

impl CoercedSelectorKeyRef<'_> {
    pub const DEFAULT_KEY_STR: &'static str = "DEFAULT";
}

impl fmt::Display for CoercedSelectorKeyRef<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CoercedSelectorKeyRef::Target(target) => write!(f, "{}", target.0),
            CoercedSelectorKeyRef::Default => write!(f, "{}", Self::DEFAULT_KEY_STR),
        }
    }
}

/// Rejects duplicate keys in a `select()`, including keys that differ only in
/// how they spell the same label.
pub(crate) fn check_select_keys_unique<V>(
    entries: &[(ConfigurationSettingKey, V)],
) -> buck2_error::Result<()> {
    fn duplicate_key(key: &ConfigurationSettingKey) -> buck2_error::Error {
        buck2_error!(
            buck2_error::ErrorTag::Input,
            "duplicate key `{key}` in `select()`"
        )
    }

    // This is possible when select keys are specified like:
    // ```
    // select({
    //   "cell//foo:bar": 2,
    //   "//foo:bar": 1,
    //   ":bar": 3,
    // })
    // ```
    // Keys are unique strings, but resolved to the same target.

    // Quadratic is cheaper than hashing for small `N`.
    // For 32 entries we do 496 comparisons, which is cheaper than 32 hashing operations.
    if entries.len() <= 32 {
        for i in 0..entries.len() {
            for j in i + 1..entries.len() {
                if entries[i].0 == entries[j].0 {
                    return Err(duplicate_key(&entries[i].0));
                }
            }
        }
    } else {
        let mut visited_keys: BuckMutSet<&ConfigurationSettingKey> =
            BuckMutSet::with_capacity_and_hasher(entries.len(), BuckHasherBuilder);
        for (k, _) in entries {
            if !visited_keys.insert(k) {
                return Err(duplicate_key(k));
            }
        }
    }

    Ok(())
}

pub(crate) fn select_fail_to_json(message: &str) -> Result<serde_json::Value, buck2_error::Error> {
    Ok(serde_json::Value::Object(serde_json::Map::from_iter([
        (
            "__type".to_owned(),
            serde_json::Value::String("select_fail".to_owned()),
        ),
        (
            "message".to_owned(),
            serde_json::Value::String(message.to_owned()),
        ),
    ])))
}

pub(crate) fn select_incompatible_to_json(
    message: &str,
) -> Result<serde_json::Value, buck2_error::Error> {
    Ok(serde_json::Value::Object(serde_json::Map::from_iter([
        (
            "__type".to_owned(),
            serde_json::Value::String("select_incompatible".to_owned()),
        ),
        (
            "message".to_owned(),
            serde_json::Value::String(message.to_owned()),
        ),
    ])))
}

impl Semantics for kinds::SelectFail {
    fn traverse<'a>(_: Self::Parts<'a>, _: Traverse<'_, 'a>) -> buck2_error::Result<()> {
        Ok(())
    }

    fn configure(
        (m,): Self::Parts<'_>,
        _: Configure<'_, '_>,
    ) -> ResultMaybeCompatible<ConfiguredAttr> {
        select_fail(m)
    }

    fn traverse_configured(
        (m,): Self::Parts<'_>,
        _: TraverseConfigured<'_, '_>,
    ) -> ResultMaybeCompatible<()> {
        select_fail(m)
    }

    fn concat_leaves<'a>(
        (m,): Self::Parts<'a>,
        _: ConcatLeaves<'_, 'a>,
    ) -> ResultMaybeCompatible<()> {
        select_fail(m)
    }

    fn to_json((m,): Self::Parts<'_>, _: ToJson<'_, '_>) -> buck2_error::Result<serde_json::Value> {
        select_fail_to_json(m)
    }

    fn fmt((m,): Self::Parts<'_>, Fmt { f, .. }: Fmt<'_, '_, '_>) -> std::fmt::Result {
        write!(f, "select_fail(\"{m}\")")
    }

    fn any_matches(_: Self::Parts<'_>, _: MatchesAny<'_, '_>) -> buck2_error::Result<bool> {
        Ok(false)
    }

    fn serialize<S: Serializer>(
        (m,): Self::Parts<'_>,
        SerializeKind { ser, .. }: SerializeKind<'_, '_, S>,
    ) -> Result<S::Ok, S::Error> {
        select_fail_to_json(m).map_err(ser_err::<S>)?.serialize(ser)
    }
}

impl Semantics for kinds::SelectIncompatible {
    fn traverse<'a>(_: Self::Parts<'a>, _: Traverse<'_, 'a>) -> buck2_error::Result<()> {
        Ok(())
    }

    fn configure(
        (m,): Self::Parts<'_>,
        Configure { ctx, .. }: Configure<'_, '_>,
    ) -> ResultMaybeCompatible<ConfiguredAttr> {
        select_incompatible(m, ctx)
    }

    fn traverse_configured(
        (m,): Self::Parts<'_>,
        TraverseConfigured { ctx, .. }: TraverseConfigured<'_, '_>,
    ) -> ResultMaybeCompatible<()> {
        select_incompatible(m, ctx)
    }

    fn concat_leaves<'a>(
        (m,): Self::Parts<'a>,
        ConcatLeaves { ctx, .. }: ConcatLeaves<'_, 'a>,
    ) -> ResultMaybeCompatible<()> {
        select_incompatible(m, ctx)
    }

    fn to_json((m,): Self::Parts<'_>, _: ToJson<'_, '_>) -> buck2_error::Result<serde_json::Value> {
        select_incompatible_to_json(m)
    }

    fn fmt((m,): Self::Parts<'_>, Fmt { f, .. }: Fmt<'_, '_, '_>) -> std::fmt::Result {
        write!(f, "select_incompatible(\"{m}\")")
    }

    fn any_matches(_: Self::Parts<'_>, _: MatchesAny<'_, '_>) -> buck2_error::Result<bool> {
        Ok(false)
    }

    fn serialize<S: Serializer>(
        (m,): Self::Parts<'_>,
        SerializeKind { ser, .. }: SerializeKind<'_, '_, S>,
    ) -> Result<S::Ok, S::Error> {
        select_incompatible_to_json(m)
            .map_err(ser_err::<S>)?
            .serialize(ser)
    }
}

fn select_fail<T>(message: &str) -> ResultMaybeCompatible<T> {
    buck2_error!(
        buck2_error::ErrorTag::Input,
        "select resolved to select_fail(): {message}"
    )
    .into()
}

fn select_incompatible<T>(
    message: &str,
    ctx: &dyn AttrConfigurationContext,
) -> ResultMaybeCompatible<T> {
    ResultMaybeCompatible::Incompatible(ctx.incompatible_platform_reason(
        IncompatiblePlatformReasonCause::SelectIncompatible(ArcStr::from(message)),
    ))
}

impl Semantics for kinds::Selector {
    fn traverse<'a>(
        (entries, default): Self::Parts<'a>,
        Traverse { t, pkg, tr, .. }: Traverse<'_, 'a>,
    ) -> buck2_error::Result<()> {
        for (key, val) in entries.iter() {
            tr.configuration_dep(key, ConfigurationDepKind::SelectKey)?;
            val.traverse(t, pkg, tr)?;
        }
        if let Some(d) = default {
            d.traverse(t, pkg, tr)?;
        }
        Ok(())
    }

    fn configure(
        (entries, default): Self::Parts<'_>,
        Configure { v, t, ctx, name }: Configure<'_, '_>,
    ) -> ResultMaybeCompatible<ConfiguredAttr> {
        match CoercedAttrRef::select_view(ctx, v, entries, default, name)? {
            Some(selected) => selected.configure(t, ctx, name),
            None => CoercedAttrRef::select_no_match_error(ctx, entries).into(),
        }
    }

    fn traverse_configured(
        (entries, default): Self::Parts<'_>,
        TraverseConfigured {
            v,
            t,
            pkg,
            ctx,
            tr,
            name,
        }: TraverseConfigured<'_, '_>,
    ) -> ResultMaybeCompatible<()> {
        match CoercedAttrRef::select_view(ctx, v, entries, default, name) {
            Ok(Some(selected)) => selected.traverse_configured(t, pkg, ctx, tr, name),
            Ok(None) => CoercedAttrRef::select_no_match_error(ctx, entries).into(),
            Err(e) => e.into(),
        }
    }

    fn concat_leaves<'a>(
        (entries, default): Self::Parts<'a>,
        ConcatLeaves { v, ctx, out }: ConcatLeaves<'_, 'a>,
    ) -> ResultMaybeCompatible<()> {
        match CoercedAttrRef::select_view(ctx, v, entries, default, None) {
            Ok(Some(selected)) => selected.concat_leaves(ctx, out),
            Ok(None) => CoercedAttrRef::select_no_match_error(ctx, entries).into(),
            Err(e) => e.into(),
        }
    }

    fn to_json(
        (entries, default): Self::Parts<'_>,
        ToJson { ctx, .. }: ToJson<'_, '_>,
    ) -> buck2_error::Result<serde_json::Value> {
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

    fn fmt(
        (entries, default): Self::Parts<'_>,
        Fmt { ctx, f, .. }: Fmt<'_, '_, '_>,
    ) -> std::fmt::Result {
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

    fn any_matches(
        (entries, default): Self::Parts<'_>,
        MatchesAny { filter, .. }: MatchesAny<'_, '_>,
    ) -> buck2_error::Result<bool> {
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

    fn serialize<S: Serializer>(
        (entries, default): Self::Parts<'_>,
        SerializeKind { ctx, ser, .. }: SerializeKind<'_, '_, S>,
    ) -> Result<S::Ok, S::Error> {
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
        map.serialize_entry(
            "entries",
            &Entries {
                entries,
                default,
                ctx,
            },
        )?;
        map.end()
    }
}

impl Semantics for kinds::Concat {
    fn traverse<'a>(
        (items,): Self::Parts<'a>,
        Traverse { t, pkg, tr, .. }: Traverse<'_, 'a>,
    ) -> buck2_error::Result<()> {
        for item in items.iter() {
            item.traverse(t, pkg, tr)?;
        }
        Ok(())
    }

    fn configure(
        (items,): Self::Parts<'_>,
        Configure { t, ctx, name, .. }: Configure<'_, '_>,
    ) -> ResultMaybeCompatible<ConfiguredAttr> {
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

    fn traverse_configured(
        (items,): Self::Parts<'_>,
        TraverseConfigured {
            t,
            pkg,
            ctx,
            tr,
            name,
            ..
        }: TraverseConfigured<'_, '_>,
    ) -> ResultMaybeCompatible<()> {
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

    fn concat_leaves<'a>(
        (items,): Self::Parts<'a>,
        ConcatLeaves { ctx, out, .. }: ConcatLeaves<'_, 'a>,
    ) -> ResultMaybeCompatible<()> {
        for item in items.iter() {
            item.concat_leaves(ctx, out)?;
        }
        ResultMaybeCompatible::Compatible(())
    }

    fn to_json(
        (items,): Self::Parts<'_>,
        ToJson { ctx, .. }: ToJson<'_, '_>,
    ) -> buck2_error::Result<serde_json::Value> {
        Ok(serde_json::Value::Object(serde_json::Map::from_iter([
            (
                "__type".to_owned(),
                serde_json::Value::String("concat".to_owned()),
            ),
            ("items".to_owned(), json_seq(items, ctx)?),
        ])))
    }

    fn fmt((items,): Self::Parts<'_>, Fmt { ctx, f, .. }: Fmt<'_, '_, '_>) -> std::fmt::Result {
        items
            .iter()
            .map(|a| AttrRefDisplay { ctx, r: a })
            .format("+")
            .fmt(f)
    }

    fn any_matches(
        (items,): Self::Parts<'_>,
        MatchesAny { filter, .. }: MatchesAny<'_, '_>,
    ) -> buck2_error::Result<bool> {
        any_matches_seq(items, filter)
    }

    fn serialize<S: Serializer>(
        (items,): Self::Parts<'_>,
        SerializeKind { ctx, ser, .. }: SerializeKind<'_, '_, S>,
    ) -> Result<S::Ok, S::Error> {
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
}

/// The checks `ConfiguredAttr::concat` makes on its items, applied to the
/// resolved leaves: one concat class throughout, equal oneof variants, no
/// dict key on both sides.
fn validate_concat<'a>(t: &AttrType, leaves: &[CoercedAttrRef<'a>]) -> buck2_error::Result<()> {
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
                if seen
                    .iter()
                    .any(|s| s.word() == k.word() || s.content_eq(&k))
                {
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

#[cfg(test)]
mod tests {
    use dupe::Dupe;

    use crate::attrs::coerced_attr::check_select_keys_unique;
    use crate::configuration::resolved::ConfigurationSettingKey;

    #[test]
    fn test_check_select_keys_unique_small() {
        let a = (ConfigurationSettingKey::testing_parse("foo//:a"), ());
        let b = (ConfigurationSettingKey::testing_parse("foo//:b"), ());
        let c = (ConfigurationSettingKey::testing_parse("foo//:c"), ());
        assert!(check_select_keys_unique(&[a.dupe(), b.dupe(), a.dupe()]).is_err());
        assert!(check_select_keys_unique(&[a.dupe(), b.dupe(), b.dupe()]).is_err());
        assert!(check_select_keys_unique(&[a.dupe(), a.dupe(), b.dupe()]).is_err());
        assert!(check_select_keys_unique(&[a, b, c]).is_ok());
    }

    #[test]
    fn test_check_select_keys_unique_large() {
        let mut long = (0..100)
            .map(|i| {
                (
                    ConfigurationSettingKey::testing_parse(&format!("foo//:{i}")),
                    (),
                )
            })
            .collect::<Vec<_>>();
        assert!(check_select_keys_unique(&long).is_ok());
        long[10].0 = long[0].0.dupe();
        assert!(check_select_keys_unique(&long).is_err());
    }
}
