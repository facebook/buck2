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
use crate::attrs::arena::semantics::any_matches_seq;
use crate::attrs::arena::semantics::impl_semantics;
use crate::attrs::arena::semantics::inner_type;
use crate::attrs::arena::semantics::json_seq;
use crate::attrs::arena::semantics::ser_err;
use crate::attrs::arena::semantics::ser_keyed;
use crate::attrs::arena::semantics::ser_seq;
use crate::attrs::attr_type::AttrType;
use crate::attrs::attr_type::AttrTypeInner;
use crate::attrs::attr_type::configuration_dep::ConfigurationDepKind;
use crate::attrs::configuration_context::AttrConfigurationContext;
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

impl_semantics!(SelectFail {
    traverse(v, (m,), t, pkg, tr) { Ok(()) }
    configure(v, (m,), t, ctx, name) { select_fail(m) }
    traverse_configured(v, (m,), t, pkg, ctx, tr, name) { select_fail(m) }
    concat_leaves(v, (m,), ctx, out) { select_fail(m) }
    json(v, (m,), ctx) { select_fail_to_json(m) }
    fmt(v, (m,), ctx, f) { write!(f, "select_fail(\"{m}\")") }
    any_matches(v, (m,), filter) { Ok(false) }
    serialize<S>(v, (m,), ctx, ser) { select_fail_to_json(m).map_err(ser_err::<S>)?.serialize(ser) }
});

impl_semantics!(SelectIncompatible {
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

impl_semantics!(Selector {
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

impl_semantics!(Concat {
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
