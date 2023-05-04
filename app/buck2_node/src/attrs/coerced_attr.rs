/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashSet;
use std::fmt::Display;
use std::hash::Hash;

use allocative::Allocative;
use buck2_core::buck_path::path::BuckPathRef;
use buck2_core::configuration::config_setting::ConfigSettingData;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::package::PackageLabel;
use buck2_core::target::label::TargetLabel;
use buck2_util::arc_str::ArcSlice;
use dupe::Dupe;
use dupe::IterDupedExt;
use gazebo::prelude::SliceExt;
use itertools::Itertools;
use serde::Serialize;
use serde::Serializer;
use serde_json::to_value;
use starlark_map::StarlarkHasherBuilder;

use crate::attrs::attr_type::any_matches::AnyMatches;
use crate::attrs::attr_type::attr_config::CoercedAttrExtraTypes;
use crate::attrs::attr_type::bool::BoolLiteral;
use crate::attrs::attr_type::dict::DictLiteral;
use crate::attrs::attr_type::list::ListLiteral;
use crate::attrs::attr_type::string::StringLiteral;
use crate::attrs::attr_type::tuple::TupleLiteral;
use crate::attrs::configuration_context::AttrConfigurationContext;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::display::AttrDisplayWithContext;
use crate::attrs::display::AttrDisplayWithContextExt;
use crate::attrs::fmt_context::AttrFmtContext;
use crate::attrs::json::ToJsonWithContext;
use crate::attrs::serialize::AttrSerializeWithContext;
use crate::attrs::traversal::CoercedAttrTraversal;
use crate::visibility::VisibilitySpecification;

#[derive(thiserror::Error, Debug)]
enum SelectError {
    #[error("None of {} conditions matched configuration `{}` and no default was set:\n{}",
        .1.len(),
        .0,
        .1.iter().map(| s | format ! ("  {}", s)).join("\n"),
    )]
    MissingDefault(ConfigurationData, Vec<TargetLabel>),
    #[error(
        "Both select keys `{0}` and `{1}` match the configuration, but neither is more specific"
    )]
    TwoKeysDoNotRefineEachOther(String, String),
    #[error("concat with no items (internal error)")]
    ConcatEmpty,
    #[error("duplicate key `{0}` in `select()`")]
    DuplicateKey(String),
}

enum CoercedSelectorKeyRef<'a> {
    Target(&'a TargetLabel),
    Default,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Allocative)]
pub struct CoercedSelector {
    pub(crate) entries: ArcSlice<(TargetLabel, CoercedAttr)>,
    pub(crate) default: Option<CoercedAttr>,
}

impl CoercedSelector {
    pub fn new(
        entries: ArcSlice<(TargetLabel, CoercedAttr)>,
        default: Option<CoercedAttr>,
    ) -> anyhow::Result<CoercedSelector> {
        Self::check_all_keys_unique(&entries)?;
        Ok(CoercedSelector { entries, default })
    }

    fn check_all_keys_unique(entries: &[(TargetLabel, CoercedAttr)]) -> anyhow::Result<()> {
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
                        return Err(SelectError::DuplicateKey(entries[i].0.to_string()).into());
                    }
                }
            }
        } else {
            let mut visited_keys: HashSet<&TargetLabel, _> =
                HashSet::with_capacity_and_hasher(entries.len(), StarlarkHasherBuilder);
            for (k, _) in entries {
                if !visited_keys.insert(k) {
                    return Err(SelectError::DuplicateKey(k.to_string()).into());
                }
            }
        }

        Ok(())
    }

    fn all_entries(&self) -> impl Iterator<Item = (CoercedSelectorKeyRef, &CoercedAttr)> {
        self.entries
            .iter()
            .map(|(k, v)| (CoercedSelectorKeyRef::Target(k), v))
            .chain(
                self.default
                    .iter()
                    .map(|default| (CoercedSelectorKeyRef::Default, default)),
            )
    }

    fn all_values(&self) -> impl Iterator<Item = &'_ CoercedAttr> {
        self.all_entries().map(|(_, v)| v)
    }
}

/// CoercedAttr is the "coerced" representation of an attribute. It has been type-checked and converted to
/// specific types (for example, where we expect target-like things, it has been converted to something like
/// a TargetLabel or ProvidersLabel).
///
/// CoercedAttr  provides support for the `select()` function. All coerced data is
/// potentially represented by a select that represents possibly different
/// configured representations.
///
/// CoercedData::Concat supports a representation for when a selectable is added
/// to something. Not all types support this case and those will return an error
/// during coercion and not ever use the ::Concat case.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Allocative)]
pub enum CoercedAttr {
    Selector(Box<CoercedSelector>),
    Concat(Box<[Self]>),

    Bool(BoolLiteral),
    Int(i32),
    // Note we store `String`, not `Arc<str>` here, because we store full attributes
    // in unconfigured target node, but configured target node is basically a pair
    // (reference to unconfigured target node, configuration).
    //
    // Configured attributes are created on demand and destroyed immediately after use.
    //
    // So when working with configured attributes with pay with CPU for string copies,
    // but don't increase total memory usage, because these string copies are short living.
    String(StringLiteral),
    // Like String, but drawn from a set of variants, so doesn't support concat
    EnumVariant(StringLiteral),
    List(ListLiteral<CoercedAttr>),
    Tuple(TupleLiteral<CoercedAttr>),
    Dict(DictLiteral<CoercedAttr>),
    None,
    // NOTE: unlike deps, labels are not traversed, as they are typically used in lieu of deps in
    // cases that would cause cycles.
    OneOf(
        Box<Self>,
        // Index of matched oneof attr type variant.
        u32,
    ),
    Visibility(VisibilitySpecification),
    Extra(CoercedAttrExtraTypes),
}

// This is just to help understand any impact that changes have to the size of this.
// We store a lot of these, so we try to keep it to a reasonable size.
static_assertions::assert_eq_size!(CoercedAttr, [usize; 3]);

/// Provides roughly the stringified version of the starlark code that would produce this attr. For example, a dictionary
/// of string keys and values may result in `{"key1":"value1","key2":"value2"}` (note that strings will explicitly include
/// the wrapping `"`).
impl AttrDisplayWithContext for CoercedAttr {
    fn fmt(&self, ctx: &AttrFmtContext, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CoercedAttr::Selector(s) => {
                write!(f, "select(")?;
                for (i, (key, value)) in s.all_entries().enumerate() {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    match key {
                        CoercedSelectorKeyRef::Target(k) => {
                            write!(f, "\"{}\"={}", k, value.as_display(ctx))?;
                        }
                        CoercedSelectorKeyRef::Default => {
                            write!(f, "\"DEFAULT\"={}", value.as_display(ctx))?;
                        }
                    }
                }
                write!(f, ")")?;
                Ok(())
            }
            CoercedAttr::Concat(items) => {
                write!(f, "{}", items.iter().map(|a| a.as_display(ctx)).format("+"))
            }
            CoercedAttr::Bool(v) => {
                write!(f, "{}", v)
            }
            CoercedAttr::Int(v) => {
                write!(f, "{}", v)
            }
            CoercedAttr::String(v) | CoercedAttr::EnumVariant(v) => Display::fmt(v, f),
            CoercedAttr::List(list) => AttrDisplayWithContext::fmt(list, ctx, f),
            CoercedAttr::Tuple(v) => AttrDisplayWithContext::fmt(v, ctx, f),
            CoercedAttr::Dict(v) => AttrDisplayWithContext::fmt(v, ctx, f),
            CoercedAttr::None => write!(f, "None"),
            CoercedAttr::OneOf(box l, _) => AttrDisplayWithContext::fmt(l, ctx, f),
            CoercedAttr::Visibility(v) => Display::fmt(v, f),
            CoercedAttr::Extra(u) => AttrDisplayWithContext::fmt(u, ctx, f),
        }
    }
}

impl AttrSerializeWithContext for CoercedAttr {
    fn serialize_with_ctx<S>(&self, ctx: &AttrFmtContext, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        // TODO this is inefficient. We should impl Serialize and derive value from this instead.
        self.to_json(ctx)
            .map_err(|e| serde::ser::Error::custom(format!("{}", e)))?
            .serialize(s)
    }
}

impl CoercedAttr {
    /// Converts the coerced attr to a serde_json Value. This is generally just used for debugging or introspective
    /// things, a lot of the types will be dropped without special handling. For example, an artifact will just end
    /// up as the stringified version of its coerced value (i.e. while `//a:b` might represent some list of targets,
    /// in to_json it just appears as the string "//a:b").
    pub fn to_json(&self, ctx: &AttrFmtContext) -> anyhow::Result<serde_json::Value> {
        match self {
            CoercedAttr::Selector(s) => {
                let mut map = serde_json::Map::new();
                for (key, value) in s.all_entries() {
                    match key {
                        CoercedSelectorKeyRef::Target(k) => {
                            map.insert(k.to_string(), value.to_json(ctx)?);
                        }
                        CoercedSelectorKeyRef::Default => {
                            map.insert("DEFAULT".to_owned(), value.to_json(ctx)?);
                        }
                    }
                }
                let select = serde_json::Value::Object(map);

                Ok(serde_json::Value::Object(serde_json::Map::from_iter([
                    (
                        "__type".to_owned(),
                        serde_json::Value::String("selector".to_owned()),
                    ),
                    ("entries".to_owned(), select),
                ])))
            }
            CoercedAttr::Concat(items) => {
                Ok(serde_json::Value::Object(serde_json::Map::from_iter([
                    (
                        "__type".to_owned(),
                        serde_json::Value::String("concat".to_owned()),
                    ),
                    (
                        "items".to_owned(),
                        serde_json::Value::Array(
                            items.try_map(|item| CoercedAttr::to_json(item, ctx))?,
                        ),
                    ),
                ])))
            }
            CoercedAttr::Bool(v) => Ok(to_value(v)?),
            CoercedAttr::Int(v) => Ok(to_value(v)?),
            CoercedAttr::String(v) | CoercedAttr::EnumVariant(v) => Ok(to_value(v)?),
            CoercedAttr::List(list) => list.to_json(ctx),
            CoercedAttr::Tuple(list) => list.to_json(ctx),
            CoercedAttr::Dict(dict) => dict.to_json(ctx),
            CoercedAttr::None => Ok(serde_json::Value::Null),
            CoercedAttr::OneOf(box l, _) => l.to_json(ctx),
            CoercedAttr::Visibility(v) => Ok(v.to_json()),
            CoercedAttr::Extra(u) => u.to_json(ctx),
        }
    }

    /// Returns `true` if the result of evaluating this literal can ever be None.
    pub fn may_return_none(&self) -> bool {
        match self {
            CoercedAttr::None => true,
            CoercedAttr::Selector(s) => s.all_values().any(|x| x.may_return_none()),
            CoercedAttr::Concat(_) => false,
            _ => false,
        }
    }

    /// Traverses the coerced attribute and provides the traverser callbacks for all deps (those in select conditions
    /// are passed as configuration deps).
    pub fn traverse<'a>(
        &'a self,
        pkg: PackageLabel,
        traversal: &mut dyn CoercedAttrTraversal<'a>,
    ) -> anyhow::Result<()> {
        match self {
            CoercedAttr::Selector(box CoercedSelector { entries, default }) => {
                for (condition, value) in entries.iter() {
                    traversal.configuration_dep(condition)?;
                    value.traverse(pkg.dupe(), traversal)?;
                }
                if let Some(v) = default {
                    v.traverse(pkg.dupe(), traversal)?;
                }
                Ok(())
            }
            CoercedAttr::Concat(items) => {
                for item in &**items {
                    item.traverse(pkg.dupe(), traversal)?;
                }
                Ok(())
            }
            CoercedAttr::Bool(_) => Ok(()),
            CoercedAttr::Int(_) => Ok(()),
            CoercedAttr::String(_) => Ok(()),
            CoercedAttr::EnumVariant(_) => Ok(()),
            CoercedAttr::List(list) => {
                for v in list.iter() {
                    v.traverse(pkg.dupe(), traversal)?;
                }
                Ok(())
            }
            CoercedAttr::Tuple(list) => {
                for v in list.iter() {
                    v.traverse(pkg.dupe(), traversal)?;
                }
                Ok(())
            }
            CoercedAttr::Dict(dict) => {
                for (k, v) in dict.iter() {
                    k.traverse(pkg.dupe(), traversal)?;
                    v.traverse(pkg.dupe(), traversal)?;
                }
                Ok(())
            }
            CoercedAttr::None => Ok(()),
            CoercedAttr::OneOf(box l, _) => l.traverse(pkg, traversal),
            CoercedAttr::Visibility(..) => Ok(()),
            CoercedAttr::Extra(u) => match u {
                CoercedAttrExtraTypes::ExplicitConfiguredDep(dep) => dep.traverse(traversal),
                CoercedAttrExtraTypes::SplitTransitionDep(dep) => {
                    traversal.split_transition_dep(dep.label.target(), &dep.transition)
                }
                CoercedAttrExtraTypes::ConfiguredDep(dep) => {
                    traversal.dep(dep.label.target().unconfigured())
                }
                CoercedAttrExtraTypes::ConfigurationDep(dep) => traversal.configuration_dep(dep),
                CoercedAttrExtraTypes::Dep(dep) => dep.traverse(traversal),
                CoercedAttrExtraTypes::SourceLabel(s) => traversal.dep(s.target()),
                CoercedAttrExtraTypes::Label(label) => traversal.label(label),
                CoercedAttrExtraTypes::Arg(arg) => arg.traverse(traversal),
                CoercedAttrExtraTypes::Query(query) => query.traverse(traversal),
                CoercedAttrExtraTypes::SourceFile(source) => {
                    for x in source.inputs() {
                        traversal.input(BuckPathRef::new(pkg.dupe(), x))?;
                    }
                    Ok(())
                }
            },
        }
    }

    /// If more than one select key matches, select the most specific.
    pub fn select_the_most_specific<'a>(
        ctx: &dyn AttrConfigurationContext,
        select_entries: &'a [(TargetLabel, CoercedAttr)],
    ) -> anyhow::Result<Option<&'a CoercedAttr>> {
        let mut matching: Option<(&TargetLabel, &ConfigSettingData, &CoercedAttr)> = None;
        for (k, v) in select_entries {
            matching = match (ctx.matches(k), matching) {
                (None, matching) => matching,
                (Some(conf), None) => Some((k, conf, v)),
                (Some(conf), Some((prev_k, prev_conf, prev_v))) => {
                    if conf.refines(prev_conf) {
                        Some((k, conf, v))
                    } else if prev_conf.refines(conf) {
                        Some((prev_k, prev_conf, prev_v))
                    } else {
                        return Err(SelectError::TwoKeysDoNotRefineEachOther(
                            prev_k.to_string(),
                            k.to_string(),
                        )
                        .into());
                    }
                }
            }
        }
        Ok(matching.map(|(_k, _conf, v)| v))
    }

    /// Returns the "configured" representation of the attribute in the provided context.
    /// This handles the resolution of the select() conditions and delegates to
    /// the actual attr type for handling any appropriate configuration-time
    /// processing.
    pub fn configure(&self, ctx: &dyn AttrConfigurationContext) -> anyhow::Result<ConfiguredAttr> {
        Ok(match self {
            CoercedAttr::Selector(box CoercedSelector { entries, default }) => {
                if let Some(v) = Self::select_the_most_specific(ctx, entries)? {
                    return v.configure(ctx);
                }
                default
                    .as_ref()
                    .ok_or_else(|| {
                        SelectError::MissingDefault(
                            ctx.cfg().cfg().dupe(),
                            entries.iter().map(|(k, _)| k).duped().collect(),
                        )
                    })?
                    .configure(ctx)?
            }
            CoercedAttr::Concat(items) => {
                let singleton = items.len() == 1;
                let mut it = items.iter().map(|item| item.configure(ctx));
                let first = it.next().ok_or(SelectError::ConcatEmpty)??;
                if singleton {
                    first
                } else {
                    first.concat(&mut it)?
                }
            }
            CoercedAttr::Bool(v) => ConfiguredAttr::Bool(*v),
            CoercedAttr::Int(v) => ConfiguredAttr::Int(*v),
            CoercedAttr::String(v) => ConfiguredAttr::String(v.dupe()),
            CoercedAttr::EnumVariant(v) => ConfiguredAttr::EnumVariant(v.dupe()),
            CoercedAttr::List(list) => {
                ConfiguredAttr::List(ListLiteral(list.try_map(|v| v.configure(ctx))?.into()))
            }
            CoercedAttr::Tuple(list) => {
                ConfiguredAttr::Tuple(TupleLiteral(list.try_map(|v| v.configure(ctx))?.into()))
            }
            CoercedAttr::Dict(dict) => ConfiguredAttr::Dict(DictLiteral(
                dict.try_map(|(k, v)| {
                    let k2 = k.configure(ctx)?;
                    let v2 = v.configure(ctx)?;
                    anyhow::Ok((k2, v2))
                })?
                .into(),
            )),
            CoercedAttr::None => ConfiguredAttr::None,
            CoercedAttr::OneOf(l, i) => {
                let configured = l.configure(ctx)?;
                ConfiguredAttr::OneOf(Box::new(configured), *i)
            }
            CoercedAttr::Visibility(v) => ConfiguredAttr::Visibility(v.clone()),
            CoercedAttr::Extra(u) => u.configure(ctx)?,
        })
    }

    /// Checks if this attr matches the filter. For selectors and container-like things, will return true if any
    /// contained item matches the filter.
    pub fn any_matches(
        &self,
        filter: &dyn Fn(&str) -> anyhow::Result<bool>,
    ) -> anyhow::Result<bool> {
        match self {
            CoercedAttr::Selector(s) => {
                for value in s.all_values() {
                    if value.any_matches(filter)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            CoercedAttr::Concat(items) => {
                for item in &**items {
                    if item.any_matches(filter)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            CoercedAttr::String(v) | CoercedAttr::EnumVariant(v) => filter(v),
            CoercedAttr::List(vals) => vals.any_matches(filter),
            CoercedAttr::Tuple(vals) => vals.any_matches(filter),
            CoercedAttr::Dict(d) => d.any_matches(filter),
            CoercedAttr::None => Ok(false),
            CoercedAttr::Bool(b) => b.any_matches(filter),
            CoercedAttr::Int(i) => filter(&i.to_string()),
            CoercedAttr::OneOf(l, _) => l.any_matches(filter),
            CoercedAttr::Visibility(v) => v.any_matches(filter),
            CoercedAttr::Extra(d) => d.any_matches(filter),
        }
    }
}

#[cfg(test)]
mod tests {

    use buck2_core::target::label::TargetLabel;
    use dupe::Dupe;

    use crate::attrs::coerced_attr::CoercedAttr;
    use crate::attrs::coerced_attr::CoercedSelector;

    #[test]
    fn test_check_all_keys_unique_small() {
        let a = TargetLabel::testing_parse("foo//:a");
        let b = TargetLabel::testing_parse("foo//:b");
        let c = TargetLabel::testing_parse("foo//:c");
        let attr = CoercedAttr::None;
        let a = (a.dupe(), attr.clone());
        let b = (b.dupe(), attr.clone());
        let c = (c.dupe(), attr);
        assert!(
            CoercedSelector::check_all_keys_unique(&[a.clone(), b.clone(), a.clone()]).is_err()
        );
        assert!(
            CoercedSelector::check_all_keys_unique(&[a.clone(), b.clone(), b.clone()]).is_err()
        );
        assert!(
            CoercedSelector::check_all_keys_unique(&[a.clone(), a.clone(), b.clone()]).is_err()
        );
        assert!(CoercedSelector::check_all_keys_unique(&[a, b, c]).is_ok());
    }

    #[test]
    fn test_check_all_keys_unique_large() {
        let attr = CoercedAttr::None;
        let mut long = (0..100)
            .map(|i| {
                (
                    TargetLabel::testing_parse(&format!("foo//:{}", i)),
                    attr.clone(),
                )
            })
            .collect::<Vec<_>>();
        assert!(CoercedSelector::check_all_keys_unique(&long).is_ok());
        long[10].0 = long[0].0.dupe();
        assert!(CoercedSelector::check_all_keys_unique(&long).is_err());
    }
}
