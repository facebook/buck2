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
use anyhow::Context;
use buck2_core::buck_path::path::BuckPathRef;
use buck2_core::configuration::config_setting::ConfigSettingData;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::package::PackageLabel;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
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
use crate::attrs::attr_type::arg::StringWithMacros;
use crate::attrs::attr_type::attr_config::source_file_display;
use crate::attrs::attr_type::bool::BoolLiteral;
use crate::attrs::attr_type::configuration_dep::ConfigurationDepAttrType;
use crate::attrs::attr_type::configured_dep::ExplicitConfiguredDepAttrType;
use crate::attrs::attr_type::configured_dep::UnconfiguredExplicitConfiguredDep;
use crate::attrs::attr_type::dep::DepAttr;
use crate::attrs::attr_type::dep::DepAttrType;
use crate::attrs::attr_type::dep::ExplicitConfiguredDepMaybeConfigured;
use crate::attrs::attr_type::dict::DictLiteral;
use crate::attrs::attr_type::label::LabelAttrType;
use crate::attrs::attr_type::list::ListLiteral;
use crate::attrs::attr_type::query::QueryAttr;
use crate::attrs::attr_type::split_transition_dep::SplitTransitionDep;
use crate::attrs::attr_type::split_transition_dep::SplitTransitionDepAttrType;
use crate::attrs::attr_type::split_transition_dep::SplitTransitionDepMaybeConfigured;
use crate::attrs::attr_type::string::StringLiteral;
use crate::attrs::attr_type::tuple::TupleLiteral;
use crate::attrs::attr_type::AttrType;
use crate::attrs::coerced_attr_with_type::CoercedAttrWithType;
use crate::attrs::coerced_path::CoercedPath;
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

#[derive(Debug, thiserror::Error)]
enum CoercedAttrError {
    #[error("Inconsistent number of elements in tuple")]
    InconsistentTupleLength,
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
    ExplicitConfiguredDep(Box<UnconfiguredExplicitConfiguredDep>),
    SplitTransitionDep(Box<SplitTransitionDep>),
    ConfiguredDep(Box<DepAttr<ConfiguredProvidersLabel>>),
    ConfigurationDep(TargetLabel),
    Dep(Box<DepAttr<ProvidersLabel>>),
    SourceLabel(ProvidersLabel),
    // NOTE: unlike deps, labels are not traversed, as they are typically used in lieu of deps in
    // cases that would cause cycles.
    Label(ProvidersLabel),
    Arg(StringWithMacros<ProvidersLabel>),
    Query(Box<QueryAttr<ProvidersLabel>>),
    SourceFile(CoercedPath),
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
            CoercedAttr::ExplicitConfiguredDep(e) => Display::fmt(e, f),
            CoercedAttr::SplitTransitionDep(e) => Display::fmt(e, f),
            CoercedAttr::ConfiguredDep(e) => write!(f, "\"{}\"", e),
            CoercedAttr::ConfigurationDep(e) => write!(f, "\"{}\"", e),
            CoercedAttr::Dep(e) => write!(f, "\"{}\"", e),
            CoercedAttr::SourceLabel(e) => write!(f, "\"{}\"", e),
            CoercedAttr::Label(e) => write!(f, "\"{}\"", e),
            CoercedAttr::Arg(e) => write!(f, "\"{}\"", e),
            CoercedAttr::Query(e) => write!(f, "\"{}\"", e.query()),
            CoercedAttr::SourceFile(e) => write!(f, "\"{}\"", source_file_display(ctx, e)),
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
            CoercedAttr::ExplicitConfiguredDep(e) => e.to_json(),
            CoercedAttr::SplitTransitionDep(e) => e.to_json(),
            CoercedAttr::ConfiguredDep(e) => Ok(to_value(e.to_string())?),
            CoercedAttr::ConfigurationDep(e) => Ok(to_value(e.to_string())?),
            CoercedAttr::Dep(e) => Ok(to_value(e.to_string())?),
            CoercedAttr::SourceLabel(e) => Ok(to_value(e.to_string())?),
            CoercedAttr::Label(e) => Ok(to_value(e.to_string())?),
            CoercedAttr::Arg(e) => Ok(to_value(e.to_string())?),
            CoercedAttr::Query(e) => Ok(to_value(e.query())?),
            CoercedAttr::SourceFile(e) => Ok(to_value(source_file_display(ctx, e).to_string())?),
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
        t: &AttrType,
        pkg: PackageLabel,
        traversal: &mut dyn CoercedAttrTraversal<'a>,
    ) -> anyhow::Result<()> {
        match CoercedAttrWithType::pack(self, t)? {
            CoercedAttrWithType::Selector(CoercedSelector { entries, default }, t) => {
                for (condition, value) in entries.iter() {
                    traversal.configuration_dep(condition)?;
                    value.traverse(t, pkg.dupe(), traversal)?;
                }
                if let Some(v) = default {
                    v.traverse(t, pkg.dupe(), traversal)?;
                }
                Ok(())
            }
            CoercedAttrWithType::Concat(items, t) => {
                for item in items {
                    item.traverse(t, pkg.dupe(), traversal)?;
                }
                Ok(())
            }

            CoercedAttrWithType::None => Ok(()),
            CoercedAttrWithType::Some(attr, t) => attr.traverse(&t.inner, pkg.dupe(), traversal),

            CoercedAttrWithType::AnyList(list) => {
                for v in list.iter() {
                    // This is no-op now, but any may contain selects in the future.
                    v.traverse(t, pkg.dupe(), traversal)?;
                }
                Ok(())
            }
            CoercedAttrWithType::AnyTuple(tuple) => {
                for v in tuple.iter() {
                    v.traverse(t, pkg.dupe(), traversal)?;
                }
                Ok(())
            }
            CoercedAttrWithType::AnyDict(dict) => {
                for (k, v) in dict.iter() {
                    k.traverse(t, pkg.dupe(), traversal)?;
                    v.traverse(t, pkg.dupe(), traversal)?;
                }
                Ok(())
            }

            CoercedAttrWithType::Bool(..) => Ok(()),
            CoercedAttrWithType::Int(..) => Ok(()),
            CoercedAttrWithType::String(..) => Ok(()),
            CoercedAttrWithType::EnumVariant(..) => Ok(()),
            CoercedAttrWithType::List(list, t) => {
                for v in list.iter() {
                    v.traverse(&t.inner, pkg.dupe(), traversal)?;
                }
                Ok(())
            }
            CoercedAttrWithType::Tuple(list, t) => {
                if list.len() != t.xs.len() {
                    return Err(CoercedAttrError::InconsistentTupleLength.into());
                }

                for (v, vt) in list.iter().zip(&t.xs) {
                    v.traverse(vt, pkg.dupe(), traversal)?;
                }
                Ok(())
            }
            CoercedAttrWithType::Dict(dict, t) => {
                for (k, v) in dict.iter() {
                    k.traverse(&t.key, pkg.dupe(), traversal)?;
                    v.traverse(&t.value, pkg.dupe(), traversal)?;
                }
                Ok(())
            }

            CoercedAttrWithType::OneOf(l, i, t) => {
                let item_type = t.xs.get(i as usize).context("invalid enum")?;
                l.traverse(item_type, pkg, traversal)
            }
            CoercedAttrWithType::Visibility(..) => Ok(()),
            CoercedAttrWithType::ExplicitConfiguredDep(dep, _t) => dep.traverse(traversal),
            CoercedAttrWithType::SplitTransitionDep(dep, _t) => {
                traversal.split_transition_dep(dep.label.target(), &dep.transition)
            }
            CoercedAttrWithType::ConfiguredDep(dep) => {
                traversal.dep(dep.label.target().unconfigured())
            }
            CoercedAttrWithType::ConfigurationDep(dep, _t) => traversal.configuration_dep(dep),
            CoercedAttrWithType::Dep(dep, _t) => dep.traverse(traversal),
            CoercedAttrWithType::SourceLabel(s, _t) => traversal.dep(s.target()),
            CoercedAttrWithType::Label(label, _t) => traversal.label(label),
            CoercedAttrWithType::Arg(arg, _t) => arg.traverse(traversal),
            CoercedAttrWithType::Query(query, _t) => query.traverse(traversal),
            CoercedAttrWithType::SourceFile(source, _t) => {
                for x in source.inputs() {
                    traversal.input(BuckPathRef::new(pkg.dupe(), x))?;
                }
                Ok(())
            }
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
    pub fn configure(
        &self,
        ty: &AttrType,
        ctx: &dyn AttrConfigurationContext,
    ) -> anyhow::Result<ConfiguredAttr> {
        Ok(match CoercedAttrWithType::pack(self, ty)? {
            CoercedAttrWithType::Selector(CoercedSelector { entries, default }, t) => {
                if let Some(v) = Self::select_the_most_specific(ctx, entries)? {
                    return v.configure(t, ctx);
                }
                default
                    .as_ref()
                    .ok_or_else(|| {
                        SelectError::MissingDefault(
                            ctx.cfg().cfg().dupe(),
                            entries.iter().map(|(k, _)| k).duped().collect(),
                        )
                    })?
                    .configure(t, ctx)?
            }
            CoercedAttrWithType::Concat(items, t) => {
                let singleton = items.len() == 1;
                let mut it = items.iter().map(|item| item.configure(t, ctx));
                let first = it.next().ok_or(SelectError::ConcatEmpty)??;
                if singleton {
                    first
                } else {
                    first.concat(&mut it)?
                }
            }

            CoercedAttrWithType::AnyList(list) => {
                ConfiguredAttr::List(ListLiteral(list.try_map(|v| v.configure(ty, ctx))?.into()))
            }
            CoercedAttrWithType::AnyTuple(tuple) => ConfiguredAttr::Tuple(TupleLiteral(
                tuple.try_map(|v| v.configure(ty, ctx))?.into(),
            )),
            CoercedAttrWithType::AnyDict(dict) => ConfiguredAttr::Dict(DictLiteral(
                dict.try_map(|(k, v)| {
                    let k2 = k.configure(ty, ctx)?;
                    let v2 = v.configure(ty, ctx)?;
                    anyhow::Ok((k2, v2))
                })?
                .into(),
            )),

            CoercedAttrWithType::Bool(v, _t) => ConfiguredAttr::Bool(v),
            CoercedAttrWithType::Int(v, _t) => ConfiguredAttr::Int(v),
            CoercedAttrWithType::String(v, _t) => ConfiguredAttr::String(v.dupe()),
            CoercedAttrWithType::EnumVariant(v, _t) => ConfiguredAttr::EnumVariant(v.dupe()),
            CoercedAttrWithType::List(list, t) => ConfiguredAttr::List(ListLiteral(
                list.try_map(|v| v.configure(&t.inner, ctx))?.into(),
            )),
            CoercedAttrWithType::Tuple(list, t) => {
                if list.len() != t.xs.len() {
                    return Err(CoercedAttrError::InconsistentTupleLength.into());
                }
                ConfiguredAttr::Tuple(TupleLiteral(
                    list.iter()
                        .zip(&t.xs)
                        .map(|(v, vt)| v.configure(vt, ctx))
                        .collect::<anyhow::Result<_>>()?,
                ))
            }
            CoercedAttrWithType::Dict(dict, t) => ConfiguredAttr::Dict(DictLiteral(
                dict.try_map(|(k, v)| {
                    let k2 = k.configure(&t.key, ctx)?;
                    let v2 = v.configure(&t.value, ctx)?;
                    anyhow::Ok((k2, v2))
                })?
                .into(),
            )),
            CoercedAttrWithType::None => ConfiguredAttr::None,
            CoercedAttrWithType::Some(attr, t) => attr.configure(&t.inner, ctx)?,
            CoercedAttrWithType::OneOf(l, i, t) => {
                let item_ty = &t.xs[i as usize];
                let configured = l.configure(item_ty, ctx)?;
                ConfiguredAttr::OneOf(Box::new(configured), i)
            }
            CoercedAttrWithType::Visibility(v, _) => ConfiguredAttr::Visibility(v.clone()),
            CoercedAttrWithType::ExplicitConfiguredDep(dep, _) => {
                ExplicitConfiguredDepAttrType::configure(ctx, dep)?
            }
            CoercedAttrWithType::SplitTransitionDep(dep, _) => {
                SplitTransitionDepAttrType::configure(ctx, dep)?
            }
            CoercedAttrWithType::ConfiguredDep(dep) => ConfiguredAttr::Dep(Box::new(dep.clone())),
            CoercedAttrWithType::ConfigurationDep(dep, _) => {
                ConfigurationDepAttrType::configure(ctx, dep)?
            }
            CoercedAttrWithType::Dep(dep, _) => DepAttrType::configure(ctx, dep)?,
            CoercedAttrWithType::SourceLabel(source, _) => ConfiguredAttr::SourceLabel(Box::new(
                source.configure_pair(ctx.cfg().cfg_pair().dupe()),
            )),
            CoercedAttrWithType::Label(label, _) => LabelAttrType::configure(ctx, label)?,
            CoercedAttrWithType::Arg(arg, _) => ConfiguredAttr::Arg(arg.configure(ctx)?),
            CoercedAttrWithType::Query(query, _) => {
                ConfiguredAttr::Query(Box::new(query.configure(ctx)?))
            }
            CoercedAttrWithType::SourceFile(s, _) => ConfiguredAttr::SourceFile(s.clone()),
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
            CoercedAttr::ExplicitConfiguredDep(e) => e.any_matches(filter),
            CoercedAttr::SplitTransitionDep(e) => e.any_matches(filter),
            CoercedAttr::ConfiguredDep(e) => filter(&e.to_string()),
            CoercedAttr::ConfigurationDep(e) => filter(&e.to_string()),
            CoercedAttr::Dep(e) => filter(&e.to_string()),
            CoercedAttr::SourceLabel(e) => filter(&e.to_string()),
            CoercedAttr::Label(e) => filter(&e.to_string()),
            CoercedAttr::Arg(e) => filter(&e.to_string()),
            CoercedAttr::Query(e) => filter(e.query()),
            CoercedAttr::SourceFile(e) => filter(&e.path().to_string()),
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
