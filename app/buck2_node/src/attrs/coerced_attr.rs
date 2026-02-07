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
use std::collections::HashSet;
use std::fmt::Display;
use std::hash::Hash;
use std::ops::Deref;

use allocative::Allocative;
use buck2_core::configuration::config_setting::ConfigSettingData;
use buck2_core::package::PackageLabel;
use buck2_core::package::source_path::SourcePathRef;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::soft_error;
use buck2_core::target::label::label::TargetLabel;
use buck2_data::error::ErrorTag;
use buck2_error::BuckErrorContext;
use buck2_error::buck2_error;
use buck2_error::internal_error;
use buck2_util::arc_str::ArcSlice;
use buck2_util::arc_str::ArcStr;
use display_container::fmt_keyed_container;
use dupe::Dupe;
use gazebo::prelude::SliceExt;
use itertools::Itertools;
use pagable::Pagable;
use serde::Serialize;
use serde::Serializer;
use serde_json::to_value;
use smallvec::SmallVec;
use starlark_map::StarlarkHasherBuilder;

use super::values::TargetModifiersValue;
use crate::attrs::attr_type::AttrType;
use crate::attrs::attr_type::any_matches::AnyMatches;
use crate::attrs::attr_type::arg::StringWithMacros;
use crate::attrs::attr_type::attr_config::source_file_display;
use crate::attrs::attr_type::bool::BoolLiteral;
use crate::attrs::attr_type::configuration_dep::ConfigurationDepAttrType;
use crate::attrs::attr_type::configuration_dep::ConfigurationDepKind;
use crate::attrs::attr_type::configured_dep::ExplicitConfiguredDepAttrType;
use crate::attrs::attr_type::configured_dep::UnconfiguredExplicitConfiguredDep;
use crate::attrs::attr_type::dep::DepAttr;
use crate::attrs::attr_type::dict::DictLiteral;
use crate::attrs::attr_type::label::LabelAttrType;
use crate::attrs::attr_type::list::ListLiteral;
use crate::attrs::attr_type::query::QueryAttr;
use crate::attrs::attr_type::string::StringLiteral;
use crate::attrs::attr_type::transition_dep::CoercedTransitionDep;
use crate::attrs::attr_type::tuple::TupleLiteral;
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
use crate::configuration::resolved::ConfigurationSettingKey;
use crate::metadata::map::MetadataMap;
use crate::visibility::VisibilitySpecification;
use crate::visibility::WithinViewSpecification;

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

#[derive(Debug, Clone, PartialEq, Eq, Hash, Allocative, Pagable)]
pub struct CoercedSelector {
    pub(crate) entries: ArcSlice<(ConfigurationSettingKey, CoercedAttr)>,
    pub(crate) default: Option<CoercedAttr>,
}

impl AttrDisplayWithContext for CoercedSelector {
    fn fmt(&self, ctx: &AttrFmtContext, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "select(")?;
        fmt_keyed_container(
            f,
            "{",
            "}",
            ": ",
            self.all_entries().map(|(k, v)| {
                (
                    match k {
                        CoercedSelectorKeyRef::Target(k) => format!("\"{k}\""),
                        CoercedSelectorKeyRef::Default => "\"DEFAULT\"".to_owned(),
                    },
                    v.as_display(ctx),
                )
            }),
        )?;
        write!(f, ")")?;
        Ok(())
    }
}

impl AttrSerializeWithContext for CoercedSelector {
    fn serialize_with_ctx<S>(&self, ctx: &AttrFmtContext, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.to_json(ctx)
            .map_err(|e| serde::ser::Error::custom(format!("{e}")))?
            .serialize(s)
    }
}

impl CoercedSelector {
    pub fn new(
        entries: ArcSlice<(ConfigurationSettingKey, CoercedAttr)>,
        default: Option<CoercedAttr>,
    ) -> buck2_error::Result<CoercedSelector> {
        Self::check_all_keys_unique(&entries)?;
        Ok(CoercedSelector { entries, default })
    }

    fn check_all_keys_unique(
        entries: &[(ConfigurationSettingKey, CoercedAttr)],
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
            let mut visited_keys: HashSet<&ConfigurationSettingKey, _> =
                HashSet::with_capacity_and_hasher(entries.len(), StarlarkHasherBuilder);
            for (k, _) in entries {
                if !visited_keys.insert(k) {
                    return Err(duplicate_key(k));
                }
            }
        }

        Ok(())
    }

    pub fn all_entries(&self) -> impl Iterator<Item = (CoercedSelectorKeyRef<'_>, &CoercedAttr)> {
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

    pub fn to_json(&self, ctx: &AttrFmtContext) -> buck2_error::Result<serde_json::Value> {
        let mut map = serde_json::Map::new();
        for (key, value) in self.all_entries() {
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

    fn fail_to_json(message: &ArcStr) -> Result<serde_json::Value, buck2_error::Error> {
        Ok(serde_json::Value::Object(serde_json::Map::from_iter([
            (
                "__type".to_owned(),
                serde_json::Value::String("select_fail".to_owned()),
            ),
            (
                "message".to_owned(),
                serde_json::Value::String(message.to_string()),
            ),
        ])))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Allocative, Pagable)]
pub struct CoercedConcat(pub Box<[CoercedAttr]>);

impl Deref for CoercedConcat {
    type Target = [CoercedAttr];

    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}

impl CoercedConcat {
    pub fn to_json(&self, ctx: &AttrFmtContext) -> buck2_error::Result<serde_json::Value> {
        Ok(serde_json::Value::Object(serde_json::Map::from_iter([
            (
                "__type".to_owned(),
                serde_json::Value::String("concat".to_owned()),
            ),
            (
                "items".to_owned(),
                serde_json::Value::Array(self.0.try_map(|item| CoercedAttr::to_json(item, ctx))?),
            ),
        ])))
    }
}

impl AttrDisplayWithContext for CoercedConcat {
    fn fmt(&self, ctx: &AttrFmtContext, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.iter().map(|a| a.as_display(ctx)).format("+").fmt(f)
    }
}

impl AttrSerializeWithContext for CoercedConcat {
    fn serialize_with_ctx<S>(&self, ctx: &AttrFmtContext, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.to_json(ctx)
            .map_err(|e| serde::ser::Error::custom(format!("{e}")))?
            .serialize(s)
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, Allocative, Pagable)]
pub enum CoercedAttr {
    Selector(Box<CoercedSelector>),
    SelectFail(ArcStr),
    Concat(CoercedConcat),

    Bool(BoolLiteral),
    Int(i64),
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
    WithinView(WithinViewSpecification),
    ExplicitConfiguredDep(Box<UnconfiguredExplicitConfiguredDep>),
    TransitionDep(Box<CoercedTransitionDep>),
    SplitTransitionDep(ProvidersLabel),
    ConfiguredDepForForwardNode(Box<DepAttr<ConfiguredProvidersLabel>>),
    ConfigurationDep(ProvidersLabel),
    PluginDep(TargetLabel),
    Dep(ProvidersLabel),
    SourceLabel(ProvidersLabel),
    // NOTE: unlike deps, labels are not traversed, as they are typically used in lieu of deps in
    // cases that would cause cycles.
    Label(ProvidersLabel),
    Arg(StringWithMacros<ProvidersLabel>),
    Query(Box<QueryAttr<ProvidersLabel>>),
    SourceFile(CoercedPath),
    Metadata(MetadataMap),
    TargetModifiers(TargetModifiersValue),
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
            CoercedAttr::Selector(s) => s.fmt(ctx, f),
            CoercedAttr::SelectFail(s) => write!(f, "select_fail(\"{}\")", &s),
            CoercedAttr::Concat(c) => c.fmt(ctx, f),
            CoercedAttr::Bool(v) => {
                write!(f, "{v}")
            }
            CoercedAttr::Int(v) => {
                write!(f, "{v}")
            }
            CoercedAttr::String(v) | CoercedAttr::EnumVariant(v) => {
                AttrDisplayWithContext::fmt(v, ctx, f)
            }
            CoercedAttr::List(list) => AttrDisplayWithContext::fmt(list, ctx, f),
            CoercedAttr::Tuple(v) => AttrDisplayWithContext::fmt(v, ctx, f),
            CoercedAttr::Dict(v) => AttrDisplayWithContext::fmt(v, ctx, f),
            CoercedAttr::None => write!(f, "None"),
            CoercedAttr::OneOf(box l, _) => AttrDisplayWithContext::fmt(l, ctx, f),
            CoercedAttr::Visibility(v) => Display::fmt(v, f),
            CoercedAttr::WithinView(v) => Display::fmt(v, f),
            CoercedAttr::ExplicitConfiguredDep(e) => Display::fmt(e, f),
            CoercedAttr::SplitTransitionDep(e) => Display::fmt(e, f),
            CoercedAttr::TransitionDep(e) => Display::fmt(e, f),
            CoercedAttr::ConfiguredDepForForwardNode(e) => write!(f, "\"{e}\""),
            CoercedAttr::ConfigurationDep(e) => write!(f, "\"{e}\""),
            CoercedAttr::PluginDep(e) => write!(f, "\"{e}\""),
            CoercedAttr::Dep(e) => write!(f, "\"{e}\""),
            CoercedAttr::SourceLabel(e) => write!(f, "\"{e}\""),
            CoercedAttr::Label(e) => write!(f, "\"{e}\""),
            CoercedAttr::Arg(e) => write!(f, "\"{e}\""),
            CoercedAttr::Query(e) => write!(f, "\"{}\"", e.query.query),
            CoercedAttr::SourceFile(e) => write!(f, "\"{}\"", source_file_display(ctx, e)),
            CoercedAttr::Metadata(m) => write!(f, "{m}"),
            CoercedAttr::TargetModifiers(m) => write!(f, "{m}"),
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
            .map_err(|e| serde::ser::Error::custom(format!("{e}")))?
            .serialize(s)
    }
}

impl CoercedAttr {
    /// Converts the coerced attr to a serde_json Value. This is generally just used for debugging or introspective
    /// things, a lot of the types will be dropped without special handling. For example, an artifact will just end
    /// up as the stringified version of its coerced value (i.e. while `//a:b` might represent some list of targets,
    /// in to_json it just appears as the string "//a:b").
    pub fn to_json(&self, ctx: &AttrFmtContext) -> buck2_error::Result<serde_json::Value> {
        match self {
            CoercedAttr::Selector(s) => s.to_json(ctx),
            CoercedAttr::SelectFail(string_literal) => {
                CoercedSelector::fail_to_json(string_literal)
            }
            CoercedAttr::Concat(c) => c.to_json(ctx),
            CoercedAttr::Bool(v) => Ok(to_value(v)?),
            CoercedAttr::Int(v) => Ok(to_value(v)?),
            CoercedAttr::String(v) | CoercedAttr::EnumVariant(v) => Ok(to_value(v)?),
            CoercedAttr::List(list) => list.to_json(ctx),
            CoercedAttr::Tuple(list) => list.to_json(ctx),
            CoercedAttr::Dict(dict) => dict.to_json(ctx),
            CoercedAttr::None => Ok(serde_json::Value::Null),
            CoercedAttr::OneOf(box l, _) => l.to_json(ctx),
            CoercedAttr::Visibility(v) => Ok(v.to_json()),
            CoercedAttr::WithinView(v) => Ok(v.to_json()),
            CoercedAttr::ExplicitConfiguredDep(e) => e.to_json(),
            CoercedAttr::SplitTransitionDep(e) => Ok(to_value(e.to_string())?),
            CoercedAttr::TransitionDep(e) => e.to_json(),
            CoercedAttr::ConfiguredDepForForwardNode(e) => Ok(to_value(e.to_string())?),
            CoercedAttr::ConfigurationDep(e) => Ok(to_value(e.to_string())?),
            CoercedAttr::PluginDep(e) => Ok(to_value(e.to_string())?),
            CoercedAttr::Dep(e) => Ok(to_value(e.to_string())?),
            CoercedAttr::SourceLabel(e) => Ok(to_value(e.to_string())?),
            CoercedAttr::Label(e) => Ok(to_value(e.to_string())?),
            CoercedAttr::Arg(e) => Ok(to_value(e.to_string())?),
            CoercedAttr::Query(e) => Ok(to_value(&e.query.query)?),
            CoercedAttr::SourceFile(e) => Ok(to_value(source_file_display(ctx, e).to_string())?),
            CoercedAttr::Metadata(m) => Ok(m.to_value()),
            CoercedAttr::TargetModifiers(m) => Ok(m.to_value()),
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
    ) -> buck2_error::Result<()> {
        match CoercedAttrWithType::pack(self, t)? {
            CoercedAttrWithType::Selector(CoercedSelector { entries, default }, t) => {
                for (condition, value) in entries.iter() {
                    traversal.configuration_dep(&condition.0, ConfigurationDepKind::SelectKey)?;
                    value.traverse(t, pkg, traversal)?;
                }
                if let Some(v) = default {
                    v.traverse(t, pkg, traversal)?;
                }
                Ok(())
            }
            CoercedAttrWithType::Concat(items, t) => {
                for item in items {
                    item.traverse(t, pkg, traversal)?;
                }
                Ok(())
            }
            CoercedAttrWithType::SelectFail(..) => Ok(()),

            CoercedAttrWithType::None => Ok(()),
            CoercedAttrWithType::Some(attr, t) => attr.traverse(&t.inner, pkg, traversal),

            CoercedAttrWithType::AnyList(list) => {
                for v in list.iter() {
                    // This is no-op now, but any may contain selects in the future.
                    v.traverse(t, pkg, traversal)?;
                }
                Ok(())
            }
            CoercedAttrWithType::AnyTuple(tuple) => {
                for v in tuple.iter() {
                    v.traverse(t, pkg, traversal)?;
                }
                Ok(())
            }
            CoercedAttrWithType::AnyDict(dict) => {
                for (k, v) in dict.iter() {
                    k.traverse(t, pkg, traversal)?;
                    v.traverse(t, pkg, traversal)?;
                }
                Ok(())
            }

            CoercedAttrWithType::Bool(..) => Ok(()),
            CoercedAttrWithType::Int(..) => Ok(()),
            CoercedAttrWithType::String(..) => Ok(()),
            CoercedAttrWithType::EnumVariant(..) => Ok(()),
            CoercedAttrWithType::List(list, t) => {
                for v in list.iter() {
                    v.traverse(&t.inner, pkg, traversal)?;
                }
                Ok(())
            }
            CoercedAttrWithType::Tuple(list, t) => {
                if list.len() != t.xs.len() {
                    return Err(internal_error!("Inconsistent number of elements in tuple"));
                }

                for (v, vt) in list.iter().zip(&t.xs) {
                    v.traverse(vt, pkg, traversal)?;
                }
                Ok(())
            }
            CoercedAttrWithType::Dict(dict, t) => {
                for (k, v) in dict.iter() {
                    k.traverse(&t.key, pkg, traversal)?;
                    v.traverse(&t.value, pkg, traversal)?;
                }
                Ok(())
            }

            CoercedAttrWithType::OneOf(l, i, t) => {
                let item_type =
                    t.xs.get(i as usize)
                        .ok_or_else(|| internal_error!("invalid enum"))?;
                l.traverse(item_type, pkg, traversal)
            }
            CoercedAttrWithType::Visibility(..) => Ok(()),
            CoercedAttrWithType::WithinView(..) => Ok(()),
            CoercedAttrWithType::ExplicitConfiguredDep(dep, _t) => dep.traverse(traversal),
            CoercedAttrWithType::SplitTransitionDep(dep, t) => {
                traversal.split_transition_dep(dep, &t.transition)
            }
            CoercedAttrWithType::TransitionDep(dep, t) => dep.traverse(traversal, t),
            CoercedAttrWithType::ConfiguredDep(dep) => traversal.dep(&dep.label.unconfigured()),
            CoercedAttrWithType::ConfigurationDep(dep, t) => traversal.configuration_dep(dep, t.0),
            CoercedAttrWithType::PluginDep(dep, t) => traversal.plugin_dep(dep, t.kind()),
            CoercedAttrWithType::Dep(dep, t) => {
                DepAttr::<ProvidersLabel>::traverse(dep, t, traversal)
            }
            CoercedAttrWithType::SourceLabel(s, _t) => traversal.dep(s),
            CoercedAttrWithType::Label(label, _t) => traversal.label(label),
            CoercedAttrWithType::Arg(arg, _t) => arg.traverse(traversal, pkg),
            CoercedAttrWithType::Query(query, _t) => query.traverse(traversal),
            CoercedAttrWithType::SourceFile(source, _t) => {
                for x in source.inputs() {
                    traversal.input(SourcePathRef::new(pkg, x))?;
                }
                Ok(())
            }
            CoercedAttrWithType::Metadata(..) => Ok(()),
            CoercedAttrWithType::TargetModifiers(..) => Ok(()),
        }
    }

    /// If more than one select key matches, select the most specific.
    pub fn select_the_most_specific<'a, 'x>(
        select_entries: impl IntoIterator<
            Item = (
                &'x ConfigurationSettingKey,
                &'x ConfigSettingData,
                &'a CoercedAttr,
            ),
        >,
    ) -> buck2_error::Result<Option<&'a CoercedAttr>> {
        let select_entries_vec = SmallVec::<[_; 17]>::from_iter(select_entries);

        let mut select_entries = select_entries_vec.iter().copied();
        let Some(mut matching): Option<(
            &ConfigurationSettingKey,
            &ConfigSettingData,
            &CoercedAttr,
        )> = select_entries.next() else {
            return Ok(None);
        };

        for (k, conf, v) in select_entries {
            let (prev_k, prev_conf, prev_v) = matching;
            matching = {
                {
                    if conf.refines(prev_conf) {
                        (k, conf, v)
                    } else if prev_conf.refines(conf) {
                        (prev_k, prev_conf, prev_v)
                    } else {
                        return Self::select_the_most_specific_slow(select_entries_vec);
                    }
                }
            }
        }
        Ok(Some(matching.2))
    }

    /// Select the first matching entry without considering specificity.
    pub fn select_the_first_match<'a, 'x>(
        select_entries: impl IntoIterator<
            Item = (
                &'x ConfigurationSettingKey,
                &'x ConfigSettingData,
                &'a CoercedAttr,
            ),
        >,
    ) -> Option<&'a CoercedAttr> {
        select_entries.into_iter().next().map(|(_, _, v)| v)
    }

    fn select_the_most_specific_slow<'a>(
        select_entries: SmallVec<
            [(
                &ConfigurationSettingKey,
                &ConfigSettingData,
                &'a CoercedAttr,
            ); 17],
        >,
    ) -> buck2_error::Result<Option<&'a CoercedAttr>> {
        let mut entries =
            SmallVec::<[(&ConfigurationSettingKey, &ConfigSettingData, &CoercedAttr); 17]>::new();

        for (k, d, v) in select_entries {
            // If there's entry for `linux-arm32` and current is `arm32`, skip current.
            if entries.iter().any(|(_, prev_d, _)| prev_d.refines(d)) {
                continue;
            }
            // If current is `linux-arm32`, remove `arm32` from `entries`.
            entries.retain(|(_, prev_d, _)| !d.refines(prev_d));
            entries.push((k, d, v));
        }
        match entries.as_slice() {
            [] => Err(internal_error!(
                "no entries after slow select the most specific"
            )),
            [(.., x)] => Ok(Some(x)),
            multiple_entries => {
                // Check if all entries have the same value
                let (first_key, _, first_value) = &multiple_entries[0];
                // Find the first entry with a different value, if any
                let different_value_entry = multiple_entries
                    .iter()
                    .skip(1)
                    .find(|(_, _, v)| v != first_value);
                if let Some((different_key, _, _)) = different_value_entry {
                    // Report the ambiguity error with the specific keys that have different values
                    Err(buck2_error!(
                        buck2_error::ErrorTag::Input,
                        "Both select keys `{first_key}` and `{different_key}` match the configuration, but neither is more specific and they have different values"
                    ))
                } else {
                    // If all values are the same, return that value
                    Ok(Some(first_value))
                }
            }
        }
    }

    fn select<'a>(
        ctx: &dyn AttrConfigurationContext,
        select: &'a CoercedSelector,
    ) -> buck2_error::Result<&'a CoercedAttr> {
        let CoercedSelector { entries, default } = select;
        let matched_cfg_keys = ctx.matched_cfg_keys();
        let resolved_entries: Vec<_> = entries
            .iter()
            .filter_map(|(k, v)| matched_cfg_keys.setting_matches(k).map(|conf| (k, conf, v)))
            .collect();

        if let Some(v) = Self::select_the_most_specific(resolved_entries.iter().copied())? {
            // Compute first match for comparison (data collection for future migration)
            let first_match = Self::select_the_first_match(resolved_entries.iter().copied());
            // Compare with first match and emit soft error if they differ
            match first_match {
                Some(first) if first != v => {
                    let _unused = soft_error!(
                        "select_first_match_differs",
                        buck2_error!(
                            buck2_error::ErrorTag::Input,
                            "First matching select key has different value than most specific match: {}",
                            select.as_display_no_ctx()
                        ),
                        quiet: true,
                    );
                }
                _ => {}
            }
            Ok(v)
        } else {
            default.as_ref().ok_or_else(|| {
                buck2_error!(
                    buck2_error::ErrorTag::Input,
                    "None of {} conditions matched configuration `{}` and no default was set:\n{}",
                    entries.len(),
                    ctx.cfg().cfg(),
                    entries.iter().map(|(s, _)| format!("  {s}")).join("\n"),
                )
            })
        }
    }

    /// Returns the "configured" representation of the attribute in the provided context.
    /// This handles the resolution of the select() conditions and delegates to
    /// the actual attr type for handling any appropriate configuration-time
    /// processing.
    pub fn configure(
        &self,
        ty: &AttrType,
        ctx: &dyn AttrConfigurationContext,
    ) -> buck2_error::Result<ConfiguredAttr> {
        self.configure_inner(ty, ctx).tag(ErrorTag::ConfigureAttr)
    }

    fn configure_inner(
        &self,
        ty: &AttrType,
        ctx: &dyn AttrConfigurationContext,
    ) -> buck2_error::Result<ConfiguredAttr> {
        Ok(match CoercedAttrWithType::pack(self, ty)? {
            CoercedAttrWithType::Selector(select, t) => {
                Self::select(ctx, select)?.configure(t, ctx)?
            }
            CoercedAttrWithType::Concat(items, t) => {
                let singleton = items.len() == 1;
                let mut it = items.iter().map(|item| item.configure(t, ctx));
                let first = it
                    .next()
                    .ok_or_else(|| internal_error!("concat with no items"))??;
                if singleton {
                    first
                } else {
                    first.concat(t, &mut it)?
                }
            }
            CoercedAttrWithType::SelectFail(message, _) => {
                return Err(buck2_error!(
                    buck2_error::ErrorTag::Input,
                    "select resolved to select_fail(): {message}"
                ));
            }
            CoercedAttrWithType::AnyList(list) => ConfiguredAttr::List(ListLiteral(
                list.try_map(|v| v.configure(AttrType::any_ref(), ctx))?
                    .into(),
            )),
            CoercedAttrWithType::AnyTuple(tuple) => ConfiguredAttr::Tuple(TupleLiteral(
                tuple
                    .try_map(|v| v.configure(AttrType::any_ref(), ctx))?
                    .into(),
            )),
            CoercedAttrWithType::AnyDict(dict) => ConfiguredAttr::Dict(DictLiteral(
                dict.try_map(|(k, v)| {
                    let k2 = k.configure(AttrType::any_ref(), ctx)?;
                    let v2 = v.configure(AttrType::any_ref(), ctx)?;
                    buck2_error::Ok((k2, v2))
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
                    return Err(internal_error!("Inconsistent number of elements in tuple"));
                }
                ConfiguredAttr::Tuple(TupleLiteral(
                    list.iter()
                        .zip(&t.xs)
                        .map(|(v, vt)| v.configure(vt, ctx))
                        .collect::<buck2_error::Result<_>>()?,
                ))
            }
            CoercedAttrWithType::Dict(dict, t) => ConfiguredAttr::Dict(DictLiteral(
                dict.try_map(|(k, v)| {
                    let k2 = k.configure(&t.key, ctx)?;
                    let v2 = v.configure(&t.value, ctx)?;
                    buck2_error::Ok((k2, v2))
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
            CoercedAttrWithType::WithinView(v, _) => ConfiguredAttr::WithinView(v.clone()),
            CoercedAttrWithType::ExplicitConfiguredDep(dep, _) => {
                ExplicitConfiguredDepAttrType::configure(ctx, dep)?
            }
            CoercedAttrWithType::SplitTransitionDep(dep, t) => t.configure(dep, ctx)?,
            CoercedAttrWithType::TransitionDep(dep, t) => t.configure(dep, ctx)?,
            CoercedAttrWithType::ConfiguredDep(dep) => ConfiguredAttr::Dep(Box::new(dep.clone())),
            CoercedAttrWithType::ConfigurationDep(dep, _) => {
                ConfigurationDepAttrType::configure(ctx, dep)?
            }
            CoercedAttrWithType::PluginDep(dep, t) => {
                ConfiguredAttr::PluginDep(dep.dupe(), t.kind().dupe())
            }
            CoercedAttrWithType::Dep(dep, t) => t.configure(dep, ctx)?,
            CoercedAttrWithType::SourceLabel(source, _) => {
                ConfiguredAttr::SourceLabel(source.configure_pair(ctx.cfg().cfg_pair().dupe()))
            }
            CoercedAttrWithType::Label(label, _) => LabelAttrType::configure(ctx, label)?,
            CoercedAttrWithType::Arg(arg, attr_type) => {
                ConfiguredAttr::Arg(arg.configure(ctx, attr_type.anon_target_compatible)?)
            }
            CoercedAttrWithType::Query(query, _) => {
                ConfiguredAttr::Query(Box::new(query.configure(ctx)?))
            }
            CoercedAttrWithType::SourceFile(s, _) => ConfiguredAttr::SourceFile(s.clone()),
            CoercedAttrWithType::Metadata(m, _) => ConfiguredAttr::Metadata(m.clone()),
            CoercedAttrWithType::TargetModifiers(m, _) => ConfiguredAttr::TargetModifiers(m.dupe()),
        })
    }

    /// Checks if this attr matches the filter. For selectors and container-like things, will return true if any
    /// contained item matches the filter.
    pub fn any_matches(
        &self,
        filter: &dyn Fn(&str) -> buck2_error::Result<bool>,
    ) -> buck2_error::Result<bool> {
        match self {
            CoercedAttr::Selector(s) => {
                for value in s.all_values() {
                    if value.any_matches(filter)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            CoercedAttr::Concat(c) => {
                for item in &*c.0 {
                    if item.any_matches(filter)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            CoercedAttr::SelectFail(_) => Ok(false),
            CoercedAttr::String(v) | CoercedAttr::EnumVariant(v) => filter(v),
            CoercedAttr::List(vals) => vals.any_matches(filter),
            CoercedAttr::Tuple(vals) => vals.any_matches(filter),
            CoercedAttr::Dict(d) => d.any_matches(filter),
            CoercedAttr::None => Ok(false),
            CoercedAttr::Bool(b) => b.any_matches(filter),
            CoercedAttr::Int(i) => filter(&i.to_string()),
            CoercedAttr::OneOf(l, _) => l.any_matches(filter),
            CoercedAttr::Visibility(v) => v.any_matches(filter),
            CoercedAttr::WithinView(v) => v.any_matches(filter),
            CoercedAttr::ExplicitConfiguredDep(e) => e.any_matches(filter),
            CoercedAttr::TransitionDep(v) => v.any_matches(filter),
            CoercedAttr::SplitTransitionDep(e) => filter(&e.to_string()),
            CoercedAttr::ConfiguredDepForForwardNode(e) => filter(&e.to_string()),
            CoercedAttr::ConfigurationDep(e) => filter(&e.to_string()),
            CoercedAttr::PluginDep(e) => filter(&e.to_string()),
            CoercedAttr::Dep(e) => filter(&e.to_string()),
            CoercedAttr::SourceLabel(e) => filter(&e.to_string()),
            CoercedAttr::Label(e) => filter(&e.to_string()),
            CoercedAttr::Arg(e) => filter(&e.to_string()),
            CoercedAttr::Query(e) => filter(&e.query.query),
            CoercedAttr::SourceFile(e) => filter(&e.path().to_string()),
            CoercedAttr::Metadata(e) => e.any_matches(filter),
            CoercedAttr::TargetModifiers(e) => e.any_matches(filter),
        }
    }
}

#[cfg(test)]
mod tests {

    use dupe::Dupe;

    use crate::attrs::coerced_attr::CoercedAttr;
    use crate::attrs::coerced_attr::CoercedSelector;
    use crate::configuration::resolved::ConfigurationSettingKey;

    #[test]
    fn test_check_all_keys_unique_small() {
        let a = ConfigurationSettingKey::testing_parse("foo//:a");
        let b = ConfigurationSettingKey::testing_parse("foo//:b");
        let c = ConfigurationSettingKey::testing_parse("foo//:c");
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
                    ConfigurationSettingKey::testing_parse(&format!("foo//:{i}")),
                    attr.clone(),
                )
            })
            .collect::<Vec<_>>();
        assert!(CoercedSelector::check_all_keys_unique(&long).is_ok());
        long[10].0 = long[0].0.dupe();
        assert!(CoercedSelector::check_all_keys_unique(&long).is_err());
    }
}
