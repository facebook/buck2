/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::fmt::Display;

use allocative::Allocative;
use buck2_core::buck_path::path::BuckPathRef;
use buck2_core::collections::ordered_map::OrderedMap;
use buck2_core::package::PackageLabel;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::target::label::TargetLabel;
use buck2_util::arc_str::ArcStr;
use dupe::Dupe;
use serde::Serialize;
use serde::Serializer;
use starlark_map::small_map;

use crate::attrs::attr_type::arg::StringWithMacros;
use crate::attrs::attr_type::attr_config::source_file_display;
use crate::attrs::attr_type::bool::BoolLiteral;
use crate::attrs::attr_type::configured_dep::ConfiguredExplicitConfiguredDep;
use crate::attrs::attr_type::dep::DepAttr;
use crate::attrs::attr_type::dict::DictLiteral;
use crate::attrs::attr_type::list::ListLiteral;
use crate::attrs::attr_type::query::QueryAttr;
use crate::attrs::attr_type::split_transition_dep::ConfiguredSplitTransitionDep;
use crate::attrs::attr_type::string::StringLiteral;
use crate::attrs::attr_type::tuple::TupleLiteral;
use crate::attrs::coerced_path::CoercedPath;
use crate::attrs::configured_traversal::ConfiguredAttrTraversal;
use crate::attrs::display::AttrDisplayWithContext;
use crate::attrs::display::AttrDisplayWithContextExt;
use crate::attrs::fmt_context::AttrFmtContext;
use crate::attrs::json::ToJsonWithContext;
use crate::attrs::serialize::AttrSerializeWithContext;
use crate::visibility::VisibilitySpecification;

#[derive(Debug, thiserror::Error)]
enum ConfiguredAttrError {
    #[error("addition not supported for this attribute type `{0}`.")]
    ConcatNotSupported(String),
    #[error("addition not supported for these attribute type `{0}` and value `{1}`.")]
    ConcatNotSupportedValues(&'static str, String),
    #[error("got same key in both sides of dictionary concat (key `{0}`).")]
    DictConcatDuplicateKeys(String),
    #[error("addition not supported for values of different types")]
    ConcatDifferentTypes,
    #[error("while concat, LHS is oneof, expecting RHS to also be oneof (internal error)")]
    LhsOneOfRhsNotOneOf,
    #[error("expecting a list, got `{0}`")]
    ExpectingList(String),
    #[error("expecting configuration dep, got `{0}`")]
    ExpectingConfigurationDep(String),
}

#[derive(Eq, PartialEq, Hash, Clone, Allocative, Debug)]
pub enum ConfiguredAttr {
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
    List(ListLiteral<ConfiguredAttr>),
    Tuple(TupleLiteral<ConfiguredAttr>),
    Dict(DictLiteral<ConfiguredAttr>),
    None,
    // NOTE: unlike deps, labels are not traversed, as they are typically used in lieu of deps in
    // cases that would cause cycles.
    OneOf(
        Box<Self>,
        // Index of matched oneof attr type variant.
        u32,
    ),
    Visibility(VisibilitySpecification),
    ExplicitConfiguredDep(Box<ConfiguredExplicitConfiguredDep>),
    SplitTransitionDep(Box<ConfiguredSplitTransitionDep>),
    ConfigurationDep(Box<TargetLabel>),
    Dep(Box<DepAttr<ConfiguredProvidersLabel>>),
    SourceLabel(Box<ConfiguredProvidersLabel>),
    // NOTE: unlike deps, labels are not traversed, as they are typically used in lieu of deps in
    // cases that would cause cycles.
    Label(Box<ConfiguredProvidersLabel>),
    Arg(StringWithMacros<ConfiguredAttr>),
    Query(Box<QueryAttr<ConfiguredAttr>>),
    SourceFile(CoercedPath),
}

impl AttrSerializeWithContext for ConfiguredAttr {
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

impl AttrDisplayWithContext for ConfiguredAttr {
    fn fmt(&self, ctx: &AttrFmtContext, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConfiguredAttr::Bool(v) => {
                write!(f, "{}", v)
            }
            ConfiguredAttr::Int(v) => {
                write!(f, "{}", v)
            }
            ConfiguredAttr::String(v) | ConfiguredAttr::EnumVariant(v) => Display::fmt(v, f),
            ConfiguredAttr::List(list) => AttrDisplayWithContext::fmt(list, ctx, f),
            ConfiguredAttr::Tuple(v) => AttrDisplayWithContext::fmt(v, ctx, f),
            ConfiguredAttr::Dict(v) => AttrDisplayWithContext::fmt(v, ctx, f),
            ConfiguredAttr::None => write!(f, "None"),
            ConfiguredAttr::OneOf(box l, _) => AttrDisplayWithContext::fmt(l, ctx, f),
            ConfiguredAttr::Visibility(v) => Display::fmt(v, f),
            ConfiguredAttr::ExplicitConfiguredDep(e) => Display::fmt(e, f),
            ConfiguredAttr::SplitTransitionDep(e) => Display::fmt(e, f),
            ConfiguredAttr::ConfigurationDep(e) => write!(f, "\"{}\"", e),
            ConfiguredAttr::Dep(e) => write!(f, "\"{}\"", e),
            ConfiguredAttr::SourceLabel(e) => write!(f, "\"{}\"", e),
            ConfiguredAttr::Label(e) => write!(f, "\"{}\"", e),
            ConfiguredAttr::Arg(e) => write!(f, "\"{}\"", e),
            ConfiguredAttr::Query(e) => write!(f, "\"{}\"", e.query()),
            ConfiguredAttr::SourceFile(e) => write!(f, "\"{}\"", source_file_display(ctx, e)),
        }
    }
}

impl ConfiguredAttr {
    /// Traverses the configured attribute and calls the traverse for every encountered target label (in deps, sources, or other places).
    pub fn traverse<'a>(
        &'a self,
        pkg: PackageLabel,
        traversal: &mut dyn ConfiguredAttrTraversal,
    ) -> anyhow::Result<()> {
        match self {
            ConfiguredAttr::Bool(_) => Ok(()),
            ConfiguredAttr::Int(_) => Ok(()),
            ConfiguredAttr::String(_) => Ok(()),
            ConfiguredAttr::EnumVariant(_) => Ok(()),
            ConfiguredAttr::List(list) => {
                for v in list.iter() {
                    v.traverse(pkg.dupe(), traversal)?;
                }
                Ok(())
            }
            ConfiguredAttr::Tuple(list) => {
                for v in list.iter() {
                    v.traverse(pkg.dupe(), traversal)?;
                }
                Ok(())
            }
            ConfiguredAttr::Dict(dict) => {
                for (k, v) in dict.iter() {
                    k.traverse(pkg.dupe(), traversal)?;
                    v.traverse(pkg.dupe(), traversal)?;
                }
                Ok(())
            }
            ConfiguredAttr::None => Ok(()),
            ConfiguredAttr::OneOf(l, _) => l.traverse(pkg, traversal),
            ConfiguredAttr::Visibility(..) => Ok(()),
            ConfiguredAttr::ExplicitConfiguredDep(dep) => dep.as_ref().traverse(traversal),
            ConfiguredAttr::SplitTransitionDep(deps) => {
                for target in deps.deps.values() {
                    traversal.dep(target)?;
                }
                Ok(())
            }
            ConfiguredAttr::ConfigurationDep(dep) => traversal.configuration_dep(dep),
            ConfiguredAttr::Dep(dep) => dep.traverse(traversal),
            ConfiguredAttr::SourceLabel(dep) => traversal.dep(dep),
            ConfiguredAttr::Label(label) => traversal.label(label),
            ConfiguredAttr::Arg(arg) => arg.traverse(traversal),
            ConfiguredAttr::Query(query) => query.traverse(traversal),
            ConfiguredAttr::SourceFile(source) => {
                for x in source.inputs() {
                    traversal.input(BuckPathRef::new(pkg.dupe(), x))?;
                }
                Ok(())
            }
        }
    }

    /// Used for concatting the configured result of concatted selects. For most types this isn't allowed (it
    /// should be unreachable as concat-ability is checked during coercion and the type would've returned false from `AttrType::supports_concat`).
    /// This is used when a select() is added to another value, like `select(<...>) + select(<...>)` or `select(<...>) + [...]`.
    pub(crate) fn concat(
        self,
        items: &mut dyn Iterator<Item = anyhow::Result<Self>>,
    ) -> anyhow::Result<Self> {
        let mismatch = |ty, attr: ConfiguredAttr| {
            Err(ConfiguredAttrError::ConcatNotSupportedValues(
                ty,
                attr.as_display_no_ctx().to_string(),
            )
            .into())
        };

        match self {
            ConfiguredAttr::OneOf(box first, first_i) => {
                first.concat(&mut items.map(|next| {
                    match next? {
                        ConfiguredAttr::OneOf(box next, next_i) => {
                            if first_i != next_i {
                                // TODO(nga): figure out how to make better error message.
                                //   We already lost lhs type here.
                                return Err(ConfiguredAttrError::ConcatDifferentTypes.into());
                            }
                            Ok(next)
                        }
                        _ => Err(ConfiguredAttrError::LhsOneOfRhsNotOneOf.into()),
                    }
                }))
            }
            ConfiguredAttr::List(list) => {
                let mut res = list.to_vec();
                for x in items {
                    match x? {
                        ConfiguredAttr::List(list2) => {
                            res.extend(list2.iter().cloned());
                        }
                        attr => return mismatch("list", attr),
                    }
                }
                Ok(ConfiguredAttr::List(ListLiteral(res.into())))
            }
            ConfiguredAttr::Dict(left) => {
                let mut res = OrderedMap::new();
                for (k, v) in left.iter().cloned() {
                    res.insert(k, v);
                }
                for x in items {
                    match x? {
                        ConfiguredAttr::Dict(right) => {
                            for (k, v) in right.iter().cloned() {
                                match res.entry(k) {
                                    small_map::Entry::Vacant(e) => {
                                        e.insert(v);
                                    }
                                    small_map::Entry::Occupied(e) => {
                                        return Err(ConfiguredAttrError::DictConcatDuplicateKeys(
                                            e.key().as_display_no_ctx().to_string(),
                                        )
                                        .into());
                                    }
                                }
                            }
                        }
                        attr => return mismatch("dict", attr),
                    }
                }
                Ok(ConfiguredAttr::Dict(res.into_iter().collect()))
            }
            ConfiguredAttr::String(res) => {
                let mut items = items.peekable();
                if items.peek().is_none() {
                    Ok(ConfiguredAttr::String(res))
                } else {
                    let mut res = str::to_owned(&res.0);
                    for x in items {
                        match x? {
                            ConfiguredAttr::String(right) => res.push_str(&right.0),
                            attr => return mismatch("string", attr),
                        }
                    }
                    Ok(ConfiguredAttr::String(StringLiteral(ArcStr::from(res))))
                }
            }
            ConfiguredAttr::Arg(left) => {
                let res = left.concat(items.map(|x| {
                    match x? {
                        ConfiguredAttr::Arg(x) => Ok(x),
                        attr => Err(ConfiguredAttrError::ConcatNotSupportedValues(
                            "arg",
                            attr.as_display_no_ctx().to_string(),
                        )
                        .into()),
                    }
                }))?;
                Ok(ConfiguredAttr::Arg(res))
            }
            val => Err(ConfiguredAttrError::ConcatNotSupported(
                val.as_display_no_ctx().to_string(),
            )
            .into()),
        }
    }

    pub(crate) fn try_into_configuration_dep(self) -> anyhow::Result<TargetLabel> {
        match self {
            ConfiguredAttr::ConfigurationDep(d) => Ok(*d),
            a => Err(ConfiguredAttrError::ExpectingConfigurationDep(
                a.as_display_no_ctx().to_string(),
            )
            .into()),
        }
    }

    pub fn unpack_list(&self) -> Option<&[ConfiguredAttr]> {
        match self {
            ConfiguredAttr::List(list) => Some(list),
            _ => None,
        }
    }

    pub(crate) fn try_into_list(self) -> anyhow::Result<Vec<ConfiguredAttr>> {
        match self {
            ConfiguredAttr::List(list) => Ok(list.to_vec()),
            a => Err(ConfiguredAttrError::ExpectingList(a.as_display_no_ctx().to_string()).into()),
        }
    }
}
