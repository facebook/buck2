/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;

use allocative::Allocative;
use buck2_core::buck_path::path::BuckPathRef;
use buck2_core::package::PackageLabel;
use dupe::Dupe;
use gazebo::prelude::*;
use static_assertions::assert_eq_size;

use super::attr_config::CoercedAttrExtraTypes;
use crate::attrs::attr_type::any_matches::AnyMatches;
use crate::attrs::attr_type::attr_config::AttrConfig;
use crate::attrs::attr_type::bool::BoolLiteral;
use crate::attrs::attr_type::dict::DictLiteral;
use crate::attrs::attr_type::list::ListLiteral;
use crate::attrs::attr_type::string::StringLiteral;
use crate::attrs::attr_type::tuple::TupleLiteral;
use crate::attrs::coerced_attr::CoercedAttr;
use crate::attrs::configuration_context::AttrConfigurationContext;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::display::AttrDisplayWithContext;
use crate::attrs::fmt_context::AttrFmtContext;
use crate::attrs::json::ToJsonWithContext;
use crate::attrs::traversal::CoercedAttrTraversal;
use crate::visibility::VisibilitySpecification;

#[derive(Debug, Eq, PartialEq, Hash, Clone, Allocative)]
pub enum AttrLiteral<C: AttrConfig> {
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
    List(ListLiteral<C>),
    Tuple(TupleLiteral<C>),
    Dict(DictLiteral<C>),
    None,
    // NOTE: unlike deps, labels are not traversed, as they are typically used in lieu of deps in
    // cases that would cause cycles.
    OneOf(
        Box<Self>,
        // Index of matched oneof attr type variant.
        u32,
    ),
    Visibility(VisibilitySpecification),
    Extra(C::ExtraTypes),
}

// Prevent size regression.
assert_eq_size!(AttrLiteral<CoercedAttr>, [usize; 3]);

impl<C: AttrConfig> AttrDisplayWithContext for AttrLiteral<C> {
    fn fmt(&self, ctx: &AttrFmtContext, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AttrLiteral::Bool(v) => {
                write!(f, "{}", v)
            }
            AttrLiteral::Int(v) => {
                write!(f, "{}", v)
            }
            AttrLiteral::String(v) | AttrLiteral::EnumVariant(v) => Display::fmt(v, f),
            AttrLiteral::List(list) => AttrDisplayWithContext::fmt(list, ctx, f),
            AttrLiteral::Tuple(v) => AttrDisplayWithContext::fmt(v, ctx, f),
            AttrLiteral::Dict(v) => AttrDisplayWithContext::fmt(v, ctx, f),
            AttrLiteral::None => write!(f, "None"),
            AttrLiteral::OneOf(box l, _) => AttrDisplayWithContext::fmt(l, ctx, f),
            AttrLiteral::Visibility(v) => Display::fmt(v, f),
            AttrLiteral::Extra(u) => AttrDisplayWithContext::fmt(u, ctx, f),
        }
    }
}

impl<C: AttrConfig> AttrLiteral<C> {
    pub fn to_json(&self, ctx: &AttrFmtContext) -> anyhow::Result<serde_json::Value> {
        use serde_json::to_value;
        match self {
            AttrLiteral::Bool(v) => Ok(to_value(v)?),
            AttrLiteral::Int(v) => Ok(to_value(v)?),
            AttrLiteral::String(v) | AttrLiteral::EnumVariant(v) => Ok(to_value(v)?),
            AttrLiteral::List(list) => list.to_json(ctx),
            AttrLiteral::Tuple(list) => list.to_json(ctx),
            AttrLiteral::Dict(dict) => dict.to_json(ctx),
            AttrLiteral::None => Ok(serde_json::Value::Null),
            AttrLiteral::OneOf(box l, _) => l.to_json(ctx),
            AttrLiteral::Visibility(v) => Ok(v.to_json()),
            AttrLiteral::Extra(u) => u.to_json(ctx),
        }
    }

    /// Checks if this attr matches the filter. For container-like things, will return true if any contained item matches the filter.
    pub fn any_matches(
        &self,
        filter: &dyn Fn(&str) -> anyhow::Result<bool>,
    ) -> anyhow::Result<bool> {
        match self {
            AttrLiteral::String(v) | AttrLiteral::EnumVariant(v) => filter(v),
            AttrLiteral::List(vals) => vals.any_matches(filter),
            AttrLiteral::Tuple(vals) => vals.any_matches(filter),
            AttrLiteral::Dict(d) => d.any_matches(filter),
            AttrLiteral::None => Ok(false),
            AttrLiteral::Bool(b) => b.any_matches(filter),
            AttrLiteral::Int(i) => filter(&i.to_string()),
            AttrLiteral::OneOf(l, _) => l.any_matches(filter),
            AttrLiteral::Visibility(v) => v.any_matches(filter),
            AttrLiteral::Extra(d) => d.any_matches(filter),
        }
    }
}

impl AttrLiteral<CoercedAttr> {
    pub fn configure(&self, ctx: &dyn AttrConfigurationContext) -> anyhow::Result<ConfiguredAttr> {
        Ok(match self {
            AttrLiteral::Bool(v) => ConfiguredAttr::Bool(*v),
            AttrLiteral::Int(v) => ConfiguredAttr::Int(*v),
            AttrLiteral::String(v) => ConfiguredAttr::String(v.dupe()),
            AttrLiteral::EnumVariant(v) => ConfiguredAttr::EnumVariant(v.dupe()),
            AttrLiteral::List(list) => {
                ConfiguredAttr::List(ListLiteral(list.try_map(|v| v.configure(ctx))?.into()))
            }
            AttrLiteral::Tuple(list) => {
                ConfiguredAttr::Tuple(TupleLiteral(list.try_map(|v| v.configure(ctx))?.into()))
            }
            AttrLiteral::Dict(dict) => ConfiguredAttr::Dict(DictLiteral(
                dict.try_map(|(k, v)| {
                    let k2 = k.configure(ctx)?;
                    let v2 = v.configure(ctx)?;
                    anyhow::Ok((k2, v2))
                })?
                .into(),
            )),
            AttrLiteral::None => ConfiguredAttr::None,
            AttrLiteral::OneOf(l, i) => {
                let configured = l.configure(ctx)?;
                ConfiguredAttr::OneOf(Box::new(configured), *i)
            }
            AttrLiteral::Visibility(v) => ConfiguredAttr::Visibility(v.clone()),
            AttrLiteral::Extra(u) => u.configure(ctx)?,
        })
    }

    pub(crate) fn traverse<'a>(
        &'a self,
        pkg: PackageLabel,
        traversal: &mut dyn CoercedAttrTraversal<'a>,
    ) -> anyhow::Result<()> {
        match self {
            AttrLiteral::Bool(_) => Ok(()),
            AttrLiteral::Int(_) => Ok(()),
            AttrLiteral::String(_) => Ok(()),
            AttrLiteral::EnumVariant(_) => Ok(()),
            AttrLiteral::List(list) => {
                for v in list.iter() {
                    v.traverse(pkg.dupe(), traversal)?;
                }
                Ok(())
            }
            AttrLiteral::Tuple(list) => {
                for v in list.iter() {
                    v.traverse(pkg.dupe(), traversal)?;
                }
                Ok(())
            }
            AttrLiteral::Dict(dict) => {
                for (k, v) in dict.iter() {
                    k.traverse(pkg.dupe(), traversal)?;
                    v.traverse(pkg.dupe(), traversal)?;
                }
                Ok(())
            }
            AttrLiteral::None => Ok(()),
            AttrLiteral::OneOf(box l, _) => l.traverse(pkg, traversal),
            AttrLiteral::Visibility(..) => Ok(()),
            AttrLiteral::Extra(u) => match u {
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
}
