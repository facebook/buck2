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
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersLabelMaybeConfigured;
use buck2_core::target::label::TargetLabel;
use dupe::Dupe;
use either::Either;
use serde_json::to_value;

use super::arg::StringWithMacros;
use super::dep::DepAttr;
use super::query::QueryAttr;
use crate::attrs::attr_type::any_matches::AnyMatches;
use crate::attrs::attr_type::attr_like::AttrLike;
use crate::attrs::attr_type::configuration_dep::ConfigurationDepAttrType;
use crate::attrs::attr_type::configured_dep::ExplicitConfiguredDepAttrType;
use crate::attrs::attr_type::configured_dep::UnconfiguredExplicitConfiguredDep;
use crate::attrs::attr_type::dep::DepAttrType;
use crate::attrs::attr_type::dep::ExplicitConfiguredDepMaybeConfigured;
use crate::attrs::attr_type::label::LabelAttrType;
use crate::attrs::attr_type::split_transition_dep::SplitTransitionDep;
use crate::attrs::attr_type::split_transition_dep::SplitTransitionDepAttrType;
use crate::attrs::attr_type::split_transition_dep::SplitTransitionDepMaybeConfigured;
use crate::attrs::coerced_attr::CoercedAttr;
use crate::attrs::coerced_path::CoercedPath;
use crate::attrs::configuration_context::AttrConfigurationContext;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::display::AttrDisplayWithContext;
use crate::attrs::fmt_context::AttrFmtContext;
use crate::attrs::json::ToJsonWithContext;

/// AttrConfig is used to implement things just once to cover both the configured and
/// unconfigured case. ExtraTypes contains the specifications for the configured vs

/// unconfigured case. Additional attr types can use ExtraTypes to define additional
/// attrs without needing to modify the existing ConfiguredAttr or CoercedAttr implementations.
///
/// For attributes, the difference between the coerced value and the configured value is
/// (1) selects are resolved and (2) configurable things are configured. This trait allows
/// most of the attr representation to be shared between those two states.
///
/// There's really just two implementations of this, one for coerced attrs with
/// unconfigured types and one for configured attrs with the configured types.
pub trait AttrConfig: AttrLike + AttrDisplayWithContext + AnyMatches + ToJsonWithContext {
    type ProvidersType: ProvidersLabelMaybeConfigured + AttrLike;
}

/// Needed to support `ExtraTypes` for within `AttrConfig`.
pub trait AttrConfigExtraTypes: AnyMatches + ToJsonWithContext {}

impl AttrConfig for ConfiguredAttr {
    type ProvidersType = ConfiguredProvidersLabel;
}

impl ToJsonWithContext for ConfiguredAttr {
    fn to_json(&self, ctx: &AttrFmtContext) -> anyhow::Result<serde_json::Value> {
        match self {
            ConfiguredAttr::Bool(v) => Ok(to_value(v)?),
            ConfiguredAttr::Int(v) => Ok(to_value(v)?),
            ConfiguredAttr::String(v) | ConfiguredAttr::EnumVariant(v) => Ok(to_value(v)?),
            ConfiguredAttr::List(list) => list.to_json(ctx),
            ConfiguredAttr::Tuple(list) => list.to_json(ctx),
            ConfiguredAttr::Dict(dict) => dict.to_json(ctx),
            ConfiguredAttr::None => Ok(serde_json::Value::Null),
            ConfiguredAttr::OneOf(box l, _) => l.to_json(ctx),
            ConfiguredAttr::Visibility(v) => Ok(v.to_json()),
            ConfiguredAttr::ExplicitConfiguredDep(e) => e.to_json(),
            ConfiguredAttr::SplitTransitionDep(e) => e.to_json(),
            ConfiguredAttr::ConfigurationDep(e) => Ok(to_value(e.to_string())?),
            ConfiguredAttr::Dep(e) => Ok(to_value(e.to_string())?),
            ConfiguredAttr::SourceLabel(e) => Ok(to_value(e.to_string())?),
            ConfiguredAttr::Label(e) => Ok(to_value(e.to_string())?),
            ConfiguredAttr::Arg(e) => Ok(to_value(e.to_string())?),
            ConfiguredAttr::Query(e) => Ok(to_value(e.query())?),
            ConfiguredAttr::SourceFile(e) => Ok(to_value(source_file_display(ctx, e).to_string())?),
        }
    }
}

impl AnyMatches for ConfiguredAttr {
    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool> {
        match self {
            ConfiguredAttr::String(v) | ConfiguredAttr::EnumVariant(v) => filter(v),
            ConfiguredAttr::List(vals) => vals.any_matches(filter),
            ConfiguredAttr::Tuple(vals) => vals.any_matches(filter),
            ConfiguredAttr::Dict(d) => d.any_matches(filter),
            ConfiguredAttr::None => Ok(false),
            ConfiguredAttr::Bool(b) => b.any_matches(filter),
            ConfiguredAttr::Int(i) => filter(&i.to_string()),
            ConfiguredAttr::OneOf(l, _) => l.any_matches(filter),
            ConfiguredAttr::Visibility(v) => v.any_matches(filter),
            ConfiguredAttr::ExplicitConfiguredDep(e) => e.any_matches(filter),
            ConfiguredAttr::SplitTransitionDep(e) => e.any_matches(filter),
            ConfiguredAttr::ConfigurationDep(e) => filter(&e.to_string()),
            ConfiguredAttr::Dep(e) => filter(&e.to_string()),
            ConfiguredAttr::SourceLabel(e) => filter(&e.to_string()),
            ConfiguredAttr::Label(e) => filter(&e.to_string()),
            ConfiguredAttr::Arg(e) => filter(&e.to_string()),
            ConfiguredAttr::Query(e) => filter(e.query()),
            ConfiguredAttr::SourceFile(e) => filter(&e.path().to_string()),
        }
    }
}

/// Attribute literal type encoding for CoercedAttrs.
#[derive(Allocative, Debug, Clone, Eq, PartialEq, Hash)]
pub enum CoercedAttrExtraTypes {
    ExplicitConfiguredDep(Box<UnconfiguredExplicitConfiguredDep>),
    SplitTransitionDep(Box<SplitTransitionDep>),
    ConfiguredDep(Box<DepAttr<ConfiguredProvidersLabel>>),
    ConfigurationDep(Box<TargetLabel>),
    Dep(Box<DepAttr<ProvidersLabel>>),
    SourceLabel(Box<ProvidersLabel>),
    // NOTE: unlike deps, labels are not traversed, as they are typically used in lieu of deps in
    // cases that would cause cycles.
    Label(Box<ProvidersLabel>),
    Arg(StringWithMacros<CoercedAttr>),
    Query(Box<QueryAttr<CoercedAttr>>),
    SourceFile(CoercedPath),
}

impl AttrConfigExtraTypes for CoercedAttrExtraTypes {}

impl ToJsonWithContext for CoercedAttrExtraTypes {
    fn to_json(&self, ctx: &AttrFmtContext) -> anyhow::Result<serde_json::Value> {
        match self {
            Self::ExplicitConfiguredDep(e) => e.to_json(),
            Self::SplitTransitionDep(e) => e.to_json(),
            Self::ConfiguredDep(e) => Ok(to_value(e.to_string())?),
            Self::ConfigurationDep(e) => Ok(to_value(e.to_string())?),
            Self::Dep(e) => Ok(to_value(e.to_string())?),
            Self::SourceLabel(e) => Ok(to_value(e.to_string())?),
            Self::Label(e) => Ok(to_value(e.to_string())?),
            Self::Arg(e) => Ok(to_value(e.to_string())?),
            Self::Query(e) => Ok(to_value(e.query())?),
            Self::SourceFile(e) => Ok(to_value(source_file_display(ctx, e).to_string())?),
        }
    }
}

impl AnyMatches for CoercedAttrExtraTypes {
    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool> {
        match self {
            Self::ExplicitConfiguredDep(e) => e.any_matches(filter),
            Self::SplitTransitionDep(e) => e.any_matches(filter),
            Self::ConfiguredDep(e) => filter(&e.to_string()),
            Self::ConfigurationDep(e) => filter(&e.to_string()),
            Self::Dep(e) => filter(&e.to_string()),
            Self::SourceLabel(e) => filter(&e.to_string()),
            Self::Label(e) => filter(&e.to_string()),
            Self::Arg(e) => filter(&e.to_string()),
            Self::Query(e) => filter(e.query()),
            Self::SourceFile(e) => filter(&e.path().to_string()),
        }
    }
}

impl AttrDisplayWithContext for CoercedAttrExtraTypes {
    fn fmt(&self, ctx: &AttrFmtContext, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ExplicitConfiguredDep(e) => Display::fmt(e, f),
            Self::SplitTransitionDep(e) => Display::fmt(e, f),
            Self::ConfiguredDep(e) => write!(f, "\"{}\"", e),
            Self::ConfigurationDep(e) => write!(f, "\"{}\"", e),
            Self::Dep(e) => write!(f, "\"{}\"", e),
            Self::SourceLabel(e) => write!(f, "\"{}\"", e),
            Self::Label(e) => write!(f, "\"{}\"", e),
            Self::Arg(e) => write!(f, "\"{}\"", e),
            Self::Query(e) => write!(f, "\"{}\"", e.query()),
            Self::SourceFile(e) => write!(f, "\"{}\"", source_file_display(ctx, e)),
        }
    }
}

impl CoercedAttrExtraTypes {
    pub(crate) fn configure(
        &self,
        ctx: &dyn AttrConfigurationContext,
    ) -> anyhow::Result<ConfiguredAttr> {
        Ok(match self {
            CoercedAttrExtraTypes::ExplicitConfiguredDep(dep) => {
                ExplicitConfiguredDepAttrType::configure(ctx, dep)?
            }
            CoercedAttrExtraTypes::SplitTransitionDep(dep) => {
                SplitTransitionDepAttrType::configure(ctx, dep)?
            }
            CoercedAttrExtraTypes::ConfiguredDep(dep) => ConfiguredAttr::Dep(dep.clone()),
            CoercedAttrExtraTypes::ConfigurationDep(dep) => {
                ConfigurationDepAttrType::configure(ctx, dep)?
            }
            CoercedAttrExtraTypes::Dep(dep) => DepAttrType::configure(ctx, dep)?,
            CoercedAttrExtraTypes::SourceLabel(source) => ConfiguredAttr::SourceLabel(Box::new(
                source.configure_pair(ctx.cfg().cfg_pair().dupe()),
            )),
            CoercedAttrExtraTypes::Label(label) => LabelAttrType::configure(ctx, label)?,
            CoercedAttrExtraTypes::Arg(arg) => ConfiguredAttr::Arg(arg.configure(ctx)?),
            CoercedAttrExtraTypes::Query(query) => {
                ConfiguredAttr::Query(Box::new(query.configure(ctx)?))
            }
            CoercedAttrExtraTypes::SourceFile(s) => ConfiguredAttr::SourceFile(s.clone()),
        })
    }
}

impl AttrConfig for CoercedAttr {
    type ProvidersType = ProvidersLabel;
}

impl ToJsonWithContext for CoercedAttr {
    fn to_json(&self, ctx: &AttrFmtContext) -> anyhow::Result<serde_json::Value> {
        CoercedAttr::to_json(self, ctx)
    }
}

impl AnyMatches for CoercedAttr {
    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool> {
        CoercedAttr::any_matches(self, filter)
    }
}

pub(crate) fn source_file_display<'a>(
    ctx: &'a AttrFmtContext,
    source_file: &'a CoercedPath,
) -> impl Display + 'a {
    match &ctx.package {
        Some(pkg) => Either::Left(BuckPathRef::new(pkg.dupe(), source_file.path())),
        None => {
            // This code is unreachable, but better this than panic.
            Either::Right(format!("<no package>/{}", source_file.path()))
        }
    }
}
