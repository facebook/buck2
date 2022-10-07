/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;

use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::target::TargetLabel;
use gazebo::prelude::*;

use crate::attrs::attr_type::arg::StringWithMacros;
use crate::attrs::attr_type::attr_config::AttrConfig;
use crate::attrs::attr_type::configuration_dep::ConfigurationDepAttrType;
use crate::attrs::attr_type::configured_dep::ExplicitConfiguredDepAttrType;
use crate::attrs::attr_type::dep::DepAttr;
use crate::attrs::attr_type::dep::DepAttrType;
use crate::attrs::attr_type::dep::ExplicitConfiguredDepMaybeConfigured;
use crate::attrs::attr_type::label::LabelAttrType;
use crate::attrs::attr_type::query::QueryAttr;
use crate::attrs::attr_type::split_transition_dep::SplitTransitionDepAttrType;
use crate::attrs::attr_type::split_transition_dep::SplitTransitionDepMaybeConfigured;
use crate::attrs::attr_type::AttrType;
use crate::attrs::coerced_attr::CoercedAttr;
use crate::attrs::coerced_path::CoercedPath;
use crate::attrs::configuration_context::AttrConfigurationContext;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::configured_traversal::ConfiguredAttrTraversal;
use crate::attrs::traversal::CoercedAttrTraversal;

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum AttrLiteral<C: AttrConfig> {
    Bool(bool),
    Int(i32),
    // Note we store `String`, not `Arc<str>` here, because we store full attributes
    // in unconfigured target node, but configured target node is basically a pair
    // (reference to unconfigured target node, configuration).
    //
    // Configured attributes are created on demand and destroyed immediately after use.
    //
    // So when working with configured attributes with pay with CPU for string copies,
    // but don't increase total memory usage, because these string copies are short living.
    String(String),
    // Like String, but drawn from a set of variants, so doesn't support concat
    EnumVariant(String),
    // Type of list elements is used to verify that concatenation is valid.
    // That only can be checked after configuration took place,
    // so pass the type info together with values to be used later.
    List(Box<[C]>, AttrType),
    // We make Tuple a Box<[C]> so we can share code paths with List
    Tuple(Box<[C]>),
    Dict(Vec<(C, C)>),
    None,
    Dep(Box<DepAttr<C::ProvidersType>>),
    ConfiguredDep(Box<DepAttr<ConfiguredProvidersLabel>>),
    ExplicitConfiguredDep(Box<C::ExplicitConfiguredDepType>),
    ConfigurationDep(TargetLabel),
    SplitTransitionDep(Box<C::SplitTransitionDepType>),
    Query(Box<QueryAttr<C>>),
    SourceLabel(Box<C::ProvidersType>),
    SourceFile(Box<CoercedPath>),
    Arg(StringWithMacros<C>),
    // NOTE: unlike deps, labels are not traversed, as they are typically used in lieu of deps in
    // cases that would cause cycles.
    Label(Box<C::ProvidersType>),
}

impl<C: AttrConfig> Display for AttrLiteral<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AttrLiteral::Bool(v) => {
                let s = if *v { "True" } else { "False" };
                write!(f, "{}", s)
            }
            AttrLiteral::Int(v) => {
                write!(f, "{}", v)
            }
            AttrLiteral::String(v) | AttrLiteral::EnumVariant(v) => {
                if f.alternate() {
                    f.write_str(v)
                } else {
                    write!(f, "\"{}\"", v)
                }
            }
            AttrLiteral::List(v, _) => {
                write!(f, "[")?;
                for (i, v) in v.iter().enumerate() {
                    if i != 0 {
                        write!(f, ",")?;
                    }
                    Display::fmt(v, f)?;
                }
                write!(f, "]")?;
                Ok(())
            }
            AttrLiteral::Tuple(v) => {
                write!(f, "(")?;
                for (i, v) in v.iter().enumerate() {
                    if i != 0 {
                        write!(f, ",")?;
                    }
                    Display::fmt(v, f)?;
                }
                write!(f, ")")?;
                Ok(())
            }
            AttrLiteral::Dict(v) => {
                write!(f, "{{")?;
                for (i, (k, v)) in v.iter().enumerate() {
                    if i != 0 {
                        write!(f, ",")?;
                    }
                    write!(f, "{}: {}", k, v)?;
                }
                write!(f, "}}")?;
                Ok(())
            }
            AttrLiteral::None => write!(f, "None"),
            AttrLiteral::Dep(v) => write!(f, "\"{}\"", v),
            AttrLiteral::ConfiguredDep(v) => write!(f, "\"{}\"", v),
            AttrLiteral::ExplicitConfiguredDep(d) => Display::fmt(d, f),
            AttrLiteral::ConfigurationDep(v) => write!(f, "\"{}\"", v),
            AttrLiteral::Query(v) => write!(f, "\"{}\"", v.query()),
            AttrLiteral::SourceLabel(v) => write!(f, "\"{}\"", v),
            AttrLiteral::SourceFile(v) => write!(f, "\"{}\"", v.path()),
            AttrLiteral::Arg(a) => write!(f, "\"{}\"", a),
            AttrLiteral::SplitTransitionDep(d) => Display::fmt(d, f),
            AttrLiteral::Label(l) => write!(f, "\"{}\"", l),
        }
    }
}

impl<C: AttrConfig> AttrLiteral<C> {
    pub fn to_json(&self) -> anyhow::Result<serde_json::Value> {
        use serde_json::to_value;
        match self {
            AttrLiteral::Bool(v) => Ok(to_value(v)?),
            AttrLiteral::Int(v) => Ok(to_value(v)?),
            AttrLiteral::String(v) | AttrLiteral::EnumVariant(v) => Ok(to_value(v)?),
            AttrLiteral::List(list, _) | AttrLiteral::Tuple(list) => {
                Ok(to_value(list.try_map(|c| c.to_json())?)?)
            }
            AttrLiteral::Dict(dict) => {
                let mut res: serde_json::Map<String, serde_json::Value> =
                    serde_json::Map::with_capacity(dict.len());
                for (k, v) in dict {
                    res.insert(k.to_json()?.as_str().unwrap().to_owned(), v.to_json()?);
                }
                Ok(res.into())
            }
            AttrLiteral::None => Ok(serde_json::Value::Null),
            AttrLiteral::Dep(l) => Ok(to_value(l.to_string())?),
            AttrLiteral::ConfiguredDep(l) => Ok(to_value(l.to_string())?),
            AttrLiteral::ExplicitConfiguredDep(l) => l.to_json(),
            AttrLiteral::Query(q) => Ok(to_value(q.query())?),
            AttrLiteral::SourceFile(s) => Ok(to_value(s.path().to_string())?),
            AttrLiteral::SourceLabel(s) => Ok(to_value(s.to_string())?),
            AttrLiteral::Arg(a) => Ok(to_value(a.to_string())?),
            AttrLiteral::ConfigurationDep(l) => Ok(to_value(l.to_string())?),
            AttrLiteral::SplitTransitionDep(l) => l.to_json(),
            AttrLiteral::Label(l) => Ok(to_value(l.to_string())?),
        }
    }

    /// Checks if this attr matches the filter. For container-like things, will return true if any contained item matches the filter.
    pub fn any_matches(
        &self,
        filter: &dyn Fn(&str) -> anyhow::Result<bool>,
    ) -> anyhow::Result<bool> {
        match self {
            AttrLiteral::String(v) | AttrLiteral::EnumVariant(v) => filter(v),
            AttrLiteral::Tuple(vals) | AttrLiteral::List(vals, _) => {
                for v in vals.iter() {
                    if v.any_matches(filter)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            AttrLiteral::Dict(d) => {
                for (k, v) in d {
                    if k.any_matches(filter)? || v.any_matches(filter)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            AttrLiteral::None => Ok(false),
            AttrLiteral::Dep(d) => filter(&d.to_string()),
            AttrLiteral::ConfiguredDep(d) => filter(&d.to_string()),
            AttrLiteral::ExplicitConfiguredDep(d) => d.any_matches(filter),
            AttrLiteral::SourceFile(s) => filter(&s.path().to_string()),
            AttrLiteral::SourceLabel(s) => filter(&s.to_string()),
            AttrLiteral::Query(q) => filter(q.query()),
            AttrLiteral::Arg(a) => filter(&a.to_string()),
            AttrLiteral::Bool(b) => filter(if *b { "True" } else { "False" }),
            AttrLiteral::Int(i) => filter(&i.to_string()),
            AttrLiteral::ConfigurationDep(d) => filter(&d.to_string()),
            AttrLiteral::SplitTransitionDep(d) => d.any_matches(filter),
            AttrLiteral::Label(l) => filter(&l.to_string()),
        }
    }
}

impl AttrLiteral<ConfiguredAttr> {
    pub(crate) fn traverse<'a>(
        &'a self,
        traversal: &mut dyn ConfiguredAttrTraversal<'a>,
    ) -> anyhow::Result<()> {
        match self {
            AttrLiteral::Bool(_) => Ok(()),
            AttrLiteral::Int(_) => Ok(()),
            AttrLiteral::String(_) => Ok(()),
            AttrLiteral::EnumVariant(_) => Ok(()),
            AttrLiteral::List(list, _) | AttrLiteral::Tuple(list) => {
                for v in list.iter() {
                    v.traverse(traversal)?;
                }
                Ok(())
            }
            AttrLiteral::Dict(dict) => {
                for (k, v) in dict {
                    k.traverse(traversal)?;
                    v.traverse(traversal)?;
                }
                Ok(())
            }
            AttrLiteral::None => Ok(()),
            AttrLiteral::Dep(dep) => dep.traverse(traversal),
            AttrLiteral::ConfiguredDep(dep) => dep.traverse(traversal),
            AttrLiteral::ConfigurationDep(dep) => traversal.configuration_dep(dep),
            AttrLiteral::ExplicitConfiguredDep(dep) => dep.traverse(traversal),
            AttrLiteral::SplitTransitionDep(deps) => {
                for target in deps.deps.values() {
                    traversal.dep(target)?;
                }
                Ok(())
            }
            AttrLiteral::Query(query) => query.traverse(traversal),
            AttrLiteral::SourceFile(box source) => {
                for x in source.inputs() {
                    traversal.input(x)?;
                }
                Ok(())
            }
            AttrLiteral::SourceLabel(box dep) => traversal.dep(dep),
            AttrLiteral::Arg(arg) => arg.traverse(traversal),
            AttrLiteral::Label(label) => traversal.label(label),
        }
    }
}

impl AttrLiteral<CoercedAttr> {
    pub(crate) fn configure(
        &self,
        ctx: &dyn AttrConfigurationContext,
    ) -> anyhow::Result<ConfiguredAttr> {
        Ok(ConfiguredAttr(match self {
            AttrLiteral::Bool(v) => AttrLiteral::Bool(*v),
            AttrLiteral::Int(v) => AttrLiteral::Int(*v),
            AttrLiteral::String(v) => AttrLiteral::String(v.clone()),
            AttrLiteral::EnumVariant(v) => AttrLiteral::EnumVariant(v.clone()),
            AttrLiteral::List(list, element_type) => AttrLiteral::List(
                list.try_map(|v| v.configure(ctx))?.into_boxed_slice(),
                element_type.dupe(),
            ),
            AttrLiteral::Tuple(list) => {
                AttrLiteral::Tuple(list.try_map(|v| v.configure(ctx))?.into_boxed_slice())
            }
            AttrLiteral::Dict(dict) => AttrLiteral::Dict(dict.try_map(|(k, v)| {
                let k2 = k.configure(ctx)?;
                let v2 = v.configure(ctx)?;
                anyhow::Ok((k2, v2))
            })?),
            AttrLiteral::None => AttrLiteral::None,
            AttrLiteral::Dep(dep) => DepAttrType::configure(ctx, dep)?,
            AttrLiteral::ConfiguredDep(dep) => AttrLiteral::Dep(dep.clone()),
            AttrLiteral::ExplicitConfiguredDep(dep) => {
                ExplicitConfiguredDepAttrType::configure(ctx, dep)?
            }
            AttrLiteral::ConfigurationDep(dep) => ConfigurationDepAttrType::configure(ctx, dep)?,
            AttrLiteral::SplitTransitionDep(dep) => {
                SplitTransitionDepAttrType::configure(ctx, dep)?
            }
            AttrLiteral::Query(query) => AttrLiteral::Query(box query.configure(ctx)?),
            AttrLiteral::SourceFile(s) => AttrLiteral::SourceFile(s.clone()),
            AttrLiteral::SourceLabel(box source) => {
                AttrLiteral::SourceLabel(box source.configure(ctx.cfg().dupe()))
            }
            AttrLiteral::Arg(arg) => AttrLiteral::Arg(arg.configure(ctx)?),
            AttrLiteral::Label(label) => LabelAttrType::configure(ctx, label)?,
        }))
    }

    pub(crate) fn traverse<'a>(
        &'a self,
        traversal: &mut dyn CoercedAttrTraversal<'a>,
    ) -> anyhow::Result<()> {
        match self {
            AttrLiteral::Bool(_) => Ok(()),
            AttrLiteral::Int(_) => Ok(()),
            AttrLiteral::String(_) => Ok(()),
            AttrLiteral::EnumVariant(_) => Ok(()),
            AttrLiteral::List(list, _) | AttrLiteral::Tuple(list) => {
                for v in list.iter() {
                    v.traverse(traversal)?;
                }
                Ok(())
            }
            AttrLiteral::Dict(dict) => {
                for (k, v) in dict {
                    k.traverse(traversal)?;
                    v.traverse(traversal)?;
                }
                Ok(())
            }
            AttrLiteral::None => Ok(()),
            AttrLiteral::Dep(dep) => dep.traverse(traversal),
            AttrLiteral::ConfigurationDep(dep) => traversal.configuration_dep(dep),
            AttrLiteral::ConfiguredDep(dep) => traversal.dep(dep.label().target().unconfigured()),
            AttrLiteral::ExplicitConfiguredDep(dep) => dep.traverse(traversal),
            AttrLiteral::SplitTransitionDep(dep) => {
                traversal.split_transition_dep(dep.label.target(), &dep.transition)
            }
            AttrLiteral::Query(query) => query.traverse(traversal),
            AttrLiteral::SourceFile(box source) => {
                for x in source.inputs() {
                    traversal.input(x)?;
                }
                Ok(())
            }
            AttrLiteral::SourceLabel(box s) => traversal.dep(s.target()),
            AttrLiteral::Arg(arg) => arg.traverse(traversal),
            AttrLiteral::Label(label) => traversal.label(label),
        }
    }
}
