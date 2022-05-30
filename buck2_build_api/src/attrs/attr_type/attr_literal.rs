/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    fmt::{Debug, Display},
    hash::Hash,
    sync::Arc,
};

use buck2_core::{
    provider::{ConfiguredProvidersLabel, ProvidersLabelMaybeConfigured},
    target::{TargetLabel, TargetLabelMaybeConfigured},
};
use gazebo::prelude::*;
use starlark::{
    collections::{SmallMap, SmallSet},
    values::{dict::Dict, none::NoneType, FrozenValue, Heap, StarlarkValue, Value},
};

use crate::{
    attrs::{
        attr_type::{
            arg::{value::ResolvedStringWithMacros, StringWithMacros},
            configuration_dep::ConfigurationDepAttrType,
            dep::{
                DepAttr, DepAttrType, ExplicitConfiguredDepAttrType,
                ExplicitConfiguredDepMaybeConfigured,
            },
            label::LabelAttrType,
            query::{QueryAttr, ResolvedQueryLiterals},
            source::SourceAttrType,
            split_transition_dep::{SplitTransitionDepAttrType, SplitTransitionDepMaybeConfigured},
            AttrType,
        },
        AttrConfigurationContext, AttrResolutionContext, CoercedAttr, CoercedAttrTraversal,
        ConfiguredAttr,
    },
    interpreter::rule_defs::{
        artifact::StarlarkArtifact,
        label::{Label, LabelGen},
        provider::DependencyGen,
        transition::id::TransitionId,
    },
    path::BuckPath,
};

pub trait AttrLike: Display + Debug + Clone + Eq + PartialEq + Hash + Send + Sync {}

impl<T: Display + Debug + Clone + Eq + Hash + Send + Sync + PartialEq> AttrLike for T {}

/// AttrConfig is used to implement things just once to cover both the configured and
/// unconfigured case. For example, a Vec<C::TargetType> where C: AttrConfig, would be
/// a Vec<TargetLabel> in the unconfigured case and a Vec<ConfiguredTargetLabel> in the
/// configured case.
///
/// For attributes, the difference between the coerced value and the configured value is
/// (1) selects are resolved and (2) configurable things are configured. This trait allows
/// most of the attr representation to be shared between those two states.
///
/// There's really just two implementations of this, one for coerced attrs with
/// unconfigured types and one for configured attrs with the configured types.
pub trait AttrConfig: AttrLike {
    type TargetType: TargetLabelMaybeConfigured + AttrLike;
    type ProvidersType: ProvidersLabelMaybeConfigured + AttrLike;
    type SplitTransitionDepType: SplitTransitionDepMaybeConfigured + AttrLike;
    type ExplicitConfiguredDepType: ExplicitConfiguredDepMaybeConfigured + AttrLike;

    fn to_json(&self) -> anyhow::Result<serde_json::Value>;

    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool>;
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum AttrLiteral<C: AttrConfig> {
    Bool(bool),
    Int(i32),
    String(String),
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
    SourceFile(Box<BuckPath>),
    Arg(StringWithMacros<C>),
    // NOTE: unlike deps, labels are not traversed, as they are typically used in lieu of deps in
    // cases that would cause cycles.
    Label(Box<C::ProvidersType>),
}

static_assertions::assert_eq_size!(AttrLiteral<CoercedAttr>, [usize; 5]);
static_assertions::assert_eq_size!(AttrLiteral<ConfiguredAttr>, [usize; 5]);

impl<C: AttrConfig> AttrLiteral<C> {
    pub fn to_json(&self) -> anyhow::Result<serde_json::Value> {
        use serde_json::to_value;
        match self {
            AttrLiteral::Bool(v) => Ok(to_value(v)?),
            AttrLiteral::Int(v) => Ok(to_value(v)?),
            AttrLiteral::String(v) => Ok(to_value(v)?),
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
            AttrLiteral::SourceFile(s) => Ok(to_value(s.to_string())?),
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
            AttrLiteral::String(v) => filter(v),
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
            AttrLiteral::SourceFile(s) => filter(&s.to_string()),
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
            AttrLiteral::String(v) => {
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
            AttrLiteral::SourceFile(v) => write!(f, "\"{}\"", v),
            AttrLiteral::Arg(a) => write!(f, "\"{}\"", a),
            AttrLiteral::SplitTransitionDep(d) => Display::fmt(d, f),
            AttrLiteral::Label(l) => write!(f, "\"{}\"", l),
        }
    }
}

impl AttrLiteral<CoercedAttr> {
    #[allow(dead_code)] // TODO(nga): to be used in transition rules.
    pub(crate) fn to_value<'v>(&self, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        match self {
            AttrLiteral::None => Ok(Value::new_none()),
            AttrLiteral::Bool(b) => Ok(Value::new_bool(*b)),
            AttrLiteral::Int(i) => Ok(Value::new_int(*i)),
            AttrLiteral::String(s) => Ok(heap.alloc_str(s).to_value()),
            AttrLiteral::List(l, _) => {
                let mut v = Vec::with_capacity(l.len());
                for e in l.iter() {
                    v.push(e.to_value(heap)?);
                }
                Ok(heap.alloc_list(&v))
            }
            AttrLiteral::Tuple(l) => {
                let mut v = Vec::with_capacity(l.len());
                for e in l.iter() {
                    v.push(e.to_value(heap)?);
                }
                Ok(heap.alloc_tuple(&v))
            }
            AttrLiteral::Dict(d) => {
                let mut m = SmallMap::with_capacity(d.len());
                for (k, v) in d {
                    m.insert_hashed(k.to_value(heap)?.get_hashed()?, v.to_value(heap)?);
                }
                Ok(heap.alloc(Dict::new(m)))
            }
            x => {
                // For now this function is used to convert attributes to Starlark values
                // for transition rules which access attributes.
                //
                // For regular deps this function should fail.
                //
                // For configuration deps, this function should resolve attributes to providers,
                // but it is not implemented yet.
                Err(CoercionError::AttrCannotBeConvertedToValue(x.to_string()).into())
            }
        }
    }

    pub(crate) fn configure(
        &self,
        ctx: &dyn AttrConfigurationContext,
    ) -> anyhow::Result<ConfiguredAttr> {
        Ok(ConfiguredAttr(match self {
            AttrLiteral::Bool(v) => AttrLiteral::Bool(*v),
            AttrLiteral::Int(v) => AttrLiteral::Int(*v),
            AttrLiteral::String(v) => AttrLiteral::String(v.clone()),
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
            AttrLiteral::SourceFile(box s) => traversal.input(s),
            AttrLiteral::SourceLabel(box s) => traversal.dep(s.target()),
            AttrLiteral::Arg(arg) => arg.traverse(traversal),
            AttrLiteral::Label(label) => traversal.label(label),
        }
    }
}

pub trait ConfiguredAttrTraversal<'a> {
    fn dep(&mut self, dep: &'a ConfiguredProvidersLabel) -> anyhow::Result<()>;

    fn exec_dep(&mut self, dep: &'a ConfiguredProvidersLabel) -> anyhow::Result<()> {
        // By default, just treat it as a dep. Most things don't care about the distinction.
        self.dep(dep)
    }

    fn configuration_dep(&mut self, _dep: &'a TargetLabel) -> anyhow::Result<()> {
        Ok(())
    }

    fn query_macro(
        &mut self,
        _query: &'a str,
        _resolved_literals: &'a ResolvedQueryLiterals<ConfiguredAttr>,
    ) -> anyhow::Result<()> {
        Ok(())
    }

    fn input(&mut self, _path: &'a BuckPath) -> anyhow::Result<()> {
        Ok(())
    }

    fn label(&mut self, _label: &'a ConfiguredProvidersLabel) -> anyhow::Result<()> {
        Ok(())
    }
}

#[derive(Default, Debug)]
pub struct ConfiguredAttrInfo {
    // Including transitioned deps.
    pub deps: SmallSet<ConfiguredProvidersLabel>,
    pub execution_deps: SmallSet<ConfiguredProvidersLabel>,
    pub has_query: bool,
}

impl ConfiguredAttrInfo {
    pub fn new() -> Self {
        Self::default()
    }
}

impl<'a> ConfiguredAttrTraversal<'a> for ConfiguredAttrInfo {
    fn dep(&mut self, dep: &'a ConfiguredProvidersLabel) -> anyhow::Result<()> {
        self.deps.insert(dep.clone());
        Ok(())
    }

    fn query_macro(
        &mut self,
        _query: &'a str,
        _resolved_literals: &'a ResolvedQueryLiterals<ConfiguredAttr>,
    ) -> anyhow::Result<()> {
        self.has_query = true;
        Ok(())
    }

    fn exec_dep(&mut self, dep: &'a ConfiguredProvidersLabel) -> anyhow::Result<()> {
        self.execution_deps.insert(dep.clone());
        Ok(())
    }
}

impl AttrLiteral<ConfiguredAttr> {
    pub fn traverse<'a>(
        &'a self,
        traversal: &mut dyn ConfiguredAttrTraversal<'a>,
    ) -> anyhow::Result<()> {
        match self {
            AttrLiteral::Bool(_) => Ok(()),
            AttrLiteral::Int(_) => Ok(()),
            AttrLiteral::String(_) => Ok(()),
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
            AttrLiteral::SourceFile(box input) => traversal.input(input),
            AttrLiteral::SourceLabel(box dep) => traversal.dep(dep),
            AttrLiteral::Arg(arg) => arg.traverse(traversal),
            AttrLiteral::Label(label) => traversal.label(label),
        }
    }

    pub(crate) fn resolve_single<'v>(
        &self,
        ctx: &'v dyn AttrResolutionContext,
    ) -> anyhow::Result<Value<'v>> {
        match self {
            AttrLiteral::Bool(v) => Ok(Value::new_bool(*v)),
            AttrLiteral::Int(v) => Ok(Value::new_int(*v)),
            AttrLiteral::String(v) => Ok(ctx.heap().alloc(v)),
            AttrLiteral::List(list, _) => {
                let mut values = Vec::with_capacity(list.len());
                for v in list.iter() {
                    values.append(&mut v.resolve(ctx)?);
                }
                Ok(ctx.heap().alloc(values))
            }
            AttrLiteral::Tuple(list) => {
                let mut values = Vec::with_capacity(list.len());
                for v in list.iter() {
                    values.append(&mut v.resolve(ctx)?);
                }
                Ok(ctx.heap().alloc_tuple(&values))
            }
            AttrLiteral::Dict(dict) => {
                let mut res = SmallMap::with_capacity(dict.len());
                for (k, v) in dict {
                    res.insert_hashed(k.resolve_single(ctx)?.get_hashed()?, v.resolve_single(ctx)?);
                }
                Ok(ctx.heap().alloc(Dict::new(res)))
            }
            AttrLiteral::None => Ok(Value::new_none()),
            AttrLiteral::Dep(d) => DepAttrType::resolve_single(ctx, d),
            AttrLiteral::ConfiguredDep(..) => Err(anyhow::anyhow!(
                "ConfiguredDep attr type is used only in internal forward rules \
                    which have no corresponding starlark impl"
            )),
            AttrLiteral::ConfigurationDep(d) => ConfigurationDepAttrType::resolve_single(ctx, d),
            AttrLiteral::ExplicitConfiguredDep(d) => {
                ExplicitConfiguredDepAttrType::resolve_single(ctx, d)
            }
            AttrLiteral::SplitTransitionDep(deps) => {
                SplitTransitionDepAttrType::resolve_single(ctx, deps)
            }
            AttrLiteral::Query(query) => query.resolve(ctx),
            AttrLiteral::SourceFile(s) => Ok(SourceAttrType::resolve_single_file(ctx, s)),
            AttrLiteral::SourceLabel(s) => SourceAttrType::resolve_single_label(ctx, s),
            AttrLiteral::Arg(arg) => arg.resolve(ctx),
            AttrLiteral::Label(label) => {
                let label = Label::new(ctx.heap(), *label.clone());
                Ok(ctx.heap().alloc(label))
            }
        }
    }

    pub(crate) fn resolve<'v>(
        &self,
        ctx: &'v dyn AttrResolutionContext,
    ) -> anyhow::Result<Vec<Value<'v>>> {
        match self {
            // SourceLabel is special since it is the only type that can be expand to many
            AttrLiteral::SourceLabel(src) => SourceAttrType::resolve_label(ctx, src),
            _ => Ok(vec![self.resolve_single(ctx)?]),
        }
    }

    pub fn resolved_starlark_type(&self) -> anyhow::Result<&'static str> {
        match self {
            AttrLiteral::Bool(_) => Ok(starlark::values::bool::BOOL_TYPE),
            AttrLiteral::Int(_) => Ok(starlark::values::int::INT_TYPE),
            AttrLiteral::String(_) => Ok(starlark::values::string::STRING_TYPE),
            AttrLiteral::List(_, _) => Ok(starlark::values::list::List::TYPE),
            AttrLiteral::Tuple(_) => Ok(starlark::values::tuple::Tuple::TYPE),
            AttrLiteral::Dict(_) => Ok(Dict::TYPE),
            AttrLiteral::None => Ok(NoneType::TYPE),
            AttrLiteral::Dep(_) => {
                Ok(DependencyGen::<FrozenValue>::get_type_value_static().as_str())
            }
            AttrLiteral::ConfiguredDep(_) => Err(anyhow::anyhow!(
                "ConfiguredDep attr type is used only in internal forward rules \
                    which have no corresponding starlark impl"
            )),
            AttrLiteral::ExplicitConfiguredDep(_) => {
                Ok(DependencyGen::<FrozenValue>::get_type_value_static().as_str())
            }
            AttrLiteral::ConfigurationDep(_) => Ok(starlark::values::string::STRING_TYPE),
            AttrLiteral::SplitTransitionDep(_) => Ok(Dict::TYPE),
            AttrLiteral::Query(_) => Ok(starlark::values::list::List::TYPE),
            AttrLiteral::SourceLabel(_) => Ok(StarlarkArtifact::get_type_value_static().as_str()),
            AttrLiteral::SourceFile(_) => Ok(StarlarkArtifact::get_type_value_static().as_str()),
            AttrLiteral::Arg(_) => Ok(ResolvedStringWithMacros::get_type_value_static().as_str()),
            AttrLiteral::Label(_) => Ok(LabelGen::<FrozenValue>::get_type_value_static().as_str()),
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub(crate) enum CoercionError {
    #[error("Expected value of type `{0}`, got value with type `{1}` (value was `{2}`)")]
    TypeError(String, String, String),
    #[error("Expected target label without name. Got `{0}`")]
    UnexpectedProvidersName(String),
    #[error("Used one_of with an empty list.")]
    OneOfEmpty,
    #[error("one_of fails, the errors against each alternative in turn were:\n{}", .0.map(|x| format!("{:#}", x)).join("\n"))]
    OneOfMany(Vec<anyhow::Error>),
    #[error("default_only is not allowed to be specified, but got `{0}`")]
    DefaultOnly(String),
    #[error("enum called with `{0}`, only allowed: {}", .1.map(|x| format!("`{}`", x)).join(", "))]
    InvalidEnumVariant(String, Vec<String>),
    #[error("Attribute cannot be converted to Starlark value: `{0}`")]
    AttrCannotBeConvertedToValue(String),
}

impl CoercionError {
    pub fn type_error(expected_type: &str, value: Value) -> CoercionError {
        CoercionError::TypeError(
            expected_type.to_owned(),
            value.get_type().to_owned(),
            value.to_repr(),
        )
    }

    pub fn unexpected_providers_name(val: &str) -> CoercionError {
        CoercionError::UnexpectedProvidersName(val.to_owned())
    }

    pub fn one_of_empty() -> CoercionError {
        CoercionError::OneOfEmpty
    }

    pub fn one_of_many(mut errs: Vec<anyhow::Error>) -> anyhow::Error {
        if errs.is_empty() {
            Self::one_of_empty().into()
        } else if errs.len() == 1 {
            errs.pop().unwrap()
        } else {
            CoercionError::OneOfMany(errs).into()
        }
    }

    pub fn invalid_enum(got: &str, wanted: Vec<String>) -> CoercionError {
        CoercionError::InvalidEnumVariant(got.to_owned(), wanted)
    }

    pub fn default_only(value: Value) -> CoercionError {
        CoercionError::DefaultOnly(value.to_string())
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct CoercedDepsCollector {
    /// Contains the deps derived from the attributes.
    /// Does not include the transition, exec or configuration deps.
    pub deps: SmallSet<TargetLabel>,

    /// Contains the deps which are transitioned to other configuration
    /// (including split transitions).
    pub transition_deps: SmallSet<(TargetLabel, Arc<TransitionId>)>,

    /// Contains the execution deps derived from the attributes.
    pub exec_deps: SmallSet<TargetLabel>,

    /// Contains the configuration deps. These are deps that appear as conditions in selects.
    pub configuration_deps: SmallSet<TargetLabel>,

    /// Contains platform targets of configured_alias()
    pub platform_deps: SmallSet<TargetLabel>,
}

impl CoercedDepsCollector {
    pub fn new() -> Self {
        Self {
            deps: SmallSet::new(),
            exec_deps: SmallSet::new(),
            transition_deps: SmallSet::new(),
            configuration_deps: SmallSet::new(),
            platform_deps: SmallSet::new(),
        }
    }
}

impl<'a> CoercedAttrTraversal<'a> for CoercedDepsCollector {
    fn dep(&mut self, dep: &'a TargetLabel) -> anyhow::Result<()> {
        self.deps.insert(dep.dupe());
        Ok(())
    }

    fn exec_dep(&mut self, dep: &'a TargetLabel) -> anyhow::Result<()> {
        self.exec_deps.insert(dep.dupe());
        Ok(())
    }

    fn transition_dep(
        &mut self,
        dep: &'a TargetLabel,
        tr: &Arc<TransitionId>,
    ) -> anyhow::Result<()> {
        self.transition_deps.insert((dep.dupe(), tr.dupe()));
        Ok(())
    }

    fn split_transition_dep(
        &mut self,
        dep: &'a TargetLabel,
        tr: &Arc<TransitionId>,
    ) -> anyhow::Result<()> {
        self.transition_deps.insert((dep.dupe(), tr.dupe()));
        Ok(())
    }

    fn configuration_dep(&mut self, dep: &'a TargetLabel) -> anyhow::Result<()> {
        self.configuration_deps.insert(dep.dupe());
        Ok(())
    }

    fn platform_dep(&mut self, dep: &'a TargetLabel) -> anyhow::Result<()> {
        self.platform_deps.insert(dep.dupe());
        Ok(())
    }

    fn input(&mut self, _input: &'a BuckPath) -> anyhow::Result<()> {
        Ok(())
    }
}
