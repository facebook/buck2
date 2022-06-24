/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::hash::Hash;
use std::sync::Arc;

use buck2_core::buck_path::BuckPath;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::target::TargetLabel;
use buck2_interpreter::types::label::Label;
use buck2_interpreter::types::label::LabelGen;
use buck2_node::attrs::attr_type::attr_literal::AttrLiteral;
use buck2_node::attrs::attr_type::configuration_dep::ConfigurationDepAttrType;
use buck2_node::attrs::attr_type::configured_dep::ExplicitConfiguredDepAttrType;
use buck2_node::attrs::attr_type::dep::DepAttrType;
use buck2_node::attrs::attr_type::label::LabelAttrType;
use buck2_node::attrs::attr_type::query::ResolvedQueryLiterals;
use buck2_node::attrs::attr_type::source::SourceAttrType;
use buck2_node::attrs::attr_type::split_transition_dep::SplitTransitionDepAttrType;
use buck2_node::attrs::configuration_context::AttrConfigurationContext;
use buck2_node::attrs::configured_attr::ConfiguredAttr;
use buck2_node::attrs::traversal::CoercedAttrTraversal;
use gazebo::prelude::*;
use starlark::collections::SmallMap;
use starlark::collections::SmallSet;
use starlark::values::dict::Dict;
use starlark::values::none::NoneType;
use starlark::values::FrozenValue;
use starlark::values::Heap;
use starlark::values::StarlarkValue;
use starlark::values::Value;

use crate::attrs::analysis::AttrResolutionContext;
use crate::attrs::attr_type::arg::value::ResolvedStringWithMacros;
use crate::attrs::attr_type::arg::ConfiguredStringWithMacrosExt;
use crate::attrs::attr_type::arg::UnconfiguredStringWithMacrosExt;
use crate::attrs::attr_type::configuration_dep::ConfigurationDepAttrTypeExt;
use crate::attrs::attr_type::dep::ConfiguredDepAttrExt;
use crate::attrs::attr_type::dep::ConfiguredExplicitConfiguredDepExt;
use crate::attrs::attr_type::dep::DepAttrTypeExt;
use crate::attrs::attr_type::dep::ExplicitConfiguredDepAttrTypeExt;
use crate::attrs::attr_type::label::LabelAttrTypeExt;
use crate::attrs::attr_type::query::ConfiguredQueryAttrExt;
use crate::attrs::attr_type::query::UnconfiguredQueryAttrExt;
use crate::attrs::attr_type::source::SourceAttrTypeExt;
use crate::attrs::attr_type::split_transition_dep::SplitTransitionDepAttrTypeExt;
use crate::attrs::configured_attr::ConfiguredAttrExt;
use crate::attrs::CoercedAttr;
use crate::interpreter::rule_defs::artifact::StarlarkArtifact;
use crate::interpreter::rule_defs::provider::dependency::DependencyGen;

static_assertions::assert_eq_size!(AttrLiteral<CoercedAttr>, [usize; 4]);
static_assertions::assert_eq_size!(AttrLiteral<ConfiguredAttr>, [usize; 4]);

pub(crate) trait UnconfiguredAttrLiteralExt {
    fn to_value<'v>(&self, heap: &'v Heap) -> anyhow::Result<Value<'v>>;

    fn configure(&self, ctx: &dyn AttrConfigurationContext) -> anyhow::Result<ConfiguredAttr>;

    fn traverse<'a>(&'a self, traversal: &mut dyn CoercedAttrTraversal<'a>) -> anyhow::Result<()>;
}

impl UnconfiguredAttrLiteralExt for AttrLiteral<CoercedAttr> {
    fn to_value<'v>(&self, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
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

    fn configure(&self, ctx: &dyn AttrConfigurationContext) -> anyhow::Result<ConfiguredAttr> {
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

    fn traverse<'a>(&'a self, traversal: &mut dyn CoercedAttrTraversal<'a>) -> anyhow::Result<()> {
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

pub(crate) trait ConfiguredAttrLiteralExt {
    fn traverse<'a>(
        &'a self,
        traversal: &mut dyn ConfiguredAttrTraversal<'a>,
    ) -> anyhow::Result<()>;

    fn resolve_single<'v>(&self, ctx: &'v dyn AttrResolutionContext) -> anyhow::Result<Value<'v>>;

    fn resolve<'v>(&self, ctx: &'v dyn AttrResolutionContext) -> anyhow::Result<Vec<Value<'v>>>;

    fn resolved_starlark_type(&self) -> anyhow::Result<&'static str>;
}

impl ConfiguredAttrLiteralExt for AttrLiteral<ConfiguredAttr> {
    fn traverse<'a>(
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

    fn resolve_single<'v>(&self, ctx: &'v dyn AttrResolutionContext) -> anyhow::Result<Value<'v>> {
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
            AttrLiteral::SourceFile(s) => Ok(SourceAttrType::resolve_single_file(ctx, s.path())),
            AttrLiteral::SourceLabel(s) => SourceAttrType::resolve_single_label(ctx, s),
            AttrLiteral::Arg(arg) => arg.resolve(ctx),
            AttrLiteral::Label(label) => {
                let label = Label::new(ctx.heap(), *label.clone());
                Ok(ctx.heap().alloc(label))
            }
        }
    }

    fn resolve<'v>(&self, ctx: &'v dyn AttrResolutionContext) -> anyhow::Result<Vec<Value<'v>>> {
        match self {
            // SourceLabel is special since it is the only type that can be expand to many
            AttrLiteral::SourceLabel(src) => SourceAttrType::resolve_label(ctx, src),
            _ => Ok(vec![self.resolve_single(ctx)?]),
        }
    }

    fn resolved_starlark_type(&self) -> anyhow::Result<&'static str> {
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
