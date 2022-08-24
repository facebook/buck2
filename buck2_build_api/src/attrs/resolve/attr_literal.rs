/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;

use buck2_interpreter::types::label::Label;
use buck2_interpreter::types::label::LabelGen;
use buck2_interpreter::types::target_label::StarlarkTargetLabel;
use buck2_node::attrs::attr_type::attr_literal::AttrLiteral;
use buck2_node::attrs::attr_type::configuration_dep::ConfigurationDepAttrType;
use buck2_node::attrs::attr_type::configured_dep::ExplicitConfiguredDepAttrType;
use buck2_node::attrs::attr_type::dep::DepAttrType;
use buck2_node::attrs::attr_type::source::SourceAttrType;
use buck2_node::attrs::attr_type::split_transition_dep::SplitTransitionDepAttrType;
use buck2_node::attrs::configured_attr::ConfiguredAttr;
use gazebo::prelude::*;
use starlark::collections::SmallMap;
use starlark::values::dict::Dict;
use starlark::values::none::NoneType;
use starlark::values::FrozenValue;
use starlark::values::Heap;
use starlark::values::StarlarkValue;
use starlark::values::Value;

use crate::actions::artifact::Artifact;
use crate::actions::artifact::SourceArtifact;
use crate::attrs::resolve::attr_type::arg::ConfiguredStringWithMacrosExt;
use crate::attrs::resolve::attr_type::configuration_dep::ConfigurationDepAttrTypeExt;
use crate::attrs::resolve::attr_type::dep::DepAttrTypeExt;
use crate::attrs::resolve::attr_type::dep::ExplicitConfiguredDepAttrTypeExt;
use crate::attrs::resolve::attr_type::query::ConfiguredQueryAttrExt;
use crate::attrs::resolve::attr_type::source::SourceAttrTypeExt;
use crate::attrs::resolve::attr_type::split_transition_dep::SplitTransitionDepAttrTypeExt;
use crate::attrs::resolve::coerced_attr::CoercedAttrResolveExt;
use crate::attrs::resolve::configured_attr::ConfiguredAttrExt;
use crate::attrs::resolve::ctx::AttrResolutionContext;
use crate::attrs::CoercedAttr;
use crate::interpreter::rule_defs::artifact::StarlarkArtifact;
use crate::interpreter::rule_defs::provider::dependency::DependencyGen;

static_assertions::assert_eq_size!(AttrLiteral<CoercedAttr>, [usize; 4]);
static_assertions::assert_eq_size!(AttrLiteral<ConfiguredAttr>, [usize; 4]);

pub(crate) trait UnconfiguredAttrLiteralExt {
    fn to_value<'v>(&self, heap: &'v Heap) -> anyhow::Result<Value<'v>>;
}

impl UnconfiguredAttrLiteralExt for AttrLiteral<CoercedAttr> {
    fn to_value<'v>(&self, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        match self {
            AttrLiteral::None => Ok(Value::new_none()),
            AttrLiteral::Bool(b) => Ok(Value::new_bool(*b)),
            AttrLiteral::Int(i) => Ok(Value::new_int(*i)),
            AttrLiteral::String(s) | AttrLiteral::EnumVariant(s) => {
                Ok(heap.alloc_str(s).to_value())
            }
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
                Err(ResolveError::AttrCannotBeConvertedToValue(x.to_string()).into())
            }
        }
    }
}

pub(crate) trait ConfiguredAttrLiteralExt {
    fn resolve_single<'v>(&self, ctx: &'v dyn AttrResolutionContext) -> anyhow::Result<Value<'v>>;

    fn resolve<'v>(&self, ctx: &'v dyn AttrResolutionContext) -> anyhow::Result<Vec<Value<'v>>>;

    fn starlark_type(&self) -> anyhow::Result<&'static str>;

    /// Converts the configured attr to a starlark value without fully resolving
    fn to_value<'v>(&self, heap: &'v Heap) -> anyhow::Result<Value<'v>>;
}

impl ConfiguredAttrLiteralExt for AttrLiteral<ConfiguredAttr> {
    fn resolve_single<'v>(&self, ctx: &'v dyn AttrResolutionContext) -> anyhow::Result<Value<'v>> {
        match self {
            AttrLiteral::Bool(v) => Ok(Value::new_bool(*v)),
            AttrLiteral::Int(v) => Ok(Value::new_int(*v)),
            AttrLiteral::String(v) | AttrLiteral::EnumVariant(v) => Ok(ctx.heap().alloc(v)),
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

    fn starlark_type(&self) -> anyhow::Result<&'static str> {
        match self {
            AttrLiteral::Bool(_) => Ok(starlark::values::bool::BOOL_TYPE),
            AttrLiteral::Int(_) => Ok(starlark::values::int::INT_TYPE),
            AttrLiteral::String(_) | AttrLiteral::EnumVariant(_) => {
                Ok(starlark::values::string::STRING_TYPE)
            }
            AttrLiteral::List(_, _) => Ok(starlark::values::list::List::TYPE),
            AttrLiteral::Tuple(_) => Ok(starlark::values::tuple::Tuple::TYPE),
            AttrLiteral::Dict(_) => Ok(Dict::TYPE),
            AttrLiteral::None => Ok(NoneType::TYPE),
            AttrLiteral::Dep(_) => Ok(LabelGen::<FrozenValue>::get_type_value_static().as_str()),
            AttrLiteral::ConfiguredDep(_) => {
                Ok(LabelGen::<FrozenValue>::get_type_value_static().as_str())
            }
            AttrLiteral::ExplicitConfiguredDep(_) => {
                Ok(DependencyGen::<FrozenValue>::get_type_value_static().as_str())
            }
            AttrLiteral::ConfigurationDep(_) => Ok(starlark::values::string::STRING_TYPE),
            AttrLiteral::SplitTransitionDep(_) => Ok(Dict::TYPE),
            AttrLiteral::Query(_) => Ok(starlark::values::string::STRING_TYPE),
            AttrLiteral::SourceLabel(_) => {
                Ok(LabelGen::<FrozenValue>::get_type_value_static().as_str())
            }
            AttrLiteral::SourceFile(_) => Ok(StarlarkArtifact::get_type_value_static().as_str()),
            AttrLiteral::Arg(_) => Ok(starlark::values::string::STRING_TYPE),
            AttrLiteral::Label(_) => Ok(LabelGen::<FrozenValue>::get_type_value_static().as_str()),
        }
    }

    fn to_value<'v>(&self, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        Ok(match &self {
            AttrLiteral::Bool(v) => heap.alloc(*v),
            AttrLiteral::Int(v) => heap.alloc(*v),
            AttrLiteral::String(s) | AttrLiteral::EnumVariant(s) => heap.alloc(s),
            AttrLiteral::List(list, _ty) => heap.alloc(list.try_map(|v| v.to_value(heap))?),
            AttrLiteral::Tuple(v) => heap.alloc_tuple(&v.try_map(|v| v.to_value(heap))?),
            AttrLiteral::Dict(map) => {
                let mut res = SmallMap::with_capacity(map.len());

                for (k, v) in map {
                    res.insert_hashed(k.to_value(heap)?.get_hashed()?, v.to_value(heap)?);
                }

                heap.alloc(Dict::new(res))
            }
            AttrLiteral::None => Value::new_none(),
            AttrLiteral::Dep(d) => heap.alloc(Label::new(heap, d.label.clone())),
            AttrLiteral::ConfiguredDep(d) => heap.alloc(Label::new(heap, d.label.clone())),
            AttrLiteral::ExplicitConfiguredDep(d) => heap.alloc(Label::new(heap, d.label.clone())),
            AttrLiteral::ConfigurationDep(c) => heap.alloc(StarlarkTargetLabel::new(c.dupe())),
            AttrLiteral::SplitTransitionDep(t) => {
                let mut map = SmallMap::with_capacity(t.deps.len());

                for (trans, p) in t.deps.iter() {
                    map.insert_hashed(
                        heap.alloc(trans).get_hashed()?,
                        heap.alloc(Label::new(heap, p.clone())),
                    );
                }

                heap.alloc(Dict::new(map))
            }
            AttrLiteral::Query(q) => heap.alloc(q.query.query()),
            AttrLiteral::SourceLabel(s) => heap.alloc(Label::new(heap, *s.clone())),
            AttrLiteral::SourceFile(f) => heap.alloc(StarlarkArtifact::new(Artifact::from(
                SourceArtifact::new(f.path().clone()),
            ))),
            AttrLiteral::Arg(arg) => heap.alloc(arg.to_string()),
            AttrLiteral::Label(l) => heap.alloc(Label::new(heap, *l.clone())),
        })
    }
}

#[derive(thiserror::Error, Debug)]
pub(crate) enum ResolveError {
    #[error("Attribute cannot be converted to Starlark value: `{0}`")]
    AttrCannotBeConvertedToValue(String),
}
