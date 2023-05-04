/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::buck_path::path::BuckPath;
use buck2_core::package::PackageLabel;
use buck2_interpreter::types::label::Label;
use buck2_interpreter::types::target_label::StarlarkTargetLabel;
use buck2_node::attrs::attr_type::configuration_dep::ConfigurationDepAttrType;
use buck2_node::attrs::attr_type::configured_dep::ExplicitConfiguredDepAttrType;
use buck2_node::attrs::attr_type::dep::DepAttrType;
use buck2_node::attrs::attr_type::source::SourceAttrType;
use buck2_node::attrs::attr_type::split_transition_dep::SplitTransitionDepAttrType;
use buck2_node::attrs::configured_attr::ConfiguredAttr;
use buck2_node::visibility::VisibilitySpecification;
use dupe::Dupe;
use gazebo::prelude::SliceExt;
use starlark::values::dict::Dict;
use starlark::values::list::AllocList;
use starlark::values::list::ListRef;
use starlark::values::none::NoneType;
use starlark::values::tuple::AllocTuple;
use starlark::values::FrozenValue;
use starlark::values::Heap;
use starlark::values::StarlarkValue;
use starlark::values::Value;
use starlark_map::small_map::SmallMap;

use crate::actions::artifact::artifact_type::Artifact;
use crate::actions::artifact::source_artifact::SourceArtifact;
use crate::attrs::resolve::attr_type::arg::ConfiguredStringWithMacrosExt;
use crate::attrs::resolve::attr_type::configuration_dep::ConfigurationDepAttrTypeExt;
use crate::attrs::resolve::attr_type::dep::DepAttrTypeExt;
use crate::attrs::resolve::attr_type::dep::ExplicitConfiguredDepAttrTypeExt;
use crate::attrs::resolve::attr_type::query::ConfiguredQueryAttrExt;
use crate::attrs::resolve::attr_type::source::SourceAttrTypeExt;
use crate::attrs::resolve::attr_type::split_transition_dep::SplitTransitionDepAttrTypeExt;
use crate::attrs::resolve::ctx::AttrResolutionContext;
use crate::interpreter::rule_defs::artifact::StarlarkArtifact;
use crate::interpreter::rule_defs::provider::dependency::DependencyGen;

pub trait ConfiguredAttrExt {
    fn resolve<'v>(
        &self,
        pkg: PackageLabel,
        ctx: &dyn AttrResolutionContext<'v>,
    ) -> anyhow::Result<Vec<Value<'v>>>;

    fn resolve_single<'v>(
        &self,
        pkg: PackageLabel,
        ctx: &dyn AttrResolutionContext<'v>,
    ) -> anyhow::Result<Value<'v>>;

    fn starlark_type(&self) -> anyhow::Result<&'static str>;

    fn to_value<'v>(&self, pkg: PackageLabel, heap: &'v Heap) -> anyhow::Result<Value<'v>>;
}

impl ConfiguredAttrExt for ConfiguredAttr {
    /// "Resolves" the configured value to the resolved value provided to the rule implementation.
    ///
    /// `resolve` may return multiple values. It is up to the caller to fail if
    /// an inappropriate number of elements is returned. e.g. `attrs.list()` might
    /// accept and merge multiple returned values from `attrs.source()`, but
    /// `attrs.optional()` might only accept a single value, and fail otherwise.
    fn resolve<'v>(
        &self,
        pkg: PackageLabel,
        ctx: &dyn AttrResolutionContext<'v>,
    ) -> anyhow::Result<Vec<Value<'v>>> {
        match self {
            // SourceLabel is special since it is the only type that can be expand to many
            ConfiguredAttr::SourceLabel(src) => SourceAttrType::resolve_label(ctx, src),
            _ => Ok(vec![self.resolve_single(pkg, ctx)?]),
        }
    }

    /// Resolving a single value is common, so `resolve_single` will validate
    /// this function's output, and return a single value or an error.
    fn resolve_single<'v>(
        &self,
        pkg: PackageLabel,
        ctx: &dyn AttrResolutionContext<'v>,
    ) -> anyhow::Result<Value<'v>> {
        match self {
            ConfiguredAttr::Bool(v) => Ok(Value::new_bool(v.0)),
            ConfiguredAttr::Int(v) => Ok(Value::new_int(*v)),
            ConfiguredAttr::String(v) | ConfiguredAttr::EnumVariant(v) => {
                Ok(ctx.heap().alloc(v.as_str()))
            }
            ConfiguredAttr::List(list) => {
                let mut values = Vec::with_capacity(list.len());
                for v in list.iter() {
                    values.append(&mut v.resolve(pkg.dupe(), ctx)?);
                }
                Ok(ctx.heap().alloc(values))
            }
            ConfiguredAttr::Tuple(list) => {
                let mut values = Vec::with_capacity(list.len());
                for v in list.iter() {
                    values.append(&mut v.resolve(pkg.dupe(), ctx)?);
                }
                Ok(ctx.heap().alloc(AllocTuple(values)))
            }
            ConfiguredAttr::Dict(dict) => {
                let mut res = SmallMap::with_capacity(dict.len());
                for (k, v) in dict.iter() {
                    res.insert_hashed(
                        k.resolve_single(pkg.dupe(), ctx)?.get_hashed()?,
                        v.resolve_single(pkg.dupe(), ctx)?,
                    );
                }
                Ok(ctx.heap().alloc(Dict::new(res)))
            }
            ConfiguredAttr::None => Ok(Value::new_none()),
            ConfiguredAttr::OneOf(box l, _) => l.resolve_single(pkg, ctx),
            a @ ConfiguredAttr::Visibility(_) => {
                // TODO(nga): rule implementations should not need visibility attribute.
                //   But adding it here to preserve existing behavior.
                a.to_value(pkg, ctx.heap())
            }
            ConfiguredAttr::ExplicitConfiguredDep(d) => {
                ExplicitConfiguredDepAttrType::resolve_single(ctx, d.as_ref())
            }
            ConfiguredAttr::SplitTransitionDep(d) => {
                SplitTransitionDepAttrType::resolve_single(ctx, d.as_ref())
            }
            ConfiguredAttr::ConfigurationDep(d) => ConfigurationDepAttrType::resolve_single(ctx, d),
            ConfiguredAttr::Dep(d) => DepAttrType::resolve_single(ctx, d),
            ConfiguredAttr::SourceLabel(s) => SourceAttrType::resolve_single_label(ctx, s),
            ConfiguredAttr::Label(label) => {
                let label = Label::new(*label.clone());
                Ok(ctx.heap().alloc(label))
            }
            ConfiguredAttr::Arg(arg) => arg.resolve(ctx),
            ConfiguredAttr::Query(query) => query.resolve(ctx),
            ConfiguredAttr::SourceFile(s) => Ok(SourceAttrType::resolve_single_file(
                ctx,
                BuckPath::new(pkg.dupe(), s.path().dupe()),
            )),
        }
    }

    /// Returns the starlark type of this attr without resolving
    fn starlark_type(&self) -> anyhow::Result<&'static str> {
        match self {
            ConfiguredAttr::Bool(_) => Ok(starlark::values::bool::BOOL_TYPE),
            ConfiguredAttr::Int(_) => Ok(starlark::values::int::INT_TYPE),
            ConfiguredAttr::String(_) | ConfiguredAttr::EnumVariant(_) => {
                Ok(starlark::values::string::STRING_TYPE)
            }
            ConfiguredAttr::List(_) => Ok(starlark::values::list::ListRef::TYPE),
            ConfiguredAttr::Tuple(_) => Ok(starlark::values::tuple::TupleRef::TYPE),
            ConfiguredAttr::Dict(_) => Ok(Dict::TYPE),
            ConfiguredAttr::None => Ok(NoneType::TYPE),
            ConfiguredAttr::OneOf(box l, _) => l.starlark_type(),
            ConfiguredAttr::Visibility(..) => Ok(ListRef::TYPE),
            ConfiguredAttr::ExplicitConfiguredDep(_) => {
                Ok(DependencyGen::<FrozenValue>::get_type_value_static().as_str())
            }
            ConfiguredAttr::SplitTransitionDep(_) => Ok(Dict::TYPE),
            ConfiguredAttr::ConfigurationDep(_) => Ok(starlark::values::string::STRING_TYPE),
            ConfiguredAttr::Dep(_) => Ok(Label::get_type_value_static().as_str()),
            ConfiguredAttr::SourceLabel(_) => Ok(Label::get_type_value_static().as_str()),
            ConfiguredAttr::Label(_) => Ok(Label::get_type_value_static().as_str()),
            ConfiguredAttr::Arg(_) => Ok(starlark::values::string::STRING_TYPE),
            ConfiguredAttr::Query(_) => Ok(starlark::values::string::STRING_TYPE),
            ConfiguredAttr::SourceFile(_) => Ok(StarlarkArtifact::get_type_value_static().as_str()),
        }
    }

    /// Converts the configured attr to a starlark value without fully resolving
    fn to_value<'v>(&self, pkg: PackageLabel, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        Ok(match &self {
            ConfiguredAttr::Bool(v) => heap.alloc(v.0),
            ConfiguredAttr::Int(v) => heap.alloc(*v),
            ConfiguredAttr::String(s) | ConfiguredAttr::EnumVariant(s) => heap.alloc(s.as_str()),
            ConfiguredAttr::List(list) => {
                heap.alloc(list.try_map(|v| v.to_value(pkg.dupe(), heap))?)
            }
            ConfiguredAttr::Tuple(v) => {
                heap.alloc(AllocTuple(v.try_map(|v| v.to_value(pkg.dupe(), heap))?))
            }
            ConfiguredAttr::Dict(map) => {
                let mut res = SmallMap::with_capacity(map.len());

                for (k, v) in map.iter() {
                    res.insert_hashed(
                        k.to_value(pkg.dupe(), heap)?.get_hashed()?,
                        v.to_value(pkg.dupe(), heap)?,
                    );
                }

                heap.alloc(Dict::new(res))
            }
            ConfiguredAttr::None => Value::new_none(),
            ConfiguredAttr::OneOf(box l, _) => l.to_value(pkg, heap)?,
            ConfiguredAttr::Visibility(specs) => match specs {
                VisibilitySpecification::Public => heap.alloc(AllocList(["PUBLIC"])),
                VisibilitySpecification::VisibleTo(specs) => {
                    heap.alloc(AllocList(specs.iter().map(|s| s.to_string())))
                }
            },
            ConfiguredAttr::ExplicitConfiguredDep(d) => {
                heap.alloc(Label::new(d.as_ref().label.clone()))
            }
            ConfiguredAttr::SplitTransitionDep(t) => {
                let mut map = SmallMap::with_capacity(t.deps.len());

                for (trans, p) in t.deps.iter() {
                    map.insert_hashed(
                        heap.alloc(trans).get_hashed()?,
                        heap.alloc(Label::new(p.clone())),
                    );
                }

                heap.alloc(Dict::new(map))
            }
            ConfiguredAttr::ConfigurationDep(c) => {
                heap.alloc(StarlarkTargetLabel::new(c.as_ref().dupe()))
            }
            ConfiguredAttr::Dep(d) => heap.alloc(Label::new(d.label.clone())),
            ConfiguredAttr::SourceLabel(s) => heap.alloc(Label::new(*s.clone())),
            ConfiguredAttr::Label(l) => heap.alloc(Label::new(*l.clone())),
            ConfiguredAttr::Arg(arg) => heap.alloc(arg.to_string()),
            ConfiguredAttr::Query(query) => heap.alloc(query.query.query()),
            ConfiguredAttr::SourceFile(f) => heap.alloc(StarlarkArtifact::new(Artifact::from(
                SourceArtifact::new(BuckPath::new(pkg.to_owned(), f.path().dupe())),
            ))),
        })
    }
}
