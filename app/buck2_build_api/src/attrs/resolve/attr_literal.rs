/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;

use buck2_core::buck_path::path::BuckPath;
use buck2_core::package::PackageLabel;
use buck2_interpreter::types::label::Label;
use buck2_interpreter::types::target_label::StarlarkTargetLabel;
use buck2_node::attrs::attr_type::attr_config::ConfiguredAttrExtraTypes;
use buck2_node::attrs::attr_type::attr_literal::AttrLiteral;
use buck2_node::attrs::attr_type::configuration_dep::ConfigurationDepAttrType;
use buck2_node::attrs::attr_type::configured_dep::ExplicitConfiguredDepAttrType;
use buck2_node::attrs::attr_type::dep::DepAttrType;
use buck2_node::attrs::attr_type::source::SourceAttrType;
use buck2_node::attrs::attr_type::split_transition_dep::SplitTransitionDepAttrType;
use buck2_node::attrs::configured_attr::ConfiguredAttr;
use buck2_node::attrs::display::AttrDisplayWithContextExt;
use buck2_node::visibility::VisibilitySpecification;
use dupe::Dupe;
use gazebo::prelude::*;
use starlark::collections::SmallMap;
use starlark::values::dict::Dict;
use starlark::values::list::AllocList;
use starlark::values::list::ListRef;
use starlark::values::none::NoneType;
use starlark::values::tuple::AllocTuple;
use starlark::values::FrozenValue;
use starlark::values::Heap;
use starlark::values::StarlarkValue;
use starlark::values::Value;

use crate::actions::artifact::artifact_type::Artifact;
use crate::actions::artifact::source_artifact::SourceArtifact;
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

static_assertions::assert_eq_size!(AttrLiteral<CoercedAttr>, [usize; 3]);
static_assertions::assert_eq_size!(AttrLiteral<ConfiguredAttr>, [usize; 3]);

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
            AttrLiteral::List(l) => {
                let mut v = Vec::with_capacity(l.len());
                for e in l.iter() {
                    v.push(e.to_value(heap)?);
                }
                Ok(heap.alloc(AllocList(v)))
            }
            AttrLiteral::Tuple(l) => {
                let mut v = Vec::with_capacity(l.len());
                for e in l.iter() {
                    v.push(e.to_value(heap)?);
                }
                Ok(heap.alloc(AllocTuple(v)))
            }
            AttrLiteral::Dict(d) => {
                let mut m = SmallMap::with_capacity(d.len());
                for (k, v) in &**d {
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
                Err(
                    ResolveError::AttrCannotBeConvertedToValue(x.as_display_no_ctx().to_string())
                        .into(),
                )
            }
        }
    }
}

pub(crate) trait ConfiguredAttrLiteralExt {
    fn resolve_single<'v>(
        &self,
        pkg: PackageLabel,
        ctx: &dyn AttrResolutionContext<'v>,
    ) -> anyhow::Result<Value<'v>>;

    fn resolve<'v>(
        &self,
        pkg: PackageLabel,
        ctx: &dyn AttrResolutionContext<'v>,
    ) -> anyhow::Result<Vec<Value<'v>>>;

    fn starlark_type(&self) -> anyhow::Result<&'static str>;

    /// Converts the configured attr to a starlark value without fully resolving
    fn to_value<'v>(&self, pkg: PackageLabel, heap: &'v Heap) -> anyhow::Result<Value<'v>>;
}

impl ConfiguredAttrLiteralExt for AttrLiteral<ConfiguredAttr> {
    fn resolve_single<'v>(
        &self,
        pkg: PackageLabel,
        ctx: &dyn AttrResolutionContext<'v>,
    ) -> anyhow::Result<Value<'v>> {
        match self {
            AttrLiteral::Bool(v) => Ok(Value::new_bool(*v)),
            AttrLiteral::Int(v) => Ok(Value::new_int(*v)),
            AttrLiteral::String(v) | AttrLiteral::EnumVariant(v) => Ok(ctx.heap().alloc(&**v)),
            AttrLiteral::List(list) => {
                let mut values = Vec::with_capacity(list.len());
                for v in list.iter() {
                    values.append(&mut v.resolve(pkg.dupe(), ctx)?);
                }
                Ok(ctx.heap().alloc(values))
            }
            AttrLiteral::Tuple(list) => {
                let mut values = Vec::with_capacity(list.len());
                for v in list.iter() {
                    values.append(&mut v.resolve(pkg.dupe(), ctx)?);
                }
                Ok(ctx.heap().alloc(AllocTuple(values)))
            }
            AttrLiteral::Dict(dict) => {
                let mut res = SmallMap::with_capacity(dict.len());
                for (k, v) in &**dict {
                    res.insert_hashed(
                        k.resolve_single(pkg.dupe(), ctx)?.get_hashed()?,
                        v.resolve_single(pkg.dupe(), ctx)?,
                    );
                }
                Ok(ctx.heap().alloc(Dict::new(res)))
            }
            AttrLiteral::None => Ok(Value::new_none()),
            AttrLiteral::Query(query) => query.resolve(ctx),
            AttrLiteral::SourceFile(s) => Ok(SourceAttrType::resolve_single_file(
                ctx,
                BuckPath::new(pkg.dupe(), s.path().dupe()),
            )),
            AttrLiteral::SourceLabel(s) => SourceAttrType::resolve_single_label(ctx, s),
            AttrLiteral::Arg(arg) => arg.resolve(ctx),
            AttrLiteral::Label(label) => {
                let label = Label::new(*label.clone());
                Ok(ctx.heap().alloc(label))
            }
            AttrLiteral::OneOf(box l, _) => l.resolve_single(pkg, ctx),
            a @ AttrLiteral::Visibility(_) => {
                // TODO(nga): rule implementations should not need visibility attribute.
                //   But adding it here to preserve existing behavior.
                a.to_value(pkg, ctx.heap())
            }
            AttrLiteral::Extra(u) => match u {
                ConfiguredAttrExtraTypes::ExplicitConfiguredDep(d) => {
                    ExplicitConfiguredDepAttrType::resolve_single(ctx, d.as_ref())
                }
                ConfiguredAttrExtraTypes::SplitTransitionDep(d) => {
                    SplitTransitionDepAttrType::resolve_single(ctx, d.as_ref())
                }
                ConfiguredAttrExtraTypes::ConfigurationDep(d) => {
                    ConfigurationDepAttrType::resolve_single(ctx, d)
                }
                ConfiguredAttrExtraTypes::Dep(d) => DepAttrType::resolve_single(ctx, d),
            },
        }
    }

    fn resolve<'v>(
        &self,
        pkg: PackageLabel,
        ctx: &dyn AttrResolutionContext<'v>,
    ) -> anyhow::Result<Vec<Value<'v>>> {
        match self {
            // SourceLabel is special since it is the only type that can be expand to many
            AttrLiteral::SourceLabel(src) => SourceAttrType::resolve_label(ctx, src),
            _ => Ok(vec![self.resolve_single(pkg, ctx)?]),
        }
    }

    fn starlark_type(&self) -> anyhow::Result<&'static str> {
        match self {
            AttrLiteral::Bool(_) => Ok(starlark::values::bool::BOOL_TYPE),
            AttrLiteral::Int(_) => Ok(starlark::values::int::INT_TYPE),
            AttrLiteral::String(_) | AttrLiteral::EnumVariant(_) => {
                Ok(starlark::values::string::STRING_TYPE)
            }
            AttrLiteral::List(_) => Ok(starlark::values::list::ListRef::TYPE),
            AttrLiteral::Tuple(_) => Ok(starlark::values::tuple::TupleRef::TYPE),
            AttrLiteral::Dict(_) => Ok(Dict::TYPE),
            AttrLiteral::None => Ok(NoneType::TYPE),
            AttrLiteral::Query(_) => Ok(starlark::values::string::STRING_TYPE),
            AttrLiteral::SourceLabel(_) => Ok(Label::get_type_value_static().as_str()),
            AttrLiteral::SourceFile(_) => Ok(StarlarkArtifact::get_type_value_static().as_str()),
            AttrLiteral::Arg(_) => Ok(starlark::values::string::STRING_TYPE),
            AttrLiteral::Label(_) => Ok(Label::get_type_value_static().as_str()),
            AttrLiteral::OneOf(box l, _) => l.starlark_type(),
            AttrLiteral::Visibility(..) => Ok(ListRef::TYPE),
            AttrLiteral::Extra(u) => match u {
                ConfiguredAttrExtraTypes::ExplicitConfiguredDep(_) => {
                    Ok(DependencyGen::<FrozenValue>::get_type_value_static().as_str())
                }
                ConfiguredAttrExtraTypes::SplitTransitionDep(_) => Ok(Dict::TYPE),
                ConfiguredAttrExtraTypes::ConfigurationDep(_) => {
                    Ok(starlark::values::string::STRING_TYPE)
                }
                ConfiguredAttrExtraTypes::Dep(_) => Ok(Label::get_type_value_static().as_str()),
            },
        }
    }

    fn to_value<'v>(&self, pkg: PackageLabel, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        Ok(match &self {
            AttrLiteral::Bool(v) => heap.alloc(*v),
            AttrLiteral::Int(v) => heap.alloc(*v),
            AttrLiteral::String(s) | AttrLiteral::EnumVariant(s) => heap.alloc(&**s),
            AttrLiteral::List(list) => heap.alloc(list.try_map(|v| v.to_value(pkg.dupe(), heap))?),
            AttrLiteral::Tuple(v) => {
                heap.alloc(AllocTuple(v.try_map(|v| v.to_value(pkg.dupe(), heap))?))
            }
            AttrLiteral::Dict(map) => {
                let mut res = SmallMap::with_capacity(map.len());

                for (k, v) in &**map {
                    res.insert_hashed(
                        k.to_value(pkg.dupe(), heap)?.get_hashed()?,
                        v.to_value(pkg.dupe(), heap)?,
                    );
                }

                heap.alloc(Dict::new(res))
            }
            AttrLiteral::None => Value::new_none(),
            AttrLiteral::Query(q) => heap.alloc(q.query.query()),
            AttrLiteral::SourceLabel(s) => heap.alloc(Label::new(*s.clone())),
            AttrLiteral::SourceFile(f) => heap.alloc(StarlarkArtifact::new(Artifact::from(
                SourceArtifact::new(BuckPath::new(pkg.to_owned(), f.path().dupe())),
            ))),
            AttrLiteral::Arg(arg) => heap.alloc(arg.to_string()),
            AttrLiteral::Label(l) => heap.alloc(Label::new(*l.clone())),
            AttrLiteral::OneOf(box l, _) => l.to_value(pkg, heap)?,
            AttrLiteral::Visibility(specs) => match specs {
                VisibilitySpecification::Public => heap.alloc(AllocList(["PUBLIC"])),
                VisibilitySpecification::Default => heap.alloc(AllocList::EMPTY),
                VisibilitySpecification::VisibleTo(specs) => {
                    heap.alloc(AllocList(specs.iter().map(|s| s.to_string())))
                }
            },
            AttrLiteral::Extra(u) => match u {
                ConfiguredAttrExtraTypes::ExplicitConfiguredDep(d) => {
                    heap.alloc(Label::new(d.as_ref().label.clone()))
                }
                ConfiguredAttrExtraTypes::SplitTransitionDep(t) => {
                    let mut map = SmallMap::with_capacity(t.deps.len());

                    for (trans, p) in t.deps.iter() {
                        map.insert_hashed(
                            heap.alloc(trans).get_hashed()?,
                            heap.alloc(Label::new(p.clone())),
                        );
                    }

                    heap.alloc(Dict::new(map))
                }
                ConfiguredAttrExtraTypes::ConfigurationDep(c) => {
                    heap.alloc(StarlarkTargetLabel::new(c.as_ref().dupe()))
                }
                ConfiguredAttrExtraTypes::Dep(d) => heap.alloc(Label::new(d.label.clone())),
            },
        })
    }
}

#[derive(thiserror::Error, Debug)]
pub(crate) enum ResolveError {
    #[error("Attribute cannot be converted to Starlark value: `{0}`")]
    AttrCannotBeConvertedToValue(String),
}
