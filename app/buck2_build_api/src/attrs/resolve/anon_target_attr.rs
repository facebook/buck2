/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::package::PackageLabel;
use buck2_interpreter::types::label::Label;
use buck2_node::attrs::attr_type::dep::DepAttrType;
use dupe::Dupe;
use gazebo::prelude::SliceExt;
use starlark::values::dict::Dict;
use starlark::values::none::NoneType;
use starlark::values::tuple::AllocTuple;
use starlark::values::Heap;
use starlark::values::StarlarkValue;
use starlark::values::Value;
use starlark_map::small_map::SmallMap;

use crate::analysis::anon_target_attr::AnonTargetAttr;
use crate::attrs::resolve::attr_type::dep::DepAttrTypeExt;
use crate::attrs::resolve::ctx::AttrResolutionContext;

pub trait AnonTargetAttrExt {
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

impl AnonTargetAttrExt for AnonTargetAttr {
    /// "Resolves" the anon target attr value to the resolved value provided to the rule implementation.
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
        Ok(vec![self.resolve_single(pkg, ctx)?])
    }

    /// Resolving a single value is common, so `resolve_single` will validate
    /// this function's output, and return a single value or an error.
    fn resolve_single<'v>(
        &self,
        pkg: PackageLabel,
        ctx: &dyn AttrResolutionContext<'v>,
    ) -> anyhow::Result<Value<'v>> {
        match self {
            AnonTargetAttr::Bool(v) => Ok(Value::new_bool(v.0)),
            AnonTargetAttr::Int(v) => Ok(Value::new_int(*v)),
            AnonTargetAttr::String(v) | AnonTargetAttr::EnumVariant(v) => {
                Ok(ctx.heap().alloc(v.as_str()))
            }
            AnonTargetAttr::List(list) => {
                let mut values = Vec::with_capacity(list.len());
                for v in list.iter() {
                    values.append(&mut v.resolve(pkg.dupe(), ctx)?);
                }
                Ok(ctx.heap().alloc(values))
            }
            AnonTargetAttr::Tuple(list) => {
                let mut values = Vec::with_capacity(list.len());
                for v in list.iter() {
                    values.append(&mut v.resolve(pkg.dupe(), ctx)?);
                }
                Ok(ctx.heap().alloc(AllocTuple(values)))
            }
            AnonTargetAttr::Dict(dict) => {
                let mut res = SmallMap::with_capacity(dict.len());
                for (k, v) in dict.iter() {
                    res.insert_hashed(
                        k.resolve_single(pkg.dupe(), ctx)?.get_hashed()?,
                        v.resolve_single(pkg.dupe(), ctx)?,
                    );
                }
                Ok(ctx.heap().alloc(Dict::new(res)))
            }
            AnonTargetAttr::None => Ok(Value::new_none()),
            AnonTargetAttr::OneOf(box l, _) => l.resolve_single(pkg, ctx),
            AnonTargetAttr::Dep(d) => DepAttrType::resolve_single(ctx, d),
        }
    }

    /// Returns the starlark type of this attr without resolving
    fn starlark_type(&self) -> anyhow::Result<&'static str> {
        match self {
            AnonTargetAttr::Bool(_) => Ok(starlark::values::bool::BOOL_TYPE),
            AnonTargetAttr::Int(_) => Ok(starlark::values::int::INT_TYPE),
            AnonTargetAttr::String(_) | AnonTargetAttr::EnumVariant(_) => {
                Ok(starlark::values::string::STRING_TYPE)
            }
            AnonTargetAttr::List(_) => Ok(starlark::values::list::ListRef::TYPE),
            AnonTargetAttr::Tuple(_) => Ok(starlark::values::tuple::TupleRef::TYPE),
            AnonTargetAttr::Dict(_) => Ok(Dict::TYPE),
            AnonTargetAttr::None => Ok(NoneType::TYPE),
            AnonTargetAttr::OneOf(box l, _) => l.starlark_type(),
            AnonTargetAttr::Dep(_) => Ok(Label::get_type_value_static().as_str()),
        }
    }

    /// Converts the anon target attr to a starlark value without fully resolving
    fn to_value<'v>(&self, pkg: PackageLabel, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        Ok(match &self {
            AnonTargetAttr::Bool(v) => heap.alloc(v.0),
            AnonTargetAttr::Int(v) => heap.alloc(*v),
            AnonTargetAttr::String(s) | AnonTargetAttr::EnumVariant(s) => heap.alloc(s.as_str()),
            AnonTargetAttr::List(list) => {
                heap.alloc(list.try_map(|v| v.to_value(pkg.dupe(), heap))?)
            }
            AnonTargetAttr::Tuple(v) => {
                heap.alloc(AllocTuple(v.try_map(|v| v.to_value(pkg.dupe(), heap))?))
            }
            AnonTargetAttr::Dict(map) => {
                let mut res = SmallMap::with_capacity(map.len());

                for (k, v) in map.iter() {
                    res.insert_hashed(
                        k.to_value(pkg.dupe(), heap)?.get_hashed()?,
                        v.to_value(pkg.dupe(), heap)?,
                    );
                }

                heap.alloc(Dict::new(res))
            }
            AnonTargetAttr::None => Value::new_none(),
            AnonTargetAttr::OneOf(box l, _) => l.to_value(pkg, heap)?,
            AnonTargetAttr::Dep(d) => heap.alloc(Label::new(d.label.clone())),
        })
    }
}
