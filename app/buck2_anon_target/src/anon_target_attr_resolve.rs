/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_analysis::attrs::resolve::attr_type::arg::ConfiguredStringWithMacrosExt;
use buck2_analysis::attrs::resolve::attr_type::dep::DepAttrTypeExt;
use buck2_analysis::attrs::resolve::ctx::AttrResolutionContext;
use buck2_build_api::interpreter::rule_defs::artifact::StarlarkArtifact;
use buck2_core::package::PackageLabel;
use buck2_interpreter::types::configured_providers_label::StarlarkProvidersLabel;
use buck2_node::attrs::attr_type::dep::DepAttrType;
use dupe::Dupe;
use starlark::values::dict::Dict;
use starlark::values::tuple::AllocTuple;
use starlark::values::Value;
use starlark_map::small_map::SmallMap;

use crate::anon_target_attr::AnonTargetAttr;

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
            AnonTargetAttr::Int(v) => Ok(ctx.heap().alloc(*v)),
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
            AnonTargetAttr::Artifact(d) => Ok(ctx.heap().alloc(StarlarkArtifact::new(d.clone()))),
            AnonTargetAttr::Arg(a) => a.resolve(ctx, &pkg),
            AnonTargetAttr::PromiseArtifact(artifact) => Ok(ctx.heap().alloc(artifact.clone())),
            AnonTargetAttr::Label(label) => {
                Ok(ctx.heap().alloc(StarlarkProvidersLabel::new(label.clone())))
            }
        }
    }
}
