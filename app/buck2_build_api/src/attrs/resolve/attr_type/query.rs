/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_common::result::SharedResult;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_node::attrs::attr_type::dep::DepAttrType;
use buck2_node::attrs::attr_type::query::QueryAttr;
use buck2_node::attrs::attr_type::query::QueryAttrBase;
use dupe::Dupe;
use starlark::values::Value;

use crate::attrs::resolve::attr_type::dep::DepAttrTypeExt;
use crate::attrs::resolve::ctx::AnalysisQueryResult;
use crate::attrs::resolve::ctx::AttrResolutionContext;

pub(crate) trait ConfiguredQueryAttrBaseExt {
    fn resolve(&self, ctx: &dyn AttrResolutionContext) -> SharedResult<Arc<AnalysisQueryResult>>;
}

impl ConfiguredQueryAttrBaseExt for QueryAttrBase<ConfiguredProvidersLabel> {
    fn resolve(&self, ctx: &dyn AttrResolutionContext) -> SharedResult<Arc<AnalysisQueryResult>> {
        ctx.resolve_query(&self.query)
    }
}

pub(crate) trait ConfiguredQueryAttrExt {
    fn resolve<'v>(&self, ctx: &dyn AttrResolutionContext<'v>) -> anyhow::Result<Value<'v>>;
}

impl ConfiguredQueryAttrExt for QueryAttr<ConfiguredProvidersLabel> {
    fn resolve<'v>(&self, ctx: &dyn AttrResolutionContext<'v>) -> anyhow::Result<Value<'v>> {
        let query_results = self.query.resolve(ctx)?;
        let mut dependencies = Vec::new();

        for (target, providers) in &*query_results {
            let providers_label =
                ConfiguredProvidersLabel::new(target.dupe(), ProvidersName::Default);
            if !self.providers.is_empty() {
                let provider_collection = providers.provider_collection();

                DepAttrType::check_providers(
                    &self.providers,
                    provider_collection,
                    &providers_label,
                )?;
            }
            dependencies.push(DepAttrType::alloc_dependency(
                ctx.starlark_module(),
                &providers_label,
                providers,
            ));
        }
        Ok(ctx.heap().alloc(dependencies))
    }
}
