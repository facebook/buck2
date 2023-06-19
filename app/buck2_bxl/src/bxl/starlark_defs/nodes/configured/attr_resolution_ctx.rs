/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::sync::Arc;

use buck2_analysis::analysis::calculation::get_dep_analysis;
use buck2_analysis::analysis::calculation::resolve_queries;
use buck2_analysis::analysis::env::get_dep;
use buck2_analysis::analysis::env::get_deps_from_analysis_results;
use buck2_analysis::analysis::env::resolve_query;
use buck2_analysis::analysis::env::resolve_unkeyed_placeholder;
use buck2_analysis::attrs::resolve::ctx::AnalysisQueryResult;
use buck2_analysis::attrs::resolve::ctx::AttrResolutionContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::value::FrozenCommandLineArg;
use buck2_build_api::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;
use buck2_common::result::SharedResult;
use buck2_common::result::ToSharedResultExt;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::target::label::ConfiguredTargetLabel;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use once_cell::sync::OnceCell;
use starlark::environment::Module;

use crate::bxl::starlark_defs::context::BxlContext;

// Contains a `module` that things must live on, and various `FrozenProviderCollectionValue`s
// that are NOT tied to that module. Must claim ownership of them via `add_reference` before returning them.
pub struct LazyAttrResolutionContext<'v> {
    pub module: &'v Module,
    pub configured_node: &'v ConfiguredTargetNode,
    pub ctx: &'v BxlContext<'v>,
    pub dep_analysis_results:
        OnceCell<anyhow::Result<HashMap<&'v ConfiguredTargetLabel, FrozenProviderCollectionValue>>>,
    pub query_results: OnceCell<anyhow::Result<HashMap<String, Arc<AnalysisQueryResult>>>>,
}

impl<'v> LazyAttrResolutionContext<'v> {
    pub(crate) fn dep_analysis_results(
        &self,
    ) -> &anyhow::Result<HashMap<&'v ConfiguredTargetLabel, FrozenProviderCollectionValue>> {
        self.dep_analysis_results.get_or_init(|| {
            get_deps_from_analysis_results(
                self.ctx
                    .async_ctx
                    .via_dice(|dice_ctx| get_dep_analysis(self.configured_node, dice_ctx))?,
            )
        })
    }

    pub(crate) fn query_results(
        &self,
    ) -> &anyhow::Result<HashMap<String, Arc<AnalysisQueryResult>>> {
        self.query_results.get_or_init(|| {
            self.ctx
                .async_ctx
                .via_dice(|dice_ctx| resolve_queries(dice_ctx, self.configured_node))
        })
    }
}

impl<'v> AttrResolutionContext<'v> for LazyAttrResolutionContext<'v> {
    fn starlark_module(&self) -> &'v Module {
        self.module
    }

    fn get_dep(
        &self,
        target: &ConfiguredProvidersLabel,
    ) -> anyhow::Result<FrozenProviderCollectionValue> {
        match self.dep_analysis_results() {
            Ok(deps) => get_dep(deps, target, self.module),
            Err(e) => Err(anyhow::anyhow!("Error getting deps from analysis: `{}`", e)),
        }
    }

    fn resolve_unkeyed_placeholder(
        &self,
        name: &str,
    ) -> anyhow::Result<Option<FrozenCommandLineArg>> {
        match self.dep_analysis_results() {
            Ok(deps) => Ok(resolve_unkeyed_placeholder(deps, name, self.module)),
            Err(e) => Err(anyhow::anyhow!(
                "Error resolving unkeyed placeholder: `{}`",
                e
            )),
        }
    }

    fn resolve_query(&self, query: &str) -> SharedResult<Arc<AnalysisQueryResult>> {
        match self.query_results() {
            Ok(res) => resolve_query(res, query, self.module),
            Err(e) => Err(anyhow::anyhow!("Error resolving query: `{}`", e)).shared_error(),
        }
    }
}
