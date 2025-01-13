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
use std::sync::OnceLock;

use buck2_analysis::analysis::calculation::get_dep_analysis;
use buck2_analysis::analysis::calculation::resolve_queries;
use buck2_analysis::analysis::env::get_dep;
use buck2_analysis::analysis::env::get_deps_from_analysis_results;
use buck2_analysis::analysis::env::resolve_query;
use buck2_analysis::analysis::env::resolve_unkeyed_placeholder;
use buck2_analysis::attrs::resolve::ctx::AnalysisQueryResult;
use buck2_analysis::attrs::resolve::ctx::AttrResolutionContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::value::FrozenCommandLineArg;
use buck2_build_api::interpreter::rule_defs::provider::collection::FrozenProviderCollection;
use buck2_build_api::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;
use buck2_core::execution_types::execution::ExecutionPlatformResolution;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use futures::FutureExt;
use starlark::environment::Module;
use starlark::values::FrozenValueTyped;

use crate::bxl::starlark_defs::context::BxlContext;

// Contains a `module` that things must live on, and various `FrozenProviderCollectionValue`s
// that are NOT tied to that module. Must claim ownership of them via `add_reference` before returning them.
pub(crate) struct LazyAttrResolutionContext<'v> {
    pub(crate) module: &'v Module,
    pub(crate) configured_node: &'v ConfiguredTargetNode,
    pub(crate) ctx: &'v BxlContext<'v>,
    pub(crate) dep_analysis_results: OnceLock<
        buck2_error::Result<HashMap<&'v ConfiguredTargetLabel, FrozenProviderCollectionValue>>,
    >,
    pub(crate) query_results:
        OnceLock<buck2_error::Result<HashMap<String, Arc<AnalysisQueryResult>>>>,
}

impl<'v> LazyAttrResolutionContext<'v> {
    pub(crate) fn dep_analysis_results(
        &self,
    ) -> &buck2_error::Result<HashMap<&'v ConfiguredTargetLabel, FrozenProviderCollectionValue>>
    {
        self.dep_analysis_results.get_or_init(|| {
            get_deps_from_analysis_results(self.ctx.async_ctx.borrow_mut().via(|dice_ctx| {
                get_dep_analysis(self.configured_node.as_ref(), dice_ctx).boxed_local()
            })?)
        })
    }

    pub(crate) fn query_results(
        &self,
    ) -> &buck2_error::Result<HashMap<String, Arc<AnalysisQueryResult>>> {
        self.query_results.get_or_init(|| {
            self.ctx.async_ctx.borrow_mut().via(|dice_ctx| {
                resolve_queries(dice_ctx, self.configured_node.as_ref()).boxed_local()
            })
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
    ) -> buck2_error::Result<FrozenValueTyped<'v, FrozenProviderCollection>> {
        match self.dep_analysis_results() {
            Ok(deps) => Ok(get_dep(deps, target, self.module)?),
            Err(e) => Err(buck2_error::buck2_error!(
                buck2_error::ErrorTag::Tier0,
                "Error getting deps from analysis: `{}`",
                e
            )),
        }
    }

    fn resolve_unkeyed_placeholder(
        &self,
        name: &str,
    ) -> buck2_error::Result<Option<FrozenCommandLineArg>> {
        match self.dep_analysis_results() {
            Ok(deps) => Ok(resolve_unkeyed_placeholder(deps, name, self.module)),
            Err(e) => Err(buck2_error::buck2_error!(
                buck2_error::ErrorTag::Tier0,
                "Error resolving unkeyed placeholder: `{}`",
                e
            )),
        }
    }

    fn resolve_query(&self, query: &str) -> buck2_error::Result<Arc<AnalysisQueryResult>> {
        match self.query_results() {
            Ok(res) => resolve_query(res, query, self.module),
            Err(e) => Err(buck2_error::buck2_error!(
                buck2_error::ErrorTag::Tier0,
                "Error resolving query: `{}`",
                e
            ))
            .map_err(buck2_error::Error::from),
        }
    }

    fn execution_platform_resolution(&self) -> &ExecutionPlatformResolution {
        self.configured_node.execution_platform_resolution()
    }
}
