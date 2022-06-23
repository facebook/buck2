/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::result::SharedResult;
use buck2_core::target::ConfiguredTargetLabel;
use starlark::environment::Module;
use starlark::values::FrozenRef;
use starlark::values::Heap;

use crate::interpreter::rule_defs::cmd_args::FrozenCommandLineArgLike;
use crate::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;

pub type AnalysisQueryResult = Vec<(ConfiguredTargetLabel, FrozenProviderCollectionValue)>;

/// The context for attribute resolution. Provides access to the providers from
/// dependents.
pub(crate) trait AttrResolutionContext {
    fn starlark_module(&self) -> &Module;

    fn heap(&self) -> &Heap {
        self.starlark_module().heap()
    }

    /// Get the `ProviderCollection` for this label. This is converted to a `Dependency`
    /// by the `resolve()` method in `attrs::label`
    fn get_dep(&self, target: &ConfiguredProvidersLabel) -> Option<FrozenProviderCollectionValue>;

    fn resolve_unkeyed_placeholder(
        &self,
        name: &str,
    ) -> Option<FrozenRef<'static, dyn FrozenCommandLineArgLike + 'static>>;

    /// Provides the result of the query. This will only provide results for queries that are reported during the configured attr traversal.
    // TODO(cjhopman): Ideally, we wouldn't need to split query attr resolution in this way, but processing queries is an async operation and the starlark Heap cannot be used in async code.
    fn resolve_query(&self, query: &str) -> SharedResult<Arc<AnalysisQueryResult>>;
}
