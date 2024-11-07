/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use async_trait::async_trait;
use buck2_artifact::actions::key::ActionKey;
use buck2_build_api::actions::query::ActionQueryNode;
use buck2_build_api::actions::query::ActionQueryNodeRef;
use buck2_build_api::analysis::AnalysisResult;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_query::query::environment::QueryEnvironment;
use buck2_query::query::graph::successors::AsyncChildVisitor;
use buck2_query::query::syntax::simple::eval::error::QueryError;
use buck2_query::query::syntax::simple::eval::file_set::FileSet;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use buck2_query::query::syntax::simple::functions::docs::QueryEnvironmentDescription;
use buck2_query::query::syntax::simple::functions::DefaultQueryFunctionsModule;
use buck2_query::query::syntax::simple::functions::HasModuleDescription;
use buck2_query::query::traversal::async_depth_first_postorder_traversal;
use buck2_query::query::traversal::async_depth_limited_traversal;
use buck2_query::query::traversal::AsyncNodeLookup;
use dice::DiceComputations;

use crate::aquery::functions::AqueryFunctions;
use crate::cquery::environment::CqueryDelegate;
use crate::uquery::environment::QueryLiterals;

/// CqueryDelegate resolves information needed by the QueryEnvironment.
#[async_trait]
pub(crate) trait AqueryDelegate: Send + Sync {
    fn cquery_delegate(&self) -> &dyn CqueryDelegate;

    fn ctx<'a>(&'a self) -> DiceComputations<'a>;

    async fn get_node(&self, key: &ActionKey) -> buck2_error::Result<ActionQueryNode>;

    async fn expand_artifacts(
        &self,
        artifacts: &[ArtifactGroup],
    ) -> buck2_error::Result<Vec<ActionQueryNode>>;

    async fn get_target_set_from_analysis(
        &self,
        configured_label: &ConfiguredProvidersLabel,
        analysis: AnalysisResult,
    ) -> buck2_error::Result<TargetSet<ActionQueryNode>>;
}

pub(crate) struct AqueryEnvironment<'c> {
    pub(super) delegate: Arc<dyn AqueryDelegate + 'c>,
    literals: Arc<dyn QueryLiterals<ActionQueryNode> + 'c>,
}

impl<'c> AqueryEnvironment<'c> {
    pub(crate) fn new(
        delegate: Arc<dyn AqueryDelegate + 'c>,
        literals: Arc<dyn QueryLiterals<ActionQueryNode> + 'c>,
    ) -> Self {
        Self { delegate, literals }
    }

    async fn get_node(&self, label: &ActionQueryNodeRef) -> buck2_error::Result<ActionQueryNode> {
        // We do not allow traversing edges in targets in aquery
        self.delegate.get_node(label.require_action()?).await
    }

    pub(crate) fn describe() -> QueryEnvironmentDescription {
        QueryEnvironmentDescription {
            name: "Aquery Environment".to_owned(),
            mods: vec![
                DefaultQueryFunctionsModule::<Self>::describe(),
                AqueryFunctions::describe(),
            ],
        }
    }
}

#[async_trait]
impl<'c> QueryEnvironment for AqueryEnvironment<'c> {
    type Target = ActionQueryNode;

    async fn get_node(&self, node_ref: &ActionQueryNodeRef) -> buck2_error::Result<Self::Target> {
        AqueryEnvironment::get_node(self, node_ref).await
    }

    async fn get_node_for_default_configured_target(
        &self,
        _node_ref: &ActionQueryNodeRef,
    ) -> buck2_error::Result<MaybeCompatible<Self::Target>> {
        Err(QueryError::FunctionUnimplemented(
            "get_node_for_default_configured_target() only for CqueryEnvironment",
        )
        .into())
    }

    async fn eval_literals(
        &self,
        literals: &[&str],
    ) -> buck2_error::Result<TargetSet<Self::Target>> {
        self.literals
            .eval_literals(literals, &mut self.delegate.ctx())
            .await
    }

    async fn eval_file_literal(&self, literal: &str) -> buck2_error::Result<FileSet> {
        self.delegate
            .cquery_delegate()
            .uquery_delegate()
            .eval_file_literal(literal)
            .await
    }

    async fn dfs_postorder(
        &self,
        root: &TargetSet<Self::Target>,
        traversal_delegate: impl AsyncChildVisitor<Self::Target>,
        visit: impl FnMut(Self::Target) -> buck2_error::Result<()> + Send,
    ) -> buck2_error::Result<()> {
        // TODO(cjhopman): The query nodes deps are going to flatten the tset structure for its deps. In a typical
        // build graph, a traversal over just the graph of ActionQueryNode ends up being an `O(n)` operation at each
        // node and ends up with an `O(n^2)` cost. If instead we were to not flatten the structure and traverse the
        // mixed graph of action nodes and tset nodes, we'd get closer to `O(n + e)` which in practice is much better
        // (hence the whole point of tsets). While we can't change the ActionQueryNode deps() function to not flatten
        // the tset, we aren't required to do these traversal's using that function.
        async_depth_first_postorder_traversal(
            &AqueryNodeLookup {
                roots: root,
                env: self,
            },
            root.iter_names(),
            traversal_delegate,
            visit,
        )
        .await
    }

    async fn depth_limited_traversal(
        &self,
        root: &TargetSet<Self::Target>,
        delegate: impl AsyncChildVisitor<Self::Target>,
        visit: impl FnMut(Self::Target) -> buck2_error::Result<()> + Send,
        depth: u32,
    ) -> buck2_error::Result<()> {
        // TODO(cjhopman): See above.
        async_depth_limited_traversal(
            &AqueryNodeLookup {
                roots: root,
                env: self,
            },
            root.iter_names(),
            delegate,
            visit,
            depth,
        )
        .await
    }

    async fn owner(&self, _paths: &FileSet) -> buck2_error::Result<TargetSet<Self::Target>> {
        Err(QueryError::NotAvailableInContext("owner").into())
    }

    async fn targets_in_buildfile(
        &self,
        _paths: &FileSet,
    ) -> buck2_error::Result<TargetSet<Self::Target>> {
        Err(QueryError::NotAvailableInContext("targets_in_buildfile").into())
    }
}

struct AqueryNodeLookup<'a, 'c> {
    roots: &'a TargetSet<ActionQueryNode>,
    env: &'a AqueryEnvironment<'c>,
}

#[async_trait]
impl<'a, 'c> AsyncNodeLookup<ActionQueryNode> for AqueryNodeLookup<'a, 'c> {
    async fn get(&self, label: &ActionQueryNodeRef) -> buck2_error::Result<ActionQueryNode> {
        // Lookup the node in `roots` first since `env.get_node` isn't capable of looking up
        // analysis nodes, and while won't find new analysis nodes while doing a DFS, we might pass
        // in roots that *are* analysis nodes.
        if let Some(v) = self.roots.get(label) {
            return Ok(v.clone());
        }
        self.env.get_node(label).await
    }
}
