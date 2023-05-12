/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::collections::VecDeque;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_common::result::SharedResult;
use buck2_core::target::label::ConfiguredTargetLabel;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::configured_ref::ConfiguredGraphNodeRef;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use derive_more::Display;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use dupe::IterDupedExt;
use dupe::OptionDupedExt;
use indexmap::IndexMap;
use more_futures::cancellation::CancellationContext;

use crate::actions::artifact::artifact_type::Artifact;
use crate::nodes::calculation::NodeCalculation;
use crate::query::analysis::environment::get_from_template_placeholder_info;
use crate::query::analysis::environment::ConfiguredGraphQueryEnvironmentDelegate;

pub struct AnalysisDiceQueryDelegate<'c> {
    pub ctx: &'c DiceComputations,
}

impl<'c> AnalysisDiceQueryDelegate<'c> {
    pub(crate) fn ctx(&self) -> &DiceComputations {
        self.ctx
    }
}

pub struct AnalysisConfiguredGraphQueryDelegate<'a> {
    pub dice_query_delegate: Arc<AnalysisDiceQueryDelegate<'a>>,
    pub resolved_literals: HashMap<String, ConfiguredTargetNode>,
}

#[async_trait]
impl<'a> ConfiguredGraphQueryEnvironmentDelegate for AnalysisConfiguredGraphQueryDelegate<'a> {
    fn eval_literal(&self, literal: &str) -> anyhow::Result<ConfiguredTargetNode> {
        self.resolved_literals
            .get(literal)
            .duped()
            .ok_or_else(|| anyhow::anyhow!(""))
    }

    async fn get_targets_from_template_placeholder_info(
        &self,
        template_name: &'static str,
        targets: TargetSet<ConfiguredGraphNodeRef>,
    ) -> anyhow::Result<TargetSet<ConfiguredGraphNodeRef>> {
        #[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Allocative)]
        #[display(fmt = "template_placeholder_info_query({})", template_name)]
        struct TemplatePlaceholderInfoQueryKey {
            template_name: &'static str,
            // Use `ConfiguredTargetLabel` instead of `ConfiguredGraphNodeRef` here because `ConfiguredGraphNodeRef`
            // only computes Hash and PartialEq based on the label. If we use `ConfiguredGraphNodeRef` directly we
            // may cache stale ConfiguredTargetNodes and end up with a bug like T133069783.
            targets: Arc<Vec<ConfiguredTargetLabel>>,
        }

        #[async_trait]
        impl Key for TemplatePlaceholderInfoQueryKey {
            type Value = SharedResult<Arc<TargetSet<ConfiguredGraphNodeRef>>>;

            async fn compute(
                &self,
                ctx: &DiceComputations,
                _cancellation: &CancellationContext,
            ) -> Self::Value {
                let (targets, label_to_artifact) = futures::future::try_join(
                    futures::future::try_join_all(self.targets.iter().map(|target| async move {
                        ctx.get_configured_target_node(target)
                            .await?
                            .require_compatible()
                    })),
                    get_from_template_placeholder_info(
                        ctx,
                        self.template_name,
                        self.targets.iter().duped(),
                    ),
                )
                .await?;

                let targets: TargetSet<_> =
                    targets.into_iter().map(ConfiguredGraphNodeRef).collect();
                let targets = find_target_nodes(targets, label_to_artifact)?;
                Ok(Arc::new(targets))
            }

            fn equality(_: &Self::Value, _: &Self::Value) -> bool {
                // result is not comparable
                false
            }
        }

        let targets: Vec<_> = targets
            .into_iter()
            .map(|target| target.label().dupe())
            .collect();
        let targets = self
            .dice_query_delegate
            .ctx()
            .compute(&TemplatePlaceholderInfoQueryKey {
                template_name,
                targets: Arc::new(targets),
            })
            .await??;

        // TODO(scottcao): Make all query functions return an Arc as an output so we can avoid making an unnecessary
        // clone here
        Ok(targets.as_ref().clone())
    }
}

/// Finds the nodes for a list of target labels within the deps of the provided targets.
///
/// It may seem like if we have ConfiguredTargetLabel we should just be able to lookup the
/// nodes directly, but that would require going through dice and then dice would record
/// dependencies on all the nodes that we lookup. It's common for these queries to operate
/// over inputs that are aggregated as data flows up the graph and it's important that we
/// don't inadvertently cause flattening of those sets.
fn find_target_nodes(
    targets: TargetSet<ConfiguredGraphNodeRef>,
    label_to_artifact: IndexMap<ConfiguredTargetLabel, Artifact>,
) -> anyhow::Result<TargetSet<ConfiguredGraphNodeRef>> {
    let mut queue: VecDeque<_> = targets.iter().duped().collect();
    let mut seen = targets;
    let mut result = TargetSet::new();

    while let Some(target) = queue.pop_front() {
        if label_to_artifact.contains_key(target.label()) {
            result.insert(target.dupe());
        }
        if result.len() == label_to_artifact.len() {
            return Ok(result);
        }
        for dep in target.0.target_deps() {
            let dep = ConfiguredGraphNodeRef(dep.dupe());
            if seen.insert(dep.dupe()) {
                queue.push_back(dep);
            }
        }
    }

    Ok(result)
}
