/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Implementation of the cli and query_* attr query language.
use std::{
    collections::{BTreeMap, BTreeSet},
    sync::Arc,
};

use async_trait::async_trait;
use buck2_core::{
    fs::{paths::AbsPathBuf, project::ProjectRelativePathBuf},
    package::Package,
    result::ToSharedResultExt,
    target::{TargetLabel, TargetName},
};
use buck2_interpreter::pattern::PackageSpec;
use buck2_query::query::{
    environment::QueryEnvironment,
    syntax::simple::{
        eval::{label_indexed::LabelIndexed, set::TargetSet, values::QueryEvaluationResult},
        functions::DefaultQueryFunctionsModule,
    },
    traversal::{AsyncTraversalDelegate, ChildVisitor},
};
use dice::DiceComputations;
use futures::{stream::FuturesUnordered, StreamExt};
use gazebo::prelude::*;

use crate::{
    nodes::configured::ConfiguredTargetNode,
    query::{
        analysis::evaluator::eval_query,
        cquery::environment::CqueryEnvironment,
        dice::{get_dice_query_delegate, DiceQueryDelegate},
        uquery::environment::{PreresolvedQueryLiterals, UqueryDelegate},
    },
};

pub struct CqueryEvaluator<'c> {
    dice_query_delegate: Arc<DiceQueryDelegate<'c>>,
    functions: DefaultQueryFunctionsModule<CqueryEnvironment<'c>>,
}

impl CqueryEvaluator<'_> {
    pub async fn eval_query(
        &self,
        query: &str,
        query_args: Vec<String>,
        target_universe: Option<Vec<String>>,
    ) -> anyhow::Result<QueryEvaluationResult<ConfiguredTargetNode>> {
        eval_query(&self.functions, query, query_args, async move |literals| {
            let resolved_literals = match target_universe {
                None => {
                    PreresolvedQueryLiterals::pre_resolve(&*self.dice_query_delegate, &literals)
                        .await
                }
                Some(universe) => {
                    resolve_literals_in_universe(
                        &*self.dice_query_delegate,
                        &CqueryEnvironment::new(
                            self.dice_query_delegate.dupe(),
                            self.dice_query_delegate.dupe(),
                        ),
                        &literals,
                        &universe,
                    )
                    .await?
                }
            };
            Ok(CqueryEnvironment::new(
                self.dice_query_delegate.dupe(),
                Arc::new(resolved_literals),
            ))
        })
        .await
    }
}

/// Evaluates some query expression. TargetNodes are resolved via the interpreter from
/// the provided DiceCtx.
pub async fn get_cquery_evaluator<'c>(
    ctx: &'c DiceComputations,
    working_dir: ProjectRelativePathBuf,
    project_root: AbsPathBuf,
    global_target_platform: Option<TargetLabel>,
) -> anyhow::Result<CqueryEvaluator<'c>> {
    let dice_query_delegate = Arc::new(
        get_dice_query_delegate(ctx, working_dir, project_root, global_target_platform).await?,
    );
    let functions = DefaultQueryFunctionsModule::new();
    Ok(CqueryEvaluator {
        dice_query_delegate,
        functions,
    })
}

// This will first resolve the universe to configured nodes and then gather all
// the deps. From there, it resolves the literals to any matching nodes in the universe deps.
async fn resolve_literals_in_universe(
    dice_query_delegate: &DiceQueryDelegate<'_>,
    env: &dyn QueryEnvironment<Target = ConfiguredTargetNode>,
    literals: &[String],
    universe: &[String],
) -> anyhow::Result<PreresolvedQueryLiterals<ConfiguredTargetNode>> {
    // TODO(cjhopman): We should probably also resolve the literals to TargetNode so that
    // we can get errors for packages or targets that don't exist or fail to load.
    let refs: Vec<_> = universe.map(|v| v.as_str());
    let universe_resolved = env.eval_literals(&refs).await?;
    // To support package/recursive patterns, we hold the map by package. To support a
    // single target name having multiple instances in the universe, we map them to a list of nodes.
    struct Delegate {
        targets:
            BTreeMap<Package, BTreeMap<TargetName, BTreeSet<LabelIndexed<ConfiguredTargetNode>>>>,
    }

    #[async_trait]
    impl AsyncTraversalDelegate<ConfiguredTargetNode> for Delegate {
        fn visit(&mut self, target: ConfiguredTargetNode) -> anyhow::Result<()> {
            let label = target.name();
            let package_targets: &mut _ = self
                .targets
                .entry(label.pkg().dupe())
                .or_insert_with(BTreeMap::new);

            let nodes: &mut _ = match package_targets.get_mut(label.name()) {
                Some(v) => v,
                None => package_targets
                    .entry(label.name().dupe())
                    .or_insert_with(BTreeSet::new),
            };

            nodes.insert(LabelIndexed(target));

            Ok(())
        }

        async fn for_each_child(
            &mut self,
            target: &ConfiguredTargetNode,
            func: &mut dyn ChildVisitor<ConfiguredTargetNode>,
        ) -> anyhow::Result<()> {
            for dep in target.deps() {
                func.visit(dep.target().dupe())?;
            }
            Ok(())
        }
    }
    let mut delegate = Delegate {
        targets: BTreeMap::new(),
    };
    env.dfs_postorder(&universe_resolved, &mut delegate).await?;
    let universe = delegate.targets;
    // capture a reference so the ref can be moved into the future below.
    let universe = &universe;

    // TODO(cjhopman): Using the default resolution for recursive literals is inefficient.
    // If we can have a package-trie or cellpath-trie we can do the resolution directly
    // against the universe.
    let resolution_futs: FuturesUnordered<_> = literals
        .iter()
        .map(|lit| async move {
            let result: anyhow::Result<_> = try {
                let resolved_pattern = dice_query_delegate.resolve_target_patterns(&[lit]).await?;
                let mut targets = TargetSet::new();
                for (package, spec) in resolved_pattern.specs {
                    if let Some(package_universe) = universe.get(&package) {
                        match spec {
                            PackageSpec::Targets(names) => {
                                for name in names {
                                    if let Some(nodelist) = package_universe.get(&name) {
                                        for node in nodelist {
                                            targets.insert(node.0.dupe());
                                        }
                                    }
                                }
                            }
                            PackageSpec::All => {
                                for nodelist in package_universe.values() {
                                    for node in nodelist {
                                        targets.insert(node.0.dupe());
                                    }
                                }
                            }
                        }
                    }
                }
                targets
            };

            (lit.to_owned(), result.shared_error())
        })
        .collect();

    let resolved = resolution_futs.collect().await;
    Ok(PreresolvedQueryLiterals::new(resolved))
}
