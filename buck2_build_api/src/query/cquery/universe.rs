/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;

use async_trait::async_trait;
use buck2_common::pattern::resolve::ResolvedPattern;
use buck2_core::package::Package;
use buck2_core::pattern::PackageSpec;
use buck2_core::target::TargetName;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_query::query::environment::QueryEnvironment;
use buck2_query::query::syntax::simple::eval::label_indexed::LabelIndexed;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use buck2_query::query::traversal::AsyncTraversalDelegate;
use buck2_query::query::traversal::ChildVisitor;
use gazebo::dupe::Dupe;

/// Subset of targets `cquery` command works with.
///
/// Targets are resolved in the universe, and file owners are also resolved in the universe.
pub(crate) struct CqueryUniverse {
    targets: BTreeMap<Package, BTreeMap<TargetName, BTreeSet<LabelIndexed<ConfiguredTargetNode>>>>,
}

impl CqueryUniverse {
    pub(crate) fn new(
        targets: BTreeMap<
            Package,
            BTreeMap<TargetName, BTreeSet<LabelIndexed<ConfiguredTargetNode>>>,
        >,
    ) -> CqueryUniverse {
        CqueryUniverse { targets }
    }

    pub(crate) async fn build(
        universe: &TargetSet<ConfiguredTargetNode>,
        env: &dyn QueryEnvironment<Target = ConfiguredTargetNode>,
    ) -> anyhow::Result<CqueryUniverse> {
        // To support package/recursive patterns, we hold the map by package. To support a
        // single target name having multiple instances in the universe, we map them to a list of nodes.
        struct Delegate {
            targets: BTreeMap<
                Package,
                BTreeMap<TargetName, BTreeSet<LabelIndexed<ConfiguredTargetNode>>>,
            >,
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
                    func.visit(dep.name().dupe())?;
                }
                Ok(())
            }
        }
        let mut delegate = Delegate {
            targets: BTreeMap::new(),
        };
        env.dfs_postorder(universe, &mut delegate).await?;
        Ok(CqueryUniverse::new(delegate.targets))
    }

    pub(crate) fn get(
        &self,
        resolved_pattern: &ResolvedPattern<TargetName>,
    ) -> TargetSet<ConfiguredTargetNode> {
        let mut targets = TargetSet::new();
        for (package, spec) in &resolved_pattern.specs {
            if let Some(package_universe) = self.targets.get(package) {
                match spec {
                    PackageSpec::Targets(names) => {
                        for name in names {
                            if let Some(nodelist) = package_universe.get(name) {
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
    }
}
