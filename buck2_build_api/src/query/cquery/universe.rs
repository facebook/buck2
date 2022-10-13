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

use buck2_common::pattern::resolve::ResolvedPattern;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::package::Package;
use buck2_core::pattern::PackageSpec;
use buck2_core::pattern::PatternType;
use buck2_core::pattern::ProvidersPattern;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::target::TargetName;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::configured_node_visit_all_deps::configured_node_visit_all_deps;
use buck2_query::query::syntax::simple::eval::label_indexed::LabelIndexed;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use either::Either;
use gazebo::dupe::Dupe;
use gazebo::prelude::IterDuped;
use itertools::Itertools;

/// Subset of targets `cquery` command works with.
///
/// Targets are resolved in the universe, and file owners are also resolved in the universe.
pub struct CqueryUniverse {
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
    ) -> anyhow::Result<CqueryUniverse> {
        let mut targets: BTreeMap<
            Package,
            BTreeMap<TargetName, BTreeSet<LabelIndexed<ConfiguredTargetNode>>>,
        > = BTreeMap::new();

        configured_node_visit_all_deps(universe.iter().duped(), |target| {
            let label = target.name();
            let package_targets: &mut _ = targets
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
        })
        .await?;

        Ok(CqueryUniverse::new(targets))
    }

    pub(crate) fn get(
        &self,
        resolved_pattern: &ResolvedPattern<TargetName>,
    ) -> TargetSet<ConfiguredTargetNode> {
        let mut targets = TargetSet::new();
        for (package, spec) in &resolved_pattern.specs {
            targets.extend(self.get_from_package(package, spec).map(|(node, ())| node));
        }
        targets
    }

    pub fn get_provider_labels(
        &self,
        resolved_pattern: &ResolvedPattern<ProvidersPattern>,
    ) -> Vec<ConfiguredProvidersLabel> {
        let mut targets = Vec::new();
        for (package, spec) in &resolved_pattern.specs {
            targets.extend(
                self.get_from_package(package, spec)
                    .map(|(node, providers)| {
                        ConfiguredProvidersLabel::new(node.name().dupe(), providers)
                    }),
            );
        }
        targets
    }

    fn get_from_package<'a, P: PatternType>(
        &'a self,
        package: &Package,
        spec: &'a PackageSpec<P>,
    ) -> impl Iterator<Item = (&'a ConfiguredTargetNode, P::ExtraParts)> + 'a {
        self.targets
            .get(package)
            .into_iter()
            .flat_map(move |package_universe| match spec {
                PackageSpec::Targets(names) => Either::Left(names.iter().flat_map(|name| {
                    package_universe
                        .get(name.target())
                        .into_iter()
                        .flat_map(|nodes| {
                            nodes
                                .iter()
                                .map(|node| (&node.0, name.extra_parts().clone()))
                        })
                })),
                PackageSpec::All => Either::Right(
                    package_universe
                        .values()
                        .flatten()
                        .map(|node| (&node.0, P::ExtraParts::default())),
                ),
            })
    }

    pub(crate) fn owners(&self, path: &CellPath) -> Vec<ConfiguredTargetNode> {
        let mut nodes = Vec::new();

        // We lookup in all ancestors because we still have package boundary violations.
        // But we ignore symlinks: path may be a symlink target
        // for a target living in another directory.
        // This is another reason to not support symlinks in buck.
        for package in path.ancestors() {
            // Here we allocate package for possibly non-existent package,
            // violating `Package` assumptions.
            // This does not leave this function, so we are probably fine.
            // We do it because the map is by `Package`,
            // and `BTreeMap` does not allow lookup by equivalent key.
            let package = Package::from_cell_path(&package);
            let package_data = match self.targets.get(&package) {
                None => continue,
                Some(package_data) => package_data,
            };
            for node in package_data.values().flatten() {
                if node.0.inputs().contains(path) {
                    nodes.push(node.0.dupe());
                }
            }
        }
        nodes
    }
}
