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
use buck2_core::package::Package;
use buck2_core::pattern::PackageSpec;
use buck2_core::target::TargetName;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_query::query::syntax::simple::eval::label_indexed::LabelIndexed;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use gazebo::dupe::Dupe;

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
