/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use allocative::Allocative;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::package::PackageLabel;
use buck2_core::package::source_path::SourcePathRef;
use buck2_core::plugins::PluginKind;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::label::label::TargetLabel;
use dupe::Dupe;
use mini_vec::MiniBoxSlice;
use pagable::Pagable;
use starlark_map::ordered_set::OrderedSet;
use starlark_map::vec2::Vec2;

use crate::attrs::attr_type::configuration_dep::ConfigurationDepKind;
use crate::attrs::traversal::CoercedAttrTraversal;

#[derive(Default, Debug, PartialEq, Eq, Hash, Allocative, Pagable)]
pub struct CoercedDeps {
    /// Contains the deps derived from the attributes.
    /// Does not include the transition, exec or configuration deps.
    pub deps: MiniBoxSlice<TargetLabel>,

    /// Contains the deps which are transitioned to other configuration
    /// (including split transitions).
    pub transition_deps: MiniBoxSlice<(TargetLabel, Arc<TransitionId>)>,

    /// Contains the execution deps derived from the attributes.
    pub exec_deps: MiniBoxSlice<TargetLabel>,

    /// Contains the toolchain deps derived from the attributes.
    pub toolchain_deps: MiniBoxSlice<TargetLabel>,

    /// Contains the configuration deps
    pub configuration_deps: Vec2<ProvidersLabel, ConfigurationDepKind>,

    /// Contains the plugin deps
    pub plugin_deps: MiniBoxSlice<TargetLabel>,
}

impl From<CoercedDepsCollector> for CoercedDeps {
    fn from(collector: CoercedDepsCollector) -> CoercedDeps {
        let CoercedDepsCollector {
            deps,
            transition_deps,
            exec_deps,
            toolchain_deps,
            configuration_deps,
            plugin_deps,
        } = collector;
        CoercedDeps {
            deps: deps.into_iter().collect(),
            transition_deps: transition_deps.into_iter().collect(),
            exec_deps: exec_deps.into_iter().collect(),
            toolchain_deps: toolchain_deps.into_iter().collect(),
            configuration_deps: configuration_deps.into_iter().collect(),
            plugin_deps: plugin_deps.into_iter().collect(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Allocative)]
pub struct CoercedDepsCollector {
    /// Contains the deps derived from the attributes.
    /// Does not include the transition, exec or configuration deps.
    pub deps: OrderedSet<TargetLabel>,

    /// Contains the deps which are transitioned to other configuration
    /// (including split transitions).
    pub transition_deps: OrderedSet<(TargetLabel, Arc<TransitionId>)>,

    /// Contains the execution deps derived from the attributes.
    pub exec_deps: OrderedSet<TargetLabel>,

    /// Contains the toolchain deps derived from the attributes.
    pub toolchain_deps: OrderedSet<TargetLabel>,

    /// Contains the configuration deps. These are deps that appear as conditions in selects.
    pub configuration_deps: OrderedSet<(ProvidersLabel, ConfigurationDepKind)>,

    /// Contains the plugin deps
    pub plugin_deps: OrderedSet<TargetLabel>,
}

impl CoercedDepsCollector {
    pub fn new() -> Self {
        Self {
            deps: OrderedSet::new(),
            exec_deps: OrderedSet::new(),
            toolchain_deps: OrderedSet::new(),
            transition_deps: OrderedSet::new(),
            configuration_deps: OrderedSet::new(),
            plugin_deps: OrderedSet::new(),
        }
    }
}

impl<'a> CoercedAttrTraversal<'a> for CoercedDepsCollector {
    fn dep(&mut self, dep: &ProvidersLabel) -> buck2_error::Result<()> {
        self.deps.insert(dep.target().dupe());
        Ok(())
    }

    fn exec_dep(&mut self, dep: &'a ProvidersLabel) -> buck2_error::Result<()> {
        self.exec_deps.insert(dep.target().dupe());
        Ok(())
    }

    fn toolchain_dep(&mut self, dep: &'a ProvidersLabel) -> buck2_error::Result<()> {
        self.toolchain_deps.insert(dep.target().dupe());
        Ok(())
    }

    fn transition_dep(
        &mut self,
        dep: &'a ProvidersLabel,
        tr: &Arc<TransitionId>,
    ) -> buck2_error::Result<()> {
        self.transition_deps
            .insert((dep.target().dupe(), tr.dupe()));
        Ok(())
    }

    fn split_transition_dep(
        &mut self,
        dep: &'a ProvidersLabel,
        tr: &Arc<TransitionId>,
    ) -> buck2_error::Result<()> {
        self.transition_deps
            .insert((dep.target().dupe(), tr.dupe()));
        Ok(())
    }

    fn configuration_dep(
        &mut self,
        dep: &ProvidersLabel,
        t: ConfigurationDepKind,
    ) -> buck2_error::Result<()> {
        self.configuration_deps.insert((dep.dupe(), t));
        Ok(())
    }

    fn plugin_dep(&mut self, dep: &'a TargetLabel, _kind: &PluginKind) -> buck2_error::Result<()> {
        self.plugin_deps.insert(dep.dupe());
        Ok(())
    }

    fn input(&mut self, _input: SourcePathRef) -> buck2_error::Result<()> {
        Ok(())
    }
}

/// Collects just the *packages* of a target's deps, deduplicated. Unlike [`CoercedDepsCollector`]
/// it stores no per-target dep data and keeps no per-bucket split — every dep of every kind
/// contributes its package, configuration deps included. Used where only dep package labels are
/// needed (e.g. build-signal load enrichment) so the caller doesn't have to materialize a node's
/// full `deps_cache`.
#[derive(Debug)]
pub struct DepPackagesCollector {
    pub packages: OrderedSet<PackageLabel>,
}

impl DepPackagesCollector {
    pub fn new() -> Self {
        Self {
            packages: OrderedSet::new(),
        }
    }
}

impl<'a> CoercedAttrTraversal<'a> for DepPackagesCollector {
    fn dep(&mut self, dep: &ProvidersLabel) -> buck2_error::Result<()> {
        self.packages.insert(dep.target().pkg());
        Ok(())
    }

    fn input(&mut self, _input: SourcePathRef) -> buck2_error::Result<()> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use buck2_core::cells::cell_path::CellPath;
    use buck2_core::provider::label::ProvidersLabel;
    use buck2_core::target::label::label::TargetLabel;

    use super::*;

    /// Drive a collector across every dep bucket: regular/transition/exec/toolchain/plugin
    /// (split-transition lands in the transition bucket) plus configuration. Every bucket reaches
    /// the collector through the `CoercedAttrTraversal` trait defaults routing to `dep`.
    fn drive_all_buckets<'a, T: CoercedAttrTraversal<'a>>(
        c: &mut T,
        dep: &'a ProvidersLabel,
        exec: &'a ProvidersLabel,
        toolchain: &'a ProvidersLabel,
        transition: &'a ProvidersLabel,
        split: &'a ProvidersLabel,
        plugin: &'a TargetLabel,
        cfg: &'a ProvidersLabel,
        tr_id: &Arc<TransitionId>,
        plugin_kind: &PluginKind,
    ) {
        c.dep(dep).unwrap();
        c.exec_dep(exec).unwrap();
        c.toolchain_dep(toolchain).unwrap();
        c.transition_dep(transition, tr_id).unwrap();
        c.split_transition_dep(split, tr_id).unwrap();
        c.plugin_dep(plugin, plugin_kind).unwrap();
        c.configuration_dep(cfg, ConfigurationDepKind::SelectKey)
            .unwrap();
    }

    /// `DepPackagesCollector` must collect the package of every dep bucket —
    /// regular/exec/toolchain/transition/split-transition/plugin *and* configuration. Driving
    /// every bucket guards against a future bucket whose trait default does not route to `dep`,
    /// which would silently drop those packages from `dep_packages()`.
    #[test]
    fn dep_packages_covers_every_bucket_including_configuration() {
        let dep = ProvidersLabel::default_for(TargetLabel::testing_parse("root//dep:d"));
        let exec = ProvidersLabel::default_for(TargetLabel::testing_parse("root//exec:e"));
        let toolchain = ProvidersLabel::default_for(TargetLabel::testing_parse("root//tc:t"));
        let transition = ProvidersLabel::default_for(TargetLabel::testing_parse("root//tr:x"));
        let split = ProvidersLabel::default_for(TargetLabel::testing_parse("root//split:s"));
        let plugin = TargetLabel::testing_parse("root//plugin:p");
        let cfg = ProvidersLabel::default_for(TargetLabel::testing_parse("root//cfg:c"));

        let tr_id = Arc::new(TransitionId::Target(ProvidersLabel::default_for(
            TargetLabel::testing_parse("root//tr:id"),
        )));
        let plugin_kind =
            PluginKind::new("p".to_owned(), CellPath::testing_new("root//plugins:kind"));

        let dedup = |mut v: Vec<PackageLabel>| {
            v.sort();
            v.dedup();
            v
        };

        let mut dep_packages = DepPackagesCollector::new();
        drive_all_buckets(
            &mut dep_packages,
            &dep,
            &exec,
            &toolchain,
            &transition,
            &split,
            &plugin,
            &cfg,
            &tr_id,
            &plugin_kind,
        );
        let got = dedup(dep_packages.packages.into_iter().collect());

        // Reference: the packages of every bucket `CoercedDepsCollector` splits deps into.
        let mut full = CoercedDepsCollector::new();
        drive_all_buckets(
            &mut full,
            &dep,
            &exec,
            &toolchain,
            &transition,
            &split,
            &plugin,
            &cfg,
            &tr_id,
            &plugin_kind,
        );
        let full = CoercedDeps::from(full);
        let expected = dedup(
            full.deps
                .iter()
                .chain(full.transition_deps.iter().map(|(d, _)| d))
                .chain(full.exec_deps.iter())
                .chain(full.toolchain_deps.iter())
                .chain(full.plugin_deps.iter())
                .map(|t| t.pkg())
                .chain(
                    full.configuration_deps
                        .iter()
                        .map(|(d, _)| d.target().pkg()),
                )
                .collect(),
        );

        assert_eq!(
            got, expected,
            "DepPackagesCollector must cover every dep bucket's packages"
        );
        for pkg in [
            dep.target().pkg(),
            exec.target().pkg(),
            toolchain.target().pkg(),
            transition.target().pkg(),
            split.target().pkg(),
            plugin.pkg(),
            cfg.target().pkg(),
        ] {
            assert!(got.contains(&pkg), "missing dep package `{pkg}`");
        }
    }
}
