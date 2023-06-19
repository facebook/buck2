/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::Allocative;
use buck2_core::buck_path::path::BuckPathRef;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::target::label::TargetLabel;
use buck2_util::collections::ordered_set::OrderedSet;
use dupe::Dupe;

use crate::attrs::traversal::CoercedAttrTraversal;

#[derive(Default, Debug, PartialEq, Eq, Hash, Allocative)]
pub struct CoercedDeps {
    /// Contains the deps derived from the attributes.
    /// Does not include the transition, exec or configuration deps.
    pub deps: Box<[TargetLabel]>,

    /// Contains the deps which are transitioned to other configuration
    /// (including split transitions).
    pub transition_deps: Box<[(TargetLabel, Arc<TransitionId>)]>,

    /// Contains the execution deps derived from the attributes.
    pub exec_deps: Box<[TargetLabel]>,

    /// Contains the toolchain deps derived from the attributes.
    pub toolchain_deps: Box<[TargetLabel]>,

    /// Contains the configuration deps. These are deps that appear as conditions in selects.
    pub configuration_deps: Box<[TargetLabel]>,

    /// Contains platform targets of configured_alias()
    pub platform_deps: Box<[TargetLabel]>,
}

impl From<CoercedDepsCollector> for CoercedDeps {
    fn from(collector: CoercedDepsCollector) -> CoercedDeps {
        let CoercedDepsCollector {
            deps,
            transition_deps,
            exec_deps,
            toolchain_deps,
            configuration_deps,
            platform_deps,
        } = collector;
        CoercedDeps {
            deps: deps.into_iter().collect(),
            transition_deps: transition_deps.into_iter().collect(),
            exec_deps: exec_deps.into_iter().collect(),
            toolchain_deps: toolchain_deps.into_iter().collect(),
            configuration_deps: configuration_deps.into_iter().collect(),
            platform_deps: platform_deps.into_iter().collect(),
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
    pub configuration_deps: OrderedSet<TargetLabel>,

    /// Contains platform targets of configured_alias()
    pub platform_deps: OrderedSet<TargetLabel>,
}

impl CoercedDepsCollector {
    pub fn new() -> Self {
        Self {
            deps: OrderedSet::new(),
            exec_deps: OrderedSet::new(),
            toolchain_deps: OrderedSet::new(),
            transition_deps: OrderedSet::new(),
            configuration_deps: OrderedSet::new(),
            platform_deps: OrderedSet::new(),
        }
    }
}

impl<'a> CoercedAttrTraversal<'a> for CoercedDepsCollector {
    fn dep(&mut self, dep: &'a TargetLabel) -> anyhow::Result<()> {
        self.deps.insert(dep.dupe());
        Ok(())
    }

    fn exec_dep(&mut self, dep: &'a TargetLabel) -> anyhow::Result<()> {
        self.exec_deps.insert(dep.dupe());
        Ok(())
    }

    fn toolchain_dep(&mut self, dep: &'a TargetLabel) -> anyhow::Result<()> {
        self.toolchain_deps.insert(dep.dupe());
        Ok(())
    }

    fn transition_dep(
        &mut self,
        dep: &'a TargetLabel,
        tr: &Arc<TransitionId>,
    ) -> anyhow::Result<()> {
        self.transition_deps.insert((dep.dupe(), tr.dupe()));
        Ok(())
    }

    fn split_transition_dep(
        &mut self,
        dep: &'a TargetLabel,
        tr: &Arc<TransitionId>,
    ) -> anyhow::Result<()> {
        self.transition_deps.insert((dep.dupe(), tr.dupe()));
        Ok(())
    }

    fn configuration_dep(&mut self, dep: &'a TargetLabel) -> anyhow::Result<()> {
        self.configuration_deps.insert(dep.dupe());
        Ok(())
    }

    fn platform_dep(&mut self, dep: &'a TargetLabel) -> anyhow::Result<()> {
        self.platform_deps.insert(dep.dupe());
        Ok(())
    }

    fn input(&mut self, _input: BuckPathRef) -> anyhow::Result<()> {
        Ok(())
    }
}
