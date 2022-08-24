/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_query::query::environment::QueryTarget;
use buck2_query::query::environment::QueryTargets;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use indexmap::indexmap;
use indexmap::IndexMap;
use regex::RegexSet;

use crate::dot::DotDigraph;
use crate::dot::DotEdge;
use crate::dot::DotNode;
use crate::dot::DotNodeAttrs;

pub struct DotTargetGraphNode<'a, T: QueryTarget>(&'a T, &'a DotTargetGraph<T>);

/// A simple adapter for creating a DotDiGraph for a TargetSet.
pub struct DotTargetGraph<T: QueryTarget> {
    pub targets: TargetSet<T>,
    pub attributes: Option<RegexSet>,
}

impl<'a, T: QueryTarget> DotDigraph<'a> for DotTargetGraph<T> {
    type Node = DotTargetGraphNode<'a, T>;

    fn name(&self) -> &str {
        "result_graph"
    }

    fn for_each_node<F: FnMut(&Self::Node) -> anyhow::Result<()>>(
        &'a self,
        mut f: F,
    ) -> anyhow::Result<()> {
        for node in self.targets.iter() {
            f(&DotTargetGraphNode(node, self))?;
        }
        Ok(())
    }

    fn for_each_edge<F: FnMut(&DotEdge) -> anyhow::Result<()>>(
        &'a self,
        node: &Self::Node,
        mut f: F,
    ) -> anyhow::Result<()> {
        for dep in node.0.deps() {
            // Only include edges to other nodes within the subgraph.
            if self.targets.contains(dep) {
                f(&DotEdge {
                    from: &node.0.node_ref().to_string(),
                    to: &dep.to_string(),
                })?;
            }
        }
        Ok(())
    }
}

impl<'a, T: QueryTarget> DotNode for DotTargetGraphNode<'a, T> {
    fn attrs(&self) -> anyhow::Result<DotNodeAttrs> {
        let extra = match &self.1.attributes {
            Some(attr_regex) => {
                let mut extra = IndexMap::new();
                QueryTargets::for_all_attrs::<anyhow::Error, _, _>(
                    self.0,
                    |attr_name, attr_value| {
                        if attr_regex.is_match(attr_name) {
                            extra
                                .insert(format!("buck_{}", attr_name), format!("{:#}", attr_value));
                        }
                        Ok(())
                    },
                )?;
                extra
            }
            None => indexmap![],
        };
        Ok(DotNodeAttrs {
            style: Some("filled".to_owned()),
            color: Some("#DFECDF".to_owned()),
            extra,
            ..DotNodeAttrs::default()
        })
    }

    fn id(&self) -> String {
        self.0.node_ref().to_string()
    }
}
