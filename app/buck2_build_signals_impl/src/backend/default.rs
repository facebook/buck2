/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Display;
use std::hash::Hash;
use std::sync::Arc;
use std::time::Duration;

use anyhow::Context as _;
use buck2_build_api::actions::RegisteredAction;
use buck2_build_api::build_signals::NodeDuration;
use buck2_build_signals::CriticalPathBackendName;
use buck2_events::span::SpanId;
use dupe::Dupe;
use gazebo::prelude::VecExt;
use itertools::Itertools;
use smallvec::SmallVec;

use crate::backend::backend::BuildListenerBackend;
use crate::BuildInfo;
use crate::NodeData;
use crate::NodeKey;

#[derive(Clone, Dupe)]
struct CriticalPathNode<TKey: Eq, TValue> {
    /// The aggregated duration of this critical path.
    pub duration: Duration,
    /// The value of this node. If None, this node just won't be included when displaying.
    pub value: TValue,
    pub prev: Option<TKey>,
}

fn extract_critical_path<TKey: Hash + Eq, TValue>(
    predecessors: &HashMap<TKey, CriticalPathNode<TKey, TValue>>,
) -> anyhow::Result<Vec<(&TKey, &TValue, Duration)>>
where
    TKey: Display,
{
    let mut tail = predecessors
        .iter()
        .max_by_key(|(_key, data)| data.duration)
        .map(|q| q.0);

    let mut path = vec![];
    let mut visited = HashSet::new();

    while let Some(v) = tail.take() {
        if !visited.insert(v) {
            return Err(anyhow::anyhow!(
                "Cycle in critical path: visited {} twice",
                v
            ));
        }

        tail = predecessors.get(v).and_then(|node| {
            path.push((v, &node.value, node.duration));
            node.prev.as_ref()
        });
    }

    // Take differences of adjacent elements to recover action time from cumulative sum.
    path.reverse();
    for i in (1..path.len()).rev() {
        path[i].2 = path[i].2.saturating_sub(path[i - 1].2);
    }

    Ok(path)
}

pub(crate) struct DefaultBackend {
    predecessors: HashMap<NodeKey, CriticalPathNode<NodeKey, NodeData>>,
    num_nodes: u64,
    num_edges: u64,
}

impl DefaultBackend {
    pub(crate) fn new() -> Self {
        Self {
            predecessors: HashMap::new(),
            num_nodes: 0,
            num_edges: 0,
        }
    }
}

impl BuildListenerBackend for DefaultBackend {
    fn process_node(
        &mut self,
        key: NodeKey,
        value: Option<Arc<RegisteredAction>>,
        duration: NodeDuration,
        dep_keys: impl Iterator<Item = NodeKey>,
        span_ids: SmallVec<[SpanId; 1]>,
    ) {
        let longest_ancestor = dep_keys
            .unique()
            .filter_map(|node_key| {
                self.num_edges += 1;
                let node_data = self.predecessors.get(&node_key)?;
                Some((node_key, node_data.duration))
            })
            .max_by_key(|d| d.1);

        let value = NodeData {
            action: value,
            duration,
            span_ids,
        };

        let node = match longest_ancestor {
            Some((key, ancestor_duration)) => CriticalPathNode {
                prev: Some(key.dupe()),
                value,
                duration: ancestor_duration + duration.critical_path_duration(),
            },
            None => CriticalPathNode {
                prev: None,
                value,
                duration: duration.critical_path_duration(),
            },
        };

        self.num_nodes += 1;
        self.predecessors.insert(key, node);
    }

    fn process_top_level_target(
        &mut self,
        _analysis: NodeKey,
        _artifacts: impl Iterator<Item = NodeKey>,
    ) {
    }

    fn finish(self) -> anyhow::Result<BuildInfo> {
        let critical_path = extract_critical_path(&self.predecessors)
            .context("Error extracting critical path")?
            .into_map(|(key, data, _duration)| (key.dupe(), data.clone(), None));

        Ok(BuildInfo {
            critical_path,
            num_nodes: self.num_nodes,
            num_edges: self.num_edges,
        })
    }

    fn name() -> CriticalPathBackendName {
        CriticalPathBackendName::Default
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    type CriticalPathMap = HashMap<i32, CriticalPathNode<i32, Option<i32>>>;

    fn cp_insert(
        predecessors: &mut CriticalPathMap,
        key: i32,
        prev: Option<i32>,
        duration: Duration,
    ) {
        predecessors.insert(
            key,
            CriticalPathNode {
                duration,
                value: Some(key),
                prev,
            },
        );
    }
    #[test]
    fn empty_path() {
        let predecessors = CriticalPathMap::new();
        assert_eq!(extract_critical_path(&predecessors).unwrap(), vec![]);
    }

    #[test]
    fn unit_path() {
        let mut predecessors = CriticalPathMap::new();
        cp_insert(&mut predecessors, 1, None, Duration::from_secs(3));
        assert_eq!(
            extract_critical_path(&predecessors).unwrap(),
            vec![(&1, &Some(1), Duration::from_secs(3))],
        );
    }

    #[test]
    fn long_path() {
        let mut predecessors = HashMap::new();
        /*   -> 1 -> 2 -> 3
         *   5s   6s   7s
         *
         *      1 -> 4
         *        9s
         */
        cp_insert(&mut predecessors, 1, None, Duration::from_secs(5));
        cp_insert(&mut predecessors, 2, Some(1), Duration::from_secs(11));
        cp_insert(&mut predecessors, 3, Some(2), Duration::from_secs(18));
        cp_insert(&mut predecessors, 4, Some(1), Duration::from_secs(14));
        assert_eq!(
            extract_critical_path(&predecessors).unwrap(),
            vec![
                (&1, &Some(1), Duration::from_secs(5)),
                (&2, &Some(2), Duration::from_secs(6)),
                (&3, &Some(3), Duration::from_secs(7)),
            ],
        );
    }

    #[test]
    fn cycle_path() {
        let mut predecessors = HashMap::new();
        cp_insert(&mut predecessors, 1, Some(2), Duration::from_secs(5));
        cp_insert(&mut predecessors, 2, Some(1), Duration::from_secs(11));
        assert!(extract_critical_path(&predecessors).is_err());
    }
}
