/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_query::query::environment::LabeledNode;
use buck2_query::query::traversal::ChildVisitor;

/// Function to return the successors of a node.
pub trait GraphSuccessors<N> {
    fn for_each_successor(&self, node: &N, cb: impl FnMut(&N));
}

#[async_trait]
pub trait AsyncChildVisitor<N: LabeledNode>: Send + Sync {
    async fn for_each_child(
        &mut self,
        node: &N,
        children: &mut impl ChildVisitor<N>,
    ) -> anyhow::Result<()>;
}
