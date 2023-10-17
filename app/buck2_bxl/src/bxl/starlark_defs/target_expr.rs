/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;

use buck2_query::query::environment::QueryTarget;

pub(crate) enum TargetExpr<'v, Node: QueryTarget> {
    Node(Node),
    Label(Cow<'v, Node::NodeRef>),
}

impl<'v, Node: QueryTarget> TargetExpr<'v, Node> {
    pub(crate) fn node_ref(&self) -> &Node::NodeRef {
        match self {
            TargetExpr::Node(node) => node.node_ref(),
            TargetExpr::Label(label) => label,
        }
    }
}
