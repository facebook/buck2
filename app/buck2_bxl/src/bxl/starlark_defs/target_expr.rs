/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;

use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use buck2_node::nodes::frontend::TargetGraphCalculation;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_query::query::environment::QueryTarget;
use dice::DiceComputations;
use dupe::Dupe;

#[derive(Clone)]
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

impl<'v> TargetExpr<'v, ConfiguredTargetNode> {
    pub(crate) async fn get_from_dice(
        &self,
        dice: &DiceComputations,
    ) -> anyhow::Result<MaybeCompatible<ConfiguredTargetNode>> {
        match self {
            TargetExpr::Node(node) => Ok(MaybeCompatible::Compatible(node.dupe())),
            TargetExpr::Label(label) => dice.get_configured_target_node(label).await,
        }
    }
}

impl<'v> TargetExpr<'v, TargetNode> {
    pub(crate) async fn get_from_dice(
        &self,
        dice: &DiceComputations,
    ) -> anyhow::Result<TargetNode> {
        match self {
            TargetExpr::Node(node) => Ok(node.dupe()),
            TargetExpr::Label(label) => dice.get_target_node(label).await,
        }
    }
}
