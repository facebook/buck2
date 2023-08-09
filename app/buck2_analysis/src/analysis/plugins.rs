/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::interpreter::rule_defs::plugins::AnalysisPlugins;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_node::attrs::attr_type::dep::DepAttr;
use buck2_node::attrs::attr_type::dep::DepAttrTransition;
use buck2_node::attrs::attr_type::dep::DepAttrType;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::provider_id_set::ProviderIdSet;
use dupe::IterDupedExt;
use starlark::values::Value;
use starlark::values::ValueTyped;
use starlark_map::small_map::SmallMap;

use crate::attrs::resolve::attr_type::dep::DepAttrTypeExt;
use crate::attrs::resolve::ctx::AttrResolutionContext;

pub fn plugins_to_starlark_value<'v>(
    node: &ConfiguredTargetNode,
    ctx: &dyn AttrResolutionContext<'v>,
) -> anyhow::Result<ValueTyped<'v, AnalysisPlugins<'v>>> {
    let mut plugins = SmallMap::new();
    for kind in node.uses_plugins().iter().duped() {
        let deps: Vec<Value<'_>> = node
            .plugin_lists()
            .iter_for_kind(&kind)
            .map(|(target, _)| {
                DepAttrType::resolve_single(
                    ctx,
                    &DepAttr {
                        attr_type: DepAttrType::new(ProviderIdSet::EMPTY, DepAttrTransition::Exec),
                        label: ConfiguredProvidersLabel::default_for(
                            target
                                .configure_pair_no_exec(node.execution_platform_resolution().cfg()),
                        ),
                    },
                )
            })
            .collect::<anyhow::Result<_>>()?;
        plugins.insert(kind, ctx.heap().alloc(deps));
    }
    Ok(ctx.heap().alloc_typed(AnalysisPlugins::new(plugins)))
}
