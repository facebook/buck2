/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_node::attrs::attr_type::dep::DepAttrType;
use buck2_node::attrs::attr_type::split_transition_dep::ConfiguredSplitTransitionDep;
use buck2_node::attrs::attr_type::split_transition_dep::SplitTransitionDepAttrType;
use starlark::values::Value;
use starlark::values::dict::Dict;
use starlark_map::Hashed;
use starlark_map::small_map::SmallMap;

use crate::attrs::resolve::attr_type::dep::DepAttrTypeExt;
use crate::attrs::resolve::ctx::AttrResolutionContext;

pub(crate) trait SplitTransitionDepAttrTypeExt {
    fn resolve_single<'v>(
        ctx: &dyn AttrResolutionContext<'v>,
        deps: &ConfiguredSplitTransitionDep,
    ) -> buck2_error::Result<Value<'v>>;
}

impl SplitTransitionDepAttrTypeExt for SplitTransitionDepAttrType {
    fn resolve_single<'v>(
        ctx: &dyn AttrResolutionContext<'v>,
        deps: &ConfiguredSplitTransitionDep,
    ) -> buck2_error::Result<Value<'v>> {
        let mut res = SmallMap::with_capacity(deps.deps.len());
        for (label, target) in &deps.deps {
            let label_hashed = ctx.heap().alloc_str(label).get_hashed();
            res.insert_hashed(
                Hashed::new_unchecked(label_hashed.hash(), label_hashed.key().to_value()),
                DepAttrType::resolve_single_impl(ctx, target, &deps.required_providers, false)?,
            );
        }
        Ok(ctx.heap().alloc(Dict::new(res)))
    }
}
