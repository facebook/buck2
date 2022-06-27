/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_node::attrs::attr_type::attr_literal::AttrLiteral;
use buck2_node::attrs::attr_type::dep::DepAttrType;
use buck2_node::attrs::attr_type::split_transition_dep::ConfiguredSplitTransitionDep;
use buck2_node::attrs::attr_type::split_transition_dep::SplitTransitionDep;
use buck2_node::attrs::attr_type::split_transition_dep::SplitTransitionDepAttrType;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use gazebo::dupe::Dupe;
use starlark::collections::Hashed;
use starlark::collections::SmallMap;
use starlark::values::dict::Dict;
use starlark::values::string::STRING_TYPE;
use starlark::values::Value;

use crate::attrs::analysis::AttrResolutionContext;
use crate::attrs::attr_type::attr_literal::CoercionError;
use crate::attrs::attr_type::coerce::AttrTypeCoerce;
use crate::attrs::attr_type::dep::DepAttrTypeExt;

impl AttrTypeCoerce for SplitTransitionDepAttrType {
    fn coerce_item(
        &self,
        _configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral<CoercedAttr>> {
        let label = value
            .unpack_str()
            .ok_or_else(|| CoercionError::type_error(STRING_TYPE, value))?;

        let label = ctx.coerce_label(label)?;

        Ok(AttrLiteral::SplitTransitionDep(box SplitTransitionDep {
            label,
            transition: self.transition.dupe(),
            required_providers: self.required_providers.dupe(),
        }))
    }

    fn starlark_type(&self) -> String {
        "str.type".to_owned()
    }
}

pub(crate) trait SplitTransitionDepAttrTypeExt {
    fn resolve_single<'v>(
        ctx: &'v dyn AttrResolutionContext,
        deps: &ConfiguredSplitTransitionDep,
    ) -> anyhow::Result<Value<'v>>;
}

impl SplitTransitionDepAttrTypeExt for SplitTransitionDepAttrType {
    fn resolve_single<'v>(
        ctx: &'v dyn AttrResolutionContext,
        deps: &ConfiguredSplitTransitionDep,
    ) -> anyhow::Result<Value<'v>> {
        let mut res = SmallMap::with_capacity(deps.deps.len());
        for (label, target) in &deps.deps {
            let label_hashed = ctx.heap().alloc_str(label).get_hashed();
            res.insert_hashed(
                Hashed::new_unchecked(label_hashed.hash(), label_hashed.key().to_value()),
                DepAttrType::resolve_single_impl(ctx, target, &deps.required_providers)?,
            );
        }
        Ok(ctx.heap().alloc(Dict::new(res)))
    }
}
