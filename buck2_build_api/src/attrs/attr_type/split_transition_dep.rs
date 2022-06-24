/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::provider::label::ProvidersLabel;
use buck2_node::attrs::attr_type::attr_literal::AttrLiteral;
use buck2_node::attrs::attr_type::dep::DepAttrType;
use buck2_node::attrs::attr_type::dep::ProviderIdSet;
use buck2_node::attrs::attr_type::split_transition_dep::ConfiguredSplitTransitionDep;
use buck2_node::attrs::attr_type::split_transition_dep::SplitTransitionDepAttrType;
use buck2_node::attrs::attr_type::split_transition_dep::SplitTransitionDepMaybeConfigured;
use buck2_node::attrs::configuration_context::AttrConfigurationContext;
use buck2_node::attrs::configured_attr::ConfiguredAttr;
use derive_more::Display;
use gazebo::dupe::Dupe;
use serde_json::to_value;
use starlark::collections::Hashed;
use starlark::collections::SmallMap;
use starlark::values::dict::Dict;
use starlark::values::string::STRING_TYPE;
use starlark::values::Value;

use crate::attrs::analysis::AttrResolutionContext;
use crate::attrs::attr_type::attr_literal::CoercionError;
use crate::attrs::attr_type::coerce::AttrTypeCoerce;
use crate::attrs::attr_type::dep::DepAttrTypeExt;
use crate::attrs::coerced_attr::CoercedAttr;
use crate::attrs::configurable::AttrIsConfigurable;
use crate::attrs::AttrCoercionContext;

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
    fn configure(
        ctx: &dyn AttrConfigurationContext,
        dep_attr: &SplitTransitionDep,
    ) -> anyhow::Result<AttrLiteral<ConfiguredAttr>>;

    fn resolve_single<'v>(
        ctx: &'v dyn AttrResolutionContext,
        deps: &ConfiguredSplitTransitionDep,
    ) -> anyhow::Result<Value<'v>>;
}

impl SplitTransitionDepAttrTypeExt for SplitTransitionDepAttrType {
    fn configure(
        ctx: &dyn AttrConfigurationContext,
        dep_attr: &SplitTransitionDep,
    ) -> anyhow::Result<AttrLiteral<ConfiguredAttr>> {
        let configured_providers =
            ctx.configure_split_transition_target(&dep_attr.label, &dep_attr.transition)?;
        Ok(AttrLiteral::SplitTransitionDep(
            box ConfiguredSplitTransitionDep {
                deps: configured_providers,
                required_providers: dep_attr.required_providers.dupe(),
            },
        ))
    }

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

#[derive(Display, Debug, Hash, PartialEq, Eq, Clone)]
#[display(fmt = "{}", label)]
pub struct SplitTransitionDep {
    pub(crate) label: ProvidersLabel,
    pub(crate) transition: Arc<TransitionId>,
    pub(crate) required_providers: Option<Arc<ProviderIdSet>>,
}

impl SplitTransitionDepMaybeConfigured for SplitTransitionDep {
    fn to_json(&self) -> anyhow::Result<serde_json::Value> {
        Ok(to_value(self.to_string())?)
    }

    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool> {
        filter(&self.to_string())
    }
}
