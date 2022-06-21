/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;
use std::fmt::Display;
use std::fmt::Formatter;
use std::sync::Arc;

use buck2_core::provider::ConfiguredProvidersLabel;
use buck2_core::provider::ProvidersLabel;
use derive_more::Display;
use gazebo::dupe::Dupe;
use serde_json::to_value;
use starlark::collections::Hashed;
use starlark::collections::SmallMap;
use starlark::values::dict::Dict;
use starlark::values::string::STRING_TYPE;
use starlark::values::Value;

use crate::attrs::attr_type::attr_literal::AttrLiteral;
use crate::attrs::attr_type::attr_literal::CoercionError;
use crate::attrs::attr_type::dep::DepAttrType;
use crate::attrs::attr_type::dep::ProviderIdSet;
use crate::attrs::coerced_attr::CoercedAttr;
use crate::attrs::AttrCoercionContext;
use crate::attrs::AttrConfigurationContext;
use crate::attrs::AttrResolutionContext;
use crate::attrs::ConfiguredAttr;
use crate::interpreter::rule_defs::transition::id::TransitionId;

#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) struct SplitTransitionDepAttrType {
    required_providers: Option<Arc<ProviderIdSet>>,
    transition: Arc<TransitionId>,
}

impl SplitTransitionDepAttrType {
    pub(crate) fn new(required_providers: ProviderIdSet, transition: Arc<TransitionId>) -> Self {
        let required_providers = if required_providers.is_empty() {
            None
        } else {
            Some(Arc::new(required_providers))
        };
        SplitTransitionDepAttrType {
            required_providers,
            transition,
        }
    }

    pub(crate) fn coerce_item(
        &self,
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

    pub(crate) fn starlark_type(&self) -> String {
        "str.type".to_owned()
    }

    pub(crate) fn configure(
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

    pub(crate) fn resolve_single<'v>(
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

#[derive(Hash, PartialEq, Eq, Debug, Clone)]
pub struct ConfiguredSplitTransitionDep {
    pub(crate) deps: BTreeMap<String, ConfiguredProvidersLabel>,
    pub(crate) required_providers: Option<Arc<ProviderIdSet>>,
}

impl Display for ConfiguredSplitTransitionDep {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for (i, dep) in self.deps.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{:?}: {}", dep.0, dep.1)?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

/// Configured or unconfigured.
pub trait SplitTransitionDepMaybeConfigured {
    fn to_json(&self) -> anyhow::Result<serde_json::Value>;
    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool>;
}

impl SplitTransitionDepMaybeConfigured for SplitTransitionDep {
    fn to_json(&self) -> anyhow::Result<serde_json::Value> {
        Ok(to_value(self.to_string())?)
    }

    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool> {
        filter(&self.to_string())
    }
}

impl SplitTransitionDepMaybeConfigured for ConfiguredSplitTransitionDep {
    fn to_json(&self) -> anyhow::Result<serde_json::Value> {
        let mut map = serde_json::Map::with_capacity(self.deps.len());
        for (label, target) in &self.deps {
            map.insert(label.clone(), to_value(target.to_string())?);
        }
        Ok(serde_json::Value::Object(map))
    }

    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool> {
        for (label, target) in &self.deps {
            if filter(label)? || filter(&target.to_string())? {
                return Ok(true);
            }
        }
        Ok(false)
    }
}
