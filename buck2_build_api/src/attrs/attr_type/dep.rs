/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::sync::Arc;

use anyhow::anyhow;
use buck2_core::provider::id::ProviderId;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_node::attrs::attr_type::attr_literal::AttrLiteral;
use buck2_node::attrs::attr_type::configured_dep::ConfiguredExplicitConfiguredDep;
use buck2_node::attrs::attr_type::configured_dep::ExplicitConfiguredDepAttrType;
use buck2_node::attrs::attr_type::configured_dep::UnconfiguredExplicitConfiguredDep;
use buck2_node::attrs::attr_type::dep::DepAttr;
use buck2_node::attrs::attr_type::dep::DepAttrTransition;
use buck2_node::attrs::attr_type::dep::DepAttrType;
use buck2_node::attrs::attr_type::dep::ProviderIdSet;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use buck2_node::attrs::configuration_context::AttrConfigurationContext;
use buck2_node::attrs::configured_attr::ConfiguredAttr;
use gazebo::prelude::*;
use starlark::environment::Module;
use starlark::values::string::STRING_TYPE;
use starlark::values::UnpackValue;
use starlark::values::Value;
use thiserror::Error;

use crate::attrs::analysis::AttrResolutionContext;
use crate::attrs::attr_type::attr_literal::CoercionError;
use crate::attrs::attr_type::coerce::AttrTypeCoerce;
use crate::attrs::AttrCoercionContext;
use crate::attrs::CoercedAttr;
use crate::interpreter::rule_defs::provider::collection::FrozenProviderCollection;
use crate::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;
use crate::interpreter::rule_defs::provider::dependency::Dependency;

#[derive(Error, Debug)]
pub(crate) enum ResolutionError {
    #[error("required dependency {0} was not found")]
    MissingDep(ConfiguredProvidersLabel),
    #[error("required provider {0} was not found on {1}. Found these providers: {}", .2.join(", "))]
    MissingRequiredProvider(String, ConfiguredProvidersLabel, Vec<String>),
}

pub(crate) trait DepAttrTypeExt {
    fn configure(
        ctx: &dyn AttrConfigurationContext,
        dep_attr: &DepAttr<ProvidersLabel>,
    ) -> anyhow::Result<AttrLiteral<ConfiguredAttr>>;

    fn check_providers(
        required_providers: &[Arc<ProviderId>],
        providers: &FrozenProviderCollection,
        target: &ConfiguredProvidersLabel,
    ) -> anyhow::Result<()>;

    fn alloc_dependency<'v>(
        env: &'v Module,
        target: &ConfiguredProvidersLabel,
        v: &FrozenProviderCollectionValue,
    ) -> Value<'v>;

    fn resolve_single_impl<'v>(
        ctx: &'v dyn AttrResolutionContext,
        target: &ConfiguredProvidersLabel,
        required_providers: &Option<Arc<ProviderIdSet>>,
    ) -> anyhow::Result<Value<'v>>;

    fn resolve_single<'v>(
        ctx: &'v dyn AttrResolutionContext,
        dep_attr: &DepAttr<ConfiguredProvidersLabel>,
    ) -> anyhow::Result<Value<'v>>;
}

impl DepAttrTypeExt for DepAttrType {
    fn configure(
        ctx: &dyn AttrConfigurationContext,
        dep_attr: &DepAttr<ProvidersLabel>,
    ) -> anyhow::Result<AttrLiteral<ConfiguredAttr>> {
        let label = &dep_attr.label;
        let configured_label = match &dep_attr.attr_type.transition {
            DepAttrTransition::Identity => ctx.configure_target(label),
            DepAttrTransition::Exec => ctx.configure_exec_target(label),
            DepAttrTransition::Transition(tr) => ctx.configure_transition_target(label, tr)?,
        };
        Ok(AttrLiteral::Dep(box DepAttr::new(
            dep_attr.attr_type.dupe(),
            configured_label,
        )))
    }

    fn check_providers(
        required_providers: &[Arc<ProviderId>],
        providers: &FrozenProviderCollection,
        target: &ConfiguredProvidersLabel,
    ) -> anyhow::Result<()> {
        for provider_id in required_providers {
            if !providers.contains_provider(provider_id) {
                return Err(anyhow::anyhow!(ResolutionError::MissingRequiredProvider(
                    provider_id.name().to_owned(),
                    target.clone(),
                    providers.provider_names()
                )));
            }
        }
        Ok(())
    }

    fn alloc_dependency<'v>(
        env: &'v Module,
        target: &ConfiguredProvidersLabel,
        v: &FrozenProviderCollectionValue,
    ) -> Value<'v> {
        env.heap().alloc(Dependency::new(
            env.heap(),
            target.clone(),
            v.value().owned_value(env.frozen_heap()),
        ))
    }

    fn resolve_single_impl<'v>(
        ctx: &'v dyn AttrResolutionContext,
        target: &ConfiguredProvidersLabel,
        required_providers: &Option<Arc<ProviderIdSet>>,
    ) -> anyhow::Result<Value<'v>> {
        let v = ctx.get_dep(target)?;
        let provider_collection = v.provider_collection();
        if let Some(provider_ids) = required_providers {
            Self::check_providers(provider_ids, provider_collection, target)?;
        }

        Ok(Self::alloc_dependency(ctx.starlark_module(), target, &v))
    }

    fn resolve_single<'v>(
        ctx: &'v dyn AttrResolutionContext,
        dep_attr: &DepAttr<ConfiguredProvidersLabel>,
    ) -> anyhow::Result<Value<'v>> {
        Self::resolve_single_impl(ctx, &dep_attr.label, &dep_attr.attr_type.required_providers)
    }
}

impl AttrTypeCoerce for DepAttrType {
    fn coerce_item(
        &self,
        _configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral<CoercedAttr>> {
        let label = value
            .unpack_str()
            .ok_or_else(|| anyhow!(CoercionError::type_error(STRING_TYPE, value)))?;

        let label = ctx.coerce_label(label)?;

        Ok(AttrLiteral::Dep(box DepAttr::new(self.dupe(), label)))
    }

    fn starlark_type(&self) -> String {
        "str.type".to_owned()
    }
}

pub(crate) trait ExplicitConfiguredDepAttrTypeExt {
    fn configure(
        ctx: &dyn AttrConfigurationContext,
        dep_attr: &UnconfiguredExplicitConfiguredDep,
    ) -> anyhow::Result<AttrLiteral<ConfiguredAttr>> {
        let configured = Self::configure_target_with_platform(ctx, dep_attr)?;
        Ok(AttrLiteral::ExplicitConfiguredDep(box configured))
    }

    fn configure_target_with_platform(
        ctx: &dyn AttrConfigurationContext,
        dep_attr: &UnconfiguredExplicitConfiguredDep,
    ) -> anyhow::Result<ConfiguredExplicitConfiguredDep> {
        let configuration = ctx.platform_cfg(&dep_attr.platform)?;
        let configured_label = dep_attr.label.configure(configuration.dupe());
        Ok(ConfiguredExplicitConfiguredDep::new(
            dep_attr.attr_type.dupe(),
            configured_label,
        ))
    }

    fn resolve_single<'v>(
        ctx: &'v dyn AttrResolutionContext,
        dep_attr: &ConfiguredExplicitConfiguredDep,
    ) -> anyhow::Result<Value<'v>> {
        DepAttrType::resolve_single_impl(
            ctx,
            &dep_attr.label,
            &dep_attr.attr_type.required_providers,
        )
    }
}

impl ExplicitConfiguredDepAttrTypeExt for ExplicitConfiguredDepAttrType {}

impl AttrTypeCoerce for ExplicitConfiguredDepAttrType {
    fn coerce_item(
        &self,
        _configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral<CoercedAttr>> {
        let (label_value, platform_value): (Value, Value) = UnpackValue::unpack_value(value)
            .ok_or_else(|| {
                anyhow!(CoercionError::type_error(
                    "Tuple must be a pair of two strings",
                    value,
                ))
            })?;

        let label_string = label_value
            .unpack_str()
            .ok_or_else(|| anyhow!(CoercionError::type_error(STRING_TYPE, value)))?;
        let label = ctx.coerce_label(label_string)?;

        let platform_string = platform_value
            .unpack_str()
            .ok_or_else(|| anyhow!(CoercionError::type_error(STRING_TYPE, value)))?;
        let platform = ctx.coerce_target(platform_string)?;

        Ok(AttrLiteral::ExplicitConfiguredDep(
            box UnconfiguredExplicitConfiguredDep {
                attr_type: self.dupe(),
                label,
                platform,
            },
        ))
    }

    fn starlark_type(&self) -> String {
        "(str.type, str.type)".to_owned()
    }
}
