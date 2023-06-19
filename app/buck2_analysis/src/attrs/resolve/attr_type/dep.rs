/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::interpreter::rule_defs::provider::collection::FrozenProviderCollection;
use buck2_build_api::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;
use buck2_build_api::interpreter::rule_defs::provider::dependency::Dependency;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_node::attrs::attr_type::configured_dep::ConfiguredExplicitConfiguredDep;
use buck2_node::attrs::attr_type::configured_dep::ExplicitConfiguredDepAttrType;
use buck2_node::attrs::attr_type::dep::DepAttr;
use buck2_node::attrs::attr_type::dep::DepAttrType;
use buck2_node::provider_id_set::ProviderIdSet;
use starlark::environment::Module;
use starlark::values::Value;
use thiserror::Error;

use crate::attrs::resolve::ctx::AttrResolutionContext;

#[derive(Error, Debug)]
enum ResolutionError {
    #[error("required provider `{0}` was not found on `{1}`. Found these providers: {}", .2.join(", "))]
    MissingRequiredProvider(String, ConfiguredProvidersLabel, Vec<String>),
}

pub trait DepAttrTypeExt {
    fn check_providers(
        required_providers: &ProviderIdSet,
        providers: &FrozenProviderCollection,
        target: &ConfiguredProvidersLabel,
    ) -> anyhow::Result<()>;

    fn alloc_dependency<'v>(
        env: &'v Module,
        target: &ConfiguredProvidersLabel,
        v: &FrozenProviderCollectionValue,
    ) -> Value<'v>;

    fn resolve_single_impl<'v>(
        ctx: &dyn AttrResolutionContext<'v>,
        target: &ConfiguredProvidersLabel,
        required_providers: &ProviderIdSet,
    ) -> anyhow::Result<Value<'v>>;

    fn resolve_single<'v>(
        ctx: &dyn AttrResolutionContext<'v>,
        dep_attr: &DepAttr<ConfiguredProvidersLabel>,
    ) -> anyhow::Result<Value<'v>>;
}

impl DepAttrTypeExt for DepAttrType {
    fn check_providers(
        required_providers: &ProviderIdSet,
        providers: &FrozenProviderCollection,
        target: &ConfiguredProvidersLabel,
    ) -> anyhow::Result<()> {
        for provider_id in required_providers {
            if !providers.contains_provider(provider_id) {
                return Err(ResolutionError::MissingRequiredProvider(
                    provider_id.name().to_owned(),
                    target.clone(),
                    providers.provider_names(),
                )
                .into());
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
        ctx: &dyn AttrResolutionContext<'v>,
        target: &ConfiguredProvidersLabel,
        required_providers: &ProviderIdSet,
    ) -> anyhow::Result<Value<'v>> {
        let v = ctx.get_dep(target)?;
        let provider_collection = v.provider_collection();
        Self::check_providers(required_providers, provider_collection, target)?;

        Ok(Self::alloc_dependency(ctx.starlark_module(), target, &v))
    }

    fn resolve_single<'v>(
        ctx: &dyn AttrResolutionContext<'v>,
        dep_attr: &DepAttr<ConfiguredProvidersLabel>,
    ) -> anyhow::Result<Value<'v>> {
        Self::resolve_single_impl(ctx, &dep_attr.label, &dep_attr.attr_type.required_providers)
    }
}

pub(crate) trait ExplicitConfiguredDepAttrTypeExt {
    fn resolve_single<'v>(
        ctx: &dyn AttrResolutionContext<'v>,
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
