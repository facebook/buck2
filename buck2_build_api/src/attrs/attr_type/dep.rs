/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::sync::Arc;

use anyhow::anyhow;
use buck2_core::provider::id::ProviderId;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersLabelMaybeConfigured;
use buck2_core::target::TargetLabel;
use derive_more::Display;
use gazebo::prelude::*;
use serde_json::to_value;
use starlark::environment::Module;
use starlark::values::string::STRING_TYPE;
use starlark::values::UnpackValue;
use starlark::values::Value;
use thiserror::Error;

use crate::attrs::analysis::AttrResolutionContext;
use crate::attrs::attr_type::attr_literal::AttrLike;
use crate::attrs::attr_type::attr_literal::AttrLiteral;
use crate::attrs::attr_type::attr_literal::CoercionError;
use crate::attrs::attr_type::attr_literal::ConfiguredAttrTraversal;
use crate::attrs::AttrCoercionContext;
use crate::attrs::AttrConfigurationContext;
use crate::attrs::CoercedAttr;
use crate::attrs::CoercedAttrTraversal;
use crate::attrs::ConfiguredAttr;
use crate::interpreter::rule_defs::provider::collection::FrozenProviderCollection;
use crate::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;
use crate::interpreter::rule_defs::provider::dependency::Dependency;
use crate::interpreter::rule_defs::transition::id::TransitionId;

#[derive(Error, Debug)]
pub(crate) enum ResolutionError {
    #[error("required dependency {0} was not found")]
    MissingDep(ConfiguredProvidersLabel),
    #[error("required provider {0} was not found on {1}. Found these providers: {}", .2.join(", "))]
    MissingRequiredProvider(String, ConfiguredProvidersLabel, Vec<String>),
}

// Just a placeholder for what a label should resolve to.
#[derive(Debug)]
pub struct DefaultProvider {}

pub type ProviderIdSet = Vec<Arc<ProviderId>>;

/// How configuration is changed when configuring a dep.
#[derive(Debug, Eq, PartialEq, Hash, Clone, Dupe)]
pub enum DepAttrTransition {
    // No transition.
    Identity,
    // Transition to execution platform.
    Exec,
    // Transition dependency using given transition function.
    Transition(Arc<TransitionId>),
}

/// A dep attribute accepts a target label and will resolve to the provider collection from that label's analysis.
#[derive(Debug, Eq, PartialEq, Hash, Clone, Dupe)]
pub(crate) struct DepAttrType {
    /// The set of providers that are required to be available, during attr resolution we'll verify that these
    /// are present on each attribute value.
    ///
    /// Both None and Some(Arc([])) represent that no specific providers are required.
    required_providers: Option<Arc<ProviderIdSet>>,
    transition: DepAttrTransition,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct DepAttr<T: ProvidersLabelMaybeConfigured + AttrLike> {
    attr_type: DepAttrType,
    label: T,
}

impl<T: ProvidersLabelMaybeConfigured + AttrLike> DepAttr<T> {
    pub(crate) fn new(attr_type: DepAttrType, label: T) -> Self {
        Self { attr_type, label }
    }

    pub(crate) fn label(&self) -> &T {
        &self.label
    }

    pub(crate) fn into_label(self) -> T {
        self.label
    }
}

impl<T: ProvidersLabelMaybeConfigured + AttrLike> Display for DepAttr<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.label, f)
    }
}

impl DepAttr<ProvidersLabel> {
    pub(crate) fn traverse<'a>(
        &'a self,
        traversal: &mut dyn CoercedAttrTraversal<'a>,
    ) -> anyhow::Result<()> {
        match &self.attr_type.transition {
            DepAttrTransition::Identity => traversal.dep(self.label.target()),
            DepAttrTransition::Exec => traversal.exec_dep(self.label.target()),
            DepAttrTransition::Transition(tr) => traversal.transition_dep(self.label.target(), tr),
        }
    }
}

impl DepAttr<ConfiguredProvidersLabel> {
    pub fn traverse<'a>(
        &'a self,
        traversal: &mut dyn ConfiguredAttrTraversal<'a>,
    ) -> anyhow::Result<()> {
        match &self.attr_type.transition {
            DepAttrTransition::Identity => traversal.dep(&self.label),
            DepAttrTransition::Exec => traversal.exec_dep(&self.label),
            DepAttrTransition::Transition(..) => traversal.dep(&self.label),
        }
    }
}

impl DepAttrType {
    pub fn new(required_providers: ProviderIdSet, transition: DepAttrTransition) -> Self {
        let required_providers = if required_providers.is_empty() {
            None
        } else {
            Some(Arc::new(required_providers))
        };
        Self {
            required_providers,
            transition,
        }
    }

    pub(crate) fn configure(
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

    pub fn check_providers(
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

    pub fn alloc_dependency<'v>(
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

    pub(crate) fn resolve_single_impl<'v>(
        ctx: &'v dyn AttrResolutionContext,
        target: &ConfiguredProvidersLabel,
        required_providers: &Option<Arc<ProviderIdSet>>,
    ) -> anyhow::Result<Value<'v>> {
        match ctx.get_dep(target) {
            Some(v) => {
                let provider_collection = v.provider_collection();
                if let Some(provider_ids) = required_providers {
                    Self::check_providers(provider_ids, provider_collection, target)?;
                }

                Ok(Self::alloc_dependency(ctx.starlark_module(), target, &v))
            }
            None => Err(anyhow::anyhow!(ResolutionError::MissingDep(target.clone()))),
        }
    }

    pub(crate) fn resolve_single<'v>(
        ctx: &'v dyn AttrResolutionContext,
        dep_attr: &DepAttr<ConfiguredProvidersLabel>,
    ) -> anyhow::Result<Value<'v>> {
        Self::resolve_single_impl(ctx, &dep_attr.label, &dep_attr.attr_type.required_providers)
    }
}

impl DepAttrType {
    pub(crate) fn coerce_item(
        &self,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral<CoercedAttr>> {
        let label = value
            .unpack_str()
            .ok_or_else(|| anyhow!(CoercionError::type_error(STRING_TYPE, value)))?;

        let label = ctx.coerce_label(label)?;

        Ok(AttrLiteral::Dep(box DepAttr::new(self.dupe(), label)))
    }

    pub(crate) fn starlark_type(&self) -> String {
        "str.type".to_owned()
    }
}

/// Represents attr.configured_dep()
#[derive(Debug, Eq, PartialEq, Hash, Clone, Dupe)]
pub(crate) struct ExplicitConfiguredDepAttrType {
    required_providers: Option<Arc<ProviderIdSet>>,
}

impl ExplicitConfiguredDepAttrType {
    pub fn new(required_providers: ProviderIdSet) -> Self {
        let required_providers = if required_providers.is_empty() {
            None
        } else {
            Some(Arc::new(required_providers))
        };

        Self { required_providers }
    }

    pub(crate) fn configure(
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
}

impl ExplicitConfiguredDepAttrType {
    pub(crate) fn coerce_item(
        &self,
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

    pub(crate) fn starlark_type(&self) -> String {
        "(str.type, str.type)".to_owned()
    }

    pub(crate) fn resolve_single<'v>(
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

/// Represents the value of an `attr.configured_dep()`
/// in its unconfigured form.
#[derive(Display, Debug, Hash, PartialEq, Eq, Clone)]
#[display(fmt = "({}, {})", label, platform)]
pub struct UnconfiguredExplicitConfiguredDep {
    attr_type: ExplicitConfiguredDepAttrType,
    pub(crate) label: ProvidersLabel,
    pub(crate) platform: TargetLabel,
}

/// Represents the value of an `attr.configured_dep()`
/// in its configured form.
#[derive(Display, Hash, PartialEq, Eq, Debug, Clone)]
#[display(fmt = "{}", label)]
pub struct ConfiguredExplicitConfiguredDep {
    attr_type: ExplicitConfiguredDepAttrType,
    pub(crate) label: ConfiguredProvidersLabel,
}

impl ConfiguredExplicitConfiguredDep {
    fn new(attr_type: ExplicitConfiguredDepAttrType, label: ConfiguredProvidersLabel) -> Self {
        Self { attr_type, label }
    }
}

impl UnconfiguredExplicitConfiguredDep {
    pub(crate) fn traverse<'a>(
        &'a self,
        traversal: &mut dyn CoercedAttrTraversal<'a>,
    ) -> anyhow::Result<()> {
        traversal.dep(self.label.target())?;
        traversal.platform_dep(&self.platform)
    }
}

impl ConfiguredExplicitConfiguredDep {
    pub fn traverse<'a>(
        &'a self,
        traversal: &mut dyn ConfiguredAttrTraversal<'a>,
    ) -> anyhow::Result<()> {
        traversal.dep(&self.label)
    }
}

/// Represents both configured and unconfigured forms.
pub trait ExplicitConfiguredDepMaybeConfigured {
    fn to_json(&self) -> anyhow::Result<serde_json::Value>;
    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool>;
}

impl ExplicitConfiguredDepMaybeConfigured for UnconfiguredExplicitConfiguredDep {
    fn to_json(&self) -> anyhow::Result<serde_json::Value> {
        Ok(to_value(&[
            self.label.to_string(),
            self.platform.to_string(),
        ])?)
    }

    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool> {
        filter(&self.to_string())
    }
}

impl ExplicitConfiguredDepMaybeConfigured for ConfiguredExplicitConfiguredDep {
    fn to_json(&self) -> anyhow::Result<serde_json::Value> {
        Ok(to_value(self.to_string())?)
    }

    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool> {
        filter(&self.to_string())
    }
}
