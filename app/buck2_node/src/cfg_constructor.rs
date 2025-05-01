/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_core::configuration::data::ConfigurationData;
use buck2_util::late_binding::LateBinding;
use dice::DiceComputations;

use crate::metadata::key::MetadataKeyRef;
use crate::metadata::value::MetadataValue;
use crate::nodes::unconfigured::TargetNodeRef;
use crate::rule_type::RuleType;
use crate::super_package::SuperPackage;

/// Trait for configuration constructor functions.
/// The output of invoking these functions is a PlatformInfo
#[async_trait]
pub trait CfgConstructorImpl: Send + Sync + Debug + Allocative {
    fn eval<'a>(
        &'a self,
        ctx: &'a mut DiceComputations,
        cfg: &'a ConfigurationData,
        package_cfg_modifiers: Option<&'a MetadataValue>,
        target_cfg_modifiers: Option<&'a MetadataValue>,
        cli_modifiers: &'a [String],
        rule_type: &'a RuleType,
    ) -> Pin<Box<dyn Future<Output = buck2_error::Result<ConfigurationData>> + Send + 'a>>;

    /// Returns the metadata key used to encode modifiers in PACKAGE values and metadata attribute
    fn key<'a>(&'a self) -> &'a MetadataKeyRef;
}

pub static CFG_CONSTRUCTOR_CALCULATION_IMPL: LateBinding<
    &'static dyn CfgConstructorCalculationImpl,
> = LateBinding::new("CFG_CONSTRUCTOR_CALCULATION_IMPL");

#[async_trait]
pub trait CfgConstructorCalculationImpl: Send + Sync + 'static {
    /// Invokes starlark cfg constructors on provided configuration
    /// and returns the result.
    async fn eval_cfg_constructor(
        &self,
        ctx: &mut DiceComputations<'_>,
        target: TargetNodeRef<'_>,
        super_package: &SuperPackage,
        cfg: ConfigurationData,
        cli_modifiers: &Arc<Vec<String>>,
        rule_name: &RuleType,
    ) -> buck2_error::Result<ConfigurationData>;
}
