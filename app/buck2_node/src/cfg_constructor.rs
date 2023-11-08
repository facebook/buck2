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
use crate::nodes::unconfigured::TargetNode;
use crate::super_package::SuperPackage;

/// Trait for configuration constructor functions.
/// The output of invoking these functions is a PlatformInfo
#[async_trait]
pub trait CfgConstructorImpl: Send + Sync + Debug + Allocative {
    fn eval<'a>(
        &'a self,
        ctx: &'a DiceComputations,
        cfg: &'a ConfigurationData,
        package_cfg_modifiers: Option<&'a MetadataValue>,
    ) -> Pin<Box<dyn Future<Output = anyhow::Result<ConfigurationData>> + Send + 'a>>;

    /// Returns the metadata key used to encode modifiers in PACKAGE values and metadata attribute
    fn key<'a>(&'a self) -> &'a MetadataKeyRef;
}

pub static CFG_CONSTRUCTOR_CALCULATION_IMPL: LateBinding<
    &'static dyn CfgConstructorCalculationImpl,
> = LateBinding::new("CFG_CONSTRUCTOR_CALCULATION_IMPL");

#[async_trait]
pub trait CfgConstructorCalculationImpl: Send + Sync + 'static {
    /// Loads and returns cfg constructor functions.
    async fn get_cfg_constructor(
        &self,
        ctx: &DiceComputations,
    ) -> anyhow::Result<Option<Arc<dyn CfgConstructorImpl>>>;

    /// Invokes starlark cfg constructors on provided configuration
    /// and returns the result.
    async fn eval_cfg_constructor(
        &self,
        ctx: &DiceComputations,
        target: &TargetNode,
        super_package: &SuperPackage,
        cfg: ConfigurationData,
    ) -> anyhow::Result<ConfigurationData>;
}
