/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_common::dice::cells::HasCellResolver;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::configuration::data::ConfigurationData;
use buck2_error::Context;
use buck2_interpreter::paths::package::PackageFilePath;
use buck2_interpreter_for_build::interpreter::package_file_calculation::EvalPackageFile;
use buck2_node::cfg_constructor::CfgConstructorCalculationImpl;
use buck2_node::cfg_constructor::CfgConstructorImpl;
use buck2_node::cfg_constructor::CFG_CONSTRUCTOR_CALCULATION_IMPL;
use buck2_node::metadata::value::MetadataValue;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_node::super_package::SuperPackage;
use derive_more::Display;
use dice::CancellationContext;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use dupe::OptionDupedExt;

pub struct CfgConstructorCalculationInstance;

async fn get_cfg_constructor_uncached(
    ctx: &DiceComputations,
) -> anyhow::Result<Option<Arc<dyn CfgConstructorImpl>>> {
    let root_cell = ctx.get_cell_resolver().await?.root_cell();
    let package_file_path =
        PackageFilePath::for_dir(CellPathRef::new(root_cell, CellRelativePath::empty()));
    // This returns empty super package if `PACKAGE` file does not exist.
    let super_package = ctx.eval_package_file(&package_file_path).await?;
    Ok(super_package.cfg_constructor().duped())
}

#[async_trait]
impl CfgConstructorCalculationImpl for CfgConstructorCalculationInstance {
    async fn get_cfg_constructor(
        &self,
        ctx: &DiceComputations,
    ) -> anyhow::Result<Option<Arc<dyn CfgConstructorImpl>>> {
        #[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Allocative)]
        struct GetCfgConstructorKey;

        #[async_trait]
        impl Key for GetCfgConstructorKey {
            type Value = buck2_error::Result<Option<Arc<dyn CfgConstructorImpl>>>;

            async fn compute(
                &self,
                ctx: &mut DiceComputations,
                _cancellations: &CancellationContext,
            ) -> Self::Value {
                get_cfg_constructor_uncached(ctx)
                    .await
                    .map_err(buck2_error::Error::from)
            }

            fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
                false
            }
        }

        ctx.compute(&GetCfgConstructorKey)
            .await?
            .map_err(anyhow::Error::from)
    }

    async fn eval_cfg_constructor(
        &self,
        ctx: &DiceComputations,
        target: &TargetNode,
        super_package: &SuperPackage,
        cfg: ConfigurationData,
    ) -> anyhow::Result<ConfigurationData> {
        #[derive(Clone, Display, Dupe, Debug, Eq, Hash, PartialEq, Allocative)]
        #[display(fmt = "CfgConstructorInvocationKey")]
        struct CfgConstructorInvocationKey {
            package_cfg_modifiers: Option<MetadataValue>,
            target_cfg_modifiers: Option<MetadataValue>,
            cfg: ConfigurationData,
        }

        #[async_trait]
        impl Key for CfgConstructorInvocationKey {
            type Value = buck2_error::Result<ConfigurationData>;

            async fn compute(
                &self,
                ctx: &mut DiceComputations,
                _cancellations: &CancellationContext,
            ) -> Self::Value {
                let cfg_constructor_calculation = CFG_CONSTRUCTOR_CALCULATION_IMPL.get()?;
                // Invoke eval fn from global instance of cfg constructors
                let cfg_constructor = cfg_constructor_calculation
                    .get_cfg_constructor(ctx)
                    .await?
                    .context("Internal error: Global cfg constructor instance should exist")?;
                cfg_constructor
                    .eval(
                        ctx,
                        &self.cfg,
                        self.package_cfg_modifiers.as_ref(),
                        self.target_cfg_modifiers.as_ref(),
                    )
                    .await
                    .map_err(buck2_error::Error::from)
            }

            fn equality(x: &Self::Value, y: &Self::Value) -> bool {
                match (x, y) {
                    (Ok(x), Ok(y)) => x == y,
                    _ => false,
                }
            }
        }

        match self.get_cfg_constructor(ctx).await? {
            Some(cfg_constructor) => {
                let modifier_key = cfg_constructor.key();
                let package_cfg_modifiers = super_package
                    .package_values()
                    .get_package_value_json(modifier_key)?
                    .map(MetadataValue::new);
                let target_cfg_modifiers =
                    target.metadata()?.and_then(|m| m.get(modifier_key)).duped();

                let key = CfgConstructorInvocationKey {
                    package_cfg_modifiers,
                    target_cfg_modifiers,
                    cfg,
                };
                Ok(ctx.compute(&key).await??)
            }
            // To facilitate rollout of modifiers, return original configuration if
            // no cfg constructors are available.
            None => Ok(cfg),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
struct CfgConstructorLocation {
    pub import_path: ImportPath,
    pub function: String,
}
