/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_common::dice::cells::HasCellResolver;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::package::PackageLabel;
use buck2_core::target::label::label::TargetLabel;
use buck2_error::BuckErrorContext;
use buck2_interpreter_for_build::interpreter::package_file_calculation::EvalPackageFile;
use buck2_node::cfg_constructor::CfgConstructorCalculationImpl;
use buck2_node::cfg_constructor::CfgConstructorImpl;
use buck2_node::metadata::value::MetadataValue;
use buck2_node::nodes::unconfigured::TargetNodeRef;
use buck2_node::rule_type::RuleType;
use buck2_node::super_package::SuperPackage;
use derive_more::Display;
use dice::CancellationContext;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use dupe::OptionDupedExt;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum CalculationCfgConstructorError {
    #[error(
        "Usage of both `modifiers` attribute and modifiers in metadata is not allowed for target `{0}`"
    )]
    TargetModifiersAttrAndMetadataNotAllowed(TargetLabel),
}

pub struct CfgConstructorCalculationInstance;

async fn get_cfg_constructor_uncached(
    ctx: &mut DiceComputations<'_>,
) -> buck2_error::Result<Option<Arc<dyn CfgConstructorImpl>>> {
    let root_cell = ctx.get_cell_resolver().await?.root_cell();
    let package_label = PackageLabel::new(root_cell, CellRelativePath::empty())?;
    // This returns empty super package if `PACKAGE` file does not exist.
    let super_package = ctx.eval_package_file(package_label).await?;
    Ok(super_package.cfg_constructor().duped())
}

async fn get_cfg_constructor(
    ctx: &mut DiceComputations<'_>,
) -> buck2_error::Result<Option<Arc<dyn CfgConstructorImpl>>> {
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
        .map_err(buck2_error::Error::from)
}

#[async_trait]
impl CfgConstructorCalculationImpl for CfgConstructorCalculationInstance {
    async fn eval_cfg_constructor(
        &self,
        ctx: &mut DiceComputations<'_>,
        target: TargetNodeRef<'_>,
        super_package: &SuperPackage,
        cfg: ConfigurationData,
        cli_modifiers: &Arc<Vec<String>>,
        rule_type: &RuleType,
        configuring_exec_dep: bool,
    ) -> buck2_error::Result<ConfigurationData> {
        #[derive(Clone, Display, Dupe, Debug, Eq, Hash, PartialEq, Allocative)]
        #[display("CfgConstructorInvocationKey")]
        struct CfgConstructorInvocationKey {
            package_cfg_modifiers: Option<MetadataValue>,
            target_cfg_modifiers: Option<MetadataValue>,
            cfg: ConfigurationData,
            cli_modifiers: Arc<Vec<String>>,
            rule_type: RuleType,
            configuring_exec_dep: bool,
        }

        #[async_trait]
        impl Key for CfgConstructorInvocationKey {
            type Value = buck2_error::Result<ConfigurationData>;

            async fn compute(
                &self,
                ctx: &mut DiceComputations,
                cancellation: &CancellationContext,
            ) -> Self::Value {
                let cfg_constructor = get_cfg_constructor(ctx).await?.buck_error_context(
                    "Internal error: Global cfg constructor instance should exist",
                )?;
                cfg_constructor
                    .eval(
                        ctx,
                        &self.cfg,
                        self.package_cfg_modifiers.as_ref(),
                        self.target_cfg_modifiers.as_ref(),
                        &self.cli_modifiers,
                        &self.rule_type,
                        self.configuring_exec_dep,
                        cancellation,
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

        let Some(cfg_constructor) = get_cfg_constructor(ctx).await? else {
            // To facilitate rollout of modifiers, return original configuration if
            // no cfg constructors are available.
            return Ok(cfg);
        };
        let modifier_key = cfg_constructor.key();
        let package_cfg_modifiers = super_package
            .cfg_modifiers()
            .map(|m| m.to_value())
            .map(MetadataValue::new);

        let metadata_modifiers = target.metadata()?.and_then(|m| m.get(modifier_key));
        let target_modifiers = target.target_modifiers()?;
        let target_cfg_modifiers = match (metadata_modifiers, target_modifiers) {
            (None, Some(t)) if !t.is_empty() => Some(MetadataValue(t.as_json())),
            (Some(_), Some(t)) if !t.is_empty() => {
                return Err(
                    CalculationCfgConstructorError::TargetModifiersAttrAndMetadataNotAllowed(
                        target.label().dupe(),
                    )
                    .into(),
                );
            }
            (Some(m), _) => Some(m.dupe()),
            _ => None,
        };

        // If there are no PACKAGE/target/cli modifiers, return the original configuration without computing DICE call
        // TODO(scottcao): This is just for rollout purpose. Remove once modifier is rolled out
        if package_cfg_modifiers.is_none()
            && target_cfg_modifiers.is_none()
            && cli_modifiers.is_empty()
        {
            return Ok(cfg);
        }

        let key = CfgConstructorInvocationKey {
            package_cfg_modifiers,
            target_cfg_modifiers,
            cfg,
            cli_modifiers: cli_modifiers.dupe(),
            rule_type: rule_type.dupe(),
            configuring_exec_dep,
        };
        Ok(ctx.compute(&key).await??)
    }
}
