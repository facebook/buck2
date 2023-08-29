/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(dead_code)]

use std::sync::Arc;

use async_trait::async_trait;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_configured::configuration::cfg_constructor::CfgConstructorCalculationImpl;
use buck2_configured::configuration::cfg_constructor::CfgConstructorImpl;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::CellAliasResolver;
use buck2_interpreter::load_module::InterpreterCalculation;
use buck2_interpreter::parse_import::parse_import_with_config;
use buck2_interpreter::parse_import::ParseImportOptions;
use buck2_interpreter::parse_import::RelativeImports;
use dice::DiceComputations;

use crate::CfgConstructor;
pub struct CfgConstructorCalculationInstance;

async fn get_cfg_constructor_uncached(
    ctx: &DiceComputations,
) -> anyhow::Result<Option<Arc<dyn CfgConstructorImpl>>> {
    let root_config = ctx.get_legacy_root_config_on_dice().await?;
    let Some(cfg_constructor_pre_constraint_analysis_config) =
        root_config.get("buck2", "cfg_constructor_pre_constraint_analysis")?
    else {
        return Ok(None);
    };
    let Some(cfg_constructor_post_constraint_analysis_config) =
        root_config.get("buck2", "cfg_constructor_post_constraint_analysis")?
    else {
        return Ok(None);
    };

    let cell_resolver = ctx.get_cell_resolver().await?;
    let cell_alias_resolver = cell_resolver.root_cell_cell_alias_resolver();

    let cfg_constructor_pre_constraint_analysis = parse_cfg_constructor_config(
        &cfg_constructor_pre_constraint_analysis_config,
        cell_alias_resolver,
    )?;
    let cfg_constructor_post_constraint_analysis = parse_cfg_constructor_config(
        &cfg_constructor_post_constraint_analysis_config,
        cell_alias_resolver,
    )?;

    load_cfg_constructor(
        ctx,
        &cfg_constructor_pre_constraint_analysis,
        &cfg_constructor_post_constraint_analysis,
    )
    .await
    .map(Some)
}

async fn load_cfg_constructor(
    ctx: &DiceComputations,
    cfg_constructor_pre_constraint_analysis_loc: &CfgConstructorLocation,
    cfg_constructor_post_constraint_analysis_loc: &CfgConstructorLocation,
) -> anyhow::Result<Arc<dyn CfgConstructorImpl>> {
    let (pre_constraint_analysis_module, post_constraint_analysis_module) =
        futures::future::try_join(
            ctx.get_loaded_module_from_import_path(
                &cfg_constructor_pre_constraint_analysis_loc.import_path,
            ),
            ctx.get_loaded_module_from_import_path(
                &cfg_constructor_post_constraint_analysis_loc.import_path,
            ),
        )
        .await?;
    let cfg_constructor_pre_constraint_analysis = pre_constraint_analysis_module
        .env()
        .get(&cfg_constructor_pre_constraint_analysis_loc.function)?;
    let cfg_constructor_post_constraint_analysis = post_constraint_analysis_module
        .env()
        .get(&cfg_constructor_post_constraint_analysis_loc.function)?;
    Ok(Arc::new(CfgConstructor {
        cfg_constructor_pre_constraint_analysis,
        cfg_constructor_post_constraint_analysis,
    }))
}

#[async_trait]
impl CfgConstructorCalculationImpl for CfgConstructorCalculationInstance {}

#[derive(Debug, thiserror::Error)]
enum CfgConstructorError {
    #[error("Expected config in form of `<cell>//path/to/file.bzl:function_name`, got `{0}`")]
    BadConfigFormat(String),
}

#[derive(Debug, PartialEq, Eq)]
struct CfgConstructorLocation {
    pub import_path: ImportPath,
    pub function: String,
}

fn parse_cfg_constructor_config(
    config: &str,
    cell_alias_resolver: &CellAliasResolver,
) -> anyhow::Result<CfgConstructorLocation> {
    let (path, function) = config
        .rsplit_once(':')
        .ok_or_else(|| CfgConstructorError::BadConfigFormat(config.to_owned()))?;

    let opts = ParseImportOptions {
        allow_missing_at_symbol: true,
        relative_import_option: RelativeImports::Disallow,
    };
    let import_path = parse_import_with_config(cell_alias_resolver, path, &opts)?;
    let import_path = ImportPath::new_same_cell(import_path)?;

    Ok(CfgConstructorLocation {
        import_path,
        function: function.to_owned(),
    })
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use buck2_core::cells::alias::NonEmptyCellAlias;
    use buck2_core::cells::name::CellName;
    use buck2_core::cells::CellAliasResolver;

    use super::*;

    fn resolver() -> CellAliasResolver {
        let aliases = HashMap::from([(
            NonEmptyCellAlias::new("root".to_owned()).unwrap(),
            CellName::testing_new("root"),
        )]);
        CellAliasResolver::new(CellName::testing_new("root"), aliases).expect("valid resolver")
    }

    #[test]
    fn test_parse_cfg_constructor_config() {
        let cell_alias_resolver = resolver();
        assert_eq!(
            parse_cfg_constructor_config(
                "root//cfg/cfg_constructor.bzl:function",
                &cell_alias_resolver
            )
            .unwrap(),
            CfgConstructorLocation {
                import_path: ImportPath::testing_new("root//cfg:cfg_constructor.bzl"),
                function: "function".to_owned(),
            }
        );
    }
}
