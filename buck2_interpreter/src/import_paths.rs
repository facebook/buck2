/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::convert::TryFrom;
use std::sync::Arc;

use async_trait::async_trait;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::legacy_configs::LegacyBuckConfig;
use buck2_core::cells::paths::CellPath;
use buck2_core::cells::paths::CellRelativePathBuf;
use buck2_core::cells::CellAliasResolver;
use buck2_core::result::SharedResult;
use dice::DiceComputations;
use dice::Key;
use gazebo::dupe::Dupe;

use crate::common::BuildFileCell;
use crate::common::ImportPath;
use crate::package_imports::PackageImplicitImports;

#[derive(PartialEq)]
pub struct ImportPaths {
    pub(crate) root_import: Option<ImportPath>,
    pub(crate) package_imports: PackageImplicitImports,
}

impl ImportPaths {
    pub fn parse(
        config: &LegacyBuckConfig,
        cell_name: &BuildFileCell,
        cell_alias_resolver: &CellAliasResolver,
    ) -> anyhow::Result<ImportPaths> {
        // Oddly, the root import is defined to use a more path-like representation than
        // normal imports. e.g. it uses `cell//path/to/file.bzl` instead of
        // `cell//path/to:file.bzl`.
        let root_import = config
            .get("buildfile", "includes")
            .map(|i| {
                let (cell_alias, path): (&str, &str) = i.split_once("//").unwrap_or(("", i));
                let path = CellRelativePathBuf::try_from(path.to_owned())?;
                let path = CellPath::new(
                    cell_alias_resolver.resolve(cell_alias)?.clone(),
                    path.to_buf(),
                );

                // root imports are only going to be used by a top-level module in the cell they
                // are defined, so we can set the build_file_cell early.
                ImportPath::new(path, cell_name.clone())
            })
            .map_or(Ok(None), |e: anyhow::Result<ImportPath>| e.map(Some))?;
        let package_imports = PackageImplicitImports::new(
            cell_name,
            cell_alias_resolver.dupe(),
            config.get("buildfile", "package_includes"),
        )?;
        Ok(ImportPaths {
            root_import,
            package_imports,
        })
    }
}

#[async_trait]
pub(crate) trait HasImportPaths {
    async fn import_paths_for_cell(
        &self,
        cell_name: &BuildFileCell,
    ) -> SharedResult<Arc<ImportPaths>>;
}

#[async_trait]
impl HasImportPaths for DiceComputations {
    async fn import_paths_for_cell(
        &self,
        cell_name: &BuildFileCell,
    ) -> SharedResult<Arc<ImportPaths>> {
        #[derive(Debug, Eq, PartialEq, Hash, Clone, derive_more::Display)]
        #[display(fmt = "{}", cell_name)]
        struct ImportPathsKey {
            cell_name: BuildFileCell,
        }

        #[async_trait]
        impl Key for ImportPathsKey {
            type Value = SharedResult<Arc<ImportPaths>>;

            async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
                let config = ctx
                    .get_legacy_config_for_cell(self.cell_name.name())
                    .await?;
                let cell_resolver = ctx.get_cell_resolver().await;
                let cell_alias_resolver = cell_resolver
                    .get(self.cell_name.name())?
                    .cell_alias_resolver();
                Ok(Arc::new(ImportPaths::parse(
                    &config,
                    &self.cell_name,
                    cell_alias_resolver,
                )?))
            }

            fn equality(x: &Self::Value, y: &Self::Value) -> bool {
                match (x, y) {
                    (Ok(x), Ok(y)) => x == y,
                    _ => false,
                }
            }
        }

        self.compute(&ImportPathsKey {
            cell_name: cell_name.clone(),
        })
        .await
    }
}
