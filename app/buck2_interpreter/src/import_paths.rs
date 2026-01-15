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
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::legacy_configs::key::BuckconfigKeyRef;
use buck2_common::legacy_configs::view::LegacyBuckConfigView;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::CellAliasResolver;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::paths::CellRelativePathBuf;
use dice::DiceComputations;
use dice::Key;
use dice_futures::cancellation::CancellationContext;
use dupe::Dupe;
use pagable::Pagable;

use crate::package_imports::PackageImplicitImports;

#[derive(PartialEq, Allocative, Pagable)]
pub struct ImplicitImportPaths {
    pub root_import: Option<ImportPath>,
    pub package_imports: PackageImplicitImports,
}

impl ImplicitImportPaths {
    pub fn parse(
        mut config: impl LegacyBuckConfigView,
        cell_name: BuildFileCell,
        cell_alias_resolver: &CellAliasResolver,
    ) -> buck2_error::Result<ImplicitImportPaths> {
        // Oddly, the root import is defined to use a more path-like representation than
        // normal imports. e.g. it uses `cell//path/to/file.bzl` instead of
        // `cell//path/to:file.bzl`.
        let root_import = config
            .get(BuckconfigKeyRef {
                section: "buildfile",
                property: "includes",
            })?
            .map(|i| {
                let (cell_alias, path): (&str, &str) = i.split_once("//").unwrap_or(("", &*i));
                let path = CellRelativePathBuf::try_from(path.to_owned())?;
                let path = CellPath::new(cell_alias_resolver.resolve(cell_alias)?, path.to_buf());

                // root imports are only going to be used by a top-level module in the cell they
                // are defined, so we can set the build_file_cell early.
                ImportPath::new_with_build_file_cells(path, cell_name)
            })
            .map_or(Ok(None), |e: buck2_error::Result<ImportPath>| e.map(Some))?;
        let package_imports = PackageImplicitImports::new(
            cell_name,
            cell_alias_resolver.dupe(),
            config
                .get(BuckconfigKeyRef {
                    section: "buildfile",
                    property: "package_includes",
                })?
                .as_deref(),
        )?;
        Ok(ImplicitImportPaths {
            root_import,
            package_imports,
        })
    }

    pub fn root_import(&self) -> Option<&ImportPath> {
        self.root_import.as_ref()
    }
}

#[async_trait]
pub trait HasImportPaths {
    async fn import_paths_for_cell(
        &mut self,
        cell_name: BuildFileCell,
    ) -> buck2_error::Result<Arc<ImplicitImportPaths>>;
}

#[async_trait]
impl HasImportPaths for DiceComputations<'_> {
    async fn import_paths_for_cell(
        &mut self,
        cell_name: BuildFileCell,
    ) -> buck2_error::Result<Arc<ImplicitImportPaths>> {
        #[derive(Debug, Eq, PartialEq, Hash, Clone, derive_more::Display, Allocative)]
        #[display("{}", cell_name)]
        struct ImportPathsKey {
            cell_name: BuildFileCell,
        }

        #[async_trait]
        impl Key for ImportPathsKey {
            type Value = buck2_error::Result<Arc<ImplicitImportPaths>>;

            async fn compute(
                &self,
                ctx: &mut DiceComputations,
                _cancellation: &CancellationContext,
            ) -> Self::Value {
                let config = ctx.get_legacy_config_on_dice(self.cell_name.name()).await?;
                let cell_alias_resolver =
                    ctx.get_cell_alias_resolver(self.cell_name.name()).await?;

                Ok(Arc::new(ImplicitImportPaths::parse(
                    config.view(ctx),
                    self.cell_name,
                    &cell_alias_resolver,
                )?))
            }

            fn equality(x: &Self::Value, y: &Self::Value) -> bool {
                match (x, y) {
                    (Ok(x), Ok(y)) => x == y,
                    _ => false,
                }
            }
        }

        self.compute(&ImportPathsKey { cell_name }).await?
    }
}
