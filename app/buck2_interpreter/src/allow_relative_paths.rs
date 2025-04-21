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
use buck2_common::dice::file_ops::DiceFileComputations;
use buck2_common::file_ops::RawPathMetadata;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::legacy_configs::key::BuckconfigKeyRef;
use buck2_common::legacy_configs::view::LegacyBuckConfigView;
use buck2_core::cells::CellAliasResolver;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::cell_path_with_allowed_relative_dir::CellPathWithAllowedRelativeDir;
use buck2_core::cells::paths::CellRelativePathBuf;
use buck2_futures::cancellation::CancellationContext;
use dice::DiceComputations;
use dice::Key;

#[derive(buck2_error::Error, Debug)]
#[buck2(input)]
enum RelativePathParseError {
    #[error("Relative path `{0}` could not be parsed.")]
    InvalidAllowedDirForRelativePaths(String),
    #[error("Multiple relative-enabled directory prefixes found for the current dir: `{0}`.")]
    MultipleAllowedDirsForCurrentDir(String),
    #[error(
        "Symlink found on the way from current dir `{0}` to allowed relative dir `{1}`: `{2}`."
    )]
    SymlinkFound(String, String, String),
}

#[async_trait]
pub trait HasAllowRelativePaths {
    async fn dirs_allowing_relative_paths(
        &mut self,
        cell_path: CellPath,
    ) -> buck2_error::Result<Arc<CellPathWithAllowedRelativeDir>>;
}

#[async_trait]
impl HasAllowRelativePaths for DiceComputations<'_> {
    async fn dirs_allowing_relative_paths(
        &mut self,
        cell_path: CellPath,
    ) -> buck2_error::Result<Arc<CellPathWithAllowedRelativeDir>> {
        #[derive(Debug, Eq, PartialEq, Hash, Clone, derive_more::Display, Allocative)]
        #[display("{}", cell_path)]
        struct AllowRelativePathsKey {
            cell_path: CellPath,
        }

        #[async_trait]
        impl Key for AllowRelativePathsKey {
            type Value = buck2_error::Result<Arc<CellPathWithAllowedRelativeDir>>;

            async fn compute(
                &self,
                ctx: &mut DiceComputations,
                _cancellation: &CancellationContext,
            ) -> Self::Value {
                let config = ctx.get_legacy_root_config_on_dice().await?;
                let cell_resolver = ctx.get_cell_resolver().await?;
                let cell_alias_resolver = cell_resolver.root_cell_cell_alias_resolver();

                let allowed_relative_dirs_for_current_dir = config
                    .view(ctx)
                    .parse_list::<String>(BuckconfigKeyRef {
                        section: "buck2",
                        property: "directories_to_allow_relative_paths",
                    })?
                    .unwrap_or_default()
                    .into_iter()
                    .filter_map(|dir| {
                        get_cell_path_from_raw_string(dir, cell_alias_resolver)
                            .map(|allow_dir| {
                                if self.cell_path.starts_with(allow_dir.as_ref()) {
                                    Some(allow_dir)
                                } else {
                                    None
                                }
                            })
                            .transpose()
                    })
                    .collect::<Result<Vec<CellPath>, _>>()?;

                if allowed_relative_dirs_for_current_dir.len() > 1 {
                    return Err(RelativePathParseError::MultipleAllowedDirsForCurrentDir(
                        self.cell_path.to_string(),
                    )
                    .into());
                }

                let allowed_relative_dir = allowed_relative_dirs_for_current_dir.first();

                if let Some(allowed_relative_dir) = allowed_relative_dir {
                    validate_no_symlinks_between_current_dir_and_allowed_dir(
                        ctx,
                        &self.cell_path,
                        allowed_relative_dir,
                    )
                    .await?;
                }

                Ok(Arc::new(CellPathWithAllowedRelativeDir::new(
                    self.cell_path.clone(),
                    allowed_relative_dir.cloned(),
                )))
            }

            fn equality(x: &Self::Value, y: &Self::Value) -> bool {
                match (x, y) {
                    (Ok(x), Ok(y)) => x == y,
                    _ => false,
                }
            }
        }

        self.compute(&AllowRelativePathsKey { cell_path }).await?
    }
}

fn get_cell_path_from_raw_string(
    dir: String,
    cell_alias_resolver: &CellAliasResolver,
) -> buck2_error::Result<CellPath> {
    let Some((cell_str, path_str)) = dir.trim().split_once("//") else {
        return Err(RelativePathParseError::InvalidAllowedDirForRelativePaths(dir).into());
    };
    let cell = cell_alias_resolver.resolve(cell_str)?;
    let path: CellRelativePathBuf = CellRelativePathBuf::try_from(path_str.to_owned())?;
    Ok(CellPath::new(cell, path))
}

async fn validate_no_symlinks_between_current_dir_and_allowed_dir(
    ctx: &mut DiceComputations<'_>,
    current_dir: &CellPath,
    allowed_dir: &CellPath,
) -> buck2_error::Result<()> {
    // For all the paths between current_dir and allowed_dir, inclusive
    // check that there are no symlinks
    let mut current_dir = current_dir.as_ref();
    loop {
        if let RawPathMetadata::Symlink { at, to: _ } =
            DiceFileComputations::read_path_metadata(ctx, current_dir).await?
        {
            return Err(RelativePathParseError::SymlinkFound(
                current_dir.to_string(),
                allowed_dir.to_string(),
                at.as_ref().to_string(),
            )
            .into());
        }
        if current_dir == allowed_dir.as_ref() {
            break;
        }
        current_dir = current_dir
            .parent()
            .expect("current dir should have parent if not equal to allowed dir");
    }
    Ok(())
}
