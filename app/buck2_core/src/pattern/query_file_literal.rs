/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::Path;

use crate::cells::cell_path::CellPath;
use crate::cells::paths::CellRelativePath;
use crate::cells::CellAliasResolver;
use crate::cells::CellResolver;
use crate::fs::paths::abs_norm_path::AbsNormPath;
use crate::fs::project::ProjectRoot;
use crate::pattern::maybe_split_cell_alias_and_relative_path;

pub fn parse_query_file_literal(
    literal: &str,
    cell_alias_resolver: &CellAliasResolver,
    cell_resolver: &CellResolver,
    working_dir_abs: &AbsNormPath,
    project_root: &ProjectRoot,
) -> anyhow::Result<CellPath> {
    match maybe_split_cell_alias_and_relative_path(literal)? {
        Some((alias, path)) => {
            let cell_name = cell_alias_resolver.resolve(alias.as_str())?;

            let cell_relative_path = CellRelativePath::new(path);

            Ok(CellPath::new(cell_name, cell_relative_path.to_buf()))
        }
        None => {
            let path = Path::new(literal);
            // Note if the path is absolute, this `join` is a no-op.
            let path_abs = working_dir_abs.as_abs_path().join(path);
            let project_path = project_root.relativize_any(path_abs)?;
            cell_resolver.get_cell_path(&project_path)
        }
    }
}
