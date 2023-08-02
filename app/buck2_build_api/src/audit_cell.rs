/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::cells::CellResolver;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_util::late_binding::LateBinding;
use indexmap::IndexMap;

pub static AUDIT_CELL: LateBinding<
    for<'v> fn(
        aliases_to_resolve: &'v Vec<String>,
        aliases: bool,
        cells: &'v CellResolver,
        cwd: &'v ProjectRelativePath,
        fs: &'v ProjectRoot,
    ) -> anyhow::Result<IndexMap<String, AbsNormPathBuf>>,
> = LateBinding::new("AUDIT_CELL");

pub fn audit_cell<'v>(
    aliases_to_resolve: &'v Vec<String>,
    aliases: bool,
    cells: &'v CellResolver,
    cwd: &'v ProjectRelativePath,
    fs: &'v ProjectRoot,
) -> anyhow::Result<IndexMap<String, AbsNormPathBuf>> {
    (AUDIT_CELL.get()?)(aliases_to_resolve, aliases, cells, cwd, fs)
}
