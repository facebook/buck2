/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_client_ctx::path_arg::PathArg;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::CellResolver;
use buck2_interpreter::common::OwnedStarlarkPath;
use buck2_server_ctx::ctx::ServerCommandContextTrait;

/// Find the paths to apply Starlark to (e.g. linter, typecheck)
pub(crate) fn starlark_files(
    paths: &[PathArg],
    context: &dyn ServerCommandContextTrait,
    cell_resolver: &CellResolver,
) -> anyhow::Result<Vec<OwnedStarlarkPath>> {
    let mut files = Vec::with_capacity(paths.len());
    for path in paths {
        let cell_path = cell_resolver.get_cell_path_from_abs_or_rel_path(
            path.path(),
            context.project_root(),
            context.working_dir(),
        )?;
        let build_file_cell = BuildFileCell::new(cell_path.cell().to_owned());
        files.push(OwnedStarlarkPath::LoadFile(ImportPath::new(
            cell_path,
            build_file_cell,
        )?));
    }
    Ok(files)
}
