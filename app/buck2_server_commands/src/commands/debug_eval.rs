/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_cli_proto::new_generic::DebugEvalRequest;
use buck2_cli_proto::new_generic::DebugEvalResponse;
use buck2_common::dice::cells::HasCellResolver;
use buck2_core::bxl::BxlFilePath;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_path::AbsPathBuf;
use buck2_interpreter::load_module::InterpreterCalculation;
use buck2_interpreter::paths::module::OwnedStarlarkModulePath;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum DebugEvalError {
    #[error("Can only eval `.bzl` or `.bxl`, but got `{0}`")]
    InvalidImportPath(CellPath),
}

pub(crate) async fn debug_eval_command(
    context: &dyn ServerCommandContextTrait,
    req: DebugEvalRequest,
) -> buck2_error::Result<DebugEvalResponse> {
    context
        .with_dice_ctx(|server_ctx, mut ctx| async move {
            let cell_resolver = ctx.get_cell_resolver().await?;
            let current_cell_path = cell_resolver.get_cell_path(server_ctx.working_dir())?;
            let mut loads = Vec::new();

            let ctx = &ctx;
            for path in req.paths {
                let path = AbsPathBuf::new(path)?;
                let path = fs_util::canonicalize(&path)?;
                let path = context.project_root().relativize(&path)?;
                let path = cell_resolver.get_cell_path(&path)?;
                let import_path = if path.path().as_str().ends_with(".bzl") {
                    OwnedStarlarkModulePath::LoadFile(ImportPath::new_with_build_file_cells(
                        path,
                        BuildFileCell::new(current_cell_path.cell()),
                    )?)
                } else if path.path().as_str().ends_with(".bxl") {
                    OwnedStarlarkModulePath::BxlFile(BxlFilePath::new(path)?)
                } else {
                    return Err(DebugEvalError::InvalidImportPath(path).into());
                };
                loads
                    .push(async move { ctx.clone().get_loaded_module(import_path.borrow()).await });
            }

            // Catch errors, ignore results.
            buck2_util::future::try_join_all(loads).await?;

            Ok(DebugEvalResponse {})
        })
        .await
}
