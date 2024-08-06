/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::Path;

use buck2_cli_proto::new_generic::DocsResponse;
use buck2_cli_proto::new_generic::DocsStarlarkBuiltinsRequest;
use buck2_core::fs::paths::abs_path::AbsPath;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use dice::DiceTransaction;

use crate::get_builtin_docs;
use crate::markdown::generate_markdown_files;

pub(crate) async fn docs_starlark_builtins(
    _server_ctx: &dyn ServerCommandContextTrait,
    _dice_ctx: DiceTransaction,
    request: &DocsStarlarkBuiltinsRequest,
) -> anyhow::Result<DocsResponse> {
    let docs = get_builtin_docs();
    let path = AbsPath::new(Path::new(&request.path))?;
    generate_markdown_files(path, Path::new(""), Path::new(""), docs)?;

    Ok(DocsResponse { json_output: None })
}
