/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_client_ctx::path_arg::PathArg;

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
pub(crate) struct MarkdownFileOptions {
    #[structopt(
        long = "markdown-files-destination-dir",
        required_if_eq("format", "markdown_files")
    )]
    pub(crate) destination_dir: Option<PathArg>,
    #[structopt(long = "markdown-files-native-subdir", default_value = "native")]
    pub(crate) native_subdir: String,
    #[structopt(long = "markdown-files-starlark-subdir", default_value = "starlark")]
    pub(crate) starlark_subdir: String,
}
