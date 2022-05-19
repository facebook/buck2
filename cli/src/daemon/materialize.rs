/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::fs::project::ProjectRelativePath;

use crate::daemon::server::BaseCommandContext;

pub async fn materialize(server_ctx: BaseCommandContext, paths: Vec<String>) -> anyhow::Result<()> {
    let mut project_paths = Vec::new();
    for path in paths {
        project_paths.push(ProjectRelativePath::new(&path)?.to_owned())
    }
    server_ctx
        .materializer
        .ensure_materialized(project_paths)
        .await
}
