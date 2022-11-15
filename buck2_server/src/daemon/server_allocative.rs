/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::FlameGraphBuilder;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_path::AbsPathBuf;

use crate::daemon::server::BuckdServerData;

pub(crate) async fn spawn_allocative(
    buckd_server_data: Arc<BuckdServerData>,
    path: AbsPathBuf,
) -> anyhow::Result<()> {
    tokio::task::spawn_blocking(move || {
        let mut graph = FlameGraphBuilder::default();
        // TODO(nga): this can take a long time. Emit some progress.
        graph.visit_global_roots();
        graph.visit_root(&buckd_server_data);
        let fg = graph.finish();
        fs_util::create_dir_if_not_exists(&path)?;
        fs_util::write(path.join("flamegraph.src"), &fg.flamegraph())?;
        let mut fg_svg = Vec::new();
        inferno::flamegraph::from_reader(
            &mut inferno::flamegraph::Options::default(),
            fg.flamegraph().as_bytes(),
            &mut fg_svg,
        )?;
        fs_util::write(path.join("flamegraph.svg"), &fg_svg)?;

        fs_util::write(path.join("warnings.txt"), fg.warnings())?;

        anyhow::Ok(())
    })
    .await?
}
