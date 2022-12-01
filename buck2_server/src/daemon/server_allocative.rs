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
use buck2_events::dispatch::EventDispatcher;

use crate::daemon::server::BuckdServerData;

pub(crate) async fn spawn_allocative(
    buckd_server_data: Arc<BuckdServerData>,
    path: AbsPathBuf,
    dispatcher: EventDispatcher,
) -> anyhow::Result<()> {
    tokio::task::spawn_blocking(move || {
        let mut graph = FlameGraphBuilder::default();
        dispatcher.console_message(
            "Starting allocative profiling. It may take a while to finish...".to_owned(),
        );
        // TODO(nga): Emit some progress.
        dispatcher.console_message("Visiting global roots...".to_owned());
        graph.visit_global_roots();
        dispatcher.console_message("Visiting buckd...".to_owned());
        graph.visit_root(&buckd_server_data);
        let fg = graph.finish();
        fs_util::create_dir_if_not_exists(&path)?;
        dispatcher.console_message(format!("Writing allocative to `{}`...", path.display()));
        fs_util::write(path.join("flamegraph.src"), &fg.flamegraph())?;
        let mut fg_svg = Vec::new();
        inferno::flamegraph::from_reader(
            &mut inferno::flamegraph::Options::default(),
            fg.flamegraph().as_bytes(),
            &mut fg_svg,
        )?;
        fs_util::write(path.join("flamegraph.svg"), &fg_svg)?;

        fs_util::write(path.join("warnings.txt"), fg.warnings())?;

        dispatcher.console_message("Profile written.".to_owned());

        anyhow::Ok(())
    })
    .await?
}
