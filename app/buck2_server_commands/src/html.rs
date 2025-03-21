/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::Write;

#[cfg(fbcode_build)]
use buck2_explain::output_format;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_query::query::environment::QueryTarget;
use buck2_query::query::syntax::simple::eval::set::TargetSet;

pub struct HtmlTargetGraph {
    pub targets: TargetSet<ConfiguredTargetNode>,
    // TODO iguridi: add attributes
}

pub struct Html {}

impl Html {
    pub(crate) async fn render<W: Write, T: QueryTarget>(
        graph: TargetSet<T>,
        mut w: W,
        trace_id: String,
    ) -> buck2_error::Result<()> {
        let res;
        #[cfg(fbcode_build)]
        {
            use std::io::Cursor;

            use buck2_common::manifold::Bucket;
            use buck2_common::manifold::ManifoldClient;

            let html_out = output_format(graph)?;
            let mut cursor = &mut Cursor::new(html_out.as_bytes());
            let manifold_path = format!("flat/{}-graph.html", trace_id);
            let manifold = ManifoldClient::new().await?;

            manifold
                .read_and_upload(
                    Bucket::EVENT_LOGS,
                    &manifold_path,
                    Default::default(),
                    &mut cursor,
                )
                .await?;
            res = format!(
                "\nView html in your browser: https://interncache-all.fbcdn.net/manifold/buck2_logs/{} (requires VPN/lighthouse)\n",
                manifold_path
            );
        }
        #[cfg(not(fbcode_build))]
        {
            let _unused = (graph, trace_id);
            res = "Not implemented".to_owned();
        }

        writeln!(w, "{}", res)?;
        Ok(())
    }
}
