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
    pub(crate) fn render<W: Write, T: QueryTarget>(
        graph: TargetSet<T>,
        mut w: W,
    ) -> buck2_error::Result<()> {
        // create flatbuffer
        let html_out;
        #[cfg(fbcode_build)]
        {
            html_out = output_format(graph)?;
        }
        #[cfg(not(fbcode_build))]
        {
            let _unused = graph;
            html_out = "Not implemented".to_owned();
        }

        writeln!(w, "{}", html_out)?;
        Ok(())
    }
}
