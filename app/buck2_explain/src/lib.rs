/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fs;
use std::io::Cursor;

use base64::engine::general_purpose::STANDARD;
use base64::Engine;
use buck2_core::fs::paths::abs_path::AbsPathBuf;

#[allow(unused_imports)]
#[allow(unused_extern_crates)]
#[allow(clippy::extra_unused_lifetimes)]
mod explain_generated;
mod flatbuffers;
use buck2_common::manifold::Bucket;
use buck2_common::manifold::ManifoldClient;
use buck2_node::nodes::configured::ConfiguredTargetNode;

const HTML_PLACEHOLDER: &str = "XXDATAXX";

#[derive(Default)]
pub struct ActionEntryData {
    // TODO iguridi: add more interesting action fields e.g. duration
    pub category: Option<String>,
    pub failed: bool,
    pub repros: Vec<String>,
}

pub async fn main(
    data: Vec<ConfiguredTargetNode>,
    executed_actions: Vec<(String, ActionEntryData)>,
    output: Option<&AbsPathBuf>,
    fbs_dump: Option<&AbsPathBuf>,
    manifold_path: Option<&str>,
) -> anyhow::Result<()> {
    let fbs = flatbuffers::gen_fbs(data, executed_actions)?;

    let fbs = fbs.finished_data();
    let base64 = STANDARD.encode(&fbs);

    // For dev purposes, dump the base64 encoded flatbuffer to a file
    if let Some(fbs_dump) = fbs_dump {
        fs::write(fbs_dump, &base64)?;
    }

    let html_out = {
        let html_in = include_str!("explain.html");
        if !html_in.contains(HTML_PLACEHOLDER) {
            return Err(anyhow::anyhow!("HTML template is not valid"));
        }

        html_in.replace(HTML_PLACEHOLDER, &base64)
    };

    let mut cursor = &mut Cursor::new(html_out.as_bytes());

    if let Some(o) = output {
        fs::write(o, &html_out)?
    };

    if let Some(p) = manifold_path {
        // TODO iguridi: compress before upload
        // TODO iguridi: write and upload concurrently
        let manifold = ManifoldClient::new().await?;

        manifold
            .read_and_upload(Bucket::EVENT_LOGS, &p, Default::default(), &mut cursor)
            .await?;
    }

    Ok(())
}
