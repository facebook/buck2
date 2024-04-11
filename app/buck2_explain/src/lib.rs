/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fs;

use buck2_core::fs::paths::abs_path::AbsPathBuf;

pub fn main(data: String, output: &AbsPathBuf) -> anyhow::Result<()> {
    let html_in = include_str!("explain.html");
    let html_out = html_in.replace("XXDATAXX", &data);
    // TODO: find a better way to assert it actually replaced something
    if html_in == html_out {
        return Err(anyhow::anyhow!("HTML template is not valid"));
    }

    fs::write(output, &html_out)?;

    Ok(())
}
