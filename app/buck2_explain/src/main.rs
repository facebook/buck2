/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::Write;

fn main() -> anyhow::Result<()> {
    let data = "ewogICAgImZiY29kZS8vYnVjazI6YnVjazIgKGNmZzptYWNvcy14ODZfNjQtbWFjb3N4LW5vLXNhbiM4YmU4MWQ5OTNkZmI3NmEwKSI6IHsKICAgICAgImJ1Y2sudHlwZSI6ICJhbGlhcyIsCiAgICAgICJidWNrLnBhY2thZ2UiOiAiZmJjb2RlLy9idWNrMjpUQVJHRVRTIiwKICAgICAgIm5hbWUiOiAiYnVjazIiLAogICAgICAiZGVmYXVsdF90YXJnZXRfcGxhdGZvcm0iOiAib3ZyX2NvbmZpZy8vcGxhdGZvcm0vbWFjb3M6eDg2XzY0LWZic291cmNlIChjZmc6bWFjb3MteDg2XzY0LW1hY29zeC1uby1zYW4jOGJlODFkOTkzZGZiNzZhMCkiLAogICAgICAidGFyZ2V0X2NvbXBhdGlibGVfd2l0aCI6IFtdLAogICAgICAiY29tcGF0aWJsZV93aXRoIjogW10sCiAgICAgICJleGVjX2NvbXBhdGlibGVfd2l0aCI6IFtdLAogICAgICAidmlzaWJpbGl0eSI6IFsKICAgICAgICAiUFVCTElDIgogICAgICBdLAogICAgICAid2l0aGluX3ZpZXciOiBbCiAgICAgICAgIlBVQkxJQyIKICAgICAgXSwKICAgICAgIm1ldGFkYXRhIjoge30sCiAgICAgICJ0ZXN0cyI6IFtdLAogICAgICAiX2FwcGxlX3BsYXRmb3JtcyI6IHt9LAogICAgICAiYWN0dWFsIjogImZiY29kZS8vYnVjazIvYXBwL2J1Y2syOmJ1Y2syLWJpbiAoY2ZnOm1hY29zLXg4Nl82NC1tYWNvc3gtbm8tc2FuIzhiZTgxZDk5M2RmYjc2YTApIiwKICAgICAgImJ1Y2syX2NvbXBhdGliaWxpdHkiOiAidW5rbm93biIsCiAgICAgICJjb250YWN0cyI6IFtdLAogICAgICAiZGVmYXVsdF9ob3N0X3BsYXRmb3JtIjogIm92cl9jb25maWcvL3BsYXRmb3JtL21hY29zOng4Nl82NC1mYnNvdXJjZS1sb2NhbCAoY2ZnOm1hY29zLXg4Nl82NC1tYWNvc3gtbm8tc2FuIzhiZTgxZDk5M2RmYjc2YTApIiwKICAgICAgImxhYmVscyI6IFsKICAgICAgICAiY2k6YWFyY2g2NDpza2lwX3Rlc3QiCiAgICAgIF0sCiAgICAgICJsaWNlbnNlcyI6IFtdCiAgICB9CiAgfQo=";

    let html_in = include_str!("explain.html");
    let html_out = html_in.replace("XXDATAXX", data);
    // TODO: find a better way to assert it actually replaced something
    if html_in == html_out {
        return Err(anyhow::anyhow!("HTML template is not valid"));
    }

    let mut writer = std::fs::File::create("/tmp/index.html")?;
    writer.write_all(&html_out.into_bytes()[..])?;
    writer.flush()?;

    Ok(())
}
