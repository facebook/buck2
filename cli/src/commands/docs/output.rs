/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use gazebo::prelude::*;

#[derive(Debug, Clone, Dupe, clap::ArgEnum)]
#[clap(rename_all = "snake_case")]
enum DocsOutputFormatArg {
    Markdown,
    Rendered,
}

#[derive(Debug, clap::Parser)]
pub(crate) struct DocsOutputFormatOptions {
    /// How to format the documentation
    #[clap(
        long = "format",
        default_value = "rendered",
        arg_enum,
        ignore_case = true
    )]
    format: DocsOutputFormatArg,
}

impl DocsOutputFormatOptions {
    pub fn emit_markdown(&self, markdown: &str) -> anyhow::Result<()> {
        match self.format {
            DocsOutputFormatArg::Markdown => {
                buck2_client_ctx::println!("{}", markdown)?;
            }
            DocsOutputFormatArg::Rendered => {
                let skin = termimad::MadSkin::default();
                let area = termimad::Area::full_screen();
                let width = std::cmp::min(100, area.width) as usize;
                let rendered = skin.text(markdown, Some(width));
                buck2_client_ctx::println!("{}", rendered)?;
            }
        }

        Ok(())
    }
}
