/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! A debugging tool for working on the macro parser (used to parse attrs.arg() values into structured data).
// Plugins
#![cfg_attr(feature = "gazebo_lint", feature(plugin))]
#![cfg_attr(feature = "gazebo_lint", allow(deprecated))] // :(
#![cfg_attr(feature = "gazebo_lint", plugin(gazebo_lint))]

use structopt::clap::AppSettings;
use structopt::StructOpt;

#[cfg_attr(all(unix, not(fbcode_build)), global_allocator)]
#[cfg(all(unix, not(fbcode_build)))]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "buck-macroparse",
    about = "run buck string parameter macro parsing",
    global_settings(&[AppSettings::ColoredHelp]),
)]

pub struct Opt {
    #[structopt(name = "FILE", help = "parse lines in this file")]
    file: String,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let opt = Opt::from_args();
    let file = std::fs::read_to_string(opt.file)?;

    for line in file.lines() {
        println!(
            "line: `{}`\n result: `{:?}`",
            line,
            buck2_node::attrs::attr_type::arg::parser::parse_macros(line)
        );
    }

    Ok(())
}
