/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! A debugging tool for working on the macro parser (used to parse attr.arg() values into structured data).
// Plugins
#![cfg_attr(feature = "gazebo_lint", feature(plugin))]
#![cfg_attr(feature = "gazebo_lint", allow(deprecated))] // :(
#![cfg_attr(feature = "gazebo_lint", plugin(gazebo_lint))]

use std::io::BufRead;

use buck2_query_parser::parse_expr;
use structopt::clap::AppSettings;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "buck-queryparse",
    about = "run buck query parsing",
    global_settings(&[AppSettings::ColoredHelp]),
)]

pub struct Opt {
    #[structopt(name = "FILE", help = "parse lines in this file")]
    file: String,
}

fn main() -> anyhow::Result<()> {
    let opt = Opt::from_args();
    let f = std::fs::File::open(opt.file).unwrap();
    let file = std::io::BufReader::new(&f);
    let lines: Vec<_> = file.lines().map(|l| l.unwrap()).collect();

    for line in lines {
        println!("line: `{}`\n result: `{:?}`", line, parse_expr(&line));
    }

    Ok(())
}
