/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fs::File;
use std::path::PathBuf;

use clap::Parser;
use dice::introspection::graph::SerializedGraphNodesForKey;

#[derive(Debug, clap::Parser)]
#[clap(name = "read_dump", about = "dice dump reader")]
pub(crate) struct Opt {
    #[clap(name = "DICE_DUMP", help = "The dice dump")]
    file: PathBuf,
    #[clap(long = "out", help = "Copy the output to this path")]
    out: Option<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let clap = Opt::clap();
    let matches = clap.get_matches_from(std::env::args().collect::<Vec<String>>());
    let opt = Opt::from_clap(&matches);

    let file = File::open(opt.file)?;

    let out: Vec<SerializedGraphNodesForKey> = bincode::deserialize_from(&file)?;

    match opt.out {
        Some(path) => {
            serde_json::to_writer_pretty(File::create(path)?, &out)?;
        }
        None => {
            serde_json::to_writer_pretty(std::io::stdout(), &out)?;
        }
    };

    Ok(())
}
